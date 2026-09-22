(require "helix/treesitter.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require "helix/components.scm")
(require (prefix-in static. "helix/static.scm"))
(require-builtin helix/core/text)

(struct CtxNode (node named-nodes) #:mutable)
(struct NodeStyle (node style))
(define curr-txt (string->rope ""))

(define (list-coerce l)
  (if (list? l)
      l
      '()))

(define (insert-node-sorted node lst)
  (define (start n)
    (tsnode-start-byte (NodeStyle-node n)))
  (cond
    [(empty? lst) (list node)]
    [(> (tsnode-start-byte (NodeStyle-node (first lst))) (tsnode-start-byte (NodeStyle-node node)))
     (cons node lst)]
    [else (cons (first lst) (insert-node-sorted node (rest lst)))]))

(define MAX-CACHED-QUERIES 10)
(define cached-queries '())

(define (match-node-to-context node contexts s)
  (define node-start (tsnode-start-byte node))

  (define (bsearch left right)
    (if (>= left right)
        left
        (let* ([mid (quotient (+ left right) 2)]
               [mid-node (list-ref contexts mid)])
          (if (< node-start (tsnode-start-byte (CtxNode-node mid-node)))
              (bsearch (+ mid 1) right)
              (bsearch left mid)))))

  (define idx (bsearch 0 (length contexts)))

  (define ctx-node (list-ref contexts idx))
  (set-CtxNode-named-nodes! ctx-node
                            (insert-node-sorted (NodeStyle node s) (CtxNode-named-nodes ctx-node))))

(define valid-captures
  (hash "name"
        (style)
        "function.macro"
        (theme-scope-ref "function.macro")
        "function"
        (theme-scope-ref "function")
        "constant"
        (theme-scope-ref "constant")
        "type"
        (theme-scope-ref "type")
        "type.primitive"
        (theme-scope-ref "type.builtin")
        "keyword"
        (theme-scope-ref "keyword")
        "keyword.function"
        (theme-scope-ref "keyword.function")
        "keyword.directive"
        (theme-scope-ref "keyword.directive")
        "keyword.control"
        (theme-scope-ref "keyword.control")
        "namespace"
        (theme-scope-ref "namespace")
        "heading"
        (theme-scope-ref "markup.heading")))

(define (enumerate-captures match)
  (if (TSMatch? match)
      (let ([contexts (reverse (map (lambda (x) (CtxNode x '()))
                                    (list-coerce (tsmatch-capture match "context"))))])
        (for-each (lambda (elem)
                    (cond
                      [(hash-contains? valid-captures elem)
                       (for-each (lambda (y)
                                   (match-node-to-context y contexts (hash-get valid-captures elem)))
                                 (tsmatch-capture match elem))]))
                  (tsmatch-captures match))
        contexts)
      #f))

(define query-path
  (if (current-module)
      (string-append (parent-name (current-module)) "/queries/")
      (string-append (static.get-helix-cwd) "/queries/")))

(define (get-pos pred lst)
  (letrec ([loop (lambda (pred vec idx)
                   (cond
                     [(>= idx (length vec)) #f]
                     [(pred (list-ref vec idx)) idx]
                     [else (loop pred vec (add1 idx))]))])
    (loop pred lst 0)))

(define (get-query lang)
  (let* ([filepath (string-append query-path lang ".tsq")]
         [exists (path-exists? filepath)]
         [pos (get-pos (lambda (x) (string=? (first x) lang)) cached-queries)])
    (cond
      [(not exists) #f]
      [(number? pos) (let ([elem (list-ref cached-queries pos)]) (second elem))]
      [else
       (let ([query (string->tsquery lang (read-port-to-string (open-input-file filepath)))])
         (set! cached-queries (take (cons (list lang query) cached-queries) MAX-CACHED-QUERIES))
         query)])))

(define (get-current-doc-id)
  (editor->doc-id (editor-focus)))

(define query-loader (tsquery-loader get-query))

(define (get-contexts doc-id)
  (let ([match (query-document query-loader doc-id)])
    (if (TSMatch? match)
        (begin
          (define c #f)

          (set! c (enumerate-captures match))
          c)
        '())))

(define (tsnode-text-slice node text)
  (let ([start (tsnode-start-byte node)]
        [end (tsnode-end-byte node)])
    (if (< end (rope-len-bytes text))
        (string-append (rope->string (rope->byte-slice text start end)) " ")
        "")))

(define cached-match '())

(define (refresh-context-query! doc-id)
  (set! curr-txt (editor->text doc-id))
  (set! cached-match (get-contexts doc-id)))

(define queued (box #f))
(define cached-path '())

(define (get-path match)
  (cond
    [(empty? match) '()]
    [(unbox queued) cached-path]
    [else
     (let* ([text curr-txt]
            [pos (rope-char->byte text (min (cursor-position) (rope-len-chars text)))])
       (map (lambda (x)
              (map (lambda (y) (span (tsnode-text-slice (NodeStyle-node y) text) (NodeStyle-style y)))
                   (CtxNode-named-nodes x)))
            (filter (lambda (y)
                      ;; end byte is exclusive: abutting nodes otherwise both
                      ;; match a cursor sitting exactly on the boundary
                      (and (< pos (tsnode-end-byte (CtxNode-node y)))
                           (>= pos (tsnode-start-byte (CtxNode-node y)))))
                    match)))]))

; (define debounce-delay-ms 200)

(define (debounce debounce-delay-ms func)
  ;; If we haven't queued it, queue it up
  (unless (unbox queued)
    (set-box! queued #t)
    (enqueue-thread-local-callback-with-delay debounce-delay-ms
                                              (lambda ()
                                                (func)
                                                (set-box! queued #f)))))

(define (sep lst s)
  (cond
    [(empty? lst) lst]
    [(empty? (rest lst)) lst]
    [else (cons (first lst) (cons s (sep (rest lst) s)))]))

(define context-status-element
  (status-element (lambda (view-id focused)
                    (if focused
                        (foldl (lambda (x acc) (append (cons (span "> " (style)) x) acc))
                               '()
                               (begin
                                 (set! cached-path (get-path cached-match))
                                 cached-path))
                        '()))))

(define (context-enable side)
  (register-hook 'document-changed
                 (lambda (_ _)
                   (debounce 200 (lambda () (refresh-context-query! (get-current-doc-id))))))
  (register-hook 'document-focus-lost
                 (lambda (_) (debounce 50 (lambda () (refresh-context-query! (get-current-doc-id))))))
  (register-hook 'document-closed
                 (lambda (_)

                   (debounce 50 (lambda () (refresh-context-query! (get-current-doc-id))))))
  (register-hook 'post-command
                 (lambda (cmd)
                   (if (string-contains? cmd "quit")
                       (debounce 200 (lambda () (refresh-context-query! (get-current-doc-id)))))))

  (push-status-element! side context-status-element)
  (debounce 200 (lambda () (refresh-context-query! (get-current-doc-id)))))

(provide context-status-element
         context-enable)
