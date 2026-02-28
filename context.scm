(require "helix/treesitter.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require "helix/components.scm")
(require (prefix-in static. "helix/static.scm"))
(require-builtin helix/core/text)

(struct CtxNode (node named-nodes) #:mutable)
(struct NodeStyle (node style))

(define (list-coerce l)
  (if (list? l)
      l
      '()))

(define (insert-node-sorted node lst)
  (define (start n)
    (tsnode-start-byte (NodeStyle-node n)))

  (define (binsert left right)
    (if (>= left right)
        (append (take lst left) (list node) (drop lst left))
        (let* ([mid (quotient (+ left right) 2)]
               [mid-node (list-ref lst mid)])
          (if (< (start node) (start mid-node))
              (binsert left mid)
              (binsert (+ mid 1) right)))))

  (binsert 0 (length lst)))
(define MAX-CACHED-QUERIES 10)
(define cached-queries '())

(define (match-node-to-context node contexts s)
  (cond
    [(empty? contexts) #f]
    [(tsnode-within-byte-range? node
                                (tsnode-start-byte (CtxNode-node (first contexts)))
                                (tsnode-end-byte (CtxNode-node (first contexts))))

     (begin
       (set-CtxNode-named-nodes! (first contexts)
                                 (insert-node-sorted (NodeStyle node s)
                                                     (CtxNode-named-nodes (first contexts))))
       #t)]
    [else (match-node-to-context node (rest contexts) s)]))

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
        (for-each (lambda (x)
                    (cond
                      [(hash-contains? valid-captures x)
                       (for-each (lambda (y)
                                   (match-node-to-context y contexts (hash-get valid-captures x)))
                                 (tsmatch-capture match x))]))
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
        (enumerate-captures match)
        '())))

(define (tsnode-text-slice node text)
  (let ([start (tsnode-start-byte node)]
        [end (tsnode-end-byte node)])
    (if (< end (rope-len-bytes text))
        (string-append (rope->string (rope->byte-slice text start end)) " ")
        "")))

(define cached-match '())

(define (refresh-context-query! doc-id)
  (set! cached-match (get-contexts doc-id)))

(define (get-path match doc-id)
  (if (empty? match)
      '()
      (let* ([text (editor->text doc-id)]
             [pos (rope-char->byte text (cursor-position))])
        (map (lambda (x)
               (map (lambda (y)
                      (span (tsnode-text-slice (NodeStyle-node y) text) (NodeStyle-style y)))
                    (CtxNode-named-nodes x)))
             (filter (lambda (y)
                       (and (<= pos (tsnode-end-byte (CtxNode-node y)))
                            (>= pos (tsnode-start-byte (CtxNode-node y)))))
                     match)))))

(define (sep lst s)
  (cond
    [(empty? lst) lst]
    [(empty? (rest lst)) lst]
    [else (cons (first lst) (cons s (sep (rest lst) s)))]))

(define context-status-element
  (status-element (lambda (view-id focused)
                    (if focused
                        (cons (span " " (style))
                              (foldl (lambda (x acc)
                                       (append x
                                               (if (empty? acc)
                                                   acc
                                                   (cons (span ": " (style)) acc))))
                                     '()
                                     (get-path cached-match (editor->doc-id view-id))))
                        '()))))

(define (context-enable side)
  (register-hook 'document-changed (lambda (doc-id _) (refresh-context-query! doc-id)))
  (register-hook 'document-focus-lost (lambda (_) (refresh-context-query! (get-current-doc-id))))
  (push-status-element! side context-status-element))

(provide context-status-element
         context-enable)
