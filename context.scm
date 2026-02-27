(require "helix/treesitter.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require "helix/components.scm")
(require (prefix-in static. "helix/static.scm"))
(require-builtin helix/core/text)

(struct ConMatch (surrounding nodes))

;; simple stack of cached queries
(define MAX-CACHED-QUERIES 10)
(define cached-queries '())

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

(define (list-coerce l)
  (if (list? l)
      l
      '()))

(define (get-contexts doc-id)
  (let ([match (query-document query-loader doc-id)])
    (if (TSMatch? match)
        (ConMatch (list-coerce (tsmatch-capture match "context"))
                  (list-coerce (tsmatch-capture match "context.name")))
        #f)))

(define (tsnode-text-slice node text)
  (let ([start (tsnode-start-byte node)]
        [end (tsnode-end-byte node)])
    (if (< end (rope-len-bytes text))
        (rope->string (rope->byte-slice text start end))
        "")))

(define cached-match #f)

(define (refresh-context-query! doc-id)
  (set! cached-match (get-contexts doc-id)))

(define (get-path match doc-id)
  (if (ConMatch? match)
      (let* ([text (editor->text doc-id)]
             [pos (rope-char->byte text (cursor-position))])
        (map (lambda (x) (tsnode-text-slice (second x) text))
             (filter
              (lambda (z)
                (and (<= pos (tsnode-end-byte (first z))) (>= pos (tsnode-start-byte (first z)))))
              (transduce (ConMatch-surrounding match) (zipping (ConMatch-nodes match)) (into-list)))))
      '()))

(define (sep lst s)
  (cond
    [(empty? lst) lst]
    [(empty? (rest lst)) lst]
    [else (cons (first lst) (cons s (sep (rest lst) s)))]))

(define context-status-element
  (status-element (lambda (view-id focused)
                    (if focused
                        (sep (map (lambda (x) (span x (style)))
                                  (get-path cached-match (editor->doc-id view-id)))
                             (span " : " (theme-scope-ref "keyword")))
                        '()))))

(define (context-enable side)
  (register-hook 'document-changed (lambda (doc-id _) (refresh-context-query! doc-id)))
  (register-hook 'document-focus-lost (lambda (_) (refresh-context-query! (get-current-doc-id))))
  (push-status-element! side context-status-element))

(provide context-status-element
         context-enable)
