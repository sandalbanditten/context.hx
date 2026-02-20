(require "helix/treesitter.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require "helix/components.scm")
(require-builtin helix/core/text)

;; simple stack of cached queries
(define MAX-CACHED-QUERIES 10)
(define cached-queries '())

(define (get-pos pred lst)
  (letrec ([loop (lambda (pred vec idx)
                   (cond
                     [(>= idx (length vec)) #f]
                     [(pred (list-ref vec idx)) idx]
                     [else (loop pred vec (add1 idx))]))])
    (loop pred lst 0)))

(define (get-query lang)
  (let* ([filepath (string-append (parent-name (current-module)) "/queries/" lang ".tsq")]
         [exists (path-exists? filepath)]
         [pos (get-pos (lambda (x) (string=? (first x) lang)) cached-queries)])
    (cond
      [(not exists) #f]
      [pos (let ([elem (list-ref cached-queries pos)]) (second elem))]
      [else
       (let ([query (string->tsquery lang (read-port-to-string (open-input-file filepath)))])
         (set! cached-queries (take (cons (list lang query) cached-queries) MAX-CACHED-QUERIES))
         query)])))

(define (get-current-doc-id)
  (let* ([focus (editor-focus)])
    (if focus
        (editor->doc-id focus)
        #f)))

(define query-loader (tsquery-loader get-query))

(define (get-contexts doc-id)
  (let ([text (editor->text doc-id)]) (query-document query-loader doc-id)))

(define (captures)
  (let ([doc-id (get-current-doc-id)])
    (if (doc-id)
        (tsmatch-capture (get-contexts doc-id) "context")
        '())))

(define (tsnode-ancestor? node t)
  (let ([r-start (tsnode-start-byte node)]
        [r-end (tsnode-end-byte node)]
        [start (tsnode-start-byte t)]
        [end (tsnode-end-byte t)])
    (and (<= r-start start) (>= r-end end))))

(define (tsnode-text-slice node text)
  (let ([start (tsnode-start-byte node)]
        [end (tsnode-end-byte node)])
    (rope->byte-slice text start end)))

(define cached-match #f)
(define path "")

(define (refresh-context-query!)
  (let ([doc-id (get-current-doc-id)])
    (if doc-id
        (set! cached-match (get-contexts doc-id)))))

(define (get-path match doc-id tree)
  (if (and doc-id (TSMatch? match) (TSTree? tree) (not (empty? (tsmatch-captures match))))
      (let* ([root (tstree->root tree)]
             [text (editor->text doc-id)]
             [cursor-pos (rope-char->byte text (cursor-position))]
             [start (tsnode-descendant-byte-range root cursor-pos cursor-pos)]
             [surrounding (tsmatch-capture match "context")]
             [named (tsmatch-capture match "context.name")])
        (foldr (lambda (x acc)
                 (let ([surrounding (first x)]
                       [named (last x)])
                   (if (and (tsnode-ancestor? surrounding start) (not (equal? surrounding root)))
                       (cons (rope->string (tsnode-text-slice named text)) acc)
                       acc)))
               '()
               (transduce surrounding (zipping named) (into-list))))
      '()))

(define (set-path! doc-id)
  (set! path (string-join (get-path cached-match doc-id (document->tree doc-id)) " -> ")))

(define context-status-element
  (status-element (lambda (doc-id focused)
                    (list (if focused
                              (begin
                                (set-path! doc-id)
                                (string-append " " path " "))
                              "")
                          (style-with-bold (style))))))

(define (context-enable side)
  (register-hook! "post-command"
                  (lambda (cmd)
                    (unless (and (not (string-contains? cmd "quit"))
                                 (not (string-contains? cmd "move"))
                                 (not (string-contains? cmd "write")))
                      (refresh-context-query!))))
  (push-status-element! side context-status-element))

(provide context-status-element
         context-enable)
