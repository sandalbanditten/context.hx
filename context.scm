(require "helix/treesitter.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require "helix/components.scm")
(require-builtin helix/core/text)

(define (get-current-doc-id)
  (let* ([focus (editor-focus)]) (editor->doc-id focus)))

(define scm-query (read-port-to-string (open-input-file "scheme.tsq")))

(define query-loader
  (tsquery-loader (lambda (lang)
                    (cond
                      [(string=? lang "scheme") (string->tsquery lang scm-query)]
                      [else #f]))))

(define (get-contexts)
  (let ([text (editor->text (get-current-doc-id))])
    (query-document query-loader (get-current-doc-id))))

(define (captures)
  (tsmatch-capture (get-contexts) "context"))

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

(define INSERT (string->editor-mode "insert"))

(define cached-match #f)
(define path "")

(define (enable-hooks)
  (register-hook! "on-mode-switch"
                  (lambda (ev)
                    (cond
                      [(equal? (mode-switch-old ev) INSERT)
                       (begin
                         (refresh-context-query!))])))
  (register-hook! "post-command"
                  (lambda (_)
                    (begin
                      (refresh-context-query!)))))

(define (refresh-context-query!)
  (set! cached-match (get-contexts)))

(define (get-path match)
  (if (and (TSMatch? match))
      (let* ([doc-id (get-current-doc-id)]
             [tree (document->tree doc-id)]
             [root (tstree->root tree)]
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

; (define (get-path match)
;   (and (TSMatch? match)
;        (let* ([doc-id (get-current-doc-id)]
;               [tree (document->tree doc-id)]
;               [root (and tree (tstree->root tree))]
;               [text (and doc-id (editor->text doc-id))]
;               [cursor-pos (and text (rope-char->byte text (cursor-position)))]
;               [start (and root cursor-pos (tsnode-descendant-byte-range root cursor-pos cursor-pos))]
;               [surrounding (and start (tsmatch-capture match "context"))]
;               [named (and surrounding (tsmatch-capture match "context.name"))])
;          (and root
;               text
;               start
;               surrounding
;               named
;               (foldl (lambda (x acc)
;                        (let ([surrounding (first x)]
;                              [named (last x)])
;                          (if (and (tsnode-ancestor? surrounding start)
;                                   (not (equal? surrounding root)))
;                              (cons (rope->string (tsnode-text-slice named text)) acc)
;                              acc)))
;                      '()
;                      (reverse (transduce surrounding (zipping named) (into-list)))))))
;   '())

(define (set-path!)
  (set! path (string-join (get-path cached-match) " -> ")))

(statusline #:center (list (status-element (lambda ()
                                             (list (begin
                                                     (set-path!)
                                                     path)
                                                   (style-with-bold (style)))))))
