(require "helix/treesitter.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require "helix/components.scm")
(require (prefix-in static. "helix/static.scm"))
(require-builtin helix/core/text)

(struct ConMatch (tree surrounding nodes))

;; simple stack of cached queries
(define MAX-CACHED-QUERIES 10)
(define cached-queries '())

(define cached-tree-start -1)

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

(define (tree-eq? a b)
  (let ([a-root (tstree->root a)]
        [b-root (tstree->root b)])
    (and (= (tsnode-end-byte a-root) (tsnode-end-byte b-root))
         (= (tsnode-start-byte a-root) (tsnode-start-byte b-root))
         (string=? (tstree->language a) (tstree->language b)))))

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

(define (get-trees-at-cursor doc-id)
  (let* ([text (editor->text doc-id)]
         [pos (rope-char->byte text (cursor-position))])
    (document->layers-byte-range doc-id pos pos)))

(define (list-coerce l)
  (if (list? l)
      l
      '()))

(define (get-contexts doc-id matches)
  (letrec
      ([trees (get-trees-at-cursor doc-id)]
       [loop
        (lambda (lst acc)
          (cond
            [(bool? lst) acc]
            [(empty? lst) acc]
            [else
             (let ([idx (begin
                          (get-pos (lambda (x) (tree-eq? (ConMatch-tree x) (first lst))) matches))])
               (cond
                 [(number? idx) (loop (rest lst) (cons (list-ref matches idx) acc))]
                 ;; root tree doesn't have query? don't bother.
                 [(TSQuery? (get-query (tstree->language (first lst))))
                  (begin
                    (loop
                     (rest lst)
                     (let* ([tree (first lst)]
                            [root (tstree->root tree)]
                            [match (query-document-byte-range query-loader
                                                              doc-id
                                                              (tsnode-start-byte root)
                                                              ;; skibidi finess
                                                              (sub1 (tsnode-end-byte root)))])
                       (if (TSMatch? match)
                           (let ([named (filter (lambda (x) (tree-eq? (tsnode->tstree x) tree))
                                                (list-coerce (tsmatch-capture match "context.name")))]
                                 [surrounding (filter (lambda (x) (tree-eq? (tsnode->tstree x) tree))
                                                      (list-coerce (tsmatch-capture match
                                                                                    "context")))])
                             (cons (ConMatch tree surrounding named) acc))
                           acc))))]
                 [else (loop (rest lst) acc)]))]))])
    (loop trees '())))

(define (tsnode-text-slice node text)
  (let ([start (tsnode-start-byte node)]
        [end (tsnode-end-byte node)])
    (if (< end (rope-len-bytes text))
        (rope->string (rope->byte-slice text start end))
        "")))

(define cached-match '())
(define path "")

(define (refresh-context-query! full doc-id)
  (set! cached-match
        (get-contexts doc-id
                      (if full
                          '()
                          cached-match))))

(define (get-path match text pos)
  (if (empty? match)
      '()
      (foldr (lambda (x acc)
               (append acc
                       (map (lambda (y) (tsnode-text-slice (second y) text))
                            (filter (lambda (z)
                                      (let ([surrounding (first z)])
                                        (and (<= pos (tsnode-end-byte surrounding))
                                             (>= pos (tsnode-start-byte surrounding)))))
                                    (transduce (ConMatch-surrounding x)
                                               (zipping (ConMatch-nodes x))
                                               (into-list))))))
             '()
             match)))

(define (set-path! doc-id)
  (set! path
        (string-join (let* ([text (editor->text doc-id)]
                            [pos (rope-char->byte text (cursor-position))]
                            [tree (document->tree-byte-range doc-id pos pos)])
                       (if (and (TSTree? tree)
                                (not (= (tsnode-start-byte (tstree->root tree)) cached-tree-start)))
                           (begin
                             (set! cached-tree-start (tsnode-start-byte (tstree->root tree)))
                             (refresh-context-query! #f doc-id)))
                       (get-path cached-match text pos))
                     " > ")))

(define context-status-element
  (status-element (lambda (view-id focused)
                    (list (span (if focused
                                    (begin
                                      (set-path! (editor->doc-id view-id))
                                      (string-append " " path " "))
                                    "")
                                (style-with-bold (style)))))))

(define (context-enable side)
  (register-hook 'document-changed (lambda (doc-id _) (refresh-context-query! #f doc-id)))
  (register-hook 'document-focus-lost (lambda (_) (refresh-context-query! #t (get-current-doc-id))))
  (push-status-element! side context-status-element))

(provide context-status-element
         context-enable)
