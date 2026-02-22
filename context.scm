(require "helix/treesitter.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/configuration.scm")
(require "helix/components.scm")
(require-builtin helix/core/text)

(struct ConMatch (tree surrouding nodes))

;; simple stack of cached queries
(define MAX-CACHED-QUERIES 10)
(define cached-queries '())

(define cached-tree-start -1)

(define (get-pos pred lst)
  (letrec ([loop (lambda (pred vec idx)
                   (cond
                     [(>= idx (length vec)) #f]
                     [(pred (list-ref vec idx)) idx]
                     [else (loop pred vec (add1 idx))]))])
    (loop pred lst 0)))

(define (get-query lang)
  (let* ([filepath (string-append "/queries/" lang ".tsq")]
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
  (let* ([focus (editor-focus)])
    (if focus
        (editor->doc-id focus)
        #f)))

(define query-loader (tsquery-loader get-query))

(define (get-trees-at-cursor doc-id)
  (let* ([text (editor->text doc-id)]
         [pos (rope-char->byte text (cursor-position))])
    (document->layers-byte-range doc-id pos pos)))

(define (get-contexts doc-id matches)
  (letrec ([trees (get-trees-at-cursor doc-id)]
           [loop
            (lambda (lst acc)
              (cond
                [(empty? lst) acc]
                [(member (first lst) (map ConMatch-tree matches))
                 (let ([idx (get-pos (lambda (x) (equal? (ConMatch-tree x) (first lst))) matches)])
                   (set-status! "skibidi")
                   (loop (rest lst) (cons (list-ref matches idx) acc)))]
                ;; root tree doesn't have query? don't bother.
                [(TSQuery? (get-query (tstree->language (first lst))))
                 (loop (rest lst)
                       (let* ([tree (first lst)]
                              [root (tstree->root tree)]
                              [match (query-document-byte-range
                                      query-loader
                                      doc-id
                                      (tsnode-start-byte root)
                                      ;; skibidi finess
                                      (begin
                                        ; (set-status! (number->string (sub1 (tsnode-end-byte root))))
                                        (sub1 (tsnode-end-byte root))))]
                              [named (filter (lambda (x) (equal? (tsnode->tstree x) tree))
                                             (tsmatch-capture match "context"))]
                              [surrounding (filter (lambda (x) (equal? (tsnode->tstree x) tree))
                                                   (tsmatch-capture match "context.name"))])
                         (loop (rest lst) (cons (ConMatch tree named surrounding) acc))))]))])
    (loop trees '())))

(define (tsnode-text-slice node text)
  (let ([start (tsnode-start-byte node)]
        [end (tsnode-end-byte node)])
    (rope->byte-slice text start end)))

(define cached-match '())
(define path "")

(define (refresh-context-query!)
  (let* ([doc-id (get-current-doc-id)])
    (if doc-id
        (set! cached-match (get-contexts doc-id cached-match)))))

(define (get-path match text pos)
  (if (and (not (empty? match)))
      (foldr (lambda (x acc)
               (append (map (lambda (y) (rope->string (tsnode-text-slice (second y) text)))
                            (filter (lambda (z)
                                      ((let ([surrounding (first z)])
                                         (and (<= pos (tsnode-end-byte surrounding))
                                              (>= pos (tsnode-start-byte surrounding))))))
                                    (transduce (ConMatch-surrouding x)
                                               (zipping (ConMatch-nodes x))
                                               (into-list))))
                       acc))
             '()
             match)
      '()))

(define (set-path! doc-id)
  (set! path
        (string-join (let* ([text (editor->text doc-id)]
                            [pos (rope-char->byte text (cursor-position))]
                            [tree (document->tree-byte-range doc-id pos pos)])
                       (when (and (TSTree? tree)
                                  (not (= (tsnode-start-byte (tstree->root tree)) cached-tree-start)))
                         (set! cached-tree-start (tsnode-start-byte (tstree->root tree)))
                         (refresh-context-query!))
                       (get-path cached-match text pos))
                     " > ")))

(define context-status-element
  (status-element (lambda (doc-id focused)
                    (list (if focused
                              (begin
                                (set-path! doc-id)
                                (string-append " " path " "))
                              "")
                          (style-with-bold (style))))))

(define (context-enable side)
  (register-hook! "document-changed" (lambda (_ _) (refresh-context-query!)))
  (register-hook! "document-focus-lost" (lambda (_) (refresh-context-query!)))
  (push-status-element! side context-status-element))

(provide context-status-element
         context-enable)
