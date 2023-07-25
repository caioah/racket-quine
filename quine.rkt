#lang racket
(require racket/gui (only-in fmt program-format) racket/exn compiler/embed
         (only-in framework racket:text% racket:get-keymap keymap:get-editor))
(define this-code "")
(define-syntax-rule (quine ((var val) ...) body ...)
  (let* ((var val) ...)
    (set! this-code (program-format (format "~s" '(let* ((var val) ...) body ...))))
    body ...))
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (try-eval-string fail x [n ns])
  (with-handlers ([exn:fail? (λ (exn) (fail (exn->string exn)))])
    (eval (read (open-input-string x)) n)))
(define (msgbox-error msg)
  (message-box "Error" msg #f '(stop ok)))
(define (add-editor-functions km)
  (send* km
    (chain-to-keymap (keymap:get-editor) #f)
    (add-function "execute"
                  (λ (txt ev)
                    (let ([out (open-output-string)]
                          [s (send txt get-start-position)])
                      ((λ (e) (when (= e s) (set! s (send txt get-backward-sexp e)))
                         (send txt kill 0 s e)) (send txt get-end-position))
                      (parameterize ([current-output-port out])
                        (try-eval-string msgbox-error (send the-clipboard get-clipboard-string 0)))
                      (send txt insert (pretty-format (get-output-string out) #:mode 'display) s))))
    (add-function "execute-replace"
                  (λ (txt ev) (let ([s (send txt get-start-position)])
                                ((λ (e) (when (= e s) (set! s (send txt get-backward-sexp e)))
                                   (send txt kill 0 s e)) (send txt get-end-position))
                                (send txt insert (pretty-format (try-eval-string msgbox-error (send the-clipboard get-clipboard-string 0)) #:mode 'display) s))))
    (add-function "autoindent"
                  (λ (txt ev) (let ([clip (send the-clipboard get-clipboard-string 0)])
                    (send txt kill 0 0 (send txt last-position))
                    (send txt insert (program-format (send the-clipboard get-clipboard-string 0)))
                    (send the-clipboard set-clipboard-string clip 0))))
    (add-function "paste-clipboard"
                  (λ (txt ev) (send txt insert (send the-clipboard get-clipboard-string 0))))
    (map-function "c:i" "autoindent")
    (map-function "c:~s:v" "paste-clipboard")
    (map-function "~c:s:enter" "execute")
    (map-function "c:~s:enter" "execute-replace")))

(quine 
 ([frm (new frame% [label "Quine"] [min-width 800] [min-height 500])]
  [run (λ (str) (set! this-code str) (send frm show #f)
         (try-eval-string (λ (err-msg) (send frm show #t) (msgbox-error err-msg)) str))]
  [txt (new racket:text% [auto-wrap #t])]
  [edt (new editor-canvas% [parent frm] [editor txt])]
  [btn (new button% [label "Run"] [parent frm] 
            [callback (λ (btn ev) (run (send txt get-text)))])])
 (add-editor-functions (send txt get-keymap))
 (send* txt
   (insert this-code)
   (set-max-undo-history 512))
 (send frm show #t))