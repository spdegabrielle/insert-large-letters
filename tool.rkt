#lang typed/racket

(require drracket/tool
         ;racket/gui
        ; 
        typed/mred/mred
         typed/framework/framework
         racket/class
         string-constants
         )

(require/typed framework
               [frame:focus-table-mixin
                (-> Dialog% Dialog%)])

(define-type Bitmap-Message%
  (Class (init [parent (Instance Horizontal-Panel%)])
         [set-bm ((U (Instance Bitmap%) #f) -> Void)]))

(require/typed "bitmap-message.rkt"
               [bitmap-message% Bitmap-Message%])



(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1) (void))
    (define (phase2) (void))
    
    
    (define frame-mixin 
      (mixin (drracket:unit:frame<%>) () 
        
        (inherit get-current-tab get-menu-bar get-button-panel
                 register-toolbar-button
                 get-edit-target-object
                 get-definitions-text)
        
        (define freq-canvas #f)
        (define freq-panel-parent #f)
        
        ;; make-large-letters-dialog : string char top-level-window<%> -> void
        (define (make-large-letters-dialog comment-prefix comment-character parent)
          (define dlg (new dialog% 
                           [parent parent] 
                           [width 700]
                           [label (string-constant large-semicolon-letters)]))
          (define text-field 
            (new text-field% 
                 [parent dlg] 
                 [label (string-constant text-to-insert)]
                 [callback (λ: ([x : Any] [y : Any]) (update-txt (send (assert text-field defined?) get-value)))]))
          
          (define info-bar (new horizontal-panel%
                                [parent dlg]
                                [stretchable-height #f]))
          (define font-choice
            (let ([tmp-bdc (make-object bitmap-dc% (make-object bitmap% 1 1 #f))])
              (new choice%
                   [label (string-constant fonts)]
                   [parent info-bar]
                   [choices (map (λ: ((x : String)) (format "~a~a" x (get-w-size tmp-bdc x))) 
                                 (get-face-list))]
                   [callback
                    (λ: ([x : Any] [y : Any])
                      (let ([old (preferences:get 'drracket:large-letters-font)]
                            [choice (send (assert font-choice defined?) get-selection)])
                        (when choice
                          (preferences:set 'drracket:large-letters-font
                                           (cons (list-ref (get-face-list)
                                                           choice)
                                                 (if old
                                                     (cdr old)
                                                     (send (get-default-font) get-point-size))))
                          (update-txt (send (assert text-field defined?) get-value)))))])))
          
          (define count (new message% [label (format columns-string 1000)] [parent info-bar]))
          (define pane1 (new horizontal-pane% (parent info-bar)))
          
          (define dark-msg (new bitmap-message% [parent info-bar]))
          (define pane2 (new horizontal-pane% (parent info-bar)))
          
          (define txt (new racket:text%))
          (define ec (new editor-canvas% [parent dlg] [editor txt]))
          (define button-panel (new horizontal-panel%
                                    [parent dlg]
                                    [stretchable-height #f]
                                    [alignment '(right center)]))
          (define-values (ok cancel)
            (gui-utils:ok/cancel-buttons button-panel
                                         (λ (x y) (set! ok #t) (send dlg show #f))
                                         (λ (x y) (send dlg show #f))))
          (define (update-txt str)
            (send txt begin-edit-sequence)
            (send txt lock #f)
            (send txt delete 0 (send txt last-position))
            (let ([bm (render-large-letters comment-prefix comment-character (get-chosen-font) str txt)])
              (send ec set-line-count (+ 1 (send txt last-paragraph)))
              (send txt lock #t)
              (send txt end-edit-sequence)
              (send count set-label (format columns-string (get-max-line-width txt)))
              (send dark-msg set-bm bm)))
          
          
          ;; CHANGE - get-face can return #f
          (let ([face (send (get-chosen-font) get-face)])
            (when face
              (let loop ([i 0]
                         [faces (get-face-list)])
                (cond
                  [(null? faces) (void)]
                  [else (cond
                          [(equal? face (car faces))
                           (send (assert font-choice defined?) set-selection i)]
                          [else
                           (loop (+ i 1) (cdr faces))])]))))
          
          (send txt auto-wrap #f)
          (update-txt " ")
          (send (assert text-field defined?) focus)
          (send dlg show #t)
          (and ok (send (assert text-field defined?) get-value)))
        ;; END make-large-letters-dialog           
        
        (define (insert-large-letters comment-prefix comment-character edit parent)
          (let ([str (make-large-letters-dialog comment-prefix comment-character #f)])
            (when (and str
                       (not (equal? str "")))
              (render-large-letters comment-prefix comment-character (get-chosen-font) str edit)
              (void))))
        
        (define (get-default-font)
          (send (send (editor:get-standard-style-list)
                      find-named-style
                      "Standard")
                get-font))
        
        
        (define (get-chosen-font)
          (let ([pref-val (preferences:get 'drracket:large-letters-font)])
            (cond
              [pref-val
               (let ([candidate (send the-font-list find-or-create-font (cdr pref-val) (car pref-val) 'default 'normal 'normal)])
                 (if (equal? (send candidate get-face) (car pref-val))
                     candidate
                     (get-default-font)))]
              [else
               (get-default-font)])))
        
        (define columns-string "~a columns")
                
        
        (define (get-w-size dc face-name)
          (let ([font (send the-font-list find-or-create-font 24 face-name 'default 'normal 'normal)])
            (let-values ([(w h a d) (send dc get-text-extent "w" font)])
              (format " (~a)" (floor (inexact->exact w))))))
        
        (define (get-max-line-width txt)
          (let loop ([i (+ (send txt last-paragraph) 1)]
                     [m  0])
            (cond
              [(zero? i) m]
              [else (loop (sub1 i)
                          (max m (- (send txt paragraph-end-position (- i 1))
                                    (send txt paragraph-start-position (- i 1)))))])))
        ; render-large-letters
        (define (render-large-letters comment-prefix comment-character the-font str edit)
          (define bdc (make-object bitmap-dc% (make-object bitmap% 1 1 #t)))
          (define-values (tw raw-th td ta) (send bdc get-text-extent str the-font))
          (define th (let-values ([(_1 h _2 _3) (send bdc get-text-extent "X" the-font)])
                       (max raw-th h)))
          (define tmp-color (make-object color%))
          
          (define (get-char x y)
            (send bdc get-pixel x y tmp-color)
            (let ([red (send tmp-color red)])
              (if (= red 0)
                  comment-character
                  #\space)))  
          (define bitmap
            (make-object bitmap% 
              (max 1 (inexact->exact tw))
              (inexact->exact th)
              #t))
          
          (define (fetch-line y)
            (let loop ([x (send bitmap get-width)]
                       [chars null])
              (cond
                [(zero? x) (apply string chars)]
                [else (loop (- x 1) (cons (get-char (- x 1) y) chars))])))
          
          (send bdc set-bitmap bitmap)
          (send bdc clear)
          (send bdc set-font the-font)
          (send bdc draw-text str 0 0)
          
          (send edit begin-edit-sequence)
          (let ([start (send edit get-start-position)]
                [end (send edit get-end-position)])
            (send edit delete start end)
            (send edit insert "\n" start start)
            (let loop ([y (send bitmap get-height)])
              (unless (zero? y)
                (send edit insert (fetch-line (- y 1)) start start)
                (send edit insert comment-prefix start start)
                (send edit insert "\n" start start)
                (loop (- y 1)))))
          (send edit end-edit-sequence)
          (send bdc set-bitmap #f)
          bitmap) ;render-large-letters
                
        (define (insert-large-semicolon-letters)
          (let ([edit (get-edit-target-object)])
            (when edit
              (define language-settings (send definitions-text get-next-settings))
              (define-values(comment-prefix comment-character)
                (if language-settings
                    (send (drracket:language-configuration:language-settings-language
                           language-settings)
                          get-comment-character)
                    (values ";" #\;)))
              (insert-large-letters comment-prefix comment-character edit this))))
        
       ; do I need to register a capability and a capability-menu-item? 
       ;(register-capability-menu-item 'drscheme:special:insert-large-letters insert-menu)         
        
        (super-new)
        
        (define definitions-text (send this get-definitions-text))
        
        (new menu-item%  
             [label "ILL" #;(string-constant insert-large-letters...)]
             [parent (send this get-insert-menu)]
             [callback (λ (x y) (insert-large-semicolon-letters))])
        
        ))
    
    (drracket:get/extend:extend-unit-frame frame-mixin)
    ))
