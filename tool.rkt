#lang racket/base

(require racket/contract
         racket/unit
         racket/class
         drracket/tool-lib
         drracket/tool
         racket/gui/base
         ;typed/racket
         ;typed/mred/mred
         string-constants
         "insert-large-letters.rkt"
         )

;(define columns-string "~a columns")

;(make-large-letters-dialog ";" #\; #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        ;(: make-large-letters-dialog (String Char Any -> (Option String)))
        
        ;; END make-large-letters-dialog           
        
        
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
         ;render-large-letters
        
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
             [label (string-constant insert-large-letters...)]
             [parent (send this get-insert-menu)]
             [callback (λ (x y) (insert-large-semicolon-letters))])
        
        ))
    
    (drracket:get/extend:extend-unit-frame frame-mixin)
    ))
