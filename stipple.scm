;; -*- mode: Gimp; -*-
(define (script-fu-stipple
         img
         drawable
         scale-x
         scale-y
         pre-posterize
         poster-levels
         encode-dots
         merge-flag)


  
  (gimp-image-undo-group-start img)
  ;; (if (not (= RGB (car (gimp-image-base-type img))))
  ;;     (gimp-image-convert-rgb img))   


  ( let* (
          (drawable-width  (car (gimp-drawable-width drawable)))
          (drawable-height  (car (gimp-drawable-height drawable)))
          )
    (define (read-values layer starting dx dy spacing-x spacing-y line-start-x line-start-y)
      (cons
       (list
        (list (car starting) (cadr starting))
        (list (+ (car starting) dx ) (+ (cadr starting) dy ))
        (+ brush-floor (* (- brush-ceiling brush-floor) (/ (- 256 (+ 0 (vector-ref (cadr (gimp-drawable-get-pixel drawable (car starting) (cadr starting))) 0))) 256 )))
        )
       (cond
        ((< (+ (car starting) dx ) drawable-width )
         (read-values
          layer
          (list (+ (car starting) dx ) (+ (cadr starting) dy ))
          dx
          dy
          spacing-x
          spacing-y
          line-start-x
          line-start-y))
        ((and (not (< (+ (car starting) dx ) drawable-width )) (< (+ (cadr starting) spacing-y 1) (car (gimp-drawable-height drawable))) )
         (read-values
          layer
          (list (+ line-start-x spacing-x ) (+ line-start-y spacing-y))
          dx
          dy
          spacing-x
          spacing-y
          (+ line-start-x spacing-x )
          (+ line-start-y spacing-y)))
        (else '()))
       
       )
      
      
      )
    (define (condense lst)
      (cond
       ((null? (cdr lst))
        (car (list lst)))
       (else
        (let* ((cval (condense (cdr lst))))
          (cond
           ((or (< delta-y (abs (- (cadr(car(car lst))) (cadr(car(car cval))))))
                (< delta-x (abs (- (car(car(car lst))) (car(car(car cval)))))) )
            ;; (print "oob no skip")
            (cons (car lst) cval)
            )
           
           ((= (caddr(car lst)) (caddr(car cval)))
            ;; (print "did skip")
            (cons (list  (car(car lst)) (cadr (car cval)) (caddr(car cval))) (cdr cval))
            )
           (else (cons (car lst) cval))
           )))))
    (define (draw-line p)
      (let* ((points (make-vector 4 'double)))
        (aset  points 0 (car (car p)))
        (aset  points 1 (cadr (car p)))
        (aset  points 2 (car (cadr p)))
        (aset  points 3 (cadr (cadr p)))
        (gimp-context-set-brush-size (caddr p))
        (gimp-paintbrush cross-layer 0 4 points 0 0)))
    (define (draw-from-list lpoints)
      (print lpoints)
      (if (not (null? lpoints))
          (draw-line (car lpoints)))
      (if (not (null? lpoints))
          (draw-from-list (cdr lpoints)))
      )
    (define (increment-color color)
      (list
       (+ 1 (car color))
       (+ 1 (cadr color))
       (+ 1 (caddr color))
       ))
    (define (make-stipples color)
      ;; select each color from 0 to 255{
      (while (<  (car color) 255)
             (gimp-selection-none img)
             (gimp-image-set-active-layer img drawable)
             
             (gimp-image-select-color img 2 drawable color)
             (print (gimp-selection-bounds img))

             (cond
              ;;if select is empty try next color
              ((< (car (gimp-selection-bounds img)) 1)
               (print "was empty")
               
               (set! color (increment-color color)))
              
              (else
               ;;make new layer
               (gimp-selection-none img)
               (let* (
                      (base-layer (car (gimp-layer-new img drawable-width drawable-height 3 "StipSamp" 100 0 )))
                      (color-value (car color))
                      )

                 (print "----------was NOT empty") 
                 (gimp-image-insert-layer  img base-layer 0 0)
                 (print "inserted layer") 
                 ;;fill new layer with white
                 (gimp-drawable-fill  base-layer 2)
                 (print "filled drawable") 
                 (print "hurling ")
                 (print color-value)
                 (print (* 100 (/ color-value 255)))
                 
                 ;;noise hurl percentage as percent out of 255
                 
                 (plug-in-randomize-hurl  1 img base-layer (* 100 (/ (- 256 color-value) 256)) 1 TRUE 0)
                 (print "hurled") 
                 ;;threshhold all but white
                 (gimp-drawable-threshold  base-layer 0 1 1)
                 (print "thresh") 
                 ;;convert white to alpha
                 (plug-in-colortoalpha 1 img base-layer '(255 255 255))


                 ;;if pattern{
                 ;; for in pattern{

                 ;;    duplicate layer

                 ;;   move by offset
                 ;;   (gimp-item-transform-translate item off-x off-y)
                 ;;   merge down

                 ;;   }
                 ;; merge layers


                 ;;}

                 ;;scale up by multipliers
                 (gimp-item-transform-scale  base-layer 0 0 (* drawable-width scale-x) (* drawable-height scale-y))
                 (print "tform") 

                 ;; cut out only the part for that color
                 (gimp-image-select-color  img 2 drawable color)
                 (print "select color") 
                 (gimp-selection-invert  img)
                 (print "invert select") 

                 ;; (gimp-drawable-edit-clear (gimp-image-get-selection image))
                 (gimp-drawable-edit-clear  base-layer)
                 (print " select clear") 

                 ;; merge down to stiple layer
                 (gimp-image-merge-down img base-layer 1)
                 (print "merge down") 
                 ;; if not 255 then recurse the next color

                 (set! color (increment-color color))
                 
                 )

               )
              )
             )



      )
    (let*
        (
         (stiples-layer (car (gimp-layer-new img drawable-width drawable-height 3 "stipples" 100 0 ))))
      ;; make new stiple layer
      (gimp-image-insert-layer img stiples-layer 0 0)
      ;; start stippling
      (gimp-context-set-sample-threshold 0) ;todo: resore old thresh
      (make-stipples '(0 0 0))

      ))

                                        ; Merge down, if required
  (if (equal? merge-flag TRUE)
      (gimp-image-merge-down img value-layer 1 )
      ()
      )
  (gimp-image-undo-group-end img)
  (gimp-displays-flush)   
  )


(script-fu-register "script-fu-stipple"
                    _"Stippling"
                    _"turn shading into stipling"
                    "{Taylor Hardy} (hardy.taylor.j@gmail.com)"
                    "{Taylor Hardy}"
                    "2019-08-15"
                    ""

                    SF-IMAGE     "Image"         0
                    SF-DRAWABLE "Current Layer" 0
                    ;; SF-TOGGLE     "Spacing based on" FALSE
                    SF-VALUE    "Scale X" "2"
                    SF-VALUE    "Scale Y" "2"
                    SF-TOGGLE     "Pre-Posterize?" FALSE
                    SF-VALUE    "Poster-Levels" "20"
                    SF-VALUE    "Pattern" "12"
                    SF-TOGGLE     "Merge Layers?" FALSE)

(script-fu-menu-register "script-fu-stipple"
                         _"<Toolbox>/Xtns/Script-Fu")


