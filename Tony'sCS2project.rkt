;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Tony'sCS2project|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;My project is a virtual piano game that allows you to play notes in any
;;octaves on a keyboard. You can play the white keys by pressing A, S, D, F,
;;G, H, and J, and black keys by pressing W, E, T, Y, and U. Unfortunately,
;;I did not successfully implant the real time temperature function, but you
;;can still shift the octave up and down by pressing the right and left arrow
;;keys. By pressing "I," the software will show you the instuction to
;;play this game. Enjoy.

(require rsound)
(require rsound/piano-tones)
(require 2htdp/image)
(require 2htdp/universe)

(define (key-color n key white-key?)
  (cond
    [(string=? n key) (if white-key? "darkgray" "dimgray")]
    [else "black"]))

(define (key-mode n key)
  (if (string=? n key) "solid" "outline"))
    
(define (real-artist w)
  (artist (world-key w)))

(define (artist n)
   (if (string=? n "i") (place-image (text "Press Left/Right Keys to" 12 "black") 530 70
                       (place-image (text "Increase/Decrease Octave." 12 "black") 530 90
                       (place-image (text "Press I for Information." 12 "black") 530 110
                       (place-image (text "Press Any Key Shown" 12 "black") 530 130
                       (place-image (text "On Screen To Begin." 12 "black") 530 150
                       (place-image (text "U" 20 "white") 390 110
                       (place-image (text "Y" 20 "white") 330 110
                       (place-image (text "T" 20 "white") 270 110
                       (place-image (text "E" 20 "white") 150 110
                       (place-image (text "W" 20 "white") 90 110
                       (place-image (text "J" 24 "black") 420 180
                       (place-image (text "H" 24 "black") 360 180
                       (place-image (text "G" 24 "black") 300 180
                       (place-image (text "F" 24 "black") 240 180
                       (place-image (text "D" 24 "black") 180 180
                       (place-image (text "S" 24 "black") 120 180
                       (place-image (text "A" 24 "black") 60 180
                       (place-image (rectangle 40 100 "solid" "black") 390 80
                          (place-image (rectangle 40 100 "solid" "black") 330 80
                           (place-image (rectangle 40 100 "solid" "black") 270 80
                             (place-image (rectangle 40 100 "solid" "black") 150 80 
                                  (place-image (rectangle 40 100 "solid" "black") 90 80 
                                       (place-image (rectangle 60 180 "outline" "black") 60 120
                                           (place-image (rectangle 60 180 "outline" "black") 120 120
                                                       (place-image (rectangle 60 180 "outline" "black") 180 120
                                                                    (place-image (rectangle 60 180 "outline" "black") 240 120
                                                                                 (place-image (rectangle 60 180 "outline" "black") 300 120
                                                                                              (place-image (rectangle 60 180 "outline" "black") 360 120
                                                                                                           (place-image (rectangle 60 180 "outline" "black") 420 120
                                                                                                                 (empty-scene 600 240))))))))))))))))))))))))))))))
       (place-image
        (rectangle 40 100 "solid" (key-color n "u" #t)) 390 80
        (place-image
         (rectangle 40 100 "solid" (key-color n "y" #t)) 330 80
         (place-image
          (rectangle 40 100 "solid" (key-color n "t" #t)) 270 80
          (place-image
           (rectangle 40 100 "solid" (key-color n "e" #t)) 150 80 
           (place-image
            (rectangle 40 100 "solid" (key-color n "w" #t)) 90 80 
            (place-image
             (rectangle 60 180 (key-mode n "a") (key-color n "a" #f)) 60 120
             (place-image
              (rectangle 60 180 (key-mode n "s") (key-color n "s" #f)) 120 120
              (place-image
               (rectangle 60 180 (key-mode n "d") (key-color n "d" #f)) 180 120
               (place-image
                (rectangle 60 180 (key-mode n "f") (key-color n "f" #f)) 240 120
                (place-image
                 (rectangle 60 180  (key-mode n "g") (key-color n "g" #f)) 300 120
                 (place-image
                  (rectangle 60 180 (key-mode n "h") (key-color n "h" #f)) 360 120
                  (place-image
                   (rectangle 60 180 (key-mode n "j") (key-color n "j" #f)) 420 120
                   (empty-scene 600 240)))))))))))))))


  
   


(define (midi-tone-for-key k octave)
  (+ (* octave 12)
     (cond
       [(string=? k "a") 60]
       [(string=? k "s") 62]
       [(string=? k "d") 64]
       [(string=? k "w") 61]
       [(string=? k "e") 63]
       [(string=? k "f") 65]
       [(string=? k "g") 67]
       [(string=? k "h") 69]
       [(string=? k "j") 71]
       [(string=? k "t") 66]
       [(string=? k "y") 68]
       [(string=? k "u") 70]
       [else octave])))

(define-struct world [octave key])
(define (key-handler w key)
  (make-world
     (cond
       [(string=? key "right") (add1 (world-octave w))]
       [(string=? key "left") (sub1 (world-octave w))]
       [else (world-octave w)])
     (if (or (string=? key "right") (string=? key "left") (string=? key "i"))
         key
         (andplay
          (piano-tone (midi-tone-for-key key (world-octave w))) key))))

(big-bang
 (make-world 0
             "i")
 [to-draw real-artist]
 [on-key key-handler])