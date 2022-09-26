;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lecture5b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define BLANK (square 0 "solid" "white"))

;;testing
(define I1 (square 10 "solid" "blue"))
(define I2 (square 30 "solid" "black"))
(define I3 (square 20 "solid" "red"))

;; DataDef

;; ListOfImages is one of:
;;  - empty
;;  - compound: (cons Image ListOfImage)
(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I1 (cons I2 empty)))

;; (define (fn-for-loi loi) empty)

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) ...]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; Functions:

;; ListOfImage -> Image
;; lay out images left to right in increasing order of size
(check-expect (arrange-images empty) BLANK)
(check-expect (arrange-images (cons I1 (cons I3 empty)))
              (beside I1 I3 BLANK))
(check-expect (arrange-images (cons I2 (cons I2 empty)))
              (beside I2 I2 BLANK))

;(define (arrange-images loi) BLANK) ;stub

(define (arrange-images loi)
  (layout-images (sort-images loi)))

;; ListOfImage -> Image
;; place images beside each other in order of list
(check-expect (layout-images empty) BLANK)
(check-expect (layout-images (cons I3 (cons I1 empty)))
              (beside I3 I1))

; (define (layout-images loi) BLANK) ;stub

(define (layout-images loi)
  (cond [(empty? loi) BLANK]
        [else (beside (first loi)
                      (layout-images (rest loi)))]))

;; ListOfImage -> ListOfImage
;; sort images in increasing order of size
(check-expect (sort-images empty) empty)
(check-expect (sort-images (cons I2 (cons I2 empty)))
              (cons I2 (cons I2 empty)))

; (define (sort-images loi) loi) ;stub

(define (sort-images loi)
  (cond [(empty? loi) empty]
        [else
         (insert (first loi)
                 (sort-images (rest loi)))]))


;; Image ListOfImage -> ListOfImage
;; insert img in proper place in list (in increasing order of size)
;; ASSUME: list is already sorted
(check-expect (insert I1 empty) (cons I1 empty))
(check-expect (insert I1 (cons I3 (cons I2 empty))) (cons I1 (cons I3 (cons I2 empty))))
(check-expect (insert I2 (cons I1 (cons I3 empty))) (cons I1 (cons I3 (cons I2 empty))))
(check-expect (insert I3 (cons I1 (cons I2 empty))) (cons I1 (cons I3 (cons I2 empty))))

;(define (insert img lst) lst) ;stub

(define (insert img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
         (if (larger? img (first loi))
             (cons (first loi)
                   (insert img
                           (rest loi)))
             (cons img loi))]))

;; Image Image -> Boolean
;; produce true if img1 is larger than img2 (by area)


;(define (larger? img1 img2) true) ;stub

(check-expect (larger? (square 10 "solid" "blue") (square 20 "solid" "red")) false)
(check-expect (larger? (square 10 "solid" "blue") (square 10 "solid" "blue")) false)
(check-expect (larger? (square 20 "solid" "red") (square 10 "solid" "green")) true)

(define (larger? img1 img2)
  (> (* (image-width img1) (image-height img1))
     (* (image-width img2) (image-height img2))))

