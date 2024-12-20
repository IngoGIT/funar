#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x
  (+ 12
     (* 2
        42)))
(define y (* x 2))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion
; - kopieren (ein letztes Mal)
; - Unterschiede ersetzen durch abstrakte Namen
; - Namen in lambda aufnehmen

; Konstruktionsanleitungen

; Kurzbeschreibung
; quadratisches Badezimmermuster aus zwei Bildern

; Signatur(deklaration)
(: tile (image image -> image))

; Test(s)
(check-expect
 (tile star1 square1)
 (above (beside star1 square1)
        (beside square1 star1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

;(tile star1 circle1)

#|
class C {
  static int m(int x) {
    ... x ...
    x = x + 1;
    ... x ....
  }
}

... C.m(42) ...
->
{
    ... 42 ...
}
|#