(declare-datatypes () ((Sign Sign.Top Sign.Bot Sign.Neg Sign.Zer Sign.Pos)))

;; Definition of Leq
(define-fun Sign.leq ((x Sign) (y Sign)) Bool
    (or (= x Sign.Bot)
        (and (= x Sign.Neg) (= y Sign.Neg))
        (and (= x Sign.Zer) (= y Sign.Zer))
        (and (= x Sign.Pos) (= y Sign.Pos))
        (= y Sign.Top)))

;; Definition of Join
(define-fun Sign.join ((x Sign) (y Sign) (z Sign)) Bool
    (or (and (= x Sign.Bot) (= y z))
        (and (= y Sign.Bot) (= x z))
        (and (= x y z))

        (and (= x Sign.Neg) (= y Sign.Zer) (= z Sign.Top))
        (and (= x Sign.Neg) (= y Sign.Pos) (= z Sign.Top))
        (and (= x Sign.Zer) (= y Sign.Neg) (= z Sign.Top))
        (and (= x Sign.Zer) (= y Sign.Pos) (= z Sign.Top))
        (and (= x Sign.Pos) (= y Sign.Neg) (= z Sign.Top))
        (and (= x Sign.Pos) (= y Sign.Zer) (= z Sign.Top))

        (and (= x Sign.Top) (= z Sign.Top))
        (and (= y Sign.Top) (= z Sign.Top))))

;; Definition of Sum
(define-fun Sign.sum ((x Sign) (y Sign) (z Sign)) Bool
    (or (and (= x Sign.Bot) (= z Sign.Bot))
        (and (= y Sign.Bot) (= z Sign.Bot))

        (and (= x Sign.Neg) (= y Sign.Neg) (= z Sign.Neg))
        (and (= x Sign.Neg) (= y Sign.Zer) (= z Sign.Neg))
        (and (= x Sign.Neg) (= y Sign.Pos) (= z Sign.Top))

        (and (= x Sign.Zer) (= y Sign.Neg) (= z Sign.Neg))
        (and (= x Sign.Zer) (= y Sign.Zer) (= z Sign.Zer))
        (and (= x Sign.Zer) (= y Sign.Pos) (= z Sign.Pos))

        (and (= x Sign.Pos) (= y Sign.Neg) (= z Sign.Top))
        (and (= x Sign.Pos) (= y Sign.Zer) (= z Sign.Pos))
        (and (= x Sign.Pos) (= y Sign.Pos) (= z Sign.Pos))

        (and (= x Sign.Top) (= z Sign.Top))
        (and (= y Sign.Top) (= z Sign.Top))))

;; Definition of height
(define-fun Sign.height ((x Sign) (h Int)) Bool
    (or
        (and (= x Sign.Top) (= h 1))
        (and (= x Sign.Neg) (= h 2))
        (and (= x Sign.Zer) (= h 2))
        (and (= x Sign.Pos) (= h 2))
        (and (= x Sign.Bot) (= h 3))))

;; Reflexivity
(define-fun reflexivity () Bool
    (forall ((x Sign))
        (Sign.leq x x)))

;; Anti-symmetri
(define-fun anti-symmetri () Bool
    (forall ((x Sign) (y Sign))
        (=> (and (Sign.leq x y) (Sign.leq y x)) (= x y))))

;; Transitivity
(define-fun transitivity () Bool
    (forall ((x Sign) (y Sign) (z Sign))
        (=> (and (Sign.leq x y) (Sign.leq y z)) (Sign.leq x z))))

;; LeastElement
(define-fun least-element () Bool
    (forall ((x Sign))
        (Sign.leq Sign.Bot x)))

;; TODO: 1. Join must be a total function (this one I got in place)
;; Join Total
(define-fun join-total () Bool
    (forall ((x Sign) (y Sign))
        (exists ((z Sign))
            (Sign.join x y z))))

;; TODO: Need help with join-1
;; Join-1
(define-fun join-1 () Bool
    (forall ((x Sign) (y Sign) (z Sign))
        (and
            (=> (Sign.join x y z) (Sign.leq x z))
            (=> (Sign.join x y z) (Sign.leq y z)))))

;; TODO: Need help with join-2
;; Join-2
(define-fun join-2 () Bool
    (forall ((x Sign) (y Sign) (z Sign) (w Sign))
        (=>
            (and (Sign.leq x z) (Sign.leq y z))
            (and (Sign.join x y w) (Sign.leq z w)))))
;; \forall x, y, z: ( x <= z AND y <= z ) => x |_| y <= z

;; Sum-Strict
(define-fun sum-strict () Bool
    (forall ((x Sign))
        (and
            (Sign.sum Sign.Bot x Sign.Bot)
            (Sign.sum x Sign.Bot Sign.Bot))))

;; Sum-Montone
;; TODO: Currently broken.
(define-fun sum-monotone () Bool
    (forall ((x1 Sign) (y1 Sign) (r1 Sign) (x2 Sign) (y2 Sign) (r2 Sign))
        (=> (and
                (Sign.leq x1 x2)
                (Sign.leq y1 y2)
                (Sign.sum x1 y1 r1)
                (Sign.sum x2 y2 r2))
            (Sign.leq r1 r2))))


;; Height-Function
(define-fun height-function () Bool
    (forall ((x Sign) (y Sign) (r1 Int) (r2 Int))
        (=>
            (and
                (Sign.height x r1)
                (Sign.height y r2)
                (= x y))
            (= r1 r2))))

;; Height-Total

;; Height-Decreasing

(assert reflexivity)
(assert anti-symmetri)
(assert transitivity)
(assert least-element)
(assert join-total)

(push)

    (check-sat)
(pop)

;;(assert sum-strict)


