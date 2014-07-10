(declare-datatypes () ((Sign Sign.Top Sign.Bot Sign.Neg Sign.Zer Sign.Pos)))

(define-fun Sign.leq ((x Sign) (y Sign)) Bool
    (or (= x Sign.Bot)
        (and (= x Sign.Neg) (= y Sign.Neg))
        (and (= x Sign.Zer) (= y Sign.Zer))
        (and (= x Sign.Pos) (= y Sign.Pos))
        (= y Sign.Top))
)

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

(assert reflexivity)
(assert anti-symmetri)
(assert transitivity)
(assert least-element)
(check-sat)