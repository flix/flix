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
    (forall ((x Sign)) (Sign.leq x x)))
(assert (not reflexivity))
(echo "Reflexivity:")
(check-sat)

;; Anti-symmetri
(push)
    (define-fun anti-symmetri () Bool
        (forall ((x Sign) (y Sign)) (=> (and (Sign.leq x y) (Sign.leq y x)) (= x y))))
    (assert (not anti-symmetri))
    (echo "Anti-symmetri:")
    (check-sat)
(pop)


