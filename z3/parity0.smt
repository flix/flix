(declare-datatypes () ((Sign Sign.Top Sign.Neg Sign.Zer Sign.Pos Sign.Bot)))

(define-fun Sign.Leq ((x0 Sign) (y0 Sign)) Bool
    (or 
        (and (= x0 Sign.Bot))
        (and (= x0 Sign.Neg) (= y0 Sign.Neg))
        (and (= x0 Sign.Zer) (= y0 Sign.Zer))
        (and (= x0 Sign.Pos) (= y0 Sign.Pos))
        (and (= y0 Sign.Top))))

(define-fun Sign.Join ((x0 Sign) (y0 Sign)) Bool
    (or 
        (and (= x0 Sign.Bot) (= z0 y0))
        (and (= y0 Sign.Bot) (= z0 x0))
        (and (= x0 Sign.Neg) (= y0 Sign.Neg) (= z0 Sign.Neg))
        (and (= x0 Sign.Neg) (= y0 Sign.Zer) (= z0 Sign.Top))
        (and (= x0 Sign.Neg) (= y0 Sign.Pos) (= z0 Sign.Top))
        (and (= x0 Sign.Zer) (= y0 Sign.Neg) (= z0 Sign.Top))
        (and (= x0 Sign.Zer) (= y0 Sign.Zer) (= z0 Sign.Zer))
        (and (= x0 Sign.Zer) (= y0 Sign.Pos) (= z0 Sign.Top))
        (and (= x0 Sign.Pos) (= y0 Sign.Neg) (= z0 Sign.Top))
        (and (= x0 Sign.Pos) (= y0 Sign.Zer) (= z0 Sign.Top))
        (and (= x0 Sign.Pos) (= y0 Sign.Pos) (= z0 Sign.Pos))
        (and (= y0 Sign.Top) (= z0 Sign.Top))
        (and (= x0 Sign.Top) (= z0 Sign.Top))))

;; Reflexivity: ∀x. x ⊑ x
(define-fun reflexivity () Bool
    (forall ((x Sign))
        (Sign.Leq x x)))
    

;; Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
(define-fun anti-symmetri () Bool
    (forall ((x Sign) (y Sign))
        (=>
            (and (Sign.Leq x y)
                 (Sign.Leq y x))
            (= x y))))
    

;; Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
(define-fun transitivity () Bool
    (forall ((x Sign) (y Sign) (z Sign))
        (=>
            (and (Sign.Leq x y)
                 (Sign.Leq y z))
            (Sign.Leq x z))))
    