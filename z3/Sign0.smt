
;; Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
(define-fun anti-symmetri () Bool
    (forall ((x Sign) (y Sign))
        (=>
            (and (Sign.Leq x y)
                 (Sign.Leq y x))
            (= x y))))
(push)
(assert anti-symmetri)
(check-sat)
(pop)
    

;; Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
(define-fun transitivity () Bool
    (forall ((x Sign) (y Sign) (z Sign))
        (=>
            (and (Sign.Leq x y)
                 (Sign.Leq y z))
            (Sign.Leq x z))))
(push)
(assert transitivity)
(check-sat)
(pop)
    

;; Least Element: ∀x. ⊥ ⊑ x.
(define-fun least-element () Bool
    (forall ((x Sign))
        (Sign.Leq Sign.Bot x)))
(push)
(assert least-element)
(check-sat)
(pop)
    

;; Functional: ∀x1, x2, y1, y2. (x1 = x2 ∧ y1 = y2) ⇒ f(x1, y1) = f(x2, y2).
(define-fun Sign.Join-functional () Bool
    (forall ((x1 Sign) (x2 Sign) (y1 Sign) (y2 Sign) (r1 Sign) (r2 Sign))
        (=>
            (and
                (= x1 x2)
                (= y1 y2)
                (Sign.Join x1 y1 r1)
                (Sign.Join x2 y2 r2))
        (= r1 r2))))
(push)
(assert Sign.Join-functional)
(check-sat)
(pop)
     

;; Total: ∀x, y, ∃z. z = f(x, y).
(define-fun Sign.Join-total () Bool
    (forall ((x Sign) (y Sign))
        (exists ((z Sign))
            (Sign.Join x y z))))
(push)
(assert Sign.Join-total)
(check-sat)
(pop)
     

;; Upper Bound: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
(define-fun Sign-upper-bound () Bool
    (forall ((x Sign) (y Sign) (z Sign))
        (and
            (=> (Sign.Join x y z) (Sign.Leq x z))
            (=> (Sign.Join x y z) (Sign.Leq y z)))))
(push)
(assert Sign-upper-bound)
(check-sat)
(pop)
    

;; Least Upper Bound: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
(define-fun Sign-least-upper-bound () Bool
    (forall ((x Sign) (y Sign) (z Sign) (w Sign))
        (=>
            (and (Sign.Leq x z)
                 (Sign.Leq y z)
                 (Sign.Join x y w))
        (Sign.Leq w z))))
(push)
(assert Sign-least-upper-bound)
(check-sat)
(pop)
    

;; Functional: ∀x, y. x = y ⇒ f(x) = f(y).
(define-fun Sign.Height-functional () Bool
    (forall ((x Sign) (y Sign) (r1 Int) (r2 Int))
        (=>
            (and
                (= x y)
                (Sign.Height x r1)
                (Sign.Height y r2))
                (= r1 r2))))
(push)
(assert Sign.Height-functional)
(check-sat)
(pop)
     

 ;; Total: ∀x. ∃y. y = f(x).
(define-fun Sign.Height-total () Bool
    (forall ((x Sign))
        (exists ((r Int))
            (Sign.Height x r))))
(push)
(assert Sign.Height-total)
(check-sat)
(pop)
     

;; Stricly-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ f(x) > f(y).
(define-fun Sign.Height-strictly-decreasing () Bool
    (forall ((x Sign) (y Sign) (r1 Int) (r2 Int))
        (=>
            (and (distinct x y)
                 (Sign.Leq x y)
                 (Sign.Height x r1)
                 (Sign.Height y r2))
            (> r1 r2))))
(push)
(assert  Sign.Height-strictly-decreasing)
(check-sat)
(pop)
    

;; Non-Negative: ∀x. f(x) > 0.
(define-fun Sign.Height-non-negative () Bool
    (forall ((x Sign) (r Int))
        (=>
            (Sign.Height x r)
                (> r 0))))
(push)
(assert Sign.Height-non-negative)
(check-sat)
(pop)
    

;; Functional: ∀x1, x2, y1, y2. (x1 = x2 ∧ y1 = y2) ⇒ f(x1, y1) = f(x2, y2).
(define-fun Sign.Sum-functional () Bool
    (forall ((x1 Sign) (x2 Sign) (y1 Sign) (y2 Sign) (r1 Sign) (r2 Sign))
        (=>
            (and
                (= x1 x2)
                (= y1 y2)
                (Sign.Sum x1 y1 r1)
                (Sign.Sum x2 y2 r2))
        (= r1 r2))))
(push)
(assert Sign.Sum-functional)
(check-sat)
(pop)
     

;; Total: ∀x, y, ∃z. z = f(x, y).
(define-fun Sign.Sum-total () Bool
    (forall ((x Sign) (y Sign))
        (exists ((z Sign))
            (Sign.Sum x y z))))
(push)
(assert Sign.Sum-total)
(check-sat)
(pop)
     

;; Strict ∀x. f(⊥, x) = ⊥ ∧ f(x, ⊥) = ⊥.
(define-fun Sign.Sum-strict () Bool
    (forall ((x Sign))
        (and
            (Sign.Sum Sign.Bot x Sign.Bot)
            (Sign.Sum x Sign.Bot Sign.Bot))))
(push)
(assert Sign.Sum-strict)
(check-sat)
(pop)
    

;; Monotone: ∀x1, x2, y1, y2. x1 ⊑ x2 ∧ y1 ⊑ y2 ⇒ f(x1, y1) ⊑ f(x2, y2).
(define-fun sum-monotone () Bool
    (forall ((x1 Sign) (x2 Sign) (y1 Sign) (y2 Sign) (r1 Sign) (r2 Sign))
        (=>
            (and
                (Sign.Leq x1 x2)
                (Sign.Leq y1 y2)
                (Sign.Sum x1 y1 r1)
                (Sign.Sum x2 y2 r2))
                (Sign.Leq r1 r2))))
(push)
(assert Sign.Sum-strict)
(check-sat)
(pop)
    
