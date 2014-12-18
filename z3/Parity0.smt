;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AUTOMATICALLY GENERATED FILE. DO NOT EDIT.                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-datatypes () ((Parity Parity.Top Parity.Odd Parity.Even Parity.Bot)))

(define-fun Parity.Leq ((x1 Parity) (y2 Parity)) Bool
    (or 
        (and (= x1 Parity.Bot))
        (and (= x1 Parity.Odd) (= y2 Parity.Odd))
        (and (= x1 Parity.Even) (= y2 Parity.Even))
        (and (= y2 Parity.Top))))

(define-fun Parity.Join ((x0 Parity) (y0 Parity) (z0 Parity)) Bool
    (or 
        (and (= x0 Parity.Bot) (= z0 y0))
        (and (= y0 Parity.Bot) (= z0 x0))
        (and (= x0 Parity.Odd) (= y0 Parity.Odd) (= z0 Parity.Odd))
        (and (= x0 Parity.Odd) (= y0 Parity.Even) (= z0 Parity.Top))
        (and (= x0 Parity.Even) (= y0 Parity.Even) (= z0 Parity.Even))
        (and (= x0 Parity.Even) (= y0 Parity.Odd) (= z0 Parity.Top))
        (and (= x0 Parity.Top) (= z0 Parity.Top))
        (and (= y0 Parity.Top) (= z0 Parity.Top))))

(define-fun Parity.Height ((x3 Parity) (y4 Int)) Bool
    (or 
        (and (= x3 Parity.Bot) (= y4 3))
        (and (= x3 Parity.Odd) (= y4 2))
        (and (= x3 Parity.Even) (= y4 2))
        (and (= x3 Parity.Top) (= y4 1))))


;; Reflexivity: ∀x. x ⊑ x
(define-fun reflexivity () Bool
    (forall ((x Parity))
        (Parity.Leq x x)))
(assert reflexivity)
(check-sat)
    

;; Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
(define-fun anti-symmetri () Bool
    (forall ((x Parity) (y Parity))
        (=>
            (and (Parity.Leq x y)
                 (Parity.Leq y x))
            (= x y))))
(assert anti-symmetri)
(check-sat)
    

;; Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
(define-fun transitivity () Bool
    (forall ((x Parity) (y Parity) (z Parity))
        (=>
            (and (Parity.Leq x y)
                 (Parity.Leq y z))
            (Parity.Leq x z))))
(assert transitivity)
(check-sat)
    

;; Least Element: ∀x. ⊥ ⊑ x.
(define-fun least-element () Bool
    (forall ((x Parity))
        (Parity.Leq Parity.Bot x)))
(assert least-element)
(check-sat)
    

;; Functional: ∀x1, x2, y1, y2. (x1 = x2 ∧ y1 = y2) ⇒ f(x1, y1) = f(x2, y2).
(define-fun Parity.Join-functional () Bool
    (forall ((x1 Parity) (x2 Parity) (y1 Parity) (y2 Parity) (r1 Parity) (r2 Parity))
        (=>
            (and
                (= x1 x2)
                (= y1 y2)
                (Parity.Join x1 y1 r1)
                (Parity.Join x2 y2 r2))
        (= r1 r2))))
(assert Parity.Join-functional)
(check-sat)
     

;; Total: ∀x, y, ∃z. z = f(x, y).
(define-fun Parity.Join-total () Bool
    (forall ((x Parity) (y Parity))
        (exists ((z Parity))
            (Parity.Join x y z))))
(assert Parity.Join-total)
(check-sat)
     

;; Upper Bound: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
(define-fun Parity-upper-bound () Bool
    (forall ((x Parity) (y Parity) (z Parity))
        (and
            (=> (Parity.Join x y z) (Parity.Leq x z))
            (=> (Parity.Join x y z) (Parity.Leq y z)))))
(assert Parity-upper-bound)
(check-sat)
    

;; Least Upper Bound: ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
(define-fun Parity-least-upper-bound () Bool
    (forall ((x Parity) (y Parity) (z Parity) (w Parity))
        (=>
            (and (Parity.Leq x z)
                 (Parity.Leq y z)
                 (Parity.Join x y w))
        (Parity.Leq w z))))
(assert Parity-least-upper-bound)
(check-sat)
    

;; Functional: ∀x, y. x = y ⇒ f(x) = f(y).
(define-fun Parity.Height-functional () Bool
    (forall ((x Parity) (y Parity) (r1 Int) (r2 Int))
        (=>
            (and
                (= x y)
                (Parity.Height x r1)
                (Parity.Height y r2))
                (= r1 r2))))
(assert Parity.Height-functional)
(check-sat)
     

 ;; Total: ∀x. ∃y. y = f(x).
(define-fun Parity.Height-total () Bool
    (forall ((x Parity))
        (exists ((r Int))
            (Parity.Height x r))))
(assert Parity.Height-total)
(check-sat)
     

;; Stricly-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ f(x) > f(y).
(define-fun Parity.Height-strictly-decreasing () Bool
    (forall ((x Parity) (y Parity) (r1 Int) (r2 Int))
        (=>
            (and (distinct x y)
                 (Parity.Leq x y)
                 (Parity.Height x r1)
                 (Parity.Height y r2))
            (> r1 r2))))
(assert  Parity.Height-strictly-decreasing)
(check-sat)
    

;; Non-Negative: ∀x. f(x) > 0.
(define-fun Parity.Height-non-negative () Bool
    (forall ((x Parity) (r Int))
        (=>
            (Parity.Height x r)
                (> r 0))))
(assert Parity.Height-non-negative)
(check-sat)
    
(define-fun Parity.Sum ((x0 Parity) (y0 Parity) (z0 Parity)) Bool
    (or 
        (and (= x0 Parity.Bot) (= z0 Parity.Bot))
        (and (= y0 Parity.Bot) (= z0 Parity.Bot))
        (and (= x0 Parity.Odd) (= y0 Parity.Odd) (= z0 Parity.Even))
        (and (= x0 Parity.Odd) (= y0 Parity.Even) (= z0 Parity.Odd))
        (and (= x0 Parity.Even) (= y0 Parity.Odd) (= z0 Parity.Odd))
        (and (= x0 Parity.Even) (= y0 Parity.Even) (= z0 Parity.Even))
        (and (= x0 Parity.Top) (= z0 Parity.Top))
        (and (= y0 Parity.Top) (= z0 Parity.Top))))


;; Functional: ∀x1, x2, y1, y2. (x1 = x2 ∧ y1 = y2) ⇒ f(x1, y1) = f(x2, y2).
(define-fun Parity.Sum-functional () Bool
    (forall ((x1 Parity) (x2 Parity) (y1 Parity) (y2 Parity) (r1 Parity) (r2 Parity))
        (=>
            (and
                (= x1 x2)
                (= y1 y2)
                (Parity.Sum x1 y1 r1)
                (Parity.Sum x2 y2 r2))
        (= r1 r2))))
(assert Parity.Sum-functional)
(check-sat)
     

;; Total: ∀x, y, ∃z. z = f(x, y).
(define-fun Parity.Sum-total () Bool
    (forall ((x Parity) (y Parity))
        (exists ((z Parity))
            (Parity.Sum x y z))))
(assert Parity.Sum-total)
(check-sat)
     

;; Strict ∀x. f(⊥, x) = ⊥ ∧ f(x, ⊥) = ⊥.
(define-fun Parity.Sum-strict () Bool
    (forall ((x Parity))
        (and
            (Parity.Sum Parity.Bot x Parity.Bot)
            (Parity.Sum x Parity.Bot Parity.Bot))))
(assert Parity.Sum-strict)
(check-sat)
    

;; Monotone: ∀x1, x2, y1, y2. x1 ⊑ x2 ∧ y1 ⊑ y2 ⇒ f(x1, y1) ⊑ f(x2, y2).
(define-fun sum-monotone () Bool
    (forall ((x1 Parity) (x2 Parity) (y1 Parity) (y2 Parity) (r1 Parity) (r2 Parity))
        (=>
            (and
                (Parity.Leq x1 x2)
                (Parity.Leq y1 y2)
                (Parity.Sum x1 y1 r1)
                (Parity.Sum x2 y2 r2))
                (Parity.Leq r1 r2))))
(assert Parity.Sum-strict)
(check-sat)
    
