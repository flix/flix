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
(echo "Anti-Symmetri:")
(assert anti-symmetri)
(check-sat)
    

;; Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
(define-fun transitivity () Bool
    (forall ((x Parity) (y Parity) (z Parity))
        (=>
            (and (Parity.Leq x y)
                 (Parity.Leq y z))
            (Parity.Leq x z))))
    

;; Least Element: ∀x. ⊥ ⊑ x.
(define-fun least-element () Bool
    (forall ((x Parity))
        (Parity.Leq Parity.Bot x)))
    
