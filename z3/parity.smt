;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lattice Elements                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-datatypes () ((Sign Sign.Top Sign.Bot Sign.Neg Sign.Zer Sign.Pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lattice Definitions                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: We could also use define-fun in a better way.

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

        (and (= x Sign.Top) (= y Sign.Top) (= z Sign.Top))))

;; Definition of height
(define-fun Sign.height ((x Sign) (h Int)) Bool
    (or
        (and (= x Sign.Top) (= h 1))
        (and (= x Sign.Neg) (= h 2))
        (and (= x Sign.Zer) (= h 2))
        (and (= x Sign.Pos) (= h 2))
        (and (= x Sign.Bot) (= h 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lattice Order                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reflexivity: ∀x. x ⊑ x
(define-fun reflexivity () Bool
    (forall ((x Sign))
        (Sign.leq x x)))

;; Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
(define-fun anti-symmetri () Bool
    (forall ((x Sign) (y Sign))
        (=>
            (and (Sign.leq x y)
                 (Sign.leq y x))
            (= x y))))

;; Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
(define-fun transitivity () Bool
    (forall ((x Sign) (y Sign) (z Sign))
        (=>
            (and (Sign.leq x y)
                 (Sign.leq y z))
            (Sign.leq x z))))

;; Least Element: ∀x. ⊥ ⊑ x.
(define-fun least-element () Bool
    (forall ((x Sign))
        (Sign.leq Sign.Bot x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Join                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Join is Functional: ∀x1, x2, y1, y2. (x1 = x2 ∧ y1 = y2) ⇒ (x1 ⨆ y1 = x2 ⨆ y2)
(define-fun join-function () Bool
    (forall ((x1 Sign) (x2 Sign) (y1 Sign) (y2 Sign) (r1 Sign) (r2 Sign))
        (=>
            (and
                (= x1 x2)
                (= y1 y2)
                (Sign.join x1 y1 r1)
                (Sign.join x2 y2 r2))
            (= r1 r2))))

;; Join is Total: ∀x, y, ∃z. z = x ⨆ y.
(define-fun join-total () Bool
    (forall ((x Sign) (y Sign))
        (exists ((z Sign))
            (Sign.join x y z))))

;; Join-Lub-1: ∀x, y, z. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y.
(define-fun join-lub-1 () Bool
    (forall ((x Sign) (y Sign) (z Sign))
        (and
            (=> (Sign.join x y z) (Sign.leq x z))
            (=> (Sign.join x y z) (Sign.leq y z)))))

;; Join-Lub-2
;; ∀x, y, z. x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y ⊑ z.
(define-fun join-lub-2 () Bool
    (forall ((x Sign) (y Sign) (z Sign) (w Sign))
        (=>
            (and (Sign.leq x z)
                 (Sign.leq y z)
                 (Sign.join x y w))
            (Sign.leq w z))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transfer Functions                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sum is Functional: ∀x1, x2, y1, y2. (x1 = x2 ∧ y1 = y2) ⇒ (sum(x1, y1) = sum(x2, y2))
(define-fun sum-function () Bool
    (forall ((x1 Sign) (x2 Sign) (y1 Sign) (y2 Sign) (r1 Sign) (r2 Sign))
        (=>
            (and
                (= x1 x2)
                (= y1 y2)
                (Sign.sum x1 y1 r1)
                (Sign.sum x2 y2 r2))
            (= r1 r2))))

;; Sum-Strict ∀x. sum(⊥, x) = ⊥ ∧ sum(x, ⊥) = ⊥
(define-fun sum-strict () Bool
    (forall ((x Sign))
        (and
            (Sign.sum Sign.Bot x Sign.Bot)
            (Sign.sum x Sign.Bot Sign.Bot))))

;; Sum-Montone
;; ∀x1, x2, y1, y2. x1 ⊑ x2 ∧ y1 ⊑ y2 ⇒ f(x1, y1) ⊑ f(x2, y2)
(define-fun sum-monotone () Bool
    (forall ((x1 Sign) (x2 Sign) (y1 Sign) (y2 Sign) (r1 Sign) (r2 Sign))
        (=>
            (and
                (Sign.leq x1 x2)
                (Sign.leq y1 y2)
                (Sign.sum x1 y1 r1)
                (Sign.sum x2 y2 r2))
            (Sign.leq r1 r2))))

;; Sum-Distributive: ∀x, y, f(x ⨆ y) = f(x) ⨆ f(y).
;; Sum-Distributive: ∀x1, x2, y1, y2, sum(x1 ⨆ y1, x2 ⨆ y2) = sum(x1, y1) ⨆ sum(x2, y2).
;; Sum-Distributive: ∀x1, x2, y1, y2, r1 = x1 ⨆ y1, r2 = x2 ⨆ y2, r3 = sum(r1, r2), r4 = sum(x1, y1), r5 = sum(x2, y2), r6 = r4 ⨆ r5 ⇒ r3 = r6.
;;(define-fun sum-distributive () Bool
;;    (forall ((x1 Sign) (x2 Sign) (y1 Sign) (y2 Sign) (r1 Sign) (r2 Sign) (r3 Sign) (r4 Sign) (r5 Sign) (r6 Sign))
;;        (=>
;;            (and
;;                (Sign.join x1 y1 r1)
;;                (Sign.join x2 y2 r2)
;;                (Sign.sum r1 r2 r3)
;;                (Sign.sum x1 y1 r4)
;;                (Sign.sum x2 y2 r5)
;;                (Sign.join r4 r5 r6))
;;            (= r3 r6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Height                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Height-Function: ∀x, y. x = y ⇒ h(x) = h(y)
(define-fun height-function () Bool
    (forall ((x Sign) (y Sign) (r1 Int) (r2 Int))
        (=>
            (and
                (= x y)
                (Sign.height x r1)
                (Sign.height y r2))
            (= r1 r2))))

;; Height-Total: ∀x. ∃y. y = h(x).
(define-fun height-total () Bool
    (forall ((x Sign))
        (exists ((r Int))
            (Sign.height x r))))

;; Height-NonNegative: ∀x. h(x) > 0.
(define-fun height-non-negative () Bool
    (forall ((x Sign) (h Int))
        (=>
            (Sign.height x h)
            (> h 0))))

;; Height-Decreasing: ∀x, y. x ⊑ y ∧ x != y ⇒ h(x) > h(y).
(define-fun height-decreasing () Bool
    (forall ((x Sign) (y Sign) (h1 Int) (h2 Int))
        (=>
            (and (distinct x y)
                 (Sign.leq x y)
                 (Sign.height x h1)
                 (Sign.height y h2))
            (> h1 h2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assertions                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lattice order
(assert reflexivity)
(assert anti-symmetri)
(assert transitivity)
(assert least-element)

;; join
(assert join-function)
(assert join-total)
(assert join-lub-1)
(assert join-lub-2)

;; transfer functions
(assert sum-function)
(assert sum-monotone)
(assert sum-strict)
;;(assert sum-distributive)

;; height
(assert height-function)
(assert height-total)
(assert height-non-negative)
(assert height-decreasing)

(check-sat)
