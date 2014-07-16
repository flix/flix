package impl.verifier

class Function1 {
  //  ;; Height-Function: ∀x, y. x = y ⇒ h(x) = h(y)
  //  (define-fun height-function () Bool
  //    (forall ((x Sign) (y Sign) (r1 Int) (r2 Int))
  //      (=>
  //  (and
  //    (= x y)
  //  (Sign.height x r1)
  //  (Sign.height y r2))
  //  (= r1 r2))))
  //
  //  ;; Height-Total: ∀x. ∃y. y = h(x).
  //  (define-fun height-total () Bool
  //    (forall ((x Sign))
  //      (exists ((r Int))
  //        (Sign.height x r))))
}
