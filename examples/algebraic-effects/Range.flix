///
/// This example aims to help you understand the basics
/// of effects and effect handlers.
/// We build a simple effectful iterator over a
/// range of numbers and define a function that
/// sums that range.
///
/// There are three things to pay attention to:
/// - How to use effects to signal the caller
/// - Handling the effect at the call site
/// - Using the continuation to transfer control back to the callee
///
/// Inspired by the Iterator benchmark from Effekt Lang
///
pub def main(): Unit \ IO = {
    let sum = EffectExamples.Range.sumTo(42);
    println("The sum from 0..42 is: ${sum}")
}

mod EffectExamples.Range {

    // Effect declaration
    eff Emission {
        def emit(e: Int32): Unit
    }

    ///
    /// Iterates over the range between `lo` and `hi`.
    ///
    pub def range(lo: Int32, hi: Int32): Unit \ Emission =
        // Note the effect in the type
        if (lo > hi) {
            ()
        }
        else {
            // Raise the effect to signal the caller with a value.
            Emission.emit(lo);
            range(lo + 1, hi)
        }

    ///
    /// Returns the sum from `0` to `n`.
    ///
    pub def sumTo(n: Int32): Int32 = region rc {
        let s = Ref.fresh(rc, 0);
        try {
            // Call an effectful function.
            // Note that `sumTo` does not
            // inherit the `Emission` effect type
            // that `range` has, because we
            // handle the effect below.
            range(0, n);
            Ref.get(s)
        } with Emission {
            def emit(e, k) = {
                // Store the value `e` sent by the callee.
                Ref.put(Ref.get(s) + e, s);

                // Call the continuation `k` to transfer
                // control back to the callee.
                k()
            }
        }
    }
}
