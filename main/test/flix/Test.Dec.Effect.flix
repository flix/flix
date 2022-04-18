namespace Test/Dec/Effect {
    eff Console {
        // Will be removed after Weeder checks are added
        def readLine(): String
        def println(s: String): Unit
    }

    eff Empty

    // Will be removed after Weeder checks are added
    eff Polymorphic[a] {
        def op[a](): a
    }
}
