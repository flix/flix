namespace Test/Exp/Effect {

    // no test annotations for now
    // just making it public for redundancy purposes
    // @test
    pub def handleEff01(): Bool = {
        try true with {
            def Fail.fail(_) = false
        }
    }

    // @test
    pub def handleEff02(): Bool = {
        try true with {
            def Fail.fail(_) = resume(false)
        }
    }

    // @test
    pub def perform01(): Bool = {
        do Fail.fail(":(")
    }

    // @test
    pub def perform02(): Bool = {
        try {
            let x = do Console.readln();
            let y = do Console.readln();
            x == y
        } with {
            def Console.readln() = resume("hello")
            def Console.println(_) = resume()
        }
    }

    // @test
    pub def handlePerform01(): Bool = {
        try do Fail.fail("rats") with {
            def Fail.fail(_) = true
        }
    }

    // @test
    pub def handlePerform02(): Bool = {
        try {
            let _ = do Console.readln();
            true
        } with {
            def Console.readln() = "read"
            def Console.println(_) = resume()
        }
    }

    // @test
    pub def handlePerform03(): Bool = {
        // drunken coin flip: flip a coin to see if we can flip a coin
        try {
            try {
                let caught = do Flip.flip();
                if (caught) {
                    Fail.fail("dropped the coin :(")
                } else {
                    do Flip.flip()
                }
            } with {
                def Flip.flip() = resume(true)
            }
        } with {
            def Fail.fail(_) = false
        }
    }

    // @test
    pub def handlePerform04(): Bool = {
        // flaky failure: flip a coin to see if we recover
        try {
            try {
                try {
                    do Fail.fail("oh no!")
                } with {
                    def Fail.fail(msg) = {
                        let reallyFail = do Flip.flip();
                        if (reallyFail) {
                            do Fail.fail(msg)
                        } else {
                            ()
                        }
                    }
                }
            } with {
                def Flip.flip() = resume(false)
            }
        } with {
            def Fail.fail(_) = false
        }
    }

    eff Fail {
        def fail(x: String): Unit
    }

    eff Console {
        def readln(): String
        def println(x: String): Unit
    }

    eff Flip {
        def flip(): Bool
    }
}
