mod Test.Exp.TryWith {
    eff Print {
        pub def doIt(): Unit
    }

    eff Print2 {
        pub def doIt2(): Unit
    }

    eff Greet {
        pub def greet(name: String): Unit
    }

    eff Greet2 {
        pub def greet2(name: String): Unit
    }

    @Test
    def testTryWith01(): Unit = {
        try {
            do Test.Exp.TryWith.Print.doIt()
        } with Print {
            def doIt(_cont) = ()
        }
    }

    @Test
    def testTryWith02(): Unit = {
        try {
            do Test.Exp.TryWith.Print.doIt()
        } with Print {
            def doIt(_cont) = {
                try {
                    do Test.Exp.TryWith.Print2.doIt2()
                } with Print2 {
                    def doIt2(_cont) = ()
                }
            }
        }
    }

    @Test
    def testTryWith03(): Unit = {
        let letsDoIt = () -> do Test.Exp.TryWith.Print.doIt();
        try {
            letsDoIt()
        } with Print {
            def doIt(_cont) = ()
        }
    }

    @Test
    def testTryWith04(): Unit = {
        try {
            do Test.Exp.TryWith.Greet.greet("Alice")
        } with Greet {
            def greet(_, _cont) = ()
        }
    }

    @Test
    def testTryWith05(): Unit = {
        try {
            do Test.Exp.TryWith.Greet.greet("Duchess")
        } with Greet {
            def greet(name, _cont) = {
                try {
                    do Test.Exp.TryWith.Greet2.greet2(name)
                } with Greet2 {
                    def greet2(_, _cont) = ()
                }
            }
        }
    }

    @Test
    def testTryWith06(): Unit = {
        def greetAll(names: List[String]) = match names {
            case Nil => ()
            case hd :: tl => {
                do Test.Exp.TryWith.Greet.greet(hd);
                greetAll(tl)
            }
        };

        try {
            greetAll("Cheshire Cat" :: "Queen of Hearts" :: "White Rabbit" :: "Dormouse" :: Nil)
        } with Greet {
            def greet(_, _cont) = ()
        }
    }

    @Test
    def testTryWith07(): Unit = {
        try {
            try {
                do Test.Exp.TryWith.Print.doIt();
                do Test.Exp.TryWith.Print2.doIt2()
            } with Print {
                def doIt(_cont) = ()
            }
        } with Print2 {
            def doIt2(_cont) = ()
        }
    }

    @Test
    def testTryWith08(): Unit = {
        try {
            try {
                do Test.Exp.TryWith.Print.doIt()
            } with Print {
                def doIt(_cont) = ()
            };
            do Test.Exp.TryWith.Print2.doIt2()
        } with Print2 {
            def doIt2(_cont) = ()
        }
    }
}
