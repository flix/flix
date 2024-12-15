mod Test.Exp.RunWith {
    eff Print {
        def doIt(): Unit
    }

    eff Print2 {
        def doIt2(): Unit
    }

    eff Greet {
        def greet(name: String): Unit
    }

    eff Greet2 {
        def greet2(name: String): Unit
    }

    @Test
    def testRunWith01(): Unit = {
        run {
            Test.Exp.RunWith.Print.doIt()
        } with Print {
            def doIt(_cont) = ()
        }
    }

    @Test
    def testRunWith02(): Unit = {
        run {
            Test.Exp.RunWith.Print.doIt()
        } with Print {
            def doIt(_cont) = {
                run {
                    Test.Exp.RunWith.Print2.doIt2()
                } with Print2 {
                    def doIt2(_cont) = ()
                }
            }
        }
    }

    @Test
    def testRunWith03(): Unit = {
        let letsDoIt = () -> Test.Exp.RunWith.Print.doIt();
        run {
            letsDoIt()
        } with Print {
            def doIt(_cont) = ()
        }
    }

    @Test
    def testRunWith04(): Unit = {
        run {
            Test.Exp.RunWith.Greet.greet("Alice")
        } with Greet {
            def greet(_, _cont) = ()
        }
    }

    @Test
    def testRunWith05(): Unit = {
        run {
            Test.Exp.RunWith.Greet.greet("Duchess")
        } with Greet {
            def greet(name, _cont) = {
                run {
                    Test.Exp.RunWith.Greet2.greet2(name)
                } with Greet2 {
                    def greet2(_, _cont) = ()
                }
            }
        }
    }

    @Test
    def testRunWith06(): Unit = {
        def greetAll(names: List[String]) = match names {
            case Nil => ()
            case hd :: tl => {
                Test.Exp.RunWith.Greet.greet(hd);
                greetAll(tl)
            }
        };

        run {
            greetAll("Cheshire Cat" :: "Queen of Hearts" :: "White Rabbit" :: "Dormouse" :: Nil)
        } with Greet {
            def greet(_, _cont) = ()
        }
    }

    @Test
    def testRunWith07(): Unit = {
        run {
            Test.Exp.RunWith.Print.doIt()
        } with Print {
            def doIt(_cont) = ()
        } with Print {
            def doIt(_cont) = ()
        }
    }

    @Test
    def testRunWith08(): Unit = {
        run {
            Test.Exp.RunWith.Print.doIt()
        } with Print {
            def doIt(_cont) = {
                run {
                    Test.Exp.RunWith.Print2.doIt2()
                } with Print2 {
                    def doIt2(_cont) = ()
                } with Print2 {
                    def doIt2(_cont) = ()
                }
            }
        } with Print {
            def doIt(_cont) = ()
        }
    }

    @Test
    def testRunWith09(): Unit = {
        let f = () -> Test.Exp.RunWith.Print.doIt();
        run {
            f()
        } with Print {
            def doIt(_cont) = ()
        } with Print {
            def doIt(_cont) = ()
        }
    }

    @Test
    def testRunWith10(): Unit = {
        run {
            Test.Exp.RunWith.Greet.greet("Alice")
        } with Greet {
            def greet(_, _cont) = ()
        } with Greet {
            def greet(_, _cont) = ()
        }
    }

    @Test
    def testRunWith11(): Unit = {
        run {
            Test.Exp.RunWith.Greet.greet("Duchess")
        } with Greet {
            def greet(name, _cont) = {
                run {
                    Test.Exp.RunWith.Greet2.greet2(name)
                } with Greet2 {
                    def greet2(_, _cont) = ()
                }
            }
        } with Print { // Irrelevant effect but here to test the syntax
            def doIt(_) = ()
        }
    }

    @Test
    def testRunWith12(): Unit = {
        def greetAll(names: List[String]) = match names {
            case Nil => ()
            case hd :: tl => {
                Test.Exp.RunWith.Greet.greet(hd);
                greetAll(tl)
            }
        };

        run {
            greetAll("Cheshire Cat" :: "Queen of Hearts" :: "White Rabbit" :: "Dormouse" :: Nil)
        } with Greet {
            def greet(_, _cont) = ()
        } with Greet {
            def greet(_, _cont) = ()
        }
    }
}
