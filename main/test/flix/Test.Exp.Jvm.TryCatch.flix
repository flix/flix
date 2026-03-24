mod Test.Exp.Jvm.TryCatch {

    use Assert.{success, fail}

    import java.lang.Math
    import java.lang.ArithmeticException
    import java.lang.IllegalArgumentException
    import java.lang.Exception

    @Test
    def testNoException01(): Unit \ Assert =
        try {
            success("no exception thrown")
        } catch {
            case _: Exception => fail("unexpected exception")
        }

    @Test
    def testSimpleException01(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: Exception => success("caught exception")
        }

    @Test
    def testMultipleExceptions01(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: ArithmeticException => success("caught ArithmeticException")
            case _: IllegalArgumentException => fail("wrong exception type")
        }

    @Test
    def testMultipleExceptions02(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: IllegalArgumentException => fail("wrong exception type")
            case _: ArithmeticException => success("caught ArithmeticException")
        }

    @Test
    def testMultipleExceptions03(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: IllegalArgumentException => fail("wrong exception type")
        } catch {
            case _: ArithmeticException => success("caught ArithmeticException")
        }

    @Test
    def testBinding01(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case e: ArithmeticException => if (e.toString().isEmpty()) fail("empty toString") else success("got exception message")
            case _: Exception => fail("wrong exception type")
        }

    @Test
    def testBinding02(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case e: ArithmeticException => if (e.getMessage().isEmpty()) fail("empty getMessage") else success("got exception message")
            case _: Exception => fail("wrong exception type")
        }

    @Test
    def testPriority01(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: Exception => success("caught by Exception handler first")
            case _: ArithmeticException => fail("should not reach ArithmeticException handler")
        }

    @Test
    def testPriority02(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: ArithmeticException => success("caught ArithmeticException")
            case _: Exception => fail("should not reach Exception handler")
        }

    @Test
    def testNesting01(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException => success("caught in inner handler")
            }
        } catch {
            case _: Exception => fail("should not reach outer handler")
        }
    }

    @Test
    def testNesting02(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: IllegalArgumentException => fail("wrong exception type")
            }
        } catch {
            case _: ArithmeticException => success("caught in outer handler")
        }
    }

    @Test
    def testNesting03(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException =>
                    exception();
                    fail("expected exception in handler")
            }
        } catch {
            case _: Exception => success("caught in outer Exception handler")
            case _: ArithmeticException => fail("should not reach outer ArithmeticException handler")
        }
    }

    @Test
    def testNesting04(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException =>
                    exception();
                    fail("expected exception in handler")
            }
        } catch {
            case _: ArithmeticException => success("caught in outer ArithmeticException handler")
            case _: Exception => fail("should not reach outer Exception handler")
        }
    }

    @Test
    def testNesting05(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException =>
                    exception();
                    fail("expected exception in handler")
                case _: Exception => fail("should not reach inner Exception handler")
            }
        } catch {
            case _: ArithmeticException => success("caught in outer ArithmeticException handler")
            case _: Exception => fail("should not reach outer Exception handler")
        }
    }

    //
    // Different return types (not just Unit \ Assert)
    //

    @Test
    def testReturnType01(): Unit \ Assert + IO =
        let result: Int32 =
            try {
                exception();
                0
            } catch {
                case _: ArithmeticException => 42
            };
        Assert.assertEq(expected = 42, result)

    @Test
    def testReturnType02(): Unit \ Assert + IO =
        let result: String =
            try {
                exception();
                "no exception"
            } catch {
                case _: ArithmeticException => "caught"
            };
        Assert.assertEq(expected = "caught", result)

    @Test
    def testReturnType03(): Unit \ Assert + IO =
        let result: Option[Int32] =
            try {
                exception();
                None
            } catch {
                case _: ArithmeticException => Some(42)
            };
        Assert.assertEq(expected = Some(42), result)

    @Test
    def testReturnType04(): Unit \ Assert + IO =
        let result: Result[String, Int32] =
            try {
                exception();
                Ok(0)
            } catch {
                case _: ArithmeticException => Err("caught")
            };
        Assert.assertErr(result)

    @Test
    def testReturnType05(): Unit \ Assert =
        let result: Int32 =
            try {
                42
            } catch {
                case _: Exception => 0
            };
        Assert.assertEq(expected = 42, result)

    @Test
    def testReturnType06(): Unit \ Assert + IO =
        let result: List[Int32] =
            try {
                exception();
                Nil
            } catch {
                case _: ArithmeticException => 1 :: 2 :: 3 :: Nil
            };
        Assert.assertEq(expected = 1 :: 2 :: 3 :: Nil, result)

    //
    // Throwing/catching from inside a catch handler
    //

    @Test
    def testThrowInCatch01(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException =>
                    exception();
                    fail("expected exception in catch handler")
            }
        } catch {
            case _: ArithmeticException => success("caught re-thrown exception from catch handler")
        }
    }

    @Test
    def testThrowInCatch02(): Unit \ Assert + IO = {
        let result: Int32 =
            try {
                try {
                    exception();
                    0
                } catch {
                    case _: ArithmeticException =>
                        exception();
                        0
                }
            } catch {
                case _: ArithmeticException => 99
            };
        Assert.assertEq(expected = 99, result)
    }

    @Test
    def testThrowInCatch03(): Unit \ Assert + IO = {
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: ArithmeticException =>
                try {
                    exception();
                    fail("expected exception in nested try")
                } catch {
                    case _: ArithmeticException => success("caught in nested try inside catch")
                }
        }
    }

    @Test
    def testThrowInCatch04(): Unit \ Assert + IO = {
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: ArithmeticException =>
                try {
                    success("no exception in nested try")
                } catch {
                    case _: Exception => fail("should not reach nested catch")
                }
        }
    }

    @Test
    def testThrowInCatch05(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: IllegalArgumentException =>
                    fail("wrong exception type in inner catch")
            }
        } catch {
            case _: ArithmeticException =>
                try {
                    exception();
                    fail("expected exception in outer catch")
                } catch {
                    case _: ArithmeticException => success("caught in nested try inside outer catch")
                }
        }
    }

    //
    // Sequential (non-nested) try-catches
    //

    @Test
    def testSequential01(): Unit \ Assert + IO = {
        try {
            exception();
            fail("expected exception in first try")
        } catch {
            case _: ArithmeticException => success("caught in first try")
        };
        try {
            exception();
            fail("expected exception in second try")
        } catch {
            case _: ArithmeticException => success("caught in second try")
        }
    }

    @Test
    def testSequential02(): Unit \ Assert + IO = {
        try {
            success("no exception in first try")
        } catch {
            case _: Exception => fail("should not reach first catch")
        };
        try {
            exception();
            fail("expected exception in second try")
        } catch {
            case _: ArithmeticException => success("caught in second try")
        }
    }

    @Test
    def testSequential03(): Unit \ Assert + IO = {
        try {
            exception();
            fail("expected exception in first try")
        } catch {
            case _: ArithmeticException => success("caught in first try")
        };
        try {
            success("no exception in second try")
        } catch {
            case _: Exception => fail("should not reach second catch")
        }
    }

    @Test
    def testSequential04(): Unit \ Assert = {
        try {
            success("no exception in first try")
        } catch {
            case _: Exception => fail("should not reach first catch")
        };
        try {
            success("no exception in second try")
        } catch {
            case _: Exception => fail("should not reach second catch")
        }
    }

    @Test
    def testSequential05(): Unit \ Assert + IO = {
        try {
            exception();
            fail("expected exception in first try")
        } catch {
            case _: ArithmeticException => success("caught in first try")
        };
        try {
            exception();
            fail("expected exception in second try")
        } catch {
            case _: ArithmeticException => success("caught in second try")
        };
        try {
            exception();
            fail("expected exception in third try")
        } catch {
            case _: ArithmeticException => success("caught in third try")
        }
    }

    @Test
    def testSequential06(): Unit \ Assert + IO = {
        let a: Int32 =
            try {
                exception();
                0
            } catch {
                case _: ArithmeticException => 1
            };
        let b: Int32 =
            try {
                exception();
                0
            } catch {
                case _: ArithmeticException => 2
            };
        Assert.assertEq(expected = 3, a + b)
    }

    @Test
    def testSequential07(): Unit \ Assert + IO = {
        let a: String =
            try {
                exception();
                ""
            } catch {
                case _: ArithmeticException => "hello"
            };
        let b: String =
            try {
                exception();
                ""
            } catch {
                case _: ArithmeticException => " world"
            };
        Assert.assertEq(expected = "hello world", String.concat(a, b))
    }

    def exception(): Unit \ IO =
        discard Math.floorDiv(1, 0);
        ()

}
