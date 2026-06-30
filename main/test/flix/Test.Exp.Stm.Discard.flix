///
/// Tests for Destination-Driven Code Generation.
///
/// The compiler optimizes statement expressions whose results are discarded.
/// When a unit-returning operation appears in statement position (i.e. its
/// result is unused), the compiler can skip generating the unnecessary
/// `getstatic Unit$.INSTANCE; pop` bytecode pair.
///
mod Test.Exp.StmDiscard {

    import java.lang.ArithmeticException
    import java.lang.Exception
    import java.util.ArrayList

    use Assert.{assertEq, assertTrue}

    // -----------------------------------------------------------------------
    // 1. Array store discard — ArrayStore returns Unit
    // -----------------------------------------------------------------------

    @Test
    def testDiscardArrayStore01(): Unit \ Assert = region rc {
        let a = Array#{10, 20, 30} @ rc;
        %%ARRAY_STORE%%(a, 1, 99);
        assertEq(expected = 99, %%ARRAY_LOAD%%(a, 1))
    }

    @Test
    def testDiscardArrayStore02(): Unit \ Assert = region rc {
        let a = Array#{0, 0, 0} @ rc;
        %%ARRAY_STORE%%(a, 0, 1);
        %%ARRAY_STORE%%(a, 1, 2);
        %%ARRAY_STORE%%(a, 2, 3);
        assertEq(expected = 6,
            %%ARRAY_LOAD%%(a, 0) + %%ARRAY_LOAD%%(a, 1) + %%ARRAY_LOAD%%(a, 2))
    }

    @Test
    def testDiscardArrayStore03(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let _u = { %%ARRAY_STORE%%(a, 0, 42); () };
        assertEq(expected = 42, %%ARRAY_LOAD%%(a, 0))
    }

    // -----------------------------------------------------------------------
    // 2. Void Java method call discard — InvokeMethod returning void
    // -----------------------------------------------------------------------

    @Test
    def testDiscardVoidMethod01(): Unit \ Assert + IO =
        let al = new ArrayList();
        al.add("hello");
        al.add("world");
        al.clear();
        assertEq(expected = 0, al.size())

    @Test
    def testDiscardVoidMethod02(): Unit \ Assert + IO =
        let al = new ArrayList();
        al.add("a");
        al.add("b");
        al.clear();
        al.add("c");
        assertEq(expected = 1, al.size())

    @Test
    def testDiscardVoidMethod03(): Unit \ Assert + IO =
        let al = new ArrayList();
        al.add("x");
        let _u = { al.clear(); () };
        assertEq(expected = 0, al.size())

    // -----------------------------------------------------------------------
    // 3. If-then-else with discardable branches
    // -----------------------------------------------------------------------

    @Test
    def testDiscardIfThenElse01(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let cond = true;
        (if (cond) %%ARRAY_STORE%%(a, 0, 1) else %%ARRAY_STORE%%(a, 0, 2));
        assertEq(expected = 1, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardIfThenElse02(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let cond = false;
        (if (cond) %%ARRAY_STORE%%(a, 0, 1) else %%ARRAY_STORE%%(a, 0, 2));
        assertEq(expected = 2, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardIfThenElse03(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let x = 2;
        (if (x == 1) %%ARRAY_STORE%%(a, 0, 10)
         else if (x == 2) %%ARRAY_STORE%%(a, 0, 20)
         else %%ARRAY_STORE%%(a, 0, 30));
        assertEq(expected = 20, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardIfThenElse04(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let x = 3;
        (if (x == 1) %%ARRAY_STORE%%(a, 0, 10)
         else if (x == 2) %%ARRAY_STORE%%(a, 0, 20)
         else %%ARRAY_STORE%%(a, 0, 30));
        assertEq(expected = 30, %%ARRAY_LOAD%%(a, 0))
    }

    // -----------------------------------------------------------------------
    // 4. Let forwarding
    // -----------------------------------------------------------------------

    @Test
    def testDiscardLet01(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let _u = {
            let _x = 1;
            %%ARRAY_STORE%%(a, 0, 77)
        };
        assertEq(expected = 77, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardLet02(): Unit \ Assert =
        let x = {
            let a = 1;
            let b = 2;
            a + b
        };
        assertEq(expected = 3, x)

    @Test
    def testDiscardLet03(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let _v = 42;
        %%ARRAY_STORE%%(a, 0, 77);
        assertEq(expected = 77, %%ARRAY_LOAD%%(a, 0))
    }

    // -----------------------------------------------------------------------
    // 5. Match/Switch forwarding
    // -----------------------------------------------------------------------

    @Test
    def testDiscardMatch01(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let opt: Option[Int32] = Some(42);
        (match opt {
            case Some(v) => %%ARRAY_STORE%%(a, 0, v)
            case None    => %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = 42, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardMatch02(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let opt: Option[Int32] = None;
        (match opt {
            case Some(v) => %%ARRAY_STORE%%(a, 0, v)
            case None    => %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = -1, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardMatch03(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let res: Result[String, Int32] = Ok(99);
        (match res {
            case Ok(v)  => %%ARRAY_STORE%%(a, 0, v)
            case Err(_) => %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = 99, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardMatch04(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let res: Result[String, Int32] = Err("oops");
        (match res {
            case Ok(v)  => %%ARRAY_STORE%%(a, 0, v)
            case Err(_) => %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = -1, %%ARRAY_LOAD%%(a, 0))
    }

    // -----------------------------------------------------------------------
    // 6. Try-catch forwarding
    // -----------------------------------------------------------------------

    @Test
    def testDiscardTryCatch01(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        (try {
            %%ARRAY_STORE%%(a, 0, 42)
        } catch {
            case _: Exception => %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = 42, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardTryCatch02(): Unit \ Assert + IO = region rc {
        let a = Array#{0} @ rc;
        (try {
            throwArithmeticException();
            %%ARRAY_STORE%%(a, 0, 1)
        } catch {
            case _: ArithmeticException => %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = -1, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardTryCatch03(): Unit \ Assert =
        let x = try { 42 } catch { case _: Exception => -1 };
        assertEq(expected = 42, x)

    @Test
    def testDiscardTryCatch04(): Unit \ Assert + IO =
        let x = try {
            throwArithmeticException();
            1
        } catch {
            case _: ArithmeticException => -1
        };
        assertEq(expected = -1, x)

    // -----------------------------------------------------------------------
    // 7. Region forwarding
    // -----------------------------------------------------------------------

    @Test
    def testDiscardRegion01(): Unit \ Assert =
        let x = region rc {
            let a = Array#{0} @ rc;
            %%ARRAY_STORE%%(a, 0, 42);
            %%ARRAY_LOAD%%(a, 0)
        };
        assertEq(expected = 42, x)

    @Test
    def testDiscardRegion02(): Unit \ Assert =
        let x = region rc {
            let a = Array#{7} @ rc;
            %%ARRAY_LOAD%%(a, 0)
        };
        assertEq(expected = 7, x)

    // -----------------------------------------------------------------------
    // 8. Nested combinations
    // -----------------------------------------------------------------------

    @Test
    def testDiscardNested01(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let cond = true;
        (if (cond) {
            let _v = 1;
            %%ARRAY_STORE%%(a, 0, 10)
        } else {
            let _v = 2;
            %%ARRAY_STORE%%(a, 0, 20)
        });
        assertEq(expected = 10, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardNested02(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let opt: Option[Int32] = Some(5);
        let cond = true;
        (if (cond) {
            match opt {
                case Some(v) => %%ARRAY_STORE%%(a, 0, v)
                case None    => %%ARRAY_STORE%%(a, 0, 0)
            }
        } else {
            %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = 5, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardNested03(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let opt: Option[Int32] = Some(42);
        (match opt {
            case Some(v) =>
                try {
                    %%ARRAY_STORE%%(a, 0, v)
                } catch {
                    case _: Exception => %%ARRAY_STORE%%(a, 0, -1)
                }
            case None => %%ARRAY_STORE%%(a, 0, -2)
        });
        assertEq(expected = 42, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardNested04(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let x = 10;
        ({
            let cond = x > 0;
            if (cond) {
                let opt: Option[Int32] = Some(x);
                match opt {
                    case Some(v) => %%ARRAY_STORE%%(a, 0, v)
                    case None    => %%ARRAY_STORE%%(a, 0, 0)
                }
            } else {
                %%ARRAY_STORE%%(a, 0, -1)
            }
        });
        assertEq(expected = 10, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testDiscardNested05(): Unit \ Assert = region rc {
        let a = Array#{0, 0, 0} @ rc;
        %%ARRAY_STORE%%(a, 0, 1);
        (if (true) %%ARRAY_STORE%%(a, 1, 2) else %%ARRAY_STORE%%(a, 1, 0));
        (match Some(3) {
            case Some(v) => %%ARRAY_STORE%%(a, 2, v)
            case None    => %%ARRAY_STORE%%(a, 2, 0)
        });
        assertEq(expected = 6,
            %%ARRAY_LOAD%%(a, 0) + %%ARRAY_LOAD%%(a, 1) + %%ARRAY_LOAD%%(a, 2))
    }

    // -----------------------------------------------------------------------
    // 9. Non-discardable expressions — fallback to push+pop
    // -----------------------------------------------------------------------

    @Test
    def testNonDiscardable01(): Unit \ Assert + IO =
        let al = new ArrayList();
        al.add("a");
        discard al.size();
        assertEq(expected = 1, al.size())

    @Test
    def testNonDiscardable02(): Unit \ Assert + IO =
        let al = new ArrayList();
        al.add("a");
        al.add("b");
        discard al.toString();
        assertEq(expected = 2, al.size())

    @Test
    def testNonDiscardable03(): Unit \ Assert + IO =
        let al = new ArrayList();
        discard al.isEmpty();
        assertEq(expected = 0, al.size())

    @Test
    def testNonDiscardable04(): Unit \ Assert + IO = region rc {
        let a = Array#{0} @ rc;
        let al = new ArrayList();
        %%ARRAY_STORE%%(a, 0, 10);
        discard al.size();
        assertEq(expected = 10, %%ARRAY_LOAD%%(a, 0))
    }

    // -----------------------------------------------------------------------
    // 10. Side effects are preserved
    // -----------------------------------------------------------------------

    @Test
    def testSideEffectsPreserved01(): Unit \ Assert = region rc {
        let a = Array#{0, 0, 0, 0, 0} @ rc;
        %%ARRAY_STORE%%(a, 0, 1);
        %%ARRAY_STORE%%(a, 1, 2);
        %%ARRAY_STORE%%(a, 2, 3);
        %%ARRAY_STORE%%(a, 3, 4);
        %%ARRAY_STORE%%(a, 4, 5);
        assertEq(expected = 15,
            %%ARRAY_LOAD%%(a, 0) + %%ARRAY_LOAD%%(a, 1) + %%ARRAY_LOAD%%(a, 2) +
            %%ARRAY_LOAD%%(a, 3) + %%ARRAY_LOAD%%(a, 4))
    }

    @Test
    def testSideEffectsPreserved02(): Unit \ Assert + IO =
        let al = new ArrayList();
        al.add("a");
        al.add("b");
        al.add("c");
        al.add("d");
        al.add("e");
        assertEq(expected = 5, al.size())

    @Test
    def testSideEffectsPreserved03(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        (if (true)
            %%ARRAY_STORE%%(a, 0, 42)
        else
            %%ARRAY_STORE%%(a, 0, -1));
        assertEq(expected = 42, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testSideEffectsPreserved04(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        let opt: Option[Int32] = Some(77);
        (match opt {
            case Some(v) => %%ARRAY_STORE%%(a, 0, v)
            case None    => %%ARRAY_STORE%%(a, 0, 0)
        });
        assertEq(expected = 77, %%ARRAY_LOAD%%(a, 0))
    }

    @Test
    def testSideEffectsPreserved05(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        (try {
            %%ARRAY_STORE%%(a, 0, 55)
        } catch {
            case _: Exception => %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = 55, %%ARRAY_LOAD%%(a, 0))
    }

    // -----------------------------------------------------------------------
    // 11. Mixed: array store + void Java call
    // -----------------------------------------------------------------------

    @Test
    def testDiscardMixed01(): Unit \ Assert + IO = region rc {
        let a = Array#{0} @ rc;
        let al = new ArrayList();
        %%ARRAY_STORE%%(a, 0, 11);
        al.add("x");
        al.clear();
        assertEq(expected = 11, %%ARRAY_LOAD%%(a, 0));
        assertEq(expected = 0, al.size())
    }

    @Test
    def testDiscardMixed02(): Unit \ Assert + IO = region rc {
        let a = Array#{0} @ rc;
        let al = new ArrayList();
        %%ARRAY_STORE%%(a, 0, 1);
        al.add("hello");
        discard al.size();
        assertEq(expected = 1, %%ARRAY_LOAD%%(a, 0));
        assertEq(expected = 1, al.size())
    }

    // -----------------------------------------------------------------------
    // 12. Long chains of discardable statements
    // -----------------------------------------------------------------------

    @Test
    def testDiscardChain01(): Unit \ Assert = region rc {
        let a = Array#{0, 0, 0, 0, 0, 0, 0, 0, 0, 0} @ rc;
        %%ARRAY_STORE%%(a, 0, 1);
        %%ARRAY_STORE%%(a, 1, 2);
        %%ARRAY_STORE%%(a, 2, 3);
        %%ARRAY_STORE%%(a, 3, 4);
        %%ARRAY_STORE%%(a, 4, 5);
        %%ARRAY_STORE%%(a, 5, 6);
        %%ARRAY_STORE%%(a, 6, 7);
        %%ARRAY_STORE%%(a, 7, 8);
        %%ARRAY_STORE%%(a, 8, 9);
        %%ARRAY_STORE%%(a, 9, 10);
        assertEq(expected = 55,
            %%ARRAY_LOAD%%(a, 0) + %%ARRAY_LOAD%%(a, 1) + %%ARRAY_LOAD%%(a, 2) +
            %%ARRAY_LOAD%%(a, 3) + %%ARRAY_LOAD%%(a, 4) + %%ARRAY_LOAD%%(a, 5) +
            %%ARRAY_LOAD%%(a, 6) + %%ARRAY_LOAD%%(a, 7) + %%ARRAY_LOAD%%(a, 8) +
            %%ARRAY_LOAD%%(a, 9))
    }

    @Test
    def testDiscardChain02(): Unit \ Assert + IO = region rc {
        let a = Array#{0} @ rc;
        let al = new ArrayList();
        al.add("x");
        %%ARRAY_STORE%%(a, 0, 10);
        al.clear();
        assertEq(expected = 10, %%ARRAY_LOAD%%(a, 0));
        assertEq(expected = 0, al.size())
    }

    // -----------------------------------------------------------------------
    // 13. Expressions whose result IS used (Stack destination)
    // -----------------------------------------------------------------------

    @Test
    def testResultUsed01(): Unit \ Assert =
        let x = if (true) 1 else 2;
        assertEq(expected = 1, x)

    @Test
    def testResultUsed02(): Unit \ Assert =
        let x = match Some(42) {
            case Some(v) => v
            case None    => -1
        };
        assertEq(expected = 42, x)

    @Test
    def testResultUsed03(): Unit \ Assert =
        let x = try { 42 } catch { case _: Exception => -1 };
        assertEq(expected = 42, x)

    @Test
    def testResultUsed04(): Unit \ Assert =
        let x = region rc {
            let a = Array#{99} @ rc;
            %%ARRAY_LOAD%%(a, 0)
        };
        assertEq(expected = 99, x)

    @Test
    def testResultUsed05(): Unit \ Assert =
        let x = { let a = 10; let b = 20; a + b };
        assertEq(expected = 30, x)

    // -----------------------------------------------------------------------
    // 14. Stm in tail position
    // -----------------------------------------------------------------------

    @Test
    def testStmInTail01(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        if (true) {
            %%ARRAY_STORE%%(a, 0, 42);
            assertEq(expected = 42, %%ARRAY_LOAD%%(a, 0))
        } else {
            assertEq(expected = 0, 1)
        }
    }

    @Test
    def testStmInTail02(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        match Some(42) {
            case Some(v) =>
                %%ARRAY_STORE%%(a, 0, v);
                assertEq(expected = 42, %%ARRAY_LOAD%%(a, 0))
            case None =>
                assertEq(expected = 0, 1)
        }
    }

    @Test
    def testStmInTail03(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        try {
            %%ARRAY_STORE%%(a, 0, 42);
            assertEq(expected = 42, %%ARRAY_LOAD%%(a, 0))
        } catch {
            case _: Exception => assertEq(expected = 0, 1)
        }
    }

    // -----------------------------------------------------------------------
    // 15. Complex real-world patterns
    // -----------------------------------------------------------------------

    @Test
    def testRealWorld01(): Unit \ Assert = region rc {
        let data = Array#{0, 0, 0} @ rc;
        let idx = Ref.fresh(rc, 0);
        %%ARRAY_STORE%%(data, Ref.get(idx), 10);
        Ref.put(Ref.get(idx) + 1, idx);
        %%ARRAY_STORE%%(data, Ref.get(idx), 20);
        Ref.put(Ref.get(idx) + 1, idx);
        %%ARRAY_STORE%%(data, Ref.get(idx), 30);
        assertEq(expected = 10, %%ARRAY_LOAD%%(data, 0));
        assertEq(expected = 20, %%ARRAY_LOAD%%(data, 1));
        assertEq(expected = 30, %%ARRAY_LOAD%%(data, 2))
    }

    @Test
    def testRealWorld02(): Unit \ Assert = region rc {
        let a = Array#{0, 0} @ rc;
        let cond = true;
        (if (cond) {
            %%ARRAY_STORE%%(a, 0, 10)
        } else {
            %%ARRAY_STORE%%(a, 0, -2)
        });
        (match Some(20) {
            case Some(v) =>
                (if (v > 0) %%ARRAY_STORE%%(a, 1, v) else %%ARRAY_STORE%%(a, 1, 0))
            case None =>
                %%ARRAY_STORE%%(a, 1, -1)
        });
        assertEq(expected = 10, %%ARRAY_LOAD%%(a, 0));
        assertEq(expected = 20, %%ARRAY_LOAD%%(a, 1))
    }

    @Test
    def testRealWorld03(): Unit \ Assert = region rc {
        let a = Array#{0} @ rc;
        (try {
            (match Some(99) {
                case Some(v) => %%ARRAY_STORE%%(a, 0, v)
                case None    => %%ARRAY_STORE%%(a, 0, 0)
            })
        } catch {
            case _: Exception => %%ARRAY_STORE%%(a, 0, -1)
        });
        assertEq(expected = 99, %%ARRAY_LOAD%%(a, 0))
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    def throwArithmeticException(): Unit \ IO =
        checked_ecast(throw new ArithmeticException("test"))

}
