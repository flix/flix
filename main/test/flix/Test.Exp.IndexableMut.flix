mod Test.Exp.IndexableMut {

    use Assert.assertTrue

    @Test
    def array01(): Unit \ Assert = region rc {
        assertTrue(run {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[0] = 4;
            a[0] == 4
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def array02(): Unit \ Assert = region rc {
        assertTrue(run {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[1] = 42;
            a[1] == 42
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def array03(): Unit \ Assert = region rc {
        assertTrue(run {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[3] = 200;
            a[3] == 200
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def array04(): Unit \ Assert = region rc {
        assertTrue(run {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[-1] = 4;
            false
        } with handler OutOfBounds {
            def outOfBounds(_, _) = true
        })
    }

    @Test
    def array05(): Unit \ Assert = region rc {
        assertTrue(run {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[5] = 4;
            false
        } with handler OutOfBounds {
            def outOfBounds(_, _) = true
        })
    }

    @Test
    def mutList01(): Unit \ Assert = region rc {
        assertTrue(run {
            let l = MutList.empty(rc);
            MutList.pushAll(List#{0, 1, 2, 3}, l);
            l[0] = 4;
            l[0] == 4
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def mutList02(): Unit \ Assert = region rc {
        assertTrue(run {
            let l = MutList.empty(rc);
            MutList.pushAll(List#{0, 1, 2, 3}, l);
            l[1] = 42;
            l[1] == 42
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def mutList03(): Unit \ Assert = region rc {
        assertTrue(run {
            let l = MutList.empty(rc);
            MutList.pushAll(List#{0, 1, 2, 3}, l);
            l[3] = 200;
            l[3] == 200
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def mutList04(): Unit \ Assert = region rc {
        assertTrue(run {
            let l = MutList.empty(rc);
            MutList.pushAll(List#{0, 1, 2, 3}, l);
            l[-1] = 4;
            false
        } with handler OutOfBounds {
            def outOfBounds(_, _) = true
        })
    }

    @Test
    def mutList05(): Unit \ Assert = region rc {
        assertTrue(run {
            let l = MutList.empty(rc);
            MutList.pushAll(List#{0, 1, 2, 3}, l);
            l[5] = 4;
            false
        } with handler OutOfBounds {
            def outOfBounds(_, _) = true
        })
    }

    @Test
    def mutMap01(): Unit \ Assert = region rc {
        assertTrue(run {
            let m = MutMap.empty(rc);
            MutMap.putAll(List#{(0, 1), (1, 2), (2, 3), (3, 4)}, m);
            m[0] = -1;
            m[0] == -1
        } with handler KeyNotFound {
            def keyNotFound(_, _) = false
        })
    }

    @Test
    def mutMap02(): Unit \ Assert = region rc {
        assertTrue(run {
            let m = MutMap.empty(rc);
            MutMap.putAll(List#{(0, 1), (1, 2), (2, 3), (3, 4)}, m);
            m[1] = 42;
            m[1] == 42
        } with handler KeyNotFound {
            def keyNotFound(_, _) = false
        })
    }

    @Test
    def mutMap03(): Unit \ Assert = region rc {
        assertTrue(run {
            let m = MutMap.empty(rc);
            MutMap.putAll(List#{(0, 1), (1, 2), (2, 3), (3, 4)}, m);
            m[2] = 200;
            m[2] == 200
        } with handler KeyNotFound {
            def keyNotFound(_, _) = false
        })
    }

    @Test
    def mutMap04(): Unit \ Assert = region rc {
        assertTrue(run {
            let m = MutMap.empty(rc);
            MutMap.putAll(List#{(0, 1), (1, 2), (2, 3), (3, 4)}, m);
            m[3] = 1234;
            m[3] == 1234
        } with handler KeyNotFound {
            def keyNotFound(_, _) = false
        })
    }

    @Test
    def mutMap05(): Unit \ Assert = region rc {
        assertTrue(run {
            let m = MutMap.empty(rc);
            MutMap.putAll(List#{(0, 1), (1, 2), (2, 3), (3, 4)}, m);
            m[4] = -1;
            m[4] == -1
        } with handler KeyNotFound {
            def keyNotFound(_, _) = false
        })
    }

    @Test
    def mutMap06(): Unit \ Assert = region rc {
        assertTrue(run {
            let m = MutMap.empty(rc);
            MutMap.putAll(List#{(0, 1), (1, 2), (2, 3), (3, 4)}, m);
            m[-1] = -1;
            m[-1] == -1
        } with handler KeyNotFound {
            def keyNotFound(_, _) = false
        })
    }

    @Test
    def record01(): Unit \ Assert = region rc {
        assertTrue(run {
            let r = { a = Array#{0, 1, 2, 3} @ rc };
            r#a[1] = 2;
            r#a[1] == 2
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def record02(): Unit \ Assert = region rc {
        assertTrue(run {
            let r = { r = { a = Array#{0, 1, 2, 3} @ rc } };
            r#r#a[3] = 2;
            r#r#a[3] == 2
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def record03(): Unit \ Assert = region rc {
        assertTrue(run {
            let r = { r = { a = Array#{{ a = Array#{1, 2, 3, 4} @ rc } } @ rc } };
            r#r#a[0]#a[1] = 42;
            r#r#a[0]#a[1] == 42
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def record04(): Unit \ Assert = region rc {
        assertTrue(run {
            let r = { r = { a = Array#{Array#{} @ rc, Array#{{ a = Array#{1, 2, 3, 4} @ rc }} @ rc } @ rc } };
            r#r#a[1][0]#a[1] = -1;
            r#r#a[1][0]#a[1] == -1
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def record05(): Unit \ Assert = region rc {
        assertTrue(run {
            let r = { r = { a = Array#{Array#{} @ rc, Array#{{ a = Array#{1, 2, 3, 4} @ rc }} @ rc } @ rc } };
            r#r#a[1] = Array#{} @ rc;
            r#r#a[1][0]#a[1] == -1
        } with handler OutOfBounds {
            def outOfBounds(_, _) = true
        })
    }


    @Test
    def multiple01(): Unit \ Assert = region rc {
        assertTrue(run {
            let a = Array#{Array#{Array#{1, 2, 3, 4} @ rc } @ rc } @ rc;
            a[0][0][2] = 42;
            a[0][0][2] == 42
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def multiple02(): Unit \ Assert = region rc {
        assertTrue(run {
            let a = Array#{Array#{Array#{1, 2, 3, 4} @ rc } @ rc } @ rc;
            a[0][0] = Array#{42} @ rc;
            a[0][0][0] == 42
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def multiple03(): Unit \ Assert = region rc {
        assertTrue(run {
            let a = Array#{Array#{Array#{1, 2, 3, 4} @ rc } @ rc } @ rc;
            a[0][0] = Array#{42} @ rc;
            a[0][0][2] == 42
        } with handler OutOfBounds {
            def outOfBounds(_, _) = true
        })
    }

    @Test
    def genWithRecord01(): Unit \ Assert = region rc {
        assertTrue(run {
            f()()#a[0]()[0]#r#g(rc)[2] = 42;
            true
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def genWithRecord02(): Unit \ Assert = region rc {
        assertTrue(run {
            f()()#a[0]()[0]#r#g(rc)[2] = 42;
            true
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        })
    }

    @Test
    def genWithRecord03(): Unit \ Assert = region rc {
        assertTrue(run {
            f()()#a[0]()[0]#r#g(rc)[-1] = 42;
            false
        } with handler OutOfBounds {
            def outOfBounds(_, _) = true
        })
    }

    def f(): Unit -> { a = Vector[Unit -> Vector[{ r = { g = Region[r] -> Array[Int32, r] \ r } }]] } =
        () -> {
            a = Vector#{() -> Vector#{{ r = { g = (rc: Region[r]) -> Array#{1, 2, 3, 4} @ rc } }} }
        }

}
