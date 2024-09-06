mod Test.Exp.IndexableMut {

    @Test
    def array01(): Bool = region rc {
        try {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[0] = 4;
            a[0] == 4
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def array02(): Bool = region rc {
        try {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[1] = 42;
            a[1] == 42
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def array03(): Bool = region rc {
        try {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[3] = 200;
            a[3] == 200
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def array04(): Bool = region rc {
        try {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[-1] = 4;
            false
        } with OutOfBounds {
            def outOfBounds(_, _) = true
        }
    }

    @Test
    def array05(): Bool = region rc {
        try {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[5] = 4;
            false
        } with OutOfBounds {
            def outOfBounds(_, _) = true
        }
    }

    @Test
    def mutList01(): Bool = region rc {
        try {
            let l = Vector#{0, 1, 2, 3} |> Vector.toMutList(rc);
            l[0] = 4;
            l[0] == 4
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def mutList02(): Bool = region rc {
        try {
            let l = Vector#{0, 1, 2, 3} |> Vector.toMutList(rc);
            l[1] = 42;
            l[1] == 42
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def mutList03(): Bool = region rc {
        try {
            let l = Vector#{0, 1, 2, 3} |> Vector.toMutList(rc);
            l[4] = 200;
            l[0] == 200
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def mutList04(): Bool = region rc {
        try {
            let l = Vector#{0, 1, 2, 3} |> Vector.toMutList(rc);
            l[-1] = 4;
            false
        } with OutOfBounds {
            def outOfBounds(_, _) = true
        }
    }

    @Test
    def mutList05(): Bool = region rc {
        try {
            let l = Vector#{0, 1, 2, 3} |> Vector.toMutList(rc);
            l[5] = 4;
            false
        } with OutOfBounds {
            def outOfBounds(_, _) = true
        }
    }

    @Test
    def mutMap01(): Bool = region rc {
        try {
            let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4} |> Map.toMutMap(rc);
            m[0] = -1;
            m[0] == -1
        } with KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }

    @Test
    def mutMap02(): Bool = region rc {
        try {
            let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4} |> Map.toMutMap(rc);
            m[1] = 42;
            m[1] == 42
        } with KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }

    @Test
    def mutMap03(): Bool = region rc {
        try {
            let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4} |> Map.toMutMap(rc);
            m[2] = 200;
            m[2] == 200
        } with KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }

    @Test
    def mutMap04(): Bool = region rc {
        try {
            let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4} |> Map.toMutMap(rc);
            m[3] = 1234;
            m[3] == 1234
        } with KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }

    @Test
    def mutMap05(): Bool = region rc {
        try {
            let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4} |> Map.toMutMap(rc);
            m[4] = -1;
            false
        } with KeyNotFound {
            def keyNotFound(_, _) = true
        }
    }

    @Test
    def mutMap06(): Bool = region rc {
        try {
            let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4} |> Map.toMutMap(rc);
            m[-1] = -1;
            false
        } with KeyNotFound {
            def keyNotFound(_, _) = true
        }
    }

    @Test
    def record01(): Bool = region rc {
        try {
            let r = { a = Array#{0, 1, 2, 3} @ rc };
            r#a[1] = 2;
            r#a[1] == 2
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def record02(): Bool = region rc {
        try {
            let r = { r = { a = Array#{0, 1, 2, 3} @ rc } };
            r#r#a[3] = 2;
            r#r#a[3] == 2
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def record03(): Bool = region rc {
        try {
            let r = { r = { a = Array#{{ a = Array#{1, 2, 3, 4} @ rc } } @ rc } };
            r#r#a[0]#a[1] = 42;
            r#r#a[0]#a[1] == 42
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def record04(): Bool = region rc {
        try {
            let r = { r = { a = Array#{Array#{} @ rc, Array#{{ a = Array#{1, 2, 3, 4} @ rc }} @ rc } @ rc } };
            r#r#a[1][0]#a[1] = -1;
            r#r#a[1][0]#a[1] == -1
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @Test
    def record05(): Bool = region rc {
        try {
            let r = { r = { a = Array#{Array#{} @ rc, Array#{{ a = Array#{1, 2, 3, 4} @ rc }} @ rc } @ rc } };
            r#r#a[1] = Array#{} @ rc;
            r#r#a[1][0]#a[1] == -1
        } with OutOfBounds {
            def outOfBounds(_, _) = true
        }
    }

}
