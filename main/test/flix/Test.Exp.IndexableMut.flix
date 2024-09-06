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
            a[4] = 200;
            a[0] == 200
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

}
