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
            a[-1] = 4;
            false
        } with OutOfBounds {
            def outOfBounds(_, _) = true
        }
    }

    @Test
    def array03(): Bool = region rc {
        try {
            let a = Array#{0, 1, 2, 3} @ rc;
            a[5] = 4;
            false
        } with OutOfBounds {
            def outOfBounds(_, _) = true
        }
    }

}
