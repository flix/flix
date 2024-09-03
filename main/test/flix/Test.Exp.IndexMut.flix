mod Test.Exp.IndexMut {

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

}
