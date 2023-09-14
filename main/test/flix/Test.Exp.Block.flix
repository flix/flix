mod Test.Exp.Block {

    @Test
    def testBlock01(): Int32 = {42}

    @Test
    def testBlock02(): Int32 = {{42}}

    @Test
    def testBlock03(): Int32 = {{{42}}}

    @Test
    def testBlock04(): Int32 = {
        let x = {
            let y = 42;
                y
        };
        x + 1
    }

    @Test
    def testBlock05(): Int32 = {
        let x = {
            let y = 42;
            let z = 21;
                y + z
        };
        x + 1
    }

    @Test
    def testBlock06(): Int32 = {
        let x = {
            let y = 42;
            let z = 21;
                y + z
        };
        let w = {
            let y = 84;
            let z = 11;
            y + z
        };
        x + w
    }

    @Test
    def testBlock07(): Int32 = {
        let x = {42};
        let y = {21};
            x + y
    }

    @Test
    def testBlock08(): Int32 = {
        let x = {42};
        let y = {
            let z = 21;
            let w = {11};
                z + w
        };
            x + y
    }

    @Test
    def testBlock09(): Int32 = {
        let x = {42};
        let y = {
            let z = 21;
            let w = {
                let q = 11;
                let u = 5;
                    q + u
            };
                z + w
        };
            x + y
    }

}
