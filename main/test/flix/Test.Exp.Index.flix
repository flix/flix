mod Test.Exp.Index {


    pub def testIndexVector01(): Bool \ IndexOutOfBounds = {
        Vector#{true}[0]
    }

    pub def testIndexList01(): Bool \ IndexOutOfBounds = {
        List#{true}[0]
    }

    pub def testIndexArray01(): Bool \ IndexOutOfBounds + IO = {
        (Array#{true} @ Static)[0]
    }

    pub def testIndexArray02(): Bool \ IndexOutOfBounds = region r {
        (Array#{true} @ r)[0]
    }

    pub def testIndexMap01(): Bool \ IndexOutOfBounds = {
        Map#{0 => true}[0]
    }

    pub def testIndexMutMap01(): Bool \ IndexOutOfBounds + IO = {
        (Map#{0 => true} |> Map.toMutMap(Static))[0]
    }

    pub def testIndexMutMap02(): Bool \ IndexOutOfBounds = region r {
        (Map#{0 => true} |> Map.toMutMap(r))[0]
    }
}
