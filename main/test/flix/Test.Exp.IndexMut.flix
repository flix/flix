mod Test.Exp.IndexMut {

    pub def testIndexMutArray01(): Bool \ IndexOutOfBounds + IO = {
        let arr = Array#{false} @ Static;
        arr[0] = true;
        arr[0]
    }

    pub def testIndexMutMutMap01(): Bool \ IndexOutOfBounds + IO = {
        let map = Map#{} |> Map.toMutMap(Static);
        map["x"] = true;
        map["x"]
    }

    pub def testIndexMutMutList01(): Bool \ IndexOutOfBounds + IO = {
        let list = List#{false} |> List.toMutList(Static);
        list[0] = true;
        list[0]
    }
}
