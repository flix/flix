instance ToString[Array[a, r]] with ToString[a] {
    type Aef = r + ToString.Aef[a]

    pub def toString(a: Array[a, r]): String \ r + ToString.Aef[a] = {
        Array.toString(a)
    }
}

instance ToString[MutDisjointSets[r, a]] with ToString[a] {
    type Aef = r + ToString.Aef[a]

    pub def toString(a: MutDisjointSets[r, a]): String \ r + ToString.Aef[a] = {
        "MutDisjointSets#{" +
            Foldable.foldLeft(
                (left, right) -> ToString.toString(left) + ", " + ToString.toString(right),
                "",
                a
            ) + "}"
    }
}

instance ToString[MutDeque[a, r]] with ToString[a] {
    type Aef = r + ToString.Aef[a]

    pub def toString(a: MutDeque[a, r]): String \ r + ToString.Aef[a] = {
        MutDeque.toString(a)
    }
}

instance ToString[MutQueue[a, r]] with ToString[a] {
    type Aef = r + ToString.Aef[a]

    pub def toString(a: MutQueue[a, r]): String \ r + ToString.Aef[a] = {
        MutQueue.toString(a)
    }
}

instance ToString[MutList[a, r]] with ToString[a] {
    type Aef = r + ToString.Aef[a]

    pub def toString(a: MutList[a, r]): String \ r + ToString.Aef[a] = {
        MutList.toString(a)
    }
}

instance ToString[MutMap[k, v, r]] with ToString[k], ToString[v] {
    type Aef = r + ToString.Aef[k] + ToString.Aef[v]

    pub def toString(a: MutMap[k, v, r]): String \ r + ToString.Aef[k] + ToString.Aef[v] = {
        MutMap.toString(a)
    }
}

instance ToString[MutSet[a, r]] with ToString[a] {
    type Aef = r + ToString.Aef[a]

    pub def toString(a: MutSet[a, r]): String \ r + ToString.Aef[a] = {
        MutSet.toString(a)
    }
}

instance ToString[StringBuilder[r]] {
    type Aef = r

    pub def toString(a: StringBuilder[r]): String \ r = {
        StringBuilder.toString(a)
    }
}

instance ToString[RedBlackTree[k, v]] with ToString[k], ToString[v] {
    type Aef = ToString.Aef[k] + ToString.Aef[v]

    pub def toString(a: RedBlackTree[k, v]): String \ ToString.Aef[k] + ToString.Aef[v] = {
           Map.Map(a) |> Map.toString
    }
}

instance ToString[Ref[a, r]] with ToString[a] {
    type Aef = ToString.Aef[a] + r

    pub def toString(a: Ref[a, r]): String \ ToString.Aef[a] + r = {
        ToString.toString(deref a)
    }
}
