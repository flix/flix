class Index[a] {
    type Idx: Type
    type Elm: Type
    type Aef: Eff
    pub def get(t: a, i: Index.Idx[a]): Index.Elm[a] \ Index.Aef[a]
}

instance Index[List[a]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds
    pub def get(t: List[a], i: Int32): a \ IndexOutOfBounds = match (i, t) {
        case (_, Nil) => do IndexOutOfBounds.oob()
        case (n, _) if n < 0 => do IndexOutOfBounds.oob()
        case (0, hd :: _) => hd
        case (n, _ :: tl) => Index.get(tl, n - 1)
    }
}

instance Index[MutList[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds + r
    pub def get(t: MutList[a, r], i: Int32): a \ IndexOutOfBounds + r = {
        match MutList.nth(i, t) {
            case None => do IndexOutOfBounds.oob()
            case Some(x) => x
        }
    }
}

instance Index[Vector[a]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds
    pub def get(t: Vector[a], i: Int32): a \ IndexOutOfBounds = match Vector.nth(i, t) {
        case None => do IndexOutOfBounds.oob()
        case Some(x) => x
    }
}

instance Index[Array[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds + r
    pub def get(t: Array[a, r], i: Int32): a \ IndexOutOfBounds + r = match Array.nth(i, t) {
        case None => do IndexOutOfBounds.oob()
        case Some(x) => x
    }
}

instance Index[Map[k, v]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = IndexOutOfBounds
    pub def get(t: Map[k, v], i: k): v \ IndexOutOfBounds = match Map.get(i, t) {
        case None => do IndexOutOfBounds.oob()
        case Some(x) => x
    }
}

instance Index[MutMap[k, v, r]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = IndexOutOfBounds + r
    pub def get(t: MutMap[k, v, r], i: k): v \ IndexOutOfBounds + r = match MutMap.get(i, t) {
        case None => do IndexOutOfBounds.oob()
        case Some(x) => x
    }
}

instance Index[DelayList[a]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds
    pub def get(t: DelayList[a], i: Int32): a \ IndexOutOfBounds = {
        match (i, DelayList.head(t), DelayList.tail(t)) {
            case (0, Some(hd), _) => hd
            case (n, _, tl) if n >= 1 => tl[n - 1]
            case _ => do IndexOutOfBounds.oob()
        }
    }
}

instance Index[DelayMap[k, v]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = IndexOutOfBounds
    pub def get(t: DelayMap[k, v], i: k): v \ IndexOutOfBounds = {
        match DelayMap.get(i, t) {
            case None => do IndexOutOfBounds.oob()
            case Some(x) => x
        }
    }
}

instance Index[MultiMap[k, v]] with Order[k], Order[v] {
    type Idx = k
    type Elm = Set[v]
    type Aef = Pure
    pub def get(t: MultiMap[k, v], i: k): Set[v] = {
        MultiMap.get(i, t)
    }
}

instance Index[MutDeque[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds + r
    pub def get(t: MutDeque[a, r], i: Int32): a \ IndexOutOfBounds + r = {
        if (i < 0 or i >= MutDeque.size(t)) {
            do IndexOutOfBounds.oob()
        } else {
            let MutDeque.MutDeque(_, arr, front, _) = t;
            let idx = (deref front + i) `Int32.modulo` Array.length(deref arr);
            (deref arr)[idx]
        }
    }
}

instance Index[RedBlackTree[k, v]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = IndexOutOfBounds
    pub def get(t: RedBlackTree[k, v], i: k): v \ IndexOutOfBounds = {
        match RedBlackTree.get(i, t) {
            case None => do IndexOutOfBounds.oob()
            case Some(x) => x
        }
    }
}

instance Index[Chain[a]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds
    pub def get(t: Chain[a], i: Int32): a \ IndexOutOfBounds = {
        Chain.toList(t)[i]
    }
}

instance Index[Nec[a]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds
    pub def get(t: Nec[a], i: Int32): a \ IndexOutOfBounds = {
        Nec.toList(t)[i]
    }
}
