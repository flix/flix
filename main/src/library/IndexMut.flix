class IndexMut[a] {
    type Idx: Type
    type Elm: Type
    type Aef: Eff
    pub def put(i: IndexMut.Idx[a], v: IndexMut.Elm[a], t: a): Unit \ IndexMut.Aef[a]
}

instance IndexMut[Array[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds + r
    pub def put(i: Int32, v: a, t: Array[a, r]): Unit \ IndexOutOfBounds + r = {
        if (i < 0 or i >= Array.length(t)) {
            oob()
        } else {
            Array.put(v, i, t)
        }
    }
}

instance IndexMut[MutMap[k, v, r]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = r
    pub def put(i: k, v: v, t: MutMap[k, v, r]): Unit \ r = MutMap.put!(i, v, t)
}
