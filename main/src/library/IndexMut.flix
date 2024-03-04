class IndexMut[a] {
    type Idx: Type
    type Elm: Type
    type Aef: Eff
    pub def put(t: a, i: IndexMut.Idx[a], v: IndexMut.Elm[a]): Unit \ IndexMut.Aef[a]
}

instance IndexMut[Array[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds + r
    pub def put(t: Array[a, r], i: Int32, v: a): Unit \ IndexOutOfBounds + r = {
        if (i < 0 or i >= Array.length(t)) {
            do IndexOutOfBounds.oob()
        } else {
            Array.put(v, i, t)
        }
    }
}

instance IndexMut[MutList[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds + r
    pub def put(t: MutList[a, r], i: Int32, v: a): Unit \ IndexOutOfBounds + r = {
        if (i < 0 or i >= MutList.length(t)) {
            do IndexOutOfBounds.oob()
        } else {
            MutList.insert!(v, i, t)
        }
    }
}

instance IndexMut[MutMap[k, v, r]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = r
    pub def put(t: MutMap[k, v, r], i: k, v: v): Unit \ r = MutMap.put!(i, v, t)
}

instance IndexMut[MutDeque[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds + r
    pub def put(t: MutDeque[a, r], i: Int32, v: a): Unit \ IndexOutOfBounds + r = {
        if (i < 0 or i >= MutDeque.size(t)) {
            do IndexOutOfBounds.oob()
        } else {
            let MutDeque.MutDeque(_, arr, front, _) = t;
            let idx = (deref front + i) `Int32.modulo` Array.length(deref arr);
            (deref arr)[idx] = v
        }
    }
}
