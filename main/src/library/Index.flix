eff IndexOutOfBounds {
    pub def oob(): Unit
}

def oob(): a \ IndexOutOfBounds = unchecked_cast(do IndexOutOfBounds.oob() as a)

class Index[a] {
    type Idx: Type
    type Elm: Type
    type Aef: Eff
    pub def get(i: Index.Idx[a], t: a): Index.Elm[a] \ Index.Aef[a]
}

instance Index[List[a]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds
    pub def get(i: Int32, t: List[a]): a \ IndexOutOfBounds = match (i, t) {
        case (_, Nil) => oob()
        case (n, _) if n < 0 => oob()
        case (0, hd :: _) => hd
        case (n, _ :: tl) => Index.get(n - 1, tl)
    }
}

instance Index[Vector[a]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds
    pub def get(i: Int32, t: Vector[a]): a \ IndexOutOfBounds = match Vector.nth(i, t) {
        case None => oob()
        case Some(x) => x
    }
}

instance Index[Array[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = IndexOutOfBounds + r
    pub def get(i: Int32, t: Array[a, r]): a \ IndexOutOfBounds + r = match Array.nth(i, t) {
        case None => oob()
        case Some(x) => x
    }
}

instance Index[Map[k, v]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = IndexOutOfBounds
    pub def get(i: k, t: Map[k, v]): v \ IndexOutOfBounds = match Map.get(i, t) {
        case None => oob()
        case Some(x) => x
    }
}

instance Index[MutMap[k, v, r]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = IndexOutOfBounds + r
    pub def get(i: k, t: MutMap[k, v, r]): v \ IndexOutOfBounds + r = match MutMap.get(i, t) {
        case None => oob()
        case Some(x) => x
    }
}
