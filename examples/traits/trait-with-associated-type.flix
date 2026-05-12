trait Addable[t] {
    type Rhs
    pub def add(x: t, y: Addable.Rhs[t]): t
}

instance Addable[Int32] {
    type Rhs = Int32
    pub def add(x: Int32, y: Int32): Int32 = x + y
}

instance Addable[Set[a]] with Order[a] {
    type Rhs = a
    pub def add(s: Set[a], x: a): Set[a] = Set.insert(x, s)
}

def main(): Unit \ IO =
    let x = Addable.add(123, 456);
    let y = Addable.add(Set#{1, 2, 3}, 4);
    println("x = ${x}");
    println("y = ${y}")
