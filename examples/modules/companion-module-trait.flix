trait Addable[t] {
    pub def add(x: t, y: t): t
}

mod Addable {
    pub def add3(x: t, y: t, z: t): t with Addable[t] = add(add(x, y), z)
}

instance Addable[Int32] {
    pub def add(x: Int32, y: Int32): Int32 = x + y
}

def main(): Unit \ IO =
    println(Addable.add3(1, 2, 3))
