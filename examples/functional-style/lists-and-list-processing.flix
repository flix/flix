/// We can easily construct and append lists:
def l(): List[Int32] =
    let l1 = 1 :: 2 :: 3 :: Nil;
    let l2 = 4 :: 5 :: 6 :: Nil;
    l1 ::: l2

/// We can use pattern matching to take a list apart:
def length(l: List[a]): Int32 = match l {
  case Nil     => 0
  case _ :: xs => 1 + length(xs)
}

/// The Flix library has extensive support for lists:
def main(): Unit \ IO =
    let l1 = l();
    let l2 = List.intersperse(42, l1);
    let l3 = List.map(x -> x :: x :: Nil, l2);
    let l4 = List.flatten(l3);
    println(length(l4))
