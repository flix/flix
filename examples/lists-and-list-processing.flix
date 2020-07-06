/// In Flix, as in many functional programming languages,
/// lists are the bread and butter.

/// We can easily construct a list:
def aList(): List[Int] = 1 :: 2 :: 3 :: Nil

/// We can easily append two lists:
def bList(): List[Int] = aList() ::: aList()

/// We can use pattern matching to take a list apart:
def length[a](l: List[a]): Int = match l {
  case Nil     => 0
  case x :: xs => 1 + length(xs)
}

/// The Flix library has extensive support for lists:
def main(): Bool =
    let l1 = List.range(0, 10);
    let l2 = List.intersperse(42, l1);
    let l3 = List.map(x -> x :: x :: Nil, l2);
    let l4 = List.flatten(l3);
    List.exists(x -> x == 0, l4)
