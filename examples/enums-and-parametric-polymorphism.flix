/// An algebraic data type for binary trees
/// where each leaf carries a value of type a.
enum Tree[a] {
    case Leaf(a),
    case Node(Tree[a], Tree[a])
}

/// An instance of ToString.
instance ToString[Tree[a]] with ToString[a] {
    pub def toString(t: Tree[a]): String = match t {
        case Leaf(v)    => "Leaf(${v})"
        case Node(l, r) => "Node(${l}, ${r})"
    }
}

/// A higher-order function that transforms a tree with
/// elements of type a to a tree with elements of type b.
def map[a, b](f: a -> b, t: Tree[a]): Tree[b] =
  match t {
    case Leaf(x)    => Leaf(f(x))
    case Node(l, r) => Node(map(f, l), map(f, r))
  }

/// Returns a simple tree with two leafs.
def tree(): Tree[Int] = Node(Leaf(1), Leaf(2))

/// Squares all elements in the simple tree.
def main(_args: Array[String]): Int32 & Impure =
    map(x -> x * x, tree()) |> println;
    0 // exit code
