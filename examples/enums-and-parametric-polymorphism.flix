/// An algebraic data type for binary trees with leaves.
enum Tree[a] {
    case Leaf(a),
    case Node(Tree[a], Tree[a])
}

/// A higher-order function that transforms a tree with
/// elements of type `a` to a tree with elements of type `b`.
def map(f: a -> b, t: Tree[a]): Tree[b] = match t {
    case Leaf(x)    => Leaf(f(x))
    case Node(l, r) => Node(map(f, l), map(f, r))
}

/// A function that computes the sum of all leaves in a tree.
def sum(t: Tree[Int32]): Int32 = match t {
    case Leaf(x)    => x
    case Node(l, r) => sum(l) + sum(r)
}

def main(_args: Array[String]): Int32 & Impure =
    let t1 = Node(Leaf("Hello"), Leaf("World"));
    let t2 = map(String.length, t1);
    println(sum(t2));
    0 // exit code
