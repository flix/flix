// A struct for a polymorphic binary search tree.
struct Tree[k, v, r] {
    key: k,
    value: v,
    mut left: Option[Tree[k, v, r]],
    mut right: Option[Tree[k, v, r]]
}

mod Tree {
    // A function to search the tree `t` for the given key `k`.
    pub def search(k: k, t: Tree[k, v, r]): Option[v] \ r with Order[k] =
        match (Order.compare(k, t->key)) {
            case Comparison.EqualTo  => Some(t->value)
            case Comparison.LessThan =>
                // Search in the left subtree.
                match t->left {
                    case None            => None
                    case Some(leftTree)  => search(k, leftTree)
                }
            case Comparison.GreaterThan =>
                // Search in the right subtree.
                match t->right {
                    case None            => None
                    case Some(rightTree) => search(k, rightTree)
                }
        }
}
