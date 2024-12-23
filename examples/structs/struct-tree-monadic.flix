// A variant of `struct-tree` which uses more syntactic sugar.
struct Tree[k, v, r] {
    key: k,
    value: v,
    mut left: Option[Tree[k, v, r]],
    mut right: Option[Tree[k, v, r]]
}

mod Tree {
    // A function to search the tree `t` for the given key `k`.
    pub def search(k: k, t: Tree[k, v, r]): Option[v] \ r with Order[k] =
        match (k <=> t->key) {
            case Comparison.EqualTo  => Some(t->value)
            case Comparison.LessThan =>
                // Search in the left subtree.
                forM(l <- t->left; result  <- search(k, l))
                    yield result
              case Comparison.GreaterThan =>
                // Search in the right subtree.
                forM(r <- t->right; result <- search(k, r))
                    yield result
        }
}
