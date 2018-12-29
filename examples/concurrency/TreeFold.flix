enum TreeNode {
  case Node(Int, TreeNode, TreeNode),
  case Leaf(Int)
}

def foldTree(root: TreeNode, f: (Int, Int, Int) -> Int, out: Channel[Int]): Unit = {
  out <- match root with {
    case Node(value, leftChild, rightChild) => {
      let rightChannel = chan Int 1;
      let leftChannel = chan Int 1;
      spawn foldTree(leftChild, f, leftChannel);
      spawn foldTree(rightChild, f, rightChannel);
      f(value, <- leftChannel, <- rightChannel)
    }
    case Leaf(value) => value
  };
  ()
}

def main(): Int = {
  let tree = makeTree();
  let result = chan Int 1;
  spawn foldTree(tree, (parent, lChild, rChild) -> parent+lChild+rChild, result);
  <- result
}

def makeTree(): TreeNode = {
  Node(1, Node(2, Leaf(1), Leaf(2)), Node(3, Leaf(1), Leaf(5)))
}