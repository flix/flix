mod Test.Exp.Struct.Put {

    struct Put[r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    @test
    def testStructPut01(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName == "Unlucky"
        }

    @test
    def testStructPut02(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€lstName == "Uriel"
        }

    @test
    def testStructPut03(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€age = 23;
            s€age == 23
        }

    @test
    def testStructPut04(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            not s€cowboy and s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42
        }

    @test
    def testStructMultiPut01(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€lstName = "Uriel";
            s€age = 23;
            s€cowboy = false;
            s€fstName == "Unlucky" and s€lstName == "Uriel" and s€age == 23 and s€cowboy == false
        }

    @test
    def testStructMultiPut02(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            s€age = 23;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€fstName == "Unlucky" and s€lstName == "Uriel" and s€age == 23 and s€cowboy == false
        }

    @test
    def testStructMultiPut03(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName = "Lucky";
            s€lstName = "Uriel";
            s€lstName = "Luke";
            s€lstName == "Luke" and s€fstName == "Lucky" and s€age == 42 and s€cowboy == true
        }

    @test
    def testStructMultiPut04(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€lstName = "Luke";
            s€fstName = "Lucky";
            s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42 and s€cowboy == true
        }
}

mod Test.Exp.Struct.BinaryTree {

    struct BinaryTree[t, r] {
        left: Option[BinaryTree[t, r]],
        right: Option[BinaryTree[t, r]],
        value: t
    }

    def binaryTreeSum(tree: Option[BinaryTree[Int32, r]]): Int32 \ r =
        match tree {
            case None => 0
            case Some(t) => t€value + binaryTreeSum(t€left) + binaryTreeSum(t€right)
        }

    def doubleTree(tree: Option[BinaryTree[Int32, r]]): Unit \ {r, IO} =
        match tree {
            case None => ()
            case Some(t) =>
                t€value = t€value + t€value;
                doubleTree(t€left);
                doubleTree(t€right)
        }

    @test
    def binaryTree01(): Bool \ IO =
        region rc {
            let leaf1 = new BinaryTree {
                left = None,
                right = None,
                value = 10
            } @ rc;
            let leaf2 = new BinaryTree {
                left = None,
                right = None,
                value = 9
            } @ rc;
            let leaf3 = new BinaryTree {
                left = None,
                right = None,
                value = 8
            } @ rc;
            let leaf4 = new BinaryTree {
                left = None,
                right = None,
                value = 7
            } @ rc;
            let innernode1 = new BinaryTree {
                left = Some(leaf1),
                right = Some(leaf2),
                value = 4
            } @ rc;
            let innernode2 = new BinaryTree {
                left = Some(leaf3),
                right = Some(leaf4),
                value = 4
            } @ rc;
            let tree = new BinaryTree {
                left = Some(innernode1),
                right = Some(innernode2),
                value = 5
            } @ rc;
            doubleTree(Some(tree));
            println(binaryTreeSum(Some(tree)));
            binaryTreeSum(Some(tree)) == 94
        }
}
