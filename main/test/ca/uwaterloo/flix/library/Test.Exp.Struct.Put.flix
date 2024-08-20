mod Test.Exp.Struct.Put {

    struct Person[r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    @test
    def testStructPut01(): Bool =
        region rc {
            let s = new Person { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName == "Unlucky"
        }

    @test
    def testStructPut02(): Bool =
        region rc {
            let s = new Person { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€lstName == "Uriel"
        }

    @test
    def testStructPut03(): Bool =
        region rc {
            let s = new Person { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€age = 23;
            s€age == 23
        }

    @test
    def testStructPut04(): Bool =
        region rc {
            let s = new Person { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            not s€cowboy and s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42
        }

    @test
    def testStructMultiPut01(): Bool =
        region rc {
            let s = new Person { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€lstName = "Uriel";
            s€age = 23;
            s€cowboy = false;
            s€fstName == "Unlucky" and s€lstName == "Uriel" and s€age == 23 and s€cowboy == false
        }

    @test
    def testStructMultiPut02(): Bool =
        region rc {
            let s = new Person { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            s€age = 23;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€fstName == "Unlucky" and s€lstName == "Uriel" and s€age == 23 and s€cowboy == false
        }

    @test
    def testStructMultiPut03(): Bool =
        region rc {
            let s = new Person { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName = "Lucky";
            s€lstName = "Uriel";
            s€lstName = "Luke";
            s€lstName == "Luke" and s€fstName == "Lucky" and s€age == 42 and s€cowboy == true
        }

    @test
    def testStructMultiPut04(): Bool =
        region rc {
            let s = new Person { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€lstName = "Luke";
            s€fstName = "Lucky";
            s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42 and s€cowboy == true
        }

    struct BinaryTree[t, r] {
        left: Option[BinaryTree[t, r]],
        right: Option[BinaryTree[t, r]],
        node_value: t
    }

    def binaryTreeSum(tree: Option[BinaryTree[Int32, r]]): Int32 \ r =
        match tree {
            case None => 0
            case Some(t) => t€node_value + binaryTreeSum(t€left) + binaryTreeSum(t€right)
        }

    def doubleTree(tree: Option[BinaryTree[Int32, r]]): Unit \ {r, IO} =
        match tree {
            case None => ()
            case Some(t) =>
                t€node_value = t€node_value + t€node_value;
                doubleTree(t€left);
                doubleTree(t€right)
        }

    @test
    def binaryTree01(): Bool \ IO =
        region rc {
            let leaf1 = new BinaryTree {
                left = None,
                right = None,
                node_value = 10
            } @ rc;
            let leaf2 = new BinaryTree {
                left = None,
                right = None,
                node_value = 9
            } @ rc;
            let leaf3 = new BinaryTree {
                left = None,
                right = None,
                node_value = 8
            } @ rc;
            let leaf4 = new BinaryTree {
                left = None,
                right = None,
                node_value = 7
            } @ rc;
            let innernode1 = new BinaryTree {
                left = Some(leaf1),
                right = Some(leaf2),
                node_value = 4
            } @ rc;
            let innernode2 = new BinaryTree {
                left = Some(leaf3),
                right = Some(leaf4),
                node_value = 4
            } @ rc;
            let tree = new BinaryTree {
                left = Some(innernode1),
                right = Some(innernode2),
                node_value = 5
            } @ rc;
            doubleTree(Some(tree));
            binaryTreeSum(Some(tree)) == 94
        }

    struct Nested[t, r] {
        v : t
    }

    @test
    def nestedPut01(): Bool =
        region rc {
            let nested = new Nested{ v = new Nested { v = new Nested {v = new Nested {v = 15} @ rc } @ rc} @ rc } @ rc;
            nested€v€v€v€v == 15;
            nested€v€v€v€v = 37;
            nested€v€v€v€v == 37
        }

    struct List[t, r] {
        value: t,
        next: Option[List[t, r]]
    }

    def listAppend(list: List[t, r], rc: Region[r], value: t): Unit \ r = match list€next {
        case Some(l) => listAppend(l, rc, value)
        case None => list€next = Some(new List {value = value, next = None} @ rc)
    }

    def listSum(list: List[Int32, r]): Int32 \ r =
        list€value + match list€next {
            case Some(l) => listSum(l)
            case None => 0
        }

    def listDifference(list: List[Int32, r]): Int32 \ r =
        list€value - match list€next {
            case Some(l) => listSum(l)
            case None => 0
        }

    def listReverse(list: List[Int32, r], rc: Region[r]): List[Int32, r] \ r =
        match list€next {
            case None => list
            case Some(l) =>
                let reversed = listReverse(l, rc);
                listAppend(reversed, rc, list€value);
                reversed
        }

    @test
    def testList01(): Bool =
        region rc {
            let list = new List { value = 1, next = None } @ rc;
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            listSum(list) == 55
        }

    @test
    def testList02(): Bool =
        region rc {
            let list = new List { value = 1, next = None } @ rc;
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            listSum(list) == 55
        }

    @test
    def testList03(): Bool =
        region rc {
            let list = new List { value = 1, next = None } @ rc;
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            listSum(list) == 55 and listSum(listReverse(list, rc)) == 55
        }

    @test
    def testList04(): Bool =
        region rc {
            let list = new List { value = 1, next = None } @ rc;
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            listDifference(list) == -53 and listDifference(listReverse(list, rc)) == -35
        }
}
