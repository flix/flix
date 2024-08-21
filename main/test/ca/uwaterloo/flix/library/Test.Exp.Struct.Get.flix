mod Test.Exp.Struct.Get {

    struct Get[r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    @test
    def testStructGet01(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42 and s€cowboy == true
        }

    @test
    def testStructGet02(): String =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName
        }

    @test
    def testStructGet03(): Int32 =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€age
        }

    @test
    def testStructGet04(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy
        }

    @test
    def testStructMultiGet01(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let fstName = s€fstName;
            let lstName = s€lstName;
            let age = s€age;
            let cowboy = s€cowboy;
            fstName == "Lucky" and lstName == "Luke" and age == 42 and cowboy
        }

    @test
    def testStructMultiGet02(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let cowboy = s€cowboy;
            let age = s€age;
            let lstName = s€lstName;
            let fstName = s€fstName;
            fstName == "Lucky" and lstName == "Luke" and age == 42 and cowboy
        }

    @test
    def testStructMultiGet03(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let fstName1 = s€fstName;
            let fstName2 = s€fstName;
            let lstName1 = s€lstName;
            let lstName2 = s€lstName;
            fstName1 == "Lucky" and fstName2 == "Lucky" and lstName1 == "Luke" and lstName2 == "Luke"
        }

    @test
    def testStructMultiGet04(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let lstName1 = s€lstName;
            let fstName1 = s€fstName;
            let lstName2 = s€lstName;
            let fstName2 = s€fstName;
            fstName1 == "Lucky" and fstName2 == "Lucky" and lstName1 == "Luke" and lstName2 == "Luke"
        }

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

    @test
    def binaryTree01(): Bool =
        region rc {
            let leaf = new BinaryTree {
                left = None,
                right = None,
                value = 3
            } @ rc;
            let innernode = new BinaryTree {
                left = Some(leaf),
                right = Some(leaf),
                value = 4
            } @ rc;
            let tree = new BinaryTree {
                left = Some(innernode),
                right = Some(innernode),
                value = 5
            } @ rc;
            binaryTreeSum(Some(tree)) == 25
        }

    struct Nested[v, r] {
        value: v
    }

    @test
    def nestedPut01(): Bool =
        region rc {
            let nested = new Nested{ value = new Nested { value = new Nested {value = new Nested {value = 15} @ rc } @ rc} @ rc } @ rc;
            nested€value€value€value€value == 15
        }

}
