mod Test.Exp.Struct.Get {

    struct Get[r] {
        fstName: String,
        lstName: String,
        mut age: Int32,
        cowboy: Bool
    }

    mod Get {

        use Assert.{assertEq, assertTrue}

        @Test
        def testStructGet01(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                assertEq(expected = "Lucky", s->fstName);
                assertEq(expected = "Luke", s->lstName);
                assertEq(expected = 42, s->age);
                assertTrue(s->cowboy)
            }

        @Test
        def testStructGet02(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                assertEq(expected = "Luke", s->lstName)
            }

        @Test
        def testStructGet03(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                assertEq(expected = 42, s->age)
            }

        @Test
        def testStructGet04(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                assertTrue(s->cowboy)
            }

        @Test
        def testStructGet05(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { lstName = "Luke", cowboy = true, age = 42, fstName = "Lucky" };
                assertTrue(s->cowboy)
            }

        // From https://github.com/flix/flix/issues/10057
        pub struct Node[r] {
            keys:       Int32,
            children:   Option[Int32],
            leaves:     Option[Int32],
            size:       Int32,
            parent:     Option[Int32]
        }

        mod Node {
            use Assert.assertEq

            @Test
            pub def empty(): Unit \ Assert = region rc {
                let node = new Node @ rc {
                    keys = 0,
                    children = None,
                    leaves = None,
                    size = 0,
                    parent = None
                };
                assertEq(expected = 0, node->size)
            }
        }

        @Test
        def testStructGet06(): Unit \ Assert = region _ {
            Node.empty()
        }

        @Test
        def testStructMultiGet01(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                let fstName = s->fstName;
                let lstName = s->lstName;
                let age = s->age;
                let cowboy = s->cowboy;
                assertEq(expected = "Lucky", fstName);
                assertEq(expected = "Luke", lstName);
                assertEq(expected = 42, age);
                assertTrue(cowboy)
            }

        @Test
        def testStructMultiGet02(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                let cowboy = s->cowboy;
                let age = s->age;
                let lstName = s->lstName;
                let fstName = s->fstName;
                assertEq(expected = "Lucky", fstName);
                assertEq(expected = "Luke", lstName);
                assertEq(expected = 42, age);
                assertTrue(cowboy)
            }

        @Test
        def testStructMultiGet03(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                let fstName1 = s->fstName;
                let fstName2 = s->fstName;
                let lstName1 = s->lstName;
                let lstName2 = s->lstName;
                assertEq(expected = "Lucky", fstName1);
                assertEq(expected = "Lucky", fstName2);
                assertEq(expected = "Luke", lstName1);
                assertEq(expected = "Luke", lstName2)
            }

        @Test
        def testStructMultiGet04(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                let lstName1 = s->lstName;
                let fstName1 = s->fstName;
                let lstName2 = s->lstName;
                let fstName2 = s->fstName;
                assertEq(expected = "Lucky", fstName1);
                assertEq(expected = "Lucky", fstName2);
                assertEq(expected = "Luke", lstName1);
                assertEq(expected = "Luke", lstName2)
            }

        @Test
        def testStructMultiGet05(): Unit \ Assert =
            region rc {
                let s = new Get @ rc { lstName = "Luke", fstName = "Lucky", cowboy = true, age = 42 };
                let lstName1 = s->lstName;
                let fstName1 = s->fstName;
                let lstName2 = s->lstName;
                let fstName2 = s->fstName;
                assertEq(expected = "Lucky", fstName1);
                assertEq(expected = "Lucky", fstName2);
                assertEq(expected = "Luke", lstName1);
                assertEq(expected = "Luke", lstName2)
            }

        @Test
        def testStructPureGet01(): Unit \ Assert = {
            let result = {
                let f: String -> String = region rc {
                    let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                    msg -> "${msg} ${s->fstName}"
                };
                f("Hello")
            };
            assertEq(expected = "Hello Lucky", result)
        }

        def getLastName(g: Get[rc]): String \ {} = g->lstName

        @Test
        def testStructPureGet02(): Unit \ Assert = {
            region rc {
                let s = new Get @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
                assertEq(expected = "Luke", getLastName(s))
            }
        }

        @Test
        def testStructPureGet03(): Unit \ Assert = {
            region rc {
                let s = new Get @ rc { lstName = "Luke", age = 42, fstName = "Lucky", cowboy = true };
                assertEq(expected = "Luke", getLastName(s))
            }
        }
    }

    struct BinaryTree[t, r] {
        mut left: Option[BinaryTree[t, r]],
        mut right: Option[BinaryTree[t, r]],
        mut value: t
    }

    mod BinaryTree {
        use Assert.assertEq

        def binaryTreeSum(tree: Option[BinaryTree[Int32, r]]): Int32 \ r =
            match tree {
                case None => 0
                case Some(t) => t->value + binaryTreeSum(t->left) + binaryTreeSum(t->right)
            }

        @Test
        def binaryTree01(): Unit \ Assert =
            region rc {
                let leaf = new BinaryTree @ rc {
                    left = None,
                    right = None,
                    value = 3
                };
                let innernode = new BinaryTree @ rc {
                    left = Some(leaf),
                    right = Some(leaf),
                    value = 4
                };
                let tree = new BinaryTree @ rc {
                    left = Some(innernode),
                    right = Some(innernode),
                    value = 5
                };
                assertEq(expected = 25, binaryTreeSum(Some(tree)))
            }

        @Test
        def binaryTree02(): Unit \ Assert =
            region rc {
                let leaf = new BinaryTree @ rc {
                    right = None,
                    left = None,
                    value = 3
                };
                let innernode = new BinaryTree @ rc {
                    right = Some(leaf),
                    value = 4,
                    left = Some(leaf)
                };
                let tree = new BinaryTree @ rc {
                    value = 5,
                    right = Some(innernode),
                    left = Some(innernode)
                };
                assertEq(expected = 25, binaryTreeSum(Some(tree)))
            }
    }

    struct Nested[v, r] {
        mut v: v
    }
    mod Nested {
        use Assert.assertEq

        @Test
        def nestedPut01(): Unit \ Assert =
            region rc {
                let nested = new Nested @ rc { v = new Nested @ rc { v = new Nested @ rc {v = new Nested @ rc {v = 15}}}};
                assertEq(expected = 15, nested->v->v->v->v)
            }

    }

}
