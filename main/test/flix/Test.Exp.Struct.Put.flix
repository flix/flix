struct Person[r] {
    mut fstName: String,
    mut lstName: String,
    mut age: Int32,
    mut cowboy: Bool
}
mod Person {

    use Assert.{assertEq, assertFalse, assertTrue}

    @Test
    def testStructPut01(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->fstName = "Unlucky";
            assertEq(expected = "Unlucky", s->fstName)
        }

    @Test
    def testStructPut02(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->lstName = "Uriel";
            assertEq(expected = "Uriel", s->lstName)
        }

    @Test
    def testStructPut03(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->age = 23;
            assertEq(expected = 23, s->age)
        }

    @Test
    def testStructPut04(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->cowboy = false;
            assertFalse(s->cowboy);
            assertEq(expected = "Lucky", s->fstName);
            assertEq(expected = "Luke", s->lstName);
            assertEq(expected = 42, s->age)
        }

    @Test
    def testStructPut05(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { age = 42, cowboy = true, fstName = "Lucky", lstName = "Luke" };
            s->cowboy = false;
            assertFalse(s->cowboy);
            assertEq(expected = "Lucky", s->fstName);
            assertEq(expected = "Luke", s->lstName);
            assertEq(expected = 42, s->age)
        }

    @Test
    def testStructMultiPut01(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->fstName = "Unlucky";
            s->lstName = "Uriel";
            s->age = 23;
            s->cowboy = false;
            assertEq(expected = "Unlucky", s->fstName);
            assertEq(expected = "Uriel", s->lstName);
            assertEq(expected = 23, s->age);
            assertFalse(s->cowboy)
        }

    @Test
    def testStructMultiPut02(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->cowboy = false;
            s->age = 23;
            s->lstName = "Uriel";
            s->fstName = "Unlucky";
            assertEq(expected = "Unlucky", s->fstName);
            assertEq(expected = "Uriel", s->lstName);
            assertEq(expected = 23, s->age);
            assertFalse(s->cowboy)
        }

    @Test
    def testStructMultiPut03(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->fstName = "Unlucky";
            s->fstName = "Lucky";
            s->lstName = "Uriel";
            s->lstName = "Luke";
            assertEq(expected = "Luke", s->lstName);
            assertEq(expected = "Lucky", s->fstName);
            assertEq(expected = 42, s->age);
            assertTrue(s->cowboy)
        }

    @Test
    def testStructMultiPut04(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->lstName = "Uriel";
            s->fstName = "Unlucky";
            s->lstName = "Luke";
            s->fstName = "Lucky";
            assertEq(expected = "Lucky", s->fstName);
            assertEq(expected = "Luke", s->lstName);
            assertEq(expected = 42, s->age);
            assertTrue(s->cowboy)
        }

    @Test
    def testStructMultiPut05(): Unit \ Assert =
        region rc {
            let s = new Person @ rc { cowboy = true, fstName = "Lucky", age = 42, lstName = "Luke" };
            s->lstName = "Uriel";
            s->fstName = "Unlucky";
            s->lstName = "Luke";
            s->fstName = "Lucky";
            assertEq(expected = "Lucky", s->fstName);
            assertEq(expected = "Luke", s->lstName);
            assertEq(expected = 42, s->age);
            assertTrue(s->cowboy)
        }

}

struct BinaryTree[t, r] {
    mut left: Option[BinaryTree[t, r]],
    mut right: Option[BinaryTree[t, r]],
    mut node_value: t
}

mod BinaryTree {

    use Assert.assertEq

    def binaryTreeSum(tree: Option[BinaryTree[Int32, r]]): Int32 \ r =
        match tree {
            case None => 0
            case Some(t) => t->node_value + binaryTreeSum(t->left) + binaryTreeSum(t->right)
        }

    def doubleTree(tree: Option[BinaryTree[Int32, r]]): Unit \ {r, IO} =
        match tree {
            case None => ()
            case Some(t) =>
                t->node_value = t->node_value + t->node_value;
                doubleTree(t->left);
                doubleTree(t->right)
        }

    @Test
    def binaryTree01(): Unit \ (Assert + IO) =
        region rc {
            let leaf1 = new BinaryTree @ rc {
                left = None,
                right = None,
                node_value = 10
            };
            let leaf2 = new BinaryTree @ rc {
                left = None,
                right = None,
                node_value = 9
            };
            let leaf3 = new BinaryTree @ rc {
                left = None,
                right = None,
                node_value = 8
            };
            let leaf4 = new BinaryTree @ rc {
                left = None,
                right = None,
                node_value = 7
            };
            let innernode1 = new BinaryTree @ rc {
                left = Some(leaf1),
                right = Some(leaf2),
                node_value = 4
            };
            let innernode2 = new BinaryTree @ rc {
                left = Some(leaf3),
                right = Some(leaf4),
                node_value = 4
            };
            let tree = new BinaryTree @ rc {
                left = Some(innernode1),
                right = Some(innernode2),
                node_value = 5
            };
            doubleTree(Some(tree));
            assertEq(expected = 94, binaryTreeSum(Some(tree)))
        }

    @Test
    def binaryTree02(): Unit \ (Assert + IO) =
        region rc {
            let leaf1 = new BinaryTree @ rc {
                right = None,
                left = None,
                node_value = 10
            };
            let leaf2 = new BinaryTree @ rc {
                node_value = 9,
                right = None,
                left = None
            };
            let leaf3 = new BinaryTree @ rc {
                left = None,
                right = None,
                node_value = 8
            };
            let leaf4 = new BinaryTree @ rc {
                left = None,
                node_value = 7,
                right = None
            };
            let innernode1 = new BinaryTree @ rc {
                right = Some(leaf2),
                left = Some(leaf1),
                node_value = 4
            };
            let innernode2 = new BinaryTree @ rc {
                node_value = 4,
                right = Some(leaf4),
                left = Some(leaf3)
            };
            let tree = new BinaryTree @ rc {
                left = Some(innernode1),
                node_value = 5,
                right = Some(innernode2)
            };
            doubleTree(Some(tree));
            assertEq(expected = 94, binaryTreeSum(Some(tree)))
        }

}
struct Nested[t, r] {
    mut v : t
}

mod Nested {
    use Assert.assertEq

    @Test
    def nestedPut01(): Unit \ Assert =
        region rc {
            let nested = new Nested @ rc { v = new Nested @ rc { v = new Nested @ rc {v = new Nested @ rc {v = 15} }} };
            let before = nested->v->v->v->v;
            nested->v->v->v->v = 37;
            let after = nested->v->v->v->v;
            assertEq(expected = 15, before);
            assertEq(expected = 37, after)
        }
}

struct Lst[t, r] {
    mut value: t,
    mut next: Option[Lst[t, r]]
}
mod Lst {
    use Assert.assertEq

    def listAppend(list: Lst[t, r], rc: Region[r], value: t): Unit \ r = match list->next {
        case Some(l) => listAppend(l, rc, value)
        case None => list->next = Some(new Lst @ rc {value = value, next = None})
    }

    def listSum(list: Lst[Int32, r]): Int32 \ r =
        list->value + match list->next {
            case Some(l) => listSum(l)
            case None => 0
        }

    def listDifference(list: Lst[Int32, r]): Int32 \ r =
        list->value - match list->next {
            case Some(l) => listSum(l)
            case None => 0
        }

    def listReverse(list: Lst[Int32, r], rc: Region[r]): Lst[Int32, r] \ r =
        match list->next {
            case None => list
            case Some(l) =>
                let reversed = listReverse(l, rc);
                listAppend(reversed, rc, list->value);
                reversed
        }

    @Test
    def testLst01(): Unit \ Assert =
        region rc {
            let list = new Lst @ rc { value = 1, next = None };
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            assertEq(expected = 55, listSum(list))
        }

    @Test
    def testLst02(): Unit \ Assert =
        region rc {
            let list = new Lst @ rc { value = 1, next = None };
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            assertEq(expected = 55, listSum(list))
        }

    @Test
    def testLst03(): Unit \ Assert =
        region rc {
            let list = new Lst @ rc { value = 1, next = None };
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            assertEq(expected = 55, listSum(list));
            assertEq(expected = 55, listSum(listReverse(list, rc)))
        }

    @Test
    def testLst04(): Unit \ Assert =
        region rc {
            let list = new Lst @ rc { value = 1, next = None };
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            assertEq(expected = -53, listDifference(list));
            assertEq(expected = -35, listDifference(listReverse(list, rc)))
        }
}
