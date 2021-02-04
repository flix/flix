pub enum Test {
    case Test
}

pub enum Suite {
    case Suite(List[Test])
    case ParallelSuite(List[Test])
}

class Diffable[a] {
    def diff(x: a, y: a): String
}



pub def defineTest[a : Eq, r :# Record](test: {name: String, expected: Lazy[a], actual: Lazy[a] | r}): Test = ???

pub def defineTestWithOps[a : Eq, r :# Record, r2 :# Record](test: {name: String, expected: Lazy[a], actual: Lazy[a] | r}, opts: {timeout: Option[Int] | r2}): Test = ???

pub def disableTest(r: {name: String, expected: Lazy[a], actual: Lazy[a]}): Test = ???

// TODO: parallel

pub def defaultTest(): {} = {}

class DefTest[r :# Record] {
    pub def defTest(): { retry: Int | r }
}

pub def defaultOpts(): { timeout: Option[Int] } = { timeout = None }

// TODO: Use a map?
pub /* const */ def listSuite(): Suite = Suite(
    defineTest({
        +name        = "List.head01",
        +actual      = lazy List.head(1 :: Nil),
        +expected    = lazy Some(1)
        | defaultTest()
    })

    ::

    disableTest({
        name        = "List.head02",
        actual      = lazy List.head(1 :: 2 :: Nil),
        expected    = lazy Some(1)
    })

    ::

    defineTest({
        name        = "List.head03",
        actual      = lazy List.head(1 :: Nil),
        expected    = lazy Some(1),
        retry       = 3
    })

    ::

    defineTestWithOps({
        name        = "List.head04",
        actual      = lazy List.head(1 :: 2 :: 3 :: Nil),
        expected    = lazy Some(1)
    })({timeout = Some(123)})

    ::

    Nil
)
