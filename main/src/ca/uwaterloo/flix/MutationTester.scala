package ca.uwaterloo.flix

    object MutationTester {
        def run(files , options , tester, testee): Unit {
            val flix = new Flix 
            flix.addSourceCode(...)
            val root = flix.check().unsafeGet()
            val root1 = mutate(root)
            val result = flix.codeGen(root1).unsafeGet()
            val tests = result.getTests()
        }

        def mutate(root): List[ast] { 

        }

        def mutateExpr(e: expr): {
            e match {
                case _ => ...
            } 
        }
    }