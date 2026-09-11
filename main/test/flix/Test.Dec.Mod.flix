mod A {
    pub def f(): Bool = true
    pub def g(): Bool = true

    mod B {
        pub def f(): Bool = true
        pub def g(): Bool = true

        mod C {
            pub def f(): Bool = true
            pub def g(): Bool = true
        }
    }
}

mod X {
    pub def f(): Bool = true

    mod Y {
        pub def f(): Bool = X.f()

        mod Z {
            pub def f(): Bool = X.Y.f()
        }
    }
}

//
// A private module is visible from the module that declares it and from
// every module nested within it, siblings included.
//
mod Vis {

    mod Private {
        pub def f(): Bool = true

        pub enum Color with Eq {
            case Red,
            case Blue
        }

        mod Inner {
            pub def f(): Bool = true
        }

        // `Peer` and `Inner` are both declared in `Vis.Private`, so `Peer` can reach `Inner`.
        mod Peer {
            pub def f(): Bool = Vis.Private.Inner.f()
        }

        pub def peer(): Bool = Peer.f()
    }

    // `Sibling` and `Private` are both declared in `Vis`, so `Sibling` can reach `Private`.
    mod Sibling {
        use Vis.Private.f

        pub def viaPath(): Bool = Vis.Private.f()

        pub def viaUse(): Bool = f()

        pub def viaEnum(): Vis.Private.Color = Vis.Private.Color.Red

        // A descendant of a sibling can reach `Private` too.
        mod Nested {
            pub def f(): Bool = Vis.Private.f()
        }

        pub def nested(): Bool = Nested.f()
    }

    // `Vis` itself can reach into its private modules.
    pub def parent(): Bool = Private.f()

    pub def sibling(): Bool = Sibling.viaPath()

    pub def siblingUse(): Bool = Sibling.viaUse()

    pub def siblingEnum(): Bool = Sibling.viaEnum() == Private.Color.Red

    pub def siblingNested(): Bool = Sibling.nested()

    pub def nestedSibling(): Bool = Private.peer()

}

mod Test.Dec.Mod {

    @CompileTest
    def testMod01(): Bool = A.f()

    @CompileTest
    def testMod04(): Bool = A.g()

    @CompileTest
    def testVisParent01(): Bool = Vis.parent()

    @CompileTest
    def testVisSibling01(): Bool = Vis.sibling()

    @CompileTest
    def testVisSibling02(): Bool = Vis.siblingUse()

    @CompileTest
    def testVisSibling03(): Bool = Vis.siblingEnum()

    @CompileTest
    def testVisSibling04(): Bool = Vis.siblingNested()

    @CompileTest
    def testVisSibling05(): Bool = Vis.nestedSibling()

}
