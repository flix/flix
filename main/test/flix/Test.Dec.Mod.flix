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

    //
    // A private trait follows the same rule: its signatures can be called from
    // the module that declares it and from every module nested within it.
    //
    trait Flag[a] {
        pub def flag(x: a): Bool
    }

    instance Flag[Bool] {
        pub def flag(x: Bool): Bool = x
    }

    //
    // The same holds for a private effect and its operations.
    //
    eff Tick {
        def tick(x: Bool): Bool
    }

    pub def runTick(f: Unit -> Bool \ Tick): Bool =
        run f() with handler Tick {
            def tick(x, k) = k(x)
        }

    //
    // The same holds for a private enum and its cases.
    //
    enum Light {
        case On,
        case Off
    }

    pub def isOn(l: Light): Bool = match l {
        case Light.On => true
        case Light.Off => false
    }

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

            // `Peer` is nested within `Vis`, so it can reach the private trait `Flag`.
            pub def viaTrait(): Bool = Vis.Flag.flag(true)

            // Likewise for the private effect `Tick`.
            pub def viaEffect(): Bool = Vis.runTick(() -> Vis.Tick.tick(true))

            // Likewise for the private enum `Light`.
            pub def viaCase(): Bool = Vis.isOn(Vis.Light.On)
        }

        pub def peer(): Bool = Peer.f()

        pub def peerTrait(): Bool = Peer.viaTrait()

        pub def peerEffect(): Bool = Peer.viaEffect()

        pub def peerCase(): Bool = Peer.viaCase()
    }

    // `Sibling` and `Private` are both declared in `Vis`, so `Sibling` can reach `Private`.
    mod Sibling {
        use Vis.Private.f
        use Vis.Flag.flag
        use Vis.Tick.tick
        use Vis.Light.Off

        pub def viaPath(): Bool = Vis.Private.f()

        pub def viaUse(): Bool = f()

        pub def viaEnum(): Vis.Private.Color = Vis.Private.Color.Red

        pub def viaTrait(): Bool = Vis.Flag.flag(true)

        pub def viaTraitUse(): Bool = flag(true)

        pub def viaEffect(): Bool = Vis.runTick(() -> Vis.Tick.tick(true))

        pub def viaEffectUse(): Bool = Vis.runTick(() -> tick(true))

        pub def viaCase(): Bool = Vis.isOn(Vis.Light.On)

        pub def viaCaseUse(): Bool = not Vis.isOn(Off)

        pub def viaCasePattern(): Bool = match Vis.Light.On {
            case Vis.Light.On => true
            case Vis.Light.Off => false
        }

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

    // `Vis` can call a signature of its own private trait.
    pub def ownTrait(): Bool = Flag.flag(true)

    pub def siblingTrait(): Bool = Sibling.viaTrait()

    pub def siblingTraitUse(): Bool = Sibling.viaTraitUse()

    pub def nestedTrait(): Bool = Private.peerTrait()

    // `Vis` can call an operation of its own private effect.
    pub def ownEffect(): Bool = runTick(() -> Tick.tick(true))

    pub def siblingEffect(): Bool = Sibling.viaEffect()

    pub def siblingEffectUse(): Bool = Sibling.viaEffectUse()

    pub def nestedEffect(): Bool = Private.peerEffect()

    // `Vis` can construct and match the cases of its own private enum.
    pub def ownCase(): Bool = match Light.On {
        case Light.On => true
        case Light.Off => false
    }

    pub def siblingCase(): Bool = Sibling.viaCase()

    pub def siblingCaseUse(): Bool = Sibling.viaCaseUse()

    pub def siblingCasePattern(): Bool = Sibling.viaCasePattern()

    pub def nestedCase(): Bool = Private.peerCase()

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

    @CompileTest
    def testVisTrait01(): Bool = Vis.ownTrait()

    @CompileTest
    def testVisTrait02(): Bool = Vis.siblingTrait()

    @CompileTest
    def testVisTrait03(): Bool = Vis.siblingTraitUse()

    @CompileTest
    def testVisTrait04(): Bool = Vis.nestedTrait()

    @CompileTest
    def testVisEffect01(): Bool = Vis.ownEffect()

    @CompileTest
    def testVisEffect02(): Bool = Vis.siblingEffect()

    @CompileTest
    def testVisEffect03(): Bool = Vis.siblingEffectUse()

    @CompileTest
    def testVisEffect04(): Bool = Vis.nestedEffect()

    @CompileTest
    def testVisCase01(): Bool = Vis.ownCase()

    @CompileTest
    def testVisCase02(): Bool = Vis.siblingCase()

    @CompileTest
    def testVisCase03(): Bool = Vis.siblingCaseUse()

    @CompileTest
    def testVisCase04(): Bool = Vis.siblingCasePattern()

    @CompileTest
    def testVisCase05(): Bool = Vis.nestedCase()

}
