mod Subtyping {

    type alias Program = {
        classes = Vector[String],
        interfaces = Vector[String],
        classImplements = Vector[(String, String)],
        classExtends = Vector[(String, String)],
        interfaceExtends = Vector[(String, String)],
        methods = Vector[(String, String)]
    }

    def classHierarchyAnalysis(p: Program): #{ Subtype(String, String), AvailableMethod(String, String), NonSupertype(String) | r } =
        let cs    = inject p.classes          into Class;
        let is    = inject p.interfaces       into Interface;
        let impls = inject p.classImplements  into ClassImplements;
        let ce    = inject p.classExtends     into ClassExtends;
        let ies   = inject p.interfaceExtends into InterfaceExtends;
        let ms    = inject p.methods          into Method;
        let rules = #{
            Subtype(x, x)       :- Class(x).
            Subtype(x, x)       :- Interface(x).
            Subtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
            Subtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            Subtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            Subtype(sub, super) :- Subtype(sub, a), Subtype(a, super).

            AvailableMethod(class, method) :- Method(t, method), Subtype(class, t).
            NonSupertype(class)            :- Class(class), Class(otherClass), not Subtype(otherClass, class), if class != otherClass.
        };
        #(Subtype, AvailableMethod, NonSupertype) -> rules <+> cs <+> is <+> ce <+> impls <+> ies <+> ms

    pub def subtypesDistinct(types: Program): Vector[(String, String)] =
        let v = query classHierarchyAnalysis(types) select (x, y) from Subtype(x, y);
        Vector.filter(match (x, y) -> x != y, v)

    pub def extendsFinalClass(tpe: String, finalClasses: Vector[String], types: Program): Bool =
        let fcs  = inject finalClasses into FinalClass;
        let rule = #{
            ExtendsFinalClass(sub, super)  :- FinalClass(super), Subtype(sub, super), if sub != super.
        };
        let res = query fcs, rule, classHierarchyAnalysis(types) select tpe from ExtendsFinalClass(tpe, _);
        not Vector.isEmpty(res)

    pub def nonSupertypes(types: Program): Vector[String] =
        query classHierarchyAnalysis(types) select tpe from NonSupertype(tpe)

    pub def finalClassExtensions(types: Program): Vector[(String, String)] =
        query classHierarchyAnalysis(types) select (sub, sup) from ExtendsFinalClass(sub, sup)

    pub def isSubtype(subtype: String, possibleSupertype: String, types: Program): Bool =
        let res = query classHierarchyAnalysis(types) select subtype from Subtype(subtype, possibleSupertype);
        not Vector.isEmpty(res)

    pub def hasMethod(tpe: String, method: String, types: Program): Bool =
        let res = query classHierarchyAnalysis(types) select tpe from AvailableMethod(tpe, method);
        not Vector.isEmpty(res)

    pub def isNonSupertype(tpe: String, types: Program): Bool =
        not (nonSupertypes(types) |> Vector.filter(Eq.eq(tpe)) |> Vector.isEmpty)

    /////////////////////////////////////////////////
    // Tests                                       //
    /////////////////////////////////////////////////

    mod Test {

        @Test
        def testSubtypes01(): Bool =
            let result = Subtyping.subtypesDistinct(emptyFacts());
            Assert.eq(Vector#{}, result)

        @Test
        def testSubtypes02(): Bool =
            let result = Subtyping.subtypesDistinct(facts());
            let expected = Vector#{("au.content.Marker", "java.lang.Object"), ("java.lang.String", "java.io.Serializable"), ("java.lang.String", "java.lang.Object"), ("java.util.ArrayList", "java.lang.Object"), ("java.util.ArrayList", "java.util.List"), ("java.util.ArrayList", "java.util.Collection"), ("java.util.List", "java.util.Collection"), ("au.content.SpecialArrayList", "java.util.ArrayList"), ("au.content.SpecialArrayList", "java.lang.Object"), ("au.content.SpecialArrayList", "java.util.List"), ("au.content.SpecialArrayList", "java.util.Collection")} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testSubtype01(): Bool =
            let result = Subtyping.isSubtype("java.lang.String", "java.lang.Object", emptyFacts());
            Assert.eq(false, result)

        @Test
        def testSubtype02(): Bool =
            let result = Subtyping.isSubtype("java.lang.String", "java.lang.Object", facts());
            Assert.eq(true, result)

        @Test
        def testSubtype03(): Bool =
            let result = Subtyping.isSubtype("java.lang.Object", "java.lang.Object", facts());
            Assert.eq(true, result)

        @Test
        def testSubtype04(): Bool =
            let result = Subtyping.isSubtype("java.lang.Object", "java.lang.String", facts());
            Assert.eq(false, result)

        @Test
        def testSubtype05(): Bool =
            let result = Subtyping.isSubtype("java.util.ArrayList", "java.util.Collection", facts());
            Assert.eq(true, result)

        @Test
        def testSubtype06(): Bool =
            let result = Subtyping.isSubtype("au.content.SpecialArrayList", "java.util.Collection", facts());
            Assert.eq(true, result)

        @Test
        def testSubtype07(): Bool =
            let result = Subtyping.isSubtype("java.util.List", "java.util.List", facts());
            Assert.eq(true, result)

        @Test
        def testHasMethod01(): Bool =
            let result = Subtyping.hasMethod("java.lang.String", "toString", facts());
            Assert.eq(true, result)

        @Test
        def testHasMethod02(): Bool =
            let result = Subtyping.hasMethod("java.util.ArrayList", "add", facts());
            Assert.eq(true, result)

        def emptyFacts(): Subtyping.Program = {
            classes = Vector#{},
            interfaces = Vector#{},
            classImplements = Vector#{},
            classExtends = Vector#{},
            interfaceExtends = Vector#{},
            methods = Vector#{}
        }

        def facts(): Subtyping.Program = {
            classes = Vector#{
                "java.lang.Object",
                "java.lang.String",
                "java.util.ArrayList",
                "au.content.Marker",
                "au.content.SpecialArrayList"
            },
            interfaces = Vector#{
                "java.io.Serializable",
                "java.util.List",
                "java.util.Collection"
            },
            classImplements = Vector#{
                ("java.lang.String", "java.io.Serializable"),
                ("java.util.ArrayList", "java.util.List")
            },
            classExtends = Vector#{
                ("java.lang.String", "java.lang.Object"),
                ("java.util.ArrayList", "java.lang.Object"),
                ("au.content.SpecialArrayList", "java.util.ArrayList"),
                ("au.content.Marker", "java.lang.Object")
            },
            interfaceExtends = Vector#{
                ("java.util.List", "java.util.Collection")
            },
            methods = Vector#{
                ("java.lang.Object", "toString"),
                ("java.util.List", "add"),
                ("java.lang.String", "contains"),
                ("java.lang.String", "isEmpty")
            }
        }
    }
}
