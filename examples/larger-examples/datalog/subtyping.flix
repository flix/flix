mod Subtyping {

    type alias Program = {
        classes = Vector[String],
        finalClasses = Vector[String],
        interfaces = Vector[String],
        implementations = Vector[(String, String)],
        classExtends = Vector[(String, String)],
        interfaceExtends = Vector[(String, String)],
        methods = Vector[(String, String)]
    }

    def subtypes(program: Program): #{ Subtype(String, String), AvailableMethod(String, String), NonSupertype(String), ExtendsFinalClass(String, String) | r } =
        let rules = #{
            Subtype(x, x)       :- Class(x).
            Subtype(x, x)       :- Interface(x).
            Subtype(sub, super) :- Class(sub), Interface(super), Implementation(sub, super).
            Subtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            Subtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            Subtype(sub, super) :- Subtype(sub, a), Subtype(a, super).

            AvailableMethod(class, method) :- Method(t, method), Subtype(class, t).
            NonSupertype(class)            :- Class(class), Class(otherClass), not Subtype(otherClass, class), if class != otherClass.
            ExtendsFinalClass(sub, super)  :- FinalClass(super), Subtype(sub, super), if sub != super.
        };
        #(Subtype, AvailableMethod, NonSupertype, ExtendsFinalClass) -> rules <+> toPredicates(program)

    def toPredicates(program: Program): #{ Class(String), FinalClass(String), Interface(String), ClassExtends(String, String),  InterfaceExtends(String, String), Implementation(String, String), Method(String, String) | r } =
        let cs    = inject program.classes          into Class;
        let fcs   = inject program.finalClasses     into FinalClass;
        let is    = inject program.interfaces       into Interface;
        let impls = inject program.implementations  into Implementation;
        let ce    = inject program.classExtends     into ClassExtends;
        let ies   = inject program.interfaceExtends into InterfaceExtends;
        let ms    = inject program.methods          into Method;
        cs <+> fcs <+> is <+> ce <+> impls <+> ies <+> ms

    pub def subtypesDistinct(types: Program): Vector[(String, String)] =
        let v = query subtypes(types) select (x, y) from Subtype(x, y);
        Vector.filter(match (x, y) -> x != y, v)

    pub def methodsOf(tpe: String, types: Program): Vector[String] =
        query subtypes(types) select m from AvailableMethod(tpe, m)

    pub def nonSupertypes(types: Program): Vector[String] =
        query subtypes(types) select tpe from NonSupertype(tpe)

    pub def finalClassExtensions(types: Program): Vector[(String, String)] =
        query subtypes(types) select (sub, sup) from ExtendsFinalClass(sub, sup)

    pub def isSubtype(subtype: String, possibleSupertype: String, types: Program): Bool =
        let v = query subtypes(types) select (x, y) from Subtype(x, y);
        not (Vector.filter(Eq.eq((subtype, possibleSupertype)), v) |> Vector.isEmpty)

    pub def hasMethod(tpe: String, method: String, types: Program): Bool =
        not (methodsOf(tpe, types) |> Vector.filter(Eq.eq(method)) |> Vector.isEmpty)

    pub def isNonSupertype(tpe: String, types: Program): Bool =
        not (nonSupertypes(types) |> Vector.filter(Eq.eq(tpe)) |> Vector.isEmpty)

    pub def extendsFinalClass(tpe: String, types: Program): Bool =
        not (finalClassExtensions(types) |> Vector.filter(match (sub, _) -> sub == tpe) |> Vector.isEmpty)


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
        def testMethodsOf01(): Bool =
            let result = Subtyping.methodsOf("java.lang.String", facts());
            let expected = Vector#{"toString", "contains", "isEmpty"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testMethodsOf02(): Bool =
            let result = Subtyping.methodsOf("java.util.ArrayList", facts());
            let expected = Vector#{"toString", "add"} |> Vector.sort;
            Assert.eq(expected, result)

        def emptyFacts(): Subtyping.Program = {
            classes = Vector#{},
            finalClasses = Vector#{},
            interfaces = Vector#{},
            implementations = Vector#{},
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
            finalClasses = Vector#{
                "java.lang.String"
            },
            interfaces = Vector#{
                "java.io.Serializable",
                "java.util.List",
                "java.util.Collection"
            },
            implementations = Vector#{
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
