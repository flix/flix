mod Subtyping {

    type alias Program = {
        classes = Vector[String],
        interfaces = Vector[String],
        classImplements = Vector[(String, String)],
        classExtends = Vector[(String, String)],
        interfaceExtends = Vector[(String, String)],
        fields = Vector[(String, String)],
        methods = Vector[(String, String)]
    }

    def classHierarchyAnalysis(p: Program): #{ Subtype(String, String), AvailableField(String, String), AvailableMethod(String, String), NonSupertype(String) | r } =
        let cs    = inject p.classes          into Class;
        let is    = inject p.interfaces       into Interface;
        let impls = inject p.classImplements  into ClassImplements;
        let ce    = inject p.classExtends     into ClassExtends;
        let ies   = inject p.interfaceExtends into InterfaceExtends;
        let fs    = inject p.fields           into Field;
        let ms    = inject p.methods          into Method;
        let rules = #{
            Subtype(x, x)       :- Class(x).
            Subtype(x, x)       :- Interface(x).
            Subtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
            Subtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            Subtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            Subtype(sub, super) :- Subtype(sub, a), Subtype(a, super).

            AvailableField(class, field)   :- Field(t, field), Subtype(class, t).
            AvailableMethod(class, method) :- Method(t, method), Subtype(class, t).
            NonSupertype(class)            :- Class(class), Class(otherClass), not Subtype(otherClass, class), if class != otherClass.
        };
        #(Subtype, AvailableField, AvailableMethod, NonSupertype) -> rules <+> cs <+> is <+> ce <+> impls <+> ies <+> fs <+> ms

    pub def distinctSubtypes(p: Program): Vector[(String, String)] =
        let v = query classHierarchyAnalysis(p) select (x, y) from Subtype(x, y);
        Vector.filter(match (x, y) -> x != y, v)

    pub def illegalFinalClasses(p: Program, finalClasses: Vector[String]): Vector[String] =
        let fcs  = inject finalClasses into FinalClass;
        let rule = #{
            ExtendsFinalClass(sub, super)  :- FinalClass(super), Subtype(sub, super), if sub != super.
        };
        query fcs, rule, classHierarchyAnalysis(p) select tpe from ExtendsFinalClass(tpe, _)

    pub def illegalNonThrowable(p: Program, throws: Vector[String]): Vector[String] =
        let ths = inject throws into Throws;
        let rule = #{
            IllegalThrowable(t) :- Throws(t), not Subtype(t, "java.lang.Throwable").
        };
        query ths, rule, classHierarchyAnalysis(p) select tpe from IllegalThrowable(tpe)

    pub def isSubtype(p: Program, subtype: String, possibleSupertype: String): Bool =
        let res = query classHierarchyAnalysis(p) select subtype from Subtype(subtype, possibleSupertype);
        not Vector.isEmpty(res)

    pub def hasField(p: Program, field: String): Vector[String] =
        query classHierarchyAnalysis(p) select tpe from AvailableField(tpe, field)

    pub def hasMethod(p: Program, method: String): Vector[String] =
        query classHierarchyAnalysis(p) select tpe from AvailableMethod(tpe, method)

    pub def leafTypes(p: Program): Vector[String] =
        query classHierarchyAnalysis(p) select tpe from NonSupertype(tpe)


    /////////////////////////////////////////////////
    // Tests                                       //
    /////////////////////////////////////////////////

    mod Test {

        @Test
        def testSubtypes01(): Bool =
            let result = Subtyping.distinctSubtypes(emptyFacts());
            Assert.eq(Vector#{}, result)

        @Test
        def testSubtypes02(): Bool =
            let result = Subtyping.distinctSubtypes(facts());
            let expected = Vector#{("au.content.Marker", "java.lang.Object"), ("java.lang.String", "java.io.Serializable"), ("java.lang.String", "java.lang.Object"), ("java.util.ArrayList", "java.lang.Object"), ("java.util.ArrayList", "java.util.List"), ("java.util.ArrayList", "java.util.Collection"), ("java.util.List", "java.util.Collection"), ("au.content.SpecialArrayList", "java.util.ArrayList"), ("au.content.SpecialArrayList", "java.lang.Object"), ("au.content.SpecialArrayList", "java.util.List"), ("au.content.SpecialArrayList", "java.util.Collection")} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testSubtype01(): Bool =
            let result = Subtyping.isSubtype(emptyFacts(), "java.lang.String", "java.lang.Object");
            Assert.eq(false, result)

        @Test
        def testSubtype02(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.lang.String", "java.lang.Object");
            Assert.eq(true, result)

        @Test
        def testSubtype03(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.lang.Object", "java.lang.Object");
            Assert.eq(true, result)

        @Test
        def testSubtype04(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.lang.Object", "java.lang.String");
            Assert.eq(false, result)

        @Test
        def testSubtype05(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.util.ArrayList", "java.util.Collection");
            Assert.eq(true, result)

        @Test
        def testSubtype06(): Bool =
            let result = Subtyping.isSubtype(facts(), "au.content.SpecialArrayList", "java.util.Collection");
            Assert.eq(true, result)

        @Test
        def testSubtype07(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.util.List", "java.util.List");
            Assert.eq(true, result)

        @Test
        def testHasMethod01(): Bool =
            let result = Subtyping.hasMethod(facts(), "toString");
            let expected = Vector#{
                "java.lang.Object",
                "java.lang.String",
                "java.util.ArrayList",
                "au.content.Marker",
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasMethod02(): Bool =
            let result = Subtyping.hasMethod(facts(), "add");
            let expected = Vector#{
                "java.util.ArrayList",
                "au.content.SpecialArrayList",
                "java.util.List"
            } |> Vector.sort;
            Assert.eq(expected, result)

        def emptyFacts(): Subtyping.Program = {
            classes = Vector#{},
            interfaces = Vector#{},
            classImplements = Vector#{},
            classExtends = Vector#{},
            interfaceExtends = Vector#{},
            fields = Vector#{},
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
            fields = Vector#{
                ("java.util.ArrayList", "elementData"),
                ("java.util.ArrayList", "size"),
                ("au.content.SpecialArrayList", "specialField"),
                ("au.content.Marker", "markerField")
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
