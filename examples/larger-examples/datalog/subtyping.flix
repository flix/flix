mod Subtyping {

    ///
    /// Represents the entities in a Java program.
    ///
    type alias Program = {
        classes = Vector[String],
        finalClasses = Vector[String],
        interfaces = Vector[String],
        classImplements = Vector[(String, String)],
        classExtends = Vector[(String, String)],
        interfaceExtends = Vector[(String, String)],
        fields = Vector[(String, String)],
        classMethods = Vector[(String, String)],
        interfaceSignatures = Vector[(String, String)],
        throws = Vector[String]
    }

    ///
    /// Performs class hierarchy analysis on the given program `p`.
    ///
    /// The function takes a `Program` as input and returns a set of relations
    /// representing the class hierarchy analysis results. The relations include
    /// `Subtype`, `AvailableMethod`, `AvailableField`, and `NonImplementedMethod`.
    ///
    /// The `Subtype` relation represents the subtyping relationship between classes
    /// and interfaces. It includes rules for direct subtyping, class implementing
    /// an interface, class extending another class, and interface extending another
    /// interface. It also includes transitive subtyping rules.
    ///
    /// The `AvailableMethod` relation represents the available methods for a given
    /// class. It includes methods defined in the class itself and methods inherited
    /// from its supertypes.
    ///
    /// The `AvailableField` relation represents the available fields for a given
    /// class. It includes fields defined in the class itself and fields inherited
    /// from its supertypes.
    ///
    /// The `NonImplementedMethod` relation represents the methods declared in an
    /// interface that are not implemented by a class.
    ///
    /// Returns a set of relations representing the class hierarchy analysis results.
    ///
    def classHierarchyAnalysis(p: Program): #{ Subtype(String, String), AvailableField(String, String), AvailableMethod(String, String), NonImplementedMethod(String, String) | r } =
        let cs    = inject p.classes             into Class;
        let is    = inject p.interfaces          into Interface;
        let impls = inject p.classImplements     into ClassImplements;
        let ce    = inject p.classExtends        into ClassExtends;
        let ies   = inject p.interfaceExtends    into InterfaceExtends;
        let fs    = inject p.fields              into Field;
        let ms    = inject p.classMethods        into ClassMethod;
        let iss   = inject p.interfaceSignatures into InterfaceSignature;
        let rules = #{
            Subtype(x, x)       :- Class(x).
            Subtype(x, x)       :- Interface(x).
            Subtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
            Subtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            Subtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            Subtype(sub, super) :- Subtype(sub, a), Subtype(a, super).

            AvailableField(class, field)        :- Field(t, field), Subtype(class, t).
            AvailableMethod(class, method)      :- ClassMethod(t, method), Subtype(class, t).
            NonImplementedMethod(class, method) :- InterfaceSignature(t, method), Class(class), Subtype(class, t), not AvailableMethod(class, method).
        };
        #(Subtype, AvailableField, AvailableMethod, NonImplementedMethod) -> rules <+> cs <+> is <+> ce <+> impls <+> ies <+> fs <+> ms <+> iss

    ///
    /// Returns a vector of types that have the specified field `field`.
    ///
    pub def hasField(p: Program, field: String): Vector[String] =
        query classHierarchyAnalysis(p) select tpe from AvailableField(tpe, field)

    ///
    /// Returns a vector of types that have the specified method `method`.
    ///
    pub def hasMethod(p: Program, method: String): Vector[String] =
        query classHierarchyAnalysis(p) select tpe from AvailableMethod(tpe, method)

    ///
    /// Returns a vector of pairs representing non-implemented methods in the program.
    /// Each pair contains the class and method name.
    ///
    pub def nonImplementedMethods(p: Program): Vector[(String, String)] =
        query classHierarchyAnalysis(p) select (tpe, method) from NonImplementedMethod(tpe, method)

    ///
    /// Checks if a type is a subtype of another type in the program.
    /// Returns `true` if the `subtype` is a subtype of the `possibleSupertype`, `false` otherwise.
    ///
    pub def isSubtype(p: Program, subtype: String, possibleSupertype: String): Bool =
        let res = query classHierarchyAnalysis(p) select subtype from Subtype(subtype, possibleSupertype);
        not Vector.isEmpty(res)

    ///
    /// Returns a vector of types that extend the final classes in the program `p`.
    ///
    pub def illegalFinalClasses(p: Program): Vector[String] =
        let fcs  = inject p.finalClasses into Final;
        let rule = #{
            ExtendsFinalClass(sub, super)  :- Final(super), Subtype(sub, super), if sub != super.
        };
        query fcs, rule, classHierarchyAnalysis(p) select tpe from ExtendsFinalClass(tpe, _)

    ///
    /// Returns a vector of types that are thrown in the `p` program but are not subtypes of `java.lang.Throwable`.
    ///
    pub def illegalNonThrowable(p: Program): Vector[String] =
        let ths = inject p.throws into Throws;
        let rule = #{
            IllegalThrowable(t) :- Throws(t), not Subtype(t, "java.lang.Throwable").
        };
        query ths, rule, classHierarchyAnalysis(p) select tpe from IllegalThrowable(tpe)


    /////////////////////////////////////////////////
    // Tests                                       //
    /////////////////////////////////////////////////

    mod Test {

        @Test
        def testIsSubtype01(): Bool =
            let result = Subtyping.isSubtype(emptyFacts(), "java.lang.String", "java.lang.Object");
            Assert.eq(false, result)

        @Test
        def testIsSubtype02(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.lang.String", "java.lang.Object");
            Assert.eq(true, result)

        @Test
        def testIsSubtype03(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.lang.Object", "java.lang.Object");
            Assert.eq(true, result)

        @Test
        def testIsSubtype04(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.lang.Object", "java.lang.String");
            Assert.eq(false, result)

        @Test
        def testIsSubtype05(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.util.ArrayList", "java.util.Collection");
            Assert.eq(true, result)

        @Test
        def testIsSubtype06(): Bool =
            let result = Subtyping.isSubtype(facts(), "au.content.SpecialArrayList", "java.util.Collection");
            Assert.eq(true, result)

        @Test
        def testIsSubtype07(): Bool =
            let result = Subtyping.isSubtype(facts(), "java.util.List", "java.util.List");
            Assert.eq(true, result)

        @Test
        def testHasField01(): Bool =
            let result = Subtyping.hasField(facts(), "elementData");
            let expected = Vector#{
                "java.util.ArrayList",
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasField02(): Bool =
            let result = Subtyping.hasField(facts(), "size");
            let expected = Vector#{
                "java.util.ArrayList",
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasField03(): Bool =
            let result = Subtyping.hasField(facts(), "specialField");
            let expected = Vector#{
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasField04(): Bool =
            let result = Subtyping.hasField(facts(), "markerField");
            let expected = Vector#{
                "au.content.Marker"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasMethod01(): Bool =
            let result = Subtyping.hasMethod(facts(), "toString");
            let expected = Vector#{
                "java.lang.Object",
                "java.lang.String",
                "java.lang.Throwable",
                "java.lang.Exception",
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
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasMethod03(): Bool =
            let result = Subtyping.hasMethod(facts(), "contains");
            let expected = Vector#{
                "java.lang.String"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testNonImplementedMethods01(): Bool =
            let result = Subtyping.nonImplementedMethods(facts());
            let expected = Vector#{
                ("java.util.ArrayList", "isEmpty"),
                ("au.content.SpecialArrayList", "isEmpty")
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalFinalClasses01(): Bool =
            let result = Subtyping.illegalFinalClasses({ finalClasses = Vector#{"java.lang.String"} | facts() });
            let expected = Vector#{} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalFinalClasses02(): Bool =
            let result = Subtyping.illegalFinalClasses({ finalClasses = Vector#{"java.util.ArrayList"} | facts() });
            let expected = Vector#{"au.content.SpecialArrayList"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalFinalClasses03(): Bool =
            let result = Subtyping.illegalFinalClasses({finalClasses = Vector#{"java.lang.Object"} | facts() });
            let expected = Vector#{
                "java.lang.String",
                "java.util.ArrayList",
                "java.lang.Throwable",
                "java.lang.Exception",
                "au.content.Marker",
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalFinalClasses04(): Bool =
            let result = Subtyping.illegalFinalClasses({ finalClasses = Vector#{"java.lang.Object", "java.util.ArrayList"} | facts() } );
            let expected = Vector#{
                "java.lang.String",
                "java.util.ArrayList",
                "java.lang.Throwable",
                "java.lang.Exception",
                "au.content.Marker",
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalNonThrowable01(): Bool =
            let result = Subtyping.illegalNonThrowable({ throws = Vector#{"java.lang.String"} | facts() });
            let expected = Vector#{"java.lang.String"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalNonThrowable02(): Bool =
            let result = Subtyping.illegalNonThrowable({ throws = Vector#{"java.lang.String", "java.util.ArrayList"} | facts() });
            let expected = Vector#{"java.lang.String", "java.util.ArrayList"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalNonThrowable03(): Bool =
            let result = Subtyping.illegalNonThrowable({ throws = Vector#{"java.lang.String", "java.util.ArrayList", "java.lang.Throwable"} | facts() });
            let expected = Vector#{"java.lang.String", "java.util.ArrayList"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalNonThrowable04(): Bool =
            let result = Subtyping.illegalNonThrowable({ throws = Vector#{"java.lang.String", "java.util.ArrayList", "java.lang.Throwable", "java.lang.Exception"} | facts() });
            let expected = Vector#{"java.lang.String", "java.util.ArrayList"} |> Vector.sort;
            Assert.eq(expected, result)

        def emptyFacts(): Subtyping.Program = {
            classes = Vector#{},
            finalClasses = Vector#{},
            interfaces = Vector#{},
            classImplements = Vector#{},
            classExtends = Vector#{},
            interfaceExtends = Vector#{},
            fields = Vector#{},
            classMethods = Vector#{},
            interfaceSignatures = Vector#{},
            throws = Vector#{}
        }

        def facts(): Subtyping.Program = {
            classes = Vector#{
                "java.lang.Object",
                "java.lang.String",
                "java.lang.Throwable",
                "java.lang.Exception",
                "java.util.ArrayList",
                "au.content.Marker",
                "au.content.SpecialArrayList"
            },
            finalClasses = Vector#{},
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
                ("java.lang.Throwable", "java.lang.Object"),
                ("java.lang.Exception", "java.lang.Throwable"),
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
            classMethods = Vector#{
                ("java.util.ArrayList", "add"),
                ("java.lang.Object", "toString"),
                ("java.lang.String", "contains"),
                ("java.lang.String", "isEmpty")
            },
            interfaceSignatures = Vector#{
                ("java.util.List", "add"),
                ("java.util.Collection", "isEmpty")
            },
            throws = Vector#{}
        }
    }
}
