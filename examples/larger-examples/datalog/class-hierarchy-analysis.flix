mod ClassHierarchyAnalysis {

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
        abstractMethods = Vector[(String, String)],
        throws = Vector[String]
    }

    ///
    /// Performs class hierarchy analysis on the given program `p`.
    ///
    /// The function takes a `Program` as input and returns a set of relations
    /// representing the class hierarchy analysis results. The relations include
    /// `Subtype`, `AvailableMethod`, `AvailableField`, and `InheritedMethod`.
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
    /// The `InheritedMethod` relation represents the methods that are implemented
    /// on a class or by a super class.
    ///
    /// Returns a set of relations representing the class hierarchy analysis results.
    ///
    def classHierarchyAnalysis(p: Program): #{ Subtype(String, String), AvailableField(String, String), AvailableMethod(String, String), InheritedMethod(String, String, String) | r } =
        let cs    = inject p#classes          into Class;
        let is    = inject p#interfaces       into Interface;
        let impls = inject p#classImplements  into ClassImplements;
        let ce    = inject p#classExtends     into ClassExtends;
        let ies   = inject p#interfaceExtends into InterfaceExtends;
        let fs    = inject p#fields           into Field;
        let ms    = inject p#classMethods     into ClassMethod;
        let ams   = inject p#abstractMethods  into AbstractMethod;
        let rules = #{
            Subtype(x, x)       :- Class(x).
            Subtype(x, x)       :- Interface(x).
            Subtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
            Subtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            Subtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            Subtype(sub, super) :- Subtype(sub, a), Subtype(a, super).

            AvailableField(class, field) :- Field(super, field), Subtype(class, super).

            AvailableMethod(class, method) :- Class(class), ClassMethod(super, method), Subtype(class, super).
            AvailableMethod(class, method) :- Class(class), AbstractMethod(super, method), Subtype(class, super).

            InheritedMethod(class, method, class) :- Class(class), ClassMethod(class, method).
            InheritedMethod(sub, method, source)  :- Class(sub), Subtype(sub, super), InheritedMethod(super, method, source), not ClassMethod(sub, method).
        };
        #(Subtype, AvailableField, AvailableMethod, InheritedMethod) -> rules <+> cs <+> is <+> ce <+> impls <+> ies <+> fs <+> ms <+> ams

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
    /// Checks if a type is a subtype of another type in the program.
    /// Returns `true` if the `subtype` is a subtype of the `possibleSupertype`, `false` otherwise.
    ///
    pub def isSubtype(p: Program, subtype: String, possibleSupertype: String): Bool =
        let res = query classHierarchyAnalysis(p) select subtype from Subtype(subtype, possibleSupertype);
        not Vector.isEmpty(res)

    ///
    /// Returns a vector of pairs where each pair `(class, method)`
    /// represents that an implementation of `method` is missing from
    /// `class` in the program `p`.
    ///
    pub def missingMethods(p: Program): Vector[(String, String)] =
        let cs   = inject p#classes into Class;
        let rule = #{
            ConcreteMethod(class, method) :- InheritedMethod(class, method, _).
            MissingMethod(class, method)  :- Class(class), AvailableMethod(class, method), not ConcreteMethod(class, method).
        };
        query cs, rule, classHierarchyAnalysis(p) select (class, method) from MissingMethod(class, method)

    ///
    /// Returns a vector of types that extend the final classes in the program `p`.
    ///
    pub def illegalFinalClassExtension(p: Program): Vector[String] =
        let fcs  = inject p#finalClasses into Final;
        let rule = #{
            ExtendsFinalClass(sub, super)  :- Final(super), Subtype(sub, super), if sub != super.
        };
        query fcs, rule, classHierarchyAnalysis(p) select tpe from ExtendsFinalClass(tpe, _)

    ///
    /// Returns a vector of types that are thrown in the `p` program but are not subtypes of `java.lang.Throwable`.
    ///
    pub def illegalNonThrowable(p: Program): Vector[String] =
        let ths = inject p#throws into Throws;
        let rule = #{
            IllegalThrowable(t) :- Throws(t), not Subtype(t, "java.lang.Throwable").
        };
        query ths, rule, classHierarchyAnalysis(p) select tpe from IllegalThrowable(tpe)

    ///
    /// Finds and returns a vector of tuples representing illegal multiple inheritance in the program `p`.
    ///
    pub def illegalMultipleInheritance(p: Program): Vector[(String, String, String)] =
        let cs   = inject p#classes into Class;
        let rule = #{
            IllegalMultipleInheritance(class, super1, super2) :-
                Class(class),
                Class(super1),
                Class(super2),
                Subtype(class, super1),
                Subtype(class, super2),
                if  class  != super1
                and class  != super2
                and super1 != super2.
        };
        query cs, rule, classHierarchyAnalysis(p) select (class, super1, super2) from IllegalMultipleInheritance(class, super1, super2)


    /////////////////////////////////////////////////
    // Tests                                       //
    /////////////////////////////////////////////////

    mod Test {

        @Test
        def testIsSubtype01(): Bool =
            let result = ClassHierarchyAnalysis.isSubtype(emptyFacts(), "java.lang.String", "java.lang.Object");
            Assert.eq(false, result)

        @Test
        def testIsSubtype02(): Bool =
            let result = ClassHierarchyAnalysis.isSubtype(facts(), "java.lang.String", "java.lang.Object");
            Assert.eq(true, result)

        @Test
        def testIsSubtype03(): Bool =
            let result = ClassHierarchyAnalysis.isSubtype(facts(), "java.lang.Object", "java.lang.Object");
            Assert.eq(true, result)

        @Test
        def testIsSubtype04(): Bool =
            let result = ClassHierarchyAnalysis.isSubtype(facts(), "java.lang.Object", "java.lang.String");
            Assert.eq(false, result)

        @Test
        def testIsSubtype05(): Bool =
            let result = ClassHierarchyAnalysis.isSubtype(facts(), "java.util.ArrayList", "java.util.Collection");
            Assert.eq(true, result)

        @Test
        def testIsSubtype06(): Bool =
            let result = ClassHierarchyAnalysis.isSubtype(facts(), "au.content.SpecialArrayList", "java.util.Collection");
            Assert.eq(true, result)

        @Test
        def testIsSubtype07(): Bool =
            let result = ClassHierarchyAnalysis.isSubtype(facts(), "java.util.List", "java.util.List");
            Assert.eq(true, result)

        @Test
        def testHasField01(): Bool =
            let result = ClassHierarchyAnalysis.hasField(facts(), "elementData");
            let expected = Vector#{
                "java.util.ArrayList",
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasField02(): Bool =
            let result = ClassHierarchyAnalysis.hasField(facts(), "size");
            let expected = Vector#{
                "java.util.ArrayList",
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasField03(): Bool =
            let result = ClassHierarchyAnalysis.hasField(facts(), "specialField");
            let expected = Vector#{
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasField04(): Bool =
            let result = ClassHierarchyAnalysis.hasField(facts(), "markerField");
            let expected = Vector#{
                "au.content.Marker"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasMethod01(): Bool =
            let result = ClassHierarchyAnalysis.hasMethod(facts(), "toString");
            let expected = facts()#classes |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasMethod02(): Bool =
            let result = ClassHierarchyAnalysis.hasMethod(facts(), "add");
            let expected = Vector#{
                "java.util.ArrayList",
                "au.content.SpecialArrayList"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testHasMethod03(): Bool =
            let result = ClassHierarchyAnalysis.hasMethod(facts(), "contains");
            let expected = Vector#{
                "java.lang.String"
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testmissingMethodss01(): Bool =
            let result = ClassHierarchyAnalysis.missingMethods(facts());
            let expected = Vector#{
                ("java.util.ArrayList", "isEmpty"),
                ("au.content.SpecialArrayList", "isEmpty")
            } |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalFinalClassExtension01(): Bool =
            let result = ClassHierarchyAnalysis.illegalFinalClassExtension({ finalClasses = Vector#{"java.lang.String"} | facts() });
            let expected = Vector#{} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalFinalClassExtension02(): Bool =
            let result = ClassHierarchyAnalysis.illegalFinalClassExtension({ finalClasses = Vector#{"java.util.ArrayList"} | facts() });
            let expected = Vector#{"au.content.SpecialArrayList"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalFinalClassExtension03(): Bool =
            let result = ClassHierarchyAnalysis.illegalFinalClassExtension({ finalClasses = Vector#{"java.lang.Object"} | facts() });
            let expected = facts()#classes |> Vector.filter(x -> x != "java.lang.Object") |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalFinalClassExtension04(): Bool =
            let result = ClassHierarchyAnalysis.illegalFinalClassExtension({ finalClasses = Vector#{"java.lang.Object", "java.util.ArrayList"} | facts() } );
            let expected = facts()#classes |> Vector.filter(x -> x != "java.lang.Object") |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalNonThrowable01(): Bool =
            let result = ClassHierarchyAnalysis.illegalNonThrowable({ throws = Vector#{"java.lang.String"} | facts() });
            let expected = Vector#{"java.lang.String"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalNonThrowable02(): Bool =
            let result = ClassHierarchyAnalysis.illegalNonThrowable({ throws = Vector#{"java.lang.String", "java.util.ArrayList"} | facts() });
            let expected = Vector#{"java.lang.String", "java.util.ArrayList"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalNonThrowable03(): Bool =
            let result = ClassHierarchyAnalysis.illegalNonThrowable({ throws = Vector#{"java.lang.String", "java.util.ArrayList", "java.lang.Throwable"} | facts() });
            let expected = Vector#{"java.lang.String", "java.util.ArrayList"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalNonThrowable04(): Bool =
            let result = ClassHierarchyAnalysis.illegalNonThrowable({ throws = Vector#{"java.lang.String", "java.util.ArrayList", "java.lang.Throwable", "java.lang.Exception"} | facts() });
            let expected = Vector#{"java.lang.String", "java.util.ArrayList"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIllegalMulitpleInheritance01(): Bool =
            let result = ClassHierarchyAnalysis.illegalMultipleInheritance(facts()) |> Vector.memberOf(("au.content.MultipleInheritance", "au.content.Super1", "au.content.Super2"));
            Assert.eq(true, result)

        @Test
        def testIllegalMulitpleInheritance02(): Bool =
            let result = ClassHierarchyAnalysis.illegalMultipleInheritance(facts()) |> Vector.memberOf(("au.content.MultipleInheritance", "au.content.Super2", "au.content.Super1"));
            Assert.eq(true, result)

        @Test
        def testIllegalMulitpleInheritance04(): Bool =
            let result = ClassHierarchyAnalysis.illegalMultipleInheritance(facts()) |> Vector.map(match (x, _, _) -> x) |> Vector.memberOf("java.lang.Object");
            Assert.eq(false, result)

        def emptyFacts(): ClassHierarchyAnalysis.Program = {
            classes = Vector#{},
            finalClasses = Vector#{},
            interfaces = Vector#{},
            classImplements = Vector#{},
            classExtends = Vector#{},
            interfaceExtends = Vector#{},
            fields = Vector#{},
            classMethods = Vector#{},
            abstractMethods = Vector#{},
            throws = Vector#{}
        }

        def facts(): ClassHierarchyAnalysis.Program = {
            classes = Vector#{
                "java.lang.Object",
                "java.lang.String",
                "java.lang.Throwable",
                "java.lang.Exception",
                "java.util.ArrayList",
                "au.content.Marker",
                "au.content.SpecialArrayList",
                "au.content.Super1",
                "au.content.Super2",
                "au.content.MultipleInheritance"
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
                ("au.content.Marker", "java.lang.Object"),
                ("au.content.Super1", "java.lang.Object"),
                ("au.content.Super2", "java.lang.Object"),
                ("au.content.MultipleInheritance", "au.content.Super1"),
                ("au.content.MultipleInheritance", "au.content.Super2")
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
            abstractMethods = Vector#{
                ("java.util.List", "add"),
                ("java.util.Collection", "isEmpty")
            },
            throws = Vector#{}
        }
    }
}
