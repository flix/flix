mod Subtyping {

    type alias Types[r : SchemaRow] = #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String), Method(String, String) | r }

    def open(types: #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String), Method(String, String) }): Types[r] = types

    pub def subtypes(classes: List[String], interfaces: List[String], classExtensions: List[(String, String)], implementations: List[(String, String)], interfaceExtensions: List[(String, String)], methods: List[String]): #{ IsSubtype(String, String)| r } =
        let cs    = inject classes             into Class;
        let is    = inject interfaces          into Interface;
        let ce    = inject classExtensions     into ClassExtends;
        let impls = inject implementations     into ClassImplements;
        let ies   = inject interfaceExtensions into InterfaceExtends;
        let ms    = inject methods             into Method;
        let rules = #(IsSubtype) -> #{
            IsSubtype(sub, super) :- Class(sub), Class(super), if sub == super.
            IsSubtype(sub, super) :- Interface(sub), Interface(super), if sub == super.
            IsSubtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            IsSubtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
            IsSubtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            IsSubtype(sub, super) :- IsSubtype(sub, a), IsSubtype(a, super).
        };
        solve cs, is, ce, impls, ies, ms, rules project IsSubtype

    pub def subtypesDistinct(types: Types[#()]): Vector[(String, String)] =
        subtypes(types) |> Vector.filter(match (x, y) -> x != y)

    pub def methodsOf(tpe: String, types: Types[#()]): Vector[String] =
        let rules = #{
            AvailableMethod(class, method) :- Method(t, method), IsSubtype(class, t).
        };
        query rules, subtypeRules(), open(types) select m from AvailableMethod(tpe, m)

    pub def nonSupertypes(types: Types[#()]): Vector[String] =
        let rules = #{
            NonSupertype(class) :- Class(class), Class(otherClass), not IsSubtype(otherClass, class), if class != otherClass.
        };
        query rules, subtypeRules(), open(types) select tpe from NonSupertype(tpe)

    pub def inheritanceCycles(types: Types[#()]): Vector[(String, String)] =
        let rules = #{
            InheritanceCycle(sub, super) :- IsSubtype(sub, super), IsSubtype(super, sub), if sub != super.
        };
        query rules, subtypeRules(), open(types) select (sub, super) from InheritanceCycle(sub, super)

    pub def objectOnlyExtensions(types: Types[#()]): Vector[String] =
        let rules = #{
            ExtendsOnlyObject(class) :- ClassExtends(class, "java.lang.Object"), ClassExtends(class, otherClass), if otherClass == "java.lang.Object".
        };
        query rules, subtypeRules(), open(types) select class from ExtendsOnlyObject(class)

    pub def isSubtype(subtype: String, possibleSupertype: String, types: Types[#()]): Bool =
        not (subtypes(types) |> Vector.filter(Eq.eq((subtype, possibleSupertype))) |> Vector.isEmpty)

    pub def hasMethod(tpe: String, method: String, types: Types[#()]): Bool =
        not (methodsOf(tpe, types) |> Vector.filter(Eq.eq(method)) |> Vector.isEmpty)

    pub def isNonSupertype(tpe: String, types: Types[#()]): Bool =
        not (nonSupertypes(types) |> Vector.filter(Eq.eq(tpe)) |> Vector.isEmpty)

    pub def hasInheritanceCycle(types: Types[#()]): Bool =
        not (inheritanceCycles(types) |> Vector.isEmpty)

    pub def extendsOnlyObject(tpe: String, types: Types[#()]): Bool =
        not (objectOnlyExtensions(types) |> Vector.filter(Eq.eq(tpe)) |> Vector.isEmpty)

    mod Test {

        @Test
        def testSubtypes01(): Bool =
            let result = Subtyping.subtypesDistinct(#{});
            Assert.eq(Vector#{}, result)

        @Test
        def testSubtypes02(): Bool =
            let result = Subtyping.subtypesDistinct(facts());
            let expected = Vector#{("au.content.Marker", "java.lang.Object"), ("java.lang.String", "java.io.Serializable"), ("java.lang.String", "java.lang.Object"), ("java.util.ArrayList", "java.lang.Object"), ("java.util.ArrayList", "java.util.List"), ("java.util.ArrayList", "java.util.Collection"), ("java.util.List", "java.util.Collection"), ("au.content.SpecialArrayList", "java.util.ArrayList"), ("au.content.SpecialArrayList", "java.lang.Object"), ("au.content.SpecialArrayList", "java.util.List"), ("au.content.SpecialArrayList", "java.util.Collection")} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIsSubtype01(): Bool =
            let result = Subtyping.isSubtype("java.lang.String", "java.lang.Object", #{});
            Assert.eq(false, result)

        @Test
        def testIsSubtype02(): Bool =
            let result = Subtyping.isSubtype("java.lang.String", "java.lang.Object", facts());
            Assert.eq(true, result)

        @Test
        def testIsSubtype03(): Bool =
            let result = Subtyping.isSubtype("java.lang.Object", "java.lang.Object", facts());
            Assert.eq(true, result)

        @Test
        def testIsSubtype04(): Bool =
            let result = Subtyping.isSubtype("java.lang.Object", "java.lang.String", facts());
            Assert.eq(false, result)

        @Test
        def testIsSubtype05(): Bool =
            let result = Subtyping.isSubtype("java.util.ArrayList", "java.util.Collection", facts());
            Assert.eq(true, result)

        @Test
        def testIsSubtype06(): Bool =
            let result = Subtyping.isSubtype("au.content.SpecialArrayList", "java.util.Collection", facts());
            Assert.eq(true, result)

        @Test
        def testIsSubtype07(): Bool =
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

        def classes(): List[String] =
            List#{"java.lang.Object", "java.lang.String", "java.util.ArrayList", "au.content.Marker", "au.content.SpecialArrayList"}

        def interfaces(): List[String] =
            List#{"java.io.Serializable", "java.util.List", "java.util.Collection"}

        def classExtensions(): List[(String, String)] =
            List#{("java.lang.String", "java.lang.Object"), ("java.util.ArrayList", "java.lang.Object"), ("au.content.SpecialArrayList", "java.util.ArrayList"), ("au.content.Marker", "java.lang.Object")}

        def facts(): Subtyping.Types[r] = #{
            Class("java.lang.Object").
            Class("java.lang.String").
            Class("java.util.ArrayList").
            Class("au.content.Marker").
            Class("au.content.SpecialArrayList").
            Interface("java.io.Serializable").
            Interface("java.util.List").
            Interface("java.util.Collection").
            ClassExtends("java.lang.String", "java.lang.Object").
            ClassExtends("java.util.ArrayList", "java.lang.Object").
            ClassExtends("au.content.SpecialArrayList", "java.util.ArrayList").
            ClassExtends("au.content.Marker", "java.lang.Object").
            ClassImplements("java.lang.String", "java.io.Serializable").
            ClassImplements("java.util.ArrayList", "java.util.List").
            InterfaceExtends("java.util.List", "java.util.Collection").
            Method("java.lang.Object", "toString").
            Method("java.util.List", "add").
            Method("java.lang.String", "contains").
            Method("java.lang.String", "isEmpty").
        }
    }
}
