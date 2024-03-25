mod Subtyping {

    use JavaType.{Class, Interface, ClassExtends, InterfaceExtends, Implementation, Method};

    pub enum JavaType with Order, Eq, ToString {
        case Class(String),
        case Interface(String),
        case ClassExtends(String, String),
        case InterfaceExtends(String, String),
        case Implementation(String, String),
        case Method(String, String)
    }

    def isClass(tpe: JavaType): Bool = match tpe {
        case Class(_) => true
        case _        => false
    }

    def isInterface(tpe: JavaType): Bool = match tpe {
        case Interface(_) => true
        case _            => false
    }

    def isClassExtends(tpe: JavaType): Bool = match tpe {
        case ClassExtends(_) => true
        case _               => false
    }

    def isImplementation(tpe: JavaType): Bool = match tpe {
        case Implementation(_) => true
        case _                 => false
    }

    def isInterfaceExtends(tpe: JavaType): Bool = match tpe {
        case InterfaceExtends(_) => true
        case _                   => false
    }

    def isMethod(tpe: JavaType): Bool = match tpe {
        case Method(_) => true
        case _         => false
    }

    def extractOne(tpe: JavaType): String = match tpe {
        case Class(s)               => s
        case Interface(s)           => s
        case ClassExtends(s, _)     => s
        case InterfaceExtends(s, _) => s
        case Implementation(s, _)   => s
        case Method(s, _)           => s
    }

    def extractTwo(tpe: JavaType): (String, String) = match tpe {
        case Class(s)                 => (s, "")
        case Interface(s)             => (s, "")
        case ClassExtends(s1, s2)     => (s1, s2)
        case InterfaceExtends(s1, s2) => (s1, s2)
        case Implementation(s1, s2)   => (s1, s2)
        case Method(s1, s2)           => (s1, s2)
    }

    def toPredicates(program: t[JavaType]): #{ Class(String), Interface(String), ClassExtends(String, String),  InterfaceExtends(String, String), Implementation(String, String), Method(String, String) | r } with Foldable[t] =
        // TODO: Use mutable state so we only have to traverse `program` once.
        let cs    = inject program |> Foldable.filter(isClass)            |> Functor.map(extractOne) into Class;
        let is    = inject program |> Foldable.filter(isInterface)        |> Functor.map(extractOne) into Interface;
        let impls = inject program |> Foldable.filter(isImplementation)   |> Functor.map(extractTwo) into Implementation;
        let ce    = inject program |> Foldable.filter(isClassExtends)     |> Functor.map(extractTwo) into ClassExtends;
        let ies   = inject program |> Foldable.filter(isInterfaceExtends) |> Functor.map(extractTwo) into InterfaceExtends;
        let ms    = inject program |> Foldable.filter(isMethod)           |> Functor.map(extractTwo) into Method;
        solve cs, is, ce, impls, ies, ms

    def subtypes(program : t[JavaType]): #{ Subtype(String, String)| r } with Foldable[t] =
        let rules = #(Subtype) -> #{
            Subtype(x, x)       :- Class(x).
            Subtype(x, x)       :- Interface(x).
            Subtype(sub, super) :- Class(sub), Interface(super), Implementation(sub, super).
            Subtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            Subtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            Subtype(sub, super) :- Subtype(sub, a), Subtype(a, super).
        };
        solve toPredicates(program), rules project Subtype

    pub def subtypesDistinct(types: t[JavaType]): Vector[(String, String)] with Foldable[t] =
        let v = query subtypes(types) select (x, y) from Subtype(x, y);
        Vector.filter(match (x, y) -> x != y, v)

    pub def methodsOf(tpe: String, types: t[JavaType]): Vector[String] with Foldable[t] =
        let rules = #{
            AvailableMethod(class, method) :- Method(t, method), IsSubtype(class, t).
        };
        query rules, subtypes(types), toPredicates(types) select m from AvailableMethod(tpe, m)

    pub def nonSupertypes(types: t[JavaType]): Vector[String] with Foldable[t] =
        let rules = #{
            NonSupertype(class) :- Class(class), Class(otherClass), not IsSubtype(otherClass, class), if class != otherClass.
        };
        query rules, subtypes(types), toPredicates(types) select tpe from NonSupertype(tpe)

    pub def inheritanceCycles(types: t[JavaType]): Vector[(String, String)] with Foldable[t] =
        let rules = #{
            InheritanceCycle(sub, super) :- IsSubtype(sub, super), IsSubtype(super, sub), if sub != super.
        };
        query rules, subtypes(types), toPredicates(types) select (sub, super) from InheritanceCycle(sub, super)

    pub def objectOnlyExtensions(types: t[JavaType]): Vector[String] with Foldable[t] =
        let rules = #{
            ExtendsOnlyObject(class) :- ClassExtends(class, "java.lang.Object"), ClassExtends(class, otherClass), if otherClass == "java.lang.Object".
        };
        query rules, subtypes(types), toPredicates(types) select class from ExtendsOnlyObject(class)

    pub def isSubtype(subtype: String, possibleSupertype: String, types: t[JavaType]): Bool with Foldable[t] =
        let v = query subtypes(types) select (x, y) from Subtype(x, y);
        not (Vector.filter(Eq.eq((subtype, possibleSupertype)), v) |> Vector.isEmpty)

    pub def hasMethod(tpe: String, method: String, types: t[JavaType]): Bool with Foldable[t] =
        not (methodsOf(tpe, types) |> Vector.filter(Eq.eq(method)) |> Vector.isEmpty)

    pub def isNonSupertype(tpe: String, types: t[JavaType]): Bool with Foldable[t] =
        not (nonSupertypes(types) |> Vector.filter(Eq.eq(tpe)) |> Vector.isEmpty)

    pub def hasInheritanceCycle(types: t[JavaType]): Bool with Foldable[t] =
        not (inheritanceCycles(types) |> Vector.isEmpty)

    pub def extendsOnlyObject(tpe: String, types: t[JavaType]): Bool with Foldable[t] =
        not (objectOnlyExtensions(types) |> Vector.filter(Eq.eq(tpe)) |> Vector.isEmpty)

    mod Test {

        use Subtyping.JavaType.{Class, Interface, Implementation, ClassExtends, InterfaceExtends, Method};

        @Test
        def testSubtypes01(): Bool =
            let result = Subtyping.subtypesDistinct(Nil);
            Assert.eq(Vector#{}, result)

        @Test
        def testSubtypes02(): Bool =
            let result = Subtyping.subtypesDistinct(facts());
            let expected = Vector#{("au.content.Marker", "java.lang.Object"), ("java.lang.String", "java.io.Serializable"), ("java.lang.String", "java.lang.Object"), ("java.util.ArrayList", "java.lang.Object"), ("java.util.ArrayList", "java.util.List"), ("java.util.ArrayList", "java.util.Collection"), ("java.util.List", "java.util.Collection"), ("au.content.SpecialArrayList", "java.util.ArrayList"), ("au.content.SpecialArrayList", "java.lang.Object"), ("au.content.SpecialArrayList", "java.util.List"), ("au.content.SpecialArrayList", "java.util.Collection")} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testIsSubtype01(): Bool =
            let result = Subtyping.isSubtype("java.lang.String", "java.lang.Object", Nil);
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

        def _classes(): List[String] =
            List#{"java.lang.Object", "java.lang.String", "java.util.ArrayList", "au.content.Marker", "au.content.SpecialArrayList"}

        def _interfaces(): List[String] =
            List#{"java.io.Serializable", "java.util.List", "java.util.Collection"}

        def _classExtensions(): List[(String, String)] =
            List#{("java.lang.String", "java.lang.Object"), ("java.util.ArrayList", "java.lang.Object"), ("au.content.SpecialArrayList", "java.util.ArrayList"), ("au.content.Marker", "java.lang.Object")}

        def facts(): List[Subtyping.JavaType] = List#{
            Class("java.lang.Object"),
            Class("java.lang.String"),
            Class("java.util.ArrayList"),
            Class("au.content.Marker"),
            Class("au.content.SpecialArrayList"),
            Interface("java.io.Serializable"),
            Interface("java.util.List"),
            Interface("java.util.Collection"),
            ClassExtends("java.lang.String", "java.lang.Object"),
            ClassExtends("java.util.ArrayList", "java.lang.Object"),
            ClassExtends("au.content.SpecialArrayList", "java.util.ArrayList"),
            ClassExtends("au.content.Marker", "java.lang.Object"),
            Implementation("java.lang.String", "java.io.Serializable"),
            Implementation("java.util.ArrayList", "java.util.List"),
            InterfaceExtends("java.util.List", "java.util.Collection"),
            Method("java.lang.Object", "toString"),
            Method("java.util.List", "add"),
            Method("java.lang.String", "contains"),
            Method("java.lang.String", "isEmpty")
        }
    }
}
