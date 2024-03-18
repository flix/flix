mod Subtyping {

    type alias Types[r : SchemaRow] = #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String), Method(String, String) | r }

    def open(types: #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String), Method(String, String) }): Types[r] = types

    def subtypeRules(): #{ IsSubtype(String, String), Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String) | r } = #{
        IsSubtype(sub, super) :- Class(sub), Class(super), if sub == super.
        IsSubtype(sub, super) :- Interface(sub), Interface(super), if sub == super.
        IsSubtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
        IsSubtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
        IsSubtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
        IsSubtype(sub, super) :- IsSubtype(sub, a), IsSubtype(a, super).
    }

    pub def subtypes(types: Types[#()]): Vector[(String, String)] = {
        let result = query subtypeRules(), open(types) select (sub, super) from IsSubtype(sub, super);
        result |> Vector.filter(match (x, y) -> x != y)
    }

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
            let result = Subtyping.subtypes(#{});
            Assert.eq(Vector#{}, result)

        @Test
        def testSubtypes02(): Bool =
            let result = Subtyping.subtypes(facts());
            let expected = Vector#{("java.lang.String", "java.io.Serializable"), ("java.lang.String", "java.lang.Object"), ("java.util.ArrayList", "java.lang.Object"), ("java.util.ArrayList", "java.util.List"), ("java.util.ArrayList", "java.util.Collection"), ("java.util.List", "java.util.Collection")} |> Vector.sort;
            Assert.eq(expected, result)

        def facts(): Subtyping.Types[r] = #{
            Class("java.lang.Object").
            Class("java.lang.String").
            Class("java.util.ArrayList").
            Interface("java.io.Serializable").
            Interface("java.util.List").
            Interface("java.util.Collection").
            ClassExtends("java.lang.String", "java.lang.Object").
            ClassExtends("java.util.ArrayList", "java.lang.Object").
            ClassExtends("au.content.SpecialArrayList", "java.util.ArrayList").
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
