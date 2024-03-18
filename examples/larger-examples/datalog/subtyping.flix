mod Subtyping {

    type alias Types[r : SchemaRow] = #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String), Method(String, String) | r }

    def open(types: #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String), Method(String, String) }): Types[r] = types

    pub def subtypes(types: Types[#()]): Vector[(String, String)] = {
        let rules = #{
            IsSubtype(sub, super) :- Class(sub), Class(super), if sub == super.
            IsSubtype(sub, super) :- Interface(sub), Interface(super), if sub == super.
            IsSubtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            IsSubtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
            IsSubtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            IsSubtype(sub, super) :- IsSubtype(sub, a), IsSubtype(a, super).
        };
        query rules, open(types) select (sub, super) from IsSubtype(sub, super)
    }

    pub def methodsOf(tpe: String, types: Types[#()]): Vector[String] =
        let subs = inject subtypes(types) into IsSubtype;
        let rules = #{
            AvailableMethod(class, method) :- Method(t, method), IsSubtype(class, t).
        };
        query rules, subs, open(types) select m from AvailableMethod(tpe, m)

    pub def isSubtype(subtype: String, possibleSupertype: String, types: Types[#()]): Bool =
        not (subtypes(types) |> Vector.filter(Eq.eq((subtype, possibleSupertype))) |> Vector.isEmpty)

}
