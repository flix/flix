mod Subtyping {

    type alias Types[r : SchemaRow] = #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplement(String, String), InterfaceExtends(String, String) | r }

    pub def subtypes(types: #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplement(String, String), InterfaceExtends(String, String) | r }): Map[String, String] = {
        let facts = #{
            IsSubtype(sub, super) :- Class(sub), Class(super), if sub == super.
            IsSubtype(sub, super) :- Interface(sub), Interface(super), if sub == super.
            IsSubtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            IsSubtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
            IsSubtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            IsSubtype(sub, super) :- IsSubtype(sub, a), IsSubtype(a, super).
        };
        let subtypes = query facts, types select (sub, super) from IsSubtype(sub, super);
        subtypes |> Vector.toMap
    }

}
