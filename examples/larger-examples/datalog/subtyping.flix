mod Subtyping {

    pub def subtypes(types: #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String) }): Map[String, String] = {
        let rules = #{
            IsSubtype(sub, super) :- Class(sub), Class(super), if sub == super.
            IsSubtype(sub, super) :- Interface(sub), Interface(super), if sub == super.
            IsSubtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            IsSubtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
            IsSubtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            IsSubtype(sub, super) :- IsSubtype(sub, a), IsSubtype(a, super).
        };
        let subtypes = query rules, types select (sub, super) from IsSubtype(sub, super);
        subtypes |> Vector.toMap
    }

}
