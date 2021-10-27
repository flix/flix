def heirsAndUsurpers(g: #{Parent(Person, Person), Emperor(Person)}): (#{Heir(Person, Person)}, #{Usurper(Person)}) =
    let p = #{
        Ancestor(x, y) :- Parent(x, y), not Emperor(y).
        Ancestor(x, z) :- Ancestor(x, y), Parent(y, z), not Emperor(z).
        Heir(x, y) :- Emperor(x), Parent(x, y), Emperor(y).
        Heir(x, z) :- Emperor(x), Ancestor(x, y), Parent(y, z), Emperor(z).
        Usurper(x) :- Emperor(x), not Heir(x, _).
    };
    let m = solve g <+> p;
    (project Heir m, project Usurper m)
