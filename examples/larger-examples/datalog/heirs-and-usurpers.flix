pub def heirsAndUsurpers(parents: List[(person, person)], emperors: List[person]): {heirs = List[person], usurpers = List[person]} with Order[person] =
    let p = inject parents into Parent;
    let e = inject emperors into Emperor;
    let lp = #{
        Ancestor(x, y) :- Parent(x, y), not Emperor(y).
        Ancestor(x, z) :- Ancestor(x, y), Parent(y, z), not Emperor(z).
        HeirOf(x, y) :- Emperor(x), Parent(x, y), Emperor(y).
        HeirOf(x, z) :- Emperor(x), Ancestor(x, y), Parent(y, z), Emperor(z).
        Heir(x) :- HeirOf(x, _).
        Usurper(x) :- Emperor(x), not Heir(x).
    };
    let solution = solve p, e, lp;
    let heirs = query solution select x from Heir(x);
    let usurpers = query solution select x from Usurper(x);
    {heirs = heirs, usurpers = usurpers}
