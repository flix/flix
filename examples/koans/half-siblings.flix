def halfSiblings(g: #{Parent(Person, Person), Male(Person), Female(Person)}): #{HalfBro(Person, Person), HalfSis(Person, Person)} =
    let p = #{
        Sibling(c, s) :- Parent(c, p), Parent(s, p), if c != s.
        HalfBro(c, s) :- Sibling(c, s), Male(s),
                         Parent(s, p), not Parent(c, p).
        HalfSis(c, s) :- Sibling(c, s), Female(s),
                         Parent(s, p), not Parent(c, p).
    };
    let m = solve (p <+> g);
    (project HalfBro m) <+> (project HalfSis m)
