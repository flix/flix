pub def halfSiblings(siblings: List[(person, person)]): Vector[(person, person)] with Order[person] =
    let s = inject siblings into Sibling;
    let lp: #{ Sibling(person, person), Parent(person, person), HalfSibling(person, person) } = #{
        Sibling(x, y) :- Parent(x, p), Parent(y, p), if x != y.
        HalfSibling(x, y) :- Sibling(x, y), Parent(y, p), not Parent(x, p).
    };
    query s, lp select (x, y) from HalfSibling(x, y)
