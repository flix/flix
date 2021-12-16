pub def halfSiblings(siblings: List[(person, person)]): Array[(person, person)] with Boxable[person] =
    let s = project siblings into Sibling;
    let lp = #{
        Sibling(x, y) :- Parent(x, p), Parent(y, p), if x != y.
        HalfSibling(x, y) :- Sibling(x, y), Parent(y, p), not Parent(x, p).
    };
    query s, lp select (x, y) from HalfSibling(x, y)
