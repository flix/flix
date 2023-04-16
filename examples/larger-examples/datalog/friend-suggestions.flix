pub def friendSuggestions(friends: List[(person, person)]): List[(person, person)] with Order[person] =
    let f = inject friends into Friend;
    let lp = #{
        Suggestion(me, nf) :-
            Friend(me, f1), Friend(me, f2), Friend(me, f3),
            Friend(f1, nf), Friend(f2, nf), Friend(f3, nf),
            not Friend(me, nf),
            if f1 != f2 and f2 != f3 and f1 != f3.
    };
    query f, lp select (x, y) from Suggestion(x, y)
