def friendSuggestions(g: #{Friend(Person, Person)}): #{Suggestion(Person, Person)} =
    let p = #{
        Suggestion(me, nf) :-
            Friend(me, f1), Friend(me, f2), Friend(me, f3),
            Friend(f1, nf), Friend(f2, nf), Friend(f3, nf),
            not Friend(me, nf),
            if f1 != f2 and f2 != f3 and f1 != f3.
    };
    project Suggestion (solve (g <+> p))
