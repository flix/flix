namespace Example {

    pub def safeConnects(src: n, links: f[(n, n)]): {safe::List[n], unsafe::List[n]} with Foldable[f], Boxable[n] =
        let db = inject links into Link;
        let pr = #{
            Linked(a, b) :- Link(a, b).
            Linked(a, b) :- Link(b, a).

            Connected(a, b) :- Linked(a, b).
            Connected(a, b) :- Connected(a, c), Linked(c, b).

            Cutpoint(x, a, b) :- Connected(a, b), Station(x), not Circumvent(x, a, b).

            Circumvent(x, a, b) :- Linked(a, b), if x != a, Station(x), if x != b.
            Circumvent(x, a, b) :- Circumvent(x, a, c), Circumvent(x, c, b).

            HasICutPoint(a, b) :- Cutpoint(x, a, b), if x != a, if x != b.
            SafelyConnected(a, b) :- Connected(a, b), not HasICutPoint(a, b).
            UnsafelyConnected(a, b) :- Station(a), Station(b), not SafelyConnected(a, b).
            Station(x) :- Linked(x, _).
        };
        let model = solve db <+> pr;
        let safe = query model select b from SafelyConnected(src, b);
        let unsafe = query model select b from UnsafelyConnected(src, b);
        {safe = safe, unsafe = unsafe}

    def exampleGraph(): Set[(String, String)] =
        let _graphString =
            "             ┌───────┐   ┌───────┐             " ::
            "     ┌───────┤ semel ├───┤  bis  │             " ::
            "     │       └───┬───┘   └───┬───┘             " ::
            "     │           │           │                 " ::
            " ┌───┴───┐   ┌───┴───┐   ┌───┴───┐   ┌───────┐ " ::
            " │ clote ├───┤quincy ├───┤  ter  ├───┤ olfe  │ " ::
            " └───┬───┘   └───┬───┘   └───┬───┘   └───────┘ " ::
            "     │           │           │                 " ::
            "     │       ┌───┴───┐   ┌───┴───┐             " ::
            "     └───────┤ mamuk ├───┤ icsi  │             " ::
            "             └┬─────┬┘   └───────┘             " ::
            "              │     │                          " ::
            "       ┌──────┴┐   ┌┴──────┐                   " ::
            "       │ dalte ├───┤quater │                   " ::
            "       └───────┘   └───────┘                   " ::
            Nil;

        Set#{
            ("semel", "bis"   ),
            ("semel", "quincy"),
            ("semel", "clote" ),
            ("bis"  , "ter"   ),
            ("ter"  , "olfe"  ),
            ("ter"  , "quincy"),
            ("ter"  , "icsi"  ),
            ("icsi" , "mamuk" ),
            ("mamuk", "quincy"),
            ("mamuk", "clote" ),
            ("mamuk", "quater"),
            ("mamuk", "dalte" ),
            ("clote", "quincy"),
            ("dalte", "quater")
        }

    pub def test(): Unit \ IO =
        let rec = safeConnects("bis", exampleGraph());
        rec.safe |> List.sort |> List.map(Console.green) |> List.foreach(println);
        rec.unsafe |> List.sort |> List.map(Console.red) |> List.foreach(println)

}
