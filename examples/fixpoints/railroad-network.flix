///
/// Define a function that decides whether pairs of railroad stations in `links` are safely connected.
///
/// Given two railroad stations a and b, if they are only connected through some station c,
/// then they are unsafely connected. Because if c fails there is no way to get from a to b.
///
def safeConnects(src: n, links: f[(n, n)]): {safe = Vector[n], unsafe1 = Vector[n]} \ Foldable.Aef[f] with Foldable[f], Order[n] =
    let db = inject links into Link(_, _);
    let pr = #{
        // Directly linked stations.
        Linked(a, b) :- Link(a, b).
        Linked(a, b) :- Link(b, a).

        // Indirectly linked stations.
        Connected(a, b) :- Linked(a, b).
        Connected(a, b) :- Connected(a, c), Linked(c, b).

        // Station `x` is a cutpoint between `a` and `b`.
        Cutpoint(x, a, b) :- Connected(a, b), Station(x), not Circumvent(x, a, b).

        // Stations `a` and `b` are linked without using station `x`.
        Circumvent(x, a, b) :- Linked(a, b), if (x != a), Station(x), if (x != b).
        Circumvent(x, a, b) :- Circumvent(x, a, c), Circumvent(x, c, b).

        // Check the cutpoint is neither station `a` or `b`.
        HasICutPoint(a, b) :- Cutpoint(x, a, b), if (x != a), if (x != b).
        // Stations are safely connected if there is no cutpoints.
        SafelyConnected(a, b) :- Connected(a, b), not HasICutPoint(a, b).
        // The set of stations not safely connected are unsafely connected.
        UnsafelyConnected(a, b) :- Station(a), Station(b), not SafelyConnected(a, b).
        // All stations.
        Station(src).
        Station(x) :- Linked(x, _).
    };
    let model = solve db <+> pr;
    let safe = query model select b from SafelyConnected(src, b);
    let unsafe1 = query model select b from UnsafelyConnected(src, b);
    {safe = safe, unsafe1 = unsafe1}

///
/// Define some example graph to test the function `safeConnects`.
///
def exampleGraph(): Set[(String, String)] =
    // Visual representation of the graph.
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
    // The graph as a set of links.
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

///
/// Test which of the stations are safely and unsafely connected from `bis`.
///
def test(): Unit \ {Sys, IO} = Environment.handle(() -> {
    let rec = safeConnects("bis", exampleGraph());
    println("Safe:");
    rec#safe |> Vector.sort |> Vector.map(Chalk.green) |> Vector.forEach(println);
    println("Unsafe:");
    rec#unsafe1 |> Vector.sort |> Vector.map(Chalk.red) |> Vector.forEach(println)
})()
