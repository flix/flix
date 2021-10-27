def travel(tg: #{Train(City, City)}, bg: #{Bus(City, City)}, src: City, dst: City): Option[#{Path(City, City)}] =
    let p = #{
        Path(x, y) :- Train(x, y).
        Path(x, z) :- Path(x, y), Train(y, z).
        Path(x, y) :- Bus(x, y), not Train(x, y).
        Path(x, z) :- Path(x, y), Bus(y, z), not Train(y, z).
    };
    let m = solve (tg <+> bg <+> p);
    if (m |= Path(src, dst).) Some(project Path m) else None
