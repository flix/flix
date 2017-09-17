@test
def nested01(): Bool = Option.isNone(None: Option[Option[Int]])

@test
def nested02(): Bool = Option.isSome(Some(None))

@test
def nested03(): Bool = Option.isSome(Some(Some(42)))

@test
def nested04(): Bool = Option.isSome(Some(Some(Some(42))))

@test
def nested05(): Bool =
    let o1 = Some(true);
        Option.getWithDefault(o1, false)

@test
def nested06(): Bool =
    let o1 = Some(Some(true));
    let o2 = Option.getWithDefault(o1, Some(false));
        Option.getWithDefault(o2, false)

@test
def nested07(): Bool =
    let o1 = Some(Some(Some(true)));
    let o2 = Option.getWithDefault(o1, Some(Some(false)));
    let o3 = Option.getWithDefault(o2, Some(false));
        Option.getWithDefault(o3, false)

@test
def nested08(): Bool =
    let o1 = Some(Some(Some(true)));
    let o2 = Option.flatten(o1);
    let o3 = Option.flatten(o2);
        Option.getWithDefault(o3, false)
