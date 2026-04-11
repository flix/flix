mod Api {

    @Export
    pub def callUsers(seed: Int32): String \ Wit.HostDemoUsers =
        let promoted = match Wit.HostDemoUsers.promote(Some(("alice", seed))) {
            case Ok((name, score)) => "${name}:${Int32.toString(score)}"
            case Err(code) => "err:${Int32.toString(code)}"
        };

        let pairs = Wit.HostDemoUsers.pairUsers(("alice", 1) :: ("bob", 2) :: Nil);
        let pairText = pairs |> List.map(match (name, score) -> "${name}:${Int32.toString(score)}") |> String.intercalate(",");
        promoted + "|" + pairText

}
