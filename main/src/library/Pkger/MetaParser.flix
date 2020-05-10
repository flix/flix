namespace MetaParser {
    pub def parse(input: String): Option[List[(String, String)]] =
        input |>
        String.toList |>
        ParseToml.parse |>
        Option.flatMap(
            Map.toList >> List.map(t -> match t {
                case (k, ParseToml/InnerToml.StringVal(s)) => Some((k, s))
                case _ => None
            }) >> List.foldRight(Option.map2((h, tail) -> h :: tail), Some(Nil))
        )
}
