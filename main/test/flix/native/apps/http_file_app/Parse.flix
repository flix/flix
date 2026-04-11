mod NativeHttpFileParse {

    ///
    /// Extracts the value of a `token=...` line from the response body.
    ///
    pub def extractToken(body: String): Result[String, String] =
        match findToken(String.lines(body)) {
            case Some(t) => Ok(t)
            case None => Err("missing token=")
        }

    def findToken(lines: List[String]): Option[String] = match lines {
        case Nil => None
        case l :: ls =>
            if (String.startsWith({prefix = "token="}, l))
                match String.splitOn({substr = "="}, l) {
                    case _ :: v :: Nil => Some(v)
                    case _ => None
                }
            else
                findToken(ls)
    }

}
