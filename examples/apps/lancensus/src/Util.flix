mod Util {
    use Json.FromJson
    use Json.JsonElement
    use Json.Parse
    use Json.Path.Path
    
    pub type alias Stat = Map[String, (BigInt, BigInt)]

    pub type alias Stats = Map[String, Stat]

    pub def fromJson(json: JsonElement): a \ AnalysisFailure with Json.FromJson[a] =
        match FromJson.fromJson(json) {
            case Result.Ok(res)  => res
            case Result.Err(err) => AnalysisFailure.raiseJsonError(err)
        }
        
    def getKey(key: String, map: Map[String, JsonElement]): a \ AnalysisFailure with Json.FromJson[a] = 
        match Json.getAtKey(Path.Root, key, map) {
            case Ok(res) => res
            case Err(err) => AnalysisFailure.raiseJsonError(err)
        }
    
    pub def accessJson(key: String, json: JsonElement): a \ AnalysisFailure with Json.FromJson[a] = fromJson(json) |> getKey(key)

    def parse(jsonString: String): JsonElement \ AnalysisFailure = 
        match Parse.parse(jsonString) {
            case None => AnalysisFailure.raiseParseError()
            case Some(json) => json
        }

    pub def getAndParse(url: String, header: Map[String, List[String]]): JsonElement \ HttpWithResult + AnalysisFailure = 
        match HttpWithResult.get(url, header) {
            case Result.Ok(response)  => response |> Http.Response.body |> parse
            case Result.Err(err) => AnalysisFailure.raiseRequestFailure(err)
        }
        
    pub def getAllPages(url: String, headers: Map[String, List[String]]): List[JsonElement] \ HttpWithResult  + AnalysisFailure = {
        match HttpWithResult.get(url, headers) {
            case Result.Ok(response)  => 
                if (Http.Response.status(response) != 200)
                    Nil
                else 
                    let thisPage: List[JsonElement] = response |> Http.Response.body |> parse |> fromJson;
                    let nextPagesOpt = forM (
                        linkValueList <- Http.Response.headers(response) |> Map.get("link");
                        linkString <- List.head(linkValueList);
                        // http <- Regex.(regex"<([^>]*)>; rel=\"next\"", linkString)
                        links = String.split({regex = ","}, linkString);
                        next_link <- List.find(lk -> String.contains({substr = "rel=\"next\""}, lk), links);
                        httpWithoutPrefix <- String.stripPrefix({substr = "<"}, next_link);
                        http <- String.stripSuffix({substr = ">; rel=\"next\""}, httpWithoutPrefix)
                    ) yield getAllPages(http, headers);
                    let nextPages = Option.getWithDefault(Nil, nextPagesOpt);
                    thisPage ::: nextPages
            case Result.Err(err) => AnalysisFailure.raiseRequestFailure(err)
        }
    }
    
    pub def extractExt(filePath: String): Option[String] = {
        let generatedPaths = regex".*(/?dist/|/?build/|/?_build/|/?out/|/?target/).*" :: regex".*(package-lock\\.json|yarn\\.lock)$" :: regex".*/?node_modules/.*" :: regex".*(ssl\\.crt|\\.env|\\.config\\.js)$" :: Nil;
        if (List.exists(generatedPath -> {
            Regex.isSubmatch(generatedPath, filePath)
        }, generatedPaths)) None else {
            let lastPart = String.takeWhileRight(c -> c != '/', filePath);
            match String.findIndexOfRight(c -> c == '.', lastPart) {
                case Option.Some(idx) if (idx != 0 and idx != String.length(lastPart) - 1) => 
                    Some(String.drop(idx + 1, lastPart))
                case _ => None
            }
        }
    }
}
