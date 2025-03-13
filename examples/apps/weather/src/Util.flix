use Json.JsonElement
use Json.Parse

instance ToString[JsonElement] {

    pub def toString(json: JsonElement): String = match json {
        case JsonElement.JsonObject(jobj) => 
            let map : List[String] = foreach (
                (k, v) <- jobj
            ) yield "${k} : ${ToString.toString(v)}, ";
            let content = List.reduceLeft(String.concat, map) |> Option.getWithDefault("");
            "{${content}}"
        case JsonElement.JsonArray(jarray)  => 
            let array = List.map(e -> ToString.toString(e), jarray);
            let content = List.reduceLeft(String.concat, array) |> Option.getWithDefault("");
            "[${content}]"
        case JsonElement.JsonString(jString) => jString
        case JsonElement.JsonNumber(jNumber) => ToString.toString(jNumber)
        case JsonElement.JsonBool(jBool)   => ToString.toString(jBool)
        case JsonElement.JsonNull          => "Null"
    }
}

def requestAndParse(url: String, header: Map[String, List[String]]): Option[JsonElement] \ HttpWithResult = 
    forM(
        response <- HttpWithResult.get(url, header) |> Result.toOption ;
        body <- Parse.parse(Http.Response.body(response))
    ) yield body

def getFloat64FromJObject(key: String, json: JsonElement): Option[Float64] = match json {
    case JsonElement.JsonObject(m) => match Map.get(key, m) {
        case Option.Some(JsonElement.JsonNumber(n)) => BigDecimal.tryToFloat64(n)
        case _ => None
    }
    case _ => None
}

def getStringFromJObject(key: String, json: JsonElement): Option[String] = match json {
    case JsonElement.JsonObject(m) => match Map.get(key, m) {
        case Some(JsonElement.JsonString(s)) => Some(s)
        case _ => None
    }
    case _ => None
}

def getFromJObject(key: String, json: JsonElement): Option[JsonElement] = match json {
    case JsonElement.JsonObject(outMap) => match Map.get(key, outMap) {
        case Some(m) => Some(m)
        case _ => None
    }
    case _ => None
}
