/*
 * Copyright 2026 Flix Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestJson {

    use Assert.{assertEq, assertErr};
    use Util.Json
    use Util.Json.Json.{JArray, JBool, JNull, JNumber, JObject, JString}
    use Util.Json.JsonError.JsonError
    use Util.Json.JsonError.JsonError.{Decode, Parse}
    use Util.Json.JsonError.ParseErrorKind.{DuplicateKey, InvalidEscape, InvalidUnicode, UnexpectedChar, UnexpectedEof}
    use Util.Json.JsonError.DecodeErrorKind.{MissingIndex, MissingKey, TypeMismatch}
    use Util.Json.JsonPath.JsonPath
    use Util.Json.JsonPath.JsonStep
    use Util.Json.JsonPath.JsonStep.{Index, Key}
    use Util.Json.ToJson.ToJson
    use Util.Json.FromJson.FromJson
    use Util.Json.FromJson.{getAtKey, getAtKeyOpt, getAtPath, decodeAtKey, decodeAtIndex, decodeAtKeyOpt, decodeAtPath, decodeAtPathOpt}

    def num(n: Int32): Json.Json = JNumber(Int32.toBigDecimal(n))

    /////////////////////////////////////////////////////////////////////////////
    // toCompactString — primitives                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toCompactString01(): Unit \ Assert =
        assertEq(expected = "null", Json.toCompactString(JNull))

    @Test
    def toCompactString02(): Unit \ Assert =
        assertEq(expected = "true", Json.toCompactString(JBool(true)))

    @Test
    def toCompactString03(): Unit \ Assert =
        assertEq(expected = "false", Json.toCompactString(JBool(false)))

    @Test
    def toCompactString04(): Unit \ Assert =
        assertEq(expected = "0", Json.toCompactString(num(0)))

    @Test
    def toCompactString05(): Unit \ Assert =
        assertEq(expected = "42", Json.toCompactString(num(42)))

    @Test
    def toCompactString06(): Unit \ Assert =
        assertEq(expected = "-7", Json.toCompactString(num(-7)))

    @Test
    def toCompactString07(): Unit \ Assert =
        assertEq(expected = "3.14", Json.toCompactString(JNumber(3.14ff)))

    /////////////////////////////////////////////////////////////////////////////
    // toCompactString — strings (no escape needed)                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toCompactString08(): Unit \ Assert =
        assertEq(expected = "\"\"", Json.toCompactString(JString("")))

    @Test
    def toCompactString09(): Unit \ Assert =
        assertEq(expected = "\"hello\"", Json.toCompactString(JString("hello")))

    @Test
    def toCompactString10(): Unit \ Assert =
        assertEq(expected = "\" \"", Json.toCompactString(JString(" ")))

    @Test
    def toCompactString11(): Unit \ Assert =
        assertEq(expected = "\"/\"", Json.toCompactString(JString("/")))

    @Test
    def toCompactString12(): Unit \ Assert =
        assertEq(
            expected = "\"http://example.com/path\"",
            Json.toCompactString(JString("http://example.com/path"))
        )

    @Test
    def toCompactString13(): Unit \ Assert =
        assertEq(expected = "\"café\"", Json.toCompactString(JString("café")))

    /////////////////////////////////////////////////////////////////////////////
    // toCompactString — strings with escapes                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toCompactString14(): Unit \ Assert =
        assertEq(expected = "\"\\\\\"", Json.toCompactString(JString("\\")))

    @Test
    def toCompactString15(): Unit \ Assert =
        assertEq(expected = "\"\\\"\"", Json.toCompactString(JString("\"")))

    @Test
    def toCompactString16(): Unit \ Assert =
        assertEq(expected = "\"\\b\"", Json.toCompactString(JString("\u0008")))

    @Test
    def toCompactString17(): Unit \ Assert =
        assertEq(expected = "\"\\t\"", Json.toCompactString(JString("\t")))

    @Test
    def toCompactString18(): Unit \ Assert =
        assertEq(expected = "\"\\n\"", Json.toCompactString(JString("\n")))

    @Test
    def toCompactString19(): Unit \ Assert =
        assertEq(expected = "\"\\f\"", Json.toCompactString(JString("\u000C")))

    @Test
    def toCompactString20(): Unit \ Assert =
        assertEq(expected = "\"\\r\"", Json.toCompactString(JString("\r")))

    @Test
    def toCompactString21(): Unit \ Assert =
        assertEq(expected = "\"\\u0000\"", Json.toCompactString(JString("\u0000")))

    @Test
    def toCompactString22(): Unit \ Assert =
        assertEq(expected = "\"\\u0001\"", Json.toCompactString(JString("\u0001")))

    @Test
    def toCompactString23(): Unit \ Assert =
        assertEq(expected = "\"\\u000b\"", Json.toCompactString(JString("\u000B")))

    @Test
    def toCompactString24(): Unit \ Assert =
        assertEq(expected = "\"\\u001f\"", Json.toCompactString(JString("\u001F")))

    @Test
    def toCompactString25(): Unit \ Assert =
        assertEq(expected = "\"a\\nb\\tc\"", Json.toCompactString(JString("a\nb\tc")))

    // High-BMP and non-BMP characters (code point >= 0x20) must pass through
    // unescaped, per RFC 8259. These exercise the >= U+8000 range, where a
    // signed reinterpretation of the UTF-16 code unit would go negative.

    @Test
    def toCompactStringHigh1(): Unit \ Assert =
        assertEq(expected = "\"耀\"", Json.toCompactString(JString("耀")))

    @Test
    def toCompactStringHigh2(): Unit \ Assert =
        assertEq(expected = "\"한\"", Json.toCompactString(JString("한")))

    @Test
    def toCompactStringHigh3(): Unit \ Assert =
        assertEq(expected = "\"￿\"", Json.toCompactString(JString("￿")))

    // Non-BMP character (emoji U+1F600) encoded as a surrogate pair.
    @Test
    def toCompactStringHigh4(): Unit \ Assert =
        assertEq(expected = "\"😀\"", Json.toCompactString(JString("😀")))

    // Round-trip: a high-BMP character survives encode then decode unchanged.
    @Test
    def toCompactStringHigh5(): Unit \ Assert =
        assertEq(
            expected = (Ok(JString("耀")): Result[JsonError, Json.Json]),
            Json.parse(Json.toCompactString(JString("耀"))))

    /////////////////////////////////////////////////////////////////////////////
    // toCompactString — arrays                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toCompactString26(): Unit \ Assert =
        assertEq(expected = "[]", Json.toCompactString(JArray(Vector#{})))

    @Test
    def toCompactString27(): Unit \ Assert =
        assertEq(expected = "[1]", Json.toCompactString(JArray(Vector#{num(1)})))

    @Test
    def toCompactString28(): Unit \ Assert =
        assertEq(
            expected = "[1,2,3]",
            Json.toCompactString(JArray(Vector#{num(1), num(2), num(3)}))
        )

    @Test
    def toCompactString29(): Unit \ Assert =
        assertEq(
            expected = "[[1],[2]]",
            Json.toCompactString(JArray(Vector#{JArray(Vector#{num(1)}), JArray(Vector#{num(2)})}))
        )

    @Test
    def toCompactString30(): Unit \ Assert =
        assertEq(
            expected = "[null,true,1,\"x\"]",
            Json.toCompactString(JArray(Vector#{JNull, JBool(true), num(1), JString("x")}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // toCompactString — objects                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toCompactString31(): Unit \ Assert =
        assertEq(expected = "{}", Json.toCompactString(JObject(Map#{})))

    @Test
    def toCompactString32(): Unit \ Assert =
        assertEq(
            expected = "{\"a\":1}",
            Json.toCompactString(JObject(Map#{"a" => num(1)}))
        )

    @Test
    def toCompactString33(): Unit \ Assert =
        assertEq(
            expected = "{\"a\":1,\"b\":2,\"c\":3}",
            Json.toCompactString(JObject(Map#{"a" => num(1), "b" => num(2), "c" => num(3)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // toPrettyString — primitives (no layout effect)                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toPrettyString01(): Unit \ Assert =
        assertEq(expected = "null", Json.toPrettyString(2, JNull))

    @Test
    def toPrettyString02(): Unit \ Assert =
        assertEq(expected = "true", Json.toPrettyString(2, JBool(true)))

    @Test
    def toPrettyString03(): Unit \ Assert =
        assertEq(expected = "42", Json.toPrettyString(2, num(42)))

    @Test
    def toPrettyString04(): Unit \ Assert =
        assertEq(expected = "\"hello\"", Json.toPrettyString(2, JString("hello")))

    /////////////////////////////////////////////////////////////////////////////
    // toPrettyString — empty containers stay on one line                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toPrettyString05(): Unit \ Assert =
        assertEq(expected = "[]", Json.toPrettyString(2, JArray(Vector#{})))

    @Test
    def toPrettyString06(): Unit \ Assert =
        assertEq(expected = "{}", Json.toPrettyString(2, JObject(Map#{})))

    /////////////////////////////////////////////////////////////////////////////
    // toPrettyString — arrays                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toPrettyString07(): Unit \ Assert =
        assertEq(
            expected = "[\n  1\n]",
            Json.toPrettyString(2, JArray(Vector#{num(1)}))
        )

    @Test
    def toPrettyString08(): Unit \ Assert =
        assertEq(
            expected = "[\n  1,\n  2,\n  3\n]",
            Json.toPrettyString(2, JArray(Vector#{num(1), num(2), num(3)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // toPrettyString — objects                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toPrettyString09(): Unit \ Assert =
        assertEq(
            expected = "{\n  \"a\": 1\n}",
            Json.toPrettyString(2, JObject(Map#{"a" => num(1)}))
        )

    @Test
    def toPrettyString10(): Unit \ Assert =
        assertEq(
            expected = "{\n  \"a\": 1,\n  \"b\": 2\n}",
            Json.toPrettyString(2, JObject(Map#{"a" => num(1), "b" => num(2)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // toPrettyString — nesting                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toPrettyString11(): Unit \ Assert =
        assertEq(
            expected = "[\n  [\n    1\n  ],\n  [\n    2\n  ]\n]",
            Json.toPrettyString(2, JArray(Vector#{JArray(Vector#{num(1)}), JArray(Vector#{num(2)})}))
        )

    @Test
    def toPrettyString12(): Unit \ Assert =
        assertEq(
            expected =
                "{\n" +
                "  \"users\": [\n" +
                "    {\n" +
                "      \"name\": \"alice\"\n" +
                "    },\n" +
                "    {\n" +
                "      \"name\": \"bob\"\n" +
                "    }\n" +
                "  ]\n" +
                "}",
            Json.toPrettyString(2, JObject(Map#{
                "users" => JArray(Vector#{
                    JObject(Map#{"name" => JString("alice")}),
                    JObject(Map#{"name" => JString("bob")})
                })
            }))
        )

    /////////////////////////////////////////////////////////////////////////////
    // toPrettyString — different indent                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toPrettyString13(): Unit \ Assert =
        assertEq(
            expected = "[\n    1,\n    2\n]",
            Json.toPrettyString(4, JArray(Vector#{num(1), num(2)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // toRichString — primitives are coloured                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toRichString01(): Unit \ Assert =
        assertEq(expected = RichString.gray("null"), Json.toRichString(2, JNull))

    @Test
    def toRichString02(): Unit \ Assert =
        assertEq(expected = RichString.magenta("true"), Json.toRichString(2, JBool(true)))

    @Test
    def toRichString03(): Unit \ Assert =
        assertEq(expected = RichString.magenta("false"), Json.toRichString(2, JBool(false)))

    @Test
    def toRichString04(): Unit \ Assert =
        assertEq(expected = RichString.yellow("42"), Json.toRichString(2, num(42)))

    @Test
    def toRichString05(): Unit \ Assert =
        assertEq(expected = RichString.green("\"hi\""), Json.toRichString(2, JString("hi")))

    /////////////////////////////////////////////////////////////////////////////
    // toRichString — layout matches toPrettyString                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toRichString06(): Unit \ Assert =
        let j = JArray(Vector#{});
        assertEq(expected = Json.toPrettyString(2, j), RichString.toPlainString(Json.toRichString(2, j)))

    @Test
    def toRichString07(): Unit \ Assert =
        let j = JObject(Map#{});
        assertEq(expected = Json.toPrettyString(2, j), RichString.toPlainString(Json.toRichString(2, j)))

    @Test
    def toRichString08(): Unit \ Assert =
        let j = JArray(Vector#{num(1)});
        assertEq(expected = Json.toPrettyString(2, j), RichString.toPlainString(Json.toRichString(2, j)))

    @Test
    def toRichString09(): Unit \ Assert =
        let j = JArray(Vector#{num(1), num(2), num(3)});
        assertEq(expected = Json.toPrettyString(2, j), RichString.toPlainString(Json.toRichString(2, j)))

    @Test
    def toRichString10(): Unit \ Assert =
        let j = JObject(Map#{"a" => num(1), "b" => num(2)});
        assertEq(expected = Json.toPrettyString(2, j), RichString.toPlainString(Json.toRichString(2, j)))

    @Test
    def toRichString11(): Unit \ Assert =
        let j = JObject(Map#{
            "users" => JArray(Vector#{
                JObject(Map#{"name" => JString("alice")}),
                JObject(Map#{"name" => JString("bob")})
            })
        });
        assertEq(expected = Json.toPrettyString(2, j), RichString.toPlainString(Json.toRichString(2, j)))

    /////////////////////////////////////////////////////////////////////////////
    // toRichString — full colour spot-check on a single-key object            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toRichString12(): Unit \ Assert =
        let expected =
            RichString.fromString("{\n  ") +
            RichString.cyan("\"a\"") +
            RichString.fromString(": ") +
            RichString.yellow("1") +
            RichString.fromString("\n}");
        assertEq(expected = expected, Json.toRichString(2, JObject(Map#{"a" => num(1)})))

    /////////////////////////////////////////////////////////////////////////////
    // parse — primitives                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parse01(): Unit \ Assert =
        assertEq(expected = Ok(JNull), Json.parse("null"))

    @Test
    def parse02(): Unit \ Assert =
        assertEq(expected = Ok(JBool(true)), Json.parse("true"))

    @Test
    def parse03(): Unit \ Assert =
        assertEq(expected = Ok(JBool(false)), Json.parse("false"))

    @Test
    def parse04(): Unit \ Assert =
        assertEq(expected = Ok(num(0)), Json.parse("0"))

    @Test
    def parse05(): Unit \ Assert =
        assertEq(expected = Ok(num(42)), Json.parse("42"))

    @Test
    def parse06(): Unit \ Assert =
        assertEq(expected = Ok(num(-7)), Json.parse("-7"))

    @Test
    def parse07(): Unit \ Assert =
        assertEq(expected = Ok(JNumber(3.14ff)), Json.parse("3.14"))

    /////////////////////////////////////////////////////////////////////////////
    // parse — strings                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parse08(): Unit \ Assert =
        assertEq(expected = Ok(JString("")), Json.parse("\"\""))

    @Test
    def parse09(): Unit \ Assert =
        assertEq(expected = Ok(JString("hello")), Json.parse("\"hello\""))

    @Test
    def parse10(): Unit \ Assert =
        assertEq(expected = Ok(JString("\\")), Json.parse("\"\\\\\""))

    @Test
    def parse11(): Unit \ Assert =
        assertEq(expected = Ok(JString("\"")), Json.parse("\"\\\"\""))

    @Test
    def parse12(): Unit \ Assert =
        assertEq(expected = Ok(JString("\n")), Json.parse("\"\\n\""))

    @Test
    def parse13(): Unit \ Assert =
        assertEq(expected = Ok(JString("\t")), Json.parse("\"\\t\""))

    @Test
    def parse14(): Unit \ Assert =
        assertEq(expected = Ok(JString("\u0008")), Json.parse("\"\\b\""))

    @Test
    def parse15(): Unit \ Assert =
        assertEq(expected = Ok(JString("\u000C")), Json.parse("\"\\f\""))

    @Test
    def parse16(): Unit \ Assert =
        assertEq(expected = Ok(JString("\r")), Json.parse("\"\\r\""))

    @Test
    def parse17(): Unit \ Assert =
        assertEq(expected = Ok(JString("\u0001")), Json.parse("\"\\u0001\""))

    @Test
    def parse18(): Unit \ Assert =
        assertEq(expected = Ok(JString("café")), Json.parse("\"café\""))

    // A `\uXXXX` escape must be exactly four hex digits. A sign or whitespace
    // in the four-character window must be rejected, not parsed by Int32.parse
    // into an out-of-range code point (which previously crashed appendCodePoint).

    @Test
    def parseUnicodeNeg(): Unit \ Assert =
        assertEq(
            expected = (Err(Parse(3, InvalidUnicode("-100"))): Result[JsonError, Json.Json]),
            Json.parse("\"\\u-100\""))

    @Test
    def parseUnicodePlus(): Unit \ Assert =
        assertEq(
            expected = (Err(Parse(3, InvalidUnicode("+041"))): Result[JsonError, Json.Json]),
            Json.parse("\"\\u+041\""))

    @Test
    def parseUnicodeSpace(): Unit \ Assert =
        assertEq(
            expected = (Err(Parse(3, InvalidUnicode(" 041"))): Result[JsonError, Json.Json]),
            Json.parse("\"\\u 041\""))

    @Test
    def parseUnicodeValid(): Unit \ Assert =
        assertEq(expected = Ok(JString("A")), Json.parse("\"\\u0041\""))

    /////////////////////////////////////////////////////////////////////////////
    // parse — arrays                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parse19(): Unit \ Assert =
        assertEq(expected = Ok(JArray(Vector#{})), Json.parse("[]"))

    @Test
    def parse20(): Unit \ Assert =
        assertEq(expected = Ok(JArray(Vector#{num(1)})), Json.parse("[1]"))

    @Test
    def parse21(): Unit \ Assert =
        assertEq(
            expected = Ok(JArray(Vector#{num(1), num(2), num(3)})),
            Json.parse("[1,2,3]")
        )

    @Test
    def parse22(): Unit \ Assert =
        assertEq(
            expected = Ok(JArray(Vector#{JArray(Vector#{num(1)}), JArray(Vector#{num(2)})})),
            Json.parse("[[1],[2]]")
        )

    @Test
    def parse23(): Unit \ Assert =
        assertEq(
            expected = Ok(JArray(Vector#{JNull, JBool(true), num(1), JString("x")})),
            Json.parse("[null,true,1,\"x\"]")
        )

    /////////////////////////////////////////////////////////////////////////////
    // parse — objects                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parse24(): Unit \ Assert =
        assertEq(expected = Ok(JObject(Map#{})), Json.parse("{}"))

    @Test
    def parse25(): Unit \ Assert =
        assertEq(
            expected = Ok(JObject(Map#{"a" => num(1)})),
            Json.parse("{\"a\":1}")
        )

    @Test
    def parse26(): Unit \ Assert =
        assertEq(
            expected = Ok(JObject(Map#{"a" => num(1), "b" => num(2)})),
            Json.parse("{\"a\":1,\"b\":2}")
        )

    @Test
    def parse27(): Unit \ Assert =
        assertEq(
            expected = Ok(JObject(Map#{
                "users" => JArray(Vector#{
                    JObject(Map#{"name" => JString("alice")}),
                    JObject(Map#{"name" => JString("bob")})
                })
            })),
            Json.parse("{\"users\":[{\"name\":\"alice\"},{\"name\":\"bob\"}]}")
        )

    /////////////////////////////////////////////////////////////////////////////
    // parse — whitespace                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parse28(): Unit \ Assert =
        assertEq(expected = Ok(JNull), Json.parse("  null  "))

    @Test
    def parse29(): Unit \ Assert =
        assertEq(
            expected = Ok(JArray(Vector#{num(1), num(2)})),
            Json.parse("[ 1 , 2 ]")
        )

    @Test
    def parse30(): Unit \ Assert =
        assertEq(
            expected = Ok(JObject(Map#{"a" => num(1)})),
            Json.parse("{ \"a\" : 1 }")
        )

    /////////////////////////////////////////////////////////////////////////////
    // parse — negative cases                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parse31(): Unit \ Assert =
        assertErr(Json.parse(""))

    @Test
    def parse32(): Unit \ Assert =
        assertErr(Json.parse("null x"))

    @Test
    def parse33(): Unit \ Assert =
        assertErr(Json.parse("["))

    @Test
    def parse34(): Unit \ Assert =
        assertErr(Json.parse("{"))

    @Test
    def parse35(): Unit \ Assert =
        assertErr(Json.parse("{1:2}"))

    @Test
    def parse36(): Unit \ Assert =
        assertErr(Json.parse("{\"a\":1,\"a\":2}"))

    @Test
    def parse37(): Unit \ Assert =
        assertErr(Json.parse("\"\\q\""))

    @Test
    def parse38(): Unit \ Assert =
        assertErr(Json.parse("\"abc"))

    @Test
    def parse39(): Unit \ Assert =
        assertErr(Json.parse("[1,"))

    /////////////////////////////////////////////////////////////////////////////
    // parse — complex / deeply-nested                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parse40(): Unit \ Assert =
        // 4-level deep object
        assertEq(
            expected = Ok(JObject(Map#{
                "a" => JObject(Map#{
                    "b" => JObject(Map#{
                        "c" => JObject(Map#{
                            "d" => num(1)
                        })
                    })
                })
            })),
            Json.parse("{\"a\":{\"b\":{\"c\":{\"d\":1}}}}")
        )

    @Test
    def parse41(): Unit \ Assert =
        // Object with array of objects
        assertEq(
            expected = Ok(JObject(Map#{
                "items" => JArray(Vector#{
                    JObject(Map#{"id" => num(1), "name" => JString("a")}),
                    JObject(Map#{"id" => num(2), "name" => JString("b")})
                })
            })),
            Json.parse("{\"items\":[{\"id\":1,\"name\":\"a\"},{\"id\":2,\"name\":\"b\"}]}")
        )

    @Test
    def parse42(): Unit \ Assert =
        // Arrays nested 3 deep
        assertEq(
            expected = Ok(JArray(Vector#{
                JArray(Vector#{
                    JArray(Vector#{num(1), num(2)}),
                    JArray(Vector#{num(3), num(4)})
                }),
                JArray(Vector#{
                    JArray(Vector#{num(5), num(6)})
                })
            })),
            Json.parse("[[[1,2],[3,4]],[[5,6]]]")
        )

    @Test
    def parse43(): Unit \ Assert =
        // Object mixing all primitive value types
        assertEq(
            expected = Ok(JObject(Map#{
                "s" => JString("x"),
                "n" => num(1),
                "f" => JNumber(1.5ff),
                "b" => JBool(true),
                "z" => JNull
            })),
            Json.parse("{\"s\":\"x\",\"n\":1,\"f\":1.5,\"b\":true,\"z\":null}")
        )

    @Test
    def parse44(): Unit \ Assert =
        // Object with array and object as values
        assertEq(
            expected = Ok(JObject(Map#{
                "tags" => JArray(Vector#{JString("a"), JString("b")}),
                "meta" => JObject(Map#{"k" => JString("v")})
            })),
            Json.parse("{\"tags\":[\"a\",\"b\"],\"meta\":{\"k\":\"v\"}}")
        )

    @Test
    def parse45(): Unit \ Assert =
        // Object with empty containers as values
        assertEq(
            expected = Ok(JObject(Map#{
                "arr" => JArray(Vector#{}),
                "obj" => JObject(Map#{}),
                "val" => num(42)
            })),
            Json.parse("{\"arr\":[],\"obj\":{},\"val\":42}")
        )

    @Test
    def parse46(): Unit \ Assert =
        // Whitespace at every position in a nested structure
        assertEq(
            expected = Ok(JObject(Map#{
                "outer" => JObject(Map#{
                    "inner" => JArray(Vector#{num(1), num(2)})
                })
            })),
            Json.parse("{ \"outer\" : { \"inner\" : [ 1 , 2 ] } }")
        )

    @Test
    def parse47(): Unit \ Assert =
        // Array with strings containing escapes
        assertEq(
            expected = Ok(JArray(Vector#{
                JString("plain"),
                JString("with\nnewline"),
                JString("with\\backslash")
            })),
            Json.parse("[\"plain\",\"with\\nnewline\",\"with\\\\backslash\"]")
        )

    @Test
    def parse48(): Unit \ Assert =
        // Realistic API-shaped payload
        assertEq(
            expected = Ok(JObject(Map#{
                "user" => JObject(Map#{
                    "id" => num(42),
                    "name" => JString("alice"),
                    "emails" => JArray(Vector#{JString("a@x.com"), JString("b@y.com")})
                })
            })),
            Json.parse("{\"user\":{\"id\":42,\"name\":\"alice\",\"emails\":[\"a@x.com\",\"b@y.com\"]}}")
        )

    @Test
    def parse49(): Unit \ Assert =
        // Heterogeneous array with empty object at the end
        assertEq(
            expected = Ok(JArray(Vector#{
                JObject(Map#{"a" => num(1)}),
                JObject(Map#{"b" => JArray(Vector#{JBool(true), JNull})}),
                JObject(Map#{})
            })),
            Json.parse("[{\"a\":1},{\"b\":[true,null]},{}]")
        )

    /////////////////////////////////////////////////////////////////////////////
    // ToJson — primitives                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toJson01(): Unit \ Assert =
        assertEq(expected = JNull, ToJson.toJson(()))

    @Test
    def toJson02(): Unit \ Assert =
        assertEq(expected = JBool(true), ToJson.toJson(true))

    @Test
    def toJson03(): Unit \ Assert =
        assertEq(expected = JBool(false), ToJson.toJson(false))

    @Test
    def toJson05(): Unit \ Assert =
        assertEq(expected = JString(""), ToJson.toJson(""))

    @Test
    def toJson06(): Unit \ Assert =
        assertEq(expected = JString("hello"), ToJson.toJson("hello"))

    /////////////////////////////////////////////////////////////////////////////
    // ToJson — integers                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toJson07(): Unit \ Assert =
        assertEq(expected = num(42), ToJson.toJson(42i8))

    @Test
    def toJson08(): Unit \ Assert =
        assertEq(expected = num(-7), ToJson.toJson(-7i16))

    @Test
    def toJson09(): Unit \ Assert =
        assertEq(expected = num(0), ToJson.toJson(0i32))

    @Test
    def toJson10(): Unit \ Assert =
        assertEq(expected = num(123456789), ToJson.toJson(123456789i64))

    @Test
    def toJson11(): Unit \ Assert =
        assertEq(expected = JNumber(BigInt.toBigDecimal(100ii)), ToJson.toJson(100ii))

    @Test
    def toJson12(): Unit \ Assert =
        assertEq(expected = JNumber(3.14ff), ToJson.toJson(3.14ff))

    /////////////////////////////////////////////////////////////////////////////
    // ToJson — floats                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toJson13(): Unit \ Assert =
        // Float64 finite values become JNumber via tryToBigDecimal
        match ToJson.toJson(2.5f64) {
            case JNumber(_) => assertEq(expected = true, true)
            case _          => assertEq(expected = true, false)
        }

    @Test
    def toJson14(): Unit \ Assert =
        // NaN -> JNull (RFC 8259 forbids NaN)
        assertEq(expected = JNull, ToJson.toJson(Float64.nan()))

    @Test
    def toJson15(): Unit \ Assert =
        // +Infinity -> JNull
        assertEq(expected = JNull, ToJson.toJson(Float64.positiveInfinity()))

    @Test
    def toJson16(): Unit \ Assert =
        // -Infinity -> JNull
        assertEq(expected = JNull, ToJson.toJson(Float64.negativeInfinity()))

    @Test
    def toJson17(): Unit \ Assert =
        match ToJson.toJson(1.5f32) {
            case JNumber(_) => assertEq(expected = true, true)
            case _          => assertEq(expected = true, false)
        }

    @Test
    def toJson18(): Unit \ Assert =
        assertEq(expected = JNull, ToJson.toJson(Float32.nan()))

    /////////////////////////////////////////////////////////////////////////////
    // ToJson — Json identity                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toJson19(): Unit \ Assert =
        assertEq(expected = num(7), ToJson.toJson(num(7)))

    @Test
    def toJson20(): Unit \ Assert =
        let j = JObject(Map#{"k" => JArray(Vector#{JBool(true)})});
        assertEq(expected = j, ToJson.toJson(j))

    /////////////////////////////////////////////////////////////////////////////
    // ToJson — Option                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toJson21(): Unit \ Assert =
        assertEq(expected = JNull, ToJson.toJson((None: Option[Int32])))

    @Test
    def toJson22(): Unit \ Assert =
        assertEq(expected = num(42), ToJson.toJson(Some(42)))

    @Test
    def toJson23(): Unit \ Assert =
        assertEq(expected = JString("hi"), ToJson.toJson(Some("hi")))

    /////////////////////////////////////////////////////////////////////////////
    // ToJson — collections                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toJson24(): Unit \ Assert =
        assertEq(expected = JArray(Vector#{}), ToJson.toJson((Vector#{}: Vector[Int32])))

    @Test
    def toJson25(): Unit \ Assert =
        assertEq(
            expected = JArray(Vector#{num(1), num(2), num(3)}),
            ToJson.toJson(Vector#{1, 2, 3})
        )

    @Test
    def toJson26(): Unit \ Assert =
        assertEq(expected = JArray(Vector#{}), ToJson.toJson((Nil: List[Int32])))

    @Test
    def toJson27(): Unit \ Assert =
        assertEq(
            expected = JArray(Vector#{JString("a"), JString("b")}),
            ToJson.toJson("a" :: "b" :: Nil)
        )

    @Test
    def toJson28(): Unit \ Assert =
        assertEq(expected = JObject(Map#{}), ToJson.toJson((Map#{}: Map[String, Int32])))

    @Test
    def toJson29(): Unit \ Assert =
        assertEq(
            expected = JObject(Map#{"a" => num(1), "b" => num(2)}),
            ToJson.toJson(Map#{"a" => 1, "b" => 2})
        )

    @Test
    def toJson30(): Unit \ Assert =
        // Non-String keys are stringified via ToString.
        assertEq(
            expected = JObject(Map#{"1" => JBool(true), "2" => JBool(false)}),
            ToJson.toJson(Map#{1 => true, 2 => false})
        )

    /////////////////////////////////////////////////////////////////////////////
    // ToJson — tuples                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toJson31(): Unit \ Assert =
        assertEq(
            expected = JArray(Vector#{num(1), JString("x")}),
            ToJson.toJson((1, "x"))
        )

    @Test
    def toJson32(): Unit \ Assert =
        assertEq(
            expected = JArray(Vector#{num(1), JString("x"), JBool(true)}),
            ToJson.toJson((1, "x", true))
        )

    @Test
    def toJson33(): Unit \ Assert =
        assertEq(
            expected = JArray(Vector#{num(1), num(2), num(3), num(4), num(5)}),
            ToJson.toJson((1, 2, 3, 4, 5))
        )

    @Test
    def toJson34(): Unit \ Assert =
        assertEq(
            expected = JArray(Vector#{
                num(1), num(2), num(3), num(4), num(5),
                num(6), num(7), num(8), num(9), num(10)
            }),
            ToJson.toJson((1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
        )

    /////////////////////////////////////////////////////////////////////////////
    // ToJson — nested via instances                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toJson35(): Unit \ Assert =
        // Option[Vector[Int32]] composes Option + Vector + Int32 instances.
        assertEq(
            expected = JArray(Vector#{num(1), num(2)}),
            ToJson.toJson(Some(Vector#{1, 2}))
        )

    @Test
    def toJson36(): Unit \ Assert =
        // Map of String to List[Bool]
        assertEq(
            expected = JObject(Map#{
                "flags" => JArray(Vector#{JBool(true), JBool(false)})
            }),
            ToJson.toJson(Map#{"flags" => true :: false :: Nil})
        )

    @Test
    def toJson37(): Unit \ Assert =
        // Round-trip toJson then toCompactString
        assertEq(
            expected = "{\"a\":1,\"b\":[true,null]}",
            Json.toCompactString(ToJson.toJson(Map#{
                "a" => num(1),
                "b" => JArray(Vector#{JBool(true), JNull})
            }))
        )

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — primitives                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromJson01(): Unit \ Assert =
        assertEq(expected = (Ok(JBool(true)): Result[JsonError, Json.Json]), FromJson.fromJson(JBool(true)))

    @Test
    def fromJson02(): Unit \ Assert =
        assertEq(expected = (Ok(()): Result[JsonError, Unit]), FromJson.fromJson(JNull))

    @Test
    def fromJson03(): Unit \ Assert =
        assertEq(expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Unit"))): Result[JsonError, Unit]), FromJson.fromJson(JBool(true)))

    @Test
    def fromJson04(): Unit \ Assert =
        assertEq(expected = (Ok(true): Result[JsonError, Bool]), FromJson.fromJson(JBool(true)))

    @Test
    def fromJson05(): Unit \ Assert =
        assertEq(expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Bool"))): Result[JsonError, Bool]), FromJson.fromJson(JString("yes")))

    @Test
    def fromJson08(): Unit \ Assert =
        assertEq(expected = (Ok("hello"): Result[JsonError, String]), FromJson.fromJson(JString("hello")))

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — numbers                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromJson09(): Unit \ Assert =
        assertEq(expected = (Ok(42i8): Result[JsonError, Int8]), FromJson.fromJson(num(42)))

    @Test
    def fromJson10(): Unit \ Assert =
        assertEq(expected = (Ok(-7i16): Result[JsonError, Int16]), FromJson.fromJson(num(-7)))

    @Test
    def fromJson11(): Unit \ Assert =
        assertEq(expected = (Ok(123i32): Result[JsonError, Int32]), FromJson.fromJson(num(123)))

    @Test
    def fromJson12(): Unit \ Assert =
        assertEq(expected = (Ok(99i64): Result[JsonError, Int64]), FromJson.fromJson(num(99)))

    @Test
    def fromJson13(): Unit \ Assert =
        // Out-of-range Int8.
        assertEq(expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int8"))): Result[JsonError, Int8]), FromJson.fromJson(num(200)))

    @Test
    def fromJson14(): Unit \ Assert =
        // Fractional values are rejected (no silent truncation).
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int32"))): Result[JsonError, Int32]),
            FromJson.fromJson(JNumber(1.9ff))
        )

    @Test
    def fromJson15(): Unit \ Assert =
        assertEq(expected = (Ok(BigDecimal.toBigInt(100.0ff)): Result[JsonError, BigInt]), FromJson.fromJson(num(100)))

    @Test
    def fromJson16(): Unit \ Assert =
        assertEq(expected = (Ok(3.14ff): Result[JsonError, BigDecimal]), FromJson.fromJson(JNumber(3.14ff)))

    @Test
    def fromJson17(): Unit \ Assert =
        assertEq(expected = (Ok(2.5f64): Result[JsonError, Float64]), FromJson.fromJson(JNumber(2.5ff)))

    @Test
    def fromJson18(): Unit \ Assert =
        assertEq(expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int32"))): Result[JsonError, Int32]), FromJson.fromJson(JBool(true)))

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — Option                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromJson19(): Unit \ Assert =
        assertEq(expected = (Ok(None): Result[JsonError, Option[Int32]]), FromJson.fromJson(JNull))

    @Test
    def fromJson20(): Unit \ Assert =
        assertEq(expected = (Ok(Some(7i32)): Result[JsonError, Option[Int32]]), FromJson.fromJson(num(7)))

    @Test
    def fromJson21(): Unit \ Assert =
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int32"))): Result[JsonError, Option[Int32]]),
            FromJson.fromJson(JString("nope"))
        )

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — collections                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromJson22(): Unit \ Assert =
        assertEq(
            expected = (Ok(Vector#{1, 2, 3}): Result[JsonError, Vector[Int32]]),
            FromJson.fromJson(JArray(Vector#{num(1), num(2), num(3)}))
        )

    @Test
    def fromJson23(): Unit \ Assert =
        assertEq(
            expected = (Ok((Vector#{}: Vector[Int32])): Result[JsonError, Vector[Int32]]),
            FromJson.fromJson(JArray(Vector#{}))
        )

    @Test
    def fromJson24(): Unit \ Assert =
        assertEq(
            expected = (Ok("a" :: "b" :: Nil): Result[JsonError, List[String]]),
            FromJson.fromJson(JArray(Vector#{JString("a"), JString("b")}))
        )

    @Test
    def fromJson25(): Unit \ Assert =
        // First failing element propagates with its index in the path.
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Index(1) :: Nil), TypeMismatch("Int32"))): Result[JsonError, Vector[Int32]]),
            FromJson.fromJson(JArray(Vector#{num(1), JBool(true)}))
        )

    @Test
    def fromJson26(): Unit \ Assert =
        assertEq(
            expected = (Ok(Map#{"a" => 1i32, "b" => 2i32}): Result[JsonError, Map[String, Int32]]),
            FromJson.fromJson(JObject(Map#{"a" => num(1), "b" => num(2)}))
        )

    @Test
    def fromJson27(): Unit \ Assert =
        // Map keys are decoded via FromString.
        assertEq(
            expected = (Ok(Map#{1i32 => true, 2i32 => false}): Result[JsonError, Map[Int32, Bool]]),
            FromJson.fromJson(JObject(Map#{"1" => JBool(true), "2" => JBool(false)}))
        )

    @Test
    def fromJson28(): Unit \ Assert =
        // Map key that fails FromString errors, with the failing string key in the path.
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("notANumber") :: Nil), TypeMismatch("Map key"))): Result[JsonError, Map[Int32, Bool]]),
            FromJson.fromJson(JObject(Map#{"notANumber" => JBool(true)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — tuples                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromJson29(): Unit \ Assert =
        assertEq(
            expected = (Ok((1i32, "x")): Result[JsonError, (Int32, String)]),
            FromJson.fromJson(JArray(Vector#{num(1), JString("x")}))
        )

    @Test
    def fromJson30(): Unit \ Assert =
        // Wrong arity.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("JArray of length 2"))): Result[JsonError, (Int32, String)]),
            FromJson.fromJson(JArray(Vector#{num(1), JString("x"), JBool(true)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — getAtKey / getAtKeyOpt                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromJson31(): Unit \ Assert =
        assertEq(
            expected = Ok(num(1)),
            getAtKey("a", JObject(Map#{"a" => num(1), "b" => num(2)}))
        )

    @Test
    def fromJson32(): Unit \ Assert =
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), MissingKey("missing"))): Result[JsonError, Json.Json]),
            getAtKey("missing", JObject(Map#{"a" => num(1)}))
        )

    @Test
    def fromJson33(): Unit \ Assert =
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("JObject"))): Result[JsonError, Json.Json]),
            getAtKey("a", num(42))
        )

    @Test
    def fromJson34(): Unit \ Assert =
        assertEq(
            expected = Ok(Some(num(1))),
            getAtKeyOpt("a", JObject(Map#{"a" => num(1)}))
        )

    @Test
    def fromJson35(): Unit \ Assert =
        assertEq(
            expected = (Ok(None): Result[JsonError, Option[Json.Json]]),
            getAtKeyOpt("missing", JObject(Map#{"a" => num(1)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — decodeAtKey / decodeAtKeyOpt                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromJson36(): Unit \ Assert =
        assertEq(
            expected = (Ok(42i32): Result[JsonError, Int32]),
            decodeAtKey("a", JObject(Map#{"a" => num(42)}))
        )

    @Test
    def fromJson37(): Unit \ Assert =
        // Missing key bubbles up as MissingKey, with empty path (we never descended).
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), MissingKey("a"))): Result[JsonError, Int32]),
            decodeAtKey("a", JObject(Map#{"b" => num(42)}))
        )

    @Test
    def fromJson38(): Unit \ Assert =
        assertEq(
            expected = (Ok(Some(7i32)): Result[JsonError, Option[Int32]]),
            decodeAtKeyOpt("a", JObject(Map#{"a" => num(7)}))
        )

    @Test
    def fromJson39(): Unit \ Assert =
        // Absent key yields Ok(None) for the Opt variant.
        assertEq(
            expected = (Ok(None): Result[JsonError, Option[Int32]]),
            decodeAtKeyOpt("missing", JObject(Map#{"a" => num(1)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — round-trip with ToJson                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromJson40(): Unit \ Assert =
        // toJson then fromJson yields the original value for a nested structure.
        let original: Map[String, List[Int32]] = Map#{
            "a" => 1 :: 2 :: Nil,
            "b" => 3 :: Nil
        };
        assertEq(
            expected = (Ok(original): Result[JsonError, Map[String, List[Int32]]]),
            FromJson.fromJson(ToJson.toJson(original))
        )

    @Test
    def fromJson41(): Unit \ Assert =
        // Round-trip through parse + fromJson.
        assertEq(
            expected = (Ok((1i32, true)): Result[JsonError, (Int32, Bool)]),
            Result.flatMap(FromJson.fromJson, Json.parse("[1,true]"))
        )

    /////////////////////////////////////////////////////////////////////////////
    // JsonPath — empty / cons / snoc / steps                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def jsonPath01(): Unit \ Assert =
        assertEq(expected = (Nil: List[JsonStep]), JsonPath.steps(JsonPath.empty()))

    @Test
    def jsonPath02(): Unit \ Assert =
        // cons prepends — head is the outermost (root) step.
        let p = JsonPath.empty() |> JsonPath.cons(Key("name")) |> JsonPath.cons(Key("users"));
        assertEq(expected = Key("users") :: Key("name") :: Nil, JsonPath.steps(p))

    @Test
    def jsonPath03(): Unit \ Assert =
        // snoc appends — tail extends towards the leaf.
        let p = JsonPath.empty() |> p1 -> JsonPath.snoc(p1, Key("users")) |> p2 -> JsonPath.snoc(p2, Index(0));
        assertEq(expected = Key("users") :: Index(0) :: Nil, JsonPath.steps(p))

    @Test
    def jsonPath04(): Unit \ Assert =
        // fromList and steps round-trip.
        let xs = Key("a") :: Index(3) :: Key("b") :: Nil;
        assertEq(expected = xs, JsonPath.steps(JsonPath.fromList(xs)))

    /////////////////////////////////////////////////////////////////////////////
    // JsonPath — toJsonPointer                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def jsonPath05(): Unit \ Assert =
        // RFC 6901: empty path is the empty string (root).
        assertEq(expected = "", JsonPath.toJsonPointer(JsonPath.empty()))

    @Test
    def jsonPath06(): Unit \ Assert =
        assertEq(
            expected = "/users",
            JsonPath.toJsonPointer(JsonPath.fromList(Key("users") :: Nil))
        )

    @Test
    def jsonPath07(): Unit \ Assert =
        assertEq(
            expected = "/0",
            JsonPath.toJsonPointer(JsonPath.fromList(Index(0) :: Nil))
        )

    @Test
    def jsonPath08(): Unit \ Assert =
        assertEq(
            expected = "/users/3/address/zip",
            JsonPath.toJsonPointer(
                JsonPath.fromList(Key("users") :: Index(3) :: Key("address") :: Key("zip") :: Nil)
            )
        )

    @Test
    def jsonPath09(): Unit \ Assert =
        // RFC 6901 escaping: '/' becomes '~1'.
        assertEq(
            expected = "/a~1b",
            JsonPath.toJsonPointer(JsonPath.fromList(Key("a/b") :: Nil))
        )

    @Test
    def jsonPath10(): Unit \ Assert =
        // RFC 6901 escaping: '~' becomes '~0'.
        assertEq(
            expected = "/a~0b",
            JsonPath.toJsonPointer(JsonPath.fromList(Key("a~b") :: Nil))
        )

    @Test
    def jsonPath11(): Unit \ Assert =
        // RFC 6901 escaping: order matters — '~' is escaped before '/'.
        // The literal key "~/" becomes "~0~1".
        assertEq(
            expected = "/~0~1",
            JsonPath.toJsonPointer(JsonPath.fromList(Key("~/") :: Nil))
        )

    @Test
    def jsonPath12(): Unit \ Assert =
        // The empty key is represented as a single '/' in JSON Pointer.
        assertEq(
            expected = "/",
            JsonPath.toJsonPointer(JsonPath.fromList(Key("") :: Nil))
        )

    /////////////////////////////////////////////////////////////////////////////
    // JsonPath — fromJsonPointer                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def jsonPath13(): Unit \ Assert =
        assertEq(
            expected = Some(JsonPath.empty()),
            JsonPath.fromJsonPointer("")
        )

    @Test
    def jsonPath14(): Unit \ Assert =
        // All-digit segments parse as Index.
        assertEq(
            expected = Some(JsonPath.fromList(Index(0) :: Nil)),
            JsonPath.fromJsonPointer("/0")
        )

    @Test
    def jsonPath15(): Unit \ Assert =
        assertEq(
            expected = Some(JsonPath.fromList(Key("users") :: Index(3) :: Key("name") :: Nil)),
            JsonPath.fromJsonPointer("/users/3/name")
        )

    @Test
    def jsonPath16(): Unit \ Assert =
        // ~1 unescapes to '/'.
        assertEq(
            expected = Some(JsonPath.fromList(Key("a/b") :: Nil)),
            JsonPath.fromJsonPointer("/a~1b")
        )

    @Test
    def jsonPath17(): Unit \ Assert =
        // ~0 unescapes to '~'.
        assertEq(
            expected = Some(JsonPath.fromList(Key("a~b") :: Nil)),
            JsonPath.fromJsonPointer("/a~0b")
        )

    @Test
    def jsonPath18(): Unit \ Assert =
        // A non-empty pointer must start with '/'.
        assertEq(
            expected = (None: Option[JsonPath]),
            JsonPath.fromJsonPointer("users/0")
        )

    @Test
    def jsonPath19(): Unit \ Assert =
        // Round-trip: toJsonPointer >> fromJsonPointer for typical paths.
        let p = JsonPath.fromList(Key("users") :: Index(3) :: Key("name") :: Nil);
        assertEq(expected = Some(p), JsonPath.fromJsonPointer(JsonPath.toJsonPointer(p)))

    /////////////////////////////////////////////////////////////////////////////
    // JsonPath — navigate                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def jsonPath20(): Unit \ Assert =
        // Empty path returns the root.
        let j = JObject(Map#{"a" => num(1)});
        assertEq(expected = Some(j), JsonPath.navigate(JsonPath.empty(), j))

    @Test
    def jsonPath21(): Unit \ Assert =
        let j = JObject(Map#{"a" => num(42)});
        assertEq(
            expected = Some(num(42)),
            JsonPath.navigate(JsonPath.fromList(Key("a") :: Nil), j)
        )

    @Test
    def jsonPath22(): Unit \ Assert =
        let j = JArray(Vector#{num(10), num(20), num(30)});
        assertEq(
            expected = Some(num(20)),
            JsonPath.navigate(JsonPath.fromList(Index(1) :: Nil), j)
        )

    @Test
    def jsonPath23(): Unit \ Assert =
        // Nested traversal.
        let j = JObject(Map#{"users" => JArray(Vector#{
            JObject(Map#{"name" => JString("alice")})
        })});
        assertEq(
            expected = Some(JString("alice")),
            JsonPath.navigate(JsonPath.fromList(Key("users") :: Index(0) :: Key("name") :: Nil), j)
        )

    @Test
    def jsonPath24(): Unit \ Assert =
        // Missing key.
        let j = JObject(Map#{"a" => num(1)});
        assertEq(
            expected = (None: Option[Json.Json]),
            JsonPath.navigate(JsonPath.fromList(Key("b") :: Nil), j)
        )

    @Test
    def jsonPath25(): Unit \ Assert =
        // Index out of range.
        let j = JArray(Vector#{num(1), num(2)});
        assertEq(
            expected = (None: Option[Json.Json]),
            JsonPath.navigate(JsonPath.fromList(Index(5) :: Nil), j)
        )

    @Test
    def jsonPath26(): Unit \ Assert =
        // Wrong shape — descending into a primitive.
        assertEq(
            expected = (None: Option[Json.Json]),
            JsonPath.navigate(JsonPath.fromList(Key("a") :: Nil), num(42))
        )

    /////////////////////////////////////////////////////////////////////////////
    // JsonPath — ToString and Formattable                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def jsonPath27(): Unit \ Assert =
        // ToString matches toJsonPointer.
        let p = JsonPath.fromList(Key("users") :: Index(0) :: Nil);
        assertEq(expected = "/users/0", ToString.toString(p))

    @Test
    def jsonPath28(): Unit \ Assert =
        // Formattable's plain text matches toJsonPointer.
        let p = JsonPath.fromList(Key("users") :: Index(0) :: Key("name") :: Nil);
        assertEq(
            expected = "/users/0/name",
            RichString.toPlainString(Formattable.format(p))
        )

    @Test
    def jsonPath29(): Unit \ Assert =
        // Eq via structural equality.
        let p1 = JsonPath.fromList(Key("a") :: Index(0) :: Nil);
        let p2 = JsonPath.fromList(Key("a") :: Index(0) :: Nil);
        assertEq(expected = true, p1 == p2)

    /////////////////////////////////////////////////////////////////////////////
    // Path attachment — leaf decode errors carry the empty path               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pathError01(): Unit \ Assert =
        // A primitive type mismatch at the root has empty path.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Bool"))): Result[JsonError, Bool]),
            FromJson.fromJson(JString("nope"))
        )

    @Test
    def pathError02(): Unit \ Assert =
        // getAtKey on a missing key has empty path; the kind names the key.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), MissingKey("nope"))): Result[JsonError, Json.Json]),
            getAtKey("nope", JObject(Map#{"a" => num(1)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // Path attachment — decodeAtKey wraps with Key step                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pathError03(): Unit \ Assert =
        // Key present but value not decodable: path = Key(k).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Nil), TypeMismatch("Int32"))): Result[JsonError, Int32]),
            decodeAtKey("a", JObject(Map#{"a" => JString("nope")}))
        )

    @Test
    def pathError04(): Unit \ Assert =
        // Outer "a" descended; inner Map fails at key "x".
        // Path threads: Key("a") :: Key("x").
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Key("x") :: Nil), TypeMismatch("Int32"))): Result[JsonError, Map[String, Int32]]),
            decodeAtKey("a", JObject(Map#{
                "a" => JObject(Map#{"x" => JBool(true)})
            }))
        )

    /////////////////////////////////////////////////////////////////////////////
    // Path attachment — decodeAtIndex wraps with Index step                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pathError05(): Unit \ Assert =
        // Element present but not decodable: path = Index(i).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Index(1) :: Nil), TypeMismatch("Int32"))): Result[JsonError, Int32]),
            decodeAtIndex(1, JArray(Vector#{num(0), JString("nope")}))
        )

    @Test
    def pathError06(): Unit \ Assert =
        // Out-of-range index bubbles up as MissingIndex with empty path.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), MissingIndex(5))): Result[JsonError, Int32]),
            decodeAtIndex(5, JArray(Vector#{num(0), num(1)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // Path attachment — Vector / List / Map / Tuple instances                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pathError07(): Unit \ Assert =
        // Vector element failure: path = Index(i).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Index(1) :: Nil), TypeMismatch("Int32"))): Result[JsonError, Vector[Int32]]),
            FromJson.fromJson(JArray(Vector#{num(1), JBool(true)}))
        )

    @Test
    def pathError08(): Unit \ Assert =
        // List element failure: path = Index(i).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Index(0) :: Nil), TypeMismatch("Int32"))): Result[JsonError, List[Int32]]),
            FromJson.fromJson(JArray(Vector#{JBool(true), num(2)}))
        )

    @Test
    def pathError09(): Unit \ Assert =
        // Map value failure: path = Key(k).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("b") :: Nil), TypeMismatch("Int32"))): Result[JsonError, Map[String, Int32]]),
            FromJson.fromJson(JObject(Map#{"a" => num(1), "b" => JBool(true)}))
        )

    @Test
    def pathError10(): Unit \ Assert =
        // Map key parse failure: path = Key(stringKey).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("notANumber") :: Nil), TypeMismatch("Map key"))): Result[JsonError, Map[Int32, Bool]]),
            FromJson.fromJson(JObject(Map#{"notANumber" => JBool(true)}))
        )

    @Test
    def pathError11(): Unit \ Assert =
        // Tuple element failure: path = Index(i).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Index(1) :: Nil), TypeMismatch("String"))): Result[JsonError, (Int32, String)]),
            FromJson.fromJson(JArray(Vector#{num(1), num(2)}))
        )

    @Test
    def pathError12(): Unit \ Assert =
        // Tuple wrong arity: path = empty (the array as a whole doesn't match shape).
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("JArray of length 2"))): Result[JsonError, (Int32, String)]),
            FromJson.fromJson(JArray(Vector#{num(1)}))
        )

    /////////////////////////////////////////////////////////////////////////////
    // Path attachment — Option pass-through and nested composition             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pathError13(): Unit \ Assert =
        // Option does not introduce a step; the inner decode error path is preserved.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int32"))): Result[JsonError, Option[Int32]]),
            FromJson.fromJson(JString("nope"))
        )

    @Test
    def pathError14(): Unit \ Assert =
        // Nested: outer object → inner array → element fails.
        // Path threads through the recursive instances: Key("xs") :: Index(2).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("xs") :: Index(2) :: Nil), TypeMismatch("Int32"))): Result[JsonError, Vector[Int32]]),
            decodeAtKey("xs", JObject(Map#{
                "xs" => JArray(Vector#{num(1), num(2), JBool(true)})
            }))
        )

    @Test
    def pathError15(): Unit \ Assert =
        // Nested: outer object → inner tuple → element fails.
        // Path: Key("pair") :: Index(1).
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("pair") :: Index(1) :: Nil), TypeMismatch("String"))): Result[JsonError, (Int32, String)]),
            decodeAtKey("pair", JObject(Map#{
                "pair" => JArray(Vector#{num(1), num(2)})
            }))
        )

    /////////////////////////////////////////////////////////////////////////////
    // Parse errors are wrapped in the Parse case (no path)                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pathError16(): Unit \ Assert =
        // A parser failure produces a Parse(...) error, not Decode.
        match Json.parse("{") {
            case Err(Parse(_, _)) => assertEq(expected = true, true)
            case _             => assertEq(expected = true, false)
        }

    /////////////////////////////////////////////////////////////////////////////
    // Json.decode — combined parse + fromJson                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def decode01(): Unit \ Assert =
        // Successful round-trip: parse + decode in one call.
        assertEq(
            expected = (Ok(42i32): Result[JsonError, Int32]),
            Json.decode("42")
        )

    @Test
    def decode02(): Unit \ Assert =
        assertEq(
            expected = (Ok((1i32, true)): Result[JsonError, (Int32, Bool)]),
            Json.decode("[1,true]")
        )

    @Test
    def decode03(): Unit \ Assert =
        // Parse failure surfaces as Err(Parse(...)).
        match (Json.decode("{"): Result[JsonError, Json.Json]) {
            case Err(Parse(_, _)) => assertEq(expected = true, true)
            case _             => assertEq(expected = true, false)
        }

    @Test
    def decode04(): Unit \ Assert =
        // Valid JSON, wrong shape: surfaces as Err(Decode(...)) at empty path.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int32"))): Result[JsonError, Int32]),
            Json.decode("\"not a number\"")
        )

    @Test
    def decode05(): Unit \ Assert =
        // Valid JSON; nested decode failure carries the failing element's path.
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Index(1) :: Nil), TypeMismatch("Int32"))): Result[JsonError, Vector[Int32]]),
            Json.decode("[1,true,3]")
        )

    /////////////////////////////////////////////////////////////////////////////
    // JNumber equality is numeric, not by BigDecimal scale                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def numericEq01(): Unit \ Assert =
        // Top-level scalar: "1" and "1.0" are the same JSON number.
        assertEq(expected = Json.parse("1"), Json.parse("1.0"))

    @Test
    def numericEq02(): Unit \ Assert =
        // Nested in array: scale differences inside elements don't break equality.
        assertEq(expected = Json.parse("[1, 2, 3]"), Json.parse("[1.0, 2.00, 3.000]"))

    @Test
    def numericEq03(): Unit \ Assert =
        // Nested in object: scale differences inside values don't break equality.
        assertEq(expected = Json.parse("{\"a\": 1, \"b\": 2}"), Json.parse("{\"a\": 1.0, \"b\": 2.00}"))

    /////////////////////////////////////////////////////////////////////////////
    // Number rendering uses plain decimal, never scientific notation          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def plainNumber01(): Unit \ Assert =
        // Adjusted exponent < -6 would flip Java's BigDecimal.toString to "1E-7".
        assertEq(
            expected = Ok("0.0000001"),
            Result.map(Json.toCompactString, Json.parse("0.0000001"))
        )

    @Test
    def plainNumber02(): Unit \ Assert =
        // Negative scale (literal with exponent) would flip to "1E+5".
        assertEq(
            expected = Ok("100000"),
            Result.map(Json.toCompactString, Json.parse("1e5"))
        )

    @Test
    def plainNumber03(): Unit \ Assert =
        // Pretty-printer takes the same path; non-trivial mantissa + exponent.
        assertEq(
            expected = Ok("15000000000"),
            Result.map(j -> Json.toPrettyString(2, j), Json.parse("1.5e10"))
        )

    /////////////////////////////////////////////////////////////////////////////
    // FromJson — integer types accept integer-valued reals, reject fractions //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def integerValued01(): Unit \ Assert =
        // Int8 accepts an integer-valued real (42.0).
        assertEq(expected = (Ok(42i8): Result[JsonError, Int8]), FromJson.fromJson(JNumber(42.0ff)))

    @Test
    def integerValued02(): Unit \ Assert =
        // Int8 rejects a genuine fraction (42.5).
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int8"))): Result[JsonError, Int8]),
            FromJson.fromJson(JNumber(42.5ff))
        )

    @Test
    def integerValued03(): Unit \ Assert =
        // Int16 accepts an integer-valued real (-7.0).
        assertEq(expected = (Ok(-7i16): Result[JsonError, Int16]), FromJson.fromJson(JNumber(-7.0ff)))

    @Test
    def integerValued04(): Unit \ Assert =
        // Int16 rejects a genuine fraction (7.5).
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int16"))): Result[JsonError, Int16]),
            FromJson.fromJson(JNumber(7.5ff))
        )

    @Test
    def integerValued05(): Unit \ Assert =
        // Int32 accepts an integer-valued real (100.0).
        assertEq(expected = (Ok(100i32): Result[JsonError, Int32]), FromJson.fromJson(JNumber(100.0ff)))

    @Test
    def integerValued06(): Unit \ Assert =
        // Int32 rejects a negative fraction — locks down "no truncate towards zero".
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int32"))): Result[JsonError, Int32]),
            FromJson.fromJson(JNumber(-1.5ff))
        )

    @Test
    def integerValued07(): Unit \ Assert =
        // Int64 accepts an integer-valued real (9999.0).
        assertEq(expected = (Ok(9999i64): Result[JsonError, Int64]), FromJson.fromJson(JNumber(9999.0ff)))

    @Test
    def integerValued08(): Unit \ Assert =
        // Int64 rejects a genuine fraction (9999.5).
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("Int64"))): Result[JsonError, Int64]),
            FromJson.fromJson(JNumber(9999.5ff))
        )

    @Test
    def integerValued09(): Unit \ Assert =
        // BigInt accepts an integer-valued real (42.0).
        assertEq(expected = (Ok(42ii): Result[JsonError, BigInt]), FromJson.fromJson(JNumber(42.0ff)))

    @Test
    def integerValued10(): Unit \ Assert =
        // BigInt rejects a genuine fraction (42.5).
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("BigInt"))): Result[JsonError, BigInt]),
            FromJson.fromJson(JNumber(42.5ff))
        )

    /////////////////////////////////////////////////////////////////////////////
    // Parse errors carry the UTF-16 offset of the failure                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parseOffset01(): Unit \ Assert =
        // Empty input: parser is at offset 0 expecting any value.
        assertEq(
            expected = (Err(Parse(0, UnexpectedEof)): Result[JsonError, Json.Json]),
            Json.parse("")
        )

    @Test
    def parseOffset02(): Unit \ Assert =
        // Trailing junk after a value: offset of the first stray char.
        assertEq(
            expected = (Err(Parse(5, UnexpectedChar('x'))): Result[JsonError, Json.Json]),
            Json.parse("null x")
        )

    @Test
    def parseOffset03(): Unit \ Assert =
        // Unterminated string: EOF at the end of input.
        assertEq(
            expected = (Err(Parse(4, UnexpectedEof)): Result[JsonError, Json.Json]),
            Json.parse("\"abc")
        )

    @Test
    def parseOffset04(): Unit \ Assert =
        // Bad escape: offset of the offending escape character.
        assertEq(
            expected = (Err(Parse(2, InvalidEscape("q"))): Result[JsonError, Json.Json]),
            Json.parse("\"\\q\"")
        )

    @Test
    def parseOffset05(): Unit \ Assert =
        // Duplicate key: offset of the second occurrence's key string.
        assertEq(
            expected = (Err(Parse(7, DuplicateKey("a"))): Result[JsonError, Json.Json]),
            Json.parse("{\"a\":1,\"a\":2}")
        )

    /////////////////////////////////////////////////////////////////////////////
    // ToJson / FromJson — Set                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def setJson01(): Unit \ Assert =
        // Empty set encodes as an empty array.
        assertEq(expected = JArray(Vector#{}), ToJson.toJson((Set#{}: Set[Int32])))

    @Test
    def setJson02(): Unit \ Assert =
        // Set elements are emitted in sorted order regardless of insertion order.
        assertEq(
            expected = JArray(Vector#{num(1), num(2), num(3)}),
            ToJson.toJson(Set#{3i32, 1i32, 2i32})
        )

    @Test
    def setJson03(): Unit \ Assert =
        // Decoding an empty array yields the empty set.
        assertEq(
            expected = (Ok((Set#{}: Set[Int32])): Result[JsonError, Set[Int32]]),
            FromJson.fromJson(JArray(Vector#{}))
        )

    @Test
    def setJson04(): Unit \ Assert =
        // Decoding an array dedups by Order[a].
        assertEq(
            expected = (Ok(Set#{1i32, 2i32}): Result[JsonError, Set[Int32]]),
            FromJson.fromJson(JArray(Vector#{num(1), num(2), num(1)}))
        )

    @Test
    def setJson05(): Unit \ Assert =
        // Element decode failures carry the failing index in the path.
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Index(1) :: Nil), TypeMismatch("Int32"))): Result[JsonError, Set[Int32]]),
            FromJson.fromJson(JArray(Vector#{num(1), JBool(true)}))
        )

    @Test
    def setJson06(): Unit \ Assert =
        // Decoding a non-array produces a TypeMismatch at the empty path.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), TypeMismatch("JArray"))): Result[JsonError, Set[Int32]]),
            FromJson.fromJson(JString("nope"))
        )

    /////////////////////////////////////////////////////////////////////////////
    // getAtPath — raw multi-step lookup                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getAtPath01(): Unit \ Assert =
        // Empty path returns the document itself.
        assertEq(
            expected = (Ok(num(7)): Result[JsonError, Json.Json]),
            getAtPath(JsonPath.empty(), num(7))
        )

    @Test
    def getAtPath02(): Unit \ Assert =
        // Happy path through nested objects and arrays.
        let p = JsonPath.fromList(Key("a") :: Index(1) :: Key("b") :: Nil);
        let j = JObject(Map#{
            "a" => JArray(Vector#{
                num(0),
                JObject(Map#{"b" => JString("hit")})
            })
        });
        assertEq(
            expected = (Ok(JString("hit")): Result[JsonError, Json.Json]),
            getAtPath(p, j)
        )

    @Test
    def getAtPath03(): Unit \ Assert =
        // Missing key in the middle: path points to the parent (everything
        // we successfully consumed), kind carries the missing key.
        let p = JsonPath.fromList(Key("a") :: Key("b") :: Key("missing") :: Nil);
        let j = JObject(Map#{
            "a" => JObject(Map#{"b" => JObject(Map#{})})
        });
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Key("b") :: Nil), MissingKey("missing"))): Result[JsonError, Json.Json]),
            getAtPath(p, j)
        )

    @Test
    def getAtPath04(): Unit \ Assert =
        // Out-of-range index: path is the parent prefix, kind is MissingIndex.
        let p = JsonPath.fromList(Key("xs") :: Index(5) :: Nil);
        let j = JObject(Map#{"xs" => JArray(Vector#{num(0), num(1)})});
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("xs") :: Nil), MissingIndex(5))): Result[JsonError, Json.Json]),
            getAtPath(p, j)
        )

    @Test
    def getAtPath05(): Unit \ Assert =
        // Negative index: also MissingIndex.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), MissingIndex(-1))): Result[JsonError, Json.Json]),
            getAtPath(JsonPath.fromList(Index(-1) :: Nil), JArray(Vector#{num(0)}))
        )

    @Test
    def getAtPath06(): Unit \ Assert =
        // Key step into a non-object: TypeMismatch at the consumed prefix.
        let p = JsonPath.fromList(Key("a") :: Key("b") :: Nil);
        let j = JObject(Map#{"a" => num(5)});
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Nil), TypeMismatch("JObject"))): Result[JsonError, Json.Json]),
            getAtPath(p, j)
        )

    @Test
    def getAtPath07(): Unit \ Assert =
        // Index step into a non-array: TypeMismatch at the consumed prefix.
        let p = JsonPath.fromList(Key("a") :: Index(0) :: Nil);
        let j = JObject(Map#{"a" => num(5)});
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Nil), TypeMismatch("JArray"))): Result[JsonError, Json.Json]),
            getAtPath(p, j)
        )

    /////////////////////////////////////////////////////////////////////////////
    // decodeAtPath — typed multi-step lookup                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def decodeAtPath01(): Unit \ Assert =
        // Happy path with mixed Key/Index steps.
        let p = JsonPath.fromList(Key("a") :: Index(1) :: Key("b") :: Nil);
        let j = JObject(Map#{
            "a" => JArray(Vector#{num(0), JObject(Map#{"b" => num(42)})})
        });
        assertEq(
            expected = (Ok(42i32): Result[JsonError, Int32]),
            decodeAtPath(p, j)
        )

    @Test
    def decodeAtPath02(): Unit \ Assert =
        // Empty path: equivalent to FromJson.fromJson on the document.
        assertEq(
            expected = (Ok(7i32): Result[JsonError, Int32]),
            decodeAtPath(JsonPath.empty(), num(7))
        )

    @Test
    def decodeAtPath03(): Unit \ Assert =
        // Leaf decode failure: full p-prefixed path is reported.
        let p = JsonPath.fromList(Key("a") :: Key("b") :: Key("c") :: Nil);
        let j = JObject(Map#{
            "a" => JObject(Map#{"b" => JObject(Map#{"c" => num(5)})})
        });
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Key("b") :: Key("c") :: Nil), TypeMismatch("String"))): Result[JsonError, String]),
            decodeAtPath(p, j)
        )

    @Test
    def decodeAtPath04(): Unit \ Assert =
        // Missing key mid-path: error at the parent, kind = MissingKey.
        let p = JsonPath.fromList(Key("a") :: Key("b") :: Key("missing") :: Nil);
        let j = JObject(Map#{
            "a" => JObject(Map#{"b" => JObject(Map#{})})
        });
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Key("b") :: Nil), MissingKey("missing"))): Result[JsonError, Int32]),
            decodeAtPath(p, j)
        )

    @Test
    def decodeAtPath05(): Unit \ Assert =
        // Out-of-range index mid-path: error at the parent, kind = MissingIndex.
        let p = JsonPath.fromList(Key("xs") :: Index(5) :: Nil);
        let j = JObject(Map#{"xs" => JArray(Vector#{num(0), num(1)})});
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("xs") :: Nil), MissingIndex(5))): Result[JsonError, Int32]),
            decodeAtPath(p, j)
        )

    @Test
    def decodeAtPath06(): Unit \ Assert =
        // Leaf decoder is itself a Vector decoder; its inner /0 is stitched
        // onto the navigation path /users.
        let p = JsonPath.fromList(Key("users") :: Nil);
        let j = JObject(Map#{
            "users" => JArray(Vector#{JBool(true), num(2)})
        });
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("users") :: Index(0) :: Nil), TypeMismatch("Int32"))): Result[JsonError, Vector[Int32]]),
            decodeAtPath(p, j)
        )

    /////////////////////////////////////////////////////////////////////////////
    // decodeAtPathOpt — missing-along-path = Ok(None)                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def decodeAtPathOpt01(): Unit \ Assert =
        // Happy path: present and decodable → Ok(Some).
        let p = JsonPath.fromList(Key("a") :: Key("b") :: Nil);
        let j = JObject(Map#{"a" => JObject(Map#{"b" => num(7)})});
        assertEq(
            expected = (Ok(Some(7i32)): Result[JsonError, Option[Int32]]),
            decodeAtPathOpt(p, j)
        )

    @Test
    def decodeAtPathOpt02(): Unit \ Assert =
        // Missing key along the path → Ok(None).
        let p = JsonPath.fromList(Key("a") :: Key("missing") :: Nil);
        let j = JObject(Map#{"a" => JObject(Map#{})});
        assertEq(
            expected = (Ok(None): Result[JsonError, Option[Int32]]),
            decodeAtPathOpt(p, j)
        )

    @Test
    def decodeAtPathOpt03(): Unit \ Assert =
        // Missing index along the path → Ok(None).
        let p = JsonPath.fromList(Key("xs") :: Index(99) :: Nil);
        let j = JObject(Map#{"xs" => JArray(Vector#{num(0)})});
        assertEq(
            expected = (Ok(None): Result[JsonError, Option[Int32]]),
            decodeAtPathOpt(p, j)
        )

    @Test
    def decodeAtPathOpt04(): Unit \ Assert =
        // Type mismatch (Key into non-object) is *not* swallowed.
        let p = JsonPath.fromList(Key("a") :: Key("b") :: Nil);
        let j = JObject(Map#{"a" => num(5)});
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Nil), TypeMismatch("JObject"))): Result[JsonError, Option[Int32]]),
            decodeAtPathOpt(p, j)
        )

    @Test
    def decodeAtPathOpt05(): Unit \ Assert =
        // Leaf decode failure is *not* swallowed; full path is reported.
        let p = JsonPath.fromList(Key("a") :: Nil);
        let j = JObject(Map#{"a" => JString("nope")});
        assertEq(
            expected = (Err(Decode(JsonPath.fromList(Key("a") :: Nil), TypeMismatch("Int32"))): Result[JsonError, Option[Int32]]),
            decodeAtPathOpt(p, j)
        )

    /////////////////////////////////////////////////////////////////////////////
    // MissingIndex — surface in single-step decodeAtIndex and ToString        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def missingIndex01(): Unit \ Assert =
        // Negative index also surfaces as MissingIndex.
        assertEq(
            expected = (Err(Decode(JsonPath.empty(), MissingIndex(-1))): Result[JsonError, Int32]),
            decodeAtIndex(-1, JArray(Vector#{num(0)}))
        )

    @Test
    def missingIndex02(): Unit \ Assert =
        // ToString rendering of the kind.
        assertEq(
            expected = "missing index 5",
            ToString.toString((MissingIndex(5): Util.Json.JsonError.DecodeErrorKind))
        )

    @Test
    def missingIndex03(): Unit \ Assert =
        // Whole-error rendering: kind + path.
        let e: JsonError = Decode(JsonPath.fromList(Key("xs") :: Nil), MissingIndex(5));
        assertEq(
            expected = "missing index 5 at /xs",
            ToString.toString(e)
        )

}
