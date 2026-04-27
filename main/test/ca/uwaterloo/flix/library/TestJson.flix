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
    use Json.Json.{JArray, JBool, JNull, JNumber, JObject, JString}
    use Json.JsonError.JsonError
    use Json.JsonError.JsonError.{MissingKey, TypeMismatch}
    use Json.ToJson.ToJson
    use Json.FromJson.FromJson
    use Json.FromJson.{getAtKey, getAtKeyOpt, decodeAtKey, decodeAtKeyOpt}

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
    def toJson04(): Unit \ Assert =
        assertEq(expected = JString("a"), ToJson.toJson('a'))

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
        assertEq(expected = (Err(TypeMismatch("Unit")): Result[JsonError, Unit]), FromJson.fromJson(JBool(true)))

    @Test
    def fromJson04(): Unit \ Assert =
        assertEq(expected = (Ok(true): Result[JsonError, Bool]), FromJson.fromJson(JBool(true)))

    @Test
    def fromJson05(): Unit \ Assert =
        assertEq(expected = (Err(TypeMismatch("Bool")): Result[JsonError, Bool]), FromJson.fromJson(JString("yes")))

    @Test
    def fromJson06(): Unit \ Assert =
        assertEq(expected = (Ok('x'): Result[JsonError, Char]), FromJson.fromJson(JString("x")))

    @Test
    def fromJson07(): Unit \ Assert =
        // Multi-char string is not a Char.
        assertEq(expected = (Err(TypeMismatch("Char")): Result[JsonError, Char]), FromJson.fromJson(JString("ab")))

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
        assertEq(expected = (Err(TypeMismatch("Int8")): Result[JsonError, Int8]), FromJson.fromJson(num(200)))

    @Test
    def fromJson14(): Unit \ Assert =
        // Fractional values are truncated towards 0.
        assertEq(expected = (Ok(1i32): Result[JsonError, Int32]), FromJson.fromJson(JNumber(1.9ff)))

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
        assertEq(expected = (Err(TypeMismatch("Int32")): Result[JsonError, Int32]), FromJson.fromJson(JBool(true)))

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
            expected = (Err(TypeMismatch("Int32")): Result[JsonError, Option[Int32]]),
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
        // First failing element propagates.
        assertEq(
            expected = (Err(TypeMismatch("Int32")): Result[JsonError, Vector[Int32]]),
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
        // Map key that fails FromString errors.
        assertEq(
            expected = (Err(TypeMismatch("Map key")): Result[JsonError, Map[Int32, Bool]]),
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
            expected = (Err(TypeMismatch("JArray of length 2")): Result[JsonError, (Int32, String)]),
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
            expected = Err(MissingKey("missing")),
            getAtKey("missing", JObject(Map#{"a" => num(1)}))
        )

    @Test
    def fromJson33(): Unit \ Assert =
        assertEq(
            expected = Err(TypeMismatch("JObject")),
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
        // Missing key bubbles up as MissingKey.
        assertEq(
            expected = (Err(MissingKey("a")): Result[JsonError, Int32]),
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

}
