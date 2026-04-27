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

    use Assert.assertEq;
    use Json.Json.{JArray, JBool, JNull, JNumber, JObject, JString}

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

}
