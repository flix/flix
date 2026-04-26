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

}
