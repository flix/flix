/*
 *  Copyright 2021 Jonathan Lindegaard Starup
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

mod Test.Exp.Jvm.TestTryCatch {

    def exception(): Unit \ IO =
        import static java.lang.Math.floorDiv(Int32, Int32): Int32 \ IO;
        discard floorDiv(1, 0);
        ()

    @test
    def testNoException01(): Bool =
        try {
            true
        } catch {
            case _: ##java.lang.Exception => false
        }

    @test
    def testSimpleException01(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: ##java.lang.Exception => true
        }

    @test
    def testMultipleExceptions01(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: ##java.lang.ArithmeticException => true
            case _: ##java.lang.IllegalArgumentException => false
        }

    @test
    def testMultipleExceptions02(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: ##java.lang.IllegalArgumentException => false
            case _: ##java.lang.ArithmeticException => true
        }

    @test
    def testMultipleExceptions03(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: ##java.lang.IllegalArgumentException => false
        } catch {
            case _: ##java.lang.ArithmeticException => true
        }

    @test
    def testBinding01(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case e: ##java.lang.ArithmeticException => if (e.toString().isEmpty()) false else true
            case _: ##java.lang.Exception => false
        }

    @test
    def testBinding02(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case e: ##java.lang.ArithmeticException => if (e.getMessage().isEmpty()) false else true
            case _: ##java.lang.Exception => false
        }

    @test
    def testPriority01(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: ##java.lang.Exception => true
            case _: ##java.lang.ArithmeticException => false
        }

    @test
    def testPriority02(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: ##java.lang.ArithmeticException => true
            case _: ##java.lang.Exception => false
        }

}
