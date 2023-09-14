/*
 * Copyright 2023 Stephen Tetley
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

mod TestAdaptor {

    /////////////////////////////////////////////////////////////////////////////
    // Comparator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def comparator01(): Bool \ IO =
        import java.util.Comparator.compare(##java.lang.Object, ##java.lang.Object): Int32 \ {};
        let c1 = Adaptor.comparator(Proxy.Proxy: Proxy[String]);
        compare(c1, checked_cast("aaaa"), checked_cast("aaaa")) == 0

    @Test
    def comparator02(): Bool \ IO =
        import java.util.Comparator.compare(##java.lang.Object, ##java.lang.Object): Int32 \ {};
        let c1 = Adaptor.comparator(Proxy.Proxy: Proxy[String]);
        compare(c1, checked_cast("aaaa"), checked_cast("bbbb")) == -1

    @Test
    def comparator03(): Bool \ IO =
        import java.util.Comparator.compare(##java.lang.Object, ##java.lang.Object): Int32 \ {};
        let c1 = Adaptor.comparator(Proxy.Proxy: Proxy[String]);
        compare(c1, checked_cast("bbbb"), checked_cast("aaaa")) == 1

    /////////////////////////////////////////////////////////////////////////////
    // fromList                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromList01(): Bool \ {} =
        import static java.util.List.of(): ##java.util.List \ {};
        let l = of();
        Adaptor.fromList(l) == (List#{} : List[String])

    @Test
    def fromList02(): Bool \ {} =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("hello"));
        Adaptor.fromList(l) == List#{"hello"}

    @Test
    def fromList03(): Bool \ {} =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("hello"), checked_cast("world"));
        Adaptor.fromList(l) == List#{"hello", "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromSet                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromSet01(): Bool \ {} =
        import static java.util.Set.of(): ##java.util.Set \ {};
        let s = of();
        Adaptor.fromSet(s) == (Set#{} : Set[String])

    @Test
    def fromSet02(): Bool \ {} =
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("hello"));
        Adaptor.fromSet(s) == Set#{"hello"}

    @Test
    def fromSet03(): Bool \ {} =
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("hello"), checked_cast("world"));
        Adaptor.fromSet(s) == Set#{"hello", "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromMap01(): Bool \ {} =
        import static java.util.Map.of(): ##java.util.Map \ {};
        let m = of();
        Adaptor.fromMap(m) == (Map#{} : Map[String, String])

    @Test
    def fromMap02(): Bool \ {} =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = of(checked_cast("a"), checked_cast("hello"));
        Adaptor.fromMap(m) == Map#{"a" => "hello"}

    @Test
    def fromMap03(): Bool \ {} =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world"));
        Adaptor.fromMap(m) == Map#{"a" => "hello", "b" => "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromIterator                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromIterator01(): Bool \ {} = region rc {
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.List.iterator(): ##java.util.Iterator \ {};
        let iter = of() |> iterator;
        let flixIter = Adaptor.fromIterator(rc, Proxy.Proxy: Proxy[String], iter);
        Iterator.toList(flixIter) == (Nil : List[String])
    }

    @Test
    def fromIterator02(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.iterator(): ##java.util.Iterator \ {};
        let iter = of(checked_cast("hello")) |> iterator;
        let flixIter = Adaptor.fromIterator(rc, Proxy.Proxy: Proxy[String], iter);
        Iterator.toList(flixIter) == List#{"hello"}
    }


    @Test
    def fromIterator03(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.iterator(): ##java.util.Iterator \ {};
        let iter = of(checked_cast("hello"), checked_cast("world")) |> iterator;
        let flixIter = Adaptor.fromIterator(rc, Proxy.Proxy: Proxy[String], iter);
        Iterator.toList(flixIter) == List#{"hello", "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool \ IO =
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toList(Nil : List[String]);
        equals(of(), checked_cast(l))

    @Test
    def toList02(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toList(List#{"hello"});
        equals(of(checked_cast("hello")), checked_cast(l))

    @Test
    def toList03(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toList(List#{"hello", "world"});
        equals(of(checked_cast("hello"), checked_cast("world")), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // toArrayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArrayList01(): Bool \ IO =
        import new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.ArrayList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toArrayList(Nil : List[String]);
        equals(newArrayList(unchecked_cast(of() as ##java.util.Collection)), checked_cast(l))

    @Test
    def toArrayList02(): Bool \ IO =
        import new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.ArrayList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toArrayList(List#{"hello"});
        equals(newArrayList(unchecked_cast(of(checked_cast("hello")) as ##java.util.Collection)), checked_cast(l))

    @Test
    def toArrayList03(): Bool \ IO =
        import new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.ArrayList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toArrayList(List#{"hello", "world"});
        equals(newArrayList(unchecked_cast(of(checked_cast("hello"), checked_cast("world")) as ##java.util.Collection)), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // toLinkedList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toLinkedList01(): Bool \ IO =
        import new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.LinkedList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toLinkedList(Nil : List[String]);
        equals(newLinkedList(unchecked_cast(of() as ##java.util.Collection)), checked_cast(l))

    @Test
    def toLinkedList02(): Bool \ IO =
        import new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.LinkedList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toLinkedList(List#{"hello"});
        equals(newLinkedList(unchecked_cast(of(checked_cast("hello")) as ##java.util.Collection)), checked_cast(l))

    @Test
    def toLinkedList03(): Bool \ IO =
        import new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.LinkedList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toLinkedList(List#{"hello", "world"});
        equals(newLinkedList(unchecked_cast(of(checked_cast("hello"), checked_cast("world")) as ##java.util.Collection)), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool \ IO =
        import static java.util.Set.of(): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toSet(Set#{} : Set[String]);
        equals(of(), checked_cast(s))

    @Test
    def toSet02(): Bool \ IO =
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toSet(Set#{"hello"});
        equals(of(checked_cast("hello")), checked_cast(s))

    @Test
    def toSet03(): Bool \ IO =
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toSet(Set#{"hello", "world"});
        equals(of(checked_cast("hello"), checked_cast("world")), checked_cast(s))

    /////////////////////////////////////////////////////////////////////////////
    // toTreeSet                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toTreeSet01(): Bool \ IO =
        import new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.TreeSet.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toTreeSet(Set#{} : Set[String]);
        equals(newTreeSet(unchecked_cast(of() as ##java.util.Collection)), checked_cast(s))

    @Test
    def toTreeSet02(): Bool \ IO =
        import new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.TreeSet.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toTreeSet(Set#{"hello"});
        equals(newTreeSet(unchecked_cast(of(checked_cast("hello")) as ##java.util.Collection)), checked_cast(s))

    @Test
    def toTreeSet03(): Bool \ IO =
        import new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.TreeSet.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toTreeSet(Set#{"hello", "world"});
        equals(newTreeSet(unchecked_cast(of(checked_cast("hello"), checked_cast("world")) as ##java.util.Collection)), checked_cast(s))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Bool \ IO =
        import static java.util.Map.of(): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toMap(Map#{} : Map[String, String]);
        equals(of(), checked_cast(m))

    @Test
    def toMap02(): Bool \ IO =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toMap(Map#{"a" => "hello"});
        equals(of(checked_cast("a"), checked_cast("hello")), checked_cast(m))

    @Test
    def toMap03(): Bool \ IO =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toMap(Map#{"a" => "hello", "b" => "world"});
        equals(of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world")), checked_cast(m))

    /////////////////////////////////////////////////////////////////////////////
    // toTreeMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toTreeMap01(): Bool \ IO =
        import new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(): ##java.util.Map \ {};
        import java.util.TreeMap.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toTreeMap(Map#{} : Map[String, String]);
        equals(newTreeMap(of()), checked_cast(m))

    @Test
    def toTreeMap02(): Bool \ IO =
        import new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.TreeMap.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toTreeMap(Map#{"a" => "hello"});
        equals(newTreeMap(of(checked_cast("a"), checked_cast("hello"))), checked_cast(m))

    @Test
    def toTreeMap03(): Bool \ IO =
        import new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.TreeMap.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toTreeMap(Map#{"a" => "hello", "b" => "world"});
        equals(newTreeMap(of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world"))), checked_cast(m))


}
