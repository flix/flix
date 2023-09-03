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

    @test
    def comparator01(): Bool \ IO =
        import java.util.Comparator.compare(##java.lang.Object, ##java.lang.Object): Int32 \ {};
        let c1 = Adaptor.comparator(Proxy.Proxy: Proxy[String]);
        compare(c1, checked_cast("aaaa"), checked_cast("aaaa")) == 0

    @test
    def comparator02(): Bool \ IO =
        import java.util.Comparator.compare(##java.lang.Object, ##java.lang.Object): Int32 \ {};
        let c1 = Adaptor.comparator(Proxy.Proxy: Proxy[String]);
        compare(c1, checked_cast("aaaa"), checked_cast("bbbb")) == -1

    @test
    def comparator03(): Bool \ IO =
        import java.util.Comparator.compare(##java.lang.Object, ##java.lang.Object): Int32 \ {};
        let c1 = Adaptor.comparator(Proxy.Proxy: Proxy[String]);
        compare(c1, checked_cast("bbbb"), checked_cast("aaaa")) == 1

    /////////////////////////////////////////////////////////////////////////////
    // fromOptional                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromOptional01(): Bool \ {} =
        import static java.util.Optional.empty(): ##java.util.Optional \ {};
        let o = empty();
        Adaptor.fromOptional(Proxy.Proxy: Proxy[String], o) == (None : Option[String])

    @test
    def fromOptional02(): Bool \ {} =
        import static java.util.Optional.of(##java.lang.Object): ##java.util.Optional \ {};
        let o = of(checked_cast("Hello"));
        Adaptor.fromOptional(Proxy.Proxy: Proxy[String], o) == Some("Hello")

    /////////////////////////////////////////////////////////////////////////////
    // fromMapEntry                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMapEntry01(): Bool \ {} =
        import static java.util.Map.entry(##java.lang.Object, ##java.lang.Object): ##java.util.Map$Entry \ {};
        let e = entry(checked_cast("hello"), checked_cast("world"));
        Adaptor.fromMapEntry(Proxy.Proxy: Proxy[String], Proxy.Proxy: Proxy[String], e) == ("hello", "world")

    /////////////////////////////////////////////////////////////////////////////
    // fromList                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromList01(): Bool \ {} =
        import static java.util.List.of(): ##java.util.List \ {};
        let l = of();
        Adaptor.fromList(l) == (List#{} : List[String])

    @test
    def fromList02(): Bool \ {} =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("hello"));
        Adaptor.fromList(l) == List#{"hello"}

    @test
    def fromList03(): Bool \ {} =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("hello"), checked_cast("world"));
        Adaptor.fromList(l) == List#{"hello", "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromListToIterator                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromListToIterator01(): Bool \ {} = region rc {
        import static java.util.List.of(): ##java.util.List \ {};
        let l = of();
        Iterator.toList(Adaptor.fromListToIterator(rc, Proxy.Proxy: Proxy[String], l)) == (List#{} : List[String])
    }

    @test
    def fromListToIterator02(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("hello"));
        Iterator.toList(Adaptor.fromListToIterator(rc, Proxy.Proxy: Proxy[String], l)) == List#{"hello"}
    }

    @test
    def fromListToIterator03(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("hello"), checked_cast("world"));
        Iterator.toList(Adaptor.fromListToIterator(rc, Proxy.Proxy: Proxy[String], l)) == List#{"hello", "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromSet                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromSet01(): Bool \ {} =
        import static java.util.Set.of(): ##java.util.Set \ {};
        let s = of();
        Adaptor.fromSet(s) == (Set#{} : Set[String])

    @test
    def fromSet02(): Bool \ {} =
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("hello"));
        Adaptor.fromSet(s) == Set#{"hello"}

    @test
    def fromSet03(): Bool \ {} =
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("hello"), checked_cast("world"));
        Adaptor.fromSet(s) == Set#{"hello", "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromSetToIterator                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromSetToIterator01(): Bool \ {} = region rc {
        import static java.util.Set.of(): ##java.util.Set \ {};
        let s = of();
        Iterator.toSet(Adaptor.fromSetToIterator(rc, Proxy.Proxy: Proxy[String], s)) == (Set#{} : Set[String])
    }

    @test
    def fromSetToIterator02(): Bool \ {} = region rc {
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("hello"));
        Iterator.toSet(Adaptor.fromSetToIterator(rc, Proxy.Proxy: Proxy[String], s)) == Set#{"hello"}
    }

    @test
    def fromSetToIterator03(): Bool \ {} = region rc {
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("hello"), checked_cast("world"));
        Iterator.toSet(Adaptor.fromSetToIterator(rc, Proxy.Proxy: Proxy[String], s)) == Set#{"hello", "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMap01(): Bool \ {} =
        import static java.util.Map.of(): ##java.util.Map \ {};
        let m = of();
        Adaptor.fromMap(m) == (Map#{} : Map[String, String])

    @test
    def fromMap02(): Bool \ {} =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = of(checked_cast("a"), checked_cast("hello"));
        Adaptor.fromMap(m) == Map#{"a" => "hello"}

    @test
    def fromMap03(): Bool \ {} =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world"));
        Adaptor.fromMap(m) == Map#{"a" => "hello", "b" => "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromMapToIterator                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMapToIterator01(): Bool \ {} = region rc {
        import static java.util.Map.of(): ##java.util.Map \ {};
        let m = of();
        Iterator.toMap(Adaptor.fromMapToIterator(rc, m)) == (Map#{} : Map[String, String])
    }

    @test
    def fromMapToIterator02(): Bool \ {} = region rc {
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = of(checked_cast("a"), checked_cast("hello"));
        Iterator.toMap(Adaptor.fromMapToIterator(rc, m)) == Map#{"a" => "hello"}
    }

    @test
    def fromMapToIterator03(): Bool \ {} = region rc {
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world"));
        Iterator.toMap(Adaptor.fromMapToIterator(rc, m)) == Map#{"a" => "hello", "b" => "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromIterator                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromIterator01(): Bool \ {} = region rc {
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.List.iterator(): ##java.util.Iterator \ {};
        let iter = of() |> iterator;
        let flixIter = Adaptor.fromIterator(rc, Proxy.Proxy: Proxy[String], iter);
        Iterator.toList(flixIter) == (Nil : List[String])
    }

    @test
    def fromIterator02(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.iterator(): ##java.util.Iterator \ {};
        let iter = of(checked_cast("hello")) |> iterator;
        let flixIter = Adaptor.fromIterator(rc, Proxy.Proxy: Proxy[String], iter);
        Iterator.toList(flixIter) == List#{"hello"}
    }


    @test
    def fromIterator03(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.iterator(): ##java.util.Iterator \ {};
        let iter = of(checked_cast("hello"), checked_cast("world")) |> iterator;
        let flixIter = Adaptor.fromIterator(rc, Proxy.Proxy: Proxy[String], iter);
        Iterator.toList(flixIter) == List#{"hello", "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromStreamToIterator                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromStreamToIterator01(): Bool \ {} = region rc {
        import static java.util.stream.Stream.empty(): ##java.util.stream.Stream \ {};
        let s = empty();
        Iterator.toList(Adaptor.fromStreamToIterator(rc, Proxy.Proxy: Proxy[String], s)) == (Nil : List[String])
    }

    @test
    def fromStreamToIterator02(): Bool \ {} = region rc {
        import static java.util.stream.Stream.of(##java.lang.Object): ##java.util.stream.Stream \ {};
        let s = of(checked_cast("hello"));
        Iterator.toList(Adaptor.fromStreamToIterator(rc, Proxy.Proxy: Proxy[String], s)) == "hello" :: Nil
    }

    @test
    def fromStreamToIterator03(): Bool \ {} = region rc {
        import static java.util.stream.Stream.of(Array[##java.lang.Object, rc]): ##java.util.stream.Stream \ {};
        let arr = Array#{checked_cast("hello"), checked_cast("world")} @ rc;
        let s = of(arr);
        Iterator.toList(Adaptor.fromStreamToIterator(rc, Proxy.Proxy: Proxy[String], s)) == "hello" :: "world" :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromCollectionToIterator                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromCollectionToIterator01(): Bool \ {} = region rc {
        import static java.util.List.of(): ##java.util.List \ {};
        let col = unchecked_cast(of() as ##java.util.Collection);
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, Proxy.Proxy: Proxy[String], col)) == (Nil : List[String])
    }

    @test
    def fromCollectionToIterator02(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let col = unchecked_cast(of(checked_cast("hello")) as ##java.util.Collection);
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, Proxy.Proxy: Proxy[String], col)) == "hello" :: Nil
    }

    @test
    def fromCollectionToIterator03(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let col = unchecked_cast(of(checked_cast("hello"), checked_cast("world")) as ##java.util.Collection);
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, Proxy.Proxy: Proxy[String], col)) == "hello" :: "world" :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toOptional                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toOptional01(): Bool \ IO =
        import static java.util.Optional.empty(): ##java.util.Optional \ {};
        import java.util.Optional.equals(##java.lang.Object): Bool \ {};
        let o = Adaptor.toOptional(None : Option[String]);
        equals(o, checked_cast(empty()))

    @test
    def toOptional02(): Bool \ IO =
        import static java.util.Optional.of(##java.lang.Object): ##java.util.Optional \ {};
        import java.util.Optional.equals(##java.lang.Object): Bool \ {};
        let o = Adaptor.toOptional(Some("hello"));
        equals(o, checked_cast(of(checked_cast("hello"))))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool \ IO =
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toList(Nil : List[String]);
        equals(of(), checked_cast(l))

    @test
    def toList02(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toList(List#{"hello"});
        equals(of(checked_cast("hello")), checked_cast(l))

    @test
    def toList03(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toList(List#{"hello", "world"});
        equals(of(checked_cast("hello"), checked_cast("world")), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // toArrayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArrayList01(): Bool \ IO =
        import new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.ArrayList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toArrayList(Nil : List[String]);
        equals(newArrayList(unchecked_cast(of() as ##java.util.Collection)), checked_cast(l))

    @test
    def toArrayList02(): Bool \ IO =
        import new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.ArrayList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toArrayList(List#{"hello"});
        equals(newArrayList(unchecked_cast(of(checked_cast("hello")) as ##java.util.Collection)), checked_cast(l))

    @test
    def toArrayList03(): Bool \ IO =
        import new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.ArrayList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toArrayList(List#{"hello", "world"});
        equals(newArrayList(unchecked_cast(of(checked_cast("hello"), checked_cast("world")) as ##java.util.Collection)), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // toLinkedList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toLinkedList01(): Bool \ IO =
        import new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.LinkedList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toLinkedList(Nil : List[String]);
        equals(newLinkedList(unchecked_cast(of() as ##java.util.Collection)), checked_cast(l))

    @test
    def toLinkedList02(): Bool \ IO =
        import new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.LinkedList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toLinkedList(List#{"hello"});
        equals(newLinkedList(unchecked_cast(of(checked_cast("hello")) as ##java.util.Collection)), checked_cast(l))

    @test
    def toLinkedList03(): Bool \ IO =
        import new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.LinkedList.equals(##java.lang.Object): Bool \ {};
        let l = Adaptor.toLinkedList(List#{"hello", "world"});
        equals(newLinkedList(unchecked_cast(of(checked_cast("hello"), checked_cast("world")) as ##java.util.Collection)), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool \ IO =
        import static java.util.Set.of(): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toSet(Set#{} : Set[String]);
        equals(of(), checked_cast(s))

    @test
    def toSet02(): Bool \ IO =
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toSet(Set#{"hello"});
        equals(of(checked_cast("hello")), checked_cast(s))

    @test
    def toSet03(): Bool \ IO =
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toSet(Set#{"hello", "world"});
        equals(of(checked_cast("hello"), checked_cast("world")), checked_cast(s))

    /////////////////////////////////////////////////////////////////////////////
    // toTreeSet                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toTreeSet01(): Bool \ IO =
        import new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.TreeSet.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toTreeSet(Set#{} : Set[String]);
        equals(newTreeSet(unchecked_cast(of() as ##java.util.Collection)), checked_cast(s))

    @test
    def toTreeSet02(): Bool \ IO =
        import new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.TreeSet.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toTreeSet(Set#{"hello"});
        equals(newTreeSet(unchecked_cast(of(checked_cast("hello")) as ##java.util.Collection)), checked_cast(s))

    @test
    def toTreeSet03(): Bool \ IO =
        import new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.TreeSet.equals(##java.lang.Object): Bool \ {};
        let s = Adaptor.toTreeSet(Set#{"hello", "world"});
        equals(newTreeSet(unchecked_cast(of(checked_cast("hello"), checked_cast("world")) as ##java.util.Collection)), checked_cast(s))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool \ IO =
        import static java.util.Map.of(): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toMap(Map#{} : Map[String, String]);
        equals(of(), checked_cast(m))

    @test
    def toMap02(): Bool \ IO =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toMap(Map#{"a" => "hello"});
        equals(of(checked_cast("a"), checked_cast("hello")), checked_cast(m))

    @test
    def toMap03(): Bool \ IO =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toMap(Map#{"a" => "hello", "b" => "world"});
        equals(of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world")), checked_cast(m))

    /////////////////////////////////////////////////////////////////////////////
    // toTreeMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toTreeMap01(): Bool \ IO =
        import new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(): ##java.util.Map \ {};
        import java.util.TreeMap.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toTreeMap(Map#{} : Map[String, String]);
        equals(newTreeMap(of()), checked_cast(m))

    @test
    def toTreeMap02(): Bool \ IO =
        import new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.TreeMap.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toTreeMap(Map#{"a" => "hello"});
        equals(newTreeMap(of(checked_cast("a"), checked_cast("hello"))), checked_cast(m))

    @test
    def toTreeMap03(): Bool \ IO =
        import new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.TreeMap.equals(##java.lang.Object): Bool \ {};
        let m = Adaptor.toTreeMap(Map#{"a" => "hello", "b" => "world"});
        equals(newTreeMap(of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world"))), checked_cast(m))


}
