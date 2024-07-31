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
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        c1.compare("aaaa", "aaaa") == 0

    @test
    def comparator02(): Bool \ IO =
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        c1.compare("aaaa", "bbbb") == -1

    @test
    def comparator03(): Bool \ IO =
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        c1.compare("bbbb", "aaaa") == 1

    /////////////////////////////////////////////////////////////////////////////
    // fromOptional                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromOptional01(): Bool \ {} =
        import static java.util.Optional.empty(): ##java.util.Optional \ {};
        let o = empty();
        Adaptor.fromOptional((Proxy.Proxy: Proxy[String]), o) == (None : Option[String])

    @test
    def fromOptional02(): Bool \ {} =
        import static java.util.Optional.of(##java.lang.Object): ##java.util.Optional \ {};
        let o = of(checked_cast("Hello"));
        Adaptor.fromOptional((Proxy.Proxy: Proxy[String]), o) == Some("Hello")

    /////////////////////////////////////////////////////////////////////////////
    // fromMapEntry                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMapEntry01(): Bool \ {} =
        import static java.util.Map.entry(##java.lang.Object, ##java.lang.Object): ##java.util.Map$Entry \ {};
        let e = entry(checked_cast("hello"), checked_cast("world"));
        Adaptor.fromMapEntry((Proxy.Proxy: Proxy[String]), (Proxy.Proxy: Proxy[String]), e) == ("hello", "world")

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
        Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)) == (List#{} : List[String])
    }

    @test
    def fromListToIterator02(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("hello"));
        Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)) == List#{"hello"}
    }

    @test
    def fromListToIterator03(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("hello"), checked_cast("world"));
        Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)) == List#{"hello", "world"}
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
        Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == (Set#{} : Set[String])
    }

    @test
    def fromSetToIterator02(): Bool \ {} = region rc {
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("hello"));
        Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == Set#{"hello"}
    }

    @test
    def fromSetToIterator03(): Bool \ {} = region rc {
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("hello"), checked_cast("world"));
        Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == Set#{"hello", "world"}
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
        let iter = unsafe of().iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        Iterator.toList(flixIter) == (Nil : List[String])
    }

    @test
    def fromIterator02(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let iter = unsafe of(checked_cast("hello")).iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        Iterator.toList(flixIter) == List#{"hello"}
    }


    @test
    def fromIterator03(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let iter = unsafe of(checked_cast("hello"), checked_cast("world")).iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        Iterator.toList(flixIter) == List#{"hello", "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromStreamToIterator                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromStreamToIterator01(): Bool \ {} = region rc {
        import static java.util.stream.Stream.empty(): ##java.util.stream.Stream \ {};
        let s = empty();
        Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == (Nil : List[String])
    }

    @test
    def fromStreamToIterator02(): Bool \ {} = region rc {
        import static java.util.stream.Stream.of(##java.lang.Object): ##java.util.stream.Stream \ {};
        let s = of(checked_cast("hello"));
        Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == "hello" :: Nil
    }

    @test
    def fromStreamToIterator03(): Bool \ {} = region rc {
        import static java.util.stream.Stream.of(Array[##java.lang.Object, rc]): ##java.util.stream.Stream \ {};
        let arr = Array#{checked_cast("hello"), checked_cast("world")} @ rc;
        let s = of(arr);
        Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == "hello" :: "world" :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromCollectionToIterator                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromCollectionToIterator01(): Bool \ {} = region rc {
        import static java.util.List.of(): ##java.util.List \ {};
        let col = checked_cast(of());
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)) == (Nil : List[String])
    }

    @test
    def fromCollectionToIterator02(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let col = checked_cast(of(checked_cast("hello")));
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)) == "hello" :: Nil
    }

    @test
    def fromCollectionToIterator03(): Bool \ {} = region rc {
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let col = checked_cast(of(checked_cast("hello"), checked_cast("world")));
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)) == "hello" :: "world" :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toOptional                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toOptional01(): Bool \ IO =
        import static java.util.Optional.empty(): ##java.util.Optional \ {};
        let o = Adaptor.toOptional(None);
        o.equals(empty())

    @test
    def toOptional02(): Bool \ IO =
        import static java.util.Optional.of(##java.lang.Object): ##java.util.Optional \ {};
        let o = Adaptor.toOptional(Some("hello"));
        o.equals(of(checked_cast("hello")))

    /////////////////////////////////////////////////////////////////////////////
    // toMapEntry                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMapEntry01(): Bool \ IO =
        import static java.util.Map.entry(##java.lang.Object, ##java.lang.Object): ##java.util.Map$Entry \ {};
        let e = Adaptor.toMapEntry(("hello", "world"));
        e.equals(entry(checked_cast("hello"), checked_cast("world")))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool \ IO =
        import static java.util.List.of(): ##java.util.List \ {};
        let l = Adaptor.toList(Nil);
        of().equals(l)

    @test
    def toList02(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = Adaptor.toList(List#{"hello"});
        of(checked_cast("hello")).equals(l)

    @test
    def toList03(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = Adaptor.toList(List#{"hello", "world"});
        of(checked_cast("hello"), checked_cast("world")).equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // toArrayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArrayList01(): Bool \ IO =
        import java_new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(): ##java.util.List \ {};
        let l = Adaptor.toArrayList(Nil);
        newArrayList(checked_cast(of())).equals(l)

    @test
    def toArrayList02(): Bool \ IO =
        import java_new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = Adaptor.toArrayList(List#{"hello"});
        newArrayList(checked_cast(of(checked_cast("hello")))).equals(l)

    @test
    def toArrayList03(): Bool \ IO =
        import java_new java.util.ArrayList(##java.util.Collection): ##java.util.ArrayList \ {} as newArrayList;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = Adaptor.toArrayList(List#{"hello", "world"});
        newArrayList(checked_cast(of(checked_cast("hello"), checked_cast("world")))).equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // toLinkedList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toLinkedList01(): Bool \ IO =
        import java_new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(): ##java.util.List \ {};
        let l = Adaptor.toLinkedList(Nil);
        newLinkedList(checked_cast(of())).equals(l)

    @test
    def toLinkedList02(): Bool \ IO =
        import java_new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = Adaptor.toLinkedList(List#{"hello"});
        newLinkedList(checked_cast(of(checked_cast("hello")))).equals(l)

    @test
    def toLinkedList03(): Bool \ IO =
        import java_new java.util.LinkedList(##java.util.Collection): ##java.util.LinkedList \ {} as newLinkedList;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = Adaptor.toLinkedList(List#{"hello", "world"});
        newLinkedList(checked_cast(of(checked_cast("hello"), checked_cast("world")))).equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool \ IO =
        import static java.util.Set.of(): ##java.util.Set \ {};
        let s = Adaptor.toSet((Set#{} : Set[String]));
        of().equals(s)

    @test
    def toSet02(): Bool \ IO =
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        let s = Adaptor.toSet(Set#{"hello"});
        of(checked_cast("hello")).equals(s)

    @test
    def toSet03(): Bool \ IO =
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        let s = Adaptor.toSet(Set#{"hello", "world"});
        of(checked_cast("hello"), checked_cast("world")).equals(s)

    /////////////////////////////////////////////////////////////////////////////
    // toTreeSet                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toTreeSet01(): Bool \ IO =
        import java_new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(): ##java.util.List \ {};
        let s = Adaptor.toTreeSet((Set#{} : Set[String]));
        newTreeSet(checked_cast(of())).equals(s)

    @test
    def toTreeSet02(): Bool \ IO =
        import java_new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let s = Adaptor.toTreeSet(Set#{"hello"});
        newTreeSet(checked_cast(of(checked_cast("hello")))).equals(s)

    @test
    def toTreeSet03(): Bool \ IO =
        import java_new java.util.TreeSet(##java.util.Collection): ##java.util.TreeSet \ {} as newTreeSet;
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let s = Adaptor.toTreeSet(Set#{"hello", "world"});
        newTreeSet(checked_cast(of(checked_cast("hello"), checked_cast("world")))).equals(s)

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool \ IO =
        import static java.util.Map.of(): ##java.util.Map \ {};
        let m = Adaptor.toMap((Map#{} : Map[String, String]));
        of().equals(m)

    @test
    def toMap02(): Bool \ IO =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = Adaptor.toMap(Map#{"a" => "hello"});
        of(checked_cast("a"), checked_cast("hello")).equals(m)

    @test
    def toMap03(): Bool \ IO =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = Adaptor.toMap(Map#{"a" => "hello", "b" => "world"});
        of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world")).equals(m)

    /////////////////////////////////////////////////////////////////////////////
    // toTreeMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toTreeMap01(): Bool \ IO =
        import java_new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(): ##java.util.Map \ {};
        let m = Adaptor.toTreeMap((Map#{} : Map[String, String]));
        newTreeMap(of()).equals(m)

    @test
    def toTreeMap02(): Bool \ IO =
        import java_new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = Adaptor.toTreeMap(Map#{"a" => "hello"});
        newTreeMap(of(checked_cast("a"), checked_cast("hello"))).equals(m)

    @test
    def toTreeMap03(): Bool \ IO =
        import java_new java.util.TreeMap(##java.util.Map): ##java.util.TreeMap \ {} as newTreeMap;
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = Adaptor.toTreeMap(Map#{"a" => "hello", "b" => "world"});
        newTreeMap(of(checked_cast("a"), checked_cast("hello"), checked_cast("b"), checked_cast("world"))).equals(m)


}
