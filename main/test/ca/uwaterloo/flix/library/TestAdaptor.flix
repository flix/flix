mod TestAdaptor {

    use Assert.{assertEq, assertTrue};

    import java.util.Optional
    import java.util.stream.Stream
    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.util.{List => JList}
    import java.util.TreeMap
    import java.util.TreeSet
    import java.util.ArrayList
    import java.util.LinkedList
    import java.util.Arrays

    /////////////////////////////////////////////////////////////////////////////
    // Comparator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test @Skip
    def comparator01(): Unit \ Assert + IO =
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        assertEq(expected = 0, c1.compare("aaaa", "aaaa"))

    @Test
    def comparator02(): Unit \ Assert + IO =
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        assertEq(expected = -1, c1.compare("aaaa", "bbbb"))

    @Test
    def comparator03(): Unit \ Assert + IO =
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        assertEq(expected = 1, c1.compare("bbbb", "aaaa"))

    /////////////////////////////////////////////////////////////////////////////
    // fromOptional                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromOptional01(): Unit \ Assert =
        let o = unsafe Optional.empty();
        assertEq(expected = (None : Option[String]), Adaptor.fromOptional((Proxy.Proxy: Proxy[String]), o))

    @Test
    def fromOptional02(): Unit \ Assert =
        let o = unsafe Optional.of("Hello");
        assertEq(expected = Some("Hello"), Adaptor.fromOptional((Proxy.Proxy: Proxy[String]), o))

    /////////////////////////////////////////////////////////////////////////////
    // fromMapEntry                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromMapEntry01(): Unit \ Assert =
        let e = unsafe JMap.entry("hello", "world");
        assertEq(expected = ("hello", "world"), Adaptor.fromMapEntry((Proxy.Proxy: Proxy[String]), (Proxy.Proxy: Proxy[String]), e))

    /////////////////////////////////////////////////////////////////////////////
    // fromList                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromList01(): Unit \ Assert =
        let l = unsafe JList.of();
        assertEq(expected = (List#{} : List[String]), Adaptor.fromList(l))

    @Test
    def fromList02(): Unit \ Assert =
        let l = unsafe JList.of("hello");
        assertEq(expected = List#{"hello"}, Adaptor.fromList(l))

    @Test
    def fromList03(): Unit \ Assert =
        let l = unsafe JList.of("hello", "world");
        assertEq(expected = List#{"hello", "world"}, Adaptor.fromList(l))

    /////////////////////////////////////////////////////////////////////////////
    // fromListToIterator                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromListToIterator01(): Unit \ Assert = region rc {
        let l = unsafe JList.of();
        assertEq(expected = (List#{} : List[String]), Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)))
    }

    @Test
    def fromListToIterator02(): Unit \ Assert = region rc {
        let l = unsafe JList.of("hello");
        assertEq(expected = List#{"hello"}, Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)))
    }

    @Test
    def fromListToIterator03(): Unit \ Assert = region rc {
        let l = unsafe JList.of("hello", "world");
        assertEq(expected = List#{"hello", "world"}, Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromSet                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromSet01(): Unit \ Assert =
        let s = unsafe JSet.of();
        assertEq(expected = (Set#{} : Set[String]), Adaptor.fromSet(s))

    @Test
    def fromSet02(): Unit \ Assert =
        let s = unsafe JSet.of("hello");
        assertEq(expected = Set#{"hello"}, Adaptor.fromSet(s))

    @Test
    def fromSet03(): Unit \ Assert =
        let s = unsafe JSet.of("hello", "world");
        assertEq(expected = Set#{"hello", "world"}, Adaptor.fromSet(s))

    /////////////////////////////////////////////////////////////////////////////
    // fromSetToIterator                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromSetToIterator01(): Unit \ Assert = region rc {
        let s = unsafe JSet.of();
        assertEq(expected = (Set#{} : Set[String]), Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)))
    }

    @Test
    def fromSetToIterator02(): Unit \ Assert = region rc {
        let s = unsafe JSet.of("hello");
        assertEq(expected = Set#{"hello"}, Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)))
    }

    @Test
    def fromSetToIterator03(): Unit \ Assert = region rc {
        let s = unsafe JSet.of("hello", "world");
        assertEq(expected = Set#{"hello", "world"}, Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromMap01(): Unit \ Assert =
        let m = unsafe JMap.of();
        assertEq(expected = (Map#{} : Map[String, String]), Adaptor.fromMap(m))

    @Test
    def fromMap02(): Unit \ Assert =
        let m = unsafe JMap.of("a", "hello");
        assertEq(expected = Map#{"a" => "hello"}, Adaptor.fromMap(m))

    @Test
    def fromMap03(): Unit \ Assert =
        let m = unsafe JMap.of("a", "hello", "b", "world");
        assertEq(expected = Map#{"a" => "hello", "b" => "world"}, Adaptor.fromMap(m))

    /////////////////////////////////////////////////////////////////////////////
    // fromMapToIterator                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromMapToIterator01(): Unit \ Assert = region rc {
        let m = unsafe JMap.of();
        assertEq(expected = (Map#{} : Map[String, String]), Iterator.toMap(Adaptor.fromMapToIterator(rc, m)))
    }

    @Test
    def fromMapToIterator02(): Unit \ Assert = region rc {
        let m = unsafe JMap.of("a", "hello");
        assertEq(expected = Map#{"a" => "hello"}, Iterator.toMap(Adaptor.fromMapToIterator(rc, m)))
    }

    @Test
    def fromMapToIterator03(): Unit \ Assert = region rc {
        let m = unsafe JMap.of("a", "hello", "b", "world");
        assertEq(expected = Map#{"a" => "hello", "b" => "world"}, Iterator.toMap(Adaptor.fromMapToIterator(rc, m)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromIterator                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromIterator01(): Unit \ Assert = region rc {
        let iter = unsafe JList.of().iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        assertEq(expected = (Nil : List[String]), Iterator.toList(flixIter))
    }

    @Test
    def fromIterator02(): Unit \ Assert = region rc {
        let iter = unsafe JList.of("hello").iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        assertEq(expected = List#{"hello"}, Iterator.toList(flixIter))
    }


    @Test
    def fromIterator03(): Unit \ Assert = region rc {
        let iter = unsafe JList.of("hello", "world").iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        assertEq(expected = List#{"hello", "world"}, Iterator.toList(flixIter))
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromStreamToIterator                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromStreamToIterator01(): Unit \ Assert = region rc {
        let s = unsafe Stream.empty();
        assertEq(expected = (Nil : List[String]), Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)))
    }

    @Test
    def fromStreamToIterator02(): Unit \ Assert = region rc {
        let s = unsafe Stream.of("hello");
        assertEq(expected = "hello" :: Nil, Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)))
    }

    @Test
    def fromStreamToIterator03(): Unit \ Assert = region rc {
        let s = unsafe Arrays.stream(Vector#{"hello", "world"});
        assertEq(expected = "hello" :: "world" :: Nil, Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromCollectionToIterator                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromCollectionToIterator01(): Unit \ Assert = region rc {
        let col = checked_cast (unsafe JList.of());
        assertEq(expected = (Nil : List[String]), Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)))
    }

    @Test
    def fromCollectionToIterator02(): Unit \ Assert = region rc {
        let col = checked_cast (unsafe JList.of("hello"));
        assertEq(expected = "hello" :: Nil, Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)))
    }

    @Test
    def fromCollectionToIterator03(): Unit \ Assert = region rc {
        let col = checked_cast (unsafe JList.of("hello", "world"));
        assertEq(expected = "hello" :: "world" :: Nil, Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toOptional                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toOptional01(): Unit \ Assert + IO =
        let o = Adaptor.toOptional(None);
        assertTrue(o.equals(Optional.empty()))

    @Test
    def toOptional02(): Unit \ Assert + IO =
        let o = Adaptor.toOptional(Some("hello"));
        assertTrue(o.equals(Optional.of("hello")))

    /////////////////////////////////////////////////////////////////////////////
    // toMapEntry                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapEntry01(): Unit \ Assert + IO =
        let e = Adaptor.toMapEntry(("hello", "world"));
        assertTrue(e.equals(JMap.entry("hello", "world")))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert + IO =
        let l = Adaptor.toList(Nil);
        assertTrue(JList.of().equals(l))

    @Test
    def toList02(): Unit \ Assert + IO =
        let l = Adaptor.toList(List#{"hello"});
        assertTrue(JList.of("hello").equals(l))

    @Test
    def toList03(): Unit \ Assert + IO =
        let l = Adaptor.toList(List#{"hello", "world"});
        assertTrue(JList.of("hello", "world").equals(l))

    /////////////////////////////////////////////////////////////////////////////
    // toArrayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArrayList01(): Unit \ Assert + IO =
        let l = Adaptor.toArrayList(Nil);
        assertTrue(new ArrayList(JList.of()).equals(l))

    @Test
    def toArrayList02(): Unit \ Assert + IO =
        let l = Adaptor.toArrayList(List#{"hello"});
        assertTrue(new ArrayList(JList.of("hello")).equals(l))

    @Test
    def toArrayList03(): Unit \ Assert + IO =
        let l = Adaptor.toArrayList(List#{"hello", "world"});
        assertTrue(new ArrayList(JList.of("hello", "world")).equals(l))

    /////////////////////////////////////////////////////////////////////////////
    // toLinkedList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toLinkedList01(): Unit \ Assert + IO =
        let l = Adaptor.toLinkedList(Nil);
        assertTrue(new LinkedList(JList.of()).equals(l))

    @Test
    def toLinkedList02(): Unit \ Assert + IO =
        let l = Adaptor.toLinkedList(List#{"hello"});
        assertTrue(new LinkedList(JList.of("hello")).equals(l))

    @Test
    def toLinkedList03(): Unit \ Assert + IO =
        let l = Adaptor.toLinkedList(List#{"hello", "world"});
        assertTrue(new LinkedList(JList.of("hello", "world")).equals(l))

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert + IO =
        let s = Adaptor.toSet((Set#{} : Set[String]));
        assertTrue(JSet.of().equals(s))

    @Test
    def toSet02(): Unit \ Assert + IO =
        let s = Adaptor.toSet(Set#{"hello"});
        assertTrue(JSet.of("hello").equals(s))

    @Test
    def toSet03(): Unit \ Assert + IO =
        let s = Adaptor.toSet(Set#{"hello", "world"});
        assertTrue(JSet.of("hello", "world").equals(s))

    /////////////////////////////////////////////////////////////////////////////
    // toTreeSet                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toTreeSet01(): Unit \ Assert + IO =
        let s = Adaptor.toTreeSet((Set#{} : Set[String]));
        assertTrue(new TreeSet(JList.of()).equals(s))

    @Test
    def toTreeSet02(): Unit \ Assert + IO =
        let s = Adaptor.toTreeSet(Set#{"hello"});
        assertTrue(new TreeSet(JList.of("hello")).equals(s))

    @Test
    def toTreeSet03(): Unit \ Assert + IO =
        let s = Adaptor.toTreeSet(Set#{"hello", "world"});
        assertTrue(new TreeSet(JList.of("hello", "world")).equals(s))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert + IO =
        let m = Adaptor.toMap((Map#{} : Map[String, String]));
        assertTrue(JMap.of().equals(m))

    @Test
    def toMap02(): Unit \ Assert + IO =
        let m = Adaptor.toMap(Map#{"a" => "hello"});
        assertTrue(JMap.of("a", "hello").equals(m))

    @Test
    def toMap03(): Unit \ Assert + IO =
        let m = Adaptor.toMap(Map#{"a" => "hello", "b" => "world"});
        assertTrue(JMap.of("a", "hello", "b", "world").equals(m))

    /////////////////////////////////////////////////////////////////////////////
    // toTreeMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toTreeMap01(): Unit \ Assert + IO =
        let m = Adaptor.toTreeMap((Map#{} : Map[String, String]));
        assertTrue(new TreeMap(JMap.of()).equals(m))

    @Test
    def toTreeMap02(): Unit \ Assert + IO =
        let m = Adaptor.toTreeMap(Map#{"a" => "hello"});
        assertTrue(new TreeMap(JMap.of("a", "hello")).equals(m))

    @Test
    def toTreeMap03(): Unit \ Assert + IO =
        let m = Adaptor.toTreeMap(Map#{"a" => "hello", "b" => "world"});
        assertTrue(new TreeMap(JMap.of("a", "hello", "b", "world")).equals(m))

}
