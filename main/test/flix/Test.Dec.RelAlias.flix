mod Test.Dec.RelAlias {

    //@test
    //def testDecRel01(): #{ A } = #{}

    //@test
    //def testDecRel02(): #{ A } = #{ A(123). }

    //@test
    //def testDecRel03(): #{ A } = #{ A(123). A(456). }

   // @test
    //def testDecRel04(): #{ A, B } = #{ }

    //@test
    //def testDecRel05(): #{ A, B } = #{ A(123). }

    //@test
    //def testDecRel06(): #{ A, B } = #{ A(123). B(true, 456). }

    //@test
    //def testDecRel07(): #{ A, B, C[Bool] } = #{ A(123). B(true, 456). C(true, true). }

    //@test
    //def testDecRel08(): X = #{ A(21). B(true, 42). }

    //@test
    //def testDecRel09(): Y = #{ A(21). A(42). C(true, true). }

   // @test
    //def testDecRel10(): Unit = f(#{})

    //@test
    //def testDecRel11(): Unit = f(#{ A(21). })

   // @test
    //def testDecRel12(): Unit = f(#{ A(21). B(true, 42). })

    //@test
    //def testDecRel13(): Unit = f(#{ A(21). B(true, 42). C(true, true). })

    //@test
    //def testDecRel14(): Unit = f(#{ A(21). B(true, 42). C(true, true). } : Z[#( A, B, C[Bool] )])

    //def f(_: Z[_]): Unit = ()

    //type alias X = #{ A, B }

    //type alias Y = #{ A, B, C[Bool] }

    //type alias Z[r: SchemaRow] = #{ A | r }

}
