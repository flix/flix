mod Test.Equality.Set {

    @test
    def testEqSet01(): Bool = (Set#{}: Set[Unit]) == (Set#{}: Set[Unit])

    @test
    def testEqSet02(): Bool = Set#{1} == Set#{1}

    @test
    def testEqSet03(): Bool = Set#{Set#{3}} == Set#{Set#{3}}

    @test
    def testEqSet04(): Bool = Set#{Set#{3, 2}} == Set#{Set#{2, 3}}

    @test
    def testEqSet05(): Bool = Set#{Set#{(Set#{}: Set[Unit])}} == Set#{Set#{Set#{}}}

    @test
    def testEqSet06(): Bool = Set#{Set#{Set#{4}}} == Set#{Set#{Set#{4}}}

    @test
    def testEqSet07(): Bool = Set#{Set#{Set#{4, 11}}} == Set#{Set#{Set#{11, 4}}}

    @test
    def testEqSet08(): Bool = Set#{Set#{Set#{4, 12, 1}, Set#{16, 24, 1}}} == Set#{Set#{Set#{4, 1, 12}, Set#{24, 16, 1}}}

    @test
    def testEqSet09(): Bool = Set#{Set#{Set#{4, 12, 1}, Set#{22, 3, 33}, Set#{16, 24, 1}}} ==
                                Set#{Set#{Set#{22, 3, 33}, Set#{4, 1, 12}, Set#{24, 16, 1}}}

    @test
    def testEqSet10(): Bool = Set#{Set#{Set#{4, 12, 1}, Set#{22, 3, 33}, Set#{16, 24, 1}, Set#{}}} ==
                                Set#{Set#{Set#{22, 3, 33}, Set#{}, Set#{4, 1, 12}, Set#{24, 16, 1}}}

    @test
    def testEqSet11(): Bool = Set#{1, 2} == Set#{2, 1}

    @test
    def testEqSet12(): Bool = Set#{1, 2} == Set#{1, 2}

    @test
    def testEqSet13(): Bool = Set#{Set#{}, Set#{1, 2}} == Set#{Set#{1, 2}, Set#{}}

    @test
    def testEqSet14(): Bool = Set#{Set#{44}, Set#{1, 2}} == Set#{Set#{1, 2}, Set#{44}}

    @test
    def testEqSet15(): Bool = Set#{Set#{Set#{}}, Set#{Set#{}, Set#{2}}} == Set#{Set#{Set#{2}, Set#{}}, Set#{Set#{}}}

    @test
    def testEqSet16(): Bool = Set#{Set#{Set#{}, Set#{4, 5}}, Set#{Set#{1}, Set#{6, 7, 9}}} == Set#{Set#{Set#{1}, Set#{7, 9, 6}}, Set#{Set#{}, Set#{4, 5}}}

    @test
    def testEqSet17(): Bool = Set#{Set#{Set#{}, Set#{4, 5}, Set#{2, 3, 4}}, Set#{Set#{1}, Set#{6, 7, 9}}} ==
                                Set#{Set#{Set#{1}, Set#{7, 9, 6}}, Set#{Set#{2, 3, 4}, Set#{}, Set#{4, 5}}}

    @test
    def testEqSet18(): Bool = Set#{Set#{Set#{Set#{4}}, Set#{Set#{4, 5}, Set#{2, 1}}}, Set#{Set#{Set#{1}}, Set#{Set#{6, 7, 9}, Set#{2, 3}}}} ==
                                Set#{Set#{Set#{Set#{1, 2}, Set#{5, 4}}, Set#{Set#{4}}}, Set#{Set#{Set#{1}}, Set#{Set#{2, 3}, Set#{9, 6, 7}}}}

    @test
    def testEqSet19(): Bool = Set#{1, 2, 3} == Set#{2, 3, 1}

    @test
    def testEqSet20(): Bool = Set#{Set#{}, Set#{1, 2, 3}, Set#{2, 3, 4}} == Set#{Set#{4, 2, 3}, Set#{3, 1, 2}, Set#{}}

    @test
    def testEqSet21(): Bool = Set#{Set#{}, Set#{Set#{1, 3}, Set#{4}}, Set#{Set#{2, 3}, Set#{4}}} == Set#{Set#{Set#{4}, Set#{2, 3}}, Set#{Set#{3, 1}, Set#{4}}, Set#{}}

    @test
    def testEqSet22(): Bool = Set#{Set#{}, Set#{Set#{Set#{1, 3}}, Set#{Set#{4}}}, Set#{Set#{Set#{2}, Set#{3}}, Set#{Set#{4}}}} ==
                                Set#{Set#{Set#{Set#{4}}, Set#{Set#{2}, Set#{3}}}, Set#{Set#{Set#{3, 1}}, Set#{Set#{4}}}, Set#{}}

    @test
    def testNeqSet01(): Bool = Set#{1} != Set#{}

    @test
    def testNeqSet02(): Bool = Set#{1} != Set#{2}

    @test
    def testNeqSet03(): Bool = Set#{Set#{3}} != Set#{Set#{4}}

    @test
    def testNeqSet04(): Bool = Set#{Set#{3, 2}} != Set#{Set#{2, 4}}

    @test
    def testNeqSet05(): Bool = Set#{Set#{3, 2}} != Set#{Set#{2, 3}, Set#{}}

    @test
    def testNeqSet06(): Bool = Set#{Set#{Set#{}}} != Set#{Set#{Set#{11}}}

    @test
    def testNeqSet07(): Bool = {
        let x = Set#{Set#{}};
        let y: Set[Set[Set[Unit]]] = Set#{Set#{Set#{}}};
        x != y
    }

    @test
    def testNeqSet08(): Bool = Set#{Set#{Set#{4}}} != Set#{Set#{Set#{5}}}

    @test
    def testNeqSet09(): Bool = Set#{Set#{Set#{4}}} != Set#{Set#{Set#{4}, Set#{4, 1}}}

    @test
    def testNeqSet10(): Bool = Set#{Set#{Set#{4, 11}}} != Set#{Set#{Set#{11, 5}}}

    @test
    def testNeqSet11(): Bool = Set#{Set#{Set#{4, 12, 1}, Set#{16, 24, 1}}} != Set#{Set#{Set#{4, 12}, Set#{24, 16, 1}}}

    @test
    def testNeqSet12(): Bool = Set#{Set#{Set#{4, 12, 1}, Set#{22, 3, 33}, Set#{16, 24, 1}}} !=
                                Set#{Set#{Set#{22, 3, 33}, Set#{4, 1, 12}, Set#{24, 16, 11}}}

    @test
    def testNeqSet13(): Bool = Set#{Set#{Set#{4, 12, 1}, Set#{22, 3, 33}, Set#{16, 24, 1}}} !=
                                Set#{Set#{Set#{22, 3, 33}, Set#{4, 1, 12}, Set#{9}, Set#{24, 16, 1}}}

    @test
    def testNeqSet14(): Bool = Set#{Set#{Set#{4, 12, 1}, Set#{22, 3, 33}, Set#{16, 24, 1}, Set#{}}} !=
                                Set#{Set#{Set#{2, 3, 33}, Set#{}, Set#{4, 1, 12}, Set#{24, 16, 11}}}

    @test
    def testNeqSet15(): Bool = Set#{1, 2} != Set#{2, 3}

    @test
    def testNeqSet16(): Bool = Set#{1, 2} != Set#{8}

    @test
    def testNeqSet17(): Bool = Set#{Set#{}, Set#{1, 2}} != Set#{Set#{1}, Set#{2}}

    @test
    def testNeqSet18(): Bool = Set#{Set#{44}, Set#{1, 2}} != Set#{Set#{1, 3}, Set#{44}}

    @test
    def testNeqSet19(): Bool = Set#{Set#{44}, Set#{1, 2}} != Set#{Set#{1, 2}, Set#{2, 3}, Set#{44}}

    @test
    def testNeqSet20(): Bool = Set#{Set#{Set#{8}}, Set#{Set#{}, Set#{2}}} != Set#{Set#{Set#{2}, Set#{}}, Set#{Set#{}}}

    @test
    def testNeqSet21(): Bool = Set#{Set#{Set#{}, Set#{4, 5}}, Set#{Set#{1}, Set#{6, 8, 9}}} != Set#{Set#{Set#{1}, Set#{7, 9, 6}}, Set#{Set#{}, Set#{4, 5}}}

    @test
    def testNeqSet22(): Bool = Set#{Set#{Set#{}, Set#{4, 5}, Set#{2, 3, 4}}, Set#{Set#{1}, Set#{6, 7, 9}}} !=
                                Set#{Set#{Set#{1}, Set#{7, 9, 6}}, Set#{Set#{2, 7, 4}, Set#{1}, Set#{4, 5}}}

    @test
    def testNeqSet23(): Bool = Set#{Set#{Set#{Set#{4}}, Set#{Set#{4, 5}, Set#{2, 1}}}, Set#{Set#{Set#{1}}, Set#{Set#{6, 7, 9}, Set#{2, 3}}}} !=
                                Set#{Set#{Set#{Set#{1, 2, 3}, Set#{5, 4}}, Set#{Set#{4}}}, Set#{Set#{Set#{1}}, Set#{Set#{2, 3}, Set#{9, 6, 7}}}}

    @test
    def testNeqSet24(): Bool = Set#{1, 2, 3} != Set#{2, 3, 4}

    @test
    def testNeqSet25(): Bool = Set#{1, 2, 3} != Set#{2, 3, 1, 0}

    @test
    def testNeqSet26(): Bool = Set#{Set#{}, Set#{1, 2, 3}, Set#{2, 3}} != Set#{Set#{4, 2, 3}, Set#{3, 1, 2}, Set#{}}

    @test
    def testNeqSet27(): Bool = Set#{Set#{}, Set#{Set#{1, 3}, Set#{4, 5}}, Set#{Set#{2, 3}, Set#{4}}} !=
                                 Set#{Set#{Set#{4}, Set#{2, 3, 4}}, Set#{Set#{3, 1}, Set#{4, 5}}, Set#{}}

    @test
    def testNeqSet28(): Bool = Set#{Set#{}, Set#{Set#{Set#{1, 3}}, Set#{Set#{4}}}, Set#{Set#{Set#{2}, Set#{3}}, Set#{Set#{4}}}} !=
                                Set#{Set#{Set#{Set#{4}}, Set#{Set#{2}, Set#{3}}}, Set#{Set#{Set#{3, 1, 17}}, Set#{Set#{4}}}, Set#{}}

}
