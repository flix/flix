mod Test.Equality.Map {

    @Test
    def testEqMap01(): Bool = (Map#{}: Map[Unit, Unit]) == Map#{}: Map[Unit, Unit]

    @Test
    def testEqMap02(): Bool = Map#{1 => 2} == Map#{1 => 2}

    @Test
    def testEqMap03(): Bool = Map#{Map#{3 => 2} => 4} == Map#{Map#{3 => 2} => 4}

    @Test
    def testEqMap04(): Bool = Map#{Map#{3 => 2, 2 => 2} => Map#{1 => 4}} == Map#{Map#{2 => 2, 3 => 2} => Map#{1 => 4}}

    @Test
    def testEqMap05(): Bool = Map#{3 => Map#{1 => 4, 2 => 8}} == Map#{3 => Map#{2 => 8, 1 => 4}}

    @Test
    def testEqMap06(): Bool = Map#{Map#{3 => Map#{2 => 1, 5 => 6}, 2 => Map#{2 => 6, 8 => 16}} => Map#{1 => Map#{1 => 4, 2 => 3, 9 => 8}}} ==
                                Map#{Map#{2 => Map#{2 => 6, 8 => 16}, 3 => Map#{5 => 6, 2 => 1}} => Map#{1 => Map#{2 => 3, 9 => 8, 1 => 4}}}

    @Test
    def testEqMap07(): Bool = Map#{Map#{Map#{3 => 3} => Map#{2 => 1, 5 => 6}, Map#{2 => 2} => Map#{2 => 6, 8 => 16}} => Map#{1 => Map#{1 => 4, 2 => 3, 9 => 8}}} ==
                                Map#{Map#{Map#{2 => 2} => Map#{2 => 6, 8 => 16}, Map#{3 => 3} => Map#{5 => 6, 2 => 1}} => Map#{1 => Map#{2 => 3, 9 => 8, 1 => 4}}}

    @Test
    def testEqMap08(): Bool = Map#{Map#{Map#{3 => 3, 8 => 8} => Map#{2 => 1, 5 => 6}, Map#{2 => 2} => Map#{2 => 6, 8 => 16}} => Map#{1 => Map#{1 => 4, 2 => 3, 9 => 8}}} ==
                                Map#{Map#{Map#{2 => 2} => Map#{2 => 6, 8 => 16}, Map#{8 => 8, 3 => 3} => Map#{5 => 6, 2 => 1}} => Map#{1 => Map#{2 => 3, 9 => 8, 1 => 4}}}

    @Test
    def testEqMap09(): Bool = Map#{1 => 2, 3 => 4} == Map#{1 => 2, 3 => 4}

    @Test
    def testEqMap10(): Bool = Map#{1 => 2, 3 => 4} == Map#{3 => 4, 1 => 2}

    @Test
    def testEqMap11(): Bool = Map#{Map#{1 => 2} => Map#{3 => 4}, Map#{3 => 4} => Map#{4 => 5}} ==
                                Map#{Map#{3 => 4} => Map#{4 => 5}, Map#{1 => 2} => Map#{3 => 4}}

    @Test
    def testEqMap12(): Bool = Map#{Map#{1 => 2, 8 => 9} => Map#{3 => 4}, Map#{3 => 4, 4 => 3, 7 => 8} => Map#{4 => 5}} ==
                                Map#{Map#{7 => 8, 4 => 3, 3 => 4} => Map#{4 => 5}, Map#{8 => 9, 1 => 2} => Map#{3 => 4}}

    @Test
    def testEqMap13(): Bool = Map#{Map#{1 => 2, 8 => 9} => Map#{3 => Map#{4 => 4, 2 => 6}}, Map#{3 => 4, 4 => 3, 7 => 8} => Map#{4 => Map#{2 => 6, 4 => 4}}} ==
                                Map#{Map#{7 => 8, 4 => 3, 3 => 4} => Map#{4 => Map#{2 => 6, 4 => 4}}, Map#{8 => 9, 1 => 2} => Map#{3 => Map#{2 => 6, 4 => 4}}}

    @Test
    def testEqMap14(): Bool = Map#{1 => 2, 5 => 6, 3 => 4} == Map#{1 => 2, 5 => 6, 3 => 4}

    @Test
    def testEqMap15(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{5 => 6, 3 => 4, 1 => 2}

    @Test
    def testNeqMap01(): Bool = Map#{} != Map#{1 => 2}

    @Test
    def testNeqMap02(): Bool = Map#{1 => 2} != Map#{1 => 3}

    @Test
    def testNeqMap03(): Bool = Map#{1 => 2} != Map#{2 => 2}

    @Test
    def testNeqMap04(): Bool = Map#{Map#{3 => 2} => 4} != Map#{Map#{3 => 1} => 4}

    @Test
    def testNeqMap05(): Bool = Map#{Map#{3 => 2} => 4} != Map#{Map#{3 => 2} => 2}

    @Test
    def testNeqMap06(): Bool = Map#{Map#{3 => 2, 2 => 2} => Map#{1 => 4}} != Map#{Map#{2 => 2, 3 => 3} => Map#{1 => 4}}

    @Test
    def testNeqMap07(): Bool = Map#{3 => Map#{1 => 4, 2 => 8}} != Map#{3 => Map#{2 => 4, 1 => 4}}

    @Test
    def testNeqMap08(): Bool = Map#{3 => Map#{1 => 4, 2 => 8}} != Map#{6 => Map#{2 => 8, 1 => 4}}

    @Test
    def testNeqMap09(): Bool = Map#{Map#{3 => Map#{2 => 1, 5 => 6}, 2 => Map#{2 => 6, 8 => 16}} => Map#{1 => Map#{1 => 4, 2 => 3, 9 => 8}}} !=
                                Map#{Map#{2 => Map#{2 => 6, 8 => 16}, 3 => Map#{5 => 6, 2 => 19}} => Map#{1 => Map#{2 => 3, 9 => 8, 1 => 4}}}

    @Test
    def testNeqMap10(): Bool = Map#{Map#{1 => Map#{2 => 1, 5 => 6}, 2 => Map#{2 => 6, 8 => 16}} => Map#{1 => Map#{1 => 4, 2 => 3, 9 => 8}}} !=
                                Map#{Map#{2 => Map#{2 => 6, 8 => 16}, 3 => Map#{5 => 6, 2 => 1}} => Map#{1 => Map#{2 => 3, 9 => 8, 1 => 4}}}

    @Test
    def testNeqMap11(): Bool = Map#{Map#{Map#{3 => 3} => Map#{2 => 1, 5 => 6}, Map#{2 => 2} => Map#{2 => 6, 8 => 16}} => Map#{1 => Map#{1 => 4, 2 => 3, 9 => 8}}} !=
                                Map#{Map#{Map#{2 => 2} => Map#{2 => 6, 8 => 16}, Map#{3 => 3} => Map#{5 => 7, 2 => 1}} => Map#{1 => Map#{2 => 3, 9 => 8, 1 => 4}}}

    @Test
    def testNeqMap12(): Bool = Map#{Map#{Map#{3 => 3} => Map#{2 => 1, 5 => 6}, Map#{2 => 2} => Map#{2 => 6, 8 => 16}} => Map#{1 => Map#{1 => 4, 2 => 3, 9 => 8}}} !=
                                Map#{Map#{Map#{2 => 2} => Map#{2 => 6, 8 => 16}, Map#{3 => 3} => Map#{5 => 6, 2 => 1}} => Map#{1 => Map#{2 => 3, 9 => 8, 1 => 5}}}

    @Test
    def testNeqMap13(): Bool = Map#{Map#{Map#{3 => 3, 8 => 8} => Map#{2 => 1, 5 => 6}, Map#{2 => 2} => Map#{2 => 6, 8 => 16}} => Map#{1 => Map#{1 => 4, 2 => 3, 9 => 8}}} !=
                                Map#{Map#{Map#{2 => 2} => Map#{2 => 6, 8 => 16}, Map#{8 => 8, 3 => 33} => Map#{5 => 6, 2 => 1}} => Map#{1 => Map#{2 => 3, 9 => 8, 1 => 4}}}

    @Test
    def testNeqMap14(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2, 3 => 5}

    @Test
    def testNeqMap15(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2}

    @Test
    def testNeqMap16(): Bool = Map#{1 => 2, 3 => 4} != Map#{3 => 5, 1 => 2}

    @Test
    def testNeqMap17(): Bool = Map#{Map#{1 => 2} => Map#{3 => 4}, Map#{3 => 4} => Map#{4 => 5}} !=
                                Map#{Map#{3 => 4} => Map#{4 => 5}, Map#{1 => 2} => Map#{3 => 11}}

    @Test
    def testNeqMap18(): Bool = Map#{Map#{1 => 2, 8 => 9} => Map#{3 => 4}, Map#{3 => 4, 4 => 3, 7 => 8} => Map#{4 => 5}} !=
                                Map#{Map#{7 => 8, 4 => 3, 3 => 4} => Map#{4 => 5}, Map#{8 => 9, 1 => 2} => Map#{3 => 11}}

    @Test
    def testNeqMap19(): Bool = Map#{Map#{1 => 2, 8 => 9} => Map#{3 => Map#{4 => 4, 2 => 6}}, Map#{3 => 4, 4 => 3, 7 => 8} => Map#{4 => Map#{2 => 6, 4 => 4}}} !=
                                Map#{Map#{7 => 8, 4 => 3, 3 => 4} => Map#{4 => Map#{2 => 6, 4 => 4}}, Map#{8 => 10, 1 => 2} => Map#{3 => Map#{2 => 6, 4 => 4}}}

    @Test
    def testNeqMap20(): Bool = Map#{1 => 2, 5 => 6, 3 => 4} != Map#{1 => 2, 5 => 6}

    @Test
    def testNeqMap21(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} != Map#{5 => 6, 3 => 4, 1 => 3}

}
