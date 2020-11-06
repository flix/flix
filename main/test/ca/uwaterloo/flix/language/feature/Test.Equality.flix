//
//
// # Map
//

@test
def eqMap01(): Bool = Map#{} == Map#{}

@test
def eqMap02(): Bool = Map#{1 -> 2} == Map#{1 -> 2}

@test
def eqMap03(): Bool = Map#{Map#{3 -> 2} -> 4} == Map#{Map#{3 -> 2} -> 4}

@test
def eqMap04(): Bool = Map#{Map#{3 -> 2, 2 -> 2} -> Map#{1 -> 4}} == Map#{Map#{2 -> 2, 3 -> 2} -> Map#{1 -> 4}}

@test
def eqMap05(): Bool = Map#{3 -> Map#{1 -> 4, 2 -> 8}} == Map#{3 -> Map#{2 -> 8, 1 -> 4}}

@test
def eqMap06(): Bool = Map#{Map#{3 -> Map#{2 -> 1, 5 -> 6}, 2 -> Map#{2 -> 6, 8 -> 16}} -> Map#{1 -> Map#{1 -> 4, 2 -> 3, 9 -> 8}}} ==
                            Map#{Map#{2 -> Map#{2 -> 6, 8 -> 16}, 3 -> Map#{5 -> 6, 2 -> 1}} -> Map#{1 -> Map#{2 -> 3, 9 -> 8, 1 -> 4}}}

@test
def eqMap07(): Bool = Map#{Map#{Map#{3 -> 3} -> Map#{2 -> 1, 5 -> 6}, Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}} -> Map#{1 -> Map#{1 -> 4, 2 -> 3, 9 -> 8}}} ==
                            Map#{Map#{Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}, Map#{3 -> 3} -> Map#{5 -> 6, 2 -> 1}} -> Map#{1 -> Map#{2 -> 3, 9 -> 8, 1 -> 4}}}

@test
def eqMap08(): Bool = Map#{Map#{Map#{3 -> 3, 8 -> 8} -> Map#{2 -> 1, 5 -> 6}, Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}} -> Map#{1 -> Map#{1 -> 4, 2 -> 3, 9 -> 8}}} ==
                            Map#{Map#{Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}, Map#{8 -> 8, 3 -> 3} -> Map#{5 -> 6, 2 -> 1}} -> Map#{1 -> Map#{2 -> 3, 9 -> 8, 1 -> 4}}}

@test
def eqMap09(): Bool = Map#{1 -> 2, 3 -> 4} == Map#{1 -> 2, 3 -> 4}

@test
def eqMap10(): Bool = Map#{1 -> 2, 3 -> 4} == Map#{3 -> 4, 1 -> 2}

@test
def eqMap11(): Bool = Map#{Map#{1 -> 2} -> Map#{3 -> 4}, Map#{3 -> 4} -> Map#{4 -> 5}} ==
                            Map#{Map#{3 -> 4} -> Map#{4 -> 5}, Map#{1 -> 2} -> Map#{3 -> 4}}

@test
def eqMap12(): Bool = Map#{Map#{1 -> 2, 8 -> 9} -> Map#{3 -> 4}, Map#{3 -> 4, 4 -> 3, 7 -> 8} -> Map#{4 -> 5}} ==
                            Map#{Map#{7 -> 8, 4 -> 3, 3 -> 4} -> Map#{4 -> 5}, Map#{8 -> 9, 1 -> 2} -> Map#{3 -> 4}}

@test
def eqMap13(): Bool = Map#{Map#{1 -> 2, 8 -> 9} -> Map#{3 -> Map#{4 -> 4, 2 -> 6}}, Map#{3 -> 4, 4 -> 3, 7 -> 8} -> Map#{4 -> Map#{2 -> 6, 4 -> 4}}} ==
                            Map#{Map#{7 -> 8, 4 -> 3, 3 -> 4} -> Map#{4 -> Map#{2 -> 6, 4 -> 4}}, Map#{8 -> 9, 1 -> 2} -> Map#{3 -> Map#{2 -> 6, 4 -> 4}}}

@test
def eqMap14(): Bool = Map#{1 -> 2, 5 -> 6, 3 -> 4} == Map#{1 -> 2, 5 -> 6, 3 -> 4}

@test
def eqMap15(): Bool = Map#{1 -> 2, 3 -> 4, 5 -> 6} == Map#{5 -> 6, 3 -> 4, 1 -> 2}

@test
def neqMap01(): Bool = Map#{} != Map#{1 -> 2}

@test
def neqMap02(): Bool = Map#{1 -> 2} != Map#{1 -> 3}

@test
def neqMap03(): Bool = Map#{1 -> 2} != Map#{2 -> 2}

@test
def neqMap04(): Bool = Map#{Map#{3 -> 2} -> 4} != Map#{Map#{3 -> 1} -> 4}

@test
def neqMap05(): Bool = Map#{Map#{3 -> 2} -> 4} != Map#{Map#{3 -> 2} -> 2}

@test
def neqMap06(): Bool = Map#{Map#{3 -> 2, 2 -> 2} -> Map#{1 -> 4}} != Map#{Map#{2 -> 2, 3 -> 3} -> Map#{1 -> 4}}

@test
def neqMap07(): Bool = Map#{3 -> Map#{1 -> 4, 2 -> 8}} != Map#{3 -> Map#{2 -> 4, 1 -> 4}}

@test
def neqMap08(): Bool = Map#{3 -> Map#{1 -> 4, 2 -> 8}} != Map#{6 -> Map#{2 -> 8, 1 -> 4}}

@test
def neqMap09(): Bool = Map#{Map#{3 -> Map#{2 -> 1, 5 -> 6}, 2 -> Map#{2 -> 6, 8 -> 16}} -> Map#{1 -> Map#{1 -> 4, 2 -> 3, 9 -> 8}}} !=
                            Map#{Map#{2 -> Map#{2 -> 6, 8 -> 16}, 3 -> Map#{5 -> 6, 2 -> 19}} -> Map#{1 -> Map#{2 -> 3, 9 -> 8, 1 -> 4}}}

@test
def neqMap10(): Bool = Map#{Map#{1 -> Map#{2 -> 1, 5 -> 6}, 2 -> Map#{2 -> 6, 8 -> 16}} -> Map#{1 -> Map#{1 -> 4, 2 -> 3, 9 -> 8}}} !=
                            Map#{Map#{2 -> Map#{2 -> 6, 8 -> 16}, 3 -> Map#{5 -> 6, 2 -> 1}} -> Map#{1 -> Map#{2 -> 3, 9 -> 8, 1 -> 4}}}

@test
def neqMap11(): Bool = Map#{Map#{Map#{3 -> 3} -> Map#{2 -> 1, 5 -> 6}, Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}} -> Map#{1 -> Map#{1 -> 4, 2 -> 3, 9 -> 8}}} !=
                            Map#{Map#{Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}, Map#{3 -> 3} -> Map#{5 -> 7, 2 -> 1}} -> Map#{1 -> Map#{2 -> 3, 9 -> 8, 1 -> 4}}}

@test
def neqMap12(): Bool = Map#{Map#{Map#{3 -> 3} -> Map#{2 -> 1, 5 -> 6}, Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}} -> Map#{1 -> Map#{1 -> 4, 2 -> 3, 9 -> 8}}} !=
                            Map#{Map#{Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}, Map#{3 -> 3} -> Map#{5 -> 6, 2 -> 1}} -> Map#{1 -> Map#{2 -> 3, 9 -> 8, 1 -> 5}}}

@test
def neqMap13(): Bool = Map#{Map#{Map#{3 -> 3, 8 -> 8} -> Map#{2 -> 1, 5 -> 6}, Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}} -> Map#{1 -> Map#{1 -> 4, 2 -> 3, 9 -> 8}}} !=
                            Map#{Map#{Map#{2 -> 2} -> Map#{2 -> 6, 8 -> 16}, Map#{8 -> 8, 3 -> 33} -> Map#{5 -> 6, 2 -> 1}} -> Map#{1 -> Map#{2 -> 3, 9 -> 8, 1 -> 4}}}

@test
def neqMap14(): Bool = Map#{1 -> 2, 3 -> 4} != Map#{1 -> 2, 3 -> 5}

@test
def neqMap15(): Bool = Map#{1 -> 2, 3 -> 4} != Map#{1 -> 2}

@test
def neqMap16(): Bool = Map#{1 -> 2, 3 -> 4} != Map#{3 -> 5, 1 -> 2}

@test
def neqMap17(): Bool = Map#{Map#{1 -> 2} -> Map#{3 -> 4}, Map#{3 -> 4} -> Map#{4 -> 5}} !=
                            Map#{Map#{3 -> 4} -> Map#{4 -> 5}, Map#{1 -> 2} -> Map#{3 -> 11}}

@test
def neqMap18(): Bool = Map#{Map#{1 -> 2, 8 -> 9} -> Map#{3 -> 4}, Map#{3 -> 4, 4 -> 3, 7 -> 8} -> Map#{4 -> 5}} !=
                            Map#{Map#{7 -> 8, 4 -> 3, 3 -> 4} -> Map#{4 -> 5}, Map#{8 -> 9, 1 -> 2} -> Map#{3 -> 11}}

@test
def neqMap19(): Bool = Map#{Map#{1 -> 2, 8 -> 9} -> Map#{3 -> Map#{4 -> 4, 2 -> 6}}, Map#{3 -> 4, 4 -> 3, 7 -> 8} -> Map#{4 -> Map#{2 -> 6, 4 -> 4}}} !=
                            Map#{Map#{7 -> 8, 4 -> 3, 3 -> 4} -> Map#{4 -> Map#{2 -> 6, 4 -> 4}}, Map#{8 -> 10, 1 -> 2} -> Map#{3 -> Map#{2 -> 6, 4 -> 4}}}

@test
def neqMap20(): Bool = Map#{1 -> 2, 5 -> 6, 3 -> 4} != Map#{1 -> 2, 5 -> 6}

@test
def neqMap21(): Bool = Map#{1 -> 2, 3 -> 4, 5 -> 6} != Map#{5 -> 6, 3 -> 4, 1 -> 3}


//
// # Composition of Data Types
//

@test
def eqCompose01(): Bool = Some(Map#{1 -> 2, 3 -> 4}) == Some(Map#{3 -> 4, 1 -> 2})

@test
def eqCompose02(): Bool = Some(Map#{1 -> (1 :: 2 :: Nil), 3 -> (3 :: 4 :: Nil)}) ==
                                Some(Map#{3 -> (3 :: 4 :: Nil), 1 -> (1 :: 2 :: Nil)})

@test
def eqCompose03(): Bool = Some(Map#{1 -> (Set#{1, 2, 3, 4, 5} :: Set#{2, 1} :: Nil), 3 -> (Set#{3} :: Set#{4, 2} :: Nil)}) ==
                                Some(Map#{3 -> (Set#{3} :: Set#{4, 2} :: Nil), 1 -> (Set#{1, 5, 4, 2, 3} :: Set#{1, 2} :: Nil)})

@test
def eqCompose04(): Bool = Some(Map#{1 -> (Set#{1, 2, 3, 4, 5} :: Set#{2, 1} :: Nil, Ok(2)), 3 -> (Set#{3} :: Set#{4, 2} :: Nil, Ok(1))}) ==
                                Some(Map#{3 -> (Set#{3} :: Set#{4, 2} :: Nil, Ok(1)), 1 -> (Set#{1, 5, 4, 2, 3} :: Set#{1, 2} :: Nil, Ok(2))})

@test
def eqCompose05(): Bool = Some(Map#{1 -> (Set#{1, 2, 3, 4, 5} :: Set#{2, 1} :: Nil, Ok(Set#{2, 3, 8})), 3 -> (Set#{3} :: Set#{4, 2} :: Nil, Ok(Set#{99, 4}))}) ==
                                Some(Map#{3 -> (Set#{3} :: Set#{4, 2} :: Nil, Ok(Set#{4, 99})), 1 -> (Set#{1, 5, 4, 2, 3} :: Set#{1, 2} :: Nil, Ok(Set#{8, 2, 3}))})

@test
def eqCompose06(): Bool = (Map#{1 -> 4, 5 -> 6}, Set#{1, 2, 3, 4}, Err(Set#{9, 18})) ==
                                (Map#{5 -> 6, 1 -> 4}, Set#{4, 2, 3, 1}, Err(Set#{18, 9}))
