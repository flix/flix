//
// Welcome to the delta debugging tutorial!
//

rel A(x: Int)

A(1).
A(2).
A(3).
A(4).
A(5).
A(6).
A(7).
A(8).
A(9).

true :- A(3), A(7).

false :- A(3), A(7).
