//
// Welcome to the delta debugging tutorial!
//

rel A(x: Int)
rel B(x: Int)
rel C(x: Int)

A(1).
A(2).
A(3).
A(4).
A(5).
A(6).
A(7).
A(8).
A(9).
B(1).
B(2).
B(3).
B(4).
B(5).
B(6).
B(7).
B(8).
B(9).
C(1).
C(2).
C(3).
C(4).
C(5).
C(6).
C(7).
C(8).
C(9).

false :- A(7), B(1), C(3).
