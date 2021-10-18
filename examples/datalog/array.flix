// https://github.com/souffle-lang/souffle/tree/master/tests/example/array

def main(_args: Array[String]): Int32 & Impure =

    let values = #{
        Values(1, 13).
        Values(2, 21).
        Values(3, 34).
        Values(5, 55).
        Values(8, 89).
    };

    let parameters = #{
        Parameters(10).
    };

    let ddefault = #{Default(0).};

    let lp = #{

        // Added for Flix
        ValuesFirst(i) :- Values(i, _).
        ElementFirst(i) :- Element(i, _).
        ElementMinusOne(i-1, x) :- Element(i, x).
        ElementPlusOne(i+1, x) :- Element(i, x).

        // Original Program

        Indices(0).
        Indices(i+1) :- Indices(i), Parameters(l), if i >= 0 and i+1 < l.

        // The elements of the array, referenced by an index, and having a specified value. Note that if a value is not specified an element takes the default value.
        Element(i, d) :- Indices(i), not ValuesFirst(i), Default(d).
        Element(i, x) :- Indices(i), Values(i, x).

        // The value of the element to the left of the element specified by the index, or the default value if it is out of the array bounds.
        Left(i+1, d) :- not ElementFirst(i), ElementMinusOne(i, _), Default(d).
        Left(i, x) :- Element(im, x), Element(i, _), if im == i-1.

        // The value of the element to the right of the element specified by the index, or the default value if it is out of bounds.
        Right(i-1, d) :- not ElementFirst(i), ElementPlusOne(i, _), Default(d).
        Right(i, x) :- Element(ip, x), Element(i, _), if ip == i+1.

        // The local neighbourhood of a specified element, giving the value of that element, and the elements surrounding it.
        Neighbourhood(i, l, c, r) :- Left(i, l), Element(i, c), Right(i, r).
    };

    query lp <+> values <+> parameters <+> ddefault select (l, c, r) from Neighbourhood(1, l, c, r) |> println;

    0
