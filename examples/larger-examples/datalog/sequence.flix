// https://github.com/souffle-lang/benchmarks/tree/master/benchmarks/sequences

def main(): Unit \ IO =

    let numLetters = #{ NumLetters(20). };

    let lp = #{

        ////////////// Arithmetic ////////////////

        N(0).
        N(x + 1) :- N(x), NumLetters(n), if x < n.

        S(x, x+1) :- N(x).

        Add(x, 0, x) :- N(x).
        Add(x, y, r) :- S(py,y), Add(x, py, pr), S(pr, r).

        Mul(x,0,0) :- N(x).
        Mul(x, y, r) :- S(py, y), Mul(x, py, pr), Add(pr, x, r).

        Exp(x,0,1) :- N(x).
        Exp(x, y, r) :- S(py, y), Exp(x, py, pr), Mul(pr, x, r).

        Log(x, b, r) :- Exp(b, r, y), S(r, sr), Exp(b, sr, y2), N(x), if y <= x and x < y2.

        Div(x, y, r) :- Mul(r, y, a), Add(a, z, x), if 0 <= z and z < y.

        Mod(x, y, r) :- Mul(y, _, z), Add(z, r, x), if 0 <= r and r < y.

        ////////////// Trie ////////////////

        TrieLetter(i, r) :- S(x, i), NumLetters(n), Mod(x, n, r).

        TrieLevelEnd(0, 0).
        TrieLevelEnd(l, i) :- NumLetters(n), S(pl, l), TrieLevelEnd(pl, b), Exp(n, l, p), Add(b, p, i).

        TrieLevelStart(0, 0).
        TrieLevelStart(l, i) :- S(pl, l), TrieLevelEnd(pl, b), Add(b, 1, i).

        TrieLevel(0, 0).
        TrieLevel(i, b) :- N(i), S(a, b), TrieLevelEnd(a, low), TrieLevelEnd(b, high), if low < i and i <= high.

        TrieParent(i, p) :- NumLetters(n), TrieLevel(i, l), S(pl, l), TrieLevelStart(l, b), Add(b, x, i), Div(x, n, o), TrieLevelStart(pl, c), Add(c, o, p).

        TrieRoot(0).

        Trie(x) :- TrieLetter(x,_).

        ////////////// all Strings over the alphabet ////////////////

        Str(x) :- Trie(x).

        StrLen(id, l) :- TrieLevel(id, l).

        StrChain(id, id) :- Trie(id).
        StrChain(id, p) :- StrChain(id, x), TrieParent(x, p).

        StrLetterAt(id, pos, l) :- StrChain(id, p), TrieLevel(p, pos), TrieLetter(p, l).

        ////////////// example sequences ////////////////////

        // search all Palindrome
        PalinAux(s, x, x) :- Str(s), N(x), StrLen(s, l), if x <= l.
        PalinAux(s, x, x+1) :- StrLetterAt(s, x, _).
        PalinAux(s, x, sy) :- StrLetterAt(s, x, a), S(x, sx), PalinAux(s, sx, y), StrLetterAt(s, y, a), S(y, sy).

        Palindrome(s) :- StrLen(s, 0).
        Palindrome(s) :- PalinAux(s, 1, sl), StrLen(s, l), S(l, sl).

        DebugStr(18).

        Read(x,y) :- DebugStr(s), StrLetterAt(s,x,y).

    };
    query lp <+> numLetters select (x, y) from Read(x, y) |> println
