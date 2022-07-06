///
/// Flix supports writing code in an imperative
/// style if it is preferable to you.
///
/// foreach-loops are great for working with
/// mutable data structures.
///
/// This example implements copying the characters
/// of a string into an array.
///
def toArray(r: Region[r], s: String): Array[Char, r] \ Write(r) =
    region r2 { // Declare a new local region

        // Create an output array
        let len = String.length(s);
        let out = Array.new(r, 'a', len);

        ///
        /// Here we enumerate the chararacters
        /// along with their indices and store
        /// the ith element in `s` at the ith
        /// position in the array `out`.
        ///
        foreach ((x, i) <- String.enumerator(r2, s))
            out[i] = x;

        out // Return value
    }

def main(): Unit & Impure =
    let s = "Flix is cool!";
    let a = toArray(Static, s);
    println(a)
