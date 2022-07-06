///
/// Flix supports writing code in an imperative
/// style if it is preferable to you.
///
/// foreach-loops are great for working with
/// mutable data structures.
///
/// This example implements copying an array
/// in an imperative style.
///
def copy!(in: { in :: Array[a, r1] }, out: { out :: Array[a, r2] }): Unit \ { Read(r1), Write(r2) } =
    region r { // Declare a new region

        /// Here we enumerate over the elements
        /// in `in` along with the indices of the
        /// elements.
        /// We add some guards to our loop
        /// so we avoid out of bounds errors.
        foreach((x, i) <- Array.enumerator(r, in.in);
                if i < out.out.length; if not Array.isEmpty(in.in)) {
                    (out.out)[i] = x
        }
    }

def main(): Unit & Impure = region r {
    let a = Array.range(r, 0, 100); // Create a new array
    let b = Array.new(r, 0, 100); // Create an empty array
    println("b has all zeros? ${Array.forall(x -> x == 0, b)}");
    copy!(in = a, out = b);
    println("a and b has same elements now? ${a `Array.sameElements` b}")
}
