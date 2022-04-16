def main(): Int32 & Impure =
    // Test an input word and print the output
    def trial(input) = {
        let res = input |>
            longestPalindromeSequence |>
            Option.map(match (b,e) -> String.slice(b, e+1, input)) |>
            Option.getWithDefault("nothing");
        println("> Longest palindrome sequence of ${input} is ${res}")
    };

    trial("ABBAIsCool");
    trial("Hello");
    trial("YaddaYaddaYadda");
    trial("abammabba");

    0

///
/// Finds one of the longest palindrome sequences of `s` and return the interval (both ends inclusive).
/// Returns `None` if and only if `s` is the empty string.
/// Example: longestPalindromeSequence("Hello") == Some(2,3)
/// Example: longestPalindromeSequence("abammabba") == Some(1, 6)
///
def longestPalindromeSequence(s: String): Option[(Int32, Int32)] & Impure = {
    use Option.flatMap;
    let length = String.length(s);
    // Check the char equality of `s` with index `i` and `j`.
    // Out of bounds returns false.
    let sameChar = i -> j -> {
        if (i >= 0 and j < length and i <= j)
            String.charAt(i, s) == String.charAt(j, s)
        else false
    };
    // Project the indices of the string into a predicate
    let indices = project Iterator.range(0, length) |> Iterator.toList into StringIndex;
    let p = #{
        // A single char is a palindrome of length 1
        LongestPalindrome(i, i; 1) :- StringIndex(i).
        // Two neighboring chars are a palindrome of length 2 if they are the same
        LongestPalindrome(i, i+1; 2) :- StringIndex(i), if sameChar(i, i+1).
        // If there is the same char on each side of a palindrome, that interval is a
        // palindrome with length two larger than the inner one
        LongestPalindrome(b-1, e+1; l+2) :- if b <= e and b-1 >= 0 and e+1 < length,
            if sameChar(b-1, e+1), LongestPalindrome(b, e; l).
    };
    let solution = solve p <+> indices;
    // Find the option of the maximum palindrome length
    let* maxLength = query solution select l from LongestPalindrome(_, _; l) |> Array.maximum;
    // Select the first interval with a maximum length
    query solution select (b, e) from LongestPalindrome(b,e;maxLength) |> Array.head
}
