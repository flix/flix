def main(): Unit \ IO =
    // Test an input word and print the output
    def trial(input) = {
        let res = input |>
            longestPalindromeSequence |>
            Option.map(match (b,e) -> String.slice(start = b, end = e+1, input)) |>
            Option.getWithDefault("nothing");
        println("> Longest palindrome sequence of ${input} is ${res}")
    };

    trial("ABBAIsCool");
    trial("Hello");
    trial("YaddaYaddaYadda");
    trial("abammabba")

///
/// Finds one of the longest palindrome sequences of `s` and return the interval (both ends inclusive).
/// Returns `None` if and only if `s` is the empty string.
/// Example: longestPalindromeSequence("Hello") == Some(2,3)
/// Example: longestPalindromeSequence("abammabba") == Some(1, 6)
///
def longestPalindromeSequence(s: String): Option[(Int32, Int32)] = {
    let length = String.length(s);
    // Check the char equality of `s` with index `i` and `j`.
    // Out of bounds returns false.
    let sameChar = i -> j -> {
        if (i >= 0 and j < length and i <= j)
            String.charAt(i, s) == String.charAt(j, s)
        else false
    };
    // Inject the indices of the string into a predicate
    let indices = inject List.range(0, length) into StringIndex;
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
    forM (
        // Find the option of the maximum palindrome length
        maxLength <- query solution select l from LongestPalindrome(_, _; l) |> List.maximum;

        // Select the first interval with a maximum length
        res <- query solution select (b, e) from LongestPalindrome(b,e;maxLength) |> List.head
    ) yield res

}
