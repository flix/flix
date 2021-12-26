def main(_args: Array[String]): Int & Impure =
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

    0

def longestPalindromeSequence(s: String): Option[(Int, Int)] & Impure = {
    use Option.flatMap;
    let length = String.length(s);
    let sameChar = i -> j -> {
        if (i >= 0 and j < length and i <= j)
            String.charAt(i, s) == String.charAt(j, s)
        else false
    };
    let facts = project Iterator.range(0, length) |> Iterator.toList into StringIndex;
    let p = #{
        LongestPalindrome(i, i; 1) :- StringIndex(i).
        LongestPalindrome(i, i+1; 2) :- StringIndex(i), if sameChar(i, i+1).
        LongestPalindrome(b-1, e+1; l+2) :- if b <= e and b-1 >= 0 and e+1 < length,
            if sameChar(b-1, e+1), LongestPalindrome(b, e; l).
    };
    let solution = solve p <+> facts;
    let* maxLength = query solution select l from LongestPalindrome(_, _; l) |> Array.maximum;
    query solution select (b, e) from LongestPalindrome(b,e;maxLength) |> Array.head
}
