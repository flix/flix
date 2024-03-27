///
/// Converts an integer to its binary representation.
///
/// x is the integer to convert.
///
/// Returns the binary representation of the input integer.
///
def toBinaryString(x: Int32): String =
    import static java.lang.Integer.toBinaryString(Int32): ##java.lang.String \ {};
    toBinaryString(x)

/// Splits a seed in two halves.
///
/// `seed` The seed to split, must be Int32.
///
/// Returns a tuple with the two halves of the seed.
def splitSeed1(seed: Int32): (Int32, Int32) =
    let s = toBinaryString(seed);
    let len = String.length(s);
    let (s1, s2) = String.splitAt(len / 2, s);

    // Pad with 0s to get original length of seed
    // TODO consider the padding, maybe 1 for s11, or maybe XOR one half?
    // TODO The first seed will always be equal :(
    let s11 = String.padRight(len/2, '0', s1);
    let s22 = String.padLeft(len/2, '0', s2);
    (parseSeed(s11), parseSeed(s22))

/// Generate an additional seed from a given seed.
///
/// `seed` The seed to split, must be Int32.
///
/// Returns a tuple with the original seed and the new seed.
def genSeedsFromSeed(seed: Int32): (Int32, Int32) =
    let s = toBinaryString(seed);
    let len = String.length(s);
    let (s1, s2) = String.splitAt(len / 2, s);
    let new_bin_seed = s2+s1;
    (seed, parseSeed(new_bin_seed))

/// Parses a seed string and returns an integer value.
///
/// `seed` The seed string to parse.
///
/// Returns: The parsed integer value if successful.
///
/// This function will panic if the seed string cannot be parsed as an integer.
def parseSeed(seed: String): Int32 =
    match Int32.parse(2, seed) {
        case Ok(value) => value,
        case Err(_) => unreachable!()
    }

def splitSeed(r: Rand): (Rand, Rand) =
    // let goldenGamma16 = 40503; // true 16 bit golden gamma value. Its uneven.
    let goldenGamma = 265443576; //Removed last digit from the golden ratio number. Even.
    let r1 = r + goldenGamma;
    let r2 = r1 + goldenGamma;
    (r1, r2)