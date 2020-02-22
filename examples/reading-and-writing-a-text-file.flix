/// An example of how to read and write a text file.

/// Returns a list of prominent public figures.
def getData(): List[String] =
    "Luke, Lucky" :: "Duck, Donald" :: Nil

/// Writes the list of prominent figures to a file.
def writeData(path: Path): Result[Unit, Path.IOError] & Impure =
    Path.writeLines(path, getData())

/// Reads the list of prominent figures back, returning surnames.
def readData(path: Path): Result[List[Str], Path.IOError] & Impure =
    let splitAndGetFirst = (s: String -> String.split(s, ",")[0]);
    let getSurname = (xs: List[Str]) -> List.map(splitAndGetFirst, xs);
    Result.map(getSurname, Path.readLines(path))

/// Writes the text file and then reads it back in again.
def main(): Result[List[String], Path.IOError] & Impure =
    let path = Path.new("members.txt");
    Result.flatMap(_ -> readData(path), writeData(path))
