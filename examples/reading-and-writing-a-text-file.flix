/// An example of how to read and write a text file.

/// Returns a list of some prominent figures.
def getData(): List[Str] =
    "Luke, Lucky" :: "Duck, Donald" :: Nil

/// Writes the data to the given file.
def writeData(path: Path): Result[Unit, Path.IOError] =
    Path.writeLines(path, getData())

/// Reads the data back, but only returning last names.
def readData(path: Path): Result[List[Str], Path.IOError] =
    let splitAndGetFirst = (s: Str -> String.split(s, ",")[0]);
    let getLastName = (xs: List[Str]) -> List.map(splitAndGetFirst, xs);
    Result.map(getLastName, Path.readLines(path))

/// Writes the file, and then reads it back in.
def main(): Result[List[Str], Path.IOError] =
    let path = Path.new("members.txt");
    Result.flatMap(_ -> readData(path), writeData(path))
