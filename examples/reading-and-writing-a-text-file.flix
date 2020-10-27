/// An example of how to read and write a text file.
use Core/Io/IOError.IOError;
use Core/Io/File.{File, new => newFile, readLines, writeLines};

/// Returns a list of prominent public figures.
def getData(): List[String] =
    "Luke, Lucky" :: "Duck, Donald" :: Nil

/// Writes the list of prominent figures to a file.
def writeData(file: File): Result[Unit, IOError] & Impure =
    writeLines(file, getData())

/// Reads the list of prominent figures back, returning surnames.
def readData(file: File): Result[List[String], IOError] & Impure =
    let splitAndGetFirst = (s: String -> String.split(s, ",")[0]);
    let getSurname = (xs: List[String]) -> List.map(splitAndGetFirst, xs);
    Result.map(getSurname, readLines(file))

/// Writes the text file and then reads it back in again.
def main(): Result[List[String], IOError] & Impure =
    let path = newFile("members.txt");
    Result.flatMap(_ -> readData(path), writeData(path))
