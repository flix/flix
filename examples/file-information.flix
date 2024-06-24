// Getting information on files with Flix.
def main(): Unit \ IO =
  let f = "README.md";

  // Check if the file 'README.md' exists.
  match Files.exists(f) {
    case Ok(exist) => {
      println("The file ${f} exists: ${exist}.")
    }
    case Err(msg)  => println("An error occurred with message: ${msg}")
  };

  // Get statistics of the file 'README.md'.
  match Files.stat(f) {
    case Ok(stats) => {
      println("${f} is of type: ${stats#fileType}");
      println("The size of ${f} is: ${stats#size}.");
      println("The creation time of ${f} is: ${stats#creationTime}.")
    }
    case Err(msg) => println("An error occurred with message: ${msg}")
  }
