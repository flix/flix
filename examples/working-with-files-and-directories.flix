// Working with files and directories in Flix.
def main(): Unit \ IO =
  let f = "README.md";
  let dir = "src";

  // Read the file `README.md`.
  match Files.readLines(f) {
    case Ok(x :: _) => println("The first line of ${f} is: '${x}'.")
    case Ok(Nil)    => println("the file ${f} is empty.")
    case Err(msg)   => println("An error occurred with message: ${msg}")
  };

  // List the files in `src`.
  match Files.list(dir) {
    case Ok(subpaths) => {
      println("All files or directories in ${dir} is: '${subpaths}'.")
    }
    case Err(msg)     => println("An error occurred with message: ${msg}")
  }
