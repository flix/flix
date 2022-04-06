// Working with files and directories in Flix.
def main(_args: Array[String]): Int32 & Impure =
  let f = "README.md";

  // Read the file `README.md`.
  match File.readLines(f) {
    case Ok(x :: _) => println("The first line of ${f} is: '${x}'.")
    case _          => println("An error occurred or file is empty.")
  };

  let dir = "src";

  // List the files in `src`.
  match File.list(dir) {
    case Ok(subpaths) => println("All files or directories in ${dir} is: '${subpaths}'.")
    case _            => println("An error occurred or ${dir} is empty.")
  };

  0 // exit code
