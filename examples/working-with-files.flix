// Working with files in Flix.
def main(_args: Array[String]): Int32 & Impure =

  let f = "README.md";

  // Check if the file 'README.md' exists.
  match File.exists(f) {
    case Ok(readmeExists) => println("The file ${f} exists: ${readmeExists}.")
    case Err(_)           => println("An error occurred.")
  };

  // Get statistics of the file 'README.md'.
  match File.stat(f) {
    case Ok(stats) => {
      println("The size of ${f} is: ${stats.size}.");
      println("The creation time of ${f} is: ${stats.creationTime}.")
    }
    case Err(_)    => println("An error occurred.")
  };

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
