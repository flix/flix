// Getting information on files with Flix.
def main(_args: Array[String]): Int32 & Impure =
  let f = "README.md";

  // Check if the file 'README.md' exists.
  match File.exists(f) {
    case Ok(readmeExists) => {
      println("The file ${f} exists: ${readmeExists}.")
    }
    case Err(msg)         => {
      println("An error occurred. This is the message: ${msg}")
    }
  };

  // Get statistics of the file 'README.md'.
  match File.stat(f) {
    case Ok(stats) => {
      println("${f} is of type: ${stats.fileType}");
      println("The size of ${f} is: ${stats.size}.");
      println("The creation time of ${f} is: ${stats.creationTime}.")
    }
    case Err(_)    => println("An error occurred.")
  };

  0 // exit code
