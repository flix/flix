use Fs.FileSystem
use Time.Clock

/// Uses `withInMemoryFS` to perform all filesystem operations in memory with no real I/O.
/// Demonstrates write, read, list, and delete — all without touching the disk.
def main(): Unit \ { Clock, IO } =
    run {
        let result = forM (
            _       <- FileSystem.mkDirs("/data");
            _       <- FileSystem.write(str = "Hello", "/data/hello.txt");
            _       <- FileSystem.write(str = "World", "/data/world.txt");
            entries <- FileSystem.list("/data");
            content <- FileSystem.read("/data/hello.txt");
            _       <- FileSystem.delete("/data/hello.txt");
            exists  <- FileSystem.exists("/data/hello.txt")
        ) yield (entries, content, exists);
        match result {
            case Err(err) => println("Error: ${err}")
            case Ok((entries, content, exists)) =>
                println("Files in /data:");
                foreach (entry <- entries) {
                    println("  ${entry}")
                };
                println("Content: ${content}");
                println("Exists after delete: ${exists}")
        }
    } with FileSystem.withInMemoryFS
