use Fs.FileSystem

/// Uses `withChroot` to restrict all filesystem operations to a given directory subtree.
/// Operations outside the chroot fail with a `PermissionDenied` error.
def main(): Unit \ { FileSystem, IO } =
    match FileSystem.mkDirs("/tmp/flix-chroot") {
        case Err(err) => println("Setup error: ${err}")
        case Ok(_) =>
            run {
                match FileSystem.write(str = "Hello", "/tmp/flix-chroot/data.txt") {
                    case Ok(_)    => println("Write inside chroot succeeded")
                    case Err(err) => println("Error: ${err}")
                };
                match FileSystem.read("/etc/hostname") {
                    case Ok(_)    => println("Unexpected: read outside chroot succeeded")
                    case Err(err) => println("Read outside chroot blocked: ${err}")
                }
            } with FileSystem.withChroot("/tmp/flix-chroot")
    }
