import java.nio.file.Path

def main(): Unit \ IO =
    // Here is how to call a varargs method extra args:
    let p1 = Path.of("Documents", ...{"Images", "me.jpg"});

    // And here is how to call a varargs method with no extra args:
    let p2 = Path.of("Documents", (Vector.empty(): Vector[String]));

    // We print both paths to see that the args were passed correctly.
    println(p1.toString());
    println(p2.toString())
