import java.io.BufferedWriter
import java.io.FileWriter

def main(): Unit \ IO =
    let lines = List#{"Foo", "Bar", "Baz"};
    let writer = new BufferedWriter(new FileWriter("foo.txt"));
    lines |> List.forEach(line -> { writer.write(line); writer.newLine()});
    writer.close()
