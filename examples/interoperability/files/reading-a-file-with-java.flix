import java.io.BufferedReader
import java.io.FileReader

def readAllLines(reader: BufferedReader): List[String] \ IO =
    let line = reader.readLine();
    if (Object.isNull(line))
        Nil
    else
        line :: readAllLines(reader)

def main(): Unit \ IO =
    let reader = new BufferedReader(new FileReader("foo.txt"));
    let lines = readAllLines(reader);
    reader.close();
    println(lines)
