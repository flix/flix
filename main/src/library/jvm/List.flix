import java.util.{List => JList}

///
/// JVM-only extensions for `List`.
///

instance ToJava[List[a]] {
    type Out = JList
    type Aef = IO
    pub def toJava(l: List[a]): JList \ IO = Adaptor.toList(l)
}

instance ToFlix[List[a]] {
    type In = JList
    pub def toFlix(l: JList): List[a] = Adaptor.fromList(l)
}

