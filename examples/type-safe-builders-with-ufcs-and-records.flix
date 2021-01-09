/// We can use uniform function call syntax and polymorphic extensible
/// records to simulate type-safe builder patterns in Flix.

/// Here main constructs a connection "object" with both required and
/// optional parameters in type-safe, statically-checked manner.
def main(_args: Array[String]): Int32 & Impure =
    connection()
        .host("localhost")  // required, omitting is a type error.
        .port(8080)         // required, omitting is a type error.
        .user("root")       // required, omitting is a type error.
        .pass("1234")       // optional, may be omitted.
        .connect() as & Impure;
    0 // exit code

/// Repeating any of the required options is a type-error.

/// We can implement such build patterns using polymorphic records.

/// The connection "builder" function returns a record with *all*
/// *optional* fields initialized to None.
def connection(): {pass: Option[String]} = {pass = None}

/// The host function extends the options record with a hostname.
def host[r](o: {| r}, h: String): {host: String | r} = {+host = h | o}

/// The port function extends the options record with a port number.
def port[r](o: {| r}, p: Int): {port: Int | r} = {+port = p | o}

/// The user function extends the options record with a username.
def user[r](o: {| r}, u: String): {user: String | r} = {+user = u | o}

/// The pass function *updates* the options record with a new value
/// of its pass field. Notice that the options record must already
/// have a pass field for this to type check, hence the connection
/// "builder" function ensures that such a field exists.
def pass[r](o: {pass: Option[String] | r}, p: String):
           {pass: Option[String] | r} = {pass = Some(p) | o}

/// Finally, the connect function has a very natural signature that
/// requires an options record with mandatory host, port, user,
/// and pass fields. The pass field, however, is of type Option,
/// and so may have the value None, if never set.
def connect(o: {host: String, port: Int, user: String, pass: Option[String]}):
    Unit = ()

/// NB: Code reformatted in non-idiomatic style to fit.
