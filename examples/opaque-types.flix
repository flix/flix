/// An opaque type declares a new type that is different from any other
/// existing type. Opaque types can be used to differentiate types
/// that would otherwise be the same. For example:

/// An opaque type for US dollars.
opaque type USD = Int

/// An opaque type for Canadian dollars.
opaque type CAD = Int

///
/// A function that adds two US dollar amounts.
/// Cannot be used to add USD and Canadian dollars.
///
def sum(x: USD, y: USD): USD =
  let USD(u) = x;
  let USD(v) = y;
  USD(u + v)

/// Adds two dollar amounts.
def main(_args: Array[String]): Int32 & Impure =
    let USD(result) = sum(USD(1), USD(5));
    println(result);
    0 // exit code
