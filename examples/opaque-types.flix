/// An opaque type declares a new type that is different from any other
/// existing type. Opaque types can be used to differentiate types that
/// would otherwise be the same. For example:

/// An opaque type for US dollars.
enum USD(Int32)

/// An opaque type for Canadian dollars.
enum CAD(Int32)

///
/// A function that adds two US dollar amounts.
///
def sum(x: USD, y: USD): USD =
  let USD(u) = x;
  let USD(v) = y;
  USD(u + v)

/// Adds two USD amounts.
def main(): Unit & Impure =
    let USD(result) = sum(USD(1), USD(5));
    println(result)
