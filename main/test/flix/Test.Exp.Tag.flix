namespace Test/Exp/Tag {

    enum A {
        case OfUnit(Unit)
//        case OfBool(Bool)
//        case OfChar(Char)
//        case OfFloat32(Float32)
//        case OfFloat64(Float64)
//        case OfInt8(Int8)
    }

    @test
    def testTag01(): A = OfUnit(())

    @test
    def testTag02(): A = A.OfUnit(())

    @test
    def testTag03(): A = Test/Exp/Tag/A.OfUnit(())



//  /**
//    * Represents the Int16 type.
//    */
//  val Int16: Type = Type.Cst(TypeConstructor.Int16)
//
//  /**
//    * Represents the Int32 type.
//    */
//  val Int32: Type = Type.Cst(TypeConstructor.Int32)
//
//  /**
//    * Represents the Int64 type.
//    */
//  val Int64: Type = Type.Cst(TypeConstructor.Int64)
//
//  /**
//    * Represents the BigInt type.
//    */
//  val BigInt: Type = Type.Cst(TypeConstructor.BigInt)
//
//  /**
//    * Represents the String type.
//    */
//  val Str: Type = Type.Cst(TypeConstructor.Str)

//
//  /**
//    * A type constructor that represent the type of arbitrary-precision integers.
//    */
//  case object BigInt extends TypeConstructor {
//    def kind: Kind = Kind.Star
//  }
//
//  /**
//    * A type constructor that represent the type of strings.
//    */
//  case object Str extends TypeConstructor {
//    def kind: Kind = Kind.Star
//  }
//
//  /**
//    * A type constructor that represent the type of arrays.
//    */
//  case object Array extends TypeConstructor {
//    /**
//      * The shape of an array is Array[t].
//      */
//    def kind: Kind = Kind.Star -> Kind.Star
//  }
//
//  /**
//    * A type constructor that represent the type of channels.
//    */
//  case object Channel extends TypeConstructor {
//    /**
//      * The shape of a channel is Channel[t].
//      */
//    def kind: Kind = Kind.Star -> Kind.Star
//  }
//
//  /**
//    * A type constructor that represent the type of enums.
//    */
//  case class Enum(sym: Symbol.EnumSym, kind: Kind) extends TypeConstructor
//
//  /**
//    * A type constructor that represent the type of JVM classes.
//    */
//  case class Native(clazz: Class[_]) extends TypeConstructor {
//    def kind: Kind = Kind.Star
//  }
//
//  /**
//    * A type constructor that represent the type of references.
//    */
//  case object Ref extends TypeConstructor {
//    /**
//      * The shape of a reference is Ref[t].
//      */
//    def kind: Kind = Kind.Star -> Kind.Star
//  }
//
//  /**
//    * A type constructor that represent the type of tuples.
//    */
//  case class Tuple(l: Int) extends TypeConstructor {
//    /**
//      * The shape of a tuple is (t1, ..., tn).
//      */
//    def kind: Kind = ??? // TODO
//  }
//
//  /**
//    * A type constructor that represent the type of vectors.
//    */
//  case object Vector extends TypeConstructor {
//    /**
//      * The shape of a vector is Array[t;n].
//      */
//    def kind: Kind = (Kind.Star -> Kind.Nat) -> Kind.Star
//  }

}
