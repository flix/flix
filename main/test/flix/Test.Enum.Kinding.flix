// MATT move to better namespace
class CStarToStar[a: Type -> Type]
class CBoolToStar[a: Bool -> Type]
class CRecordToStar[a: Record -> Type]
class CSchemaToStar[a: Schema -> Type]
class CStar[a: Type]
class CBool[a: Bool]
class CRecord[a: Record]
class CSchema[a: Schema]
enum EStarToStar[a: Type -> Type]
enum EBoolToStar[a: Bool -> Type]
enum ERecordToStar[a: Record -> Type]
enum ESchemaToStar[a: Schema -> Type]
enum EStar[a: Type]
enum EBool[a: Bool]
enum ERecord[a: Record]
enum ESchema[a: Schema]
rel R(i: Int)

namespace Test/Enum/Kinding {
    namespace Implicit {
        enum EStar[a] {
            case CStar1(a)
            case CStar2(a -> a)
        }

        enum EStarStar[a, b] {
            case CStarStar1(a)
            case CStarStar2(b)
            case CStarStar3(a -> b)
        }
    }

    namespace Explicit {
        enum EStar[a: Type] {
            case CStar1(a)
            case CStar2(a -> a)
        }

        enum EStarStar[a: Type, b: Type] {
            case CStarStar1(a)
            case CStarStar2(b)
            case CStarStar3(a -> b)
        }

        enum EBool[a: Bool] {
            case CBool1(Int -> Int & a)
        }

        enum ERecord[a: Record] {
            case CRecord1(a)
            case CRecord2({ l: Int | a })
        }
        // These pass kinding but cause a crash in the Finalizer.
//        enum ESchema[a: Schema] {
//            case CSchema1(a)
//            case CSchema2(#{ R | a })
//        }
//
//        enum EStarToStar[a: Type -> Type] {
//            case CStarToStar1(a[Int])
//        }
//
//        enum EBoolToStar[a: Bool -> Type] {
//            case CBoolToStar1(a[Pure])
//        }
//
//        enum ERecordToStar[a: Record -> Type] {
//            case CRecordToStar1(a[{}])
//        }
//
//        enum ESchemaToStar[a: Schema -> Type] {
//            case CSchemaToStar1(a[#{}])
//        }
    }

    @test
    def empty(): Bool = true
}