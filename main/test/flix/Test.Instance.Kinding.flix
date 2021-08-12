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
enum EBool[a: Bool]
enum ERecord[a: Record]
enum ESchema[a: Schema]
rel R(i: Int)

namespace Test/Instance/Kinding {

    namespace TypeConstraint {
        enum EStar[a: Type]
        class CStarToStar[a: Type -> Type]
        class CStar1[a: Type]

        instance CStar1[EStar[a]] with CStar[a]

        instance CStar1[EStarToStar[a]] with CStarToStar[a]
    }

    @test
    def empty(): Bool = true
}
