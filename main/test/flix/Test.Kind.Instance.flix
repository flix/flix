mod Test.Kind.Instance {

    mod TypeConstraint {
        pub enum EStar[_a: Type]
        pub enum EStarToStar[_a: Type -> Type]
        class CStar[a: Type]
        class CStar1[a: Type]
        class CStarToStar[a: Type -> Type]
        class CStarToStar2[a: Type -> Type]

        instance CStar1[EStar[a]] with CStar[a]

        instance CStar1[EStarToStar[a]] with CStarToStar[a]
    }

    @Test
    def empty(): Bool = true
}
