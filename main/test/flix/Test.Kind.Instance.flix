mod Test.Kind.Instance {

    mod TypeConstraint {
        pub enum EStar[_a: Type](Unit)
        pub enum EStarToStar[_a: Type -> Type](Unit)
        trait CStar[a: Type]
        trait CStar1[a: Type]
        trait CStarToStar[a: Type -> Type]
        trait CStarToStar2[a: Type -> Type]

        instance CStar1[EStar[a]] with CStar[a]

        instance CStar1[EStarToStar[a]] with CStarToStar[a]
    }

    @test
    def empty(): Bool = true
}
