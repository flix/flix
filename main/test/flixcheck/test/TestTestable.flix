use Testable.{Testable, TFunc, TFunc2, forAll, forAll2}
use Gen.{chooseInt, Arbitrary}
// use Testable.{TestableFunc => ts} // TODO : spørg Magnus
pub def testingMain(): Unit \ IO =
    let f = TFunc.TFunc(x -> x < 100);
    let prop = Testable.property(f);
    println("Verbose check");
    let c = {size = _ -> 99| verbose()};
    check(c, prop)

pub def shouldFail(): Unit \ IO =
    // Define a testable function that should fail
    let f = TFunc.TFunc(x -> Int32.pow(x, 2) < 80);
    // Set the function as a property
    let prop = Testable.property(f);
    println("Verbose check");
    // Check the property
    verboseCheck(prop)

pub def testReverse(): Unit \ IO =
    def prop_revrev(xs: List[Char]) = List.reverse(List.reverse(xs)) == xs;
    def prop_rev(x: Int32) = (x :: Nil) == List.reverse(x :: Nil);
    def prop_RevApp(pair: (List[Int32], List[Int32])) = List.append(List.reverse(fst(pair)), List.reverse(snd(pair))) == List.reverse(List.append(snd(pair), fst(pair)));
    let f1 = TFunc.TFunc(prop_rev);
    let f2 = TFunc.TFunc(prop_RevApp);
    let f3 = TFunc.TFunc(prop_revrev);
    let propList = Testable.property(f1) :: Testable.property(f2) :: Testable.property(f3) :: Nil;
    foreach (prop <- propList) {
        verboseCheck(prop)
    }

pub def testTranspose(): Unit \ IO =
    def prop_transpose(xs: List[List[Int32]]) = List.transpose(List.transpose(xs)) == xs;
    let f = TFunc.TFunc(prop_transpose);
    verboseCheck(f)     
pub def testPos(): Unit \ IO =
    let prop = forAll(chooseInt(-5, 100), x -> x*2 >= 0);
    checkWithSeed(default(), prop, 12345i64)

pub def testtt(): Unit \ IO =
    def prop_add(x: Int32, y: Int32) = x + y == y + x;
    let f = TFunc2.TFunc2(prop_add);
    flixCheck(f)

pub def newtest(): Unit \ IO =
    let prob_true = true;
    flixCheck(prob_true)