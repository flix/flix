// Predeclares intermediate `Test.*` namespaces so that test files which
// declare deeper paths (e.g. `mod Test.Foo.Bar`) can still resolve qualified
// uses through the namespace chain. Concretely, the resolver looks up the
// first component of a qualified name as an explicit `Mod` symbol — without
// this file, references like `use Test.Foo.Bar.X` from inside another module
// would not resolve because no test file declares a top-level `mod Test {}`.
//
// Each module must have exactly one declaration site in the program, so this
// file may only declare paths that no individual test file declares with
// content. Paths that some test file already declares with content are
// intentionally absent here.
mod Test {}
mod Test.Assoc {}
mod Test.Assoc.Eff {}
mod Test.Comment {}
mod Test.Dec {}
mod Test.Dec.Assoc {}
mod Test.Dec.Assoc.Type {}
mod Test.Dec.Enum {}
mod Test.Enum {}
mod Test.Eff {}
mod Test.Eff.Poly {}
mod Test.Equality {}
mod Test.Def {}
mod Test.Def.ChooseStar {}
mod Test.DefaultHandler {}
mod Test.Derives {}
mod Test.Exp {}
mod Test.Exp.Apply {}
mod Test.Exp.Binary {}
mod Test.Exp.Chained {}
mod Test.Exp.Choose {}
mod Test.Exp.ChooseStar {}
mod Test.Exp.Concurrency {}
mod Test.Exp.Fixpoint {}
mod Test.Exp.Fixpoint.Solve {}
mod Test.Exp.Jvm {}
mod Test.Exp.Jvm.Generic {}
mod Test.Exp.Jvm.Generic.InvokeConstructor {}
mod Test.Exp.Jvm.Generic.InvokeMethod {}
mod Test.Exp.Jvm.Generic.InvokeStaticMethod {}
mod Test.Exp.Jvm.Generic.NewObject {}
mod Test.Exp.Jvm.InvokeConstructor {}
mod Test.Exp.Jvm.InvokeMethod {}
mod Test.Exp.Jvm.InvokeMethod.Overload {}
mod Test.Exp.Jvm.InvokeMethod2 {}
mod Test.Exp.Jvm.InvokeMethod2.Overload {}
mod Test.Exp.Jvm.InvokeStaticMethod {}
mod Test.Exp.Jvm.NewObject {}
mod Test.Exp.Lambda {}
mod Test.Exp.Let {}
mod Test.Exp.Match {}
mod Test.Exp.Record {}
mod Test.Exp.Struct {}
mod Test.Exp.Tag {}
mod Test.Exp.Throw {}
mod Test.Exp.Unary {}
mod Test.Handler {}
mod Test.Handler.Ask {}
mod Test.Handler.Gen {}
mod Test.Handler.MultiShot {}
mod Test.Handler.OneShot {}
mod Test.Handler.ZeroShot {}
mod Test.Import {}
mod Test.Instance {}
mod Test.Instance.Eq {}
mod Test.Instance.Eq.Constraint {}
mod Test.Integ {}
mod Test.Integ.Enum {}
mod Test.Integ.Fixpoint {}
mod Test.Integ.Trait {}
mod Test.Java {}
mod Test.Kind {}
mod Test.Lit {}
mod Test.Predicate {}
mod Test.Term {}
mod Test.Term.Var {}
mod Test.Type {}
mod Test.Unused {}
mod Test.Use {}
