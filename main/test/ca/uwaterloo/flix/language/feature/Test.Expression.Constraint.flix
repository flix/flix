rel Person(fstName: Str, lstName: Str, age: Int)
rel Senior(fstName: Str, lstName: Str)
rel Wizard(fstName: Str, lstName: Str)
rel SeniorWizard(lstName: Str, age: Int)

@test
def testConstraint01(): Schema { Person, Senior, Wizard, SeniorWizard } =
    Person("Luke", "Skywalker", 21).

@test
def testConstraint02(): Schema { Person, Senior, Wizard, SeniorWizard } =
    Person("Lucky", "Luke", 42).

@test
def testConstraint03(): Schema { Person, Senior, Wizard, SeniorWizard } =
    Person("Luke", "Skywalker", 21).
    Person("Lucky", "Luke", 42).
    Person("Gandalf", "The Grey", 84).
    Senior("Gandalf", "The Grey").
    Wizard("Gandalf", "The Grey").

@test
def testConstraint04(): Schema { Person, Senior, Wizard, SeniorWizard } =
    Person("Luke", "Skywalker", 21).
    Person("Lucky", "Luke", 42).
    Person("Gandalf", "The Grey", 84).
    Senior("Gandalf", "The Grey").
    Wizard("Gandalf", "The Grey").
    SeniorWizard(lstName, age) :- Person(fstName, lstName, age), Senior(fstName, lstName), Wizard(fstName, lstName).
