namespace Company {

    rel Employee(name: Str, department: Str);
    Employee("Adam Smith", "Sales").
    Employee("Karl Marx", "Sales").
    Employee("John Maynard Keynes", "Administration").
    Employee("Vilfredo Pareto", "Operations").

    rel Intern(name: Str, department: Str, start: Int, end: Int)
    Intern("George Washington", "Sales", 1789, 1797).
    Intern("John Adams", "Sales", 1797, 1801).
    Intern("George W. Bush", "Sales", 2001, 2009).
    Intern("Barack Obama", "Sales", 2009, 2017).

    rel Department(name: Str, head: Str)
    Department("Sales", "Adam Smith").
    Department("Administration", "John Maynard Keynes").
    Department("Operations", "Vilfredo Pareto").

    rel Salary(employee: Str, salary: Int)
    Salary("Adam Smith", 100).
    Salary("Karl Marx", 0).
    Salary("John Maynard Keynes", 5000).
    Salary("Vilfredo Pareto", 400).
    Salary("George Washington", 50).
    Salary("John Adams", 40).
    Salary("George W. Bush", 40).
    Salary("Barack Obama", 40).

    rel Building(address: Str, department: Str)
    Building("123 Main St", "Sales").
    Building("1600 Pennsylvania Ave", "Administration").
    Building("321 1st St", "Operations").

    rel HeadOfDepartment(name: Str)
    HeadOfDepartment(x) :- Department(_, x).

    rel Worker(name: Str)
    Worker(x) :- Employee(x, _).
    Worker(x) :- Intern(x, _, _, _).

    rel DepartmentHeadSalary(name: Str, salary: Int)
    DepartmentHeadSalary(x, s) :- Department(_, x), Salary(x, s), x != s.

    rel WorkerAddress(name: Str, address: Str)
    WorkerAddress(name, address) :- Employee(name, dept), Building(address, dept).
    WorkerAddress(name, address) :- Intern(name, dept, _, _), Building(address, dept).

    assert Employee("Adam Smith")
    assert WorkerAddress("Adam Smith", "123 Main St").
    assert Error#(name) :- Intern(name, _, _, _), Worker(name, _).

    print WorkerAddress.

    Print#(x) :- Employee(x, _).
    Write#(x, "tmp.txt") :- Employee(x, _).
//    Error#(x) :- Employee(x, _), Read#(x, "tmp.txt").


}
