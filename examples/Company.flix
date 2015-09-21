namespace Company {
    rel Employees(name: String, department: String)
    rel Interns(name: String, department: String, start: Int, end: Int)
    rel Departments(name: String, head: String)
    rel Salaries(employee: String, salary: Int)
    rel Buildings(address: String, department: String)

    Employees("Adam Smith", "Sales").
    Employees("Karl Marx", "Sales").
    Employees("John Maynard Keynes", "Administration").
    Employees("Vilfredo Pareto", "Operations").

    Interns("George Washington", "Sales", 1789, 1797).
    Interns("John Adams", "Sales", 1797, 1801).
    Interns("George W. Bush", "Sales", 2001, 2009).
    Interns("Barack Obama", "Sales", 2009, 2017).

    Departments("Sales", "Adam Smith").
    Departments("Administration", "John Maynard Keynes").
    Departments("Operations", "Vilfredo Pareto").

    Salaries("Adam Smith", 100).
    Salaries("Karl Marx", 0).
    Salaries("John Maynard Keynes", 5000).
    Salaries("Vilfredo Pareto", 400).
    Salaries("George Washington", 50).
    Salaries("John Adams", 40).
    Salaries("George W. Bush", 40).
    Salaries("Barack Obama", 40).

    Buildings("123 Main St", "Sales").
    Buildings("1600 Pennsylvania Ave", "Administration").
    Buildings("321 1st St", "Operations").

    rel HeadOfDepartment(name: String)
    HeadOfDepartment(x) :- Departments(_, x).

    rel Workers(name: String)
    Workers(x) :- Employees(x, _).
    Workers(x) :- Interns(x, _, _, _).

    rel DepartmentHeadSalaries(name: String, salary: Int)
    DepartmentHeadSalaries(x, s) :- Departments(_, x), Salaries(x, s).

    rel WorkerAddresses(name: String, address: String)
    WorkerAddresses(name, address) :- Employees(name, dept), Buildings(address, dept).
    WorkerAddresses(name, address) :- Interns(name, dept, _, _), Buildings(address, dept).

    //TODO(Magnus)
    //false :- Intern(name, _, _, _), Worker(name, _).
    //If Adam Smith is an employee, he must be at 123 Main St.
    //WorkerAddresses("Adam Smith", "123 Main St") :- Employees("Adam Smith", _)
}