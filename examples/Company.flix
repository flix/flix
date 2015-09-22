namespace Company {
    rel Employee(name: String, department: String)
    rel Intern(name: String, department: String, start: Int, end: Int)
    rel Department(name: String, head: String)
    rel Salary(employee: String, salary: Int)
    rel Building(address: String, department: String)

    Employee("Adam Smith", "Sales").
    Employee("Karl Marx", "Sales").
    Employee("John Maynard Keynes", "Administration").
    Employee("Vilfredo Pareto", "Operations").

    Intern("George Washington", "Sales", 1789, 1797).
    Intern("John Adams", "Sales", 1797, 1801).
    Intern("George W. Bush", "Sales", 2001, 2009).
    Intern("Barack Obama", "Sales", 2009, 2017).

    Department("Sales", "Adam Smith").
    Department("Administration", "John Maynard Keynes").
    Department("Operations", "Vilfredo Pareto").

    Salary("Adam Smith", 100).
    Salary("Karl Marx", 0).
    Salary("John Maynard Keynes", 5000).
    Salary("Vilfredo Pareto", 400).
    Salary("George Washington", 50).
    Salary("John Adams", 40).
    Salary("George W. Bush", 40).
    Salary("Barack Obama", 40).

    Building("123 Main St", "Sales").
    Building("1600 Pennsylvania Ave", "Administration").
    Building("321 1st St", "Operations").

    rel HeadOfDepartment(name: String)
    HeadOfDepartment(x) :- Department(_, x).

    rel Worker(name: String)
    Worker(x) :- Employee(x, _).
    Worker(x) :- Intern(x, _, _, _).

    rel DepartmentHeadSalary(name: String, salary: Int)
    DepartmentHeadSalary(x, s) :- Department(_, x), Salary(x, s).

    rel WorkerAddress(name: String, address: String)
    WorkerAddress(name, address) :- Employee(name, dept), Building(address, dept).
    WorkerAddress(name, address) :- Intern(name, dept, _, _), Building(address, dept).

    //TODO(Magnus)
    //false :- Intern(name, _, _, _), Worker(name, _).
    //If Adam Smith is an employee, he must be at 123 Main St.
    //WorkerAddress("Adam Smith", "123 Main St") :- Employee("Adam Smith", _)
}