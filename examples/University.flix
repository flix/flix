namespace University {

    enum Sex {
        case Male, case Female
    }

    rel Student(name: String, address: String, age: Int, sex: Sex);

    Student("Victor Garcia", "568 Whitman Court", 20, Sex.Male);
    Student("Priscilla T. Reed", "362 Oliverio Drive", 19, Sex.Male);
    Student("Ronald K. Rodriguez", "4245 Brooke Street", 23, Sex.Male);
    Student("David J. Richard", "2565 Willison Street", 18, Sex.Male);
    Student("Edgar K. Clark", "42 Bay Street", 17, Sex.Male);
    Student("Tanya J. Rivera", "1517 Lynden Road", 18, Sex.Female);
    Student("Ellen L. Matherne", "4254 Victoria Park Ave", 22, Sex.Female);
    Student("Betty J. Harris", "3437 Boulevard Lafleche", 21, Sex.Female);
    Student("Ilse G. Hurst", "1606 Adelaide St", 22, Sex.Female);
    Student("Anthony I. Killion", "692 Cardero St", 17, Sex.Male);

    rel Faculty(name: String, address: String);

    rel Staff(name: String, address: String);

    // TODO: Classes, Salary, Rooms, Exams, Grades, Publications, Grants, Department,




}