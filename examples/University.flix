namespace University {

    enum Sex {
        case Male,
        case Female
    }

    enum AcademicRank {
        case PostDoc,
        case AssistantProf,
        case AssociateProf,
        case FullProf
    }

    rel Student(name: String, address: String, age: Int, sex: Sex);
    Student("Victor K. Garcia", "568 Whitman Court", 20, Sex.Male);
    Student("Priscilla T. Reed", "362 Oliverio Drive", 19, Sex.Male);
    Student("Ronald K. Rodriguez", "4245 Brooke Street", 23, Sex.Male);
    Student("David J. Richard", "2565 Willison Street", 18, Sex.Male);
    Student("Edgar K. Clark", "42 Bay Street", 17, Sex.Male);
    Student("Tanya J. Rivera", "1517 Lynden Road", 18, Sex.Female);
    Student("Ellen L. Matherne", "4254 Victoria Park Ave", 22, Sex.Female);
    Student("Betty J. Harris", "3437 Boulevard Lafleche", 21, Sex.Female);
    Student("Ilse G. Hurst", "1606 Adelaide St", 22, Sex.Female);
    Student("Anthony I. Killion", "692 Cardero St", 17, Sex.Male);

    rel Faculty(name: String, address: String, rank: AcademicRank);
    Faculty("Darrel P. Hammond", "4880 Lilac Lane", AcademicRank.PostDoc).
    Faculty("Melissa R. Morales", "1527 Boone Street", AcademicRank.AssistantProf).
    Faculty("Elizabeth M. Khan", "1140 Ross Terrasse", AcademicRank.PostDoc).
    Faculty("Angela M. Coward", "489 Merivale Road", AcademicRank.FullProf).
    Faculty("Susan H. Warren", "1110 Rue De La Gare", AcademicRank.PostDoc).
    Faculty("Earl A. Purdy", "4638 5th Avenue", AcademicRank.AssociateProf).
    Faculty("Brandon H. Rey", "138 47th Avenue", AcademicRank.AssistantProf).

    rel Staff(name: String, address: String);
    Staff("Irma S. Duncan", "1970 Bayfield St").
    Staff("Marion S. Edgin", "3439 James Street").

    rel Salary(name: String, amount: Int);
    Salary(name,     0) :- Student(name, _, _, _, _).
    Salary(name, 50000) :- Faculty(name, _, AcademicRank.PostDoc).
    Salary(name, 60000) :- Staff(name, _).
    Salary(name, 70000) :- Faculty(name, AcademicRank.AssistantProf).
    Salary(name, 80000) :- Faculty(name, AcademicRank.AssociateProf).
    Salary(name, 90000) :- Faculty(name, AcademicRank.FullProf).

    rel Department(name: String);
    Department("Biology").
    Department("Computer Science").
    Department("Engineering").
    Department("Physics").
    Department("Math").

    rel StudiesIn(studentName: String, departmentName: String);
    StudiesIn("Victor K. Garcia", "Engineering");
    StudiesIn("Priscilla T. Reed", "Engineering");
    StudiesIn("Ronald K. Rodriguez", "Math");
    StudiesIn("David J. Richard", "Math");
    StudiesIn("Edgar K. Clark", "Math");
    StudiesIn("Tanya J. Rivera", "Biology");
    StudiesIn("Ellen L. Matherne", "Physics");
    StudiesIn("Betty J. Harris", "Biology");
    StudiesIn("Ilse G. Hurst", "Computer Science");
    StudiesIn("Anthony I. Killion", "Computer Science");

    rel WorksIn(facultyName: String, departmentName: String);
    Faculty("Darrel P. Hammond", "Math").
    Faculty("Melissa R. Morales", "Math").
    Faculty("Elizabeth M. Khan", "Engineering").
    Faculty("Angela M. Coward", "Engineering").
    Faculty("Susan H. Warren", "Biology").
    Faculty("Earl A. Purdy", "Math").
    Faculty("Brandon H. Rey", "Physics").

    rel Mathematician(name: String, amount: Int);
    Mathematician(name, amount) :- Student(name, _, _, _), WorksIn(name, "Math"), Salary(name, amount).
    Mathematician(name, amount) :- Faculty(name, _, _), WorksIn(name, "Math"), Salary(name, amount).

    rel Course(name: String, departmentName: String, teacherName: String);

    rel Grade(studentName: String, courseName: String, grade: Int);

    rel Publication(title: String, journal: String);
    Publication("Points and Problems in Advanced Probabilistic Topology", "arXiv");
    Publication("Convexity Methods in Topological Galois Theory", "arXiv");

    rel PublicationAuthor(title: String, authorName: String);

}