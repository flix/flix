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

    rel Student(name: Str, address: Str, age: Int, sex: Sex);
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

    rel Faculty(name: Str, address: Str, rank: AcademicRank);
    Faculty("Darrel P. Hammond", "4880 Lilac Lane", AcademicRank.PostDoc).
    Faculty("Melissa R. Morales", "1527 Boone Street", AcademicRank.AssistantProf).
    Faculty("Elizabeth M. Khan", "1140 Ross Terrasse", AcademicRank.PostDoc).
    Faculty("Angela M. Coward", "489 Merivale Road", AcademicRank.FullProf).
    Faculty("Susan H. Warren", "1110 Rue De La Gare", AcademicRank.PostDoc).
    Faculty("Earl A. Purdy", "4638 5th Avenue", AcademicRank.AssociateProf).
    Faculty("Brandon H. Rey", "138 47th Avenue", AcademicRank.AssistantProf).

    rel Staff(name: Str, address: Str);
    Staff("Irma S. Duncan", "1970 Bayfield St").
    Staff("Marion S. Edgin", "3439 James Street").

    rel Salary(name: Str, amount: Int);
    Salary(name,     0) :- Student(name, _, _, _, _).
    Salary(name, 50000) :- Faculty(name, _, AcademicRank.PostDoc).
    Salary(name, 60000) :- Staff(name, _).
    Salary(name, 70000) :- Faculty(name, AcademicRank.AssistantProf).
    Salary(name, 80000) :- Faculty(name, AcademicRank.AssociateProf).
    Salary(name, 90000) :- Faculty(name, AcademicRank.FullProf).

    rel Department(name: Str);
    Department("Biology").
    Department("Computer Science").
    Department("Engineering").
    Department("Physics").
    Department("Math").

    rel StudiesIn(studentName: Str, departmentName: Str);
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

    rel WorksIn(facultyName: Str, departmentName: Str);
    Faculty("Darrel P. Hammond", "Math").
    Faculty("Melissa R. Morales", "Math").
    Faculty("Elizabeth M. Khan", "Computer Science").
    Faculty("Angela M. Coward", "Engineering").
    Faculty("Susan H. Warren", "Biology").
    Faculty("Earl A. Purdy", "Math").
    Faculty("Brandon H. Rey", "Physics").

    rel Mathematician(name: Str, amount: Int);
    Mathematician(name, amount) :- Student(name, _, _, _), WorksIn(name, "Math"), Salary(name, amount).
    Mathematician(name, amount) :- Faculty(name, _, _), WorksIn(name, "Math"), Salary(name, amount).

    rel Course(name: Str, departmentName: Str, teacherName: Str);
    // TODO

    rel Grade(studentName: Str, courseName: Str, grade: Int);
    // TODO

    rel Publication(title: Str, journal: Str);
    Publication("Points and Problems in Advanced Probabilistic Topology", "arXiv");
    Publication("Convexity Methods in Topological Galois Theory", "arXiv");
    Publication("Investigation of Thin Clients", "arXiv");
    Publication("Multimodal, Client-Server Technology for SMPs", "arXiv")

    rel PublicationAuthor(title: Str, authorName: Str);
    PublicationAuthor("Points and Problems in Advanced Probabilistic Topology", "Darrel P. Hammond").
    PublicationAuthor("Points and Problems in Advanced Probabilistic Topology", "Earl A. Purdy").
    PublicationAuthor("Convexity Methods in Topological Galois Theory", "Darrel P. Hammond").
    PublicationAuthor("Investigation of Thin Clients", "Earl A. Purdy").
    PublicationAuthor("Investigation of Thin Clients", "Elizabeth M. Khan").
    PublicationAuthor("Investigation of Thin Clients", "Earl A. Purdy").
    PublicationAuthor("Investigation of Thin Clients", "Brandon H. Rey").
    PublicationAuthor("Multimodal, Client-Server Technology for SMPs", "Elizabeth M. Khan").
    PublicationAuthor("Multimodal, Client-Server Technology for SMPs", "Edgar K. Clark").

    rel StudentPaper(title: Str);
    StudentPaper(title) :- Publication(title, _), PublicationAuthor(title, authorName), Student(authorName, _, _, _).
}