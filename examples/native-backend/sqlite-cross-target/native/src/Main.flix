def expectOk(res: Result[Int32, a], label: String): a = match res {
    case Ok(value) => value
    case Err(code) => bug!(label + ": sqlite status " + Int32.toString(code))
}

def expectStatus(db: Sqlite.Db, expected: Int32, actual: Int32, label: String): Unit \ IO =
    if (actual == expected)
        ()
    else
        bug!(label + ": sqlite status " + Int32.toString(actual) + ": " + Sqlite.flixSqliteDbErrmsg(db))

pub def main(): Unit \ IO = {
    let sqliteOk = 0i32;
    let sqliteRow = 100i32;
    let sqliteDone = 101i32;

    let db = expectOk(Sqlite.flixSqliteOpen(":memory:"), "open");
    expectStatus(db, sqliteOk, Sqlite.flixSqliteDbExec(db, "create table notes (id integer primary key, body text not null, payload blob not null)"), "create-table");

    let insertStmt = expectOk(Sqlite.flixSqliteDbPrepare(db, "insert into notes(body, payload) values (?, ?)"), "prepare-insert");
    let selectStmt = expectOk(Sqlite.flixSqliteDbPrepare(db, "select id, body, payload from notes order by id"), "prepare-select");
    let payload = Array#{7i8, 8i8, 9i8, 10i8} @ Static;

    expectStatus(db, sqliteOk, Sqlite.flixSqliteStmtBindText(insertStmt, 1i32, "hello"), "bind-text");
    expectStatus(db, sqliteOk, Sqlite.flixSqliteStmtBindBlob(insertStmt, 2i32, payload), "bind-blob");
    expectStatus(db, sqliteDone, Sqlite.flixSqliteStmtStep(insertStmt), "step-insert");
    expectStatus(db, sqliteOk, Sqlite.flixSqliteStmtReset(insertStmt), "reset-insert");

    expectStatus(db, sqliteRow, Sqlite.flixSqliteStmtStep(selectStmt), "step-select-row");
    let rowId = Sqlite.flixSqliteStmtColumnInt64(selectStmt, 0i32);
    let body = Sqlite.flixSqliteStmtColumnText(selectStmt, 1i32);
    let blob = Sqlite.flixSqliteStmtColumnBlob(selectStmt, 2i32);
    expectStatus(db, sqliteDone, Sqlite.flixSqliteStmtStep(selectStmt), "step-select-done");

    let _ = Sqlite.closeStmt(selectStmt);
    let _ = Sqlite.closeStmt(insertStmt);
    let _ = Sqlite.closeDb(db);

    println(Int64.toString(rowId) + ":" + body + ":" + Int32.toString(Array.length(blob)))
}
