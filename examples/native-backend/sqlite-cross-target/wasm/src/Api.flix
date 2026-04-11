mod Api {

    def expectOk(res: Result[Int32, a], label: String): a = match res {
        case Ok(value) => value
        case Err(code) => bug!(label + ": sqlite status " + Int32.toString(code))
    }

    def expectStatus(db: Wit.ExampleSqliteBridgeDb, expected: Int32, actual: Int32, label: String): Unit \ Wit.ExampleSqliteBridge =
        if (actual == expected)
            ()
        else
            bug!(label + ": sqlite status " + Int32.toString(actual) + ": " + Wit.dbErrmsg(db))

    @Export
    pub def runDemo(): String \ IO + Wit.ExampleSqliteBridge = {
        let sqliteOk = 0i32;
        let sqliteRow = 100i32;
        let sqliteDone = 101i32;

        let db = expectOk(Wit.open(":memory:"), "open");
        expectStatus(db, sqliteOk, Wit.dbExec(db, "create table notes (id integer primary key, body text not null, payload blob not null)"), "create-table");

        let insertStmt = expectOk(Wit.dbPrepare(db, "insert into notes(body, payload) values (?, ?)"), "prepare-insert");
        let selectStmt = expectOk(Wit.dbPrepare(db, "select id, body, payload from notes order by id"), "prepare-select");
        let payload = Array#{7i8, 8i8, 9i8, 10i8} @ Static;

        expectStatus(db, sqliteOk, Wit.stmtBindText(insertStmt, 1i32, "hello"), "bind-text");
        expectStatus(db, sqliteOk, Wit.stmtBindBlob(insertStmt, 2i32, payload), "bind-blob");
        expectStatus(db, sqliteDone, Wit.stmtStep(insertStmt), "step-insert");
        expectStatus(db, sqliteOk, Wit.stmtReset(insertStmt), "reset-insert");

        expectStatus(db, sqliteRow, Wit.stmtStep(selectStmt), "step-select-row");
        let rowId = Wit.stmtColumnInt64(selectStmt, 0i32);
        let body = Wit.stmtColumnText(selectStmt, 1i32);
        let blob = Wit.stmtColumnBlob(selectStmt, 2i32);
        expectStatus(db, sqliteDone, Wit.stmtStep(selectStmt), "step-select-done");

        let _ = Wit.stmtDrop(selectStmt);
        let _ = Wit.stmtDrop(insertStmt);
        let _ = Wit.dbDrop(db);

        Int64.toString(rowId) + ":" + body + ":" + Int32.toString(Array.length(blob))
    }

}
