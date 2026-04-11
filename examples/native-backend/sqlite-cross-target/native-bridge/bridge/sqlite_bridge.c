#include "sqlite_bridge.h"

#include <sqlite3.h>
#include <stdlib.h>

struct flix_sqlite_db {
    sqlite3 *raw;
};

struct flix_sqlite_stmt {
    sqlite3_stmt *raw;
};

int32_t flix_sqlite_open(const char *path, flix_sqlite_db_t **out_db) {
    sqlite3 *raw = NULL;
    int32_t status = sqlite3_open(path, &raw);
    if (status != SQLITE_OK) {
        if (raw != NULL) {
            sqlite3_close(raw);
        }
        if (out_db != NULL) {
            *out_db = NULL;
        }
        return status;
    }

    flix_sqlite_db_t *db = malloc(sizeof(flix_sqlite_db_t));
    if (db == NULL) {
        sqlite3_close(raw);
        if (out_db != NULL) {
            *out_db = NULL;
        }
        return SQLITE_NOMEM;
    }

    db->raw = raw;
    if (out_db != NULL) {
        *out_db = db;
    }
    return SQLITE_OK;
}

void flix_sqlite_db_close(flix_sqlite_db_t *db) {
    if (db == NULL) {
        return;
    }
    if (db->raw != NULL) {
        sqlite3_close(db->raw);
    }
    free(db);
}

const char *flix_sqlite_db_errmsg(const flix_sqlite_db_t *db) {
    if (db == NULL || db->raw == NULL) {
        return "sqlite db is null";
    }
    return sqlite3_errmsg(db->raw);
}

int32_t flix_sqlite_db_exec(flix_sqlite_db_t *db, const char *sql) {
    if (db == NULL || db->raw == NULL) {
        return SQLITE_MISUSE;
    }

    char *errmsg = NULL;
    int32_t status = sqlite3_exec(db->raw, sql, NULL, NULL, &errmsg);
    if (errmsg != NULL) {
        sqlite3_free(errmsg);
    }
    return status;
}

int32_t flix_sqlite_db_prepare(flix_sqlite_db_t *db, const char *sql, flix_sqlite_stmt_t **out_stmt) {
    if (db == NULL || db->raw == NULL) {
        if (out_stmt != NULL) {
            *out_stmt = NULL;
        }
        return SQLITE_MISUSE;
    }

    sqlite3_stmt *raw = NULL;
    int32_t status = sqlite3_prepare_v2(db->raw, sql, -1, &raw, NULL);
    if (status != SQLITE_OK) {
        if (raw != NULL) {
            sqlite3_finalize(raw);
        }
        if (out_stmt != NULL) {
            *out_stmt = NULL;
        }
        return status;
    }

    flix_sqlite_stmt_t *stmt = malloc(sizeof(flix_sqlite_stmt_t));
    if (stmt == NULL) {
        sqlite3_finalize(raw);
        if (out_stmt != NULL) {
            *out_stmt = NULL;
        }
        return SQLITE_NOMEM;
    }

    stmt->raw = raw;
    if (out_stmt != NULL) {
        *out_stmt = stmt;
    }
    return SQLITE_OK;
}

void flix_sqlite_stmt_finalize(flix_sqlite_stmt_t *stmt) {
    if (stmt == NULL) {
        return;
    }
    if (stmt->raw != NULL) {
        sqlite3_finalize(stmt->raw);
    }
    free(stmt);
}

int32_t flix_sqlite_stmt_bind_text(flix_sqlite_stmt_t *stmt, int32_t index, const char *value) {
    if (stmt == NULL || stmt->raw == NULL) {
        return SQLITE_MISUSE;
    }
    return sqlite3_bind_text(stmt->raw, index, value, -1, SQLITE_TRANSIENT);
}

int32_t flix_sqlite_stmt_bind_blob(flix_sqlite_stmt_t *stmt, int32_t index, const uint8_t *bytes, size_t bytes_len) {
    if (stmt == NULL || stmt->raw == NULL) {
        return SQLITE_MISUSE;
    }
    return sqlite3_bind_blob(stmt->raw, index, bytes, (int)bytes_len, SQLITE_TRANSIENT);
}

int32_t flix_sqlite_stmt_step(flix_sqlite_stmt_t *stmt) {
    if (stmt == NULL || stmt->raw == NULL) {
        return SQLITE_MISUSE;
    }
    return sqlite3_step(stmt->raw);
}

int32_t flix_sqlite_stmt_reset(flix_sqlite_stmt_t *stmt) {
    if (stmt == NULL || stmt->raw == NULL) {
        return SQLITE_MISUSE;
    }
    return sqlite3_reset(stmt->raw);
}

int64_t flix_sqlite_stmt_column_int64(const flix_sqlite_stmt_t *stmt, int32_t index) {
    if (stmt == NULL || stmt->raw == NULL) {
        return 0;
    }
    return sqlite3_column_int64(stmt->raw, index);
}

const char *flix_sqlite_stmt_column_text(const flix_sqlite_stmt_t *stmt, int32_t index) {
    if (stmt == NULL || stmt->raw == NULL) {
        return NULL;
    }
    return (const char *)sqlite3_column_text(stmt->raw, index);
}

const uint8_t *flix_sqlite_stmt_column_blob(const flix_sqlite_stmt_t *stmt, int32_t index, size_t *out_len) {
    if (out_len != NULL) {
        *out_len = 0;
    }
    if (stmt == NULL || stmt->raw == NULL) {
        return NULL;
    }
    if (out_len != NULL) {
        *out_len = (size_t)sqlite3_column_bytes(stmt->raw, index);
    }
    return (const uint8_t *)sqlite3_column_blob(stmt->raw, index);
}
