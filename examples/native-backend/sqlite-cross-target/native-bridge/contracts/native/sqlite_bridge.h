#ifndef FLIX_SQLITE_BRIDGE_H
#define FLIX_SQLITE_BRIDGE_H

#include <stddef.h>
#include <stdint.h>

typedef struct flix_sqlite_db flix_sqlite_db_t;
typedef struct flix_sqlite_stmt flix_sqlite_stmt_t;

int32_t flix_sqlite_open(const char *path, flix_sqlite_db_t **out_db);

void flix_sqlite_db_close(flix_sqlite_db_t *db);

const char *flix_sqlite_db_errmsg(const flix_sqlite_db_t *db);

int32_t flix_sqlite_db_exec(flix_sqlite_db_t *db, const char *sql);

int32_t flix_sqlite_db_prepare(flix_sqlite_db_t *db, const char *sql, flix_sqlite_stmt_t **out_stmt);

void flix_sqlite_stmt_finalize(flix_sqlite_stmt_t *stmt);

int32_t flix_sqlite_stmt_bind_text(flix_sqlite_stmt_t *stmt, int32_t index, const char *value);

int32_t flix_sqlite_stmt_bind_blob(flix_sqlite_stmt_t *stmt, int32_t index, const uint8_t *bytes, size_t bytes_len);

int32_t flix_sqlite_stmt_step(flix_sqlite_stmt_t *stmt);

int32_t flix_sqlite_stmt_reset(flix_sqlite_stmt_t *stmt);

int64_t flix_sqlite_stmt_column_int64(const flix_sqlite_stmt_t *stmt, int32_t index);

const char *flix_sqlite_stmt_column_text(const flix_sqlite_stmt_t *stmt, int32_t index);

const uint8_t *flix_sqlite_stmt_column_blob(const flix_sqlite_stmt_t *stmt, int32_t index, size_t *out_len);

#endif
