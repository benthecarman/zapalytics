CREATE TABLE zaps
(
    id      TEXT PRIMARY KEY,
    author  TEXT    NOT NULL,
    user    TEXT    NOT NULL,
    invoice TEXT    NOT NULL,
    node_id TEXT    NOT NULL,
    amount  INTEGER NOT NULL,
    date    INTEGER NOT NULL
);

CREATE TABLE metadata
(
    user    TEXT PRIMARY KEY,
    lud06   TEXT,
    lud16   TEXT,
    node_id TEXT,
    date    INTEGER NOT NULL
);
