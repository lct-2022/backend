CREATE TABLE passport.user (
    id INTEGER NOT NULL PRIMARY KEY,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    fio TEXT,
    birthday TEXT,
    gender TEXT,
    phone TEXT,
    country TEXT,
    city TEXT,
    education TEXT,
    job TEXT,
    about TEXT,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE UNIQUE INDEX passport_user_email ON passport.user (email);


ALTER TABLE passport.user ADD COLUMN admin BOOLEAN DEFAULT False;
