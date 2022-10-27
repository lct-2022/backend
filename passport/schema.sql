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

create unique index passport_user_email on passport.user (email);
