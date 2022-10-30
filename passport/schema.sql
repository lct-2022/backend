CREATE SCHEMA IF NOT EXISTS passport;

CREATE TABLE passport.user (
    id BIGSERIAL NOT NULL PRIMARY KEY,
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

---------------
-- Migrations:

ALTER TABLE passport.user ADD COLUMN admin BOOLEAN DEFAULT False;

ALTER TABLE passport.user ADD COLUMN avatar_url TEXT;

UPDATE passport.user SET avatar_url = 'http://www.gravatar.com/avatar/501a6ae10e3fc3956ad1052cfc6d38d9?s=200'
 WHERE avatar_url is NULL;

ALTER TABLE passport.user ALTER COLUMN avatar_url SET NOT NULL;

