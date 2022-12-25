CREATE SCHEMA IF NOT EXISTS passport;

CREATE TABLE passport.user (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    nickname TEXT,
    email TEXT,
    password_hash TEXT,
    avatar_url TEXT DEFAULT 'http://www.gravatar.com/avatar/501a6ae10e3fc3956ad1052cfc6d38d9?s=200',
    admin BOOLEAN DEFAULT False,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE UNIQUE INDEX passport_user_email ON passport.user (email);
CREATE UNIQUE INDEX passport_user_nickname ON passport.user (nickname);

---------------
-- Migrations:



alter table passport.user add column robot boolean default false not null;
