CREATE TABLE platform.project (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    url TEXT NOT NULL,
    contests TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE platform.team (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    project_id INTEGER NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE platform.job (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    team_id INTEGER NOT NULL,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE platform.job_application (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    job_id INTEGER NOT NULL,
    message TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE platform.team_member (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    job_id INTEGER NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


// Migrations

ALTER TABLE platform.project ADD COLUMN author_id integer DEFAULT 1;
ALTER TABLE platform.project ALTER COLUMN author_id DROP NOT NULL, ALTER COLUMN author_id DROP DEFAULT;
