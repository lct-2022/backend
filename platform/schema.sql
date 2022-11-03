CREATE SCHEMA IF NOT EXISTS platform;

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

CREATE TYPE job_application_status AS ENUM ('applied', 'accepted', 'declined');
ALTER TABLE platform.job_application ADD COLUMN status job_application_status NOT NULL DEFAULT 'applied';


ALTER TABLE platform.team_member ADD COLUMN title TEXT NOT NULL DEFAULT 'Unknown';
ALTER TABLE platform.team_member ALTER COLUMN title DROP DEFAULT;


ALTER TABLE platform.job ADD COLUMN open BOOLEAN DEFAULT TRUE;

CREATE TABLE platform.project_chat (
    project_id BIGINT NOT NULL,
    chat_id UUID NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ,
    PRIMARY KEY (project_id, chat_id)
);


ALTER TABLE platform.project_chat ADD COLUMN private BOOLEAN DEFAULT FALSE;


CREATE TABLE platform.project_stage (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


ALTER TABLE platform.project ADD COLUMN stage_id BIGINT DEFAULT 1;

UPDATE platform.project SET stage_id = 1 + (random() * 6)::integer;
