CREATE SCHEMA IF NOT EXISTS rating;

CREATE TYPE rating_subject_type AS ENUM ('project', 'user', 'job', 'service', 'team');

CREATE TABLE rating.vote (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    subject_type rating_subject_type NOT NULL,
    subject_id INTEGER NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE UNIQUE INDEX vote_uniq ON rating.vote (subject_type, subject_id, user_id);
