create schema tv;

CREATE TABLE tv.channel (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE tv.programme (
    channel_id BIGINT NOT NULL,
    start TIMESTAMPTZ NOT NULL,
    stop TIMESTAMPTZ NOT NULL,
    title TEXT NOT NULL,
    description TEXT,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ,
    PRIMARY KEY (channel_id, start)
);


ALTER TABLE tv.channel ADD COLUMN rating BIGINT, ADD COLUMN image_url TEXT;

CREATE INDEX tv_channel_rating_idx ON tv.channel (rating);

CREATE INDEX current_programmes_idx ON tv.programme (channel_id, start, stop);


CREATE TABLE tv.programme_chat (
    channel_id BIGINT NOT NULL,
    start TIMESTAMPTZ NOT NULL,
    stop TIMESTAMPTZ NOT NULL,
    title TEXT NOT NULL,
    chat_id UUID NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ,
    PRIMARY KEY (channel_id, start)
);

CREATE UNIQUE INDEX tv_programme_chat ON tv.programme_chat (chat_id);
