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


CREATE TABLE tv.channel_source (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    url TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


alter table tv.channel alter column id type text;
alter table tv.channel add column source_id bigint not null default 1;
alter table tv.channel alter column source_id drop default;

alter table tv.programme alter column channel_id type text;
alter table tv.programme add column source_id bigint not null default 1;
alter table tv.programme alter column source_id drop default;

drop index tv.current_programmes_idx;

CREATE UNIQUE INDEX programme_pkey2 ON tv.programme (channel_id, source_id, start, stop);
ALTER TABLE tv.programme DROP CONSTRAINT programme_pkey, ADD PRIMARY KEY USING INDEX programme_pkey2;

CREATE UNIQUE INDEX programme_pkey3 ON tv.programme (source_id, channel_id, start, stop);
ALTER TABLE tv.programme DROP CONSTRAINT programme_pkey2, ADD PRIMARY KEY USING INDEX programme_pkey3;

alter table tv.programme_chat alter column channel_id type text;
alter table tv.programme_chat add column source_id bigint not null default 1;
alter table tv.programme_chat alter column source_id drop default;

CREATE UNIQUE INDEX programme_chat_pkey2 ON tv.programme_chat (channel_id, source_id, start);
ALTER TABLE tv.programme_chat DROP CONSTRAINT programme_chat_pkey, ADD PRIMARY KEY USING INDEX programme_chat_pkey2;

CREATE UNIQUE INDEX programme_chat_pkey3 ON tv.programme_chat (source_id, channel_id, start);
ALTER TABLE tv.programme_chat DROP CONSTRAINT programme_chat_pkey2, ADD PRIMARY KEY USING INDEX programme_chat_pkey3;


alter table tv.channel rename column id to channel_id;
CREATE UNIQUE INDEX channel_pkey2 ON tv.channel (source_id, channel_id);
ALTER TABLE tv.channel DROP CONSTRAINT channel_pkey, ADD PRIMARY KEY USING INDEX channel_pkey2;
