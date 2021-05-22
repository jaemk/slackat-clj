create table slackat.users (
    id int8 not null primary key default slackat.id_gen(),
    created timestamptz not null default now(),
    modified timestamptz not null default now(),
    slack_id text not null
);
--;;
create unique index user_slack_id on slackat.users(slack_id);
--;;
create table slackat.auth_tokens (
     id int8 not null primary key default slackat.id_gen(),
     created timestamptz not null default now(),
     modified timestamptz not null default now(),
     user_id int8 not null references slackat.users(id),
     signature text not null
);
--;;
create index auth_token_user on slackat.auth_tokens(user_id);
--;;
create type slackat.slack_token_type as enum ('bot', 'user');
--;;
create table slackat.slack_tokens (
    id int8 not null primary key default slackat.id_gen(),
    created timestamptz not null default now(),
    modified timestamptz not null default now(),
    nonce text not null,
    iv text not null,

    type slack_token_type not null,
    slack_id text not null,
    slack_team_id text not null,
    scope text not null,
    encrypted text not null
);
--;;
create unique index slack_token_type_slack_id_slack_team_id on slackat.slack_tokens(type, slack_id, slack_team_id);