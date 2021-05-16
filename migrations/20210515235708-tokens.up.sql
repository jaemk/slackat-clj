create table slackat.tokens (
    id int8 not null primary key default slackat.id_gen(),
    created timestamptz not null default now(),
    modified timestamptz not null default now(),
    nonce text not null,
    iv text not null,
    bot_id text not null,
    bot_scope text not null,
    bot_token text not null,
    user_id text not null,
    user_scope text not null,
    user_token text not null
);
--;;
create index token_bot_id on slackat.tokens(bot_id);
--;;
create index token_user_id on slackat.tokens(user_id);
