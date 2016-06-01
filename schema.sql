DROP TABLE IF EXISTS quotations CASCADE;
DROP TABLE IF EXISTS lines CASCADE;

create table quotations (
    id serial primary key,
    created_at timestamp not null default current_timestamp
);

create table lines (
    id serial primary key,
    quotation_id integer not null references quotations (id),
    speaker text not null,
    words text not null
);
