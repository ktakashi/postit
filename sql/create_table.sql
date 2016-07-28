-- Schema creation
create table state (
       id integer                primary key,
       name varchar(50)         not null unique
);

-- Use autoincrement keyword to generate unique id
create table users (
       id integer             primary key,
       username varchar(255) not null unique,
       password varchar(255) not null,
       create_date timestamp  default current_timestamp
);

create table user_info (
       id integer                primary key,
       userid integer            not null,
       first_names varchar(255) not null,
       middle_name varchar(255),
       last_name   varchar(255) not null,
       email       varchar(255)
       -- TODO more?
);

create table postit (
       id integer                primary key,
       userid integer            not null,
       postit clob,
       stateid integer           not null,
       create_date timestamp     default current_timestamp
);
