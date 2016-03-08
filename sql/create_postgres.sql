-- Schema creation
-- Use sequence to generate primary keys

drop database if exists postit;
drop user if exists postit;

create user postit with password 'postit';
create database postit with owner = postit;

\connect postit

create sequence users_seq start with 100;
create sequence user_info_seq;
create sequence postit_seq;
create sequence colors_seq start with 100;

create table users (
       id integer             primary key default nextval('users_seq'),
       username varchar not null unique,
       password varchar not null,
       create_date timestamp  default current_timestamp
);

create table user_info (
       id integer                primary key default nextval('user_info_seq'),
       userid integer            not null,
       first_names varchar not null,
       middle_name varchar,
       last_name   varchar not null
       -- TODO more?
);

create table postit (
       id integer                primary key default nextval('postit_seq'),
       userid integer            not null,
       postit text,
       x integer                 default 0, -- top
       y integer                 default 0, -- left
       width integer             default 300,
       height integer            default 300,
       text_color_id integer     not null,
       bg_color_id integer       not null,
       create_date timestamp     default current_timestamp
);

create table colors (
       id integer               primary key default nextval('colors_seq'),
       rgb integer              not null unique,
       color_name varchar
);

grant select,update,insert,delete on all tables in schema public to postit;
grant all on all sequences in schema public to postit;

-- insert default data
insert into users (id, username, password) values (0, 'anonymous', 'anonymous');

insert into colors (id, rgb, color_name) values (0, x'ffffff'::int, 'white');
insert into colors (id, rgb, color_name) values (1, x'000000'::int, 'black');
insert into colors (id, rgb, color_name) values (2, x'ff0000'::int, 'red');
insert into colors (id, rgb, color_name) values (3, x'00ff00'::int, 'green');
insert into colors (id, rgb, color_name) values (4, x'0000ff'::int, 'blue');

-- constraints
alter table user_info add constraint fk_ui_userid
  foreign key (userid) references users(id);
alter table postit add constraint fk_pi_userid
  foreign key (userid) references users(id);
alter table postit add constraint fk_pi_txt_color
  foreign key (text_color_id) references colors(id);
alter table postit add constraint fk_pi_bg_color
  foreign key (bg_color_id) references colors(id);


