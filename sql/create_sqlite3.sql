-- Schema creation
-- Use autoincrement keyword to generate unique id
create table users (
       id integer             primary key autoincrement,
       username varchar2(255) not null unique,
       password varchar2(255) not null,
       create_date timestamp  default current_timestamp
);

create table user_info (
       id integer                primary key autoincrement,
       userid integer            not null,
       first_names varchar2(255) not null,
       middle_name varchar2(255),
       last_name   varchar2(255) not null
       -- TODO more?
);

create table postit (
       id integer                primary key autoincrement,
       userid integer            not null,
       postit clob,
       x integer                 default 0, -- top
       y integer                 default 0, -- left
       width                     default 300,
       height                    default 300,
       text_color_id integer     not null,
       bg_color_id integer       not null,
       create_date timestamp     default current_timestamp
);

create table colors (
       id integer               primary key autoincrement,
       rgb integer              not null unique,
       color_name varchar2(255)
);

-- insert default data
insert into users (id, username, password) values (0, 'anonymous', 'anonymous');

insert into colors (id, rgb, color_name) values (0, 0xffffff, 'white');
insert into colors (id, rgb, color_name) values (1, 0x000000, 'black');
insert into colors (id, rgb, color_name) values (2, 0xff0000, 'red');
insert into colors (id, rgb, color_name) values (3, 0x00ff00, 'green');
insert into colors (id, rgb, color_name) values (4, 0x0000ff, 'blue');

-- Kludge
insert into sqlite_sequence (name, seq) values ('postit', 0);
insert into sqlite_sequence (name, seq) values ('user_info', 0);

-- constraints
-- Very unfortunately, SQLite doesn't have foreign key constraint
/*
alter table user_info add constraint fk_ui_userid
  foreign key (userid) references users(id);

alter table postit add constraint fk_pi_userid
  foreign key (userid) references users(id);
alter table postit add constraint fk_pi_txt_color
  foreign key (text_color_id) references colors(id);
alter table postit add constraint fk_pi_bg_color
  foreign key (text_bg_id) references colors(id);
*/
