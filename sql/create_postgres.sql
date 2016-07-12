-- Schema creation
-- Use sequence to generate primary keys

drop database if exists postit;
drop user if exists postit;

create user postit with password 'postit';
create database postit with owner = postit;

\connect postit

create domain clob as text;

\i create_table.sql
\i create_sequence.sql
\i alter_table.sql
\i alter_table_pg.sql
\i insert_data.sql

grant select,update,insert,delete on all tables in schema public to postit;
grant all on all sequences in schema public to postit;

