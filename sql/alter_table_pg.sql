-- PostgreSQL specific
alter table users alter column id set default nextval('user_seq');
alter table user_info alter column id set default nextval('user_info_seq');
alter table postit alter column id set default nextval('postit_seq');
