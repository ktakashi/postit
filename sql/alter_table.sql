-- commons
alter table user_info add constraint fk_ui_userid
  foreign key (userid) references users(id);
alter table postit add constraint fk_pi_userid
  foreign key (userid) references users(id);
