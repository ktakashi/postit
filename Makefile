init-sqlite:
	rm postit.db; cd sql; sqlite3 < create_sqlite3.sql
init-pg:
	cd sql; psql -f create_postgres.sql
