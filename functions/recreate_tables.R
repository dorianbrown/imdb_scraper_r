# Use this for creating the database tables.

recreate_tables <- function(db_conn) {
	library(RSQLite)
	# DROP ALL TABLES :o
	a <- dbGetQuery(db_conn, "
		select 'drop table ' || name || ';' from sqlite_master
	    where 1=1
	    and type = 'table'
	    and name != 'sqlite_sequence';
	")
	
	if (nrow(a) > 0) {
        for (i in 1:nrow(a)) {
        	drop_sql <- a[i,]
            DBI::dbExecute(db_conn, drop_sql)
        }
	}

	res <- DBI::dbExecute(db_conn,'
		create table actors(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			first_name text,
			last_name text,
			unique(first_name,last_name)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		create table directors(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			first_name text,
			last_name text,
			unique(first_name,last_name)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		create table writers(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			first_name text,
			last_name text,
			unique(first_name,last_name)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		create table genres(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			genre_name text,
			unique(genre_name)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		CREATE TABLE movies (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			title text,
			year integer,
			rating real,
			rating_votes integer,
			writer_id integer,
			medium_type text,
			unique(title, year)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		CREATE TABLE languages (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			language text,
			unique(language)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		create table movies_to_languages(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			movie_id integer,
			language_id integer,
			unique(movie_id, language_id)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		create table movies_to_genres(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			movie_id integer,
			actor_id integer,
			unique(movie_id, actor_id)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		create table movies_to_actors(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			movie_id integer,
			actor_id integer,
			unique(movie_id, actor_id)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		create table movies_to_directors(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			movie_id integer,
			director_id integer,
			unique(movie_id, director_id)
		);
	')

	res <- DBI::dbExecute(db_conn,'
		create table movies_to_movies(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			id_1 integer,
			id_2 integer,
			unique(id_1, id_2)
		);
	')
}