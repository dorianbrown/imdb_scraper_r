# Expose this for use outside of function

insert_junction_table <- function(tablename, source, target, db_conn) {
    insert_string <- target %>% 
        map(function(x) paste("insert into", tablename, "values (null,", source, ",", x, ")")) %>%
        unlist

    try(insert_string %>% map(DBI::dbExecute, conn = db_conn))
}

# Adds a movie to the local database, including all the related tables
# except for the movie_to_movie table since future id's are needed.

add_to_database <- function(input_list, db_conn) {
    
    # Some helper functions for splitting names into first/last

    first_name <- function(name_string) str_extract(name_string,'^\\w+')
    last_name <- function(name_string) str_extract(name_string,'\\w+$')

    # These are insert functions that have no dependencies on ID's

    insert_actor <- function(name, db_conn) {
        
        first_name <- name %>% first_name
        last_name <- name %>% last_name

        if (length(name) == 0 || is.na(name)){
            return(NULL)
        } else {
            try(DBI::dbExecute(db_conn, paste0("
                insert into actors 
                    (first_name, last_name) 
                values ('", first_name, "','", last_name, "')"
            )), silent = T)
            output <- DBI::dbGetQuery(db_conn, paste0("
                select id 
                from actors
                where
                    first_name = '", first_name, "' and
                    last_name = '", last_name, "'
            "))
            return(output$id)
        }
    }

    insert_director <- function(name, db_conn) {

        first_name <- name %>% first_name
        last_name <- name %>% last_name
        
        if (length(name) == 0 || is.na(name)){
            return(NULL)
        } else {
            try(DBI::dbExecute(db_conn, paste0("
                insert into directors 
                    (first_name, last_name) 
                values ('", first_name, "','", last_name, "')"
            )), silent = T)   
            output <- DBI::dbGetQuery(db_conn, paste0("
                select id 
                from directors
                where
                    first_name = '", first_name, "' and
                    last_name = '", last_name, "'
            "))
            return(output$id)
        }
    }

    insert_writer <- function(name, db_conn) {
        first_name <- name %>% first_name
        last_name <- name %>% last_name

        if (length(name) == 0 || is.na(name)){
            return(NULL)
        } else {
            try(DBI::dbExecute(db_conn, paste0("
                insert into writers 
                    (first_name, last_name) 
                values ('", first_name, "','", last_name, "')"
            )), silent = T)
            output <- DBI::dbGetQuery(db_conn, paste0("
                select id 
                from writers
                where
                    first_name = '", first_name, "' and
                    last_name = '", last_name, "'
            "))   
            return(output$id)
        }
    }

    insert_language <- function(language, db_conn) {

        if (length(language) == 0 || is.na(language)){
            return(NULL)
        } else {
            try(DBI::dbExecute(db_conn, paste0("
                insert into languages 
                    (language) 
                values ('", language, "')"
            )), silent = T)
            output <- DBI::dbGetQuery(db_conn, paste0("
                select id 
                from languages
                where
                    language = '", language, "'
            "))   
            return(output$id)
        }
    }

    insert_genre <- function(genre_name, db_conn) {

        if (length(genre_name) == 0 || is.na(genre_name)){
            return(NULL)
        } else {
            try(DBI::dbExecute(db_conn, paste0("
                insert into genres 
                    (genre_name) 
                values ('", genre_name, "')"
            )), silent = T)
            output <- DBI::dbGetQuery(db_conn, paste0("
                select id 
                from genres
                where
                    genre_name = '", genre_name, "'
            "))
            return(output$id)
        }
    }

    insert_movie <- function(input_list, db_conn) {

        writer_id <- insert_writer(input_list$writer, db_conn)

        # Title no longer unique... Could cause problems
        if (length(input_list$title) == 0 || is.na(input_list$title)) input_list$title <- "title_not_found"
        if (length(writer_id) == 0 || is.na(writer_id)) writer_id <- "null"
        if (length(input_list$year) == 0 || is.na(input_list$year) || !is.numeric(input_list$year)) input_list$year <- "null"
        if (length(input_list$rating) == 0 || is.na(input_list$rating) || !is.numeric(input_list$rating)) input_list$rating <- "null"
        if (length(input_list$rating_votes) == 0 || is.na(input_list$rating_votes) || !is.numeric(input_list$rating_votes)) input_list$rating_votes <- "null"

        DBI::dbExecute(db_conn, paste0("
            insert into movies(
                title, 
                year, 
                rating, 
                rating_votes,
                writer_id,
                medium_type
            ) values (
                '", input_list$title, "',
                '", input_list$year, "',
                '", input_list$rating, "',
                '", input_list$rating_votes, "',
                '", writer_id, "',
                '", input_list$medium_type, "'
            )
        "))
        output <- DBI::dbGetQuery(db_conn, paste0("
            select id 
            from movies
            where
                title = '", input_list$title, "'
        "))
        return(output$id)
    }

    # Now we get to the main insert. Our strategy is inserting actor/director
    # /writer first, and then using the resulting 

    movie_id <- insert_movie(input_list, db_conn)

    # Since empty directors/genres/actors/etc return NULL from insert fncs, the unlist removes them.
    director_list <- input_list$director %>% map(insert_director, db_conn = db_conn) %>% unlist
    genre_list <- input_list$genres %>% map(insert_genre, db_conn = db_conn) %>% unlist
    actor_list <- input_list$actors %>% map(insert_actor, db_conn = db_conn) %>% unlist
    language_list <- input_list$language %>% map(insert_language, db_conn = db_conn) %>% unlist

    insert_junction_table <- function(tablename, source, target, db_conn) {

        if (length(target) == 0) return()

        insert_string <- target %>% 
            map(function(x) paste("insert into", tablename, "values (null,", source, ",", x, ")")) %>%
            unlist

        try(insert_string %>% map(DBI::dbExecute, conn = db_conn), silent = T)
    }

    insert_junction_table(
        tablename = "movies_to_actors", 
        source = movie_id,
        target = actor_list,
        db_conn = db_conn
    )
    insert_junction_table(
        tablename = "movies_to_directors", 
        source = movie_id,
        target = director_list,
        db_conn = db_conn
    )
    insert_junction_table(
        tablename = "movies_to_genres", 
        source = movie_id,
        target = genre_list,
        db_conn = db_conn
    )
    insert_junction_table(
        tablename = "movies_to_languages", 
        source = movie_id,
        target = language_list,
        db_conn = db_conn
    )

    return(movie_id)

}