library(rvest)
library(stringr)
library(purrr)
library(RSQLite)
library(dplyr)

str_replace_all()

db_conn <- dbConnect(RSQLite::SQLite(), "~/imdb.db")

source('functions/scrape_movie.R')
source('functions/add_to_database.R')
source('functions/recreate_tables.R')

# Destroy and recreate all tables in sqlite db.
recreate_tables(db_conn)

# First iteration

# TODO: Exception for TV Shows

input_list <- scrape_movie("The Man in the Iron Mask")

movie_id <- add_to_database(input_list, db_conn)

current_movies <- data.frame(
	source = movie_id, 
	target = input_list$similar,
	stringsAsFactors = F
)

# Recursion for further iterations

while(TRUE) { # The main loop for the graph traversal

	# We build in a delay so that we don't get blocked from imdbb	

	next_movies <- data.frame()

	for (i in 1:nrow(current_movies)) {

		source_id <- current_movies[i,1]
		target_name <- current_movies[i,2]

		count_movie <- dbGetQuery(db_conn, paste0(
			"select 
				count(*) as count 
			from movies 
			where 
				title = '", target_name, "';"))
		if (count_movie$count > 0) next

		input_list <- tryCatch(scrape_movie(target_name), error = function(e) {return(NULL)})

		if (input_list %>% is.null) next

		movie_id <- add_to_database(input_list, db_conn)

		# Insert into mov2mov (source_id, movie_id)

		insert_junction_table(
	        tablename = "movies_to_movies",
	        source = source_id,
	        target = movie_id,
	        db_conn = db_conn
	    )
		# Add movie_id, input_list$similar to next_movies

		if (length(input_list$similar) > 0) {
			next_movies <- data.frame(
				source = movie_id, 
				target = input_list$similar,
				stringsAsFactors = F
			) %>% bind_rows(next_movies)
		}
	}
	# Call this function again with next_movies as input
	current_movies <- next_movies
}
