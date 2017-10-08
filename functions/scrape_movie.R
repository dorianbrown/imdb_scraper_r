# All our helper functions

search_movie <- function(movie_name) {
    url_name <- str_replace_all(movie_name, ' ', '+')
    html <- read_html(paste0("http://www.imdb.com/find?ref_=nv_sr_fn&q=",url_name,"&s=all"))
    new_link <- html %>% html_nodes('#findSubHeader+ .findSection .odd:nth-child(1) a') %>% html_attr('href')
    return(read_html(paste0("http://www.imdb.com/",new_link[1])))
}

clean_str <- function(string) str_replace(string,"'","")

get_year <- function(profile_html, return = T) {
    year <- profile_html %>% 
        html_node('h1') %>% 
        html_text %>% 
        trimws %>% 
        substring(nchar(.) - 4, nchar(.) - 1) %>%
        as.numeric

    if (suppressWarnings(year %>% as.numeric %>% is.na)) {
        year <- profile_html %>% 
            html_node('#title-overview-widget a:nth-child(8)') %>%
            html_text %>%
            str_extract("([0-9]{4})") %>%
            as.numeric
        return(year)
    }
    return(year)
}

get_html_node <- function(profile_html, css) {
    profile_html %>%
        html_nodes(css) %>% 
        html_text
}

get_rating_votes <- function(profile_html) {
    get_html_node(profile_html, css = '.imdbRating') %>% 
        str_extract('[0-9]+,[0-9]+') %>%
        str_replace(",","") %>%
        as.numeric()
}

# get_oscars <- function(profile_html) {
#     num_oscars <- get_html_node(profile_html, css = '#titleAwardsRanks b') %>% str_extract('[0-9]+')
    
#     if (length(num_oscars) == 0) {
#         return(0)
#     } else {
#         return(num_oscars)
#     }
# }

medium_type <- function(profile_html) {
    tv_string <- get_html_node(profile_html, css = '#title-episode-widget div:nth-child(1) .float-left')

    if ("Seasons" %in% tv_string) {
        return("TV Show")
    } else {
        return("Movie")
    }
}

# A few little helper functions to keep the next part readable
first_ten <- function(x) x[1:min(10,length(x))]
every_other <- function(x) {
    if (length(x) > 0) {
        return(x[seq(1,length(x),2)])
    } else {
        return(NULL)
    }
}
"%.%" <- compose # We do this so we don't need pryr package.

get_rating <- as.numeric %.% partial(get_html_node, css = 'strong span')
get_director <- clean_str %.% partial(get_html_node, css = '.summary_text+ .credit_summary_item .itemprop')
get_writer <- clean_str %.% partial(get_html_node, css = '.credit_summary_item:nth-child(3) .inline+ span .itemprop')
get_language <- clean_str %.% partial(get_html_node, css = '#titleDetails .txt-block:nth-child(5) a')
get_genres <- clean_str %.% trimws %.% partial(get_html_node, css = '.txt-block~ .canwrap a')
get_actors <- clean_str %.% first_ten %.% every_other %.% trimws %.% partial(get_html_node, css = '#titleCast .itemprop')


get_similar <- function(profile_html) {
    tmp_df <- profile_html %>% 
        html_nodes('.rec_poster_img') %>% 
        html_attrs %>% data.frame(stringsAsFactors = F)
    
    tmp_df[3,1:(0.5*ncol(tmp_df))] %>% as.character %>% clean_str
}

scrape_movie <- function(movie_name) {

    message(movie_name)

    movie_html <- search_movie(movie_name)

    output <- list(
        title = movie_name,
        year = get_year(movie_html),
        rating = get_rating(movie_html),
        rating_votes = get_rating_votes(movie_html),
        director = get_director(movie_html),
        writer = get_writer(movie_html),
        language = get_language(movie_html),
        actors = get_actors(movie_html),
        genres = get_genres(movie_html),
        similar = get_similar(movie_html),
        medium_type = medium_type(movie_html)
    )

    return(output)
}