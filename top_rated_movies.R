require(rvest)

url <- 'https://www.imdb.com/chart/top'

error_list <- c()
full_list <- html_session(url) %>% html_nodes("tr")

movie_list <- full_list %>% 
  html_nodes("[class = 'titleColumn']")

link_list <- full_list %>% 
  html_nodes("[class = 'wlb_ribbon']") %>%
  xml_attr("data-tconst")

#Compile data in a data frame
movieIDs <- data.frame(ID = link_list)

#IN DEVELOPMENT - Cycle through each top movie to compile more data
for (i in 1:nrow(movieIDs)) {
  
  movie_data <- ds.scrape_movie(movieIDs[i, "ID"]) #Call movie scraping function
  
  #If data was returned from the function, add it to the existing table
  if (length(movie_data) != 0) {
    top_movies[i, "title"] <- movie_data["title"]
    top_movies[i, "year"] <- movie_data["year"]
    top_movies[i, "rating"] <- movie_data["rating"]
    top_movies[i, "director"] <- movie_data["director"]
    top_movies[i, "writer"] <- movie_data["writer"]
    top_movies[i, "budget"] <- movie_data["budget"]
    top_movies[i, "runtime"] <- as.numeric(movie_data["runtime"])
    top_movies[i, "genres"] <- movie_data["genres"]

    print(paste(i, as.character(top_movies[i, "title"]), sep = ": "))
  } 
  else {
    error_list <- append(error_list, i) #Compile any movies that data was not returned for
    
    print(paste0("Error at movie ", i))
  }
}

#If there were any errors, print out the results
if (!is.null(error_list)) {
  print(paste0("There were ", length(error_list), " movies that were not retrieved. Viewing now..."))
  View(error_list)
}