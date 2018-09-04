require(rvest)

url <- 'https://www.imdb.com/chart/top'

error_list <- c()
full_list <- html_session(url) %>% html_nodes("tr")

movie_list <- full_list %>% 
  html_nodes("[class = 'titleColumn']")

link_list <- full_list %>% 
  html_nodes("[class = 'wlb_ribbon']") %>%
  xml_attr("data-tconst")

#Grab title, year, and rating
title <- movie_list %>% html_nodes("a") %>% html_text()
year <- movie_list %>% html_nodes("span") %>% html_text() %>% substr(2,5)
rating <- full_list %>% html_nodes("strong") %>% html_text() %>% as.numeric()

#Compile data in a data frame
top_movies <- data.frame(ID = link_list, title, year, rating)

#IN DEVELOPMENT - Cycle through each top movie to compile more data
for (i in 1:nrow(top_movies)) {
  
  movie_data <- ds.scrape_movie(top_movies[i, "ID"]) #Call movie scraping function
  
  #If data was returned from the function, add it to the existing table
  if (length(movie_data) != 0) {
    top_movies[i, "director"] <- movie_data["director"]
    top_movies[i, "writer"] <- movie_data["writer"]
    top_movies[i, "budget"] <- movie_data["budget"]
    top_movies[i, "runtime"] <- as.numeric(movie_data["runtime"])
    
    print(paste(i, as.character(top_movies[i, "title"]), sep = ": "))
  } 
  else {
    error_list <- append(error_list, top_movies[i, "title"]) #Compile any movies that data was not returned for
    
    print(paste0("Error at movie ", i, ": ", top_movies[i, "title"]))
  }
}

#If there were any errors, print out the results
if (!is.null(error_list)) {
  print(paste0("There were ", length(error_list), " movies that were not retrieved. Viewing now..."))
  View(error_list)
}