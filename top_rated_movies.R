require(rvest)

url <- 'https://www.imdb.com/chart/top'

full_list <- html_session(url) %>% html_nodes("tr")

movie_list <- full_list %>% 
  html_nodes("[class = 'titleColumn']")

link_list <- full_list %>% 
  html_nodes("[class = 'wlb_ribbon']") %>%
  xml_attr("data-tconst")

title <- movie_list %>% html_nodes("a") %>% html_text()
year <- movie_list %>% html_nodes("span") %>% html_text() %>% substr(2,5)
rating <- full_list %>% html_nodes("strong") %>% html_text() %>% as.numeric()

top_movies <- data.frame(ID = link_list, title, year, rating)

#IN DEVELOPMENT - Cycle through each top movie to compile more data
for (i in 1:nrow(top_movies)) {
  print(i)

  movie_data <- ds.scrape_movie(top_movies[i, "ID"])

  top_movies[i, "budget"] <- movie_data["budget"]
  top_movies[i, "runtime"] <- movie_data["runtime"]

}
