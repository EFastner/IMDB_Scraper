ds.scrape_movie <- function(movieID) {
  url <- paste0("https://www.imdb.com/title/", movieID)
  
  try_count = 0
  pagecontent = character(0)
  
  #Grab the pagecontent, check to make sure the data was grabbed or try again
  while (length(pagecontent) == 0 & try_count < 6) {
    pagecontent <- 
      html_session(url) %>% 
      html_nodes("[class = 'pagecontent']")
    
    try_count <- try_count + 1 
  }
  
  #If the function was not able to get the movie content, return nothing
  if (length(pagecontent) == 0) {
    return(character(0))
  }
  
#CREDIT_LISTS####################################################
  credit_list <- 
    pagecontent %>% 
    html_nodes("[class = 'credit_summary_item']")
  
  #Grab Directors
  director <- 
    credit_list[[1]] %>% 
    html_nodes("a") %>% 
    html_text()
  
  director <- 
    director[sapply(director, function(x) !grepl("more credit", x))] %>%
    paste(collapse = ", ")
  
  #Grab Writers
  writer <- 
    credit_list[[2]] %>%
    html_nodes("a") %>%
    html_text()
  
  writer <- 
    writer[sapply(writer, function(x) !grepl("more credit", x))] %>%
    paste(collapse = ", ")
  
#DETAILS#####################################################
  try_count = 0
  title_details_text <- character(0)
  
  #Try to grab the title details
  while (length(title_details_text) == 0 & try_count < 6) {
    title_details <- 
      pagecontent %>% 
      html_nodes("[id = 'titleDetails']") %>%
      html_nodes("div")
    
    title_details_text <- title_details %>% html_text()
    
    try_count <- try_count + 1
    }
  
  #If the function was not able to retrieve the title details, return nothing
  if (length(title_details_text) == 0) {
    return(character(0))
  }

  #Grab Budget
  scrape_row <- title_details_text[sapply(title_details_text, function(x) grepl("Budget", x))]

  budget <- 
    gsub(paste("Budget:","\n","\\(estimated\\)"," ", sep = "|"),"", scrape_row)
  
  #If the budget doesn't exist, change it to NA
  if (length(budget) == 0) {
    budget = NA
  }
  
  #Grab Runtime
  runtime <- title_details %>% html_nodes("time") %>% html_text(trim = TRUE)
  runtime <- as.numeric(gsub(" min", "", runtime[1]))
  
  #If the runtime did not exist, grab from header and convert the result to minutes
  if (length(runtime) == 0) {
    
    runtime_hrs_mins <- 
      pagecontent %>% 
      html_node("time") %>% 
      html_text(trim = TRUE)
    
    hours_stop <- regexpr("h", runtime_hrs_mins)
    
    runtime_hrs <- 
      as.numeric(substr(runtime_hrs_mins, 1, hours_stop - 1))
    
    runtime_mins <- 
      as.numeric(substr(runtime_hrs_mins, 
                        hours_stop + 2,
                        regexpr("min",runtime_hrs_mins) - 1))
    
    runtime <- (runtime_hrs * 60) + runtime_mins
  }
  
  #Compile all data in a vector and name each column
  movie_data <- c(director, writer, budget, runtime)
  names(movie_data) <- c("director", "writer", "budget", "runtime")
  
  return(movie_data)
}