ds.scrape_movie <- function(movieID) {
  url <- paste0("https://www.imdb.com/title/", movieID)
  
  pagecontent <- 
    html_session(url) %>% 
    html_nodes("[class = 'pagecontent']")
  
#CREDIT_LISTS####################################################
  credit_list <- 
    pagecontent %>% 
    html_nodes("[class = 'credit_summary_item']")
  
  #Grab Directors
  director <- 
    credit_list[1] %>% 
    html_nodes("a") %>% 
    html_text()
  
  director <- director[sapply(director, function(x) !grepl("more credit", x))]
  
  #Grab Writers
  writer <- 
    credit_list[2] %>%
    html_nodes("a") %>%
    html_text()
  
  writer <- writer[sapply(writer, function(x) !grepl("more credit", x))]
  
#DETAILS#####################################################
  title_details <- 
    pagecontent %>% 
    html_nodes("[id = 'titleDetails']") %>%
    html_nodes("div") %>% html_text(trim = TRUE)

  #Grab Budget
  scrape_row <- title_details[sapply(title_details, function(x) grepl("Budget", x))]
  budget <- gsub(paste("Budget:","\n","\\(estimated\\)"," ", sep = "|"),"", scrape_row)
  
  #If the budget doesn't exist, change it to NA
  if (length(budget) == 0) {
    budget = NA
  }
  
  #Grab Runtime
  scrape_row <- title_details[sapply(title_details, function(x) grepl("Runtime", x))]
  runtime <- gsub(paste("Runtime:","\n", "min", " ", "|", sep = "|"),"", scrape_row)
  
  #If the runtime did not exist, grab from header and convert
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
  
  
  movie_data <- c(budget, runtime)
  names(movie_data) <- c("budget", "runtime")
  
  return(movie_data)
}