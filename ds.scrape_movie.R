ds.scrape_movie <- function(movieID) {
  require(rvest)
  
  pagecontent <- ds.get_movie_page(movieID)
  
  wrap_values <- ds.scrape_title_wrap(pagecontent)
  
  credit_list <- ds.scrape_credit_list(pagecontent)

  storyline <- ds.scrape_storyline(pagecontent)
  
  detail_list <- ds.scrape_details(pagecontent)

  #Compile all data in a vector and name each column
  movie_data <- 
    c(wrap_values,
      credit_list,
      detail_list,
      storyline)

  return(movie_data)
}

ds.get_movie_page <- function(movieID, try_count = 5) {
#DESCRIPTION: Get initial movie page html data
#ARGUMENTS: movieID: ID of movie, try_count = number of tries before returning error
  
  url <- paste0("https://www.imdb.com/title/", movieID)

  pagecontent = character(0)
  
  #Grab the pagecontent, check to make sure the data was grabbed or try again
  while (length(pagecontent) == 0 & try_count > 0) {
    pagecontent <- 
      html_session(url) %>% 
      html_nodes("[class = 'pagecontent']")
    
    try_count <- try_count - 1 
  }
  
  #If the function was not able to get the movie content, return nothing
  if (length(pagecontent) == 0) {
    return(character(0))
  }
  
  return(pagecontent) 
}

ds.scrape_title_wrap <- function(pagecontent) {
#DESCRIPTION: Scrape the title_wrap section of IMDB Page
#ARGUMENTS: pagecontent = full page html pulled with ds.get_movie_page
  
  title_wrap <- ds.get_node_text(pagecontent, "[class = 'title_wrapper']", try_count = 5)
  
  # try_count = 0
  # title_wrap = character(0)
  # 
  # while (length(title_wrap) == 0 & try_count <6) {
  #   title_wrap <- 
  #     pagecontent %>%
  #     html_node("[class = 'title_wrapper']")
  #   
  #   try_count <- try_count + 1
  # }
  # 
  # if (length(title_wrap) == 0) {
  #   return(character(0))
  # }
  
  if (length(title_wrap) != 0) {
    title <- 
      gsub("\\S\\(\\d+\\)\\s+$","", title_wrap %>% html_node("h1") %>% html_text())
    
    year <- 
      title_wrap %>% 
      html_node("[id = 'titleYear']") %>% 
      html_node("a") %>% 
      html_text()
    
    rating <- 
      pagecontent %>%
      html_node("strong span") %>%
      html_text()
    
    wrap_values <- c(title, year, rating)
    names(wrap_values) <- c("title", "year", "rating")
    
    return(wrap_values)
  }
  else (
    return(character(0))
  )
}

ds.scrape_credit_list <- function(pagecontent) {
#DESCRIPTION: Scrape the credit list section of IMDB Page
#ARGUMENTS: pagecontent = full page html pulled with ds.get_movie_page
  
  credit_list <- ds.get_node_text(pagecontent, "[class = 'credit_summary_item']", try_count = 5)
  
  if (length(credit_list) != 0) {
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
    
    credit_items <- c(director, writer)
    names(credit_items) <- c("director", "writer")
    
    return(credit_items)
  } 
  else {
    return(character(0))
  }
}

ds.scrape_storyline <- function(pagecontent) {
#DESCRIPTION: Scrape the storyline section of IMDB Page
#ARGUMENTS: pagecontent = full page html pulled with ds.get_movie_page
  
  storyline <- ds.get_node_text(pagecontent, "[class = 'see-more inline canwrap']", try_count = 5)
  
  # try_count <- 0
  # storyline <- character(0)
  # 
  # while (length(storyline) == 0 & try_count < 6) {
  #   storyline <- 
  #     pagecontent %>% 
  #     html_nodes("[class = 'see-more inline canwrap']") %>%
  #     html_text()
  #   
  #   try_count <- try_count + 1
  # }
  # 
  # if (length(storyline) == 0) {
  #   return(character(0))
  # }
  
  if (length(storyline) != 0) {
    scrape_row <- 
      storyline[sapply(storyline, function(x) grepl("Genres", x))] %>% html_text()
    
    genres <- 
      gsub("\\S\\|", ", ", gsub(paste("Genres:", "\n", "\\s+", sep = "|"),"", scrape_row)) 
    
    storyline_items <- c(genres)
    names(storyline_items) <- c("genres")
    
    return(storyline_items)
  }
  else {
    return(character(0))
  }
}

ds.scrape_details <- function(pagecontent) {
#DESCRIPTION: Scrape the details section of IMDB Page
#ARGUMENTS: pagecontent = full page html pulled with ds.get_movie_page
  
  title_details_text <- ds.get_node_text(pagecontent, "[id = 'titleDetails'] div", try_count = 5)
  
  # try_count = 0
  # title_details_text <- character(0)
  # 
  # #Try to grab the title details
  # while (length(title_details_text) == 0 & try_count < 6) {
  #   title_details <- 
  #     pagecontent %>% 
  #     html_nodes("[id = 'titleDetails']") %>%
  #     html_nodes("div")
  #   
  #   title_details_text <- title_details %>% html_text()
  #   
  #   try_count <- try_count + 1
  # }
  # 
  # #If the function was not able to retrieve the title details, return nothing
  # if (length(title_details_text) == 0) {
  #   return(character(0))
  # }
  
  if (length(title_details_text) != 0) {
    #Grab Budget
    scrape_row <- 
      title_details_text[sapply(title_details_text, function(x) grepl("Budget", x))] %>% html_text()
    
    budget <- 
      gsub(paste("Budget:","\n","\\(estimated\\)"," ", sep = "|"),"", scrape_row)
    
    #If the budget doesn't exist, change it to NA
    if (length(budget) == 0) {
      budget = NA
    }
    
    #Grab Runtime
    runtime <- title_details_text %>% html_nodes("time") %>% html_text(trim = TRUE)
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
    
    detail_items <- c(budget, runtime)
    names(detail_items) <- c("budget", "runtime")
    
    return(detail_items)
  }
  else {
    return(character(0))
  }
}

ds.get_node_text <- function(full_page, node_name, try_count = 5) {
#DESCRIPTION: Convert given html node into text, check to make sure the text is extracted
#ARGUMENTS: full_page = full html page called with ds.get_movie_page, node_name = desired node, try_count = number of trys to attempt before returning nothing
  
  html_page <- character(0)
  
  while (length(html_page) == 0 & try_count > 0) {
    html_page <- 
      full_page %>% 
      html_nodes(node_name)
    
    try_count <- try_count - 1
  }
  
  if (length(html_page) == 0) {
    return(character(0))
  }
  
  return(html_page)
}
