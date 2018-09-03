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
  
  scrape_row <- title_details[sapply(title_details, function(x) grepl("Budget", x))]
  budget <- gsub(paste("Budget:","\n","\\(estimated\\)"," ", sep = "|"),"", scrape_row)
  
  scrape_row <- title_details[sapply(title_details, function(x) grepl("Runtime", x))]
  runtime <- gsub(paste("Runtime:","\n", "min", " ", sep = "|"),"", scrape_row)
  
  return(c(budget, runtime))
}