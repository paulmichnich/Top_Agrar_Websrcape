## Load necessary libraries
library("rvest")
library("dplyr")

## Replace with your path
save.path <- ""

## Define the base URL (replace the year dynamically in the loop)
base_url <- "https://www.topagrar.com/heftarchiv/?year="

## Create an empty data frame to store article titles and their associated URLs
article_data <- data.frame(Year = integer(), Heftausgabe_URL = character(), Article_Title = character(), stringsAsFactors = FALSE)

## Create an empty data frame to store any errors that occur during the process
error_log <- data.frame(Year = integer(), Heftausgabe_URL = character(), Error_Message = character(), stringsAsFactors = FALSE)

## Loop through the years 2000 to 2025
for (year in 2000:2025) {
  ## Construct the URL for each year
  year_url <- paste0(base_url, year)
  cat("Processing year:", year, "URL:", year_url, "\n")
  
  ## Read the HTML content of the year's archive page
  webpage <- read_html(year_url)
  
  ## Extract all the Heftausgabe links
  links <- webpage %>% html_nodes("a") %>% html_attr("href")
  
  ## Filter out only the links that contain "heftausgabe" or a relevant keyword
  heftausgabe_links <- links[grepl("heftausgabe", links, ignore.case = TRUE)]
  
  ## Complete the URLs if they are relative links (start with "/")
  heftausgabe_links <- ifelse(grepl("^https?://", heftausgabe_links),
                              heftausgabe_links,
                              paste0("https://www.topagrar.com", heftausgabe_links))
  
  ## Loop through each "Heftausgabe" link within the year and scrape article titles
  for (heftausgabe_url in heftausgabe_links) {
    cat("Processing Heftausgabe:", heftausgabe_url, "\n")
    
    ## Use tryCatch to handle potential errors
    tryCatch({
      ## Read the content of each Heftausgabe page
      heftausgabe_page <- read_html(heftausgabe_url)
      
      ## Extract the article titles (assuming titles are within <h2> tags, adjust if necessary)
      article_titles <- heftausgabe_page %>% html_nodes("h2") %>% html_text()
      
      ## Create a temporary data frame for the current Heftausgabe articles
      temp_data <- data.frame(Year = year, Heftausgabe_URL = heftausgabe_url, Article_Title = article_titles, stringsAsFactors = FALSE)
      
      ## Combine the temporary data with the main data frame
      article_data <- bind_rows(article_data, temp_data)
      
    }, error = function(e) {
      ## In case of error, log the year, URL, and error message
      cat("Error in processing Heftausgabe:", heftausgabe_url, "\n", "Error message:", e$message, "\n")
      
      ## Append error details to the error log data frame
      error_log <<- bind_rows(error_log, data.frame(Year = year, Heftausgabe_URL = heftausgabe_url, Error_Message = e$message, stringsAsFactors = FALSE))
    })
  }
}

article_data$Article_Title <- trimws(gsub("\\n", "", article_data$Article_Title))

## Print out the scraped article titles for all years
print(article_data)

## Optionally, save the data to a CSV file
write.csv(article_data, paste0(save.path, "/01_Data_Code/02_Data/heftausgabe_article_titles_2000_2024.csv"), row.names = FALSE)

## Save the error log to a separate CSV file for reference
write.csv(error_log, paste0(save.path, "/01_Data_Code/02_Data/heftausgabe_error_log_2000_2024.csv"), row.names = FALSE)

cat("Scraping complete! The article titles have been saved in 'heftausgabe_article_titles_2000_2025.csv'.\n")
cat("Errors encountered have been logged in 'heftausgabe_error_log_2000_2025.csv'.\n")
