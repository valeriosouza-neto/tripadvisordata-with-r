library(rvest)
library(httr) # For handling HTTP requests
library(dplyr)
library(purrr) # For list manipulation and iteration
library(stringr) # For string extraction

scrape_page <- function(url) {
  user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"
  
  page <- read_html(httr::GET(url, user_agent(user_agent)))
  
  reviews <- page %>% html_elements(".LbPSX div._c") # Adjust the selector as needed
  
  # Process each review individually
  review_data <- map(reviews, ~{
    username <- .x %>% html_element("span a.BMQDV") %>% html_text(trim = TRUE)
    contributions <- .x %>% html_element(".JINyA span") %>% html_text(trim = TRUE)
    evaluation <- .x %>% html_element("svg.UctUV") %>% html_attr("aria-label")
    title <- .x %>% html_element(".BMQDV span") %>% html_text(trim = TRUE)
    review_text <- .x %>% html_element(".JguWG span") %>% html_text(trim = TRUE)
    visitation_date <- .x %>% html_element("div.RpeCd") %>% html_text(trim = TRUE) %>% stringr::str_extract("^([^•]+)")
    tourist_type <- .x %>% html_element("div.RpeCd") %>% html_text(trim = TRUE) %>% stringr::str_extract("(?<= • ).*")
    review_usefulness <- .x %>% html_element("span.FwFXZ") %>% html_text(trim = TRUE)
    review_date <- .x %>% html_element("div.pZUbB.ncFvv") %>% html_text(trim = TRUE)
    
    # Create a tibble for each review
    tibble(
      Username = username,
      Contributions = contributions,
      Evaluation = evaluation,
      Title = title,
      ReviewText = review_text,
      VisitationDate = visitation_date,
      TouristType = tourist_type,
      ReviewUsefulness = review_usefulness,
      ReviewDate = review_date
    )
  }) %>% bind_rows() # Combine all tibbles into one
  
  return(review_data)
}

# Initial setup
base_url <- "" # Add the website of the attraction you want to collect the data
reviews_per_page <- 10
request_interval_ms <- 4000 # Time between requests in milliseconds, corrected
page_load_delay_ms <- 4000 # Time to wait for page load in milliseconds, corrected

# Initialize an empty list to store review data from all pages
all_reviews <- list()

# Pagination setup
current_url <- base_url
page_index <- 0 # Tracks the pagination offset
has_reviews <- TRUE

while (has_reviews) {
  # Scrape the current page
  Sys.sleep(page_load_delay_ms / 1000) # Wait for the page to load
  page_reviews <- scrape_page(current_url)
  all_reviews[[length(all_reviews) + 1]] <- page_reviews
  
  # Check if there are reviews on the current page; if not, exit the loop
  if (nrow(page_reviews) == 0) {
    has_reviews <- FALSE
  } else {
    # Prepare the URL for the next page
    page_index <- page_index + reviews_per_page
    current_url <- sub("d11452933-", sprintf("d11452933-or%d-", page_index), base_url)
    
    # Delay before the next request, if there are more reviews
    Sys.sleep(request_interval_ms / 1000)
  }
}

# Combine all review data into a single dataframe
final_reviews <- bind_rows(all_reviews)

# Display or write the final_reviews dataframe to a file
print(final_reviews)
# Optionally, write to a CSV file
# write.csv(final_reviews, "tripadvisor_reviews.csv", row.names = FALSE)
