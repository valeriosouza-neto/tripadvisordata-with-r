install.packages("rvest", "httr", "dplyr", "purr", "stringr")
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
    
    # Extracting Place of Origin and Contributions
    place_contributions <- .x %>% html_element(".JINyA div.biGQs._P.pZUbB.osNWb") %>% html_text(trim = TRUE)
    
    # Split the string to extract Place of Origin and Contributions
    place_contributions_parts <- str_split(place_contributions, " • ")
    
    place_of_origin <- place_contributions_parts[[1]]
    
    # If there's only one part, assume it's the Place of Origin and set Contributions to NA
    contributions <- ifelse(length(place_contributions_parts) == 2, place_contributions_parts[[2]], NA)
    
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
      PlaceOfOrigin = place_of_origin,
      Contributions = contributions,
      Evaluation = evaluation,
      ReviewTitle = title,
      ReviewText = review_text,
      ReviewUsefulness = review_usefulness,
      VisitationDate = visitation_date,
      TouristType = tourist_type,
      ReviewDate = review_date
    )
  }) %>% bind_rows() # Combine all tibbles into one
  
  return(review_data)
}

# Initial setup
base_url <- "https://www.tripadvisor.com.au/AttractionProductReview-g255069-d11452933-Sunlover_Reef_Cruises_Cairns_Great_Barrier_Reef_Experience-Cairns_Cairns_Region_Qu.html"
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


# Data cleaning and transforming

# Extract numeric part from "Evaluation" column
final_reviews$Evaluation <- as.numeric(str_extract(final_reviews$Evaluation, "\\d+\\.\\d+"))

# Define a function to map tourist types to numbers
map_tourist_type <- function(type) {
  if (is.na(type)) {
    return(0)
  } else if (type == "Solo") {
    return(1)
  } else if (type == "Couples") {
    return(2)
  } else if (type == "Friends") {
    return(3)
  } else if (type == "Family") {
    return(4)
  } else {
    return(NA)  # Return NA for unknown types
  }
}

# Apply the function to create the new variable
final_reviews$TouristType2 <- sapply(final_reviews$TouristType, map_tourist_type)

# Parse the ReviewDate column
date_parts <- str_match(final_reviews$ReviewDate, "Written (\\d+) (\\w+) (\\d+)")

# Extract day, month, and year
final_reviews$ReviewDateDay <- as.integer(date_parts[, 2])
final_reviews$ReviewDateMonth <- match(tolower(date_parts[, 3]), tolower(month.name))
final_reviews$ReviewDateYear <- as.integer(date_parts[, 4])


# Define a function to convert month names to numbers
month_to_number <- function(month_name) {
  month_number <- match(month_name, c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))
  return(ifelse(is.na(month_number), 0, month_number))
}

# Extract month and year from "VisitationDate" column
final_reviews <- final_reviews %>%
  mutate(VisitationDateMonth = month_to_number(str_extract(VisitationDate, "^[A-Za-z]+")),
         VisitationDateYear = as.integer(str_extract(VisitationDate, "\\d{4}")))

# Fill NAs in VisitationDateYear with 0
final_reviews$VisitationDateYear[is.na(final_reviews$VisitationDate)] <- 0

# Function to extract contributions from PlaceOfOrigin column
extract_contributions <- function(place_of_origin) {
  # Extract numeric part from the beginning of the string
  contributions <- str_extract(place_of_origin, "\\d+ contribution[s]?")
  # Remove the extracted contribution part from the PlaceOfOrigin column
  place_of_origin <- str_remove(place_of_origin, "\\d+ contribution[s]?")
  # Remove leading and trailing whitespace
  place_of_origin <- str_trim(place_of_origin)
  # If contributions found, return them, otherwise return NA
  if (!is.na(contributions)) {
    return(list(place_of_origin = place_of_origin, contributions = contributions))
  } else {
    return(list(place_of_origin = place_of_origin, contributions = NA))
  }
}

# Apply the function to the PlaceOfOrigin column and create Contributions column
contributions_info <- sapply(final_reviews$PlaceOfOrigin, extract_contributions)
final_reviews$PlaceOfOrigin <- contributions_info["place_of_origin", ]
final_reviews$Contributions <- contributions_info["contributions", ]


# Remove contributions from PlaceOfOrigin column
final_reviews$PlaceOfOrigin <- gsub("\\d+ contributions", "", final_reviews$PlaceOfOrigin)

# Replace blank spaces in PlaceOfOrigin column with NA
final_reviews$PlaceOfOrigin <- ifelse(final_reviews$PlaceOfOrigin == "", NA, final_reviews$PlaceOfOrigin)

# Extract numeric part from Contributions column
final_reviews$Contributions <- as.numeric(gsub("\\D", "", final_reviews$Contributions))


# Display or write the final_reviews dataframe to a file
print(final_reviews)

# Write to a CSV file
write.csv(final_reviews, "tripadvisor_reviews.csv", row.names = FALSE)
