# Load the libraries
library(dplyr)
library(wordcloud2)

# Load the dataset
apps_data <- read.csv("googleplaystore.csv", stringsAsFactors = FALSE)
reviews_data <- read.csv("googleplaystore_user_reviews.csv", stringsAsFactors = FALSE)

# Remove NAs
apps_data <- apps_data[complete.cases(apps_data), ]
reviews_data <- reviews_data[complete.cases(reviews_data), ]

# View the summary of the dataset
summary(apps_data)

# 'Reviews' column should be in numeric format. We group the column by its unique values to explore
review_counts <- apps_data %>%
  group_by(Reviews) %>%
  summarise(count = n())

# There is 1 value of 3.0M for this column.
# Hence, creating a function to remove suffix 'M' and multiplying by 1M
convert_reviews <- function(rev) {
  if (grepl("M$", rev)) {
    as.numeric(sub("M$", "", rev))
  } else {
    round(as.numeric(rev) / 1000000, 2)
  }
}

# Apply conversion function to the character column
apps_data$Reviews <- sapply(apps_data$Reviews, convert_reviews)

# Verifying if this has been corrected
review_counts_updated <- apps_data %>%
  group_by(Reviews) %>%
  summarise(count = n())

# Verifying dataset summary
summary(apps_data) # The 'Reviews' column is proper now

# Similarly, repeating this for 'Size' column
size_counts <- apps_data %>%
  group_by(Size) %>%
  summarise(count = n())

# Creating a function to handle thousands (k) and millions (M) and convert all values to Millions
convert_size <- function(size) {
  if (grepl("M$", size)) {
    as.numeric(sub("M$", "", size))
  } else if (grepl("k$", size)) {
    round(as.numeric(sub("k$", "", size)) / 1000, 2)
  } else if (grepl("[,+]", size)) {
    round(as.numeric(sub("[,+]", "", size)) / 1000000, 2)
  } else {
    as.numeric(size)
  }
}

# We also have few rows with values 'Varies with device'. Replacing them with NAs and later impute them
apps_data$Size[apps_data$Size == "Varies with device"] <- NA

# Applying conversion function to the 'Size' column
apps_data$Size <- sapply(apps_data$Size, convert_size)

# Imputings the NAs with the mean of the column
apps_data$Size[is.na(apps_data$Size)] <- mean(apps_data$Size, na.rm = TRUE)

# Verifying if this has been corrected
size_counts_updated <- apps_data %>%
  group_by(Size) %>%
  summarise(count = n())

# Verifying dataset summary
summary(apps_data) # The 'Size' column is proper now

# Similarly for the 'Installs' column
installs_counts <- apps_data %>%
  group_by(Installs) %>%
  summarise(count = n())

# Removing 1 record whose value is 'Free'
apps_data <- subset(apps_data, Installs != 'Free')

# Creating a function to remove + sign and convert the column into thousands
convert_installs <- function(installs) {
  round(as.numeric(gsub("[+,]", "", installs)) / 1000000, 2)
}

# Applying conversion function to the 'Installs' column
apps_data$Installs <- sapply(apps_data$Installs, convert_installs)

# Verifying if this has been corrected
installs_counts_updated <- apps_data %>%
  group_by(Installs) %>%
  summarise(count = n())

# Verifying dataset summary
summary(apps_data)

# Convert 'Last.Updated' column to a date field
apps_data$Last.Updated <- as.Date(apps_data$Last.Updated, format = "%B %d, %Y")

# Verifying dataset summary
str(apps_data)

# Remove the '$' symbol from the 'Price' column
apps_data$Price <- gsub("\\$", "", apps_data$Price)

# Convert the 'Price' column to numeric
apps_data$Price <- as.numeric(apps_data$Price)

str(apps_data)

# Export to csv
write.csv(apps_data, "/Users/anshita.aishwarya/Documents/Coursework/IndependentStudy/apps_data.csv")

#### Commenting this as this is being carried out in the shiny app

# # Cleaning reviews_data
# # Group by App and calculate the required values
# app_reviews <- reviews_data %>%
#   group_by(App) %>%
#   summarize(
#     Combined_Reviews = paste(Translated_Review, collapse = ", "),
#     Number_of_positive_reviews = sum(Sentiment == "Positive"),
#     Number_of_negative_reviews = sum(Sentiment == "Negative"),
#     Number_of_neutral_reviews = sum(Sentiment == "Neutral"),
#     Average_Sentiment_Polarity = mean(Sentiment_Polarity),
#     Average_Sentiment_Subjectivity = mean(Sentiment_Subjectivity)
#   )
# 
# app_reviews$Combined_Reviews <- gsub("\n", " ", app_reviews$Combined_Reviews)

# Export to csv
# write.csv(app_reviews, "/Users/anshita.aishwarya/Documents/Coursework/IndependentStudy/app_reviews.csv",
#           quote = TRUE)