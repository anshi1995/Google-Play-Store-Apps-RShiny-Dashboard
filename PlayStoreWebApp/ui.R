library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(tm)
library(wordcloud2)
library(shinyWidgets)

# Load the datasets
apps_data <- read.csv("apps_data.csv", stringsAsFactors = FALSE)
app_reviews <- read.csv("googleplaystore_user_reviews.csv", stringsAsFactors = FALSE)
app_reviews <- app_reviews[complete.cases(app_reviews), ]

# Format date in Y-m-d format
apps_data$Last.Updated <- as.Date(apps_data$Last.Updated, format = "%m/%d/%y")

# Group by App and calculate the required values
# Combining all reviews for each app for easy analysis
reviews_data <- app_reviews %>%
  group_by(App) %>%
  summarize(
    Combined_Reviews = paste(Translated_Review, collapse = ", "),
    Number_of_positive_reviews = sum(Sentiment == "Positive"),
    Number_of_negative_reviews = sum(Sentiment == "Negative"),
    Number_of_neutral_reviews = sum(Sentiment == "Neutral"),
    Average_Sentiment_Polarity = mean(Sentiment_Polarity),
    Average_Sentiment_Subjectivity = mean(Sentiment_Subjectivity)
  )

# UI

# Header
header <- dashboardHeader(title = "Google Play Store Apps Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("App Statistics Dashboard", tabName = "dashboard", selected = TRUE),
    menuItem("Sentiment Analysis Dashboard", tabName = "review_analysis"),
    menuItem("View Data", tabName = "raw_data")
  ),
  conditionalPanel(
    condition = "input.tabs == 'dashboard'",
    selectInput("category_select", "Choose a Category to filter scorecards and bar chart:",
                choices = c("All",unique(apps_data$Category)),
                selected = "All")
  ),
  conditionalPanel(
    condition = "input.tabs == 'review_analysis'",
    selectInput("app_select", "Choose an App to filter the dashboard:", choices = unique(reviews_data$App))
  )
)
  
# Body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              column(width = 2.5,
                     valueBoxOutput("Number_of_Apps")),
              column(width = 2.5,
                     valueBoxOutput("Avg_Rating")),
              column(width = 2.5,
                     valueBoxOutput("Total_Reviews")),
              column(width = 2.5,
                     valueBoxOutput("Total_Size")),
              column(width = 2.5,
                     valueBoxOutput("Total_Installs")),
              column(width = 2.5,
                     valueBoxOutput("Avg_Price"))
              
            ),
            fluidRow(
              column(width = 6.5,
                     box(title = "Apps Distribution over Genres",
                         status = "success",
                         height = "520px",
                         solidHeader = T,
                         plotlyOutput("barchart"))
              ),
              column(width = 6.5,
                     box(title = "Apps Updated over Time",
                         status = "primary",
                         height = "520px",
                         solidHeader = T,
                         radioButtons("time_period", "Choose the Time Period for line chart:",
                                      choices = c("Yearly", "Monthly", "Daily"),
                                      selected = "Yearly", inline = TRUE),
                         plotlyOutput("linechart")
                        )
                    )
            )
    ),
    tabItem(
      tabName = "review_analysis",
      fluidRow(
        valueBoxOutput("sentiment_polarity"),
        valueBoxOutput("avg_sentiment"),
        valueBoxOutput("avg_sentiment_subjectivity")
      ),
      fluidRow(
        column(
          width = 12,
          tags$p("Sentiment indicates the overall emotional tone of the reviews.
                 It can be positive, negative, or neutral.
                 The sentiment score is determined based on the average sentiment polarity value.",
                 HTML(paste0("<span style='color:green;font-weight:bold;'>Positive</span>")),
                 " sentiment is assigned when ",
                 HTML(paste0("<span style='font-weight:bold;'>sentiment polarity is greater than 0. </span>")),
                 HTML(paste0("<span style='color:red;font-weight:bold;'>Negative</span>")),
                 " sentiment is assigned when ",
                 HTML(paste0("<span style='font-weight:bold;'>sentiment polarity is less than 0. </span>")),
                 HTML(paste0("<span style='color:blue;font-weight:bold;'>Neutral</span>")),
                 " sentiment is assigned when ",
                 HTML(paste0("<span style='font-weight:bold;'>sentiment polarity is equal to 0. </span>")),
                 HTML("<br><br>"),
                 "Sentiment subjectivity indicates the degree of objectivity or subjectivity in the reviews. ",
                 HTML(paste0("<span style='color:purple;font-weight:bold;'>Higher subjectivity </span>")),
                 "indicates that the sentiment is based on personal opinions or experiences, while ",
                 HTML(paste0("<span style='color:#e6005c;font-weight:bold;'>lower subjectivity </span>")),
                 "indicates a more factual or unbaised sentiment. This numerical score ranges from 0 to 1
                 or 0 to 100 where 0 indicates complete objectivity and 1 or 100 indicates complete subjectivity",
                 style = "text-align: center; font-size: 13px; margin-top: 10px;")
        )
      ),
      fluidRow(
        column(width = 6.5,
               box(
                 plotlyOutput("review_bar"))
               ),
        column(width = 6.5,
               box(
                 wordcloud2Output("word_cloud")
               ))
        
        ),
      fluidRow(
        column(
          width = 12,
          tags$p("Check for Apps such as ",
                 HTML(paste0("<span style='font-weight:bold;'>'Amtrak'</span>")),
                 " or ",
                 HTML(paste0("<span style='font-weight:bold;'>'Air Asia'</span>")),
                 " to see the ",
                 HTML(paste0("<span style='color:red;font-weight:bold;'>Negative Sentiments.</span>")),
                 HTML("<br><br>"),
                 "Also, check for ",
                 HTML(paste0("<span style='font-weight:bold;'>'CBS News'</span>")),
                 " or ",
                 HTML(paste0("<span style='font-weight:bold;'>'HD Camera'</span>")),
                 " to see the ",
                 HTML(paste0("<span style='color:blue;font-weight:bold;'>Neutral Sentiments.</span>")),
                 style = "text-align: center; font-size: 13px; margin-top: 10px;")
        )
      )
      ),
      tabItem(
        tabName = "raw_data",
        tabPanel("Raw Data",
                 sidebarLayout(
                   sidebarPanel(width = 4,
                     status = "primary",
                     # Filter options
                     helpText(style = "font-weight:bold;","Choose values from the below filter:"),
                     helpText(style = "font-weight:bold;","(default value is 'All')"),
                     selectInput("category", "Category",
                                 choices = c("All", unique(apps_data$Category)),
                                 multiple = TRUE,
                                 selected = "All"),
                     selectInput("type", "Type",
                                 choices = c("All", unique(apps_data$Type)),
                                 multiple = TRUE,
                                 selected = "All"),
                     selectInput("rating", "Content Rating",
                                 choices = c("All", unique(apps_data$Content.Rating)),
                                 multiple = TRUE,
                                 selected = "All")
                   ),
                   mainPanel(
                     tabsetPanel(type = "tab",
                                 tabPanel("Data",
                                          DTOutput("table"),
                                          downloadButton("download_csv", "Download Filtered CSV")),
                                 tabPanel("Dataset Summary", verbatimTextOutput("summary"),
                                          tags$p(
                                            "In the dataset, ",
                                            tags$b("Installs"),
                                            " and ",
                                            tags$b("Reviews"),
                                            " are in Millions, ",
                                            tags$b("Size"),
                                            " is in GB. And ",
                                            tags$b("Price"),
                                            "is in '$'"
                                          )),
                                 tabPanel("Reference", uiOutput("ref")),
                                 
                       
                                )
                            )
                        )
                    )
              )
      )
)


dashboardPage(header, sidebar, body)
