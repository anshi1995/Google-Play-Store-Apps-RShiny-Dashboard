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

# Server
server <- function(input, output, session) {
  
  # Server-side Code for the 1st menu item "Dashboard"
  
  # Filtered data based on selected category
  filtered_data <- reactive({
    if (input$category_select != "All") {
      subset(apps_data, Category %in% input$category_select)
    } else {
      apps_data
    }
  })
  
  # Summary statistics
  output$Number_of_Apps <- renderValueBox({
    total_apps <- length(unique(filtered_data()$App))
    valueBox(prettyNum(total_apps, big.mark = ","), "Total Number of Apps",
             color = "yellow", icon = icon("hashtag"))
  })

  output$Avg_Rating <- renderValueBox({
    avg_rating <- round(mean(filtered_data()$Rating), 2)
    valueBox(avg_rating, "Average Rating",
             color = "olive", icon = icon("star"))
  })
  
  output$Total_Reviews <- renderValueBox({
    total_reviews <- sum(filtered_data()$Reviews)
    valueBox(prettyNum(total_reviews, big.mark = ","), "Total Number of Reviews (Millions)",
             color = "purple", icon = icon("thumbs-up"))
  })
  
  output$Total_Size <- renderValueBox({
    total_size <- round(sum(filtered_data()$Size) / 1000, 2)
    valueBox(total_size, "Total Size (GB)",
             color = "red", icon = icon("cloud"))
  })
  
  output$Total_Installs <- renderValueBox({
    total_installs <- sum(filtered_data()$Installs)
    valueBox(prettyNum(total_installs, big.mark = ","), "Total Number of Installs (Millions)",
             color = "aqua", icon = icon("download"))
  })
  
  output$Avg_Price <- renderValueBox({
    avg_price <- round(mean(filtered_data()$Price), 2)
    valueBox(paste0("$",avg_price), "Average Price",
             color = "fuchsia", icon = icon("dollar"))
  })
  
  # Render the bar chart
  output$barchart <- renderPlotly({
    genre_counts <- filtered_data() %>%
      select(App, Genres) %>%
      group_by(Genres) %>%
      summarise(Total_Apps = n())

    fig <- plot_ly(genre_counts, x = ~Total_Apps, y = ~Genres, type = 'bar', marker = list(
      color = "purple" ))
    fig <- fig %>%
      layout(xaxis = list(title = "Number of Apps", showgrid = FALSE,
                          tickfont = list(size = 9), titlefont = list(size = 8)),
             yaxis = list(title = "", showgrid = FALSE, tickangle = -45, tickfont = list(size = 6)),
             margin = list(l = 1, r = 1, t = 1, b = 1))
    fig
  })
  
  # Generate the line chart based on the selected time period
  output$linechart <- renderPlotly({
    
    if (input$time_period == "Yearly") {
      # Aggregate data by year
      yearly_data <- aggregate(apps_data$Last.Updated,
                               by = list(format(apps_data$Last.Updated, "%Y")), length)
      colnames(yearly_data) <- c("Year", "Count")
      
      plot_ly(data = yearly_data, x = ~Year, y = ~Count, type = "scatter",
              mode = "markers+lines", line = list(color = "green" )) %>%
        layout(xaxis = list(title = "Year", showgrid = FALSE),
               yaxis = list(title = "Count", showgrid = FALSE))
    }
    else if (input$time_period == "Monthly") {
      # Aggregate data by month
      monthly_data <- aggregate(apps_data$Last.Updated,
                                by = list(format(apps_data$Last.Updated, "%Y-%m")), length)
      colnames(monthly_data) <- c("Month", "Count")
      
      plot_ly(data = monthly_data, x = ~Month, y = ~Count, type = "scatter",
              mode = "markers+lines", line = list(color = "green" )) %>%
        layout(xaxis = list(title = "Month", showgrid = FALSE),
               yaxis = list(title = "Count", showgrid = FALSE))
    }
    else if (input$time_period == "Daily") {
      # Plot exact data
      apps_data$Last.Updated <- as.Date(apps_data$Last.Updated)
      plot_ly(data = apps_data, x = ~Last.Updated, type = "scatter",
              mode = "markers+lines", line = list(color = "green" )) %>%
        layout(xaxis = list(title = "Date", showgrid = FALSE),
               yaxis = list(title = "Count", showgrid = FALSE))
    }
  })
  
  # Server-side Code for the 2nd menu item "Reviews Analysis"
  # Select the app for filtering scorecards
  selected_app_reviews <- reactive({
    reviews_data %>%
      filter(App == input$app_select)
  })
  
  # Render the scorecards
  # Scorecard 1
  output$sentiment_polarity <- renderValueBox({
    valueBox(round(selected_app_reviews()$Average_Sentiment_Polarity, 2),
             "Average Sentiment Polarity", color = "purple", icon = icon("compass"))
  })
  
  # Scorecard 2
  output$avg_sentiment <- renderValueBox({
    if(selected_app_reviews()$Average_Sentiment_Polarity > 0) {
      color <- "green"
      sentiment <- "Positive"
      sentiment_icon <- icon("face-smile")
    }
    else if(selected_app_reviews()$Average_Sentiment_Polarity < 0) {
      color <- "red"
      sentiment <- "Negative"
      sentiment_icon <- icon("face-angry")
    }
    else {
      color <- "blue"
      sentiment <- "Neutral"
      sentiment_icon <- icon("face-meh")
    }
    valueBox(sentiment, "Sentiment", color = color, icon = sentiment_icon)
  })
  
  # Scorecard 3
  output$avg_sentiment_subjectivity <- renderValueBox({
    valueBox(round(selected_app_reviews()$Average_Sentiment_Subjectivity, 2),
             "Average Sentiment Subjectivity", color = "fuchsia", icon = icon("people-group"))
  })
  
  # Render the bar chart
  output$review_bar <- renderPlotly({
    selected_app <- input$app_select
    
    # Define bar colors for sentiments
    color_mapping <- c("#228B22", "#D22B2B", "#6495ED")
    
    plot_ly(selected_app_reviews(), x = c("Positive", "Negative", "Neutral"),
            y = c(~Number_of_positive_reviews, ~Number_of_negative_reviews, ~Number_of_neutral_reviews),
            type = "bar", marker = list(color = color_mapping)) %>%
      layout(title = paste0("Sentiment Analysis for '", selected_app, "'"),
             xaxis = list(title = "Sentiment"), yaxis = list(title = "Number of Reviews"))
  })
  
  # Render the word cloud
  output$word_cloud <- renderWordcloud2({
    
    options(warn = -1)
    
    # Create corpus for word cloud
    corpus <- Corpus(VectorSource(selected_app_reviews()$Combined_Reviews))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    # Convert corpus to word frequencies
    wordcloud_data <- TermDocumentMatrix(corpus)
    word_freq <- as.data.frame(as.matrix(wordcloud_data))
    word_freq <- data.frame(Word = rownames(word_freq), Freq = word_freq$`1`)
    word_freq <- word_freq[complete.cases(word_freq), ]
    
    # Wordcloud
    wordcloud2(word_freq, backgroundColor = "white")
    
  })
  
  # Server-side Code for the 3rd menu item "Raw Data"
  # Render table based on selected filters
  output$table <- renderDT({
    selected_category <- input$category
    selected_type <- input$type
    selected_rating <- input$rating
    
    if ("All" %in% selected_category) {
      filtered_data <- apps_data
    } else {
      filtered_data <- apps_data[apps_data$Category %in% selected_category, ]
    }
    
    if (!"All" %in% selected_type) {
      filtered_data <- filtered_data[filtered_data$Type %in% selected_type, ]
    }
    
    if (!"All" %in% selected_rating) {
      filtered_data <- filtered_data[filtered_data$Content.Rating %in% selected_rating, ]
    }
    
    brks <- seq(min(filtered_data$Installs), max(filtered_data$Installs), 0.1)
    clrs <- colorRampPalette(c("yellow2","goldenrod","darkred"))(length(brks) + 1)
    
    datatable(
      filtered_data,
      options = list(
        pageLength = 10,  # Number of rows per page
        scrollX = TRUE,  # Horizontal scrolling
        scrollY = "500px",  # Fixed height with vertical scrolling
        searching = TRUE  # Enabling search functionality
      )
    ) %>% 
      formatStyle(
        names(filtered_data),
        backgroundColor = "lightgray",
        fontWeight = "bold"  # Bold text
      ) %>%
      formatStyle(
        names(filtered_data),
        backgroundColor = "lightblue",  # Background color 
        fontWeight = "bold",
        target = 'row'  
      )
  })
  
  # Download filtered data as CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      "filtered_data.csv"
    },
    content = function(file) {
      selected_category <- input$category
      selected_type <- input$type
      selected_rating <- input$rating
      
      filtered_data <- apps_data
      if (!"All" %in% selected_category) {
        filtered_data <- filtered_data[filtered_data$Category %in% selected_category, ]
      }
      if (!"All" %in% selected_type) {
        filtered_data <- filtered_data[filtered_data$Type %in% selected_type, ]
      }
      if (!"All" %in% selected_rating) {
        filtered_data <- filtered_data[filtered_data$Content.Rating %in% selected_rating, ]
      }
      
      write.csv(filtered_data, file, row.names = FALSE)
    })
  
  # Render dataset summary
  output$summary <- renderPrint({
    summary(apps_data)
  })
  
  # Render Reference tab
  output$ref <- renderUI({
    dataset_url <- "https://www.kaggle.com/datasets/lava18/google-play-store-apps"
    dataset_hyperlink <- tags$a(href = dataset_url, target = "_blank", "Kaggle")
    github_url <- "https://github.com/anshi1995/Google-Play-Store-Apps-RShiny-Dashboard"
    github_hyperlink <- tags$a(href = github_url, target = "_blank", "GitHUb")
    portfolio_url <- "https://anshita-aishwarya.com"
    portfolio_hyperlink <- tags$a(href = portfolio_url, target = "_blank", "Portfolio")
    
    text_head <- "<h2> Google Play Store Apps Data </h2> <br>"
    text1 <- "This dataset contains"
    text2 <- "instances of Google Apps data and stores information about their genres, number of installs, size, ratings as well as their sentiment data. <br><br>"
    text3 <- "View the dataset on "
    text4 <- "View the source code on "
    text5 <- "Visit the Author's "
    
    text <- paste(text_head, text1, nrow(apps_data), text2,
                  text3, dataset_hyperlink, "<br>", text4, github_hyperlink,
                  "<br>", text5, portfolio_hyperlink)
    HTML(text)
  })
  
}

# Run the Shiny app
# shinyApp(ui, server)
