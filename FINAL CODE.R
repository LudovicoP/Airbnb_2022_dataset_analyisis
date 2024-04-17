#Download all the datasets included in the folder, then import them on RStudio

#Install libraries
{
  install.packages("dplyr")
  install.packages("lubridate")
  install.packages("stringr")
  install.packages("tidyverse")
  install.packages("lmtest")
  install.packages("graphics")
  install.packages("ggplot2")
  install.packages("leaps")
  install.packages("ggmap")
  install.packages("tm")
  install.packages("wordcloud")
  install.packages("cowplot")
  install.packages("janitor")
  install.packages("dummy")
  install.packages("gridExtra")
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tidyverse)
  library(lmtest)
  library(graphics)
  library(ggplot2)
  library(leaps)
  library(ggmap)
  library(tm)
  library(wordcloud)
  # Set API key for ggmap
  register_google(key = "AIzaSyDQ0mLXRbTWU7_-swlfc__M7DFn5Z92LJ4")
  library(cowplot)
  library(janitor)
  library(dummy)
  library(gridExtra)
}

#Data set to work easier on data
{
raw_data=AirBB_Dataset
head(raw_data)

#data = na.omit(raw_data)
#head(data)

#clean the dataset
str(raw_data)

#pick the amenities and create columns for each of them
{

# Extract amenities column and split at each comma
amenities_list <- str_extract_all(raw_data$amenities, "\\w+(?:\\s\\w+)*")

# Get unique amenities
unique_amenities <- unique(unlist(amenities_list))

# Create data frame with columns for each unique amenity
amenities_df <- data.frame(matrix(nrow = nrow(raw_data), ncol = length(unique_amenities)))
colnames(amenities_df) <- unique_amenities

# Loop through unique amenities
for (amenity in unique_amenities) {
  
  # Create new column with amenity name and TRUE/FALSE values
  amenities_df[[amenity]] <- sapply(amenities_list, function(x) amenity %in% x)
}

#switch false and true to 0 and 1
amenities_df <- data.frame(lapply(amenities_df, function(x) ifelse(x, 1, 0)))

#clean from irrelevant columns:
amenities_df <- amenities_df[, !(colnames(amenities_df) %in% c("en", "hosting_amenity_50", "hosting_amenity_49", "S", "s", "24", "in", "In", ".in", ".In", "in."))]

# Sum the number of True values for each column in amenities_df
amenities_sum <- colSums(amenities_df)

# Identify columns that sum up to 3 or less
cols_to_remove <- names(amenities_sum[amenities_sum <= 3])

# Remove identified columns from amenities_df
amenities_df <- amenities_df[, !(names(amenities_df) %in% cols_to_remove)]

# Sum to know how many times a service shows up
col_sum = colSums(amenities_df);col_sum

# Order it
sorted_col_sum <- col_sum[order(-col_sum)]; sorted_col_sum

# Divide by the number of observations to look at the probability of occurrence
p_amenities = sorted_col_sum/length(raw_data$id); p_amenities

# Collapse both in a matrix
amenities_matrix = cbind(sorted_col_sum, p_amenities)

# Subset original data to remove columns
original_data_subset <- raw_data[, !names(raw_data) %in% c("name", "amenities", "id", "description", "thumbnail_url")]

# Combine original data subset with new amenities data --> POLISHED FUNDAMENTAL
polished <- cbind(original_data_subset, amenities_df)
}

# Switch all the true and false into 1 or 0 and modify the polished
polished$cleaning_fee <- as.numeric(polished$cleaning_fee)
polished$host_identity_verified <- ifelse(polished$host_identity_verified == 't', 1, 0)

# Export the csv file
}

#Data set for each city
{
  data=Polished
  Boston=data[city=="Boston",]
  Chicago=data[city=="Chicago",]
  DC=data[city=="DC",]
  LA=data[city=="LA",]
  NYC=data[city=="NYC",]
  SF=data[city=="SF",]
  
  # Export the csvs file
}

#Variables
{
  data= Polished
  log_price = data$log_price
  
  #LOCATION:
  {
  city = data$city
  zipcode = data$zipcode
  latitude = data$latitude    
  longitude = data$longitude
  neighbourhood = data$neighbourhood
  }
  
  #FEEDBACK:
  {
  first_review = data$first_review
  number_of_reviews = data$number_of_reviews    
  review_scores_rating = data$review_scores_rating
  }
  
  #HOST:
  {
  host_since = data$host_since
  host_response_rate = data$host_response_rate
  host_identity_verified = data$host_identity_verified
  }
  
  #SERVICES
  {
  instant_bookable = data$instant_bookable
  cancellation_policy = data$cancellation_policy
  cleaning_fee = data$cleaning_fee
  }
  
  #PLACEMENT
  {
  property_type = data$property_type   
  room_type = data$room_type   
  accommodates = data$accommodates    
  bathrooms = data$bathrooms
  bedrooms = data$bedrooms    
  beds = data$beds   
  }
  
}

################################################################################

#Code to create maps
{
  # Copy variables in bins
  {
    data=Polished
    log_price = data$log_price    # numeric variable
    property_type = data$property_type    # categorical variable
    room_type = data$room_type    # categorical variable
    accommodates = data$accommodates    # numeric variable
    bathrooms = data$bathrooms    # numeric variable
    cancellation_policy = data$cancellation_policy    # categorical variable
    cleaning_fee = data$cleaning_fee    # categorical variable
    city = data$city    # categorical variable
    first_review = data$first_review    # date variable
    host_identity_verified = data$host_identity_verified    # categorical variable
    host_response_rate = data$host_response_rate    # numeric variable
    data$host_since <- as.Date(data$host_since, format = "%Y-%m-%d")
    host_since = data$host_since    # date variable
    instant_bookable = data$instant_bookable    # categorical variable
    latitude = data$latitude    # numeric variable
    longitude = data$longitude    # numeric variable
    neighbourhood = data$neighbourhood    # categorical variable
    number_of_reviews = data$number_of_reviews    # numeric variable
    review_scores_rating = data$review_scores_rating    # numeric variable
    data$zipcode = as.integer(data$zipcode)
    zipcode = data$zipcode    # categorical variable
    bedrooms = data$bedrooms    # numeric variable
    beds = data$beds    # numeric variable
  }
  # Download maps
  {
    # Get maps of the cities
    ggmap_boston <- ggmap(get_map(location = "Boston", zoom = 12))
    ggmap_chicago <- ggmap(get_map(location = "Chicago", zoom = 11))
    ggmap_dc <- ggmap(get_map(location = "Washington DC, USA", zoom = 11))
    ggmap_la <- ggmap(get_map(location = "Los Angeles", zoom = 9))
    ggmap_nyc <- ggmap(get_map(location = "New York City", zoom = 10))
    ggmap_sf <- ggmap(get_map(location = "San Francisco", zoom = 12))
  }
  # Code to plot location, run twice for a bug --> object log_price_group not found
  {
    # Convert ggmap objects to a list
    summary_ggmap <- list(ggmap_boston, ggmap_chicago, ggmap_dc, ggmap_la, ggmap_nyc, ggmap_sf)
    
    # Define colors for each group
    colors <- c("blue", "green", "yellow", "orange", "red", "black")
    
    # Create a vector of city names
    cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
    
    # Initialize an empty matrix to store the summary statistics
    summary_matrix <- matrix(nrow = length(cities), ncol = 6)
    colnames(summary_matrix) <- c("Min", "LL", "Q1", "Median", "Q3", "UL")
    
    # Create an empty list to store the ggmap objects
    FINAL_LOGPRICE_ggmap <- list()
    
    # Setting dimension for plotting points
    alpha_dim = 0.001
    
    # Loop through each city
    for (i in seq_along(cities)) {
      # Subset the data for the current city
      city_data <- data[city == cities[i], ]
      
      # Calculate summary statistics for log_price
      min_price <- max(0, min(city_data[2], na.rm = TRUE))
      q1_price <- quantile(city_data$log_price, 0.25)
      median_price <- median(city_data$log_price)
      q3_price <- quantile(city_data$log_price, 0.75)
      max_price <- max(city_data$log_price, na.rm = TRUE)
      iqr_price <- q3_price - q1_price
      ll_price <- q1_price - 1.5 * iqr_price
      ul_price <- q3_price + 1.5 * iqr_price
      
      # Store the summary statistics in the matrix
      summary_matrix[i,] <- c(min_price, ll_price, q1_price, median_price, q3_price, ul_price)
      
      # Setting log_price_group
      data$log_price_group <- cut(data$log_price, breaks = summary_matrix[i,])
      
      # Create ggmap object for the current city with colored points
      FINAL_LOGPRICE_ggmap[[i]] <- summary_ggmap[[i]] +
        geom_point(data = city_data, aes(x = longitude, y = latitude, color = log_price_group, alpha = alpha_dim)) +
        scale_color_manual(values = colors) + guides(color = guide_legend(title = "Price Group", breaks = summary_matrix[i, ], labels = c("Min", "LL", "Q1", "Median", "Q3", "UL")))
    }
    
    # Display the ggmap objects for each city
    for (i in seq_along(cities)) {
      print(FINAL_LOGPRICE_ggmap[[i]])
    }
  }
  # Code to save the images
  {
    # Export each ggmap object as a jpg file
    for (i in seq_along(cities)) {
      filename <- paste0("/Users/ludovicopanariello/Library/Mobile Documents/com~apple~CloudDocs/BOCCONI/2 semester/Statistics/Rstudio_cloud/Group work/Images price_log -- location/", cities[i], ".jpg")
      ggsave(filename, plot = FINAL_LOGPRICE_ggmap[[i]], device = "jpeg", dpi = 300)
    }
    
  }
  # Code to plot general variables
  {
    # Subset the data for the selected variable
    variable_name <- "number_of_reviews"
    data_subset <- data[c(variable_name, "latitude", "longitude", "city")]
    
    # Convert ggmap objects to a list
    summary_ggmap <- list(ggmap_boston, ggmap_chicago, ggmap_dc, ggmap_la, ggmap_nyc, ggmap_sf)
    
    # Define colors for each group
    colors <- c("blue", "green", "yellow", "orange", "red", "black")
    
    # Create a vector of city names
    cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
    
    # Initialize an empty matrix to store the summary statistics
    summary_matrix <- matrix(nrow = length(cities), ncol = 6)
    colnames(summary_matrix) <- c("Min", "LL", "Q1", "Median", "Q3", "UL")
    
    # Create an empty list to store the ggmap objects
    FINAL_LOGPRICE_ggmap <- list()
    
    # Setting dimension for plotting points
    alpha_dim <- 0.001
    
    # Loop through each city
    for (i in seq_along(cities)) {
      # Subset the data for the current city
      city_data <- data_subset[data_subset$city == cities[i], ]
      
      # Calculate summary statistics for the variable
      min_var <- -Inf
      q1_var <- quantile(city_data[[1]], 0.25, na.rm = TRUE) - 1e-5
      median_var <- median(city_data[[1]], na.rm = TRUE)
      q3_var <- quantile(city_data[[1]], 0.75, na.rm = TRUE) + 1e-5
      max_var <- +Inf
      iqr_var <- q3_var - q1_var
      ll_var <- q1_var - 1.5 * iqr_var
      ul_var <- q3_var + 1.5 * iqr_var
      
      # Store the summary statistics in the matrix
      summary_matrix[i, ] <- c(min_var, ll_var, q1_var, median_var, q3_var, ul_var)
      
      # Create groups for the data
      city_data$group <- cut(city_data[[1]], 
                             breaks = summary_matrix[i, ], 
                             include.lowest = TRUE)      
      # Create ggmap object for the current city with colored points
      FINAL_LOGPRICE_ggmap[[i]] <- summary_ggmap[[i]] +
        geom_point(data = city_data, aes(x = longitude, y = latitude, color = group, alpha = alpha_dim)) +
        scale_color_manual(values = colors) + guides(color = guide_legend(title = "Price Group", breaks = summary_matrix[i, ], labels = c("Min", "LL", "Q1", "Median", "Q3", "UL")))
    }
    # Display the ggmap objects for each city
    for (i in seq_along(cities)) {
      print(FINAL_LOGPRICE_ggmap[[i]])
    }
  }
  # Code to plot binary variables 
  {
    # Define the variable to plot
    dummy_var <- "host_identity_verified"
    
    # Convert ggmap objects to a list
    summary_ggmap <- list(ggmap_boston, ggmap_chicago, ggmap_dc, ggmap_la, ggmap_nyc, ggmap_sf)
    
    # Define colors for each group
    colors <- c("red", "green")
    
    # Create a vector of city names
    cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
    
    # Initialize an empty matrix to store the summary statistics
    summary_dummy_matrix <- matrix(nrow = length(cities), ncol = 2)
    colnames(summary_dummy_matrix) <- c("0", "1")
    
    # Create an empty list to store the ggmap objects
    FINAL_LOGPRICE_ggmap <- list()
    
    # Setting dimension for plotting points
    alpha_dim = 0.001
    
    # Loop through each city
    for (i in seq_along(cities)) {
      # Subset the data for the current city
      city_data <- data[city == cities[i], ]
      
      # Subset data to plot only if the dummy variable is 1 or 0
      city_data <- subset(city_data, subset = (cleaning_fee == 0 | cleaning_fee == 1))
      
      # Calculate summary statistics for the dummy variable
      summary_dummy_matrix[i,] <- table(city_data[dummy_var])
      
      # Create ggmap object for the current city with colored points
      FINAL_LOGPRICE_ggmap[[i]] <- summary_ggmap[[i]] +
        geom_point(data = city_data, aes(x = longitude, y = latitude, color = factor(cleaning_fee), alpha = alpha_dim)) +
        scale_color_manual(values = colors) + guides(color = guide_legend(title = "Dummy Variable", breaks = c(0,1)))
    }
    
    # Display the ggmap objects for each city
    for (i in seq_along(cities)) {
      print(FINAL_LOGPRICE_ggmap[[i]])
    }
  }
  # Code to find the most used words
  {
    # load required packages
    library(tidyverse)
    library(tm)
    library(wordcloud)
    
    # install the SnowballC package (needed for stemming)
    install.packages("SnowballC")
    
    # read in data and extract description column
    data <- read.csv("/Users/ludovicopanariello/Library/Mobile Documents/com~apple~CloudDocs/BOCCONI/2 semester/Statistics/Rstudio_cloud/Group work/AirBB Dataset.csv")
    descriptions <- data$description
    
    # clean and preprocess text data
    corpus <- Corpus(VectorSource(descriptions))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stemDocument)
    
    # create document-term matrix
    dtm <- DocumentTermMatrix(corpus)
    dtm <- removeSparseTerms(dtm, 0.95) # remove terms that appear in less than 5% of the documents
    
    # calculate word frequencies and find most common word
    freq <- colSums(as.matrix(dtm))
    ord <- order(freq, decreasing = TRUE)
    most_common_word <- names(freq)[ord[1]]
    
    # create word cloud of most common words
    wordcloud(words = names(freq), freq = freq, min.freq = 10, scale = c(3,1.5), max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
    wordcloud(words = names(freq), freq = freq, min.freq = 10, scale = c(3,1.5), max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
    
    
    # Load required packages
    library(tm)
    
    # Load data
    dataB <- read.csv("/Users/ludovicopanariello/Library/Mobile Documents/com~apple~CloudDocs/BOCCONI/2 semester/Statistics/Rstudio_cloud/Group work/AirBB Dataset.csv")
    
    # load required packages
    library(dplyr)
    library(stringr)
    library(tm)
    
    # create a corpus
    corpus <- Corpus(VectorSource(dataB$description))
    
    # clean and preprocess the corpus
    corpus_clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeWords, stopwords("english")) %>%
      tm_map(stripWhitespace)
    
    # create a document term matrix
    dtm <- DocumentTermMatrix(corpus_clean)
    
    # compute word frequency
    word_freq <- data.frame(word = colnames(dtm), freq = colSums(as.matrix(dtm)))
    
    # remove words that appear in less than 10 listings
    word_freq <- word_freq %>% filter(freq >= 10)
    
    # remove words that appear in more than 95% of listings
    num_listings <- nrow(data)
    word_freq <- word_freq %>% filter(freq <= 0.95 * num_listings)
    
    # create a vector of log prices
    log_prices <- log(dataB$price)
    
    # create a vector of descriptions
    descriptions <- dataB$description
    
    # create a data frame to store results
    word_median_price <- data.frame(word = character(),
                                    freq = numeric(),
                                    median_log_price = numeric())
    
    # loop through the words in word_freq
    for (i in 1:nrow(word_freq)) {
      word <- word_freq[i, "word"]
      freq <- word_freq[i, "freq"]
      
      # find the row index of the word in dtm
      row_index <- which(dtm$dimnames$Terms == word)
      
      # extract the relevant rows from dtm and log_prices
      dtm_subset <- dtm[, row_index]
      log_prices_subset <- log_prices[match(descriptions, dataB$description)]
      log_prices_subset <- log_prices_subset[dtm_subset > 0]
      
      # compute the median log price
      median_log_price <- median(log_prices_subset)
      
      # add the results to word_median_price
      word_median_price <- rbind(word_median_price,
                                 data.frame(word = word,
                                            freq = freq,
                                            median_log_price = median_log_price))
    }
  }
}

#Location-Placement analysis
{
  #Using a new dataframe containing means and modes
  {
    y <- neigh
    str(y)
    # Split the data frame by city
    y_list <- split(y, y$city)
    
    # assuming amenities column is available and named "amenities"
    library(dplyr)
    y <- y[complete.cases(y), ]
    max_cols <- apply(y[, !(colnames(y) %in% c("log_price", "property_type", "room_type", "accommodates", "bathrooms", "cancellation_policy", "cleaning_fee", "city", "first_review", "host_identity_verified", "host_response_rate", "host_since", "review_scores_rating", "instant_bookable", "latitude", "longitude", "number_of_reviews", "zipcode", "bedrooms", "beds", "Unnamed: 0"))], 1, function(x) names(x)[which.max(x)])
    neighbourhoods <- unique(y$neighbourhood...1 )
    max_df <- data.frame(neighbourhood = neighbourhoods, value = max_cols)
    max_df$city <- y$city[match(max_df$neighbourhood, y$neighbourhood...1)]
    max_df_subset <- subset(max_df, value != "Wireless.Internet")
    prop.table(table(max_df$value))
    max_df_subset_ordered <- max_df_subset[order(max_df_subset$city),]
    
    # Add latitude and longitude
    max_df_subset_ordered$latitude <- y$latitude[match(max_df_subset_ordered$neighbourhood, y$neighbourhood...1)]
    max_df_subset_ordered$longitude <- y$longitude[match(max_df_subset_ordered$neighbourhood, y$neighbourhood...1)]
    
    ggplot(max_df_subset_ordered, aes(x = neighbourhood, y = value, fill = value)) +
      geom_col() +
      coord_flip() +
      labs(title = "Most common amenity by neighbourhood and city",
           x = "Neighbourhood",
           y = "Most common amenity",
           fill = "Amenity")
    p <- ggplot() +
      # Add the points layer with latitude and longitude as x and y, respectively
      geom_point(data = y, aes(x = longitude, y = latitude, color = max_df_subset_ordered$value), alpha = 0.5, size = 2) +
      # Scale the color to have a different color for each amenity
      scale_color_discrete() +
      # Add a title and axis labels
      ggtitle("Locations Colored by Max Amenity") +
      xlab("Longitude") + ylab("Latitude")
    ##
    max_df_subset_ordered[max_df_subset_ordered$value == "Heating",]
    max_df_subset_ordered_filtered <- max_df_subset_ordered[!(max_df_subset_ordered$value %in% c("Heating", "Essentials", "Air.conditioning", "Kitchen")), ]
    p <- ggplot() +
      # Add the points layer with latitude and longitude as x and y, respectively
      geom_point(data = max_df_subset_ordered_filtered, aes(x = longitude, y = latitude, color = max_df_subset_ordered_filtered$value), alpha = 0.5, size = 2) +
      # Scale the color to have a different color for each amenity
      scale_color_discrete() +
      # Add a title and axis labels
      ggtitle("Locations Colored by Max Amenity") +
      xlab("Longitude") + ylab("Latitude")
    p
    
    library(ggplot2)
    library(cowplot)
    neighbourhood <- unique(na.omit(neighbourhood), na.rm = TRUE)
    plot_list <- list()
    for (city in unique(y$city)) {
      p <- ggplot(data = subset(y, city == city), aes(x = neighbourhood, y = accommodates)) + 
        geom_jitter(width = 0.2, height = 0) +
        labs(x = "Neighborhood", y = "Accommodates") +
        ggtitle(paste("Accommodates by Neighborhood in", city)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      plot_list[[city]] <- p
    }
    plot_grid(plotlist = plot_list, ncol = 2)
    
    # accomodates mean neighbourhoods
    {
      # Subset the data for the selected variable
      variable_name <- "Accomodates"
      data_subset <- y[c("accommodates", "latitude", "longitude", "city")]
      
      # Convert ggmap objects to a list
      summary_ggmap <- list(ggmap_boston, ggmap_chicago, ggmap_dc, ggmap_la, ggmap_nyc, ggmap_sf)
      
      # Define colors for each group
      colors <- c("blue", "green", "yellow", "orange", "red", "black")
      
      # Create a vector of city names
      cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
      
      # Initialize an empty matrix to store the summary statistics
      summary_matrix <- matrix(nrow = length(cities), ncol = 6)
      colnames(summary_matrix) <- c("Min", "LL", "Q1", "Median", "Q3", "UL")
      
      # Create an empty list to store the ggmap objects
      FINAL_LOGPRICE_ggmap <- list()
      
      # Setting dimension for plotting points
      alpha_dim <- 4
      
      # Loop through each city
      for (i in seq_along(cities)) {
        # Subset the data for the current city
        city_data <- data_subset[data_subset$city == cities[i], ]
        
        # Calculate summary statistics for the variable
        min_var <- -Inf
        q1_var <- quantile(city_data[[1]], 0.25, na.rm = TRUE) - 1e-5
        median_var <- median(city_data[[1]], na.rm = TRUE)
        q3_var <- quantile(city_data[[1]], 0.75, na.rm = TRUE) + 1e-5
        max_var <- +Inf
        iqr_var <- q3_var - q1_var
        ll_var <- q1_var - 1.5 * iqr_var
        ul_var <- q3_var + 1.5 * iqr_var
        
        # Store the summary statistics in the matrix
        summary_matrix[i, ] <- c(min_var, ll_var, q1_var, median_var, q3_var, ul_var)
        
        # Create groups for the data
        city_data$group <- cut(city_data[[1]], 
                               breaks = summary_matrix[i, ], 
                               include.lowest = TRUE)      
        # Create ggmap object for the current city with colored points
        FINAL_LOGPRICE_ggmap[[i]] <- summary_ggmap[[i]] +
          geom_point(data = city_data, aes(x = longitude, y = latitude, color = group, alpha = alpha_dim)) +
          scale_color_manual(values = colors) + guides(color = guide_legend(title = "Accomodates Group", breaks = summary_matrix[i, ], labels = c("Min", "LL", "Q1", "Median", "Q3", "UL")))
      }
      # Display the ggmap objects for each city
      for (i in seq_along(cities)) {
        print(FINAL_LOGPRICE_ggmap[[i]])
      }
    }
    # log mean neighbourhoods
    {
      # Subset the data for the selected variable
      variable_name <- "log_price"
      data_subset <- y[c("log_price", "latitude", "longitude", "city")]
      
      # Convert ggmap objects to a list
      summary_ggmap <- list(ggmap_boston, ggmap_chicago, ggmap_dc, ggmap_la, ggmap_nyc, ggmap_sf)
      
      # Define colors for each group
      colors <- c("blue", "green", "yellow", "orange", "red", "black")
      
      # Create a vector of city names
      cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
      
      # Initialize an empty matrix to store the summary statistics
      summary_matrix <- matrix(nrow = length(cities), ncol = 6)
      colnames(summary_matrix) <- c("Min", "LL", "Q1", "Median", "Q3", "UL")
      
      # Create an empty list to store the ggmap objects
      FINAL_LOGPRICE_ggmap <- list()
      
      # Setting dimension for plotting points
      alpha_dim <- 4
      
      # Loop through each city
      for (i in seq_along(cities)) {
        # Subset the data for the current city
        city_data <- data_subset[data_subset$city == cities[i], ]
        
        # Calculate summary statistics for the variable
        min_var <- -Inf
        q1_var <- quantile(city_data[[1]], 0.25, na.rm = TRUE) - 1e-5
        median_var <- median(city_data[[1]], na.rm = TRUE)
        q3_var <- quantile(city_data[[1]], 0.75, na.rm = TRUE) + 1e-5
        max_var <- +Inf
        iqr_var <- q3_var - q1_var
        ll_var <- q1_var - 1.5 * iqr_var
        ul_var <- q3_var + 1.5 * iqr_var
        
        # Store the summary statistics in the matrix
        summary_matrix[i, ] <- c(min_var, ll_var, q1_var, median_var, q3_var, ul_var)
        
        # Create groups for the data
        city_data$group <- cut(city_data[[1]], 
                               breaks = summary_matrix[i, ], 
                               include.lowest = TRUE)      
        # Create ggmap object for the current city with colored points
        FINAL_LOGPRICE_ggmap[[i]] <- summary_ggmap[[i]] +
          geom_point(data = city_data, aes(x = longitude, y = latitude, color = group, alpha = alpha_dim)) +
          scale_color_manual(values = colors) + guides(color = guide_legend(title = "log_price Group", breaks = summary_matrix[i, ], labels = c("Min", "LL", "Q1", "Median", "Q3", "UL")))
      }
      # Display the ggmap objects for each city
      for (i in seq_along(cities)) {
        print(FINAL_LOGPRICE_ggmap[[i]])
      }
    }
    
  }
  #Using the polished
  {
    d = Polished
    
    library(dplyr)
    library(tidyr)
    
    df_filtered <- d %>%
      filter(property_type %in% c("Timeshare", "Vacation home", "Dorm", "Hostel")) %>%
      select(city, neighbourhood, property_type, log_price)
    
    df_grouped <- df_filtered %>%
      group_by(city, neighbourhood, property_type) %>%
      summarise(mean_log_price = mean(log_price), count = n()) %>%
      pivot_wider(names_from = property_type, values_from = count)
    
    output <- df_grouped %>%
      group_by(city, neighbourhood) %>%
      summarise(mean_log_price = mean(mean_log_price),
                Timeshare = sum(Timeshare),
                `Vacation home` = sum(`Vacation home`),
                Dorm = sum(Dorm),
                Hostel = sum(Hostel)) %>%
      arrange(city, neighbourhood)
    
    print(output, n=59)
    
    # property_type che non sono nè apartment nè house
    # property_type che non sono nè apartment nè house
    # Create a named vector of colors
    property_colors <- c("Bed & Breakfast" = "lightblue",
                         "Boat" = "lightgreen",
                         "Boutique hotel" = "lightyellow",
                         "Bungalow" = "orange",
                         "Cabin" = "red",
                         "Camper/RV" = "purple",
                         "Castle" = "gainsboro",
                         "Cave" = "turquoise",
                         "Condominium" = "blue",
                         "Dorm" = "green",
                         "Guest suite" = "yellow",
                         "Guesthouse" = "orange",
                         "Hostel" = "coral",
                         "In-law" = "pink",
                         "Loft" = "brown",
                         "Other" = "gray",
                         "Serviced apartment" = "violet",
                         "Tent" = "slategray",
                         "Timeshare" = "cadetblue",
                         "Townhouse" = "gold",
                         "Treehouse" = "aquamarine",
                         "Villa" = "darkseagreen",
                         "Yurt" = "beige")
    
    # Subset the data for the selected variable
    data_subset <- d[!(d$property_type %in% c("Apartment", "House", "Condominium")), c("property_type", "latitude", "longitude", "city")]
    
    # Convert ggmap objects to a list
    summary_ggmap <- list(ggmap_boston, ggmap_chicago, ggmap_dc, ggmap_la, ggmap_nyc, ggmap_sf)
    
    # Create a vector of city names
    cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
    
    # Initialize an empty matrix to store the summary statistics
    summary_matrix <- matrix(nrow = length(cities), ncol = 6)
    
    # Create an empty list to store the ggmap objects
    FINAL_property_ggmap <- list()
    
    # Setting dimension for plotting points
    alpha_dim <- 4
    
    # Loop through each city
    for (i in seq_along(cities)) {
      # Subset the data for the current city
      city_data <- data_subset[data_subset$city == cities[i], ]
      # Create ggmap object for the current city with colored points
      FINAL_property_ggmap[[i]] <- summary_ggmap[[i]] +
        geom_point(data = city_data, aes(x = longitude, y = latitude, color = property_type, alpha = alpha_dim)) +
        scale_color_manual(values = property_colors) + guides(color = guide_legend(title = "Property type"))
    }
    
    # Display the ggmap objects for each city
    for (i in seq_along(cities)) {
      print(FINAL_property_ggmap[[i]])
    }
    
    
  }
  #Plotting on map
  {
    # accomodates mean neighbourhoods
    {
      # Subset the data for the selected variable
      variable_name <- "Accomodates"
      data_subset <- y[c("accommodates", "latitude", "longitude", "city")]
      
      # Convert ggmap objects to a list
      summary_ggmap <- list(ggmap_boston, ggmap_chicago, ggmap_dc, ggmap_la, ggmap_nyc, ggmap_sf)
      
      # Define colors for each group
      colors <- c("blue", "green", "yellow", "orange", "red", "black")
      
      # Create a vector of city names
      cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
      
      # Initialize an empty matrix to store the summary statistics
      summary_matrix <- matrix(nrow = length(cities), ncol = 6)
      colnames(summary_matrix) <- c("Min", "LL", "Q1", "Median", "Q3", "UL")
      
      # Create an empty list to store the ggmap objects
      FINAL_LOGPRICE_ggmap <- list()
      
      # Setting dimension for plotting points
      alpha_dim <- 4
      
      # Loop through each city
      for (i in seq_along(cities)) {
        # Subset the data for the current city
        city_data <- data_subset[data_subset$city == cities[i], ]
        
        # Calculate summary statistics for the variable
        min_var <- -Inf
        q1_var <- quantile(city_data[[1]], 0.25, na.rm = TRUE) - 1e-5
        median_var <- median(city_data[[1]], na.rm = TRUE)
        q3_var <- quantile(city_data[[1]], 0.75, na.rm = TRUE) + 1e-5
        max_var <- +Inf
        iqr_var <- q3_var - q1_var
        ll_var <- q1_var - 1.5 * iqr_var
        ul_var <- q3_var + 1.5 * iqr_var
        
        # Store the summary statistics in the matrix
        summary_matrix[i, ] <- c(min_var, ll_var, q1_var, median_var, q3_var, ul_var)
        
        # Create groups for the data
        city_data$group <- cut(city_data[[1]], 
                               breaks = summary_matrix[i, ], 
                               include.lowest = TRUE)      
        # Create ggmap object for the current city with colored points
        FINAL_LOGPRICE_ggmap[[i]] <- summary_ggmap[[i]] +
          geom_point(data = city_data, aes(x = longitude, y = latitude, color = group, alpha = alpha_dim)) +
          scale_color_manual(values = colors) + guides(color = guide_legend(title = "Accomodates Group", breaks = summary_matrix[i, ], labels = c("Min", "LL", "Q1", "Median", "Q3", "UL")))
      }
      # Display the ggmap objects for each city
      for (i in seq_along(cities)) {
        print(FINAL_LOGPRICE_ggmap[[i]])
      }
    }
    # log mean neighbourhoods
    {
      # Subset the data for the selected variable
      variable_name <- "log_price"
      data_subset <- y[c("log_price", "latitude", "longitude", "city")]
      
      # Convert ggmap objects to a list
      summary_ggmap <- list(ggmap_boston, ggmap_chicago, ggmap_dc, ggmap_la, ggmap_nyc, ggmap_sf)
      
      # Define colors for each group
      colors <- c("blue", "green", "yellow", "orange", "red", "black")
      
      # Create a vector of city names
      cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
      
      # Initialize an empty matrix to store the summary statistics
      summary_matrix <- matrix(nrow = length(cities), ncol = 6)
      colnames(summary_matrix) <- c("Min", "LL", "Q1", "Median", "Q3", "UL")
      
      # Create an empty list to store the ggmap objects
      FINAL_LOGPRICE_ggmap <- list()
      
      # Setting dimension for plotting points
      alpha_dim <- 4
      
      # Loop through each city
      for (i in seq_along(cities)) {
        # Subset the data for the current city
        city_data <- data_subset[data_subset$city == cities[i], ]
        
        # Calculate summary statistics for the variable
        min_var <- -Inf
        q1_var <- quantile(city_data[[1]], 0.25, na.rm = TRUE) - 1e-5
        median_var <- median(city_data[[1]], na.rm = TRUE)
        q3_var <- quantile(city_data[[1]], 0.75, na.rm = TRUE) + 1e-5
        max_var <- +Inf
        iqr_var <- q3_var - q1_var
        ll_var <- q1_var - 1.5 * iqr_var
        ul_var <- q3_var + 1.5 * iqr_var
        
        # Store the summary statistics in the matrix
        summary_matrix[i, ] <- c(min_var, ll_var, q1_var, median_var, q3_var, ul_var)
        
        # Create groups for the data
        city_data$group <- cut(city_data[[1]], 
                               breaks = summary_matrix[i, ], 
                               include.lowest = TRUE)      
        # Create ggmap object for the current city with colored points
        FINAL_LOGPRICE_ggmap[[i]] <- summary_ggmap[[i]] +
          geom_point(data = city_data, aes(x = longitude, y = latitude, color = group, alpha = alpha_dim)) +
          scale_color_manual(values = colors) + guides(color = guide_legend(title = "log_price Group", breaks = summary_matrix[i, ], labels = c("Min", "LL", "Q1", "Median", "Q3", "UL")))
      }
      # Display the ggmap objects for each city
      for (i in seq_along(cities)) {
        print(FINAL_LOGPRICE_ggmap[[i]])
      }
    }
  }
  #Using PDF and graphs to look generally to log price
  {
    # T-TEST, ANOVA AND CHI-SQUARED
    {
      # {
      #   data <- Polished
      #   # Define cities and corresponding number of top neighborhoods to select
      #   city <- c('Boston', 'Chicago', 'DC', 'LA', 'NYC', 'SF')
      #   x <- c(5, 5, 3, 6, 5, 10)
      #   
      #   # Loop over cities
      #   for (i in seq_along(city)) {
      #     # Select the neighborhoods with mean log price greater than x
      #     selected_neighborhoods <- data %>%
      #       filter(city == city[i]) %>%
      #       group_by(neighbourhood) %>%
      #       summarise(mean_log_price = mean(log_price, na.rm = TRUE)) %>%
      #       arrange(desc(mean_log_price)) %>%
      #       head(x[i]) %>%
      #       pull(neighbourhood)
      #     
      #     # Subset the data to the selected neighborhoods and columns of interest
      #     my_df <- data %>%
      #       filter(city == city[i], neighbourhood %in% selected_neighborhoods) %>%
      #       select(neighbourhood, log_price, host_identity_verified, host_since, host_response_rate,
      #              first_review, number_of_reviews, cancellation_policy,
      #              instant_bookable, cleaning_fee)
      #     my_df$host_identity_verified <- factor(my_df$host_identity_verified, levels = c("t", "f"))
      #     
      #     # Create visualizations and perform tests for each selected neighborhood
      #     for (neighborhood in selected_neighborhoods) {
      #       neighborhood_df <- subset(my_df, neighbourhood == neighborhood)
      #       
      #       # Descriptive statistics
      #       cat("\n")
      #       cat(paste0("Summary Statistics for ", neighborhood, ":\n"))
      #       print(summary(neighborhood_df))
      #       
      #       # Create visualizations
      #       if (!anyNA(neighborhood_df$log_price)) {
      #         hist(neighborhood_df$log_price, main = paste("Histogram of Log Price in", neighborhood), xlab = "Log Price")
      #         boxplot(log_price ~ room_type, data = neighborhood_df, main = paste("Boxplot of Log Price by Room Type in", neighborhood), xlab = "Room Type", ylab = "Log Price")
      #         plot(neighborhood_df$longitude, neighborhood_df$latitude, main = paste("Map of listings", neighborhood), xlab = "Longitude", ylab = "Latitude")
      #       }
      #       
      #       if (!anyNA(neighborhood_df$bathrooms) && !anyNA(neighborhood_df$bedrooms)) {
      #         plot(bedrooms ~ bathrooms, data = neighborhood_df, main = paste("Scatterplot of Bedrooms and Bathrooms in", neighborhood), xlab = "Bathrooms", ylab = "Bedrooms")
      #       }
      #       
      #       if (!anyNA(neighborhood_df$log_price) && all(is.finite(neighborhood_df$log_price))) {
      #         hist(neighborhood_df$log_price, main = paste("Histogram of Log Price in", neighborhood), xlab = "Log Price")
      #         boxplot(log_price ~ host_identity_verified, data = neighborhood_df, main = paste("Boxplot of Log Price by Host Identity Verified in", neighborhood), xlab = "Host Identity Verified", ylab = "Log Price", ylim = c(0, max(neighborhood_df$log_price, na.rm = TRUE)))
      #       }
      #       
      #       # Test hypotheses about differences between the neighborhoods
      #       # t_test <- t.test(log_price ~ host_identity_verified, data = neighborhood_df)
      #       anova <- aov(log_price ~ cancellation_policy, data = neighborhood_df)
      #       chi_squared <- chisq.test(neighborhood_df$instant_bookable, neighborhood_df$host_identity_verified)
      #       
      #       # Print the results
      #       cat("\n")
      #       cat(paste0("T-Test for Host Identity Verified in ", neighborhood, ":\n"))
      #       print(t_test)
      #       
      #       cat("\n")
      #       cat(paste0("ANOVA for Cancellation Policy in ", neighborhood, ":\n"))
      #       print(anova)
      #       
      #       cat("\n")
      #       cat(paste0("Chi-Squared Test for Instant Bookable and Host Identity Verified in ", neighborhood, ":\n"))
      #       print(chi_squared)
      #     }
      #   }
      #   
      # }
      
    }
    
    # DATA BY CITY
    {
      # Set the city to analyze
      cit <- "SF"  # Replace with the desired city name
      
      # Define the variables of interest
      var <- c("neighbourhood", "log_price", "room_type", "accommodates", "bathrooms", "bedrooms", "beds", "longitude", "latitude")
      
      # Load the data and subset for the selected city and variables
      data <- Polished
      data <- data[data$city == cit, var]
      
      # Select the variables of interest
      vars <- c("neighbourhood", "log_price", "room_type", "accommodates", "bathrooms", "bedrooms", "beds", "longitude", "latitude")
      my_df <- data[, vars]
      
      # Subset the neighbourhoods with at least x mean log price
      x <- c(5, 5, 3, 6, 5, 10)  # Adjust the thresholds as desired
      my_table <- table(my_df$neighbourhood)
      my_subset <- names(which(apply(my_table, 1, function(row) any(row > x))))
      my_df <- my_df[my_df$neighbourhood %in% my_subset, ]
      
      # Get the mean log price for each neighbourhood and sort them in descending order
      mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
      mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]
      
      # Select the top and bottom 3 neighbourhoods
      top_neighbourhoods <- mean_prices[1:3, "Group.1"]
      bottom_neighbourhoods <- mean_prices[(length(mean_prices$Group.1) - 3):length(mean_prices$Group.1), "Group.1"]
      
      # Subset the data for the top and bottom neighbourhoods and store the results
      top_data <- list()
      bottom_data <- list()
      
      for (i in 1:length(top_neighbourhoods)) {
        top_data[[i]] <- subset(my_df, neighbourhood == top_neighbourhoods[i])
      }
      
      for (i in 1:length(bottom_neighbourhoods)) {
        bottom_data[[i]] <- subset(my_df, neighbourhood == bottom_neighbourhoods[i])
      }
      
      str(my_df)
      
      library(ggplot2)
      ggplot(my_df, aes(x=log_price)) + 
        geom_histogram(binwidth=0.1, color="black", fill="blue") + 
        labs(title=paste0("Distribution of Log Price in ", city), x="Log Price", y="Count")
      ggplot(my_df, aes(x=room_type, y=log_price)) + 
        geom_boxplot(fill="blue", alpha=0.5) + 
        labs(title=paste0("Log Price by Room Type in ", city), x="Room Type", y="Log Price")
      ggplot(my_df, aes(x=longitude, y=latitude)) + 
        geom_point(alpha=0.5, color="blue") + 
        labs(title=paste0("Spatial Distribution of Listings in ", city), x="Longitude", y="Latitude")
      ggplot(my_df, aes(x=accommodates, y=log_price)) + 
        geom_point(alpha=0.5, color="blue") + 
        labs(title="Log Price by Accommodates", x="Accommodates", y="Log Price")
      ggplot(my_df, aes(x=neighbourhood)) + 
        geom_bar(fill="blue", alpha=0.5) + 
        labs(title="Number of Listings by Neighborhood", x="Neighborhood", y="Count") +
        theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
      
      y = neigh
      yboston = y[y$city == "Boston",]
      ychicago = y[y$city == "Chicago",]
      ydc = y[y$city == "DC",]
      yla = y[y$city == "LA",]
      ynyc = y[y$city == "NYC",]
      ysf = y[y$city == "SF",]
      
      yvector = c("yboston", "ychicago", "ydc", "yla", "ynyc", "ysf")
      #for boston
      # the variables to do are beds, bedrooms, property type, accomodates, bathrooms
      plot(yboston$log_price, yboston$beds)
      plot(yboston$log_price, yboston$bedrooms)
      plot(yboston$log_price, yboston$accommodates)
      plot(yboston$log_price, yboston$bathrooms)
      
      for (i in seq_along(yvector)) {
        plot(yboston$log_price ~ yboston$beds,
             xlab = "log_price", ylab = "beds", main = names(yvector)[i])
        plot(yboston$log_price ~ yboston$bedrooms,
             xlab = "log_price", ylab = "bedrooms", main = names(yvector)[i])
        plot(yboston$log_price ~ as.factor(yboston$property_type),
             xlab = "log_price", ylab = "property_type", main = names(yvector)[i])
        plot(yboston$log_price ~ yboston$accommodates,
             xlab = "log_price", ylab = "accommodates", main = names(yvector)[i])
        plot(yboston$log_price ~ yboston$bathrooms,
             xlab = "log_price", ylab = "bathrooms", main = names(yvector)[i])
      }
    }
    
    # OTHERS PLOT BY CITY
    {
      library(ggplot2)
      library(cowplot)
      neighbourhood <- unique(na.omit(neighbourhood), na.rm = TRUE)
      plot_list <- list()
      for (city in unique(y$city)) {
        p <- ggplot(data = subset(y, city == city), aes(x = neighbourhood, y = accommodates)) + 
          geom_jitter(width = 0.2, height = 0) +
          labs(x = "Neighborhood", y = "Accommodates") +
          ggtitle(paste("Accommodates by Neighborhood in", city)) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        plot_list[[city]] <- p
      }
      plot_grid(plotlist = plot_list, ncol = 2)
      
      
      # Variables to plot
      vars <- c("beds", "bedrooms", "property_type", "accommodates", "bathrooms")
      
      # Loop over each city and create the plots
      for (i in seq_along(y_list)) {
        city_df <- y_list[[i]]
        city_name <- unique(city_df$city)
        
        # Loop over each variable and create a plot
        for (var in vars) {
          plot(city_df$log_price ~ city_df[[var]],
               xlab = "log_price", ylab = var, main = city_name)
        }
      }
      
    }
    
    # INVESTIGATION ON COUNT NEIGHBOURHOOD AND PROFITABILITY 
    {
      ########### MEAN
      library(dplyr)
      library(ggplot2)
      
      # Calculate mean log price for each neighborhood
      mean_log_price <- my_df %>% 
        group_by(neighbourhood) %>% 
        summarize(mean_log_price = mean(log_price))
      
      # Merge mean_log_price with my_df
      my_df <- my_df %>% 
        left_join(mean_log_price, by = "neighbourhood")
      
      # Create bar plot with mean log price as fill color
      ggplot(my_df, aes(x = neighbourhood, y = 1, fill = mean_log_price)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "green", high = "red") +
        labs(title = "Number of Listings by Neighborhood", x = "Neighborhood", y = "Count") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
      #
      # Calculate count for each neighborhood and arrange by count
      count_df <- my_df %>% 
        group_by(neighbourhood) %>% 
        summarize(count = n()) %>% 
        arrange(desc(count))
      
      # Reorder levels of neighbourhood factor in my_df
      my_df$neighbourhood <- factor(my_df$neighbourhood, levels = count_df$neighbourhood)
      
      # Create bar plot with mean log price as fill color and ordered by count
      ggplot(my_df, aes(x = neighbourhood, y = 1, fill = mean_log_price)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "green", high = "red") +
        labs(title = "Number of Listings by Neighborhood", x = "Neighborhood", y = "Count") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
      ##
      # Create the plots
      ggplot(my_df, aes(x=neighbourhood)) + 
        geom_bar(fill="blue", alpha=0.5) + 
        labs(title="Number of Listings by Neighborhood", x="Neighborhood", y="Count") +
        theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
      
      ggplot(my_df, aes(x=log_price)) + 
        geom_histogram(binwidth=0.1, color="black", fill="blue") + 
        labs(title=paste0("Distribution of Log Price in ", city), x="Log Price", y="Count")
      
      ggplot(my_df, aes(x=room_type, y=log_price)) + 
        geom_boxplot(fill="blue", alpha=0.5) + 
        labs(title=paste0("Log Price by Room Type in ", city), x="Room Type", y="Log Price")
      
      ggplot(my_df, aes(x=accommodates, y=log_price)) + 
        geom_point(alpha=0.5, color="blue") + 
        labs(title="Log Price by Accommodates", x="Accommodates", y="Log Price")
      
      ggplot(my_df, aes(x=bathrooms, y=log_price)) + 
        geom_point(alpha=0.5, color="blue") + 
        labs(title="Log Price by Bathrooms", x="Bathrooms", y="Log Price")
      
      ggplot(my_df, aes(x=bedrooms, y=log_price)) + 
        geom_point(alpha=0.5, color="blue") + 
        labs(title="Log Price by Bedrooms", x="Bedrooms", y="Log Price")
      
      ggplot(my_df, aes(x=beds, y=log_price)) + 
        geom_point(alpha=0.5, color="blue") + 
        labs(title="Log Price by Beds", x="Beds", y="Log Price")
      
      ggplot(my_df, aes(x=longitude, y=latitude)) + 
        geom_point(alpha=0.5, color="blue") + 
        labs(title=paste0("Spatial Distribution of Listings in ", city), x="Longitude", y="Latitude")
      
      # Create cross-tabulations for top neighbourhoods
      top_room_type_accom_tab <- lapply(top_data, function(df) table(df$room_type, df$accommodates))
      
      # Create cross-tabulations for top neighbourhoods
      top_room_type_accom_tab <- lapply(top_data, function(df) table(df$room_type, df$accommodates))
      
      # Print the cross-tabulations
      for (i in 1:length(top_room_type_accom_tab)) {
        print(paste0("Top neighbourhood ", i, ":\n"))
        print(top_room_type_accom_tab[[i]])
      }
      
      
    }
    
    # GRAPHS PDFS
    {
      #Set the cities to analyze
      cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
      
      # Define the variables of interest
      var <- c("neighbourhood", "log_price", "room_type", "accommodates", "bathrooms", "bedrooms", "beds", "longitude", "latitude")
      
      # Loop through the cities
      for (city in cities) {
        
        # Load the data and subset for the selected city and variables
        data <- Polished
        data <- data[data$city == city, var]
        
        # Subset the neighbourhoods with at least x mean log price
        x <- c(5, 5, 3, 6, 5, 10)  # Adjust the thresholds as desired
        my_table <- table(data$neighbourhood)
        my_subset <- names(which(apply(my_table, 1, function(row) any(row > x))))
        my_df <- data[data$neighbourhood %in% my_subset, ]
        
        # Get the mean log price for each neighbourhood and sort them in descending order
        mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
        mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]
        
        # Select the top and bottom 3 neighbourhoods
        top_neighbourhoods <- mean_prices[1:3, "Group.1"]
        bottom_neighbourhoods <- mean_prices[(length(mean_prices$Group.1) - 3):length(mean_prices$Group.1), "Group.1"]
        
        # Subset the data for the top and bottom neighbourhoods and store the results
        top_data <- list()
        bottom_data <- list()
        
        for (i in 1:length(top_neighbourhoods)) {
          top_data[[i]] <- subset(my_df, neighbourhood == top_neighbourhoods[i])
        }
        
        for (i in 1:length(bottom_neighbourhoods)) {
          bottom_data[[i]] <- subset(my_df, neighbourhood == bottom_neighbourhoods[i])
        }
        
        # Create the plots
        hist_plot <- ggplot(my_df, aes(x=log_price)) + 
          geom_histogram(binwidth=0.1, color="black", fill="red") + 
          labs(title=paste0("Distribution of Log Price in ", city), x="Log Price", y="Count")
        
        box_plot <- ggplot(my_df, aes(x=room_type, y=log_price)) + 
          geom_boxplot(fill="red", alpha=0.5) + 
          labs(title=paste0("Log Price by Room Type in ", city), x="Room Type", y="Log Price")
        
        scatter_plot2 <- ggplot(my_df, aes(x=accommodates, y=log_price)) + 
          geom_point(alpha=0.5) +
          labs(title=paste0("Log Price by Accommodation in ", city), x="Accommodates", y="Log Price")
        
        scatter_plot2 <- ggplot(my_df, aes(x=accommodates, y=log_price)) + 
          geom_point(alpha=0.5, color="blue") +
          labs(title=paste0("Log Price by Accommodation in ", city), x="Accommodates", y="Log Price")
        
        # Create the top and bottom neighbourhood plots
        
        top_plots <- list()
        bottom_plots <- list()
        
        for (i in 1:length(top_data)) {
          top_plots[[i]] <- ggplot(top_data[[i]], aes(x=log_price)) +
            geom_histogram(binwidth=0.1, color="black", fill="red") +
            labs(title=paste0("Distribution of Log Price in ", top_neighbourhoods[i]), x="Log Price", y="Count")
        }
        
        for (i in 1:length(bottom_data)) {
          bottom_plots[[i]] <- ggplot(bottom_data[[i]], aes(x=log_price)) +
            geom_histogram(binwidth=0.1, color="black", fill="red") +
            labs(title=paste0("Distribution of Log Price in ", bottom_neighbourhoods[i]), x="Log Price", y="Count")
        }
        
        #Save the plots as pdf files
        
        pdf(paste0(city, ".pdf"))
        print(hist_plot)
        print(box_plot)
        print(scatter_plot2)
        for (i in 1:length(top_plots)) {
          print(top_plots[[i]])
        }
        for (i in 1:length(bottom_plots)) {
          print(bottom_plots[[i]])
        }
        dev.off()
      }
      
      
    }
    
    # SD AND MEAN DISTRIBUTION ROOM TYPE AND PLOT
    {
      data = Polished
      # Group data by property type and room type, and calculate mean price
      price_table <- data %>% 
        group_by(property_type, room_type) %>% 
        summarise(mean_price = mean(log_price, na.rm = TRUE))
      
      # Reshape the data to create the desired table format
      price_table_wide <- price_table %>% 
        pivot_wider(names_from = room_type, values_from = mean_price)
      
      # View the result
      vettorea <- price_table_wide[["Entire home/apt"]]
      sd(vettorea, na.rm = TRUE)
      summary(vettorea, na.rm = TRUE)
      vettoreb <- price_table_wide[["Private room"]]
      sd(vettoreb, na.rm = TRUE)
      summary(vettoreb, na.rm = TRUE)
      vettorec <- price_table_wide[["Shared room"]]
      sd(vettorec, na.rm = TRUE)
      summary(vettorec, na.rm = TRUE)
      
      
      # Create a data frame with the summary statistics
      df <- data.frame(
        group = c("Entire Room", "Private Room", "Shared Room"),
        mean = c(mean(vettorea, na.rm = TRUE), mean(vettoreb, na.rm = TRUE), mean(vettorec, na.rm = TRUE)),
        sd = c(sd(vettorea, na.rm = TRUE), sd(vettoreb, na.rm = TRUE), sd(vettorec, na.rm = TRUE))
      )
      
      
      # Plot the Gaussian distributions
      # Compute kernel density estimates
      dens_a <- density(vettorea, na.rm = TRUE)
      dens_b <- density(vettoreb, na.rm = TRUE)
      dens_c <- density(vettorec, na.rm = TRUE)
      
      # Plot the three densities
      plot(dens_a, main = "Distribution of Room Prices", xlab = "Price", ylab = "Density", col = "red")
      lines(dens_b, col = "blue")
      lines(dens_c, col = "green")
      legend("topright", legend = c("Entire Room", "Private Room", "Shared Room"), col = c("red", "blue", "green"), lty = 1)
      
      # Rename the columns
      colnames(price_table_wide)[-1] <- c("Entire Room", "Private Room", "Shared Room")
      
      # Print the table
      print(price_table_wide, n = 35)
      table(property_type, room_type)
      ############################### PLOT
      library(ggplot2)
      #, `Private Room`, `Shared Room`
      # Convert the table from wide to long format
      price_table_long <- tidyr::pivot_longer(price_table_wide, 
                                              cols = c(`Shared Room`), 
                                              names_to = "room_type", 
                                              values_to = "mean_log_price")
      
      # Create the plot
      ggplot(price_table_long, aes(x = property_type, y = mean_log_price, fill = room_type)) +
        geom_col(position = "stack") +
        labs(x = "Property Type", y = "Mean Log Price", fill = "Room Type") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  }
}

#Placement-Services-Host cluster analysis
{
data=Polished

#Relationship with log price
{
  #Cancellation policy
  {
    boxplot(log_price~cancellation_policy, main = "All cities")
    boxplot(Boston$log_price~Boston$cancellation_policy, main = "Boston")
    boxplot(Chicago$log_price~Chicago$cancellation_policy, main = "Chicago")
    boxplot(DC$log_price~DC$cancellation_policy, main = "DC")
    boxplot(LA$log_price~LA$cancellation_policy, main = "LA")
    boxplot(NYC$log_price~NYC$cancellation_policy, main = "NYC")
    boxplot(SF$log_price~SF$cancellation_policy, main = "SF")
  }
  
  #Cleaning fee
  {
    boxplot(log_price~cleaning_fee, main = "All cities")
    boxplot(Boston$log_price~Boston$cleaning_fee, main = "Boston")
    boxplot(Chicago$log_price~Chicago$cleaning_fee, main = "Chicago")
    boxplot(DC$log_price~DC$cleaning_fee, main = "DC")
    boxplot(LA$log_price~LA$cleaning_fee, main = "LA")
    boxplot(NYC$log_price~NYC$cleaning_fee, main = "NYC")
    boxplot(SF$log_price~SF$cleaning_fee, main = "SF")
  }
}

#Relationship between the variables of the cluster
{
  #Cancellation policy-Instant bookable
  {
    #all cities
    tab=table(cancellation_policy,instant_bookable)
    barplot(prop.table(tab,2), main = "All cities")
    
    #Boston
    bostontab=table(Boston$cancellation_policy,Boston$instant_bookable)
    barplot(prop.table(bostontab,2), main = "Boston")
    
    #Chicago
    Chicagotab=table(Chicago$cancellation_policy,Chicago$instant_bookable)
    barplot(prop.table(Chicagotab,2), main = "Chicago")
    
    #DC
    DCtab=table(DC$cancellation_policy,DC$instant_bookable)
    barplot(prop.table(DCtab,2), main = "DC")
    
    #LA
    LAtab=table(LA$cancellation_policy,LA$instant_bookable)
    barplot(prop.table(LAtab,2), main = "LA")
    
    #NYC
    NYCtab=table(NYC$cancellation_policy,NYC$instant_bookable)
    barplot(prop.table(NYCtab,2), main = "NYC")
    
    #SF
    SFtab=table(SF$cancellation_policy,SF$instant_bookable)
    barplot(prop.table(SFtab,2), main = "SF")
}
  #Cancellation policy-Cleaning fee
  {
    # all cities
    tab=table(cancellation_policy,cleaning_fee)
    barplot(prop.table(tab,2), main = "All cities")
    
    #Boston
    Bostontab=table(Boston$cancellation_policy,Boston$cleaning_fee)
    barplot(prop.table(Bostontab,2), main = "Boston")
    
    #Chicago
    Chicagotab=table(Chicago$cancellation_policy,Chicago$cleaning_fee)
    barplot(prop.table(Chicagotab,2), main = "Chicago")
    
    #DC
    DCtab=table(DC$cancellation_policy,DC$cleaning_fee)
    barplot(prop.table(DCtab,2), main = "DC")
    
    #LA
    LAtab=table(LA$cancellation_policy,LA$cleaning_fee)
    barplot(prop.table(LAtab,2), main = "LA")
    
    #NYC
    NYCtab=table(NYC$cancellation_policy,NYC$cleaning_fee)
    barplot(prop.table(NYCtab,2), main = "NYC")
    
    #SF
    SFtab=table(SF$cancellation_policy,SF$cleaning_fee)
    barplot(prop.table(SFtab,2), main = "SF")
}
  #Cleaning fee-Instant bookable
  {
    #All cities
    tab=table(cleaning_fee, instant_bookable)
    barplot(prop.table(tab,2), main = "All cities")
    tab1=table(instant_bookable, cleaning_fee)
    barplot(prop.table(tab1,2), main = "All cities")
    
    #Boston
    Bostontab=table(Boston$cleaning_fee, Boston$instant_bookable)
    barplot(prop.table(Bostontab,2), main = "Boston")
    Bostontab1=table(Boston$instant_bookable, Boston$cleaning_fee)
    barplot(prop.table(Bostontab1,2), main = "Boston")
    
    #Chicago
    Chicagotab=table(Chicago$cleaning_fee, Chicago$instant_bookable)
    barplot(prop.table(Chicagotab,2), main = "Chicago")
    Chicagotab1=table(Chicago$instant_bookable, Chicago$cleaning_fee)
    barplot(prop.table(Chicagotab1,2), main = "Chicago")
    
    #DC
    DCtab=table(DC$cleaning_fee, DC$instant_bookable)
    barplot(prop.table(DCtab,2), main = "DC")
    DCtab1=table(DC$instant_bookable, DC$cleaning_fee)
    barplot(prop.table(DCtab1,2), main = "DC")
    
    #LA
    LAtab=table(LA$cleaning_fee, LA$instant_bookable)
    barplot(prop.table(LAtab,2), main = "LA")
    LAtab1=table(LA$instant_bookable, LA$cleaning_fee)
    barplot(prop.table(LAtab1,2), main = "LA")
    
    #NYC
    NYCtab=table(NYC$cleaning_fee, NYC$instant_bookable)
    barplot(prop.table(NYCtab,2), main = "NYC")
    NYCtab1=table(NYC$instant_bookable, NYC$cleaning_fee)
    barplot(prop.table(NYCtab1,2), main = "NYC")
    
    #SF
    SFtab=table(SF$cleaning_fee, SF$instant_bookable)
    barplot(prop.table(SFtab,2), main = "SF")
    SFtab1=table(SF$instant_bookable, SF$cleaning_fee)
    barplot(prop.table(SFtab1,2), main = "SF")
    
}
}

#Relationship Services-Placement-Host
{
  #Services-Room Type
  {
    #Cleaning fee-Room Type
    {
      #All cities
      tab=table(cleaning_fee, room_type)
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cleaning_fee, Boston$room_type)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cleaning_fee, Chicago$room_type)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cleaning_fee, DC$room_type)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cleaning_fee, LA$room_type)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cleaning_fee, NYC$room_type)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cleaning_fee, SF$room_type)
      barplot(prop.table(SFtab,2), main = "SF")
  }
    #Instant bookable-Room Type
    { 
      # all cities
      tab=table(instant_bookable, room_type)
      barplot(prop.table(tab,2), main = "All cities")
      
      # Boston
      Bostontab=table(Boston$instant_bookable, Boston$room_type)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      # Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$room_type)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      # DC
      DCtab=table(DC$instant_bookable, DC$room_type)
      barplot(prop.table(DCtab,2), main = "DC")
      
      # LA
      LAtab=table(LA$instant_bookable, LA$room_type)
      barplot(prop.table(LAtab,2), main = "LA")
      
      # NYC
      NYCtab=table(NYC$instant_bookable, NYC$room_type)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      # SF
      SFtab=table(SF$instant_bookable, SF$room_type)
      barplot(prop.table(SFtab,2), main = "SF")
  }
    #Cancellation policy-Room Type
    {
      #All cities
      tab=table(cancellation_policy,room_type)
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy,Boston$room_type)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy,Chicago$room_type)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cancellation_policy,DC$room_type)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cancellation_policy,LA$room_type)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy,NYC$room_type)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cancellation_policy,SF$room_type)
      barplot(prop.table(SFtab,2), main = "SF")
    }
  }
  
  #Services-Property Type
  {
    #Cleaning fee-Property Type
    { 
      # All cities
      tab=table(cleaning_fee,property_type);tab
      barplot(prop.table(tab,2), cex.names = 0.5,las = 2, main = "All cities")
      
      # Boston
      Bostontab=table(Boston$cleaning_fee,Boston$property_type);tab
      barplot(prop.table(Bostontab,2), cex.names = 0.5,las = 2, main = "Boston")
      
      # Chicago
      Chicagotab=table(Chicago$cleaning_fee,Chicago$property_type);tab
      barplot(prop.table(Chicagotab,2), cex.names = 0.5,las = 2, main = "Chicago")
      
      # DC
      DCtab=table(DC$cleaning_fee,DC$property_type);tab
      barplot(prop.table(DCtab,2), cex.names = 0.5,las = 2, main = "DC")
      
      # LA
      LAtab=table(LA$cleaning_fee,LA$property_type);tab
      barplot(prop.table(LAtab,2), cex.names = 0.5,las = 2, main = "LA")
      
      # NYC
      NYCtab=table(NYC$cleaning_fee,NYC$property_type);tab
      barplot(prop.table(NYCtab,2), cex.names = 0.5,las = 2, main = "NYC")
      
      # SF
      SFtab=table(SF$cleaning_fee,SF$property_type);tab
      barplot(prop.table(SFtab,2), cex.names = 0.5,las = 2, main = "SF")
    }
    #Instant bookable-Property Type 
    {
      #All cities
      tab=table(instant_bookable, property_type);tab
      barplot(prop.table(tab,2), cex.names = 0.5,las = 2, main = "All cities")
      
      #Boston
      Bostontab=table(Boston$instant_bookable, Boston$property_type);tab
      barplot(prop.table(Bostontab,2), cex.names = 0.5,las = 2, main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$property_type);tab
      barplot(prop.table(Chicagotab,2), cex.names = 0.5,las = 2, main = "Chicago")
      
      #DC
      DCtab=table(DC$instant_bookable, DC$property_type);tab
      barplot(prop.table(DCtab,2), cex.names = 0.5,las = 2, main = "DC")
      
      #LA
      LAtab=table(LA$instant_bookable, LA$property_type);tab
      barplot(prop.table(LAtab,2), cex.names = 0.5,las = 2, main = "LA")
      
      #NYC
      NYCtab=table(NYC$instant_bookable, NYC$property_type);tab
      barplot(prop.table(NYCtab,2), cex.names = 0.5,las = 2, main = "NYC")
      
      #SF
      SFtab=table(SF$instant_bookable, SF$property_type);tab
      barplot(prop.table(SFtab,2), cex.names = 0.5,las = 2, main = "SF")
    }
    #Cancellation policy-Property Type 
    {
      #All cities
      tab=table(cancellation_policy, property_type);tab
      barplot(prop.table(tab,2), cex.names = 0.5,las = 2, main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy, Boston$property_type);tab
      barplot(prop.table(Bostontab,2), cex.names = 0.5,las = 2, main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy, Chicago$property_type);tab
      barplot(prop.table(Chicagotab,2), cex.names = 0.5,las = 2, main = "Chicago")
      
      #DC
      DCtab=table(DC$cancellation_policy, DC$property_type);tab
      barplot(prop.table(DCtab,2), cex.names = 0.5,las = 2, main = "DC")
      
      #LA
      LAtab=table(LA$cancellation_policy, LA$property_type);tab
      barplot(prop.table(LAtab,2), cex.names = 0.5,las = 2, main = "LA")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy, NYC$property_type);tab
      barplot(prop.table(NYCtab,2), cex.names = 0.5,las = 2, main = "NYC")
      
      #SF
      SFtab=table(SF$cancellation_policy, SF$property_type);tab
      barplot(prop.table(SFtab,2), cex.names = 0.5,las = 2, main = "SF")
    }
  }
  
  #Services-Bathroom
  {
    #Cleaning fee-Bathrooms
    {
      #All cities
      tab=table(cleaning_fee, bathrooms); tab
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cleaning_fee, Boston$bathrooms)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cleaning_fee, Chicago$bathrooms)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cleaning_fee, DC$bathrooms)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cleaning_fee, LA$bathrooms)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cleaning_fee, NYC$bathrooms)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cleaning_fee, SF$bathrooms)
      barplot(prop.table(SFtab,2), main = "SF")
    }
    #Instant bookable-Bathrooms
    { 
      #All cities
      tab=table(instant_bookable, bathrooms);tab
      barplot(prop.table(tab,2), main = "All cities")
      
      # Boston
      Bostontab=table(Boston$instant_bookable, Boston$bathrooms)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      # Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$bathrooms)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      # DC
      DCtab=table(DC$instant_bookable, DC$bathrooms)
      barplot(prop.table(DCtab,2), main = "DC")
      
      # LA
      LAtab=table(LA$instant_bookable, LA$bathrooms)
      barplot(prop.table(LAtab,2), main = "LA")
      
      # NYC
      NYCtab=table(NYC$instant_bookable, NYC$bathrooms)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      # SF
      SFtab=table(SF$instant_bookable, SF$bathrooms)
      barplot(prop.table(SFtab,2), main = "SF")
      
      
    }
    #Cancellation policy-Bathrooms
    { 
      #All cities
      tab=table(cancellation_policy,bathrooms);tab
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy,Boston$bathrooms)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy,Chicago$bathrooms)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cancellation_policy,DC$bathrooms)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cancellation_policy,LA$bathrooms)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy,NYC$bathrooms)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cancellation_policy,SF$bathrooms)
      barplot(prop.table(SFtab,2), main = "SF")
    }
  }
  
  #Services-Bedrooms
  {
    #Cleaning fee-Bedrooms
    {
      #All cities
      tab=table(cleaning_fee, bedrooms)
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cleaning_fee, Boston$bedrooms)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cleaning_fee, Chicago$bedrooms)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cleaning_fee, DC$bedrooms)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cleaning_fee, LA$bedrooms)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cleaning_fee, NYC$bedrooms)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cleaning_fee, SF$bedrooms)
      barplot(prop.table(SFtab,2), main = "SF")
    }
    #Instant bookable-Bedrooms
    { 
      #All cities
      tab=table(instant_bookable, bedrooms);tab
      barplot(prop.table(tab,2), main = "All cities")
      
      # Boston
      Bostontab=table(Boston$instant_bookable, Boston$bedrooms)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      # Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$bedrooms)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      # DC
      DCtab=table(DC$instant_bookable, DC$bedrooms)
      barplot(prop.table(DCtab,2), main = "DC")
      
      # LA
      LAtab=table(LA$instant_bookable, LA$bedrooms)
      barplot(prop.table(LAtab,2), main = "LA")
      
      # NYC
      NYCtab=table(NYC$instant_bookable, NYC$bedrooms)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      # SF
      SFtab=table(SF$instant_bookable, SF$bedrooms)
      barplot(prop.table(SFtab,2), main = "SF")
      
      
    }
    #Cancellation policy-Bedrooms
    { 
      #All cities
      tab=table(cancellation_policy, bedrooms)
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy,Boston$bedrooms)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy,Chicago$bedrooms)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cancellation_policy,DC$bedrooms)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cancellation_policy,LA$bedrooms)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy,NYC$bedrooms)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cancellation_policy,SF$bedrooms)
      barplot(prop.table(SFtab,2), main = "SF")
    }
  }
  
  #Services-Beds
  {
    #Cleaning fee-Beds
    {
      #All cities
      tab=table(cleaning_fee, beds);tab
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cleaning_fee, Boston$beds)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cleaning_fee, Chicago$beds)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cleaning_fee, DC$beds)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cleaning_fee, LA$beds)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cleaning_fee, NYC$beds)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cleaning_fee, SF$beds)
      barplot(prop.table(SFtab,2), main = "SF")
    }
    #Instant bookable-Beds
    { 
      #All cities
      tab=table(instant_bookable, beds);tab
      barplot(prop.table(tab,2), main = "All cities")
      
      # Boston
      Bostontab=table(Boston$instant_bookable, Boston$beds)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      # Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$beds)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      # DC
      DCtab=table(DC$instant_bookable, DC$beds)
      barplot(prop.table(DCtab,2), main = "DC")
      
      # LA
      LAtab=table(LA$instant_bookable, LA$beds)
      barplot(prop.table(LAtab,2), main = "LA")
      
      # NYC
      NYCtab=table(NYC$instant_bookable, NYC$beds)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      # SF
      SFtab=table(SF$instant_bookable, SF$beds)
      barplot(prop.table(SFtab,2), main = "SF")
      
      
    }
    #Cancellation policy-Beds
    { 
      #All cities
      tab=table(cancellation_policy,beds)
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy,Boston$beds)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy,Chicago$beds)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cancellation_policy,DC$beds)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cancellation_policy,LA$beds)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy,NYC$beds)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cancellation_policy,SF$beds)
      barplot(prop.table(SFtab,2), main = "SF")
    }
  }
  
  #Services-Accommodates
  {
    #Cleaning Fee-Accommodates
    {
      #All cities
      tab=table(cleaning_fee, accommodates);tab
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cleaning_fee, Boston$accommodates)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cleaning_fee, Chicago$accommodates)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cleaning_fee, DC$accommodates)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cleaning_fee, LA$accommodates)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cleaning_fee, NYC$accommodates)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cleaning_fee, SF$accommodates)
      barplot(prop.table(SFtab,2), main = "SF")
    }
    #Instant Bookable-Accommodates
    { 
      #All cities
      tab=table(instant_bookable,accommodates);tab
      barplot(prop.table(tab,2), main = "All cities")
      
      # Boston
      Bostontab=table(Boston$instant_bookable, Boston$accommodates)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      # Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$accommodates)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      # DC
      DCtab=table(DC$instant_bookable, DC$accommodates)
      barplot(prop.table(DCtab,2), main = "DC")
      
      # LA
      LAtab=table(LA$instant_bookable, LA$accommodates)
      barplot(prop.table(LAtab,2), main = "LA")
      
      # NYC
      NYCtab=table(NYC$instant_bookable, NYC$accommodates)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      # SF
      SFtab=table(SF$instant_bookable, SF$accommodates)
      barplot(prop.table(SFtab,2), main = "SF")
      
      
    }
    #Cancellation Policy-Accommodates
    { 
      #All cities
      tab=table(cancellation_policy,accommodates)
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy,Boston$accommodates)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy,Chicago$accommodates)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cancellation_policy,DC$accommodates)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cancellation_policy,LA$accommodates)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy,NYC$accommodates)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cancellation_policy,SF$accommodates)
      barplot(prop.table(SFtab,2), main = "SF")
    }
  }
  
  #Services-Host Identity Verified
  {
    #Cleaning fee-Host Identity Verified
    {  
      #All cities
      tab=table(cleaning_fee, host_identity_verified)
      prop.table(tab,1)
      prop.table(tab,2)
      barplot(prop.table(tab,2), main = "All cities", xlab = "host_identity_verified")
      
      #Boston
      Bostontab=table(Boston$cleaning_fee, Boston$host_identity_verified)
      barplot(prop.table(Bostontab,2), main = "Boston", xlab = "host_identity_verified")
      
      #Chicago
      Chicagotab=table(Chicago$cleaning_fee, Chicago$host_identity_verified)
      barplot(prop.table(Chicagotab,2), main = "Chicago", xlab = "host_identity_verified")
      
      #DC
      DCtab=table(DC$cleaning_fee, DC$host_identity_verified)
      barplot(prop.table(DCtab,2), main = "DC", xlab = "host_identity_verified")
      
      #LA
      LAtab=table(LA$cleaning_fee, LA$host_identity_verified)
      barplot(prop.table(LAtab,2), main = "LA", xlab = "host_identity_verified")
      
      #NYC
      NYCtab=table(NYC$cleaning_fee, NYC$host_identity_verified)
      barplot(prop.table(NYCtab,2), main = "NYC", xlab = "host_identity_verified")
      
      #SF
      SFtab=table(SF$cleaning_fee, SF$host_identity_verified)
      barplot(prop.table(SFtab,2), main = "SF", xlab = "host_identity_verified")
    }
    #Instant bookable-Host Identity Verified
    { 
      #All cities
      tab=table(instant_bookable, host_identity_verified)
      prop.table(tab,1)
      prop.table(tab,2)
      barplot(prop.table(tab,2), main = "All cities", xlab = "host_identity_verified")
      
      # Boston
      Bostontab=table(Boston$instant_bookable, Boston$host_identity_verified)
      barplot(prop.table(Bostontab,2), main = "Boston", xlab = "host_identity_verified")
      
      # Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$host_identity_verified)
      barplot(prop.table(Chicagotab,2), main = "Chicago", xlab = "host_identity_verified")
      
      # DC
      DCtab=table(DC$instant_bookable, DC$host_identity_verified)
      barplot(prop.table(DCtab,2), main = "DC", xlab = "host_identity_verified")
      
      # LA
      LAtab=table(LA$instant_bookable, LA$host_identity_verified)
      barplot(prop.table(LAtab,2), main = "LA", xlab = "host_identity_verified")
      
      # NYC
      NYCtab=table(NYC$instant_bookable, NYC$host_identity_verified)
      barplot(prop.table(NYCtab,2), main = "NYC", xlab = "host_identity_verified")
      
      # SF
      SFtab=table(SF$instant_bookable, SF$host_identity_verified)
      barplot(prop.table(SFtab,2), main = "SF")
      
      
    }
    #Cancellation Policy-Host Identity Verified
    { 
      #All cities
      tab=table(cancellation_policy, host_identity_verified)
      prop.table(tab,2)
      barplot(prop.table(tab,2), main = "All cities", xlab = "host_identity_verified")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy,Boston$host_identity_verified)
      barplot(prop.table(Bostontab,2), main = "Boston", xlab = "host_identity_verified")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy,Chicago$host_identity_verified)
      barplot(prop.table(Chicagotab,2), main = "Chicago", xlab = "host_identity_verified")
      
      #DC
      DCtab=table(DC$cancellation_policy,DC$host_identity_verified)
      barplot(prop.table(DCtab,2), main = "DC", xlab = "host_identity_verified")
      
      #LA
      LAtab=table(LA$cancellation_policy,LA$host_identity_verified)
      barplot(prop.table(LAtab,2), main = "LA", xlab = "host_identity_verified")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy,NYC$host_identity_verified)
      barplot(prop.table(NYCtab,2), main = "NYC", xlab = "host_identity_verified")
      
      #SF
      SFtab=table(SF$cancellation_policy,SF$host_identity_verified)
      barplot(prop.table(SFtab,2), main = "SF", xlab = "host_identity_verified")
    }
  }
  
  #Services-Host Response Rate
  {
    #Cleaning fee-Host Response Rate
    {
      #All cities
      tab=table(cleaning_fee, host_response_rate);round(prop.table(tab,2),5)*100
      barplot(prop.table(tab,2), main = "All cities",cex.names = 0.4,las = 2)
      
      #Boston
      Bostontab=table(Boston$cleaning_fee, Boston$host_response_rate)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cleaning_fee, Chicago$host_response_rate)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cleaning_fee, DC$host_response_rate)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cleaning_fee, LA$host_response_rate)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cleaning_fee, NYC$host_response_rate)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cleaning_fee, SF$host_response_rate)
      barplot(prop.table(SFtab,2), main = "SF")
  }
    #Instant Bookable-Host Response Rate
    { # all cities
      tab=table(instant_bookable, host_response_rate);tab
      barplot(prop.table(tab,2), main = "All cities")
      
      # Boston
      Bostontab=table(Boston$instant_bookable, Boston$host_response_rate)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      # Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$host_response_rate)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      # DC
      DCtab=table(DC$instant_bookable, DC$host_response_rate)
      barplot(prop.table(DCtab,2), main = "DC")
      
      # LA
      LAtab=table(LA$instant_bookable, LA$host_response_rate)
      barplot(prop.table(LAtab,2), main = "LA")
      
      # NYC
      NYCtab=table(NYC$instant_bookable, NYC$host_response_rate)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      # SF
      SFtab=table(SF$instant_bookable, SF$host_response_rate)
      barplot(prop.table(SFtab,2), main = "SF")
      
      
}
    #Cancellation Policy-Host Response Rate
    { 
      #All cities
      tab=table(cancellation_policy,host_response_rate); round(prop.table(tab,2),3)*100
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy,Boston$host_response_rate)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy,Chicago$host_response_rate)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cancellation_policy,DC$host_response_rate)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cancellation_policy,LA$host_response_rate)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy,NYC$host_response_rate)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cancellation_policy,SF$host_response_rate)
      barplot(prop.table(SFtab,2), main = "SF")
  }
  }
  
  #Services-Host Since
  {  
    #Cleaning Fee-Host Since
    {
      #All cities
      tab=table(cleaning_fee, host_since);tab
      round(prop.table(tab,2),5)*100
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cleaning_fee, Boston$host_since)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cleaning_fee, Chicago$host_since)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cleaning_fee, DC$host_since)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cleaning_fee, LA$host_since)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cleaning_fee, NYC$host_since)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cleaning_fee, SF$host_since)
      barplot(prop.table(SFtab,2), main = "SF")
    }
    #Instant Bookable-Host Since
    { 
      #All cities
      tab=table(instant_bookable, host_since);tab
      round(prop.table(tab,2),5)*100
      barplot(prop.table(tab,2), main = "All cities")
      
      # Boston
      Bostontab=table(Boston$instant_bookable, Boston$host_since)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      # Chicago
      Chicagotab=table(Chicago$instant_bookable, Chicago$host_since)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      # DC
      DCtab=table(DC$instant_bookable, DC$host_since)
      barplot(prop.table(DCtab,2), main = "DC")
      
      # LA
      LAtab=table(LA$instant_bookable, LA$host_since)
      barplot(prop.table(LAtab,2), main = "LA")
      
      # NYC
      NYCtab=table(NYC$instant_bookable, NYC$host_since)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      # SF
      SFtab=table(SF$instant_bookable, SF$host_since)
      barplot(prop.table(SFtab,2), main = "SF")
      
      
    }
    #Cancellation Policy-Host Since
    { 
      #All cities
      tab=table(cancellation_policy,host_since)
      round(prop.table(tab,2),5)*100
      barplot(prop.table(tab,2), main = "All cities")
      
      #Boston
      Bostontab=table(Boston$cancellation_policy,Boston$host_since)
      barplot(prop.table(Bostontab,2), main = "Boston")
      
      #Chicago
      Chicagotab=table(Chicago$cancellation_policy,Chicago$host_since)
      barplot(prop.table(Chicagotab,2), main = "Chicago")
      
      #DC
      DCtab=table(DC$cancellation_policy,DC$host_since)
      barplot(prop.table(DCtab,2), main = "DC")
      
      #LA
      LAtab=table(LA$cancellation_policy,LA$host_since)
      barplot(prop.table(LAtab,2), main = "LA")
      
      #NYC
      NYCtab=table(NYC$cancellation_policy,NYC$host_since)
      barplot(prop.table(NYCtab,2), main = "NYC")
      
      #SF
      SFtab=table(SF$cancellation_policy,SF$host_since)
      barplot(prop.table(SFtab,2), main = "SF")
    }
  }
}

#Simpson Paradox
{
  #All cities
  Q3 = quantile(log_price, 0.75); Q3
  tab1=data[log_price>Q3,]$room_type
  tab2=data[log_price>Q3,]$property_type
  table = table(tab1,tab2);table
  round(prop.table(table,2),5)*100
  #round(prop.table(table(tab1,tab2)[, colSums((table))>50],2),5)*100
  barplot(prop.table(table,2)[, colSums((table))>5],cex.names = 0.7,las = 2, main = "All cities")
  
  #Boston
  Q3 = quantile(Boston$log_price, 0.75); Q3
  tab1=Boston[log_price>Q3,]$room_type
  tab2=Boston[log_price>Q3,]$property_type
  Bostontable = table(tab1,tab2)
  round(prop.table(Bostontable,2),5)*100
  #round(prop.table(table(tab1,tab2)[, colSums((table))>20],2),5)*100
  barplot(prop.table(Bostontable,2)[, colSums((Bostontable))>5],cex.names = 0.7,las = 2, main = "Boston")
  
  #Chicago
  Q3 = quantile(Chicago$log_price, 0.75); Q3
  tab1=Chicago[log_price>Q3,]$room_type
  tab2=Chicago[log_price>Q3,]$property_type
  Chicagotable = table(tab1,tab2)
  round(prop.table(Chicagotable,2),5)*100
  #round(prop.table(table(tab1,tab2)[, colSums((table))>20],2),5)*100
  barplot(prop.table(Chicagotable,2)[, colSums((Chicagotable))>5],cex.names = 0.7,las = 2, main = "Chicago")
  
  #DC
  Q3 = quantile(DC$log_price, 0.75); Q3
  tab1=DC[log_price>Q3,]$room_type
  tab2=DC[log_price>Q3,]$property_type
  DCtable = table(tab1,tab2)
  round(prop.table(DCtable,2),5)*100
  #round(prop.table(table(tab1,tab2)[, colSums((table))>20],2),5)*100
  barplot(prop.table(DCtable,2)[, colSums((DCtable))>5],cex.names = 0.7,las = 2, main = "DC")
  
  #LA
  Q3 = quantile(LA$log_price, 0.75); Q3
  tab1=LA[log_price>Q3,]$room_type
  tab2=LA[log_price>Q3,]$property_type
  LAtable = table(tab1,tab2)
  round(prop.table(LAtable,2),5)*100
  #round(prop.table(table(tab1,tab2)[, colSums((table))>20],2),5)*100
  barplot(prop.table(LAtable,2)[, colSums((LAtable))>5],cex.names = 0.7,las = 2, main = "LA")
  
  #NYC
  Q3 = quantile(NYC$log_price, 0.75); Q3
  tab1=NYC[log_price>Q3,]$room_type
  tab2=NYC[log_price>Q3,]$property_type
  NYCtable = table(tab1,tab2)
  round(prop.table(NYCtable,2),5)*100
  #round(prop.table(table(tab1,tab2)[, colSums((table))>20],2),5)*100
  barplot(prop.table(NYCtable,2)[, colSums((NYCtable))>5],cex.names = 0.7,las = 2, main = "NYC")
  
  #SF
  Q3 = quantile(SF$log_price, 0.75); Q3
  tab1=SF[log_price>Q3,]$room_type
  tab2=SF[log_price>Q3,]$property_type
  SFtable = table(tab1,tab2)
  round(prop.table(SFtable,2),5)*100
  #round(prop.table(table(tab1,tab2)[, colSums((table))>20],2),5)*100
  barplot(prop.table(SFtable,2)[, colSums((SFtable))>5],cex.names = 0.7,las = 2, main = "SF")
}
}

#Property type analysis
{
  #We compute the mean price for each property type for each city and draw a relationship with the mean of the variables of the "placement" cluster
  #Boston
    {
      city=Boston
      tab = table(city$property_type)
      
      #Creation of the data frame with the variables we're interested in
      my_table <- table(city$property_type)
      my_subset <- names(which(my_table > 1))
      my_df <- subset(city, property_type %in% my_subset, select = c(property_type, log_price,accommodates,bedrooms,beds,bathrooms))
      my_df
      
      #Create the table with all the information we're looking for
      mean_prices <- aggregate(cbind(my_df$log_price, my_df$accommodates, my_df$bedrooms, my_df$beds, my_df$bathrooms) ~ my_df$property_type, data = my_df, mean)
      mean_prices <- mean_prices[order(mean_prices$V1, decreasing = TRUE), ]
      colnames(mean_prices)[1] <- "property_type"
      colnames(mean_prices)[2] <- "log_price"
      colnames(mean_prices)[3] <- "accommodates"
      colnames(mean_prices)[4] <- "bedrooms"
      colnames(mean_prices)[5] <- "beds"
      colnames(mean_prices)[6] <- "bathrooms"
      mean_prices
    }
    
  #Chicago
    {
      city=Chicago
      tab = table(city$property_type)
      
      #Creation of the data frame with the variables we're interested in
      my_table <- table(city$property_type)
      my_subset <- names(which(my_table > 1))
      my_df <- subset(city, property_type %in% my_subset, select = c(property_type, log_price,accommodates,bedrooms,beds,bathrooms))
      my_df
      
      #Create the table with all the information we're looking for
      mean_prices <- aggregate(cbind(my_df$log_price, my_df$accommodates, my_df$bedrooms, my_df$beds, my_df$bathrooms) ~ my_df$property_type, data = my_df, mean)
      mean_prices <- mean_prices[order(mean_prices$V1, decreasing = TRUE), ]
      colnames(mean_prices)[1] <- "property_type"
      colnames(mean_prices)[2] <- "log_price"
      colnames(mean_prices)[3] <- "accommodates"
      colnames(mean_prices)[4] <- "bedrooms"
      colnames(mean_prices)[5] <- "beds"
      colnames(mean_prices)[6] <- "bathrooms"
      mean_prices
    }
    
  #DC
    {
      city=DC
      tab = table(city$property_type)
      
      #Creation of the data frame with the variables we're interested in
      my_table <- table(city$property_type)
      my_subset <- names(which(my_table > 1))
      my_df <- subset(city, property_type %in% my_subset, select = c(property_type, log_price,accommodates,bedrooms,beds,bathrooms))
      my_df
      
      #Create the table with all the information we're looking for
      mean_prices <- aggregate(cbind(my_df$log_price, my_df$accommodates, my_df$bedrooms, my_df$beds, my_df$bathrooms) ~ my_df$property_type, data = my_df, mean)
      mean_prices <- mean_prices[order(mean_prices$V1, decreasing = TRUE), ]
      colnames(mean_prices)[1] <- "property_type"
      colnames(mean_prices)[2] <- "log_price"
      colnames(mean_prices)[3] <- "accommodates"
      colnames(mean_prices)[4] <- "bedrooms"
      colnames(mean_prices)[5] <- "beds"
      colnames(mean_prices)[6] <- "bathrooms"
      mean_prices
    }
    
  #LA
    {
      city=LA
      tab = table(city$property_type)
      
      #Creation of the data frame with the variables we're interested in
      my_table <- table(city$property_type)
      my_subset <- names(which(my_table > 1))
      my_df <- subset(city, property_type %in% my_subset, select = c(property_type, log_price,accommodates,bedrooms,beds,bathrooms))
      my_df
      
      #Create the table with all the information we're looking for
      mean_prices <- aggregate(cbind(my_df$log_price, my_df$accommodates, my_df$bedrooms, my_df$beds, my_df$bathrooms) ~ my_df$property_type, data = my_df, mean)
      mean_prices <- mean_prices[order(mean_prices$V1, decreasing = TRUE), ]
      colnames(mean_prices)[1] <- "property_type"
      colnames(mean_prices)[2] <- "log_price"
      colnames(mean_prices)[3] <- "accommodates"
      colnames(mean_prices)[4] <- "bedrooms"
      colnames(mean_prices)[5] <- "beds"
      colnames(mean_prices)[6] <- "bathrooms"
      mean_prices
    }
    
  #NYC
    {
      city=NYC
      tab = table(city$property_type)
      
      #Creation of the data frame with the variables we're interested in
      my_table <- table(city$property_type)
      my_subset <- names(which(my_table > 1))
      my_df <- subset(city, property_type %in% my_subset, select = c(property_type, log_price,accommodates,bedrooms,beds,bathrooms))
      my_df
      
      #Create the table with all the information we're looking for
      mean_prices <- aggregate(cbind(my_df$log_price, my_df$accommodates, my_df$bedrooms, my_df$beds, my_df$bathrooms) ~ my_df$property_type, data = my_df, mean)
      mean_prices <- mean_prices[order(mean_prices$V1, decreasing = TRUE), ]
      colnames(mean_prices)[1] <- "property_type"
      colnames(mean_prices)[2] <- "log_price"
      colnames(mean_prices)[3] <- "accommodates"
      colnames(mean_prices)[4] <- "bedrooms"
      colnames(mean_prices)[5] <- "beds"
      colnames(mean_prices)[6] <- "bathrooms"
      mean_prices
    }
    
  #SF
    {
      city=SF
      tab = table(city$property_type)
      
      #Creation of the data frame with the variables we're interested in
      my_table <- table(city$property_type)
      my_subset <- names(which(my_table > 1))
      my_df <- subset(city, property_type %in% my_subset, select = c(property_type, log_price,accommodates,bedrooms,beds,bathrooms))
      my_df
      
      #Create the table with all the information we're looking for
      mean_prices <- aggregate(cbind(my_df$log_price, my_df$accommodates, my_df$bedrooms, my_df$beds, my_df$bathrooms) ~ my_df$property_type, data = my_df, mean)
      mean_prices <- mean_prices[order(mean_prices$V1, decreasing = TRUE), ]
      colnames(mean_prices)[1] <- "property_type"
      colnames(mean_prices)[2] <- "log_price"
      colnames(mean_prices)[3] <- "accommodates"
      colnames(mean_prices)[4] <- "bedrooms"
      colnames(mean_prices)[5] <- "beds"
      colnames(mean_prices)[6] <- "bathrooms"
      mean_prices
    }
    
  #Understand the number of property_type for each city
    {
    aggregate(x = Polished$property_type, 
              by = list(city = Polished$city, 
                        property_type = Polished$property_type), 
              FUN = length)
    }
}

#Property type and host since analysis
{
  d = Polished
  d$host_since = as.Date(d$host_since)
  d$host_since_year = as.numeric(format(d$host_since, "%Y"))
  hist(d$host_since_year)
  
  ggplot(d, aes(x = host_since_year, fill = city)) +
    geom_histogram(binwidth = 1, alpha = 0.5) +
    scale_fill_manual(values = c("blue", "green", "red", "orange", "black", "yellow")) +
    labs(title = "Histogram of host_since_year by city",
         x = "Year", y = "Frequency", fill = "City")
  
  cities = c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
  years = 2007:2018
  
  # Define the cities and years of interest
  cities <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
  years <- 2009:2017
  
  increments <- matrix(0, nrow=length(cities), ncol=length(years))
  for (i in 1:length(cities)) {
    for (j in 1:length(years)) {
      subset <- d[d$city == cities[i] & d$host_since_year == years[j],]
      n <- nrow(subset)
      increments[i, j] <- ifelse(n == 0, 0, n) # set increment to 0 if subset is empty
    }
  }
  colnames(increments) <- years
  rownames(increments) <- cities
  
  percent_changes <- matrix(0, nrow=length(cities), ncol=length(years)-1)
  colnames(percent_changes) <- paste0("∆", years[-1]) # exclude the first year from the column names
  rownames(percent_changes) <- cities
  
  for (i in 1:length(cities)) {
    for (j in 1:(length(years)-1)) {
      percent_changes[i, j] <- paste0(round((increments[i, j+1] - increments[i, j]) / increments[i, j] * 100, 2), "%")
    }
  }
  
  # Convert the increments matrix to a data frame
  increments_df <- as.data.frame(increments)
  increments_df$city <- rownames(increments_df)
  increments_df_long <- reshape2::melt(increments_df, id.vars="city", variable.name="year", value.name="increment")
  
  # Convert the year variable to numeric
  increments_df_long$year <- as.numeric(as.character(increments_df_long$year))
  
  # Create the line chart
  ggplot(increments_df_long, aes(x=year, y=increment, group=city, color=city)) +
    geom_line() +
    ggtitle("Airbnb Listings by City and Year") +
    xlab("Year") +
    ylab("Number of Listings") +
    scale_x_continuous(breaks=years) +
    theme_bw()
  
  unique(property_type)
  
  # Create a matrix with as many rows as the product of the number of cities and the number of property types, and as many columns as the number of years
  increments_by_property_type <- matrix(0, nrow=length(property_type) * length(cities), ncol=length(years))
  for (i in 1:length(property_type)) {
    for (j in 1:length(cities)) {
      for (k in 1:length(years)) {
        subset <- d[d$city == cities[j] & d$host_since_year == years[k] & d$property_type == property_type[i],]
        n <- nrow(subset)
        increments_by_property_type[(i-1) * length(cities) + j, k] <- ifelse(n == 0, 0, n)
      }
    }
  }
  colnames(increments_by_property_type) <- years
  rownames(increments_by_property_type) <- rep(property_type, times = length(cities))
  
  # Convert the increments_by_property_type matrix to a data frame
  increments_df <- data.frame(
    property_type = rep(property_type, times = length(cities) * length(years)),
    city = rep(cities, each = length(property_type) * length(years)),
    year = rep(rep(years, each = length(property_type)), length(cities)),
    increment = as.vector(increments_by_property_type)
  )
  
  # Create the line chart
  ggplot(increments_df, aes(x=year, y=increment, group=property_type, color=property_type)) +
    geom_line() +
    ggtitle("Airbnb Listings by Property Type and Year") +
    xlab("Year") +
    ylab("Number of Listings") +
    scale_x_continuous(breaks=years) +
    theme_bw()
  
  ###########
  
  # Create a data frame with all combinations of property_type and years
  df <- expand.grid(property_type = unique(d$property_type[!d$property_type %in% c("Apartment", "House")]), 
                    year = unique(d$host_since_year))
  
  # Summarize the data by property_type and year
  increments_df <- d %>%
    filter(host_since_year %in% df$year, property_type %in% df$property_type) %>%
    group_by(property_type, host_since_year) %>%
    summarize(increment = n())
  
  # Create the line chart
  ggplot(increments_df, aes(x=host_since_year, y=increment, group=property_type, color=property_type)) +
    geom_line() +
    ggtitle("Airbnb Listings by Property Type and Year") +
    xlab("Year") +
    ylab("Number of Listings") +
    scale_x_continuous(breaks=unique(df$year)) +
    theme_bw()
  
  #######
  
  # Create a data frame with all combinations of property_type and years
  df <- expand.grid(property_type = unique(d$property_type[!d$property_type %in% c("Apartment", "House")]), 
                    year = unique(d$host_since_year))
  
  # Summarize the data by property_type and year
  increments_df <- d %>%
    filter(host_since_year %in% df$year, property_type %in% df$property_type) %>%
    group_by(property_type, host_since_year) %>%
    summarize(increment = n())
  
  # Split the data into two data frames based on the number of occurrences
  less_than_20 <- increments_df %>% filter(increment < 20)
  more_than_20 <- increments_df %>% filter(increment >= 20)
  
  # Create the line charts
  p1 <- ggplot(less_than_20, aes(x=host_since_year, y=increment, group=property_type, color=property_type)) +
    geom_line() +
    ggtitle("Airbnb Listings by Property Type and Year (Less than 20 occurrences)") +
    xlab("Year") +
    ylab("Number of Listings") +
    scale_x_continuous(breaks=unique(df$year)) +
    theme_bw()
  
  p2 <- ggplot(more_than_20, aes(x=host_since_year, y=increment, group=property_type, color=property_type)) +
    geom_line() +
    ggtitle("Airbnb Listings by Property Type and Year (20 or more occurrences)") +
    xlab("Year") +
    ylab("Number of Listings") +
    scale_x_continuous(breaks=unique(df$year)) +
    theme_bw()
  
  # Arrange the two plots side by side
  grid.arrange(p1, p2, ncol=2)
  
  # Filter the data for the property types of interest
  df <- d %>% filter(property_type %in% unique(d$property_type[!d$property_type %in% c("Apartment", "House")]))
  
  # Summarize the data by property_type and year
  increments_df <- df %>%
    group_by(property_type, host_since_year) %>%
    summarize(increment = n())
  
  # Reshape the data into a wide format
  table_df <- increments_df %>% 
    pivot_wider(names_from = host_since_year, values_from = increment)
  
  # Print the table
  table_df
  # Filter the data for the property types of interest
  df <- d[d$property_type %in% unique(d$property_type[!d$property_type %in% c("Apartment", "House")]),]
  
  # Create a matrix to store the number of listings for each property type and year
  years <- sort(unique(df$host_since_year))
  property_types <- unique(df$property_type)
  n_years <- length(years)
  n_property_types <- length(property_types)
  matrix_df <- matrix(0, nrow = n_property_types, ncol = n_years)
  rownames(matrix_df) <- property_types
  colnames(matrix_df) <- years
  
  # Fill the matrix with the number of listings for each property type and year
  for (i in 1:n_property_types) {
    for (j in 1:n_years) {
      increment <- nrow(df[df$property_type == property_types[i] & df$host_since_year == years[j],])
      matrix_df[i,j] <- increment
    }
  }
  
  # Print the matrix
  matrix_df
  
  # Load data
  airbnb <- d
  
  # Calculate average log_price by property type and year
  log_price_avg <- airbnb %>%
    group_by(property_type, host_since_year) %>%
    summarise(avg_log_price = mean(log_price), .groups = "drop")
  
  # Convert to wide format
  log_price_wide <- log_price_avg %>%
    pivot_wider(names_from = host_since_year, values_from = avg_log_price)
  
  # Print table
  print(log_price_wide, n = 35)
  library(ggplot2)
  
  # Calculate average log price for each property type over the years
  avg_log_price <- aggregate(log_price ~ property_type + host_since_year, data = d, FUN = median)
  
  # Plot the trend of average log price over the years for each property type
  ggplot(avg_log_price, aes(x = host_since_year, y = log_price, color = property_type)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2008, 2017, by = 1)) +
    labs(title = "Average Log Price by Property Type over the Years",
         x = "Year",
         y = "Average Log Price")
}

################################################################################

#City Analysis
{
  #General code
  {
    #The code won't run as long as we don't define the city. Here you can have a look at it, after an analysis for each city will be made
    
    #I create the data frame with the variables I'm interested in
    #I cannot focus on all the neighbourhoods, thus I take the ones with at least x properties
    {
      my_table <- table(city$neighbourhood); my_table
      my_subset <- names(which(my_table > x)); my_subset
      my_df <- subset(city, neighbourhood %in% my_subset, select = c(neighbourhood, log_price, host_identity_verified, host_since, host_response_rate, first_review, review_scores_rating,number_of_reviews,cancellation_policy,instant_bookable,cleaning_fee, property_type, room_type, accommodates, beds, bedrooms, bathrooms))
      my_df
    }
    
    #Understand which are the top 3 neighbourhoods, and the last 3 neighbourhoods
    {
      mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
      mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]; mean_prices
      colnames(mean_prices)[1]= "Neighbourhood"
      colnames(mean_prices)[2]= "log_price"
      mean_prices
    }
    
    #Call the top 10 neighbourhoods (1-10) and the last 10 neighbourhoods (11-20) as in some cases we want to deepen our analysis
    {
      highest_neighbourhood <- mean_prices[1, "Neighbourhood"]
      new_data_1<- subset(my_df, neighbourhood == highest_neighbourhood)
      second_neighbourhood <- mean_prices[2, "Neighbourhood"]
      new_data_2 <- subset(my_df, neighbourhood == second_neighbourhood)
      third_neighbourhood <- mean_prices[3, "Neighbourhood"]
      new_data_3 <- subset(my_df, neighbourhood == third_neighbourhood)
      fourth_neighbourhood <- mean_prices[4, "Neighbourhood"]
      new_data_4 <- subset(my_df, neighbourhood == fourth_neighbourhood)
      fifth_neighbourhood <- mean_prices[5, "Neighbourhood"]
      new_data_5 <- subset(my_df, neighbourhood == fifth_neighbourhood)
      sixth_neighbourhood <- mean_prices[6, "Neighbourhood"]
      new_data_6 <- subset(my_df, neighbourhood == sixth_neighbourhood)
      seventh_neighbourhood <- mean_prices[7, "Neighbourhood"]
      new_data_7 <- subset(my_df, neighbourhood == seventh_neighbourhood)
      eigth_neighbourhood <- mean_prices[8, "Neighbourhood"]
      new_data_8 <- subset(my_df, neighbourhood == eigth_neighbourhood)
      ninth_neighbourhood <- mean_prices[9, "Neighbourhood"]
      new_data_9 <- subset(my_df, neighbourhood == ninth_neighbourhood)
      tenth_neighbourhood <- mean_prices[10, "Neighbourhood"]
      new_data_10 <- subset(my_df, neighbourhood == tenth_neighbourhood)
      
      last_tenth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-9, "Neighbourhood"]
      new_data_11 <- subset(my_df, neighbourhood == last_tenth_neighbourhood)
      last_ninth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-8, "Neighbourhood"]
      new_data_12 <- subset(my_df, neighbourhood == last_ninth_neighbourhood)
      last_eigth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-7, "Neighbourhood"]
      new_data_13 <- subset(my_df, neighbourhood == last_eigth_neighbourhood)
      last_seventh_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-6, "Neighbourhood"]
      new_data_14 <- subset(my_df, neighbourhood == last_seventh_neighbourhood)
      last_sixth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-5, "Neighbourhood"]
      new_data_15 <- subset(my_df, neighbourhood == last_sixth_neighbourhood)
      last_fifth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-4, "Neighbourhood"]
      new_data_16 <- subset(my_df, neighbourhood == last_fifth_neighbourhood)
      last_fourth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-3, "Neighbourhood"]
      new_data_17 <- subset(my_df, neighbourhood == last_fourth_neighbourhood)
      last_third_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-2, "Neighbourhood"]
      new_data_18 <- subset(my_df, neighbourhood == last_third_neighbourhood)
      last_second_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-1, "Neighbourhood"]
      new_data_19 <- subset(my_df, neighbourhood == last_second_neighbourhood)
      last_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood), "Neighbourhood"]
      new_data_20 <- subset(my_df, neighbourhood == last_neighbourhood)
    }
  }
  
  #Boston
  {
    city=Boston
    x=5
    
    #General code
    {
      #I create the data frame with the variables I'm interested in
      #I cannot focus on all the neighbourhoods, thus I take the ones with at least x properties
      {
        my_table <- table(city$neighbourhood); my_table
        my_subset <- names(which(my_table > x)); my_subset
        my_df <- subset(city, neighbourhood %in% my_subset, select = c(neighbourhood, log_price, host_identity_verified, host_since, host_response_rate, first_review, review_scores_rating,number_of_reviews,cancellation_policy,instant_bookable,cleaning_fee, property_type, room_type, accommodates, beds, bedrooms, bathrooms))
        my_df
      }
      
      #Understand which are the top 3 neighbourhoods, and the last 3 neighbourhoods
      {
        mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
        mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]; mean_prices
        colnames(mean_prices)[1]= "Neighbourhood"
        colnames(mean_prices)[2]= "log_price"
        mean_prices
      }
      
      #Call the top 10 neighbourhoods (1-10) and the last 10 neighbourhoods (11-20) as in some cases we want to deepen our analysis
      {
        highest_neighbourhood <- mean_prices[1, "Neighbourhood"]
        new_data_1<- subset(my_df, neighbourhood == highest_neighbourhood)
        second_neighbourhood <- mean_prices[2, "Neighbourhood"]
        new_data_2 <- subset(my_df, neighbourhood == second_neighbourhood)
        third_neighbourhood <- mean_prices[3, "Neighbourhood"]
        new_data_3 <- subset(my_df, neighbourhood == third_neighbourhood)
        fourth_neighbourhood <- mean_prices[4, "Neighbourhood"]
        new_data_4 <- subset(my_df, neighbourhood == fourth_neighbourhood)
        fifth_neighbourhood <- mean_prices[5, "Neighbourhood"]
        new_data_5 <- subset(my_df, neighbourhood == fifth_neighbourhood)
        sixth_neighbourhood <- mean_prices[6, "Neighbourhood"]
        new_data_6 <- subset(my_df, neighbourhood == sixth_neighbourhood)
        seventh_neighbourhood <- mean_prices[7, "Neighbourhood"]
        new_data_7 <- subset(my_df, neighbourhood == seventh_neighbourhood)
        eigth_neighbourhood <- mean_prices[8, "Neighbourhood"]
        new_data_8 <- subset(my_df, neighbourhood == eigth_neighbourhood)
        ninth_neighbourhood <- mean_prices[9, "Neighbourhood"]
        new_data_9 <- subset(my_df, neighbourhood == ninth_neighbourhood)
        tenth_neighbourhood <- mean_prices[10, "Neighbourhood"]
        new_data_10 <- subset(my_df, neighbourhood == tenth_neighbourhood)
        
        last_tenth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-9, "Neighbourhood"]
        new_data_11 <- subset(my_df, neighbourhood == last_tenth_neighbourhood)
        last_ninth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-8, "Neighbourhood"]
        new_data_12 <- subset(my_df, neighbourhood == last_ninth_neighbourhood)
        last_eigth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-7, "Neighbourhood"]
        new_data_13 <- subset(my_df, neighbourhood == last_eigth_neighbourhood)
        last_seventh_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-6, "Neighbourhood"]
        new_data_14 <- subset(my_df, neighbourhood == last_seventh_neighbourhood)
        last_sixth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-5, "Neighbourhood"]
        new_data_15 <- subset(my_df, neighbourhood == last_sixth_neighbourhood)
        last_fifth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-4, "Neighbourhood"]
        new_data_16 <- subset(my_df, neighbourhood == last_fifth_neighbourhood)
        last_fourth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-3, "Neighbourhood"]
        new_data_17 <- subset(my_df, neighbourhood == last_fourth_neighbourhood)
        last_third_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-2, "Neighbourhood"]
        new_data_18 <- subset(my_df, neighbourhood == last_third_neighbourhood)
        last_second_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-1, "Neighbourhood"]
        new_data_19 <- subset(my_df, neighbourhood == last_second_neighbourhood)
        last_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood), "Neighbourhood"]
        new_data_20 <- subset(my_df, neighbourhood == last_neighbourhood)
      }
    }
    
    #Location-Cancellation Policy
    {
      data_list <- list(new_data_1, new_data_2, new_data_3, new_data_4, new_data_5, new_data_6, new_data_7, new_data_8, new_data_9, new_data_10, new_data_11, new_data_12, new_data_13, new_data_14, new_data_15, new_data_16, new_data_17, new_data_18, new_data_19, new_data_20)
      results <- list()
      for (i in seq_along(data_list)) {
        results[[i]] <- tapply(data_list[[i]]$log_price, data_list[[i]]$cancellation_policy, mean)
      }
      results
    }
    
    #Location-Instant Bookable
    {
      data_list <- list(new_data_1, new_data_2, new_data_3, new_data_4, new_data_5, new_data_6, new_data_7, new_data_8, new_data_9, new_data_10)
      results <- list()
      for (i in seq_along(data_list)) {
        results[[i]] <- tapply(data_list[[i]]$log_price, data_list[[i]]$instant_bookable, mean)
      }
      results
    }
    
  }
  
  #Chicago
  {
    city=Chicago
    x=5
    
    #General code
    {
      #I create the data frame with the variables I'm interested in
      #I cannot focus on all the neighbourhoods, thus I take the ones with at least x properties
      {
        my_table <- table(city$neighbourhood); my_table
        my_subset <- names(which(my_table > x)); my_subset
        my_df <- subset(city, neighbourhood %in% my_subset, select = c(neighbourhood, log_price, host_identity_verified, host_since, host_response_rate, first_review, review_scores_rating,number_of_reviews,cancellation_policy,instant_bookable,cleaning_fee, property_type, room_type, accommodates, beds, bedrooms, bathrooms))
        my_df
      }
      
      #Understand which are the top 3 neighbourhoods, and the last 3 neighbourhoods
      {
        mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
        mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]; mean_prices
        colnames(mean_prices)[1]= "Neighbourhood"
        colnames(mean_prices)[2]= "log_price"
        mean_prices
      }
      
      #Call the top 10 neighbourhoods (1-10) and the last 10 neighbourhoods (11-20) as in some cases we want to deepen our analysis
      {
        highest_neighbourhood <- mean_prices[1, "Neighbourhood"]
        new_data_1<- subset(my_df, neighbourhood == highest_neighbourhood)
        second_neighbourhood <- mean_prices[2, "Neighbourhood"]
        new_data_2 <- subset(my_df, neighbourhood == second_neighbourhood)
        third_neighbourhood <- mean_prices[3, "Neighbourhood"]
        new_data_3 <- subset(my_df, neighbourhood == third_neighbourhood)
        fourth_neighbourhood <- mean_prices[4, "Neighbourhood"]
        new_data_4 <- subset(my_df, neighbourhood == fourth_neighbourhood)
        fifth_neighbourhood <- mean_prices[5, "Neighbourhood"]
        new_data_5 <- subset(my_df, neighbourhood == fifth_neighbourhood)
        sixth_neighbourhood <- mean_prices[6, "Neighbourhood"]
        new_data_6 <- subset(my_df, neighbourhood == sixth_neighbourhood)
        seventh_neighbourhood <- mean_prices[7, "Neighbourhood"]
        new_data_7 <- subset(my_df, neighbourhood == seventh_neighbourhood)
        eigth_neighbourhood <- mean_prices[8, "Neighbourhood"]
        new_data_8 <- subset(my_df, neighbourhood == eigth_neighbourhood)
        ninth_neighbourhood <- mean_prices[9, "Neighbourhood"]
        new_data_9 <- subset(my_df, neighbourhood == ninth_neighbourhood)
        tenth_neighbourhood <- mean_prices[10, "Neighbourhood"]
        new_data_10 <- subset(my_df, neighbourhood == tenth_neighbourhood)
        
        last_tenth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-9, "Neighbourhood"]
        new_data_11 <- subset(my_df, neighbourhood == last_tenth_neighbourhood)
        last_ninth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-8, "Neighbourhood"]
        new_data_12 <- subset(my_df, neighbourhood == last_ninth_neighbourhood)
        last_eigth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-7, "Neighbourhood"]
        new_data_13 <- subset(my_df, neighbourhood == last_eigth_neighbourhood)
        last_seventh_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-6, "Neighbourhood"]
        new_data_14 <- subset(my_df, neighbourhood == last_seventh_neighbourhood)
        last_sixth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-5, "Neighbourhood"]
        new_data_15 <- subset(my_df, neighbourhood == last_sixth_neighbourhood)
        last_fifth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-4, "Neighbourhood"]
        new_data_16 <- subset(my_df, neighbourhood == last_fifth_neighbourhood)
        last_fourth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-3, "Neighbourhood"]
        new_data_17 <- subset(my_df, neighbourhood == last_fourth_neighbourhood)
        last_third_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-2, "Neighbourhood"]
        new_data_18 <- subset(my_df, neighbourhood == last_third_neighbourhood)
        last_second_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-1, "Neighbourhood"]
        new_data_19 <- subset(my_df, neighbourhood == last_second_neighbourhood)
        last_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood), "Neighbourhood"]
        new_data_20 <- subset(my_df, neighbourhood == last_neighbourhood)
      }
    }
    
    #Location-Cancellation Policy
    {
      data_list <- list(new_data_1, new_data_2, new_data_3, new_data_4, new_data_5, new_data_6, new_data_7, new_data_8, new_data_9, new_data_10)
      results <- list()
      for (i in seq_along(data_list)) {
        results[[i]] <- tapply(data_list[[i]]$log_price, data_list[[i]]$cancellation_policy, mean)
      }
      results
    }
    
  }
  
  #DC
  {
    city=DC
    x=3
    
    #General code
    {
      #I create the data frame with the variables I'm interested in
      #I cannot focus on all the neighbourhoods, thus I take the ones with at least x properties
      {
        my_table <- table(city$neighbourhood); my_table
        my_subset <- names(which(my_table > x)); my_subset
        my_df <- subset(city, neighbourhood %in% my_subset, select = c(neighbourhood, log_price, host_identity_verified, host_since, host_response_rate, first_review, review_scores_rating,number_of_reviews,cancellation_policy,instant_bookable,cleaning_fee, property_type, room_type, accommodates, beds, bedrooms, bathrooms))
        my_df
      }
      
      #Understand which are the top 3 neighbourhoods, and the last 3 neighbourhoods
      {
        mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
        mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]; mean_prices
        colnames(mean_prices)[1]= "Neighbourhood"
        colnames(mean_prices)[2]= "log_price"
        mean_prices
      }
      
      #Call the top 10 neighbourhoods (1-10) and the last 10 neighbourhoods (11-20) as in some cases we want to deepen our analysis
      {
        highest_neighbourhood <- mean_prices[1, "Neighbourhood"]
        new_data_1<- subset(my_df, neighbourhood == highest_neighbourhood)
        second_neighbourhood <- mean_prices[2, "Neighbourhood"]
        new_data_2 <- subset(my_df, neighbourhood == second_neighbourhood)
        third_neighbourhood <- mean_prices[3, "Neighbourhood"]
        new_data_3 <- subset(my_df, neighbourhood == third_neighbourhood)
        fourth_neighbourhood <- mean_prices[4, "Neighbourhood"]
        new_data_4 <- subset(my_df, neighbourhood == fourth_neighbourhood)
        fifth_neighbourhood <- mean_prices[5, "Neighbourhood"]
        new_data_5 <- subset(my_df, neighbourhood == fifth_neighbourhood)
        sixth_neighbourhood <- mean_prices[6, "Neighbourhood"]
        new_data_6 <- subset(my_df, neighbourhood == sixth_neighbourhood)
        seventh_neighbourhood <- mean_prices[7, "Neighbourhood"]
        new_data_7 <- subset(my_df, neighbourhood == seventh_neighbourhood)
        eigth_neighbourhood <- mean_prices[8, "Neighbourhood"]
        new_data_8 <- subset(my_df, neighbourhood == eigth_neighbourhood)
        ninth_neighbourhood <- mean_prices[9, "Neighbourhood"]
        new_data_9 <- subset(my_df, neighbourhood == ninth_neighbourhood)
        tenth_neighbourhood <- mean_prices[10, "Neighbourhood"]
        new_data_10 <- subset(my_df, neighbourhood == tenth_neighbourhood)
        
        last_tenth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-9, "Neighbourhood"]
        new_data_11 <- subset(my_df, neighbourhood == last_tenth_neighbourhood)
        last_ninth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-8, "Neighbourhood"]
        new_data_12 <- subset(my_df, neighbourhood == last_ninth_neighbourhood)
        last_eigth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-7, "Neighbourhood"]
        new_data_13 <- subset(my_df, neighbourhood == last_eigth_neighbourhood)
        last_seventh_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-6, "Neighbourhood"]
        new_data_14 <- subset(my_df, neighbourhood == last_seventh_neighbourhood)
        last_sixth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-5, "Neighbourhood"]
        new_data_15 <- subset(my_df, neighbourhood == last_sixth_neighbourhood)
        last_fifth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-4, "Neighbourhood"]
        new_data_16 <- subset(my_df, neighbourhood == last_fifth_neighbourhood)
        last_fourth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-3, "Neighbourhood"]
        new_data_17 <- subset(my_df, neighbourhood == last_fourth_neighbourhood)
        last_third_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-2, "Neighbourhood"]
        new_data_18 <- subset(my_df, neighbourhood == last_third_neighbourhood)
        last_second_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-1, "Neighbourhood"]
        new_data_19 <- subset(my_df, neighbourhood == last_second_neighbourhood)
        last_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood), "Neighbourhood"]
        new_data_20 <- subset(my_df, neighbourhood == last_neighbourhood)
      }
    }
    
    #Location-Number of Reviews
    {
      cor(new_data_1$number_of_reviews,new_data_1$log_price, use="complete.obs")
      plot(new_data_1$number_of_reviews,new_data_1$log_price)
      abline(lm(new_data_1$log_price~new_data_1$number_of_reviews),col="red")
      
      cor(new_data_2$number_of_reviews,new_data_2$log_price, use="complete.obs")
      plot(new_data_2$number_of_reviews,new_data_2$log_price)
      abline(lm(new_data_2$log_price~new_data_2$number_of_reviews),col="red")
      
      cor(new_data_3$number_of_reviews,new_data_3$log_price, use="complete.obs")
      plot(new_data_3$number_of_reviews,new_data_3$log_price)
      abline(lm(new_data_3$log_price~new_data_3$number_of_reviews),col="red")
      
      cor(new_data_4$number_of_reviews,new_data_4$log_price, use="complete.obs")
      plot(new_data_4$number_of_reviews,new_data_4$log_price)
      abline(lm(new_data_4$log_price~new_data_4$number_of_reviews),col="red")
      
      cor(new_data_5$number_of_reviews,new_data_5$log_price, use="complete.obs")
      plot(new_data_5$number_of_reviews,new_data_5$log_price)
      abline(lm(new_data_5$log_price~new_data_5$number_of_reviews),col="red")
      
      cor(new_data_6$number_of_reviews,new_data_6$log_price, use="complete.obs")
      plot(new_data_6$number_of_reviews,new_data_6$log_price)
      abline(lm(new_data_6$log_price~new_data_6$number_of_reviews),col="red")
      
      cor(new_data_7$number_of_reviews,new_data_7$log_price, use="complete.obs")
      plot(new_data_7$number_of_reviews,new_data_7$log_price)
      abline(lm(new_data_7$log_price~new_data_7$number_of_reviews),col="red")
      
      cor(new_data_8$number_of_reviews,new_data_8$log_price, use="complete.obs")
      plot(new_data_8$number_of_reviews,new_data_8$log_price)
      abline(lm(new_data_8$log_price~new_data_8$number_of_reviews),col="red")
      
      cor(new_data_9$number_of_reviews,new_data_9$log_price, use="complete.obs")
      plot(new_data_9$number_of_reviews,new_data_9$log_price)
      abline(lm(new_data_9$log_price~new_data_9$number_of_reviews),col="red")
      
      cor(new_data_10$number_of_reviews,new_data_10$log_price, use="complete.obs")
      plot(new_data_10$number_of_reviews,new_data_10$log_price)
      abline(lm(new_data_10$log_price~new_data_10$number_of_reviews),col="red")
      
      cor(new_data_18$number_of_reviews,new_data_18$log_price, use="complete.obs")
      plot(new_data_18$number_of_reviews,new_data_18$log_price)
      abline(lm(new_data_18$log_price~new_data_18$number_of_reviews),col="red")
      
      cor(new_data_19$number_of_reviews,new_data_19$log_price, use="complete.obs")
      plot(new_data_19$number_of_reviews,new_data_19$log_price)
      abline(lm(new_data_19$log_price~new_data_19$number_of_reviews),col="red")
      
      cor(new_data_20$number_of_reviews,new_data_20$log_price, use="complete.obs")
      plot(new_data_20$number_of_reviews,new_data_20$log_price)
      abline(lm(new_data_20$log_price~new_data_20$number_of_reviews),col="red")
    }
    
    #Location-Instant Bookable
    {
      data_list <- list(new_data_1, new_data_2, new_data_3, new_data_4, new_data_5, new_data_6, new_data_7, new_data_8, new_data_9, new_data_10)
      results <- list()
      for (i in seq_along(data_list)) {
        results[[i]] <- tapply(data_list[[i]]$log_price, data_list[[i]]$instant_bookable, mean)
      }
      results
    }
    
    #Location-Cleaning Fee
    {
      data_list <- list(new_data_1, new_data_2, new_data_3, new_data_4, new_data_5, new_data_6, new_data_7, new_data_8, new_data_9, new_data_10)
      results <- list()
      for (i in seq_along(data_list)) {
        results[[i]] <- tapply(data_list[[i]]$log_price, data_list[[i]]$cleaning_fee, mean)
      }
      results
    }
  }
  
  #LA
  {
    city=LA
    x=6
    
    #General code
    {
      #I create the data frame with the variables I'm interested in
      #I cannot focus on all the neighbourhoods, thus I take the ones with at least x properties
      {
        my_table <- table(city$neighbourhood); my_table
        my_subset <- names(which(my_table > x)); my_subset
        my_df <- subset(city, neighbourhood %in% my_subset, select = c(neighbourhood, log_price, host_identity_verified, host_since, host_response_rate, first_review, review_scores_rating,number_of_reviews,cancellation_policy,instant_bookable,cleaning_fee, property_type, room_type, accommodates, beds, bedrooms, bathrooms))
        my_df
      }
      
      #Understand which are the top 3 neighbourhoods, and the last 3 neighbourhoods
      {
        mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
        mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]; mean_prices
        colnames(mean_prices)[1]= "Neighbourhood"
        colnames(mean_prices)[2]= "log_price"
        mean_prices
      }
      
      #Call the top 10 neighbourhoods (1-10) and the last 10 neighbourhoods (11-20) as in some cases we want to deepen our analysis
      {
        highest_neighbourhood <- mean_prices[1, "Neighbourhood"]
        new_data_1<- subset(my_df, neighbourhood == highest_neighbourhood)
        second_neighbourhood <- mean_prices[2, "Neighbourhood"]
        new_data_2 <- subset(my_df, neighbourhood == second_neighbourhood)
        third_neighbourhood <- mean_prices[3, "Neighbourhood"]
        new_data_3 <- subset(my_df, neighbourhood == third_neighbourhood)
        fourth_neighbourhood <- mean_prices[4, "Neighbourhood"]
        new_data_4 <- subset(my_df, neighbourhood == fourth_neighbourhood)
        fifth_neighbourhood <- mean_prices[5, "Neighbourhood"]
        new_data_5 <- subset(my_df, neighbourhood == fifth_neighbourhood)
        sixth_neighbourhood <- mean_prices[6, "Neighbourhood"]
        new_data_6 <- subset(my_df, neighbourhood == sixth_neighbourhood)
        seventh_neighbourhood <- mean_prices[7, "Neighbourhood"]
        new_data_7 <- subset(my_df, neighbourhood == seventh_neighbourhood)
        eigth_neighbourhood <- mean_prices[8, "Neighbourhood"]
        new_data_8 <- subset(my_df, neighbourhood == eigth_neighbourhood)
        ninth_neighbourhood <- mean_prices[9, "Neighbourhood"]
        new_data_9 <- subset(my_df, neighbourhood == ninth_neighbourhood)
        tenth_neighbourhood <- mean_prices[10, "Neighbourhood"]
        new_data_10 <- subset(my_df, neighbourhood == tenth_neighbourhood)
        
        last_tenth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-9, "Neighbourhood"]
        new_data_11 <- subset(my_df, neighbourhood == last_tenth_neighbourhood)
        last_ninth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-8, "Neighbourhood"]
        new_data_12 <- subset(my_df, neighbourhood == last_ninth_neighbourhood)
        last_eigth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-7, "Neighbourhood"]
        new_data_13 <- subset(my_df, neighbourhood == last_eigth_neighbourhood)
        last_seventh_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-6, "Neighbourhood"]
        new_data_14 <- subset(my_df, neighbourhood == last_seventh_neighbourhood)
        last_sixth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-5, "Neighbourhood"]
        new_data_15 <- subset(my_df, neighbourhood == last_sixth_neighbourhood)
        last_fifth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-4, "Neighbourhood"]
        new_data_16 <- subset(my_df, neighbourhood == last_fifth_neighbourhood)
        last_fourth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-3, "Neighbourhood"]
        new_data_17 <- subset(my_df, neighbourhood == last_fourth_neighbourhood)
        last_third_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-2, "Neighbourhood"]
        new_data_18 <- subset(my_df, neighbourhood == last_third_neighbourhood)
        last_second_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-1, "Neighbourhood"]
        new_data_19 <- subset(my_df, neighbourhood == last_second_neighbourhood)
        last_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood), "Neighbourhood"]
        new_data_20 <- subset(my_df, neighbourhood == last_neighbourhood)
      }
    }
    
    #Location-Cancellation Policy
    {
      data_list <- list(new_data_1, new_data_2, new_data_3, new_data_4, new_data_5, new_data_6, new_data_7, new_data_8, new_data_9, new_data_10)
      results <- list()
      for (i in seq_along(data_list)) {
        results[[i]] <- tapply(data_list[[i]]$log_price, data_list[[i]]$cancellation_policy, mean)
      }
      results
    }
    
  }
  
  #NYC
  {
    city=NYC
    x=5
    #General code
    {
      #I create the data frame with the variables I'm interested in
      #I cannot focus on all the neighbourhoods, thus I take the ones with at least x properties
      {
        my_table <- table(city$neighbourhood); my_table
        my_subset <- names(which(my_table > x)); my_subset
        my_df <- subset(city, neighbourhood %in% my_subset, select = c(neighbourhood, log_price, host_identity_verified, host_since, host_response_rate, first_review, review_scores_rating,number_of_reviews,cancellation_policy,instant_bookable,cleaning_fee, property_type, room_type, accommodates, beds, bedrooms, bathrooms))
        my_df
      }
      
      #Understand which are the top 3 neighbourhoods, and the last 3 neighbourhoods
      {
        mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
        mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]; mean_prices
        colnames(mean_prices)[1]= "Neighbourhood"
        colnames(mean_prices)[2]= "log_price"
        mean_prices
      }
      
      #Call the top 10 neighbourhoods (1-10) and the last 10 neighbourhoods (11-20) as in some cases we want to deepen our analysis
      {
        highest_neighbourhood <- mean_prices[1, "Neighbourhood"]
        new_data_1<- subset(my_df, neighbourhood == highest_neighbourhood)
        second_neighbourhood <- mean_prices[2, "Neighbourhood"]
        new_data_2 <- subset(my_df, neighbourhood == second_neighbourhood)
        third_neighbourhood <- mean_prices[3, "Neighbourhood"]
        new_data_3 <- subset(my_df, neighbourhood == third_neighbourhood)
        fourth_neighbourhood <- mean_prices[4, "Neighbourhood"]
        new_data_4 <- subset(my_df, neighbourhood == fourth_neighbourhood)
        fifth_neighbourhood <- mean_prices[5, "Neighbourhood"]
        new_data_5 <- subset(my_df, neighbourhood == fifth_neighbourhood)
        sixth_neighbourhood <- mean_prices[6, "Neighbourhood"]
        new_data_6 <- subset(my_df, neighbourhood == sixth_neighbourhood)
        seventh_neighbourhood <- mean_prices[7, "Neighbourhood"]
        new_data_7 <- subset(my_df, neighbourhood == seventh_neighbourhood)
        eigth_neighbourhood <- mean_prices[8, "Neighbourhood"]
        new_data_8 <- subset(my_df, neighbourhood == eigth_neighbourhood)
        ninth_neighbourhood <- mean_prices[9, "Neighbourhood"]
        new_data_9 <- subset(my_df, neighbourhood == ninth_neighbourhood)
        tenth_neighbourhood <- mean_prices[10, "Neighbourhood"]
        new_data_10 <- subset(my_df, neighbourhood == tenth_neighbourhood)
        
        last_tenth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-9, "Neighbourhood"]
        new_data_11 <- subset(my_df, neighbourhood == last_tenth_neighbourhood)
        last_ninth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-8, "Neighbourhood"]
        new_data_12 <- subset(my_df, neighbourhood == last_ninth_neighbourhood)
        last_eigth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-7, "Neighbourhood"]
        new_data_13 <- subset(my_df, neighbourhood == last_eigth_neighbourhood)
        last_seventh_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-6, "Neighbourhood"]
        new_data_14 <- subset(my_df, neighbourhood == last_seventh_neighbourhood)
        last_sixth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-5, "Neighbourhood"]
        new_data_15 <- subset(my_df, neighbourhood == last_sixth_neighbourhood)
        last_fifth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-4, "Neighbourhood"]
        new_data_16 <- subset(my_df, neighbourhood == last_fifth_neighbourhood)
        last_fourth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-3, "Neighbourhood"]
        new_data_17 <- subset(my_df, neighbourhood == last_fourth_neighbourhood)
        last_third_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-2, "Neighbourhood"]
        new_data_18 <- subset(my_df, neighbourhood == last_third_neighbourhood)
        last_second_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-1, "Neighbourhood"]
        new_data_19 <- subset(my_df, neighbourhood == last_second_neighbourhood)
        last_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood), "Neighbourhood"]
        new_data_20 <- subset(my_df, neighbourhood == last_neighbourhood)
      }
    }
    
    #Location and identity verified
    {
      addmargins(prop.table(table(new_data_1$log_price,new_data_1$host_identity_verified)))
      boxplot(new_data_1$log_price~new_data_1$host_identity_verified)
      addmargins(prop.table(table(new_data_2$log_price,new_data_2$host_identity_verified)))
      boxplot(new_data_2$log_price~new_data_2$host_identity_verified)
      addmargins(prop.table(table(new_data_3$log_price,new_data_3$host_identity_verified)))
      boxplot(new_data_3$log_price~new_data_3$host_identity_verified)
      addmargins(prop.table(table(new_data_4$log_price,new_data_4$host_identity_verified)))
      boxplot(new_data_4$log_price~new_data_4$host_identity_verified)
      addmargins(prop.table(table(new_data_5$log_price,new_data_5$host_identity_verified)))
      boxplot(new_data_5$log_price~new_data_5$host_identity_verified)
      addmargins(prop.table(table(new_data_6$log_price,new_data_6$host_identity_verified)))
      boxplot(new_data_6$log_price~new_data_6$host_identity_verified)
      addmargins(prop.table(table(new_data_7$log_price,new_data_7$host_identity_verified)))
      boxplot(new_data_7$log_price~new_data_7$host_identity_verified)
      addmargins(prop.table(table(new_data_8$log_price,new_data_8$host_identity_verified)))
      boxplot(new_data_8$log_price~new_data_8$host_identity_verified)
      addmargins(prop.table(table(new_data_9$log_price,new_data_9$host_identity_verified)))
      boxplot(new_data_9$log_price~new_data_9$host_identity_verified)
      addmargins(prop.table(table(new_data_10$log_price,new_data_10$host_identity_verified)))
      boxplot(new_data_10$log_price~new_data_10$host_identity_verified)
      
      
      addmargins(prop.table(table(new_data_18$log_price,new_data_18$host_identity_verified)))
      boxplot(new_data_18$log_price~new_data_18$host_identity_verified)
      addmargins(prop.table(table(new_data_19$log_price,new_data_19$host_identity_verified)))
      boxplot(new_data_19$log_price~new_data_19$host_identity_verified)
      addmargins(prop.table(table(new_data_20$log_price,new_data_20$host_identity_verified)))
      boxplot(new_data_20$log_price~new_data_20$host_identity_verified)
    }
    
    #Location and host_since
    {
      cor(as.numeric(new_data_1$host_since),new_data_1$log_price, use="complete.obs")
      plot(new_data_1$host_since,new_data_1$log_price)
      abline(lm(new_data_1$log_price~new_data_1$host_since),col="red")
      
      cor(as.numeric(new_data_2$host_since),new_data_2$log_price, use="complete.obs")
      plot(new_data_2$host_since,new_data_2$log_price)
      abline(lm(new_data_2$log_price~new_data_2$host_since),col="red")
      
      cor(as.numeric(new_data_3$host_since),new_data_3$log_price, use="complete.obs")
      plot(new_data_3$host_since,new_data_3$log_price)
      abline(lm(new_data_3$log_price~new_data_3$host_since),col="red")
      
      cor(as.numeric(new_data_18$host_since),new_data_18$log_price, use="complete.obs")
      plot(new_data_18$host_since,new_data_18$log_price)
      abline(lm(new_data_18$log_price~new_data_18$host_since),col="red")
      
      cor(as.numeric(new_data_19$host_since),new_data_19$log_price, use="complete.obs")
      plot(new_data_19$host_since,new_data_19$log_price)
      abline(lm(new_data_19$log_price~new_data_19$host_since),col="red")
      
      cor(as.numeric(new_data_20$host_since),new_data_20$log_price, use="complete.obs")
      plot(new_data_20$host_since,new_data_20$log_price)
      abline(lm(new_data_20$log_price~new_data_20$host_since),col="red")
    }
    
    #Location and host_response
    {
      boxplot(new_data_1$log_price~new_data_1$host_response_rate)
      boxplot(new_data_2$log_price~new_data_2$host_response_rate)
      boxplot(new_data_3$log_price~new_data_3$host_response_rate)
      boxplot(new_data_18$log_price~new_data_18$host_response_rate)
      boxplot(new_data_19$log_price~new_data_19$host_response_rate)
      boxplot(new_data_20$log_price~new_data_20$host_response_rate)
      
    }
    
    #Location and first_review
    {
      cor(as.numeric(new_data_1$first_review),new_data_1$log_price, use="complete.obs")
      plot(new_data_1$first_review,new_data_1$log_price)
      abline(lm(new_data_1$log_price~new_data_1$first_review),col="red")
      
      cor(as.numeric(new_data_2$first_review),new_data_2$log_price, use="complete.obs")
      plot(new_data_2$first_review,new_data_2$log_price)
      abline(lm(new_data_2$log_price~new_data_2$first_review),col="red")
      
      cor(as.numeric(new_data_3$first_review),new_data_3$log_price, use="complete.obs")
      plot(new_data_3$first_review,new_data_3$log_price)
      abline(lm(new_data_3$log_price~new_data_3$first_review),col="red")
      
      cor(as.numeric(new_data_18$first_review),new_data_18$log_price, use="complete.obs")
      plot(new_data_18$first_review,new_data_18$log_price)
      abline(lm(new_data_18$log_price~new_data_18$first_review),col="red")
      
      cor(as.numeric(new_data_19$first_review),new_data_19$log_price, use="complete.obs")
      plot(new_data_19$first_review,new_data_19$log_price)
      abline(lm(new_data_19$log_price~new_data_19$first_review),col="red")
      
      cor(as.numeric(new_data_20$first_review),new_data_20$log_price, use="complete.obs")
      plot(new_data_20$first_review,new_data_20$log_price)
      abline(lm(new_data_20$log_price~new_data_20$first_review),col="red")
    }
    
    #Location and review score rating
    {
      data_list <- list(new_data_1, new_data_2, new_data_3, new_data_4, new_data_5, new_data_6, new_data_7, new_data_8, new_data_9, new_data_10, new_data_11, new_data_12, new_data_13, new_data_14, new_data_15, new_data_16, new_data_17, new_data_18, new_data_19, new_data_20)
      results <- list()
      for (i in seq_along(data_list)) {
        results[[i]] <- tapply(data_list[[i]]$log_price, data_list[[i]]$number_of_reviews, mean)
      }
      results
      
      
      boxplot(new_data_1$log_price~new_data_1$review_scores_rating)
      boxplot(new_data_2$log_price~new_data_2$review_scores_rating)
      boxplot(new_data_3$log_price~new_data_3$review_scores_rating)
      boxplot(new_data_18$log_price~new_data_18$review_scores_rating)
      boxplot(new_data_19$log_price~new_data_19$review_scores_rating)
      boxplot(new_data_20$log_price~new_data_20$review_scores_rating)
    }
    
    #Location and number of reviews
    {
      cor(new_data_1$number_of_reviews,new_data_1$log_price, use="complete.obs")
      plot(new_data_1$number_of_reviews,new_data_1$log_price)
      abline(lm(new_data_1$log_price~new_data_1$number_of_reviews),col="red")
      
      cor(new_data_2$number_of_reviews,new_data_2$log_price, use="complete.obs")
      plot(new_data_2$number_of_reviews,new_data_2$log_price)
      abline(lm(new_data_2$log_price~new_data_2$number_of_reviews),col="red")
      
      cor(new_data_3$number_of_reviews,new_data_3$log_price, use="complete.obs")
      plot(new_data_3$number_of_reviews,new_data_3$log_price)
      abline(lm(new_data_3$log_price~new_data_3$number_of_reviews),col="red")
      
      cor(new_data_18$number_of_reviews,new_data_18$log_price, use="complete.obs")
      plot(new_data_18$number_of_reviews,new_data_18$log_price)
      abline(lm(new_data_18$log_price~new_data_18$number_of_reviews),col="red")
      
      cor(new_data_19$number_of_reviews,new_data_19$log_price, use="complete.obs")
      plot(new_data_19$number_of_reviews,new_data_19$log_price)
      abline(lm(new_data_19$log_price~new_data_19$number_of_reviews),col="red")
      
      cor(new_data_20$number_of_reviews,new_data_20$log_price, use="complete.obs")
      plot(new_data_20$number_of_reviews,new_data_20$log_price)
      abline(lm(new_data_20$log_price~new_data_20$number_of_reviews),col="red")
    }
    
    #Location and cancellation policy
    {
      boxplot(new_data_1$log_price~new_data_1$cancellation_policy)
      boxplot(new_data_2$log_price~new_data_2$cancellation_policy)
      boxplot(new_data_3$log_price~new_data_3$cancellation_policy)
      boxplot(new_data_4$log_price~new_data_4$cancellation_policy)
      boxplot(new_data_5$log_price~new_data_5$cancellation_policy)
      boxplot(new_data_6$log_price~new_data_6$cancellation_policy)
      boxplot(new_data_7$log_price~new_data_7$cancellation_policy)
      boxplot(new_data_8$log_price~new_data_8$cancellation_policy)
      boxplot(new_data_9$log_price~new_data_9$cancellation_policy)
      boxplot(new_data_10$log_price~new_data_10$cancellation_policy)
      boxplot(new_data_11$log_price~new_data_11$cancellation_policy)
      boxplot(new_data_12$log_price~new_data_12$cancellation_policy)
      boxplot(new_data_13$log_price~new_data_13$cancellation_policy)
      boxplot(new_data_14$log_price~new_data_14$cancellation_policy)
      boxplot(new_data_15$log_price~new_data_15$cancellation_policy)
      boxplot(new_data_16$log_price~new_data_16$cancellation_policy)
      boxplot(new_data_17$log_price~new_data_17$cancellation_policy)
      boxplot(new_data_18$log_price~new_data_18$cancellation_policy)
      boxplot(new_data_19$log_price~new_data_19$cancellation_policy)
      boxplot(new_data_20$log_price~new_data_20$cancellation_policy)
    }
    
    #Location and instant bookable
    {
      boxplot(new_data_1$log_price~new_data_1$instant_bookable)
      boxplot(new_data_2$log_price~new_data_2$instant_bookable)
      boxplot(new_data_3$log_price~new_data_3$instant_bookable)
      boxplot(new_data_4$log_price~new_data_4$instant_bookable)
      boxplot(new_data_5$log_price~new_data_5$instant_bookable)
      boxplot(new_data_6$log_price~new_data_6$instant_bookable)
      boxplot(new_data_7$log_price~new_data_7$instant_bookable)
      boxplot(new_data_8$log_price~new_data_8$instant_bookable)
      boxplot(new_data_9$log_price~new_data_9$instant_bookable)
      boxplot(new_data_10$log_price~new_data_10$instant_bookable)
      boxplot(new_data_11$log_price~new_data_11$instant_bookable)
      boxplot(new_data_12$log_price~new_data_12$instant_bookable)
      boxplot(new_data_13$log_price~new_data_13$instant_bookable)
      boxplot(new_data_14$log_price~new_data_14$instant_bookable)
      boxplot(new_data_15$log_price~new_data_15$instant_bookable)
      boxplot(new_data_16$log_price~new_data_16$instant_bookable)
      boxplot(new_data_17$log_price~new_data_17$instant_bookable)
      boxplot(new_data_18$log_price~new_data_18$instant_bookable)
      boxplot(new_data_19$log_price~new_data_19$instant_bookable)
      boxplot(new_data_20$log_price~new_data_20$instant_bookable)
    }
    
    #Location and cleaning fee
    {
      boxplot(new_data_1$log_price~new_data_1$cleaning_fee)
      boxplot(new_data_2$log_price~new_data_2$cleaning_fee)
      boxplot(new_data_3$log_price~new_data_3$cleaning_fee)
      boxplot(new_data_4$log_price~new_data_4$cleaning_fee)
      boxplot(new_data_5$log_price~new_data_5$cleaning_fee)
      boxplot(new_data_6$log_price~new_data_6$cleaning_fee)
      boxplot(new_data_7$log_price~new_data_7$cleaning_fee)
      boxplot(new_data_8$log_price~new_data_8$cleaning_fee)
      boxplot(new_data_9$log_price~new_data_9$cleaning_fee)
      boxplot(new_data_10$log_price~new_data_10$cleaning_fee)
      boxplot(new_data_11$log_price~new_data_11$cleaning_fee)
      boxplot(new_data_12$log_price~new_data_12$cleaning_fee)
      boxplot(new_data_13$log_price~new_data_13$cleaning_fee)
      boxplot(new_data_14$log_price~new_data_14$cleaning_fee)
      boxplot(new_data_15$log_price~new_data_15$cleaning_fee)
      boxplot(new_data_16$log_price~new_data_16$cleaning_fee)
      boxplot(new_data_17$log_price~new_data_17$cleaning_fee)
      boxplot(new_data_18$log_price~new_data_18$cleaning_fee)
      boxplot(new_data_19$log_price~new_data_19$cleaning_fee)
      boxplot(new_data_20$log_price~new_data_20$cleaning_fee)
    }
  }
  
  #SF
  {
    city=SF
    x=10
    
    #General code
    {
      #I create the data frame with the variables I'm interested in
      #I cannot focus on all the neighbourhoods, thus I take the ones with at least x properties
      {
        my_table <- table(city$neighbourhood); my_table
        my_subset <- names(which(my_table > x)); my_subset
        my_df <- subset(city, neighbourhood %in% my_subset, select = c(neighbourhood, log_price, host_identity_verified, host_since, host_response_rate, first_review, review_scores_rating,number_of_reviews,cancellation_policy,instant_bookable,cleaning_fee, property_type, room_type, accommodates, beds, bedrooms, bathrooms))
        my_df
      }
      
      #Understand which are the top 3 neighbourhoods, and the last 3 neighbourhoods
      {
        mean_prices <- aggregate(my_df$log_price, by = list(my_df$neighbourhood), mean)
        mean_prices <- mean_prices[order(mean_prices$x, decreasing = TRUE), ]; mean_prices
        colnames(mean_prices)[1]= "Neighbourhood"
        colnames(mean_prices)[2]= "log_price"
        mean_prices
      }
      
      #Call the top 10 neighbourhoods (1-10) and the last 10 neighbourhoods (11-20) as in some cases we want to deepen our analysis
      {
        highest_neighbourhood <- mean_prices[1, "Neighbourhood"]
        new_data_1<- subset(my_df, neighbourhood == highest_neighbourhood)
        second_neighbourhood <- mean_prices[2, "Neighbourhood"]
        new_data_2 <- subset(my_df, neighbourhood == second_neighbourhood)
        third_neighbourhood <- mean_prices[3, "Neighbourhood"]
        new_data_3 <- subset(my_df, neighbourhood == third_neighbourhood)
        fourth_neighbourhood <- mean_prices[4, "Neighbourhood"]
        new_data_4 <- subset(my_df, neighbourhood == fourth_neighbourhood)
        fifth_neighbourhood <- mean_prices[5, "Neighbourhood"]
        new_data_5 <- subset(my_df, neighbourhood == fifth_neighbourhood)
        sixth_neighbourhood <- mean_prices[6, "Neighbourhood"]
        new_data_6 <- subset(my_df, neighbourhood == sixth_neighbourhood)
        seventh_neighbourhood <- mean_prices[7, "Neighbourhood"]
        new_data_7 <- subset(my_df, neighbourhood == seventh_neighbourhood)
        eigth_neighbourhood <- mean_prices[8, "Neighbourhood"]
        new_data_8 <- subset(my_df, neighbourhood == eigth_neighbourhood)
        ninth_neighbourhood <- mean_prices[9, "Neighbourhood"]
        new_data_9 <- subset(my_df, neighbourhood == ninth_neighbourhood)
        tenth_neighbourhood <- mean_prices[10, "Neighbourhood"]
        new_data_10 <- subset(my_df, neighbourhood == tenth_neighbourhood)
        
        last_tenth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-9, "Neighbourhood"]
        new_data_11 <- subset(my_df, neighbourhood == last_tenth_neighbourhood)
        last_ninth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-8, "Neighbourhood"]
        new_data_12 <- subset(my_df, neighbourhood == last_ninth_neighbourhood)
        last_eigth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-7, "Neighbourhood"]
        new_data_13 <- subset(my_df, neighbourhood == last_eigth_neighbourhood)
        last_seventh_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-6, "Neighbourhood"]
        new_data_14 <- subset(my_df, neighbourhood == last_seventh_neighbourhood)
        last_sixth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-5, "Neighbourhood"]
        new_data_15 <- subset(my_df, neighbourhood == last_sixth_neighbourhood)
        last_fifth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-4, "Neighbourhood"]
        new_data_16 <- subset(my_df, neighbourhood == last_fifth_neighbourhood)
        last_fourth_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-3, "Neighbourhood"]
        new_data_17 <- subset(my_df, neighbourhood == last_fourth_neighbourhood)
        last_third_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-2, "Neighbourhood"]
        new_data_18 <- subset(my_df, neighbourhood == last_third_neighbourhood)
        last_second_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood)-1, "Neighbourhood"]
        new_data_19 <- subset(my_df, neighbourhood == last_second_neighbourhood)
        last_neighbourhood <- mean_prices[length(mean_prices$Neighbourhood), "Neighbourhood"]
        new_data_20 <- subset(my_df, neighbourhood == last_neighbourhood)
      }
    }
    
    #Location and identity verified
    {
      addmargins(prop.table(table(new_data_1$log_price,new_data_1$host_identity_verified)))
      boxplot(new_data_1$log_price~new_data_1$host_identity_verified)
      addmargins(prop.table(table(new_data_2$log_price,new_data_2$host_identity_verified)))
      boxplot(new_data_2$log_price~new_data_2$host_identity_verified)
      addmargins(prop.table(table(new_data_3$log_price,new_data_3$host_identity_verified)))
      boxplot(new_data_3$log_price~new_data_3$host_identity_verified)
      
      addmargins(prop.table(table(new_data_18$log_price,new_data_18$host_identity_verified)))
      boxplot(new_data_18$log_price~new_data_18$host_identity_verified)
      addmargins(prop.table(table(new_data_19$log_price,new_data_19$host_identity_verified)))
      boxplot(new_data_19$log_price~new_data_19$host_identity_verified)
      addmargins(prop.table(table(new_data_20$log_price,new_data_20$host_identity_verified)))
      boxplot(new_data_20$log_price~new_data_20$host_identity_verified)
    }
    
    #Location and host_since
    {
      cor(as.numeric(new_data_1$host_since),new_data_1$log_price, use="complete.obs")
      plot(new_data_1$host_since,new_data_1$log_price)
      abline(lm(new_data_1$log_price~new_data_1$host_since),col="red")
      
      cor(as.numeric(new_data_2$host_since),new_data_2$log_price, use="complete.obs")
      plot(new_data_2$host_since,new_data_2$log_price)
      abline(lm(new_data_2$log_price~new_data_2$host_since),col="red")
      
      cor(as.numeric(new_data_3$host_since),new_data_3$log_price, use="complete.obs")
      plot(new_data_3$host_since,new_data_3$log_price)
      abline(lm(new_data_3$log_price~new_data_3$host_since),col="red")
      
      cor(as.numeric(new_data_4$host_since),new_data_4$log_price, use="complete.obs")
      plot(new_data_4$host_since,new_data_4$log_price)
      abline(lm(new_data_4$log_price~new_data_4$host_since),col="red")
      
      cor(as.numeric(new_data_5$host_since),new_data_5$log_price, use="complete.obs")
      plot(new_data_5$host_since,new_data_5$log_price)
      abline(lm(new_data_5$log_price~new_data_5$host_since),col="red")
      
      cor(as.numeric(new_data_6$host_since),new_data_6$log_price, use="complete.obs")
      plot(new_data_6$host_since,new_data_6$log_price)
      abline(lm(new_data_6$log_price~new_data_6$host_since),col="red")
      
      cor(as.numeric(new_data_7$host_since),new_data_7$log_price, use="complete.obs")
      plot(new_data_7$host_since,new_data_7$log_price)
      abline(lm(new_data_7$log_price~new_data_7$host_since),col="red")
      
      cor(as.numeric(new_data_8$host_since),new_data_8$log_price, use="complete.obs")
      plot(new_data_8$host_since,new_data_8$log_price)
      abline(lm(new_data_8$log_price~new_data_8$host_since),col="red")
      
      cor(as.numeric(new_data_9$host_since),new_data_9$log_price, use="complete.obs")
      plot(new_data_9$host_since,new_data_9$log_price)
      abline(lm(new_data_9$log_price~new_data_9$host_since),col="red")
      
      cor(as.numeric(new_data_10$host_since),new_data_10$log_price, use="complete.obs")
      plot(new_data_10$host_since,new_data_10$log_price)
      abline(lm(new_data_10$log_price~new_data_10$host_since),col="red")
      
      cor(as.numeric(new_data_18$host_since),new_data_18$log_price, use="complete.obs")
      plot(new_data_18$host_since,new_data_18$log_price)
      abline(lm(new_data_18$log_price~new_data_18$host_since),col="red")
      
      cor(as.numeric(new_data_19$host_since),new_data_19$log_price, use="complete.obs")
      plot(new_data_19$host_since,new_data_19$log_price)
      abline(lm(new_data_19$log_price~new_data_19$host_since),col="red")
      
      cor(as.numeric(new_data_20$host_since),new_data_20$log_price, use="complete.obs")
      plot(new_data_20$host_since,new_data_20$log_price)
      abline(lm(new_data_20$log_price~new_data_20$host_since),col="red")
    }
    
    #Location and host_response
    {
      boxplot(new_data_1$log_price~new_data_1$host_response_rate)
      boxplot(new_data_2$log_price~new_data_2$host_response_rate)
      boxplot(new_data_3$log_price~new_data_3$host_response_rate)
      boxplot(new_data_18$log_price~new_data_18$host_response_rate)
      boxplot(new_data_19$log_price~new_data_19$host_response_rate)
      boxplot(new_data_20$log_price~new_data_20$host_response_rate)
      
    }
    
    #Location and first_review
    {
      cor(as.numeric(new_data_1$first_review),new_data_1$log_price, use="complete.obs")
      plot(new_data_1$first_review,new_data_1$log_price)
      abline(lm(new_data_1$log_price~new_data_1$first_review),col="red")
      
      cor(as.numeric(new_data_2$first_review),new_data_2$log_price, use="complete.obs")
      plot(new_data_2$first_review,new_data_2$log_price)
      abline(lm(new_data_2$log_price~new_data_2$first_review),col="red")
      
      cor(as.numeric(new_data_3$first_review),new_data_3$log_price, use="complete.obs")
      plot(new_data_3$first_review,new_data_3$log_price)
      abline(lm(new_data_3$log_price~new_data_3$first_review),col="red")
      
      cor(as.numeric(new_data_18$first_review),new_data_18$log_price, use="complete.obs")
      plot(new_data_18$first_review,new_data_18$log_price)
      abline(lm(new_data_18$log_price~new_data_18$first_review),col="red")
      
      cor(as.numeric(new_data_19$first_review),new_data_19$log_price, use="complete.obs")
      plot(new_data_19$first_review,new_data_19$log_price)
      abline(lm(new_data_19$log_price~new_data_19$first_review),col="red")
      
      cor(as.numeric(new_data_20$first_review),new_data_20$log_price, use="complete.obs")
      plot(new_data_20$first_review,new_data_20$log_price)
      abline(lm(new_data_20$log_price~new_data_20$first_review),col="red")
    }
    
    #Location and review score rating
    {
      boxplot(new_data_1$log_price~new_data_1$review_scores_rating)
      boxplot(new_data_2$log_price~new_data_2$review_scores_rating)
      boxplot(new_data_3$log_price~new_data_3$review_scores_rating)
      boxplot(new_data_18$log_price~new_data_18$review_scores_rating)
      boxplot(new_data_19$log_price~new_data_19$review_scores_rating)
      boxplot(new_data_20$log_price~new_data_20$review_scores_rating)
    }
    
    #Location and number of reviews
    {
      cor(new_data_1$number_of_reviews,new_data_1$log_price, use="complete.obs")
      plot(new_data_1$number_of_reviews,new_data_1$log_price)
      abline(lm(new_data_1$log_price~new_data_1$number_of_reviews),col="red")
      
      cor(new_data_2$number_of_reviews,new_data_2$log_price, use="complete.obs")
      plot(new_data_2$number_of_reviews,new_data_2$log_price)
      abline(lm(new_data_2$log_price~new_data_2$number_of_reviews),col="red")
      
      cor(new_data_3$number_of_reviews,new_data_3$log_price, use="complete.obs")
      plot(new_data_3$number_of_reviews,new_data_3$log_price)
      abline(lm(new_data_3$log_price~new_data_3$number_of_reviews),col="red")
      
      cor(new_data_18$number_of_reviews,new_data_18$log_price, use="complete.obs")
      plot(new_data_18$number_of_reviews,new_data_18$log_price)
      abline(lm(new_data_18$log_price~new_data_18$number_of_reviews),col="red")
      
      cor(new_data_19$number_of_reviews,new_data_19$log_price, use="complete.obs")
      plot(new_data_19$number_of_reviews,new_data_19$log_price)
      abline(lm(new_data_19$log_price~new_data_19$number_of_reviews),col="red")
      
      cor(new_data_20$number_of_reviews,new_data_20$log_price, use="complete.obs")
      plot(new_data_20$number_of_reviews,new_data_20$log_price)
      abline(lm(new_data_20$log_price~new_data_20$number_of_reviews),col="red")
    }
    
    #Location and cancellation policy
    {
      boxplot(new_data_1$log_price~new_data_1$cancellation_policy)
      boxplot(new_data_2$log_price~new_data_2$cancellation_policy)
      boxplot(new_data_3$log_price~new_data_3$cancellation_policy)
      boxplot(new_data_4$log_price~new_data_4$cancellation_policy)
      boxplot(new_data_5$log_price~new_data_5$cancellation_policy)
      boxplot(new_data_6$log_price~new_data_6$cancellation_policy)
      boxplot(new_data_7$log_price~new_data_7$cancellation_policy)
      boxplot(new_data_8$log_price~new_data_8$cancellation_policy)
      boxplot(new_data_9$log_price~new_data_9$cancellation_policy)
      boxplot(new_data_10$log_price~new_data_10$cancellation_policy)
      boxplot(new_data_11$log_price~new_data_11$cancellation_policy)
      boxplot(new_data_12$log_price~new_data_12$cancellation_policy)
      boxplot(new_data_13$log_price~new_data_13$cancellation_policy)
      boxplot(new_data_14$log_price~new_data_14$cancellation_policy)
      boxplot(new_data_15$log_price~new_data_15$cancellation_policy)
      boxplot(new_data_16$log_price~new_data_16$cancellation_policy)
      boxplot(new_data_17$log_price~new_data_17$cancellation_policy)
      boxplot(new_data_18$log_price~new_data_18$cancellation_policy)
      boxplot(new_data_19$log_price~new_data_19$cancellation_policy)
      boxplot(new_data_20$log_price~new_data_20$cancellation_policy)
    }
    
    #Location and instant bookable
    {
      boxplot(new_data_1$log_price~new_data_1$instant_bookable)
      boxplot(new_data_2$log_price~new_data_2$instant_bookable)
      boxplot(new_data_3$log_price~new_data_3$instant_bookable)
      boxplot(new_data_4$log_price~new_data_4$instant_bookable)
      boxplot(new_data_5$log_price~new_data_5$instant_bookable)
      boxplot(new_data_6$log_price~new_data_6$instant_bookable)
      boxplot(new_data_7$log_price~new_data_7$instant_bookable)
      boxplot(new_data_8$log_price~new_data_8$instant_bookable)
      boxplot(new_data_9$log_price~new_data_9$instant_bookable)
      boxplot(new_data_10$log_price~new_data_10$instant_bookable)
      boxplot(new_data_11$log_price~new_data_11$instant_bookable)
      boxplot(new_data_12$log_price~new_data_12$instant_bookable)
      boxplot(new_data_13$log_price~new_data_13$instant_bookable)
      boxplot(new_data_14$log_price~new_data_14$instant_bookable)
      boxplot(new_data_15$log_price~new_data_15$instant_bookable)
      boxplot(new_data_16$log_price~new_data_16$instant_bookable)
      boxplot(new_data_17$log_price~new_data_17$instant_bookable)
      boxplot(new_data_18$log_price~new_data_18$instant_bookable)
      boxplot(new_data_19$log_price~new_data_19$instant_bookable)
      boxplot(new_data_20$log_price~new_data_20$instant_bookable)
    }
    
    #Location and cleaning fee
    {
      boxplot(new_data_1$log_price~new_data_1$cleaning_fee)
      boxplot(new_data_2$log_price~new_data_2$cleaning_fee)
      boxplot(new_data_3$log_price~new_data_3$cleaning_fee)
      boxplot(new_data_4$log_price~new_data_4$cleaning_fee)
      boxplot(new_data_5$log_price~new_data_5$cleaning_fee)
      boxplot(new_data_6$log_price~new_data_6$cleaning_fee)
      boxplot(new_data_7$log_price~new_data_7$cleaning_fee)
      boxplot(new_data_8$log_price~new_data_8$cleaning_fee)
      boxplot(new_data_9$log_price~new_data_9$cleaning_fee)
      boxplot(new_data_10$log_price~new_data_10$cleaning_fee)
      boxplot(new_data_11$log_price~new_data_11$cleaning_fee)
      boxplot(new_data_12$log_price~new_data_12$cleaning_fee)
      boxplot(new_data_13$log_price~new_data_13$cleaning_fee)
      boxplot(new_data_14$log_price~new_data_14$cleaning_fee)
      boxplot(new_data_15$log_price~new_data_15$cleaning_fee)
      boxplot(new_data_16$log_price~new_data_16$cleaning_fee)
      boxplot(new_data_17$log_price~new_data_17$cleaning_fee)
      boxplot(new_data_18$log_price~new_data_18$cleaning_fee)
      boxplot(new_data_19$log_price~new_data_19$cleaning_fee)
      boxplot(new_data_20$log_price~new_data_20$cleaning_fee)
    }
  }
}

#Deeper analysis for property type
{
  df_filtered <- Polished %>%
    filter(property_type %in% c("Timeshare", "Vacation home", "Dorm", "Hostel")) %>%
    select(city, neighbourhood, property_type, log_price)
  
  df_grouped <- df_filtered %>%
    group_by(city, neighbourhood, property_type) %>%
    summarise(mean_log_price = mean(log_price), count = n()) %>%
    pivot_wider(names_from = property_type, values_from = count)
  
  output <- df_grouped %>%
    group_by(city, neighbourhood) %>%
    summarise(mean_log_price = mean(mean_log_price),
              Timeshare = sum(Timeshare),
              `Vacation home` = sum(`Vacation home`),
              Dorm = sum(Dorm),
              Hostel = sum(Hostel)) %>%
    arrange(city, neighbourhood)
  
  print(output, n=59)
}

#Feedback analysis
{
  #Boston
  {
    city=Boston
    
    #Creation of the data frame
    {
      reviews_sum = tapply(city$number_of_reviews, city$neighbourhood, sum)
      reviews_df <- data.frame(neighbourhood = names(reviews_sum),reviews_sum=reviews_sum)
      avg_price <- aggregate(city$log_price, by = list(city$neighbourhood), mean)
      avg_price <- avg_price[order(avg_price$x, decreasing = TRUE), ]
      colnames(avg_price)[1] <- "neighbourhood"
      merged_df <- merge(avg_price, reviews_df, by = "neighbourhood")
      colnames(merged_df)[2] <- "log_price"
      
      obs_count <- aggregate(city$log_price, by=list(city$neighbourhood), FUN=length)
      names(obs_count) <- c("neighbourhood", "observations")
      merged_df <- merge(merged_df, obs_count, by = "neighbourhood")
      
      merged_df['rev/obs']=merged_df$reviews_sum/merged_df$observations
      
      score <- city[complete.cases(city$review_scores_rating),]
      scores = tapply(score$review_scores_rating, score$neighbourhood, mean)
      score_df <- data.frame(neighbourhood = names(scores), avg_score = scores)                         
      merged_df <- merge(merged_df, score_df, by = "neighbourhood")
      
      acc=tapply(city$accommodates,city$neighbourhood,mean)
      acc_df <- data.frame(neighbourhood = names(acc), accommodates = acc)                         
      merged_df <- merge(merged_df, acc_df, by = "neighbourhood")
      
      merged_df <- merged_df[order(merged_df$log_price, decreasing = TRUE), ]
      merged_df
    }
    
    
    #Relationship analysis
    {
      cor(merged_df$log_price,merged_df$avg_score)
      plot(merged_df$avg_score,merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$avg_score), col="red")
      
      cor(merged_df$log_price,merged_df$'rev/obs')
      plot(merged_df$'rev/obs',merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$'rev/obs'), col="red")
    }
  }
  
  #Chicago
  {
    city=Chicago
    
    #Creation of the data frame
    {
      reviews_sum = tapply(city$number_of_reviews, city$neighbourhood, sum)
      reviews_df <- data.frame(neighbourhood = names(reviews_sum),reviews_sum=reviews_sum)
      avg_price <- aggregate(city$log_price, by = list(city$neighbourhood), mean)
      avg_price <- avg_price[order(avg_price$x, decreasing = TRUE), ]
      colnames(avg_price)[1] <- "neighbourhood"
      merged_df <- merge(avg_price, reviews_df, by = "neighbourhood")
      colnames(merged_df)[2] <- "log_price"
      
      obs_count <- aggregate(city$log_price, by=list(city$neighbourhood), FUN=length)
      names(obs_count) <- c("neighbourhood", "observations")
      merged_df <- merge(merged_df, obs_count, by = "neighbourhood")
      
      merged_df['rev/obs']=merged_df$reviews_sum/merged_df$observations
      
      score <- city[complete.cases(city$review_scores_rating),]
      scores = tapply(score$review_scores_rating, score$neighbourhood, mean)
      score_df <- data.frame(neighbourhood = names(scores), avg_score = scores)                         
      merged_df <- merge(merged_df, score_df, by = "neighbourhood")
      
      acc=tapply(city$accommodates,city$neighbourhood,mean)
      acc_df <- data.frame(neighbourhood = names(acc), accommodates = acc)                         
      merged_df <- merge(merged_df, acc_df, by = "neighbourhood")
      
      merged_df <- merged_df[order(merged_df$log_price, decreasing = TRUE), ]
      merged_df
    }
    
    #Relationship analysis
    {
      cor(merged_df$log_price,merged_df$avg_score)
      plot(merged_df$avg_score,merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$avg_score), col="red")
      
      cor(merged_df$log_price,merged_df$'rev/obs')
      plot(merged_df$'rev/obs',merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$'rev/obs'), col="red")
    }
  }
  
  #DC
  {
    city=DC
    
    #Creation of the data frame
    {
      reviews_sum = tapply(city$number_of_reviews, city$neighbourhood, sum)
      reviews_df <- data.frame(neighbourhood = names(reviews_sum),reviews_sum=reviews_sum)
      avg_price <- aggregate(city$log_price, by = list(city$neighbourhood), mean)
      avg_price <- avg_price[order(avg_price$x, decreasing = TRUE), ]
      colnames(avg_price)[1] <- "neighbourhood"
      merged_df <- merge(avg_price, reviews_df, by = "neighbourhood")
      colnames(merged_df)[2] <- "log_price"
      
      obs_count <- aggregate(city$log_price, by=list(city$neighbourhood), FUN=length)
      names(obs_count) <- c("neighbourhood", "observations")
      merged_df <- merge(merged_df, obs_count, by = "neighbourhood")
      
      merged_df['rev/obs']=merged_df$reviews_sum/merged_df$observations
      
      score <- city[complete.cases(city$review_scores_rating),]
      scores = tapply(score$review_scores_rating, score$neighbourhood, mean)
      score_df <- data.frame(neighbourhood = names(scores), avg_score = scores)                         
      merged_df <- merge(merged_df, score_df, by = "neighbourhood")
      
      acc=tapply(city$accommodates,city$neighbourhood,mean)
      acc_df <- data.frame(neighbourhood = names(acc), accommodates = acc)                         
      merged_df <- merge(merged_df, acc_df, by = "neighbourhood")
      
      merged_df <- merged_df[order(merged_df$log_price, decreasing = TRUE), ]
      merged_df
    }
    
    #Relationship analysis
    {
      cor(merged_df$log_price,merged_df$avg_score)
      plot(merged_df$avg_score,merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$avg_score), col="red")
      
      cor(merged_df$log_price,merged_df$'rev/obs')
      plot(merged_df$'rev/obs',merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$'rev/obs'), col="red")
    }
  }
  
  #LA
  {
    city=LA
    
    #Creation of the data frame
    {
      reviews_sum = tapply(city$number_of_reviews, city$neighbourhood, sum)
      reviews_df <- data.frame(neighbourhood = names(reviews_sum),reviews_sum=reviews_sum)
      avg_price <- aggregate(city$log_price, by = list(city$neighbourhood), mean)
      avg_price <- avg_price[order(avg_price$x, decreasing = TRUE), ]
      colnames(avg_price)[1] <- "neighbourhood"
      merged_df <- merge(avg_price, reviews_df, by = "neighbourhood")
      colnames(merged_df)[2] <- "log_price"
      
      obs_count <- aggregate(city$log_price, by=list(city$neighbourhood), FUN=length)
      names(obs_count) <- c("neighbourhood", "observations")
      merged_df <- merge(merged_df, obs_count, by = "neighbourhood")
      
      merged_df['rev/obs']=merged_df$reviews_sum/merged_df$observations
      
      score <- city[complete.cases(city$review_scores_rating),]
      scores = tapply(score$review_scores_rating, score$neighbourhood, mean)
      score_df <- data.frame(neighbourhood = names(scores), avg_score = scores)                         
      merged_df <- merge(merged_df, score_df, by = "neighbourhood")
      
      acc=tapply(city$accommodates,city$neighbourhood,mean)
      acc_df <- data.frame(neighbourhood = names(acc), accommodates = acc)                         
      merged_df <- merge(merged_df, acc_df, by = "neighbourhood")
      
      merged_df <- merged_df[order(merged_df$log_price, decreasing = TRUE), ]
      merged_df
    }
    
    #Relationship analysis
    {
      cor(merged_df$log_price,merged_df$avg_score)
      plot(merged_df$avg_score,merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$avg_score), col="red")
      
      cor(merged_df$log_price,merged_df$'rev/obs')
      plot(merged_df$'rev/obs',merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$'rev/obs'), col="red")
      
      mean(merged_df$`rev/obs`)
    }
  }
  
  #NYC
  {
    city=NYC
    
    #Creation of the data frame
    {
      reviews_sum = tapply(city$number_of_reviews, city$neighbourhood, sum)
      reviews_df <- data.frame(neighbourhood = names(reviews_sum),reviews_sum=reviews_sum)
      avg_price <- aggregate(city$log_price, by = list(city$neighbourhood), mean)
      avg_price <- avg_price[order(avg_price$x, decreasing = TRUE), ]
      colnames(avg_price)[1] <- "neighbourhood"
      merged_df <- merge(avg_price, reviews_df, by = "neighbourhood")
      colnames(merged_df)[2] <- "log_price"
      
      obs_count <- aggregate(city$log_price, by=list(city$neighbourhood), FUN=length)
      names(obs_count) <- c("neighbourhood", "observations")
      merged_df <- merge(merged_df, obs_count, by = "neighbourhood")
      
      merged_df['rev/obs']=merged_df$reviews_sum/merged_df$observations
      
      score <- city[complete.cases(city$review_scores_rating),]
      scores = tapply(score$review_scores_rating, score$neighbourhood, mean)
      score_df <- data.frame(neighbourhood = names(scores), avg_score = scores)                         
      merged_df <- merge(merged_df, score_df, by = "neighbourhood")
      
      acc=tapply(city$accommodates,city$neighbourhood,mean)
      acc_df <- data.frame(neighbourhood = names(acc), accommodates = acc)                         
      merged_df <- merge(merged_df, acc_df, by = "neighbourhood")
      
      merged_df <- merged_df[order(merged_df$log_price, decreasing = TRUE), ]
      merged_df
    }
    
    #Relationship analysis
    {
      cor(merged_df$log_price,merged_df$avg_score)
      plot(merged_df$avg_score,merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$avg_score), col="red")
      
      cor(merged_df$log_price,merged_df$'rev/obs')
      plot(merged_df$'rev/obs',merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$'rev/obs'), col="red")
    }
  }
  
  #SF
  {
    city=SF
    
    #Creation of the data frame
    {
      reviews_sum = tapply(city$number_of_reviews, city$neighbourhood, sum)
      reviews_df <- data.frame(neighbourhood = names(reviews_sum),reviews_sum=reviews_sum)
      avg_price <- aggregate(city$log_price, by = list(city$neighbourhood), mean)
      avg_price <- avg_price[order(avg_price$x, decreasing = TRUE), ]
      colnames(avg_price)[1] <- "neighbourhood"
      merged_df <- merge(avg_price, reviews_df, by = "neighbourhood")
      colnames(merged_df)[2] <- "log_price"
      
      obs_count <- aggregate(city$log_price, by=list(city$neighbourhood), FUN=length)
      names(obs_count) <- c("neighbourhood", "observations")
      merged_df <- merge(merged_df, obs_count, by = "neighbourhood")
      
      merged_df['rev/obs']=merged_df$reviews_sum/merged_df$observations
      
      score <- city[complete.cases(city$review_scores_rating),]
      scores = tapply(score$review_scores_rating, score$neighbourhood, mean)
      score_df <- data.frame(neighbourhood = names(scores), avg_score = scores)                         
      merged_df <- merge(merged_df, score_df, by = "neighbourhood")
      
      acc=tapply(city$accommodates,city$neighbourhood,mean)
      acc_df <- data.frame(neighbourhood = names(acc), accommodates = acc)                         
      merged_df <- merge(merged_df, acc_df, by = "neighbourhood")
      
      merged_df <- merged_df[order(merged_df$log_price, decreasing = TRUE), ]
      merged_df
    }
    
    #Relationship analysis
    {
      cor(merged_df$log_price,merged_df$avg_score)
      plot(merged_df$avg_score,merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$avg_score), col="red")
      
      cor(merged_df$log_price,merged_df$'rev/obs')
      plot(merged_df$'rev/obs',merged_df$log_price)
      abline(lm(merged_df$log_price~merged_df$'rev/obs'), col="red")
    }
  }
}






