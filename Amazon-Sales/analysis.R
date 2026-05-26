## Load all necessary libraries ####
  library(tidyverse) # for data cleaning, wrangling, reading data and data visualization
  library(janitor)  # used to clean column name and categorical summary statistics
  library(skimr) # to preview dataframe for data cleaning
  library(plotly) # to make interactive plot
  library(htmlwidgets) # to save interactive plot
  library(psych) # For numerical variable descriptive statistic analysis
  library(corrplot) ## for correlation heatmap visualization

##### get working directory ####
  getwd()

##### load data ####
  amazon <- read_csv("data/merged_cleaned_data.csv")
  
  ## remove scientific notation
  options(scipen = 9999)

## Apply correct variables types
  skim(amazon)
  amazon <- amazon %>% 
    mutate(across(c(months,days,status,fulfillment,
                    sales_channel,ship_service_level,
                    courier_status,ship_postal_code,
                    category,courier_status,
                    sub_category,fulfilled_by,
                    delivery_type,amazon_prime,
                    best_seller_tag), as.factor))
  
  skim(amazon$amazon_prime)
  unique(amazon$amazon_prime)
  
  
## Analysis ####  
  #### monthly and weekly order
  amazon %>% 
    count(months)
  max(amazon$date)
  min(amazon$date) #given that the data for march is only for one, it makes the data bias. bettter to omit the march data altogether
  
  amazon %>% 
    filter(months != "March") %>% 
    count(months)
  
  ### drilling down the fulfillment patterns for amazon fulfilled ordered ####
  amazon %>% 
    filter(months != "March") %>% 
    count(fulfillment)
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon") %>% 
    count(ship_state) %>% 
    arrange(desc(n)) 
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon") %>% 
    count(ship_city) %>% 
    arrange(desc(n))
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>% 
    count(ship_service_level) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n)) ## Bengaluru city most likely have the highest order because of the fact that more than 99.1% of its entire ship service are Expedited. While the typical ratio of expedited to standard ship service level is 68.7:31.3 
  amazon %>% 
    filter(months != "March") %>%
    count(ship_service_level) %>% 
    mutate(proportion  = (n/sum(n))*100)# average ship service level is 68.7:31.3 for expedited ship and standard shipping respectively.
  
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>%
    select(ship_service_level,discount_percentage) %>%
    count(ship_service_level, discount_percentage) %>% 
    summarise(avg_discount = mean(discount_percentage),
              frequency = n()) ##average discount for product brought in BENGALURU city is 49.3 which is 2.6 less than the general average
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon") %>% 
    summarise(avg_discount = mean(discount_percentage), frequency = n()) # average discount across the entire dataset is 51.9%
  amazon %>% 
    filter(months != "March") %>% 
      summarise(avg_discount = mean(discount_percentage), frequency = n()) # average discount across the entire dataset is 51.9%
  
  ##status
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru" ) %>% 
    count(status) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n)) ## they also have less cancelled order compared to average cancelled order of order fulfilled by amazon. They also, have a better shipped rate than the general shipping average.
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon") %>% 
    count(status) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n))
  amazon %>% 
    filter(months != "March") %>% 
    count(status) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n))
  ##category
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>% 
    count(category) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n)) ## proportions of order fulfilled by Amazon and ship to the city of BENGALURU is almost the same with the general proportion of the entire dataset for ordered products for category and subcategory. 
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" ) %>% 
    count(category) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n))
  ## sub category
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>% 
    count(sub_category) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n))
  amazon %>% 
    filter(months != "March" ) %>% 
    count(sub_category) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n))
  
  ## size
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>% 
    count(size) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n))
  amazon %>% 
    filter(months != "March" ) %>% 
    count(size) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n))
  ## sales
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>% 
    summarise(total_sales_price = sum(sales_price), 
              max_price = max(sales_price),
              avg_sales_price = mean(sales_price), 
              frequency = n())
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon") %>% 
    summarise(total_sales_price = sum(sales_price), 
              max_price = max(sales_price),
              avg_sales_price = mean(sales_price), 
              frequency = n()) %>% 
    arrange(desc(total_sales_price))
  
  ##ratings
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>% 
    summarise(avg_ratings = mean(rating), 
              frequency = n()) ## Bengaluru customer product ratings is slightly higher than the average rating among all customer
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon") %>% 
    summarise(avg_ratings = mean(rating), 
              frequency = n())
  amazon %>% 
    filter(months != "March") %>% 
    summarise(avg_ratings = mean(rating), 
              frequency = n())
  
  
### shipping service level drill down ####  
  amazon %>% 
    filter(months != "March") %>% 
    count(ship_service_level)
  ##expedited ship service level
  amazon %>% 
    filter(months != "March" & ship_service_level == "Expedited") %>% 
    count(status)  %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n))
  amazon %>% 
    filter(months != "March" & ship_service_level == "Standard") %>% 
    count(status)  %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n)) # Expedited have lower cancelled order among compare to standard ship service level
  
  ## cancelled order
  amazon %>% 
    filter(months != "March" & 
             ship_service_level == "Expedited" & 
             status == "Cancelled") %>% 
    count(category)  %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    arrange(desc(n)) 
  
  amazon %>% 
    filter( months != "March" & 
             ship_service_level == "Expedited" & 
             status == "Cancelled" & category %in% c("Set","kurta")) %>% 
    summarise(avg_sale_price = mean(sales_price),
              avg_rating = mean(rating),
              avg_discount = mean(discount_percentage),
              avg_review = mean(no_of_reviews),
              frequency = n()) %>% 
    arrange(desc(avg_sale_price))
  
  amazon %>% 
    filter( months != "March" & 
              ship_service_level == "Expedited" & 
              status != "Cancelled" & category %in% c("Set","kurta")) %>% 
    summarise(avg_sale_price = mean(sales_price),
              avg_rating = mean(rating),
              avg_discount = mean(discount_percentage),
              avg_review = mean(no_of_reviews),
              frequency = n()) %>% 
    arrange(desc(avg_sale_price))
  
  
### numerical analysis ####
  merged_cleaned_data %>% 
    filter(months != "March") %>%
    drop_na(courier_status) %>% 
    count(courier_status) %>% 
    mutate(proportion = (n/sum(n))*100)
  
  amazon %>% 
    filter(months != "March" ) %>% 
    group_by(amazon_prime) %>% 
    drop_na(rating,no_of_reviews,sales_price) %>% 
    summarise(avg_rating = mean(rating),
              avg_sales_price = mean(sales_price), 
              avg_discount_percent = mean(discount_percentage),
              avg_review = mean(no_of_reviews),
              frequency = n())
  ## check for correlation 
  amazon %>% 
    select(discount_percentage,rating,sales_price) %>% 
    drop_na() %>% 
    cor(method = "pearson")
    #slight inverse relationship between discount and rating, too small therefor is it negligible.
    # extremely weak correlation sales price and rating
  
## drill down
  amazon %>% 
    filter(months != "March") %>% 
    count(rating) %>% 
    mutate(proportion  = (n/sum(n))*100) %>% 
    drop_na() %>% 
    arrange(desc(n)) 
 
## others 
  amazon %>% 
    tabyl(months,days) %>% 
     filter(months != "March")
  
  amazon %>% 
    filter(months != "March") %>% 
    count(status) %>% 
    arrange(desc(n))
  