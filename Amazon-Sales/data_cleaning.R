## Load all necessary libraries ####
  library(tidyverse) # for data cleaning, wrangling, reading data and data visualization
  library(readxl) # to read xlsx file
  library(janitor)  #used to clean column name and categorical summary statistics
  library(skimr) # to preview dataframe for data cleaning

##### get working directory ####
  getwd()
##### create necessary folder within project ####
  dir.create("data")
  dir.create("plots")
  dir.create("report")
  dir.create("scripts")
##### load data ####
  amazon_sales <- read_xlsx("data/raw_data_1.xlsx")
  amazon_sales_fashion <- read_csv("data/raw_data_2.csv")
  
  ## remove scientific notation
  options(scipen = 9999)

## Clean data ####
  ###### Amazon_sales #######
  ## Inspect data
  names(amazon_sales)
  skim(amazon_sales)
  head(amazon_sales)
  str(amazon_sales)
  

  ## Make the row 1 the header row
  colnames(amazon_sales) <- as.character(amazon_sales[1,])
  amazon_sales <- amazon_sales[-1,]
  
  ## remove duplicate
  amazon_sales <- amazon_sales %>% 
    distinct()

  ## clean columns names
  amazon_sales <- amazon_sales %>% 
                  clean_names()
  ## clean column name
  amazon_sales <- amazon_sales %>% 
                  rename(fulfillment = fulfilment)

  ## make only the first letter of each word capital letter in ship city and state columns
  amazon_sales <- amazon_sales %>% 
    mutate(across(c("ship_state","ship_city"),str_to_title))
  
  ## delete the column which is a total empty column
  amazon_sales <- amazon_sales %>% 
                  select(-na)

  ## convert variable to correct type
  amazon_sales$date <- as.Date(as.numeric(amazon_sales$date),
                             origin = "1899-12-30")
  skim(amazon_sales)
  
  ## Convert a batch of other variable to categorical variable
  amazon_sales <- amazon_sales %>% 
    mutate(across(c(status,fulfillment,
                  sales_channel,
                  ship_service_level,
                  category,size,courier_status,
                  ship_city,ship_state,fulfilled_by),
                as.factor))

  amazon_sales$qty <- as.integer(amazon_sales$qty)

  ## Extract months and day 
  amazon_sales$months <- month(amazon_sales$date,
                             label = TRUE,abbr = FALSE)
  amazon_sales$days <- wday(amazon_sales$date,
                          label = TRUE,abbr = FALSE, week_start = 1)
  amazon_sales <- amazon_sales %>% 
          relocate(c(months,days),.after = date)
  unique(amazon_sales$days)
  unique(amazon_sales$months)

  ## fill the empty cell in currency column
  amazon_sales$currency <- amazon_sales$currency %>% 
                    replace_na("INR")
  skim(amazon_sales)
  
  ## save cleaned data amazon_sales dataframe
  write_csv(amazon_sales, "data/clean_raw_data_1.csv")

  unique(amazon_sales$months)
  
###### Amazon_sales_fashion #######
  ## clean column names
  amazon_sales_fashion <- amazon_sales_fashion %>% 
                            clean_names()
  
  ## rename columns where necessary
  amazon_sales_fashion <- amazon_sales_fashion %>% 
        rename("amazon_prime" = "amazon_prime_y_or_n",
               "best_seller_tag" = "best_seller_tag_y_or_n",
               "seller_id" = "saller_id")
  
  ## remove columns with only NAs 
  amazon_sales_fashion <- amazon_sales_fashion %>% 
                        select(-c(x20:x25))
  
  ## remove duplicates if any
  amazon_sales_fashion <- amazon_sales_fashion %>% 
                            distinct()
 
  ## remove fulfilled_by_ in delivery type column
  amazon_sales_fashion$delivery_type <- 
        amazon_sales_fashion$delivery_type %>% 
                  str_remove("fulfilled_by_")

  ## remove "%" sign from discount percentage
  amazon_sales_fashion$discount_percentage <- 
    amazon_sales_fashion$discount_percentage %>% 
                  str_remove("%")

  ## turn n to no and y to yes
  amazon_sales_fashion <- amazon_sales_fashion %>% 
    mutate(amazon_prime = recode(amazon_prime, 
                                 "N"="No", "Y"="Yes"))
  amazon_sales_fashion <- amazon_sales_fashion %>% 
    mutate(best_seller_tag = recode(best_seller_tag, 
                                 "N"="No", "Y"="Yes"))
  
  unique(amazon_sales_fashion$amazon_prime)
  unique(amazon_sales_fashion$best_seller_tag)  
  skim(amazon_sales_fashion) 
  
  ## drop some columns
  amazon_sales_fashion <- amazon_sales_fashion %>% 
            select(-c(large,product_url,product_details,))
  
  ## convert to correct variable type
  amazon_sales_fashion<- amazon_sales_fashion %>% 
    mutate(across(c(amazon_prime,best_seller_tag,
                    brand,colour,delivery_type,category,
                    seller_name), as.factor))
  amazon_sales_fashion$discount_percentage <- as.numeric(
    amazon_sales_fashion$discount_percentage
  )
  
  ## clean category column
  unique(amazon_sales_fashion$category)
  amazon_sales_fashion$category <- str_replace_all(
    amazon_sales_fashion$category, "(?<=[a-z])(?=[A-Z])", " "
  )
  amazon_sales_fashion$category <- str_replace_all(
    amazon_sales_fashion$category, "Mens", "Men"
  )
  amazon_sales_fashion$category <- str_replace_all(
    amazon_sales_fashion$category, "Womens", "Women"
  )
  skim(amazon_sales_fashion)
  
  ## clean numerical columns
  unique(amazon_sales_fashion$rating) # rating can't be more than 5
  amazon_sales_fashion <- amazon_sales_fashion %>% 
    filter(rating <= 5)

  ## select columns that will be used for analysis
  amazon_sales_fashion <-  amazon_sales_fashion %>% 
    select(-c(description, product_name,seller_id,
              brand,colour,seller_name, left_in_stock))
  ##left in stock, seller name and color columns are removed because in it will cause serious bais in analysis is it used on as they have missing data from 76-88% range.
  ## The other columns are removed because they won't be used in the analysis and they have very high unique  values ranging from 29,000+ to 6,000+ for a dataframe with 29,000+ observations.
  
  ##  save cleaned amazon sales fashion data
  write_csv(amazon_sales_fashion, "data/clean_raw_data_2.csv")
  
  ##merge data for analysis
  identical(amazon_sales$asin, amazon_sales_fashion$asin)
  skim(amazon_sales$asin)
  skim(amazon_sales_fashion$asin)
  merged_cleaned_data <- inner_join(x = amazon_sales,
                                    y = amazon_sales_fashion,
                                    by = "asin")
  ## rename some columns
  merged_cleaned_data <- merged_cleaned_data %>% 
                    rename(category = category.x,
                          sub_category = category.y)
  ##relocate the rename columns to follow each other
  merged_cleaned_data <- merged_cleaned_data %>% 
                        relocate(sub_category,.after = category)
  write_csv(merged_cleaned_data, "data/merged_cleaned_data.csv")
  