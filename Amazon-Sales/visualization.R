## Load all necessary libraries ####
  library(tidyverse) # for data cleaning, wrangling, reading data and data visualization
  library(janitor)  # used to clean column name and categorical summary statistics
  library(skimr) # to preview dataframe for data cleaning
  library(plotly) # to make interactive plot
  library(htmlwidgets) # to save interactive plot
  library(psych) # For numerical variable descriptive statistic analysis
  library(reshape2) # for mosaic chart

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
  ## order factor
  amazon$months <- factor(amazon$months,levels = c("March","April",
                                                   "May","June"), 
                                                  ordered = TRUE)
  unique(amazon$days)
  amazon$days <- factor(amazon$days, levels = c("Monday", "Tuesday",
                                                "Wednesday", "Thursday",
                                                "Friday", "Saturday",
                                                "Sunday"), ordered = TRUE)
## Visualization ####  
  
  amazon %>% 
    filter(months != "March") %>% 
    count(date, fulfillment) %>%  
    drop_na() %>% 
    mutate(frequency = log(n)) %>% 
    ggplot(aes(x=date, y = frequency, color = fulfillment)) +
    geom_line(linewidth = 0.7)  +
    theme(plot.background = element_rect( fill = "#E7E7E7" ) , 
          panel.background = element_rect(fill = "#F4F4F4"),
          panel.grid.major = element_line(color = "#858585"),
          panel.grid.minor = element_line(colour = "#858585"),
          panel.border = element_rect(colour = "#E7E7E7",fill = NA),
          axis.text= element_text(colour = "black"),
          axis.title = element_text(color = "black"), 
          axis.ticks = element_line("#E7E7E7"),
          axis.line = element_line("#E7E7E7"), 
          panel.grid.major.x = element_line(colour = "#E7E7E7"),
          panel.grid.minor.x = element_line(colour = "#E7E7E7"),
          plot.title = element_text(color = "black"), 
          plot.subtitle = element_text(color = "black"), 
          plot.caption = element_text(color = "#4A4A4A")) +
    scale_color_manual(values = c(Amazon = "#118DFF",
                                 Merchant = "#F8BC32")) +
    labs(title = "Sales Fulfillment Trend", color = "Fulfilment",
         y = "Fulfilment Frequency", x = "Date")
  #there is an inverse relationship with order fulfillment between April to the first 2/3 of the month of May but there the order was synchronize from last 1/3 of May to July. 
  skim(amazon)  
  
  #### monthly and weekly order
  amazon %>% 
    filter(months != "March") %>% 
    ggplot(aes(x = months, fill = fulfillment)) +
    geom_bar()  +
    theme(plot.background = element_rect( fill = "#E7E7E7" ) , 
          panel.background = element_rect(fill = "#F4F4F4"),
          panel.grid.major = element_line(color = "#858585"),
          panel.grid.minor = element_line(colour = "#858585"),
          panel.border = element_rect(colour = "#E7E7E7",fill = NA),
          axis.text= element_text(colour = "black"),
          axis.title = element_text(color = "black"), 
          axis.ticks = element_line("#E7E7E7"),
          axis.line = element_line("#E7E7E7"), 
          panel.grid.major.x = element_line(colour = "#E7E7E7"),
          panel.grid.minor.x = element_line(colour = "#E7E7E7"),
          plot.title = element_text(color = "black"), 
          plot.subtitle = element_text(color = "black"), 
          plot.caption = element_text(color = "#4A4A4A")) +
    scale_fill_manual(values = c(Amazon = "#118DFF",
                                  Merchant = "#F8BC32")) +
    labs(title = "Sales Fulfillment Quantity", color = "Fulfilment",
         y = "Fulfilment Frequency", x = "Months")
  
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon") %>% 
    count(ship_city) %>% 
    arrange(desc(n)) %>% 
    head(15) %>% 
    mutate(ship_city = factor(ship_city, levels = unique(ship_city))) %>% 
    ggplot(aes(x =ship_city, y = n)) +
    geom_col(fill ="#118DFF") +
    coord_flip() +
    theme(plot.background = element_rect( fill = "#E7E7E7" ) , 
          panel.background = element_rect(fill = "#F4F4F4"),
          panel.grid.major = element_line(color = "#858585"),
          panel.grid.minor = element_line(colour = "#858585"),
          panel.border = element_rect(colour = "#E7E7E7",fill = NA),
          axis.text= element_text(colour = "black"),
          axis.title = element_text(color = "black"), 
          axis.ticks = element_line("#E7E7E7"),
          axis.line = element_line("#E7E7E7"), 
          panel.grid.major.x = element_line(colour = "#E7E7E7"),
          panel.grid.minor.x = element_line(colour = "#E7E7E7"),
          plot.title = element_text(color = "black"), 
          plot.subtitle = element_text(color = "black"), 
          plot.caption = element_text(color = "#4A4A4A") ) +
    labs(title = "Sales Fulfillment Quantity for Top 15 City",
         y = "Fulfilment Frequency", x = "Ship City")
  
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>% 
    count(ship_service_level) %>% 
    mutate(log_frequency = log(n)) %>% 
    ggplot(aes(x="", y = log_frequency, fill = ship_service_level)) +
    geom_bar(stat = "identity", width = 1, color = "#F1F0EC") +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = c(Expedited = "#118DFF",
                                 Standard = "#F8BC32")) +
    labs(title = "Sales Fulfilled by Amazon to Bengaluru Shipment")
  #log_frequency was used because the standard ship service was too little
  
  amazon %>% 
    filter(months != "March" & fulfillment == "Amazon" &
             ship_city == "Bengaluru") %>% 
    count(status) %>% 
    mutate(proportion  = (n/sum(n))*100) %>%
    arrange(desc(n)) %>% 
    mutate(status = factor(status,levels = unique(status))) %>% 
    ggplot(aes(x="", y = proportion, fill = status)) +
    geom_bar(stat = "identity", width = 1, color = "#F1F0EC") +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = c(Shipped = "#118DFF",
                                 Cancelled = "#FF012F",
                                 Pending = "#F8BC32")) +
    labs(title = "Sales Status by Amazon to Bengaluru")
  
  amazon %>% 
    filter(months != "March") %>% 
    count(ship_service_level) %>% 
    mutate(log_frequency = log(n)) %>% 
    ggplot(aes(x="", y = log_frequency, fill = ship_service_level)) +
    geom_bar(stat = "identity", width = 1, color = "#F1F0EC") +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = c(Expedited = "#118DFF",
                                 Standard = "#F8BC32")) +
    labs(title = "Sales Shipment")
  
  
    
skim(amazon)
  amazon %>% 
    filter(months != "March") %>%
    drop_na(courier_status) %>% 
    ggplot(aes(x = rating, fill = courier_status)) +
      geom_histogram(bins = 7, color = "black") +
    theme(plot.background = element_rect( fill = "#E7E7E7" ) , 
          panel.background = element_rect(fill = "#F4F4F4"),
          panel.grid.major = element_line(color = "#858585"),
          panel.grid.minor = element_line(colour = "#858585"),
          panel.border = element_rect(colour = "#E7E7E7",fill = NA),
          axis.text= element_text(colour = "black"),
          axis.title = element_text(color = "black"), 
          axis.ticks = element_line("#E7E7E7"),
          axis.line = element_line("#E7E7E7"), 
          panel.grid.major.x = element_line(colour = "#E7E7E7"),
          panel.grid.minor.x = element_line(colour = "#E7E7E7"),
          plot.title = element_text(color = "black"), 
          plot.subtitle = element_text(color = "black"), 
          plot.caption = element_text(color = "#4A4A4A"))+
    facet_wrap(~courier_status, scale ="free_y") +
    scale_fill_manual(values = c(Shipped = "#118DFF",
                                 Cancelled = "#FF012F",
                                 Unshipped = "#F8BC32")) +
    labs(title = "Sales Rating Distribution",
         y = "Frequency", x = "Rating",
         fill = "Courier Status")
    # highly skewed towards high rating ratings. This could mean that products are well received by the customers.
  
  ## check for correlation using correlation heatmap 
  amazon %>% 
    select(discount_percentage,rating,sales_price) %>% 
    drop_na() %>% 
    cor(method = "pearson") %>% 
    round(4) %>% 
    melt() %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color  = "white") +
    scale_fill_gradient(low = "#118DFF",high = "#FF012F") +
    geom_text(aes(label = value)) +
    labs(x ="", y = "", fill = "Pearson Correlation",
         title = "Correlation Matrix of Rating, Sales Price and Discount Rate")  +
    theme(plot.background = element_rect( fill = "#E7E7E7" ) , 
          panel.background = element_rect(fill = "#F4F4F4"),
          panel.grid.major = element_line(color = "#858585"),
          panel.grid.minor = element_line(colour = "#858585"),
          panel.border = element_rect(colour = "#E7E7E7",fill = NA),
          axis.text= element_text(colour = "black"),
          axis.title = element_text(color = "black"), 
          axis.ticks = element_line("#F4F4F4"),
          axis.line = element_line("#F4F4F4"), 
          panel.grid.major.x = element_line(colour = "#F4F4F4"),
          panel.grid.minor.x = element_line(colour = "#F4F4F4"),
          plot.title = element_text(color = "black"), 
          plot.subtitle = element_text(color = "black"), 
          plot.caption = element_text(color = "#4A4A4A")) +
    labs(title = "Correlation Matrix of Numerical Variable")
  