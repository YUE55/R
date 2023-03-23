library(tidyverse)
library(stringi)
library(rvest)
library(httr)
library(glue)
#--------------------------------1.web crawler----------------------------------
# Set agent
my_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36 Edg/109.0.1518.52"
# Get all main web page links
no_page <- seq(from=0, to=396, by=12) 
url_pages <- glue("https://www.bootskitchenappliances.com/l/fridge_freezers/1/26/?page={no_page}") 
# Collect fridge-freezers links from main web pages
df_urlfridges <- data.frame(url_fridge = NULL, scrape_time = NULL)
for (i in seq_along(url_pages)) {
  page_main <- GET(url_pages[i], user_agent(my_agent))
  # page_main$request
  if (page_main$status_code < 400) {
    page_content <- content(page_main)
    url_fridge <- page_content %>%
      html_elements("h2.listerPodtitle a") %>%
      html_attr("href")
    url_fridge <- glue("https://www.bootskitchenappliances.com{url_fridge}")
    scrape_time <- format(Sys.time(), "%D %X")
    df_temp <- data.frame(url_fridge, scrape_time)
    df_urlfridges <- bind_rows(df_urlfridges, df_temp)
    print(glue("Iteration: {i} | Time: {scrape_time}"))
    Sys.sleep(5)
  }
}
write.csv(df_urlfridges, file = "link.csv")
# Read saved link
product_link <- read.csv("link.csv")
# Collect information about fridge-freezers
df_info <- data.frame(name = NULL, price = NULL, rating = NULL, review_amount = NULL, 
                      para_info = NULL, intro_info = NULL, scrap_time = NULL)
for (i in seq(1, nrow(product_link), by = 1)) {
  page_main <- GET(product_link[i,2], user_agent(my_agent))
  if (page_main$status_code < 400) {
    # page_main$request
    page_content <- content(page_main)
    # Name
    name <- page_content %>%
      html_element("div.summary h1") %>%
      html_text2()
    # Price
    price <- page_content %>%
      html_element("div.normal-price") %>%
      html_text2() %>%
      stri_remove_na()
    if (length(price) == 0) {
      price <- page_content %>%
        html_element("div.long-price") %>%
        html_text2()
    }
    # Rating info
    ## Rating
    rating <- page_content %>%
      html_elements(xpath = "//span[@class = 'ratingValue']/span") %>%
      html_text2()
    rating  <- stri_paste(rating[1], rating[2], sep = "/")
    ## Rating amount
    review_amount <- page_content %>%
      html_element("a.reviewLink span") %>%
      html_text2()
    # Parameters info
    para_info <- page_content %>%
      html_elements(xpath = "//div[@class = 'spec']") %>%
      html_table()  %>%
      as.data.frame()
    para_info <- stri_paste(para_info[,1], para_info[,2], sep = "=", collapse = ",")
    # Introduction info
    intro_info <- page_content %>%
      html_elements(xpath = "//div[@id = 'seoDescriptionContainer']/p") %>%
      html_text2()
    intro_info <- stri_paste(intro_info, collapse = "*")
    scrap_time <- format(Sys.time(), "%D %X")
    df_info_temp <- data.frame(name, price, rating, review_amount, para_info, 
                               intro_info, scrap_time)
    df_info <- bind_rows(df_info, df_info_temp)
    print(glue("Iteration: {i} | Time: {scrap_time}"))
    Sys.sleep(5)
  }
}
write.csv(df_info, file = "fri_boots_info.csv")

#------------------------------2.data preprocessing-----------------------------
product_info <- read.csv("fri_boots_info.csv")
glimpse(product_info)
# Remove number and time
product_info <- product_info[, -which(names(product_info) == c("X","scrap_time"))]
# Filter products with rating 
process_data <- product_info[!(is.na(product_info$rating)),]
# Process rating
rate_df <- process_data$rating %>% 
  stri_split(fixed = "/", simplify = TRUE) %>%
  as.data.frame() %>%
  map_df(as.numeric) %>% 
  as.data.frame()
process_data$rating <- rate_df[,1] 
process_data <- process_data %>%
  mutate(rating_scale = rate_df[,2], .after = rating)

# Process the price field
currency <- process_data$price %>% 
  stri_extract_all(regex = "\\p{S}", simplify = TRUE) 
cur <- stri_unique(unlist(currency))
process_data$price <- process_data$price %>%  
  stri_replace_all(replacement = "", fixed = c(cur,","),vectorize_all = FALSE) %>% 
  as.numeric() 
currency <- currency %>%
  stri_replace_all(replacement = "GBP", fixed = cur , vectorize_all = FALSE) %>%
  as.data.frame()
process_data <- process_data %>%
  mutate(price_currency = currency[,1], .after = price)

# Process the para_info field
param <- stri_split(process_data$para_info, fixed = ",") 
list_param <- list()
list_name <- list()
for (i in seq(1, nrow(process_data), by = 1)) {
  param_value_temp <- apply(str_extract_all(unlist(param[[i]]),"(?==).+",simplify 
                                            = TRUE),1,paste,collapse="")  %>%
    stri_replace_all(., replacement = "", fixed = "=") 
  empty_index <- stri_isempty(param_value_temp)
  param_value_temp <- param_value_temp[!empty_index] %>%
    stri_trans_general(id = "Latin-ASCII") %>%
    stri_trans_tolower() %>%
    stri_trim_both() 
  param_name_temp <- apply(str_extract_all(unlist(param[[i]]),".+(?==)",simplify
                                           = TRUE),1,paste,collapse="") %>%
    stri_replace_all(replacement = "", fixed = c("<U+2122>","/"), vectorize_all = FALSE) %>%
    stri_replace_all("_", regex = "\\s|-|'") %>%
    stri_replace_all("", regex = "[\\p{Ps}\\p{Pe}\\p{S}]") %>%
    stri_trans_tolower()
  param_name_temp <-param_name_temp[!empty_index]
  param_df <- data.frame(name = param_name_temp, value = param_value_temp) 
  list_name[[i]] <- param_name_temp
  list_param[[i]] <- param_df
}
param <- stri_unique(unlist(list_name)) %>% as.vector()
public_param_df <- data.frame(matrix(NA, nrow = nrow(process_data), ncol = length(param)))
names(public_param_df) <- param
for (i in seq(1, nrow(process_data), by = 1)) {
  for (j in seq(1, length(param), by = 1)){
    index <- which(list_param[[i]]$name == param[j])
    temp_value <- unique(list_param[[i]]$value[index])
    if (length(temp_value) == 0){
      temp_value <- NA
    }
    public_param_df[i,j] <- temp_value
  }
}
public_param_df <- as.data.frame(public_param_df)
process_data <- cbind(process_data[, 1:which(colnames(process_data) == "intro_info")-1], 
      public_param_df, intro_info = process_data[, which(colnames(process_data) 
                                            == "intro_info"):ncol(process_data)])
process_data <- process_data[, -which(names(process_data) == "para_info")] 
# Export pre-processed data
write.csv(process_data, file = "preprocessed_data.csv")  
