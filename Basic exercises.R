# Load needed package
library(tidyverse)
library(stringi)
library(rvest)
library(ggplot2)
# Import the file and have a look at it
df_auto <- read_csv("otomoto_web2020.csv")

#glimpse(df_auto)

#--------------------------- 1.Task1--------------------------------------
# Rename the variables in the data frame
# capacity --> engine_capacity
# fuel --> fuel_type
# power --> horse_power
# brand --> make
# used --> new_used
# transmission --> gearbox
# authorised_service --> aso
df_auto <- rename(df_auto, engine_capacity = capacity, fuel_type = fuel, 
                  horse_power = power, make = brand, new_used = used,
                  gearbox = transmission, aso = authorised_service)

names(df_auto)

#--------------------------- 2.Task2--------------------------------------
#查找问题
# Process the variables
# change mileage type to numeric
mil_repaspace <- str_replace_all(df_auto$mileage,' ','') 
df_auto$mileage <- stri_extract_all(mil_repaspace, regex = "\\d+", 
                                    simplify = TRUE) %>%
  as.numeric()
# change engine_capacity type to numeric
en_extract <- stri_extract_all(df_auto$engine_capacity, regex = "\\d+\\s\\d+", 
                                 simplify = TRUE)
df_auto$engine_capacity <- str_replace_all(en_extract,' ','') %>%
  as.numeric()
# change horse_power type to numeric
df_auto$horse_power <- stri_split(df_auto$horse_power, fixed = "K", simplify = TRUE)[, 1] %>% 
  map(stri_trim) %>% 
  unlist() %>% 
  as.numeric()

glimpse(df_auto)

#--------------------------- 3.Task3--------------------------------------
# Process the price variable
price_df <- stri_split(df_auto$price, fixed = " ", simplify = TRUE) %>%
  as.data.frame()
# Name price_df column as priceValue and priceCurrency
names(price_df) <- c("priceValue", "priceCurrency")
# Process the price variable so that one column has the price (numeric)
price_df$priceValue <- str_replace_all(price_df[,1], ',', '') %>%
 as.numeric()
# Check kind of currency
currency_kind <- stri_unique(price_df$priceCurrency) #"PLN" "EUR"
# Extract the EUR-PLN exchange rate from the web
cur <- read_html("https://www.nbp.pl/homen.aspx?f=/kursy/ratesa.html")
eur_line <- cur %>% html_elements('table.nbptable') %>%
  html_table() %>%
  as.data.frame() %>%
  filter(Currency == "Euro")
# Change all the EUR price into PLN price depend the web's exchange rate
eurindex <- which(price_df$priceCurrency == currency_kind[2])
price_df[eurindex,] <- price_df %>% 
  filter(priceCurrency == currency_kind[2]) %>%
  mutate(priceValue = priceValue * eur_line$Mid.rate)
# Change all text priceCurrency to PLN
price_df[,2] <- str_replace_all(price_df$priceCurrency, 
                                        pattern = currency_kind[2], 
                                        replacement = currency_kind[1])
# Replace the column price in the original data frame
df_auto <- df_auto %>% 
  mutate(price_df, .before = price) 
price_ind <- which(colnames(df_auto) == "price" )
df_auto <- df_auto[,-price_ind]  

glimpse(df_auto)

#--------------------------- 4.Task4--------------------------------------

# Create a column-colorAscii, all color names are written in lowercase, 
# and there are no Polish diacritic marks
df_auto$colorAscii <- stri_trans_general(df_auto$color,id = "Latin-ASCII") %>%
  stri_trans_tolower() 
# Recode the variable colorAscii to have English names instead of Polish
df_colorAscii <- data.frame(
  colorAsciipolish = c("bezowy", "bialy", "bordowy", "brazowy", "czarny",
                       "czerwony", "fioletowy", "inny kolor", "niebieski",
                       "srebrny", "szary", "zielony", "zloty", "zolty"),
  colorAsciiEnglish = c("Beige", "White", "Maroon", "Brown", "Black",
                        "Red", "Purple", "Other color", "Blue",
                        "Silver", "Gray", "Green", "Gold", "Yellow"))
colornumber <- length(df_colorAscii$colorAsciipolish)#14
df_auto$colorAscii <- stri_replace_all(df_auto$colorAscii, replacement = df_colorAscii[1,2], 
                   fixed = df_colorAscii[1,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[2,2], 
                   fixed = df_colorAscii[2,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[3,2], 
                   fixed = df_colorAscii[3,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[4,2], 
                   fixed = df_colorAscii[4,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[5,2], 
                   fixed = df_colorAscii[5,1])%>%
  stri_replace_all(., replacement = df_colorAscii[6,2], 
                   fixed = df_colorAscii[6,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[7,2], 
                   fixed = df_colorAscii[7,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[8,2], 
                   fixed = df_colorAscii[8,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[9,2], 
                   fixed = df_colorAscii[9,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[10,2], 
                   fixed = df_colorAscii[10,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[11,2], 
                   fixed = df_colorAscii[11,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[12,2], 
                   fixed = df_colorAscii[12,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[13,2], 
                   fixed = df_colorAscii[13,1]) %>%
  stri_replace_all(., replacement = df_colorAscii[14,2], 
                   fixed = df_colorAscii[14,1]) %>%
  stri_trans_tolower()

unique(df_auto$colorAscii)
#--------------------------- 5.Task5--------------------------------------

# Create a column named NoOffer which is unique offer number from the url column
df_auto$NoOffer <- df_auto$url %>%
  stri_extract_all(., regex = "ID\\w+") %>%
  unlist()

glimpse(df_auto$NoOffer)

#--------------------------- 6.Task6--------------------------------------
#In the column features there is a list of elements of the car's equipment. 
#Suggest a strategy for processing this column (discussed 
#during the labs: data frame of the features). 
#Then analyze these features: the result should be graphs.

# Count the number of every feature
fea_table <- df_auto$features %>%
  stri_split(., fixed = "|", simplify = TRUE) %>%
  stri_remove_empty_na() %>%
  stri_trim() %>%
  table()

# Create unique problem feature name
pro_name <- names(fea_table) 

# Create a correct feature name(without space, bracket) and be in lowercase
cor_name <- pro_name %>%
  stri_trans_tolower() %>%
  stri_replace_all(., replacement = "_", fixed = " ") %>%
  stri_replace_all(., replacement = "", fixed = c("(", ")"), 
                   vectorize_all = FALSE)

# Creat a feature dataframe
df_fea <- fea_table %>%
  as.data.frame() %>% 
  mutate(Features = cor_name , .before = Freq) 

# Select top 20 features
top_ten_ind <- order(-df_fea$Freq)[1:20]
plot_top_fea <- df_fea[top_ten_ind,]

# Use ggplot2 create plot_top_fea and display statistics for top features
ggplot(plot_top_fea, aes(x = reorder(.,Freq), y = Freq)) +
  geom_col() +
  labs(x="Features",y="Usage Frequency",title = "The 20 most used features of cars in otomoto2020") + 
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
       axis.text=element_text(size=6, face = "bold")) +
  geom_col(fill = "#669933") +
  geom_text(aes(label=Freq),size=2, vjust=-0.5) + 
  coord_flip()

# test <- df_auto$features %>%
#   stri_trim() %>%
#   stri_split(., fixed = "|", simplify = TRUE) %>%
#   as.matrix() %>%
#   t()
# li <- match(pro_name,test[,1],nomatch = 0)
# li[which(li>0)] <- 1
#---------------------------Business problem------------------------------------
#Taking the analysis of otomoto_web2020 data as an example, web scraping can help
# us efficiently combine multiple dimensions of car data set, and quickly monitor 
# the prices of different types and features of cars, helping us optimize pricing 
#and predict car sales market trends. Through such an intelligent data capture method,
#the efficiency of information acquisition and the accuracy of information statistics 
#and predictions are greatly improved, which can efficiently help companies obtain 
#valuable data and achieve rapid and accurate matching between users and information,
#thereby attracting more potential customers and increase company's market share.




  
  

  





