# Objective
# Using the data, achieve the following objectives in Pakistan.

# 1) Identify the most popular cars in Pakistan.
# 2) Identify the cars most commonly sold in Pakistan across different brands
# 3) Which is better in terms of it's resale value.
# 4) Predict the cars for the cases, which has missing data for the cars.

# Loading packages
library(tidyverse)
library(treemapify)
library(paletteer)
library(ggtext)
library(showtext)
library(gt)

font_add_google(name = "Lora", family = "title_font")
font_add_google(name = "Roboto", family = "body_font")
showtext_auto()


# Loading data
olx_car_data <- read_csv("data/OLX_Car_Data_CSV.csv")

# Data Book

# Sample data contains vehicles registered in the year
range(olx_car_data$Year, na.rm = TRUE)[1]
range(olx_car_data$Year, na.rm = TRUE)[2]

glimpse(olx_car_data)

data_book <- data_frame(
  var_name = c("Brand", 
               "Condition",
               "Fuel",
               "KMs Driven",
               "Model",
               "Price",
               "Registered City",
               "Transaction Type",
               "Year"),
  data_type = c("character",
                "character",
                "character",
                "double",
                "character",
                "double",
                "character",
                "character",
                "double"
                ),
  description = c("Brand name of the car in sample dataset",
                  "Whether the car is new or used",
                  "Fuel types used to operate the car",
                  "Kilometers the vehicle is driven",
                  "Vehicle model from the manufacturer",
                  "Price is PKR",
                  "City of Registration",
                  "Cash transaction or leased vehicle",
                  "Year of registration"
                  )
)

data_book_gt <- data_book %>% 
  gt() %>% 
  tab_header(title = "Data book",
             subtitle = "Data type and description of variables in
             the dataset") %>% 
  cols_label(
    var_name = "Variable",
    data_type = "Data type",
    description = "Description"
  )

data_book_gt


# Listing all the different types, so it can be grouped by country/region of 
# origin. Country of manufacturing, have different price points and 
# consequently market is segmented based on it. In addition, different
# brand manufacturers cater to high end customers based on the prestige,
# and brand value. Thus to make meaningful comparison it is important
# to add variables and thus compare like groups


unique(olx_car_data$Brand)


# Feature engineering
# Creating a new variable to determine the country of origin/manufacturing

european <- c("Range Rover", "Land Rover", "Porsche", "Audi",
              "BMW", "Mercedes")
japanese <- c("Toyota", "Honda", "Mitsubishi", "Lexus", "Subaru", "Suzuki",
              "Daihatsu", "Daihatsu",  "Nissan" , "Mazda")
korean = c("Hyundai" , "KIA", "Daewoo")
chinese = c("FAW", "Changan")
american = c("Chevrolet")

olx_car_data <- olx_car_data %>% 
  mutate(origin_country = case_when(
    Brand %in% european ~ "Europe",
    Brand %in% japanese ~ "Japan",
    Brand %in% korean ~ "Korea",
    Brand %in% chinese ~ "China",
    Brand %in% american ~ "American",
    TRUE ~ as.character(Brand))
  )


# Popularity of used cars in Pakistan
# Popularity of the car is determined based on its proportionate 
# representation, in the sample data

olx_car_data_by_org <- olx_car_data %>% 
  group_by(origin_country) %>% 
  count() %>% 
  drop_na() %>% 
  arrange(desc(n)) %>% 
  ungroup()

olx_car_data_by_org_gt <- olx_car_data_by_org %>% 
  gt() %>% 
  gt::tab_header(
    title = "Number of vehicles by country of origin",
    subtitle = "Japanese vehicles seem to be the most popular"
  ) %>% 
  gt::cols_label(
    origin_country = "Origin Country",
    n = "# of Vehicle"
  ) %>% 
  gt::fmt_number(
    columns = c("n"),
    use_seps = TRUE,
    decimals = 0
  )

olx_car_data_by_org_gt




brnd_n_milag <- olx_car_data %>% 
  group_by(`Brand`) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na()



ggplot(olx_car_data, 
       aes(x = Year, y = Price)) + 
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_y_log10() +
  theme_minimal()


unique(olx_car_data$Brand)





glimpse(olx_car_data)


ggplot(data = brnd_n_milag %>% 
         filter(!`Brand` %in% c("Toyota", "Suzuki", "Honda", "Daihatsu",
                                "Mitsubishi", "Nissan")), 
       mapping = aes(area = n, label = Brand, fill = Brand)
) +
  geom_treemap() +
  geom_treemap_text(colour = "white", reflow = TRUE) +
  scale_fill_paletteer_d(`"ggsci::default_igv"`) +
  labs(
    title = "**Car brands that feature on OLX**",
    subtitle = "You have much greater options to choose from Suzuki",
    caption = "OLX Data Sourced from Kaggle \n "
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(size = 7),
    text = element_text(family = "body_font", size = 10),
    plot.title = element_markdown(family = "title_font", size = 16,
                                  color = "#0f3e63"),
    plot.margin = margin(3, 3, 3, 3, "mm")
  ) 

glimpse(olx_car_data)
