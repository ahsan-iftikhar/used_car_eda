# Loading packages
library(tidyverse)
library(treemapify)
library(paletteer)
library(ggtext)
library(showtext)

font_add_google(name = "Lora", family = "title_font")
font_add_google(name = "Roboto", family = "body_font")

showtext_auto()

# Loading data
olx_car_data <- read_csv("data/OLX_Car_Data_CSV.csv")

glimpse(olx_car_data)

brnd_n_milag <- olx_car_data %>% 
  group_by(`Brand`) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na()

dim(brnd_n_milag)

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
  theme(
    legend.position = "none",
    plot.caption = element_text(size = 7),
    text = element_text(family = "body_font", size = 10),
    plot.title = element_markdown(family = "title_font", size = 16,
                                  color = "#0f3e63"),
    plot.margin = margin(3, 3, 3, 3, "mm")
  ) 

glimpse(olx_car_data)

ggplot(olx_car_data, 
       aes(x = Year, y = Price)) + 
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_y_log10() +
  theme_minimal()

