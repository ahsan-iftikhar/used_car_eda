# Loading packages
library(tidyverse)
library(treemapify)

# Loading data
olx_car_data <- read_csv("data/OLX_Car_Data_CSV.csv")

glimpse(olx_car_data)

brnd_n_milag <- olx_car_data %>% 
  group_by(`Brand`) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na()


ggplot(data = brnd_n_milag, 
       mapping = aes(area = n, label = Brand)
       ) +
  geom_treemap() +
  geom_treemap_text(reflow = TRUE, grow = TRUE, colour = "white")


