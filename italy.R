library(tidyverse)
library(readxl)
library(lubridate)


italy <- read_excel("data/italy.xls") %>% 
  mutate(Date = ymd(Date))

p <- italy %>% 
  select(1:22) %>% 
  mutate_if(is.character, as.double) %>% 
  pivot_longer(cols = -Date) %>% 
  # filter(name == "LOM") %>% 
  ggplot(aes(x = Date, y = value, fill = name)) +
  geom_polygon() +
  geom_line() +
  facet_wrap(~name) +
  theme(legend.position = "none")

p

library(plotly)

ggplotly(p)
