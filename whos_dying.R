library(tidyverse)
library(lubridate)

source <- "https://raw.githubusercontent.com/openZH/covid_19/master/fallzahlen_kanton_alter_geschlecht_csv/COVID19_Fallzahlen_Kanton_ZH_altersklassen_geschlecht.csv"

data <- read_csv(source)

data %>% 
  filter(NewDeaths > 0) %>% 
  ggplot(aes(x = Week, y = NewDeaths, fill = AgeYearCat)) +
  geom_col() +
  facet_wrap(~AgeYearCat)
