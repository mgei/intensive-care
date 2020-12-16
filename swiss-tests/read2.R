library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# who's dying

updated <- Sys.Date()

# wget https://www.bag.admin.ch/dam/bag/de/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-basisdaten-fallzahlen.xlsx.download.xlsx/Dashboards_1&2_COVID19_swiss_data_pv.xlsx
data <- read_excel("data/Dashboards_1&2_COVID19_swiss_data_pv.xlsx")

data <- data %>% 
  mutate(replikation_dt = as.Date(replikation_dt),
         fall_dt = ymd(fall_dt),
         pttoddat = ymd(pttoddat))



deaths <- data %>% 
  group_by(akl, pttoddat) %>% 
  summarise(deaths = sum(pttod_1))

all_time <- deaths %>% 
  group_by(akl) %>% 
  summarise(deaths = sum(deaths))

since_june <- deaths %>%
  filter(pttoddat >= as.Date("2020-06-01")) %>% 
  group_by(akl) %>% 
  summarise(deaths = sum(deaths))

deaths %>% 
  ungroup() %>% 
  filter(pttoddat != max(pttoddat, na.rm = T)) %>% 
  ggplot(aes(x = pttoddat, y = deaths, color = akl)) +
  geom_line() +
  geom_text(data = all_time, aes(label = paste0("Todesf. seit März: ", number(deaths, big.mark = "'", accuracy = 1))), 
            x = as.Date("2020-04-01"), y = 35, size = 2.5, color = "#454545", hjust = 0) +
  geom_text(data = since_june, aes(label = paste0("Todesf. seit Juni: ", number(deaths, big.mark = "'", accuracy = 1))), 
            x = as.Date("2020-04-01"), y = 25, size = 2.5, color = "#454545", hjust = 0) +
  facet_wrap(~akl) +
  labs(title = "Anzahl Todesfälle pro Tag",
       subtitle = "nach Altersklasse, Daten Schweiz: BAG", y = "Anzahl Todesfälle", x = "",
       caption = paste("Data BAG, Updated", updated)) +
  theme_bw() +
  theme(legend.position = "none")


cases <- data %>% 
  group_by(akl, fall_dt) %>% 
  summarise(cases = sum(fallklasse_3))

all_time_cases <- cases %>% 
  group_by(akl) %>% 
  summarise(cases = sum(cases))

since_june_cases <- cases %>%
  filter(fall_dt >= as.Date("2020-06-01")) %>% 
  group_by(akl) %>% 
  summarise(cases = sum(cases))

cases %>% 
  ungroup() %>% 
  filter(fall_dt != max(fall_dt, na.rm = T)) %>% 
  ggplot(aes(x = fall_dt, y = cases, color = akl)) +
  geom_line() +
  geom_text(data = all_time_cases, aes(label = paste0("Pos. seit März: ", number(cases, big.mark = "'", accuracy = 1))), 
            x = as.Date("2020-03-01"), y = 480, size = 3, color = "#454545", hjust = 0) +
  geom_text(data = since_june_cases, aes(label = paste0("Pos. seit Juni: ", number(cases, big.mark = "'", accuracy = 1))), 
            x = as.Date("2020-03-01"), y = 270, size = 3, color = "#454545", hjust = 0) +
  facet_wrap(~akl) +
  labs(title = "Anzahl Fälle (Positive Tests) pro Tag",
       subtitle = "nach Altersklasse, Daten Schweiz: BAG", y = "Anzahl Positive Tests", x = "",
       caption = paste("Data BAG, Updated", updated)) +
  theme_bw() +
  theme(legend.position = "none")


death_share <- deaths %>% 
  group_by(month = month(pttoddat), akl) %>% 
  summarise(de = sum(deaths)) %>% 
  ungroup() %>% 
  left_join(
    cases %>% 
      group_by(month = month(fall_dt), akl) %>% 
      summarise(ca = sum(cases)) %>% 
      ungroup(),
    by = c("month", "akl"))

all_time_deathchance <- death_share %>%
  group_by(akl) %>% 
  summarise(de = sum(de), ca = sum(ca)) %>% 
  mutate(death_change = de/ca)

since_june_deathchance <- death_share %>%
  filter(month >= 6) %>% 
  group_by(akl) %>% 
  summarise(de = sum(de), ca = sum(ca)) %>% 
  mutate(death_change = de/ca)

death_share %>% 
  mutate(death_chance = de/ca) %>% 
  ggplot(aes(x = month, y = death_chance, fill = akl)) +
  geom_col() +
  geom_text(data = all_time_deathchance, aes(label = paste0("Tote/Pos. seit März: ", percent(death_change, accuracy = 0.01))), 
            x = 2.5, y = 0.5, size = 2.5, color = "#454545", hjust = 0) +
  geom_text(data = since_june_deathchance, aes(label = paste0("Tote/Pos. seit Juni: ", percent(death_change, accuracy = 0.01))), 
            x = 2.5, y = 0.4, size = 2.5, color = "#454545", hjust = 0) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = 3:12, labels = 3:12) +
  facet_wrap(~akl) +
  labs(title = "Sterbewahrscheinlichkeit", 
       subtitle = "nach Altersklasse, Daten Schweiz: BAG", y = "Todesfälle pro Anzahl Positive", x = "",
       caption = paste("Data BAG, Updated", updated)) +
  theme_bw() +
  theme(legend.position = "none")
  
