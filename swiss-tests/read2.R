library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# who's dying

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
  geom_text(data = all_time, aes(label = paste0("Tote seit März: ", number(deaths, big.mark = "'", accuracy = 1))), 
            x = as.Date("2020-05-01"), y = 35, size = 3, color = "#454545", hjust = 0) +
  geom_text(data = since_june, aes(label = paste0("Tote seit Juni: ", number(deaths, big.mark = "'", accuracy = 1))), 
            x = as.Date("2020-05-01"), y = 30, size = 3, color = "#454545", hjust = 0) +
  facet_wrap(~akl) +
  labs(title = "Anzahl Tote pro Tag",
       subtitle = "nach Altersklasse, Daten Schweiz: BAG", y = "Anzahl Tote", x = "") +
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
  geom_text(data = all_time_cases, aes(label = paste0("Positive seit März: ", number(cases, big.mark = "'", accuracy = 1))), 
            x = as.Date("2020-04-01"), y = 400, size = 3, color = "#454545", hjust = 0) +
  geom_text(data = since_june_cases, aes(label = paste0("Positive seit Juni: ", number(cases, big.mark = "'", accuracy = 1))), 
            x = as.Date("2020-04-01"), y = 300, size = 3, color = "#454545", hjust = 0) +
  facet_wrap(~akl) +
  labs(title = "Anzahl Fälle (Positive Tests) pro Tag",
       subtitle = "nach Altersklasse, Daten Schweiz: BAG", y = "Anzahl Positive Tests", x = "") +
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
            x = 3, y = 0.5, size = 3, color = "#454545", hjust = 0) +
  geom_text(data = since_june_deathchance, aes(label = paste0("Tote/Pos. seit Juni: ", percent(death_change, accuracy = 0.01))), 
            x = 3, y = 0.4, size = 3, color = "#454545", hjust = 0) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = 3:10, labels = c("Mrz", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt")) +
  facet_wrap(~akl) +
  labs(title = "Sterbewahrscheinlichkeit", 
       subtitle = "nach Altersklasse, Daten Schweiz: BAG", y = "Tote pro Anzahl Positive", x = "") +
  theme_bw() +
  theme(legend.position = "none")


  
