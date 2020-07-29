library(tidyverse)
library(readxl)
library(lubridate)

# data has to be manually downloaded from 
# https://www.bag.admin.ch/bag/en/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/situation-schweiz-und-international.html#-640157857

positivity <- read_excel("data/Dashboard_3_COVID19_labtests_positivity.xlsx")
testing <- read_excel("data/Dashboards_1&2_COVID19_swiss_data_pv.xlsx")

positivity_weekly <- positivity %>% 
  mutate(Kalenderwoche = week(Datum)) %>% 
  group_by(Kalenderwoche, Outcome_tests) %>% 
  summarise(Number_of_tests = sum(Number_of_tests))


positivity_weekly %>% 
  ggplot(aes(x = Kalenderwoche, y = Number_of_tests, fill = Outcome_tests)) +
  geom_col()

testing_weekly <- testing %>% 
  mutate(fall_dt = ymd(fall_dt)) %>% 
  mutate(Kalenderwoche = week(fall_dt)) %>% 
  group_by(ktn, akl, sex, Geschlecht) %>% 
  summarise(fallklasse_3 = sum(fallklasse_3, na.rm = T), pttoddat = sum(pttoddat, na.rm = T), pttod_1 = sum(pttod_1, na.rm = T))


positivity %>% 
  filter(Outcome_tests == "Positive") %>% 
  mutate(avg3 = (lag(Number_of_tests) + lag(Number_of_tests, 2) + lag(Number_of_tests, 3))/3,
         avg7 = (lag(Number_of_tests) + lag(Number_of_tests, 2) + lag(Number_of_tests, 3) +
                   lag(Number_of_tests, 4) + lag(Number_of_tests, 5) + lag(Number_of_tests, 6) + lag(Number_of_tests, 7))/7) %>% 
  mutate(change = Number_of_tests/lag(Number_of_tests)-1,
         change3 = Number_of_tests/avg3,
         change7 = Number_of_tests/avg7) %>% 
  ggplot(aes(x = Datum, y = change7)) +
  geom_line()


# change relative to same day last week
positivity %>% 
  filter(Outcome_tests == "Positive",
         Datum >= as.Date("2020-06-01")) %>% 
  mutate(changeW = Number_of_tests/lag(Number_of_tests, 7)-1) %>% 
  ggplot(aes(x = Datum, y = changeW)) +
  geom_line()


testing %>% 
  mutate(fall_dt = ymd(fall_dt)) %>% 
  filter(!is.na(fall_dt),
         Geschlecht != "Unbekannt") %>% 
  ggplot(aes(x = fall_dt, y = fallklasse_3, fill = ktn)) +
  geom_col() +
  facet_wrap(~Geschlecht)


positivity %>% 
  group_by(Datum) %>% 
  summarise(Number_of_tests = sum(Number_of_tests)) %>% 
  ggplot(aes(x = Datum, y = Number_of_tests)) +
  geom_col()  

