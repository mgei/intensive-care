library(tidyverse)
library(ggrepel)

data <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland.csv")


data_new <- read_csv("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total.csv")

data_new <- data_new %>% 
  group_by(date) %>% 
  summarise(cases = sum(ncumul_conf, na.rm = T))

data_all_CH <- data_new %>% 
  mutate(Kanton = "CH") %>% 
  rename(Date = date)


# data %>%
#   pivot_longer(names_to = "Kanton", values_to = "cases", -Date) %>% 
#   filter(Kanton != "CH") %>% 
#   ggplot(aes(x = Date, y = cases, color = Kanton)) +
#   geom_line()
# 
# # https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Switzerland
# data_early <- tibble(Date = seq(from = as.Date("2020-02-25"), to = as.Date("2020-03-05"), by = "days"),
#                      Kanton = "CH",
#                      cases = c(1, 2, 8, 13, 18, 22, 40, 55, 72, 102))
# 
# data_all_CH <- data %>% 
#   pivot_longer(names_to = "Kanton", values_to = "cases", -Date) %>% 
#   filter(Kanton == "CH") %>%
#   bind_rows(data_early) %>% 
#   arrange(Date)

data_all_CH %>%
  ggplot(aes(x = Date, y = cases)) +
  geom_line() +
  geom_text_repel(aes(label = cases)) +
  geom_vline(xintercept = as.Date("2020-03-16"), color = "red") +
  # geom_vline(xintercept = dmy_hm("16.03.2020 18:00")) +
  scale_y_log10() 


data_all_CH %>%
  mutate(g1 = cases/lag(cases)-1) %>% 
  ggplot(aes(x = Date, y = cases)) +
  geom_line() +
  geom_text_repel(aes(label = percent(g1, accuracy = 1), color = g1)) +
  geom_vline(xintercept = as.Date("2020-03-16"), color = "red") +
  # geom_vline(xintercept = dmy_hm("16.03.2020 18:00")) +
  scale_y_log10() +
  scale_color_viridis_c()

data_all_CH %>%
  mutate(g1 = cases/lag(cases)-1,
         g2 = cases/((lag(cases, 1) + lag(cases, 2))/2) - 1) %>% 
  ggplot(aes(x = Date, y = cases)) +
  geom_line() +
  geom_text_repel(aes(label = percent(g2, accuracy = 1), color = g2)) +
  geom_vline(xintercept = as.Date("2020-03-16"), color = "red") +
  # geom_vline(xintercept = dmy_hm("16.03.2020 18:00")) +
  scale_y_log10() +
  scale_color_viridis_c()

data_all_CH %>%
  mutate(g1 = cases/lag(cases)-1,
         g2 = cases/((lag(cases, 1) + lag(cases, 2))/2) - 1,
         g3 = cases/((lag(cases, 1) + lag(cases, 2) + lag(cases, 3))/3) - 1) %>% 
  rowwise() %>% 
  mutate(g1 = max(0, g1),
         g2 = max(0, g2),
         g3 = max(0, g3)) %>% 
  ggplot(aes(x = Date, y = cases)) +
  geom_line() +
  geom_text_repel(aes(label = percent(g3, accuracy = 1), color = g3)) +
  geom_vline(xintercept = as.Date("2020-03-16"), color = "red") +
  # geom_vline(xintercept = dmy_hm("16.03.2020 18:00")) +
  scale_y_log10() +
  scale_color_viridis_c()

helper_g <- data_all_CH %>%  
  mutate(g1 = cases/lag(cases) - 1) %>% 
  mutate(g1_g = g1/lag(g1) -1)

g   <- mean(helper_g$g1, na.rm = T)
g_g <- mean(helper_g$g1_g, na.rm = T)

g_lockdown   <- mean(helper_g %>% filter(Date >= as.Date("2020-03-16")) %>% .$g1, na.rm = T)
g_g_lockdown <- mean(helper_g %>% filter(Date >= as.Date("2020-03-16")) %>% .$g1_g, na.rm = T)

g_estimate <- 0.07

forecast_periods <- 14

data_forecast <- tibble(Date = seq(max(data_all_CH$Date) + days(1), length.out = forecast_periods, by = "days"),
                        helper_count = 1:forecast_periods,
                        g1 = g_estimate,
                        cases = tail(data_all_CH$cases, 2)[1]*(1+g_estimate)^helper_count)


text_size <- 3

base_actual <- data_all_CH %>% 
  mutate(g1 = cases/lag(cases)-1,
         g2 = cases/((lag(cases, 1) + lag(cases, 2))/2) - 1,
         g3 = cases/((lag(cases, 1) + lag(cases, 2) + lag(cases, 3))/3) - 1) %>% 
  rowwise() %>% 
  mutate(g1 = max(0, g1),
         g2 = max(0, g2),
         g3 = max(0, g3)) %>% 
  mutate(forecast = F)

base_actual %>% 
  bind_rows(data_forecast %>% mutate(forecast = T)) %>% 
  ggplot(aes(x = Date, y = cases)) +
  geom_line(aes(alpha = forecast)) +
  geom_point(aes(alpha = forecast)) +
  geom_text_repel(aes(label = percent(g1, accuracy = 1), color = g1, alpha = forecast), 
                  size = text_size, nudge_x = 1000, segment.alpha = 0.3, segment.size = 0.1, direction = "y") +
  geom_text_repel(aes(label = number(cases, big.mark = "'", accuracy = 1), alpha = forecast), 
                  size = text_size, nudge_x = -3, segment.alpha = 0.3, segment.size = 0.1, direction = "y") +
  geom_vline(xintercept = as.Date("2020-03-16"), color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2020-03-16") + weeks(2), color = "red", alpha = 0.5) +
  geom_label(data = tibble(x = as.Date("2020-03-16"), label = "lockdown", y = 1),
             aes(x = x, y = y, label = label), color = "red", alpha = 0.5, size = text_size) +
  geom_label(data = tibble(x = as.Date("2020-03-16") + weeks(2), label = "2 weeks into\nlockdown", y = 1),
             aes(x = x, y = y, label = label), color = "red", alpha = 0.5, size = text_size) +
  geom_hline(yintercept = seq(from = 10000, to = 60000, by = 10000), alpha = 0.2, color = "blue") +
  geom_label(data = tibble(y = seq(from = 10000, to = 60000, by = 10000)), 
             aes(label = y, y = y, x = min(data_all_CH$Date)), size = 2, color = "blue", alpha = 0.5) +
  scale_y_log10(breaks = round(2^seq(from = 0, to = 100), digits = -2)) +
  scale_color_viridis_c() +
  scale_alpha_discrete(range = c(0.8, 0.2)) +
  scale_x_date(date_breaks = "day") +
  labs(x = "", title = "number of COVID-19 cases in Switzerland", 
       subtitle = "forecast with expected daily growth 7%") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 6))

base_actual %>% 
  ggplot(aes(x = Date, y = cases)) +
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = percent(g1)), size = 2.5) +
  geom_vline(xintercept = as.Date("2020-03-16"), color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2020-03-16") + weeks(2), color = "red", alpha = 0.5) +
  geom_label(data = tibble(x = as.Date("2020-03-16"), label = "lockdown", y = 1),
             aes(x = x, y = y, label = label), color = "red", alpha = 0.5, size = text_size) +
  geom_label(data = tibble(x = as.Date("2020-03-16") + weeks(2), label = "2 weeks into\nlockdown", y = 1),
             aes(x = x, y = y, label = label), color = "red", alpha = 0.5, size = text_size) +
  scale_y_log10()

base_actual %>% 
  ggplot(aes(x = Date, y = g1)) +
  geom_line() +
  geom_point() +
  # geom_label_repel(aes(label = percent(g1)), size = 2.5) +
  geom_vline(xintercept = as.Date("2020-03-16"), color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2020-03-16") + weeks(2), color = "red", alpha = 0.5) +
  geom_label(data = tibble(x = as.Date("2020-03-16"), label = "lockdown", y = 1),
             aes(x = x, y = y, label = label), color = "red", alpha = 0.5, size = text_size) +
  geom_label(data = tibble(x = as.Date("2020-03-16") + weeks(2), label = "2 weeks into\nlockdown", y = 1),
             aes(x = x, y = y, label = label), color = "red", alpha = 0.5, size = text_size) +
  # scale_y_continuous(labels = percent) +
  geom_smooth() + 
  scale_y_log10(labels = percent) +
  labs(title = "Daily Growth Rate of Positive Tests in Switzerland")

