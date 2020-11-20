library(tidyverse)
library(lubridate)
library(readxl)

files <- read_delim("https://www.functor.xyz/covid_19b/unique_xlsx.txt", delim = " ", col_names = F) %>% 
  mutate(X2 = str_trim(X2))

files %>% 
  filter(str_detect(X2, "Datengrundlage_Grafiken_COVID-19-Bericht.xlsx")) %>% 
  mutate(date = ymd_hm(str_sub(X2, 3, 18))) %>% 
  arrange(date) %>% 
  group_by(d = as.Date(date)) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(-d) %>% 
  tail()


library(rvest)

view <- "https://www.functor.xyz/covid_19b/"

html <- read_html(view)

dirs <- html_table(html)[[1]] %>% 
  as_tibble()

dirs_filtered <- dirs %>% 
  mutate(date = ymd_hm(str_sub(X1, 1, 16))) %>% 
  filter(!is.na(date)) %>% 
  arrange(date) %>% 
  group_by(d = as.Date(date)) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(-d) %>% 
  # avoid downloading again
  filter(date >= as.Date("2020-10-28"))

for (r in 1:nrow(dirs_filtered)) {
  print(r)
  
  link <- paste0("https://www.functor.xyz/covid_19b/", 
                 dirs_filtered[[r, 1]],
                 "/200325_Datengrundlage_Grafiken_COVID-19-Bericht.xlsx")
  
  filename <- paste0("bag_excels/", as.Date(dirs_filtered[[r, "date"]]), ".xlsx")
  
  tryCatch(download.file(link, destfile = filename), 
           error = function(e) print(paste(filename, 'did not work out'))) 
}

files <- list.files("bag_excels")

data <- tibble()
for (file in files) {
  f <- paste0("bag_excels/", file)
  date <- ymd(str_remove(file, ".xlsx"))
  
  print(date)
  
  if (file.size(f)) {
    if ("COVID19 Altersverteilung Hospit" %in% excel_sheets(f)) {
      d <- read_excel(f,
                      sheet = "COVID19 Altersverteilung Hospit", skip = 5) %>% 
        filter(row_number() %in% 1:9) %>% 
        select(Altersklasse,
               `Männlich: Anzahl hospitalisiert`,
               `Weiblich: Anzahl hospitalisiert`) %>% 
        mutate(Total = `Männlich: Anzahl hospitalisiert` + `Weiblich: Anzahl hospitalisiert`)
      
      data <- bind_rows(data, 
                        d %>% mutate(date = date))
    }
  }
}

data %>% 
  arrange(Altersklasse, date) %>% 
  group_by(Altersklasse) %>% 
  mutate(m_diff = `Männlich: Anzahl hospitalisiert`-lag(`Männlich: Anzahl hospitalisiert`),
         w_diff = `Weiblich: Anzahl hospitalisiert`-lag(`Weiblich: Anzahl hospitalisiert`),
         tot_diff = Total-lag(Total)) %>% 
  ggplot(aes(y = tot_diff, x = date, fill = Altersklasse)) +
  geom_col() +
  facet_wrap(~Altersklasse)

data %>% 
  arrange(Altersklasse, date) %>% 
  group_by(Altersklasse) %>% 
  mutate(m_diff = `Männlich: Anzahl hospitalisiert`-lag(`Männlich: Anzahl hospitalisiert`),
         w_diff = `Weiblich: Anzahl hospitalisiert`-lag(`Weiblich: Anzahl hospitalisiert`),
         tot_diff = Total-lag(Total)) %>% 
  ggplot(aes(x = date, y = tot_diff, fill = Altersklasse)) +
  geom_col()



