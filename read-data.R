# Normally use tabulizer, but I have a lot of trouble with using it on Ununtu.
# Therefore use docker image and instruction from here: http://www.nagraj.net/notes/docker-tabulizer/

# cd data/ips
# mkdir output
# infile=Internet_Liste_SGI_zertifizierte_Intensivstationen_200311.01.pdf 
# docker run -ti -v $(pwd)/$infile:/$infile -v $(pwd)/output:/output vpnagraj/tabulizer $infile 1
# docker run -ti -v $(pwd)/$infile:/$infile -v $(pwd)/output:/output vpnagraj/tabulizer $infile 2
# docker run -ti -v $(pwd)/$infile:/$infile -v $(pwd)/output:/output vpnagraj/tabulizer $infile 3
# docker run -ti -v $(pwd)/$infile:/$infile -v $(pwd)/output:/output vpnagraj/tabulizer $infile 4

library(tidyverse)
library(readxl)
library(writexl)

ips <- tibble()
for (f in list.files("data/ips/output/")) {
  temp <- read_csv(paste0("data/ips/output/", f))
  
  colnames(temp) <- temp[1,] %>% unlist()
  
  temp <- temp %>%
    janitor::clean_names() %>% 
    rename(beds = zertifizierte) %>% 
    filter(row_number() > 2,
           !is.na(beds))
  
  ips <- bind_rows(ips, temp)
}

ips <- ips %>% 
  mutate(beds = as.integer(beds),
         anza = as.integer(anza),
         plz = as.integer(plz))

ips %>% 
  group_by(spital) %>% 
  summarise(beds = sum(beds))

ips %>% 
  write_xlsx("data/ips/ips.xlsx")

plz_kanton <- read_excel("data/kantone/be-b-00.04-osv-01.xls",
                         sheet = "Ortschaftenverz.-Rép. Localités")

ips <- ips %>%
  mutate(spital = if_else(is.na(spital), spital_station, spital)) %>% 
  select(-spital_station) %>% 
  select(spital, station, plz, ort, beds) %>% 
  left_join(plz_kanton %>% select(KTplz = KTKZ, plz = PLZ4) %>% group_by(plz) %>% filter(row_number() == 1) , by = "plz") %>% 
  left_join(plz_kanton %>% select(KTort = KTKZ, ort = ORTNAME) %>% group_by(ort) %>% filter(row_number() == 1), by = "ort") %>% 
  mutate(KT = if_else(is.na(KTplz), KTort, KTplz)) %>% 
  select(-KTplz, -KTort) %>% 
  distinct() %>% 
  mutate(KT = if_else(spital == "Spitalzentrum Biel (SPZ)", "BE", KT)) %>%
  mutate(KT = if_else(ort == "Genève 14", "GE", KT))


#### spialstatistik

spitalstatistik <- read_excel("data/spitalstatistik/kzp17_daten.xlsx", sheet = "KZ2017_KZP17") %>% 
  filter(!is.na(KT))

ss_betten <- spitalstatistik %>% 
  group_by(KT) %>% 
  summarise(BettenStatA = sum(BettenStatA, na.rm = T),
            Ops = sum(Ops, na.rm = T))


ips_betten <- ips %>% 
  group_by(KT) %>% 
  summarise(beds = sum(beds))

### einwohner

einwohner <- read_excel("data/kantone/je-d-21.03.02.xlsx", skip = 3) %>% 
  rename(Feld = 1) %>% 
  select(-Jahre, -Schweiz) %>% 
  filter(Feld %in% c("Einwohner in 1000", "65 und mehr")) %>% 
  mutate(Feld = if_else(Feld == "Einwohner in 1000", "pop1000", "ant65")) %>% 
  mutate_at(vars(2:ncol(.)), as.numeric) %>% 
  pivot_longer(-Feld, names_to = "KT") %>% 
  pivot_wider(names_from = Feld, values_from = value) %>% 
  mutate(pop1000_65 = pop1000*ant65/100)


left_join(ss_betten, 
          ips_betten,
          by = "KT") %>% 
  pivot_longer(-KT) %>% 
  filter(name %in% c("BettenStatA", "beds")) %>%
  mutate(value = replace_na(value, 0)) %>% 
  group_by(KT, name) %>% 
  expand(value = seq(1:value)) %>% 
  # ungroup() %>% 
  filter(value %% 50 == 0 | value == min(value) | value == max(value)) %>% 
  ggplot(aes(x = KT, y = value, color = name)) +
  geom_point() +
  facet_wrap(~name, scales = "free_y", ncol = 1)
  
left_join(ss_betten, 
          ips_betten,
          by = "KT") %>% 
  pivot_longer(-KT) %>% 
  filter(name %in% c("BettenStatA", "beds")) %>%
  mutate(value = replace_na(value, 0)) %>% 
  left_join(einwohner, by = "KT") %>% 
  mutate(pop_pro_bett = pop1000*1000/value,
         pop_65_pro_bett = pop1000_65*1000/value) %>% 
  select(KT, name, starts_with("pop_")) %>% 
  rename(Betttyp = name) %>% 
  pivot_longer(-c("KT", "Betttyp")) %>% 
  mutate(Betttyp = if_else(Betttyp == "BettenStatA", "Akut-stat.", "IPS")) %>% 
  ggplot(aes(x = reorder(KT, value), y = value, color = Betttyp, alpha = name)) +
  geom_point() +
  facet_wrap(~Betttyp, scales = "free_y", ncol = 1)

data_ips <- left_join(ss_betten, 
          ips_betten,
          by = "KT") %>% 
  pivot_longer(-KT) %>% 
  filter(name %in% c("BettenStatA", "beds")) %>%
  mutate(value = replace_na(value, 0)) %>% 
  left_join(einwohner, by = "KT") %>% 
  mutate(pop_pro_bett = pop1000*1000/value,
         pop_65_pro_bett = pop1000_65*1000/value) %>% 
  select(KT, name, value, starts_with("pop_")) %>% 
  filter(name == "beds") %>% 
  select(-name)

data_sta <- left_join(ss_betten, 
                      ips_betten,
                      by = "KT") %>% 
  pivot_longer(-KT) %>% 
  filter(name %in% c("BettenStatA", "beds")) %>%
  mutate(value = replace_na(value, 0)) %>% 
  left_join(einwohner, by = "KT") %>% 
  mutate(pop_pro_bett = pop1000*1000/value,
         pop_65_pro_bett = pop1000_65*1000/value) %>% 
  select(KT, name, value, starts_with("pop_")) %>% 
  filter(name != "beds") %>% 
  select(-name)

kt_order <- einwohner %>% 
  arrange(pop1000) %>% 
  pull(KT)

p1 <- data_ips %>% 
  pivot_longer(-KT) %>%
  filter(name == "value") %>%
  ggplot(aes(x = factor(KT, levels = kt_order), y = value)) +
  geom_col(fill = "red", alpha = 0.6) +
  coord_flip() +
  labs(x = "", y = "Betten IPS") +
  theme_light()

p2 <- data_ips %>% 
  pivot_longer(-KT) %>%
  filter(name != "value") %>%
  ggplot(aes(x = factor(KT, levels = kt_order), y = value, color = name, group = KT)) +
  geom_point() +
  scale_color_manual(values = c("gray", "red")) +
  geom_path(color = "black", alpha = 0.1) +
  coord_flip() +
  labs(x = "", y = "Einwohner pro Bett") +
  theme_light()

p3 <- data_sta %>% 
  pivot_longer(-KT) %>%
  filter(name == "value") %>%
  ggplot(aes(x = factor(KT, levels = kt_order), y = value)) +
  geom_col(fill = "blue", alpha = 0.6) +
  coord_flip() +
  labs(x = "", y = "Betten Akut-stationär") +
  theme_light()

p4 <- data_sta %>% 
  pivot_longer(-KT) %>%
  filter(name != "value") %>%
  ggplot(aes(x = factor(KT, levels = kt_order), y = value, color = name, group = KT)) +
  geom_point() +
  scale_color_manual(values = c("gray", "blue")) +
  geom_path(color = "black", alpha = 0.1) +
  coord_flip() +
  labs(x = "", y = "Einwohner pro Bett") +
  theme_light()

library(egg)

grid.arrange(p1, p2, p3, p4, nrow = 2)
