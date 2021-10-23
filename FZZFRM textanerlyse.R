#############################
#### LIBSHIT - graphische Aufbereitung
library(tidyverse)
library(lubridate)
## anpassen & einladen:
getwd()
bpo <- read_csv("beneposts.csv") %>% 
  select(-categories, -is_pm)

## counts abfragen
bp2 <- bpo %>% 
  mutate(is_lib = case_when(grepl(pattern = " lib | libs ", x = post, ignore.case = T) ~ 1, 
                            TRUE ~ 0),
         pdate = as.Date(paste0(year(created_at), "-", month(created_at), "-01")))


bp2 <- bpo %>% 
  mutate(is_lib = case_when(grepl(pattern = " drachenlord ", x = post, ignore.case = T) ~ 1, 
                            TRUE ~ 0),
         pdate = as.Date(paste0(year(created_at), "-", month(created_at), "-01")))

## plotten
bp2 %>% 
  select(pdate, is_lib) %>% 
  group_by(pdate) %>% 
  filter(pdate > "2016-01-01") %>% 
  summarize(libby = sum(is_lib)) %>% 
  ggplot(aes(x=pdate, y=libby)) + geom_col(col="LightBlue") +
  xlab("Datum") + ylab("Anzahl Lib-posts") + ggtitle("Ab wann hat Bene mit dem Lib-Shit angefangen?")

## plotten
bp2 %>% 
  select(pdate, is_lib) %>% 
  group_by(pdate) %>% 
  filter(pdate > "2016-01-01") %>% 
  summarize(libby = sum(is_lib)) %>% 
  ggplot(aes(x=pdate, y=libby)) + geom_col(col="LightBlue") +
  xlab("Datum") + ylab("Anzahl Drachenlord-posts") + ggtitle("Ab wann hat Bene mit Drachenlord-Posts angefangen?")

