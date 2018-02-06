library(tidyverse)
library(rvest)
library(lubridate)
options(OutDec= ",")
url_liste <- c(
  "https://projects.fivethirtyeight.com/soccer-predictions/premier-league/",
  "https://projects.fivethirtyeight.com/soccer-predictions/championship/",
  "https://projects.fivethirtyeight.com/soccer-predictions/bundesliga/",
  "https://projects.fivethirtyeight.com/soccer-predictions/bundesliga-2/",
  "https://projects.fivethirtyeight.com/soccer-predictions/bundesliga-austria/",
  "https://projects.fivethirtyeight.com/soccer-predictions/serie-a/",
  "https://projects.fivethirtyeight.com/soccer-predictions/serie-b/",
  "https://projects.fivethirtyeight.com/soccer-predictions/la-liga/",
  "https://projects.fivethirtyeight.com/soccer-predictions/primeira-liga/",
  "https://projects.fivethirtyeight.com/soccer-predictions/ligue-1/",
  "https://projects.fivethirtyeight.com/soccer-predictions/ligue-2/",
  "https://projects.fivethirtyeight.com/soccer-predictions/champions-league/"
)

scrape_frem_kamp <- function(url) {
  data <- read_html(url) %>% html_nodes(xpath="//*[@class='games-container upcoming']")
  
  turn <- url %>% 
    str_replace("https://projects.fivethirtyeight.com/soccer-predictions/", "") %>% 
    str_replace("/", "")
  
  home <- data %>% 
    html_nodes(".match-top") %>% 
    html_nodes(".team") %>% 
    html_text(trim = T)
  
  away <- data %>% 
    html_nodes(".match-bottom") %>% 
    html_nodes(".team") %>% 
    html_text(trim = T)
  
  prob_1 <- data %>%  
    html_nodes(xpath="//*[@class='games-container upcoming']//*[@class='match-top']/*[@class='prob']") %>% 
    html_text(trim = T) %>% 
    str_replace_all("%", "") %>% 
    as.numeric() / 100
  
  prob_x <- data %>% 
    html_nodes(".tie-prob") %>% 
    html_text(trim = T) %>% 
    str_replace_all("%", "") %>% 
    as.numeric() / 100
  
  prob_2 <- data %>% 
    html_nodes(xpath="//*[@class='games-container upcoming']//*[@class='match-bottom']/*[@class='prob']") %>% 
    html_text(trim = T) %>% 
    str_replace_all("%", "") %>% 
    as.numeric() / 100
  
  dato <- data %>% 
    html_nodes(".date") %>% 
    html_text(trim = T)
  
  dato <- str_c(dato, ifelse(as.numeric(str_extract(dato, "\\d+")) < 7, "/2018", "/2017"))
  kamp_dato <- mdy(dato)

  frem_kamp <-tibble(
    dato = today(), 
    kamp_dato = kamp_dato,
    turnering = turn,
    home = home,
    away = away,
    prob_1 = prob_1,
    prob_x = prob_x,
    prob_2 = prob_2,
    prob_max = pmax(prob_1, prob_2, prob_x, na.rm = T),
    prob_inv = 1 / prob_max,
    prob_udfald = ifelse(prob_max == prob_1, "1", ifelse(prob_max == prob_x, "X", "2"))
  ) 
  return(frem_kamp)
}

pl <- scrape_frem_kamp(url_liste[1])
alle_kamp <- tibble(
) 
for (j in seq_along(url_liste)){
  kampe <- scrape_frem_kamp(url_liste[j])
  alle_kamp <- bind_rows(alle_kamp, kampe)
}

write_excel_csv(alle_kamp,"alle-fremtidige-kampe.csv")

#behandling
for (i in 1:7) {
  if (wday(today()+i) == 2) {
    mandag <- today()+i 
  } else {
    next
  }
}

winner <- alle_kamp %>% filter(as.Date(kamp_dato) < mandag) %>% filter(prob_max > 0.6) %>% arrange(-prob_max)
View(winner)
write_excel_csv(winner,"vinder-kampe.csv")

#f?rdig