library(tidyverse)
library(rvest)

url16 <- "https://en.wikipedia.org/wiki/2016-17_Premier_League"
url17 <- "https://en.wikipedia.org/wiki/2017-18_Premier_League"


# 2016-2017 ---------------------------------------------------------------

scrape_pl_url <- function(url) {
  data <- read_html(url) %>% html_nodes(".wikitable")
  tabel <- data[5] %>% html_table(trim = T)
  output <- tabel[[1]] %>% as_tibble()
  colnames(output) <- unlist(c("home", output[, 1]))
  output
  kamp <- output %>% 
    gather(-1, key = "away", value = "score") %>% 
    filter(away != home) %>% 
    arrange(home) %>% 
    mutate(
      score = ifelse(score == "", NA, score),
      score_home = str_extract_all(score, "^\\d+", simplify = T),
      score_away = str_extract_all(score, "\\d+$", simplify = T),
      res = ifelse(!is.na(score), ifelse(score_home == score_away, "X", ifelse(score_home > score_away,1,2)), NA),
      point_home = if_else(res == "X", 1, if_else(res == "1",3,0)),
      point_away = if_else(res == "X", 1, if_else(res == "1",0,3))
    ) 
  
  return(kamp)
}

kamp16 <- scrape_pl_url(url16)
kamp17 <- scrape_pl_url(url17)


# Omdøb oprykkere/nedrykkere ----------------------------------------------

kamp16$home[kamp16$home == "Hull City"] <- "Brighton & Hove Albion/Hull City"
kamp16$home[kamp16$home == "Middlesbrough"] <- "Newcastle United/Middlesbrough"
kamp16$home[kamp16$home == "Sunderland"] <- "Huddersfield Town/Sunderland"
kamp16$away[kamp16$away == "Hull City"] <- "Brighton & Hove Albion/Hull City"
kamp16$away[kamp16$away == "Middlesbrough"] <- "Newcastle United/Middlesbrough"
kamp16$away[kamp16$away == "Sunderland"] <- "Huddersfield Town/Sunderland"

kamp17$home[kamp17$home == "Brighton & Hove Albion"] <- "Brighton & Hove Albion/Hull City"
kamp17$home[kamp17$home == "Newcastle United"] <- "Newcastle United/Middlesbrough"
kamp17$home[kamp17$home == "Huddersfield Town"] <- "Huddersfield Town/Sunderland"
kamp17$away[kamp17$away == "Brighton & Hove Albion"] <- "Brighton & Hove Albion/Hull City"
kamp17$away[kamp17$away == "Newcastle United"] <- "Newcastle United/Middlesbrough"
kamp17$away[kamp17$away == "Huddersfield Town"] <- "Huddersfield Town/Sunderland"


# Saml resultater ---------------------------------------------------------

samlet_kamp <- full_join(kamp17, kamp16, by = c("home", "away"), suffix = c("17", "16")) %>% 
  mutate(
    diff_home = point_home17 - point_home16,
    diff_away = point_away17 - point_away16
  )


# Maksimal point givet sidste sæsons resultater for uspillede kampe -------

maxpoint <- bind_cols(
  samlet_kamp %>% 
    mutate(point_home17 = ifelse(is.na(point_home17), point_home16, point_home17)) %>% 
    group_by(home) %>% 
    summarise(max_home = sum(point_home17)),
  samlet_kamp %>% 
    mutate(point_away17 = ifelse(is.na(point_away17), point_away16, point_away17)) %>% 
    group_by(away) %>% 
    summarise(max_away = sum(point_away17)) %>% 
    select(max_away)
  ) %>% mutate(total = max_away + max_home) %>% 
  arrange(-total)


# Forbedring ift. sidste sæson --------------------------------------------

forbedring <- tibble(
  hold = 1:20, 
  hjemme_p = 1:20,
  hjemme_m = 1:20, 
  ude_p = 1:20, 
  ude_m = 1:20, 
  samlet_p = 1:20,
  samlet_m = 1:20,
  samlet = 1:20
  )

for (j in seq_along(unique(samlet_kamp$home))) {
  forbedring$hold[j] <- unique(samlet_kamp$home)[j]
  forbedring$hjemme_p[j] <- sum(samlet_kamp$diff_home[samlet_kamp$home == forbedring$hold[j] & samlet_kamp$diff_home > 0], na.rm = T)
  forbedring$hjemme_m[j] <- sum(samlet_kamp$diff_home[samlet_kamp$home == forbedring$hold[j] & samlet_kamp$diff_home < 0], na.rm = T)
  forbedring$ude_p[j] <- sum(samlet_kamp$diff_away[samlet_kamp$away == forbedring$hold[j] & samlet_kamp$diff_away > 0], na.rm = T)
  forbedring$ude_m[j] <- sum(samlet_kamp$diff_away[samlet_kamp$away == forbedring$hold[j] & samlet_kamp$diff_away < 0], na.rm = T) 
  forbedring$samlet_p[j] <-  forbedring$hjemme_p[j] + forbedring$ude_p[j]
  forbedring$samlet_m[j] <- forbedring$hjemme_m[j] + forbedring$ude_m[j]
  forbedring$samlet[j] <- forbedring$samlet_p[j] + forbedring$samlet_m[j]
} 

forbedring <- forbedring %>% arrange(-samlet) 


# Forbedring ift. hver enkelt kamp ----------------------------------------

forb_total <- tibble(
  team = character(),
  opp = character(),
  home_away = character(),
  score17 = character(),
  score16 = character(),
  point17 = integer(),
  point16 = integer(),
  forskel = integer()     
)

for (j in seq_along(unique(samlet_kamp$home))) {
  hold <- unique(samlet_kamp$home)[j]
  hold_data <- samlet_kamp %>% filter(home  == hold | away == hold)
  
  forb_total <- add_row(forb_total,
    team = hold,
    opp = ifelse(hold_data$away == hold,  hold_data$home, hold_data$away),
    home_away = ifelse(hold_data$away == hold,  "away", "home"),
    score17 = hold_data$score17,
    score16 = hold_data$score16,
    point17 = ifelse(hold_data$home == hold, hold_data$point_home17, hold_data$point_away17),
    point16 = ifelse(hold_data$home == hold, hold_data$point_home16, hold_data$point_away16),
    forskel = point17 - point16
    )
}           
