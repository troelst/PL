library(tidyverse)
library(rvest)

url16 <- "https://en.wikipedia.org/wiki/2016-17_Premier_League"
url17 <- "https://en.wikipedia.org/wiki/2017-18_Premier_League"

# 2016-2017
scrape_pl_url <- function(url) {
  data <- read_html(url) %>% html_nodes(".wikitable")
  tabel <- data[5] %>% html_table(trim = T)
  output <- tabel[[1]] %>% as_tibble()
  colnames(output) <- unlist(c("home", output[, 1]))
  kamp <- output %>% 
    gather(-1, key = "away", value = "score") %>% 
    filter(away != home) %>% 
    arrange(home) %>% 
    as_tibble()
  
  score <- kamp$score %>% 
    str_extract_all("\\d+", simplify = T) %>% 
    as_tibble() %>% 
    rename("score_home" = V1, "score_away" = V2)
  score[score == ""] <- NA
  
  kamp <- bind_cols(kamp, score)
  kamp$score[kamp$score == ""] <- NA
  
  kamp <- kamp %>% 
    mutate(
      res = ifelse(score != "", ifelse(score_home == score_away, "X", ifelse(score_home > score_away,1,2)), NA),
      point_home = ifelse(res == "X", 1, ifelse(res == "1",3,0)),
      point_away = ifelse(res == "X", 1, ifelse(res == "1",0,3))
    )
  return(kamp)
}

kamp16 <- scrape_pl_url(url16)
kamp17 <- scrape_pl_url(url17)


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

samlet_kamp <- full_join(kamp17, kamp16, by = c("home", "away"), suffix = c("17", "16"))

sammepoint <- samlet_kamp %>% 
  mutate(
    point_home17 = ifelse(is.na(point_home17), point_home16, point_home17), 
    point_away17 = ifelse(is.na(point_away17), point_away16, point_away17)
    )

maxpoint <- tibble(hold = 1:20, home = 1:20, away = 1:20, total = 1:20)
for (n in seq_len(length(unique(samlet_kamp$home)))) {
  hold = unique(samlet_kamp$home)[n]
  maxpoint[n,1] = hold
  maxpoint[n,2] = sum(as.numeric(sammepoint$point_home17[sammepoint$home == hold]))
  maxpoint[n,3] = sum(as.numeric(sammepoint$point_away17[sammepoint$away == hold]))
  maxpoint[n,4] = maxpoint[n,2] + maxpoint[n,3]
}
maxpoint <- maxpoint %>% 
  arrange(total)



forskel <- samlet_kamp %>% 
  mutate(
    forskel_home = point_home17 - point_home16,
    forskel_away = point_away17 - point_away16)

forbedring <- tibble(
  hold = character(), 
  hjemme_p = integer(),
  hjemme_m = integer(), 
  ude_p = integer(), 
  ude_m = integer(), 
  samlet_p = integer(),
  samlet_m = integer(),
  samlet = integer()
  )

for (j in seq_len(length(unique(forskel$home)))) {
  for_hold <- unique(forskel$home)[j]
  for_data <- forskel %>% filter(point_home17 != "NA")
  sum_home_p <- sum(for_data$forskel_home[for_data$home == for_hold & for_data$forskel_home > 0])
  sum_home_m <- sum(for_data$forskel_home[for_data$home == for_hold & for_data$forskel_home < 0])
  sum_away_p <- sum(for_data$forskel_away[for_data$away == for_hold & for_data$forskel_away > 0])
  sum_away_m <- sum(for_data$forskel_away[for_data$away == for_hold & for_data$forskel_away < 0])
  forbedring <- add_row(forbedring,
    hold = for_hold, 
    hjemme_p = sum_home_p, 
    hjemme_m = sum_home_m, 
    ude_p = sum_away_p, 
    ude_m = sum_away_m, 
    samlet_p = sum_home_p + sum_away_p,
    samlet_m = sum_home_m + sum_away_m,
    samlet = samlet_p + samlet_m
    )
 }

forbedring <- forbedring %>% arrange(-samlet) 

allehold_samlet <- tibble(
  team = character(),
  modstander = character(),
  home_away = character(),
  score17 = character(),
  score16 = character(),
  point17 = integer(),
  point16 = integer(),
  forskel = integer()     
)

for (j in 1:20) {
  hold <- unique(forskel$home)[j]
  hold_data <- forskel %>% filter(home  == hold | away == hold)
  
  allehold_samlet <- add_row(allehold_samlet,
    team = hold,
    modstander = ifelse(hold_data$away == hold,  hold_data$home, hold_data$away),
    home_away = ifelse(hold_data$away == hold,  "away", "home"),
    score17 = hold_data$score17,
    score16 = hold_data$score16,
    point17 = ifelse(hold_data$home == hold, hold_data$point_home17, hold_data$point_away17),
    point16 = ifelse(hold_data$home == hold, hold_data$point_home16, hold_data$point_away16),
    forskel = point17 - point16
    )
}