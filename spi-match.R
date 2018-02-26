library(tidyverse)
library(lubridate)
library(zoo)

soccer_spi <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv") %>% 
  mutate(
    probmax = pmax(prob1, prob2, probtie, na.rm = T),
    probres = if_else(probmax == prob1, "1", if_else(probmax == probtie, "X", "2")),
    season = if_else(date < dmy("01-06-17"), 1617, 1718)
  ) 

# Liganavne ----------------------------------------

league <- tribble(
  ~league, ~league_id,
  "All leagues", 1,
  "Champions League", 1818,
  "Europa League", 1820,
  "Austria: Bundesliga", 1827,
  "England: Premier League", 2411,
  "England: Championship", 2412,
  "France: Ligue 1", 1843,
  "France: Ligue 2", 1844,
  "Germany: Bundesliga", 1845,
  "Germany: 2. Bundesliga", 1846,
  "Italy: Serie A", 1854,
  "Italy: Serie B", 1856,
  "Netherlands: Eredivisie", 1849,
  "Norway: Eliteserien", 1859,
  "Portugal: Primeira Liga", 1864,
  "Russia: Premier League", 1866,
  "Scotland: Premiership", 2417,
  "Spain: La Liga", 1869,
  "Spain: La Liga 2", 1871,
  "Sweden: Allsvenskan", 1874,
  "Switzerland: Super League", 1879,
  "Turkey: Süper Lig", 1882,
  "Mexico: Liga MX 16/17", 1952,
  "Mexico: Liga MX 17/18", 1975,
  "USA: MLS", 1951,
  "USA: NWSL", 4582,
  "Argentina: Superliga", 5641,
  "Brazil: Brasileirão", 2105
  )

soccer_spi <- left_join(soccer_spi, league, "league_id") %>% 
  select(date, league_id, league, everything())


# Spillede kampe -------------------------------
match_compl <- soccer_spi %>% 
  filter(!is.na(score1)) %>% 
  mutate(
    res = if_else(score1 > score2, "1", if_else(score1 < score2, "2", "X")),
    corr = if_else(probres == res, 1, 0)
  )

ssh <- match_compl %>% 
  mutate(
    probround = floor(probmax*20)/20
  ) %>% 
  group_by(league_id, probround) %>% 
  summarise(
    corr = sum(corr),
    antal = n()
  ) %>% 
  arrange(league_id, -probround) %>% 
  mutate(
    andel = cumsum(corr) / cumsum(antal)
  ) %>% 
  ungroup() %>% 
  select(league_id, probround, andel) %>% 
  filter(probround >= 0.5) %>% 
  spread(key = probround, value = andel)


# Fremtidige kampe -------------------------------------
match_upcom <- soccer_spi %>% 
  filter(is.na(score1)) %>% 
  select(-starts_with("score"), -contains("xg"), -starts_with("adj")) %>% 
  arrange(league_id, date)

winner <- match_upcom %>%
  filter(date < ceiling_date(today(), "week", week_start = 1) & probmax >= 0.6) %>%
  arrange(-probmax) %>%
  select(-league_id) %>%
  View("Vinder kampe!")




# Adjusted point ----------------------------------------------------------

match_adj <- match_compl %>% 
  filter(league == "England: Premier League", season == 1718, probmax != probtie) %>% 
  mutate(
    date = date,
    team = if_else(probmax == prob1, team1, team2),
    opp = if_else(probmax == prob1, team2, team1),
    score_t = if_else(probmax == prob1, score1, score2),
    score_o = if_else(probmax == prob1, score2, score1),
    xg_t = if_else(probmax == prob1, xg1, xg2),
    xg_o = if_else(probmax == prob1, xg2, xg1)
  )


adj_xg <- match_compl %>% 
  filter(team1 == "Manchester United" | team2 == "Manchester United", 
         season == 1718,
         league_id == 2411
  ) %>% 
  arrange(date) %>% 
  transmute(
    date = date,
    team = "Manchester United",
    opp = if_else(team == team1, team2, team1),
    homeaway = if_else(team == team1, "home", "away"),
    team_g = if_else(team == team1, score1, score2),
    opp_g = if_else(team == team1, score2, score1),
    team_xg = if_else(team == team1, xg1, xg2),
    opp_xg = if_else(team == team1, xg2, xg1),
    team_diff = cumsum(team_g - team_xg),
    opp_diff = cumsum(opp_g - opp_xg)
  ) 


ggplot(adj_xg) +
  geom_step(aes(date, (team_g-team_xg)), size = 0.4, color = "steelblue") +
  geom_step(aes(date, (opp_g-opp_xg)), size = 0.4, color = "red") +
  theme_minimal()

ggplot(adj_xg, aes(date, ((team_g-team_xg)-(opp_xg-opp_g)))) +
  geom_step()
