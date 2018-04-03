library(tidyverse)
library(lubridate)
library(patchwork)
library(RcppRoll)

soccer_spi <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv") %>% 
  mutate(
    probmax = pmax(prob1, prob2, probtie, na.rm = T),
    probres = if_else(probmax == prob1, "1", if_else(probmax == probtie, "X", "2")),
    season = if_else(date < dmy("01-06-17"), 1617, 1718),
    res = if_else(score1 > score2, "1", if_else(score1 < score2, "2", "X")),
    corr = if_else(probres == res, 1, 0),
    point1 = if_else(res == 1, 3, if_else(res == "X", 1, 0)),
    point2 = if_else(res == 2, 3, if_else(res == "X", 1, 0))
  ) %>% 
  group_by(team1) %>% 
  mutate(
    streak_home = roll_sum(point1, 5, align = "right", fill = NA)
  ) %>% 
  fill(streak_home) %>% 
  ungroup() %>% 
  group_by(team2) %>% 
  mutate(
    streak_away = roll_sum(point2, 5, align = "right", fill = NA)
  ) %>% 
  fill(streak_away) %>% 
  ungroup()


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


# Sandsynlighedstest ------------------------------------------------------

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

# Vinderkampe -------------------------------------------------------------

winner <- match_upcom %>%
  filter(date < ceiling_date(today(), "week", week_start = 1) & probmax >= 0.6) %>%
  arrange(-probmax) %>%
  select(-league_id, -res, -corr, -season, -contains("point"))

View(winner)


# Adjusted point ----------------------------------------------------------

match_adj <- tibble(
  date = today(),
  team = character(),
  opp = character(),
  score_t = integer(),
  score_o = integer(),
  point_t = integer(),
  point_o = integer(),
  xg_t = numeric(),
  xg_o = numeric(),
  xg_point_t = integer(),
  xg_point_o = integer(),
  diff_t = integer(),
  diff_o = integer(),
  diff_score_t = numeric(),
  diff_score_o = numeric(),
  forskel = integer()     
)

for (j in seq_along(unique(match_compl$team1))) {
  team <- unique(match_compl$team1)[j]
  adj_data <- match_compl %>% 
    filter(team1 == team | team2 == team) %>% 
    transmute(
      date = date,
      league = league,
      season = season,
      team = if_else(team1 == team, team1, team2),
      opp = if_else(team1 == team, team2, team1),
      score_t = if_else(team1 == team, score1, score2),
      score_o = if_else(team1 == team, score2, score1),
      point_t = if_else(score_t > score_o, 3, if_else(score_t < score_o, 0, 1)),
      point_o = if_else(score_t < score_o, 3, if_else(score_t > score_o, 0, 1)),
      xg_t = if_else(probmax == prob1, xg1, xg2),
      xg_o = if_else(probmax == prob1, xg2, xg1),
      xg_point_t = if_else(xg_t > xg_o, 3, if_else(xg_t < xg_o, 0, 1)),
      xg_point_o = if_else(xg_t < xg_o, 3, if_else(xg_t > xg_o, 0, 1)),
      diff_t = point_t - xg_point_t,
      diff_o = point_o - xg_point_o,
      diff_score_t = score_t - xg_t,
      diff_score_o = score_o - xg_o
    )
  match_adj <- bind_rows(match_adj, adj_data)
}

ggplot(match_adj %>% filter(team == "Manchester United", date > dmy("01062017"), league == "England: Premier League") %>% arrange(date)) + 
  geom_step(aes(date, cumsum(point_t)), color = "steelblue") + 
  geom_step(aes(date, cumsum(xg_point_t)), color = "red")
  

adj_func <- function(team, leagueid) {
  adj_data <- match_compl %>% 
    filter(team1 == team | team2 == team) %>% 
    transmute(
      date = date,
      league_id = league_id,
      season = season,
      team = if_else(team1 == team, team1, team2),
      opp = if_else(team1 == team, team2, team1),
      score_t = if_else(team1 == team, score1, score2),
      score_o = if_else(team1 == team, score2, score1),
      point_t = if_else(score_t > score_o, 3, if_else(score_t < score_o, 0, 1)),
      point_o = if_else(score_t < score_o, 3, if_else(score_t > score_o, 0, 1)),
      xg_score_t = if_else(probmax == prob1, xg1, xg2),
      xg_score_o = if_else(probmax == prob1, xg2, xg1),
      xg_point_t = if_else(xg_score_t > xg_score_o, 3, if_else(xg_score_t < xg_score_o, 0, 1)),
      xg_point_o = if_else(xg_score_t < xg_score_o, 3, if_else(xg_score_t > xg_score_o, 0, 1)),
      diff_point_t = point_t - xg_point_t,
      diff_point_o = point_o - xg_point_o,
      diff_score_t = score_t - xg_score_t,
      diff_score_o = score_o - xg_score_o
    ) %>% 
    filter(season == 1718, league_id == leagueid) %>% 
    arrange(date)
  
  p_point <- ggplot(adj_data %>% 
                      select(date, point_t, xg_point_t) %>% 
                      arrange(date) %>% 
                      mutate_if(is.numeric, cumsum) %>% 
                      gather(contains("point"), key = "type", value = "point")
                    ) + 
    geom_step(aes(date, point, color = type, lty = type), size = 1) + 
    scale_color_brewer(palette = "Set1") + 
    theme_minimal(11) +
    theme(legend.position = "bottom") +
    labs(caption = "Samlet point", x = "Dato", y = "Kummuleret point")
  
  p_score_t <- ggplot(adj_data %>% 
                        select(date, score_t, xg_score_t) %>% 
                        arrange(date) %>% 
                        mutate_if(is.numeric, cumsum) %>% 
                        gather(contains("score"), key = "type", value = "score")
                      ) + 
    geom_step(aes(date, score, color = type, lty = type), size = 1) + 
    scale_color_brewer(palette = "Set1") +
    theme_minimal(9) +
    theme(legend.position = "None") +
    labs(caption = "Angreb", x = "Dato", y = "Kummuleret scoret mål")
  
  p_score_o <- ggplot(adj_data %>% 
                        select(date, score_o, xg_score_o) %>% 
                        arrange(date) %>% 
                        mutate_if(is.numeric, cumsum) %>% 
                        gather(contains("score"), key = "type", value = "score")
                      ) + 
    geom_step(aes(date, score, color = type, lty = type), size = 1) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal(9) +
    theme(legend.position = "None") +
    labs(caption = "Forsvar", x = "Dato", y = "Kummuleret indkasseret mål")
  
  (p_score_t | p_score_o ) / p_point

}

match_adj <- match_compl %>% 
  filter(league == "England: Premier League", season == 1718) %>% 
  filter(team1 == team | team2 == team) %>% 
  transmute(
    date = date,
    team = if_else(team1 == team, team1, team2),
    opp = if_else(team1 == team, team2, team1),
    score_t = if_else(team1 == team, score1, score2),
    score_o = if_else(team1 == team, score2, score1),
    point_t = if_else(score_t > score_o, 3, if_else(score_t < score_o, 0, 1)),
    point_o = if_else(score_t < score_o, 3, if_else(score_t > score_o, 0, 1)),
    xg_t = if_else(probmax == prob1, xg1, xg2),
    xg_o = if_else(probmax == prob1, xg2, xg1),
    xg_point_t = if_else(xg_t > xg_o, 3, if_else(xg_t < xg_o, 0, 1)),
    xg_point_o = if_else(xg_t < xg_o, 3, if_else(xg_t > xg_o, 0, 1)),
    diff_t = point_t - xg_point_t,
    diff_o = point_o - xg_point_o,
    diff_score_t = score_t - xg_t,
    diff_score_o = score_o - xg_o
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


# top 6 PL analyse --------------------------------------------------------

top6 <- c(
  "Manchester United",
  "Chelsea",
  "Arsenal",
  "Tottenham Hotspur",
  "Manchester City",
  "Liverpool"
)

match_top6_score <- match_compl %>% 
  filter(team1 %in% top6 & team2 %in% top6, season == 1718) %>% 
  select(team1, team2, score1, score2) %>% 
  unite("score", score1, score2, sep="-") %>% 
  spread(key = team2, value = score)

top6_point <- match_compl %>% 
  filter(team1 %in% top6 & team2 %in% top6, season == 1718) %>% 
  select(team1, team2, point1, point2)

home_top6_point <- top6_point %>% 
  select(team1, point1) %>% 
  group_by(team1) %>% 
  summarise(home_point = sum(point1), home_games = n())

away_top6_point <- top6_point %>% 
  select(team2, point2) %>% 
  group_by(team2) %>% 
  summarise(away_point = sum(point2), away_games = n())

top6point <- left_join(home_top6_point, away_top6_point, c("team1" = "team2")) %>% 
  mutate(sum_point = home_point + away_point)


# running point -----------------------------------------------------------

manudt_h <- soccer_spi %>% filter(team1 == "Manchester United") %>% arrange(league_id, date)
manudt_a <- soccer_spi %>% filter(team2 == "Manchester United") %>% arrange(league_id, date)
