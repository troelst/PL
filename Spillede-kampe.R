library(tidyverse)
library(rvest)
options(OutDec= ",")
url <- c(
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
turn <- c("PL",
          "ENG-CH",
          "BundesL",
          "BundesL2",
          "A-BL",
          "SerieA",
          "SerieB",
          "LaLiga",
          "Primeira Liga",
          "Ligue1",
          "Ligue2",
          "CHL")

kampe_df <- tibble(
  turnering = character(),
  home = character(),
  away = character(),
  home_score = integer(),
  away_score = integer(),
  res = character(),
  prob_1 = numeric(),
  prob_x = numeric(),
  prob_2 = numeric(),
  prob_max = numeric(),
  prob_res = character(),
  corr = integer() 
)

for (j in seq_along(url)){
	data_f <- read_html(url[j]) %>% html_nodes(xpath='//*[@class="games-container completed"]')
	tbl_f <- data_f %>% html_nodes(".match-container")
	turnering <- turn[j]

	for (i in seq_along(tbl_f)) {
		kamp <- tbl_f[i]
		home <- kamp %>% html_attr("data-team1")
		away <- kamp %>% html_attr("data-team2")
		score <- kamp %>% html_nodes(".score") %>% html_text(trim = T)
		home_score <- score[1]
		away_score <- score[2]
		res <- as.character(ifelse(home_score == away_score, "x", ifelse(home_score > away_score, 1,2)))
		prob <- kamp %>% html_nodes(".prob") %>% html_text(trim = T) %>% str_extract_all("\\d+", simplify = T) %>% as.numeric()
		
		prob_1 <- prob[1] / 100
		prob_x <- prob[2] / 100
		prob_2 <- prob[3] / 100
		
		prob_1[is.na(prob_1)] <- 0.001
		prob_x[is.na(prob_x)] <- 0.001
		prob_2[is.na(prob_2)] <- 0.001
		
		prob_max <- max(prob_1, prob_x, prob_2)
		prob_udfald <- as.character(ifelse(prob_x == prob_max, "x", ifelse(prob_1 == prob_max, 1,2)))
		corr <- as.numeric(ifelse(res == prob_udfald, 1,0))

		kampe_df <- add_row(kampe_df,
		                    turnering = turnering,
		                    home = home,
		                    away = away,
		                    home_score = home_score,
		                    away_score = away_score,
		                    res = res,
		                    prob_1 = prob_1,
		                    prob_x = prob_x,
		                    prob_2 = prob_2,
		                    prob_max = prob_max,
		                    prob_res = prob_udfald,
		                    corr = corr 
		                    )
	}
	print(paste("Færdig med", turnering, Sys.time()))
}

write_excel_csv(kampe_df,"allekampe.csv")

ssh <- c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)

for (h in seq_along(ssh)){
	korrekte <- sum(kampe_df$corr[kampe_df$prob_max >= ssh[h]])
	kampe <- length(kampe_df$home[kampe_df$prob_max >= ssh[h]])
	ssh_andel_korrekte <- korrekte / kampe
	for (k in seq_along(turn)){
		korrekte <- sum(kampe_df$corr[kampe_df$prob_max >= ssh[h] & kampe_df$turnering == turn[k]])
		kampe <- length(kampe_df$home[kampe_df$prob_max >= ssh[h] & kampe_df$turnering ==  turn[k]])
		turn_andel_korrekte = korrekte / kampe 
		ifelse(k==1,turn_v <- rbind.data.frame(ssh_andel_korrekte,turn_andel_korrekte), turn_v <- rbind.data.frame(turn_v,turn_andel_korrekte))
	}
	ifelse(h==1, ssh_v <-turn_v, ssh_v <- cbind.data.frame(ssh_v, turn_v))
}
colnames(ssh_v) <- ssh
rownames(ssh_v) <- c("alle",turn)

View(ssh_v)

#f?rdig