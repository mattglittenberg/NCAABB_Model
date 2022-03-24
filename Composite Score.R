library(tidyverse)
library(rvest)

#Ken Pomeroy Ratings----
kenpom = read_html("https://kenpom.com/") %>%
  html_element("table") %>%
  html_table()

colnames(kenpom) = 1:21
kenpom = kenpom %>% 
  select(-c(7, 9, 11, 13, 15, 17, 19, 21))

kenpom[1,10] = "SOS AdjEM"
kenpom[1,13] = "NCSOS AdjEM"

colnames(kenpom) = kenpom[1,]
kenpom = kenpom[-1,]

kenpom_sep = kenpom %>%
  filter(Rk != "") %>%
  filter(Rk != "Rk") %>%
  mutate(Team = str_replace_all(Team, 
                                pattern = "(?<=\\D)\\s(?=\\d)",
                  replacement = ","),
         Rk = as.numeric(Rk),
         Team = toupper(Team)) %>%
  separate(col = Team,
           into = c("School", "Tourney Seed"),
           sep = ",") %>%
  filter(!is.na(`Tourney Seed`))

teams = kenpom_sep$School

kenpom_final = kenpom_sep %>%
  select(c(Rk, School)) %>%
  rename("KP_Rank" = "Rk")
#Sonny Moore Power Ratings----
moore = read_html("http://sonnymoorepowerratings.com/m-basket.htm") %>%
  html_nodes("b") %>% 
  html_text()

moore_split = moore[2] %>% 
  strsplit(split = "\r\n") %>%
  unlist()

moore_split = moore_split[-1]
moore_split = moore_split[-359]

moore_squish = str_squish(moore_split) %>%
  str_replace_all(pattern = "(?<=\\d)\\s(?=\\d)", replacement = ",") %>%
  str_replace_all(pattern = "(?<=\\d)\\s", replacement = ",") %>%
  str_replace_all(pattern = "\\s(?=\\d)", replacement = ",")

moore_tib = as.tibble(moore_squish) %>%
  separate(col = value, 
           into = c("Ranking", "School", "Wins", "Loses", "Ties", "SOS", "Rating"),
           sep = ",") %>%
  mutate(
    School = replace(School, 
                     School == "ST. MARY'S CA.", "SAINT MARY'S"),
    School = replace(School, 
                     School == "LOYOLA ILLINOIS", "LOYOLA CHICAGO"),
    School = replace(School, School == "SOUTHERN CAL", "USC"),
    School = replace(School, School == "ALABAMA BIRMINGHAM", "UAB"),
    School = replace(School, 
                     School == "MIAMI FLORIDA","MIAMI FL" ),
    School = replace(School, 
                     School == "TENNESSEE CHATTANOOGA",
                     "CHATTANOOGA"),
    School = replace(School, 
                     School == "ST. PETER'S", "SAINT PETER'S"),
    School = replace(School, 
                     School =="CAL STATE FULLERTON", 
                     "CAL ST. FULLERTON"),
    School = replace(School, 
                     School == "TEXAS A&M-CORPUS CHRISTI", 
                     "TEXAS A&M CORPUS CHRIS")
  ) %>%
  filter(School %in% teams)

moore_final = moore_tib %>%
  select(c(Ranking, School)) %>%
  rename("Moore_Rank" = "Ranking")
#Jeff Sagrin Ratings----
sagrin = read_table("C:/Users/mattg/Documents/sagrin2022.txt",
                    col_names = FALSE, na = c("|"))

sagrin2 = sagrin %>%
  unite('merged', 2:4) %>%
  mutate(merged = str_replace_all(merged, pattern = "=", replacement = ","),
         merged = str_remove_all(merged, pattern = "_(?=\\d)")) %>%
  separate(col = merged,
           into = c("school", "value"),
           sep = ",")

sagrin2[sagrin2 == ""] = NA
sagrin2[sagrin2 == "="] = NA

sagrin_list = vector("list", 358)
for (i in 1:358) {
  r = sagrin2 %>% 
    slice(i) %>% 
    unlist()
  
  sagrin_list[[i]] = na.omit(r)
}
sagrin_mat = matrix(nrow = 358, ncol = 19)
for (j in 1:length(sagrin_list)) {
  school = sagrin_list[[j]]
  for (k in 1:length(school)) {
    sagrin_mat[j, k] = sagrin_list[[j]][k]
  }
}

sagrin_tib = as_tibble(sagrin_mat) %>%
  mutate(V2 = str_remove_all(V2, "_(?!\\w)"),
         V2 = str_replace_all(V2, pattern = "_", 
                              replacement = " "),
         V6 = str_remove_all(V6, "\\("),
         V7 = str_remove_all(V7, "\\)"),
         V2 = toupper(V2)) %>%
  select(-c(V18, V19))


colnames(sagrin_tib) = c("Rank", "School", "Rating", "W", "L", 
                         "SOS", "SOS_Rank", "Top25_W", "Top25_L",
                         "Top50_W", "Top50_L", "Predictor",
                         "Predictor_Rank", "Golden_Mean", 
                         "Golden_Mean_Rank", "Recent", 
                         "Recent_Rank")

sagrin_tib = sagrin_tib %>%
  mutate(
    School = replace(School, 
                     School == "SAINT MARY'S-CAL.", 
                     "SAINT MARY'S"),
    School = replace(School, 
                     School == "LOYOLA-CHICAGO", 
                     "LOYOLA CHICAGO"),
    School = replace(School, 
                     School == "SOUTHERN CALIFORNIA",
                     "USC"),
    School = replace(School, 
                     School == "MIAMI-FLORIDA",
                     "MIAMI FL"),
    School = replace(School,
                     School == "CS FULLERTON", 
                     "CAL ST. FULLERTON"),
    School = replace(School, 
                     School == "TEXAS A&M-CORPUSCHRISTI",
                     "TEXAS A&M CORPUS CHRIS")
  ) %>%
  filter(School %in% teams)

sagrin_final = sagrin_tib %>%
  select(c(Rank, School)) %>%
  rename("Sagrin_Rank" = "Rank")
#Joel Sokols LRMC----
lrmc = read_html("https://www2.isye.gatech.edu/~jsokol/lrmc/") %>%
  html_element("table") %>%
  html_table()

colnames(lrmc) = c(1:31)
lrmc = lrmc %>%
  select(c(2,3))

colnames(lrmc) = lrmc[1,]

lrmc[lrmc == "LRMC Rank"] = NA
lrmc[lrmc == "Through games of 3/13/2022"] = NA

lrmc_tib = lrmc %>% 
  drop_na() %>%
  rename("School" = "Team") %>%
  mutate(
    School = str_replace_all(School, pattern = "_", 
                         replacement = " "),
    School = str_replace_all(School, pattern = "St", 
                           replacement = "ST."),
    School = toupper(School),
    School = replace(School, 
                     School == "ST. MARY'S CA", 
                     "SAINT MARY'S"),
    School = replace(School, 
                     School == "LOYOLA-CHICAGO", 
                     "LOYOLA CHICAGO"),
    School = replace(School, 
                     School == "S DAKOTA ST.", 
                     "SOUTH DAKOTA ST."),
    School = replace(School, 
                     School == "ST. PETER'S", 
                     "SAINT PETER'S"),
    School = replace(School, 
                     School =="CS FULLERTON", 
                     "CAL ST. FULLERTON"),
    School = replace(School, 
                     School =="TX SOUTHERN", 
                     "TEXAS SOUTHERN"),
    School = replace(School, 
                     School == "TAM C. CHRISTI", 
                     "TEXAS A&M CORPUS CHRIS")
  ) %>%
  filter(School %in% teams) %>%
  rename("LRMC_Rank" = "LRMC Rank")

#ESPN BPI----
pages = 1:8
urls = paste0("https://www.espn.com/mens-college-basketball/bpi/_/view/bpi/page/", pages)
bpi = read_html("https://www.espn.com/mens-college-basketball/bpi/_/view/bpi/") %>%
  html_elements("table")

bpi2 = html_table(bpi[[2]])
as_tibble(bpi2)

get_table <- function(url) {
  table_list = url %>%
    read_html() %>%
    html_elements("table") 
  bpi = html_table(table_list[[2]]) %>%
    as_tibble()
}

results <- lapply(urls, get_table)

bpi_tib = bind_rows(results)

bpi_tib = bpi_tib %>%
  mutate(
    TEAM = str_remove_all(TEAM, 
                          pattern = "(?<=[a-z])[A-Z][A-Z]"),
    TEAM = str_remove_all(TEAM, 
                          pattern = "(?<=[a-z])[A-Z][A-Z]"),
    TEAM = str_remove_all(TEAM, 
                          pattern = "(?<=[a-z])[A-Z]"),
    TEAM = str_remove_all(TEAM, 
                          pattern = "(?<=[A-Z])[A-Z][A-Z][A-Z]"),
    TEAM = replace(TEAM, TEAM == "UA", "UCLA"),
    TEAM = str_replace_all(TEAM, "State", "ST."),
    TEAM = ifelse(RK == 322, "NJIT", TEAM),
    TEAM = toupper(TEAM)) %>%
  rename("School" = "TEAM") %>%
  mutate(School = replace(School, 
                          School == "UCONN", 
                          "CONNECTICUT"),
         School = replace(School, 
                          School =="CSU FULLERTON", 
                          "CAL ST. FULLERTON"),
         School = replace(School, 
                          School == "MIAMI",
                          "MIAMI FL"),
         School = replace(School,
                          School == "TEXAS A&M-CCC",
                          "TEXAS A&M CORPUS CHRIS")
         ) %>%
  filter(School %in% teams)

bpi_final = bpi_tib %>%
  select(c(RK, School)) %>%
  rename("BPI_Rank" = "RK")
#ELO----
elo = read_csv("C:/Users/mattg/Documents/ELO2022.csv") %>%
  rename("School" = "TEAM") %>%
  mutate(School = toupper(School),
         ) %>%
  filter(School %in% teams)

elo_final = elo %>%
  select(c(RK, School)) %>%
  rename("ELO_Rank" = "RK")

#NumberFire----
nf = read_html("https://www.numberfire.com/ncaab/teams/power-rankings") %>%
  html_elements("table")

nf1 = html_table(nf[[1]])
nf2 = html_table(nf[[2]])


#S-Curve----
scurve = read_table("C:/Users/mattg/Documents/scurve2022.txt", col_names = FALSE) %>%
  select(c(X1, X2))

colnames(scurve) = c("Seed", "School")

scurve_tib = scurve %>%
  mutate(
    School = str_replace_all(School, pattern = "_", 
                             replacement = " "),
    School = str_replace_all(School, "State", "ST."),
    School = replace(School, 
                     School == "UConn", 
                     "CONNECTICUT"),
    School = replace(School, 
                     School == "Miami",
                     "MIAMI FL"),
    School = replace(School,
                     School == "Texas A&M-Corpus Christi",
                     "TEXAS A&M CORPUS CHRIS"),
    School = toupper(School)) %>%
  filter(School %in% teams)
#Preseason----
pre = read_html("https://www.espn.com/mens-college-basketball/rankings/_/week/1/year/2022/seasontype/2") %>%
  html_elements("table")

pre1 = html_table(pre[[1]]) %>% 
  select(c(RK, Team)) %>% 
  rename("AP_Rank" = "RK") %>%
  mutate(
    Team = str_remove_all(Team, 
                          pattern = "\\(\\d\\)"),
    Team = str_remove_all(Team, 
                          pattern = "\\(\\d\\d\\)")
  )
pre2 = html_table(pre[[2]]) %>% 
  select(c(RK, Team)) %>%
  rename("Coach_Rank" = "RK") %>%
  mutate(
    Team = str_remove_all(Team, pattern = "\\(\\d\\)"),
    Team = str_remove_all(Team, pattern = "\\(\\d\\d\\)"),
    Coach_Rank = ifelse(Team == "OSUOhio State", 17, Coach_Rank)
  )

pro_combo = full_join(pre1, pre2) %>%
  relocate(Team, .before = AP_Rank) %>%
  rename("TEAM" = "Team") %>%
  mutate(
    Rank = rowMeans(pro_combo[-1], na.rm = TRUE),
    TEAM = str_remove_all(TEAM, 
                          pattern = "[A-Z][A-Z][A-Z](?=[A-Z])"),
    TEAM = str_remove_all(TEAM, 
                          pattern = "[A-Z][A-Z](?=[A-Z])"),
    TEAM = str_remove_all(TEAM, 
                          pattern = "[A-Z](?=[A-Z])"),
    TEAM = replace(TEAM, TEAM == "A", "UCLA"),
    TEAM = toupper(TEAM)) %>%
  rename("School" = "TEAM") %>%
  mutate(School = replace(School, 
                          School == "CONN", 
                          "CONNECTICUT")
  ) %>%
  filter(School %in% teams)

sagrin21 = read_table("C:/Users/mattg/Documents/sagrin2021.txt",
                    col_names = FALSE, na = c("|"))

sagrin21_2 = sagrin21 %>%
  unite('merged', 2:4) %>%
  mutate(merged = str_replace_all(merged, pattern = "=", replacement = ","),
         merged = str_remove_all(merged, pattern = "_(?=\\d)")) %>%
  separate(col = merged,
           into = c("school", "value"),
           sep = ",")

sagrin21_2[sagrin21_2 == ""] = NA
sagrin21_2[sagrin21_2 == "="] = NA

sagrin_list2 = vector("list", 358)
for (i in 1:358) {
  r = sagrin21_2 %>% 
    slice(i) %>% 
    unlist()
  
  sagrin_list2[[i]] = na.omit(r)
}
sagrin_mat2 = matrix(nrow = 357, ncol = 19)
for (j in 1:length(sagrin_list2)) {
  school = sagrin_list2[[j]]
  for (k in 1:length(school)) {
    sagrin_mat2[j, k] = sagrin_list2[[j]][k]
  }
}

sagrin_tib2 = as_tibble(sagrin_mat2) %>%
  mutate(V2 = str_remove_all(V2, "_(?!\\w)"),
         V2 = str_replace_all(V2, pattern = "_", 
                              replacement = " "),
         V6 = str_remove_all(V6, "\\("),
         V7 = str_remove_all(V7, "\\)"),
         V2 = toupper(V2)) %>%
  select(-c(V18, V19))


colnames(sagrin_tib2) = c("Rank", "School", "Rating", "W", "L", 
                         "SOS", "SOS_Rank", "Top25_W", "Top25_L",
                         "Top50_W", "Top50_L", "Predictor",
                         "Predictor_Rank", "Golden_Mean", 
                         "Golden_Mean_Rank", "Recent", 
                         "Recent_Rank")

sagrin_tib2 = sagrin_tib2 %>%
  mutate(
    School = str_replace_all(School, pattern = "STATE", 
                             replacement = "ST."),
    School = replace(School, 
                     School == "SAINT MARY'S-CAL.", 
                     "SAINT MARY'S"),
    School = replace(School, 
                     School == "LOYOLA-CHICAGO", 
                     "LOYOLA CHICAGO"),
    School = replace(School, 
                     School == "SOUTHERN CALIFORNIA",
                     "USC"),
    School = replace(School, 
                     School == "MIAMI-FLORIDA",
                     "MIAMI FL"),
    School = replace(School,
                     School == "CS FULLERTON", 
                     "CAL ST. FULLERTON"),
    School = replace(School, 
                     School == "TEXAS A&M-CORPUSCHRISTI",
                     "TEXAS A&M CORPUS CHRIS")
  ) %>%
  filter(School %in% teams)

sagrin_pre = sagrin_tib2 %>%
  select(School, Golden_Mean_Rank) %>%
  mutate(
    Golden_Mean_Rank = as.numeric(Golden_Mean_Rank),
    Scale_Rank = (Golden_Mean_Rank - mean(Golden_Mean_Rank)) / max(Golden_Mean_Rank) - min(Golden_Mean_Rank)
  )

pre_final = sagrin_pre %>%
  left_join(pro_combo, by=c("School")) %>%
  summarise(
    School = School,
    Pre_Rank = ifelse(is.na(Rank), Golden_Mean_Rank, Rank)
  )
  
#Combined----
combined_tib = kenpom_final %>%
  left_join(moore_final, by="School") %>%
  left_join(sagrin_final, by="School") %>%
  left_join(lrmc_tib, by="School") %>%
  left_join(bpi_final, by="School") %>%
  left_join(elo_final, by="School") %>%
  left_join(scurve_tib, by="School") %>%
  left_join(pre_final, by="School") %>%
  relocate(KP_Rank, .after = School) %>%
  mutate(
    Moore_Rank = as.numeric(Moore_Rank),
    Sagrin_Rank = as.numeric(Sagrin_Rank),
    LRMC_Rank = as.numeric(LRMC_Rank),
    Pre_Rank = as.numeric(Pre_Rank),
    Comp_Score = (KP_Rank * 0.75) + (Moore_Rank * 0.75) + (Sagrin_Rank * 0.75) + (LRMC_Rank * 0.75) + (BPI_Rank * 0.75) + (ELO_Rank * 0.75) + (Seed * 0.25) + (Pre_Rank * 0.25),
    Comp_Rank = rank(Comp_Score),
    Comp_Score = range01(Comp_Score) * 100)


range01 <- function(x){(max(x)-x)/(max(x)-min(x))}
range02 <- function(x){(x-min(x))/(max(x)-min(x))}

team.value = function(x, n_max, n_min,
                      o_max, o_min){
  (n_max-n_min)/(o_max-o_min) * (x - o_max) + n_max}

team.value(x = 0.2, n_max = 10, n_min = 0, 
              o_max = 25, o_min = -0.1)
# ari = -1.03
# bay = -3.49
# uconn = 0.52
# GS = 1.1
# Zag = 0.15
# Hou = 1.48
# IL = 1.67
# IN = 0.65
# KN = 0.68
# Mich St = 1.04
# NC = 0.58
# Ohio st. = 2.46
# NCS = 1.47
# SF = 1.87
# USC = 0.8
# Tenn = 0.8
# TAM = 0.39
# UAB = 0.7
# Vand = 0.64
# WI = 0.12
# WY = 0.12

win.prob = function(teamA, teamB, vec=comp_score_vect) {
  if (teamA == teamB) {prob = 1.0}
  
  else{
    score_a = vec[teamA]
    score_b = vec[teamB]
    
    score_diff = score_a - score_b
    
    prob = 1.0 / (1.0 + 10^(-score_diff*(30.464/400)))
  }
  
  return(prob)
}

mat_names = list(as.matrix(combined_tib$School), as.matrix(combined_tib$School))
prob_matrix = matrix(ncol = 68, nrow = 68, dimnames = mat_names)
for (i in names(comp_score_vect)) {
  for(j in names(comp_score_vect)) {
    prob = win.prob(i,j, comp_score_vect)
    
    prob_matrix[i,j] = round(prob * 100, digits = 2)
  }
}

#Games----
bet = read_html("https://www.boydsbets.com/college-basketball-spread-to-moneyline-conversion/") %>%
  html_elements("table")

bet1 = html_table(bet[[1]]) %>%
  select(-c(Games, Wins, Losses)) %>%
  mutate(
    `Fav Win %` = as.numeric(str_remove_all(`Fav Win %`, 
                                            pattern = "%")),
    `Dog Win %` = as.numeric(str_remove_all(`Dog Win %`, 
                                            pattern = "%")),
    Line = as.numeric(Line))

games = read_csv("Rd64_Games.csv") 

games_prob = games %>%
  mutate(
    win_prob_A = round(win.prob(teamA = `Team A`, 
                                teamB = `Team B`) * 100, 
                       digits = 2), 
    win_prob_B = round(win.prob(teamA = `Team B`, 
                                teamB = `Team A`) * 100, 
                       digits = 2),
    fav_win_prob = ifelse(win_prob_A > win_prob_B, 
                          win_prob_A, win_prob_B))

line_vect = c()
ml_vect = c()
for (val in as.vector(games_prob$fav_win_prob)) {
  idx = which.max(-abs(val - bet1$`Fav Win %`))
  
  line_vect = append(line_vect, bet1[[idx, "Line"]])
  ml_vect = append(ml_vect, bet1[[idx, "Fav ML"]])
  
}


games_prob = cbind(games_prob, line_vect, ml_vect) %>%
  rename("Fav_ML" = "ml_vect") %>%
  rename("Line" = "line_vect") %>%
  mutate(Favorite = ifelse(win_prob_A > win_prob_B, 
                `Team A`, `Team B`), .after = win_prob_B)

test = games_prob %>%
  group_by(`Team A`, `Team B`) %>%
  mutate(
    winner = sample(c(`Team A`, `Team B`), size=1, 
                    prob = c(win_prob_A, win_prob_B)))
