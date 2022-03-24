#Upset Stats:
## Slow Pace (AdjT)
## Lots of Turnovers Forced (TOV%)
## High Reliance on 3-pointers (Points / (3*3PTM))
## Lots of Offensive Rebounds (ORB%)
## Low Luck (Luck)

## High Seed team has high Luck
## Slow Pace
## Lots of Turnovers
## High 3-pt Reliance

upset = read_csv("upset.csv") %>%
  filter(str_detect(School, "NCAA")) %>%
  mutate(School = str_trim(str_remove_all(School, "NCAA")),
         School = str_replace_all(School, "State", "ST."),
         School = toupper(School)) %>%
  filter(School %in% teams)


underdogs = kenpom_sep %>%
  mutate(`Tourney Seed` = as.numeric(`Tourney Seed`),
         Luck = as.numeric(Luck)) %>%
  filter(`Tourney Seed` > 9) %>%
  select(Rk, School, `Tourney Seed`, AdjO, AdjD, AdjT, Luck) %>%
  left_join(upset, by="School") %>%
  mutate(
    Comp_Score_dog = range01(rank(Pace) + rank(desc(`STL%`)) + rank(desc(`3PAr`)) + rank(desc(`ORB%`)) + rank(Luck))
  )

favorites = kenpom_sep %>%
  mutate(`Tourney Seed` = as.numeric(`Tourney Seed`),
         Luck = as.numeric(Luck)) %>%
  filter(`Tourney Seed` < 8) %>%
  select(Rk, School, `Tourney Seed`, AdjO, AdjD, AdjT, Luck) %>%
  left_join(upset, by="School") %>%
  mutate(
    Comp_Score_fav = range02(rank(Pace) + rank(desc(`TOV%`)) + rank(desc(`3PAr`)) + rank(`ORB%`) + rank(desc(AdjD)) + rank(desc(Luck)))
  )

games_upset = games_prob %>%
  left_join(select(favorites, c(School, Comp_Score_fav)), by=c("Team A" = "School")) %>%
  left_join(select(underdogs, c(School, Comp_Score_dog)), by=c("Team B" = "School")) %>%
  mutate(
    upset_diff = Comp_Score_dog - Comp_Score_fav
  )
