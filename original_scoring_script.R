
library(dplyr)


# Script to automate collating poster judges scores:

# Input data:
# Update this to wherever your copy of the file is stored.
df<-read.csv("~/PATH_TO_REPONSES.csv")

# Sanity checks

df %>%
  select( 2:8) %>%
  head()

df %>%
  select(4) %>%
  head()

# Rename headers as annoyingly long

df2<-df %>%
  rename( email = 2, overall_qual = 4, aim = 5, approach = 6, data_analysis = 7, discussion = 8, career_stage = 9) %>%
  select( -Timestamp, -10)

# Sum the total scores for each poster number.
# Add column counting the number of reviewers:

df2<-df2 %>%
  mutate( total_score = overall_qual + aim + approach + data_analysis + discussion)


### Summarise with number of scores and average:

poster_summary <- df2 %>%
  group_by(Poster.Number, career_stage) %>%
  summarise(
    n_judges = n_distinct(email[!is.na(email)]),
    mean_score = mean(total_score, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    flag = if_else(n_judges < 2, "CHECK - fewer than 2 judges", "OK")
  ) %>%
  arrange(n_judges, Poster.Number)

poster_summary


###### Now pick the winners:

winners<-poster_summary %>%
  arrange( career_stage, -mean_score)












