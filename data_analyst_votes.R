#' An R script to visualize votes from NW Study group's voting on Bible study to
#' be lead by Pastor Kim. Voting was a modification of the truncated Borda count
#' (https://en.wikipedia.org/wiki/Borda_count#Truncated_ballots). Voting was
#' conducted on 05-July-2022, by passing out ballots with all options and having
#' folks write: '3' for their top choice, '2' for their second choice, & '1' for
#' their third choice, leaving the remaining choices unmarked. Charley and
#' Heather Hall recorded the votes each candidate received.
#'
#' This script sums those ballots and displays graphs to show results as well as
#' breaks down how many 1st, 2nd, and 3rd choices each candidate received.
#'
#' Analysis Christopher Hall 09-July-2022

# Libraries
library(tidyverse)

# Raw data 
votes <- tibble(abraham   = c(3, 3, 2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                enc_jesus = c(3, 2, 1, 1, 3, 3, 2, 2, 3,NA,NA,NA,NA),
                jonah     = c(2, 3, 3, 1,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                psalm_119 = c(2, 1, 3, 3, 2,NA,NA,NA,NA,NA,NA,NA,NA),
                ii_peter  = c(3, 1, 2, 2, 1, 3, 3,NA,NA,NA,NA,NA,NA),
                david     = c(2, 1, 1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                phil      = c(1, 3, 1, 2,NA,NA,NA,NA,NA,NA,NA,NA,NA), 
                parables  = c(1, 2, 3, 2, 3, 3, 1, 2, 1, 1, 3, 2, 1))

# Readable study names
study_name = c("Stories from Abraham",
               "Encounters with Jesus",
               "Book of Jonah",
               "Psalm 119",
               "2 Peter",
               "Life of David",
               "Phillipians",
               "Parables of Christ")

## Creates tibbles (tables) for graphs
# Vote sums
results = tibble(
  study_name,
  weighted_votes = as_vector(summarise_all(votes, sum, na.rm = TRUE)[1,])
)

# Breakdown of 1st, 2nd, & 3rd places
choices = tibble(
  study_name,
  `# 1 choice` = as_vector(summarise_all(votes, list(~sum(.x == 3, na.rm = T)))),
  `# 2 choice` = as_vector(summarise_all(votes, list(~sum(.x == 2, na.rm = T)))),
  `# 3 choice` = as_vector(summarise_all(votes, list(~sum(.x == 1, na.rm = T))))
)

# pivot table wide to long for graph
choices_tall = pivot_longer(choices,
                            !study_name, 
                            names_to="ranked_choice", 
                            values_to="count")

# Plot results
results_graph <- ggplot(results,
                        aes(x = study_name, y = weighted_votes)) + 
  geom_col(fill = 'darkviolet') +
  geom_text(aes(label = weighted_votes), 
            vjust = -0.3, 
            color = 'darkviolet',
            size = 6) +
  labs(title = 'Vote Results',
       x = 'Study name',
       y = 'Votes (sum)') +
  theme(plot.title  = element_text(hjust = 0.5, size = 18),
        panel.grid.major.x = element_blank(),
        axis.title  = element_text(size = 16),
        axis.text   = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Plot choices
choices_graph <- ggplot(choices_tall,
                        aes(x = study_name, y = count, fill = ranked_choice,)) +
  geom_col() +
  scale_fill_manual(values = c('darkviolet', 'darkcyan', 'darkslateblue'))  +
  labs(title = "Individual Choices for Studies",
       x = "Study name",
       y = '# of Ranked choices',
       fill = 'Choices') +
  theme(plot.title  = element_text(hjust = 0.5, size = 18), 
        panel.grid.major.x = element_blank(),
        axis.title  = element_text(size = 16),
        axis.text   = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_y_continuous(minor_breaks = scales::breaks_extended(n = 15))

choices_graph_numbers = choices_graph +
  geom_text(aes(label = ifelse(count > 0, count, '')),
            position = position_stack(vjust = 0.5),
            size = 6,
            color = 'white') +
  stat_summary(fun = sum,
               aes(label = ..y.., group = study_name),
               geom = 'text',
               size = 7,
               vjust = -0.5)
