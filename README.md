# R Script (Data Validation and Preparing Winning Margin Analysis Table)

library(readxl)
library(dplyr)
library(janitor)
library(stringr)

pdf_file <- file.choose()

file_path <- file.choose()

# Display the available worksheets
excel_sheets(file_path)

raw_data <- read_excel(file_path, sheet = "all_records")

#check for duplicate records
sum(duplicated(constituency_data))

#where consistency occurs more than once
constituency_data %>%
  count(constituency, sort = TRUE) %>%
  filter(n > 1)

#check whether ballot casts exceed electorate 
constituency_data %>%
  filter(
    electorate < 0 |
    ballots_cast < 0 |
    ballots_cast > electorate
  )

# Preparing Winning Margin Table

library(dplyr)
library(tidyr)

# Convert candidate vote columns into a long-format table
candidate_long <- constituency_data %>%
  select(constituency, all_of(vote_columns)) %>%
  pivot_longer(
    cols = all_of(vote_columns),
    names_to = "Candidate",
    values_to = "Votes"
  )

#Rank candidates within each constituency
ranked_candidates <- candidate_long %>%
  group_by(constituency) %>%
  arrange(desc(Votes), .by_group = TRUE) %>%
  mutate(Rank = row_number()) %>%
  filter(Rank <= 2) %>%
  ungroup()

## What does this code do?
- pivot_longer() converts the candidate vote columns into rows, making it easier to compare candidates.
- group_by(constituency) makes R compare candidates separately within each constituency.
- arrange(desc(Votes)) places the candidate with the most votes first.
- row_number() assigns rank 1 to the highest vote total and rank 2 to the second-highest.
- filter(Rank <= 2) keeps only those two candidates.

 winning_margins <- ranked_candidates %>%
  mutate(
    Rank = if_else(Rank == 1, "Winner", "Runner_up")
  ) %>%
  pivot_wider(
    names_from = Rank,
    values_from = c(Candidate, Votes),
    names_glue = "{.value}_{Rank}"
  ) %>%
  mutate(
    Winning_Margin = Votes_Winner - Votes_Runner_up
  ) %>%
  select(
    constituency,
    Candidate_Winner,
    Votes_Winner,
    Candidate_Runner_up,
    Votes_Runner_up,
    Winning_Margin
  ) %>%
  arrange(Winning_Margin)

# Display the results
print(winning_margins)

write.csv(
  winning_margins,
  "gambia_2021_winning_margins.csv",
  row.names = FALSE
)
