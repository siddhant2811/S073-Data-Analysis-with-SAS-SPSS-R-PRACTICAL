library(dplyr)

# Load your PKL Season 12 team-level dataset
pkl <- read.csv("team_records.csv")

# ===============================================================
# Example 1: Sort by a Single Variable (Ascending)
# Sort teams by Total Points (Total_pts) – lowest to highest
# ===============================================================

pkl_sorted_totalpts <- pkl |>
  arrange(Total_pts)

head(pkl_sorted_totalpts, 5)

# ===============================================================
# Example 2: Sort by a Single Variable (Descending)
# Sort teams by Matches Won (Matches_won) – highest to lowest
# ===============================================================

pkl_sorted_matcheswon_desc <- pkl |>
  arrange(desc(Matches_won))

head(pkl_sorted_matcheswon_desc, 5)

# ===============================================================
# Example 3: Sorting by Multiple Columns
# Sort first by Matches Lost (ascending)
# Then within that, sort by Avg_pts (descending)
# ===============================================================

pkl_multi_sort <- pkl |>
  arrange(Matches_lost, desc(Avg_pts))

head(pkl_multi_sort, 10)

# ===============================================================
# Example 4: Combined Filter + Sort
# Filter teams with Successful Raids > 200
# Then sort by Successful Tackles (ascending)
# ===============================================================

strong_raiders_sorted <- pkl |>
  filter(Successful_raids > 200) |>
  arrange(Successful_tackles)

cat("Top raiding teams sorted by successful tackles:\n")

print(
  strong_raiders_sorted |>
    select(Team, Successful_raids, Successful_tackles, Total_pts) |>
    head(5)
)

