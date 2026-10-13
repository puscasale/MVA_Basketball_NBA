
# MVA Project - NBA Player Statistics 2023/2024
# Part 1: Dataset Inspection

library(readr)

# 1. Dataset Inspection

df <- read_csv("NBA_2024_per_game(26-01-2024).csv")

dim(df)
str(df)
head(df)
summary(df)

# Original dataset: 575 observations, 29 variables, 533 unique players


# 2. Duplicate Handling

sum(duplicated(df$Player))

# Identify all observations belonging to repeated players
df[duplicated(df$Player) |
     duplicated(df$Player, fromLast = TRUE),
   c("Player", "Tm", "G", "PTS")]

# 21 players have multiple records (63 rows in total)

# Keep aggregated records (TOT) and players with no repeated observations
df_clean <- df[
  df$Tm == "TOT" |
    !(duplicated(df$Player) |
        duplicated(df$Player, fromLast = TRUE)),
]

# Validate the cleaned dataset
dim(df_clean)
anyDuplicated(df_clean$Player)
nrow(df_clean) == length(unique(df$Player))
setequal(df$Player, df_clean$Player)

# Result: 533 players, 29 variables


# 3. Missing Values Analysis

sum(is.na(df_clean))
colSums(is.na(df_clean))

# 131 missing values, all in shooting percentage variables

# Investigate missing three-point shooting percentages
df_clean[is.na(df_clean$`3P%`),
         c("Player", "3P", "3PA", "3P%")]

# Check missing percentages against shooting attempts
sum(is.na(df_clean$`FG%`) & df_clean$FGA != 0)
sum(is.na(df_clean$`3P%`) & df_clean$`3PA` != 0)
sum(is.na(df_clean$`2P%`) & df_clean$`2PA` != 0)
sum(is.na(df_clean$`eFG%`) & df_clean$FGA != 0)
sum(is.na(df_clean$`FT%`) & df_clean$FTA != 0)

# All checks returned 0: missing percentages occur with zero recorded attempts
# Keep NA values to distinguish undefined percentages from 0% shooting


# 4. Descriptive Statistics

summary(df_clean)

numeric_df <- df_clean[sapply(df_clean, is.numeric)]
sapply(numeric_df, sd, na.rm = TRUE)


# PTS, AST and TRB: mean > median, suggesting right-skewness
# MP: mean and median are relatively close


# 5. Distribution Analysis

# Points per game
hist(df_clean$PTS,
     main = "Distribution of Points per Game",
     xlab = "Points per Game",
     ylab = "Number of Players",
     col = "orange",
     border = "black",
     breaks = 20)

# PTS: right-skewed distribution, most players score below 10 PPG

boxplot(df_clean$PTS,
        main = "Boxplot of Points per Game",
        ylab = "Points per Game",
        col = "orange",
        border = "black")

# Identify potential outliers in points per game
Q1 <- quantile(df_clean$PTS, 0.25)
Q3 <- quantile(df_clean$PTS, 0.75)

IQR_pts <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR_pts

df_clean[df_clean$PTS > upper_bound,
         c("Player", "PTS")]

# 12 potential outliers above 26.5 PPG


# Minutes per game
hist(df_clean$MP,
     main = "Distribution of Minutes per Game",
     xlab = "Minutes per Game",
     ylab = "Number of Players",
     col = "orange",
     border = "black",
     breaks = 20)

# MP: widely spread distribution, without pronounced skewness


# Assists per game
hist(df_clean$AST,
     main = "Distribution of Assists per Game",
     xlab = "Assists per Game",
     ylab = "Number of Players",
     col = "orange",
     border = "black",
     breaks = 20)

# AST: strongly right-skewed, with a long upper tail


# Rebounds per game
hist(df_clean$TRB,
     main = "Distribution of Rebounds per Game",
     xlab = "Rebounds per Game",
     ylab = "Number of Players",
     col = "orange",
     border = "black",
     breaks = 20)

# TRB: right-skewed, most players have relatively few rebounds


# 6. Outlier Detection

boxplot(df_clean$AST,
        main = "Boxplot of Assists per Game",
        ylab = "Assists per Game",
        col = "orange")

boxplot(df_clean$TRB,
        main = "Boxplot of Rebounds per Game",
        ylab = "Rebounds per Game",
        col = "orange")

boxplot.stats(df_clean$AST)$out
boxplot.stats(df_clean$TRB)$out

# AST: 29 potential outliers
# TRB: 23 potential outliers

