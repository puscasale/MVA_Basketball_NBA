
# MVA Project - NBA Player Statistics 2023/2024
# Part 1: Dataset Inspection

library(readr)

# 1. Dataset Inspection

df <- read_csv("NBA_2024_per_game(26-01-2024).csv")
df

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


###-----------------------------------------###
###-----------------------------------------###
###-----------------------------------------###
###-----------------------------------------###


#Correspondence Analysis: between ages and position
View(df_clean)

#1st step: convert ages to intervals (we know that ages are between 18-40)
df_clean$Age_Group <- cut(df_clean$Age,
                  c(17, 22, 29, 40),
                  c("Young", "Prime", "Veteran"),
                  include.lowest = TRUE)

pos <- which(names(df_clean) == "Age")
df_clean <- df_clean[, append(setdiff(names(df_clean), "Age_Group"), "Age_Group", after = pos)]

View(df_clean)


##2nd step: select the categorical variables we wanna create a relation with
#and create the table

tab_pos_age  <- table(df_clean$Pos,  df_clean$Age_Group)
tab_pos_team <- table(df_clean$Pos,  df_clean$Tm)
tab_age_team <- table(df_clean$Age_Group, df_clean$Tm)

tab_pos_age
contingency <- addmargins(tab_pos_age)

round(sweep(contingency,1,apply(contingency,1,sum),"/")*100,1)
round(sweep(contingency,2,apply(contingency,2,sum),"/")*100,1)

install.packages("FactoMineR")
library(FactoMineR)

?CA
res.ca=CA(contingency[1:10,1:3]) #dataset is the only compulsory parameter for the CA
res.ca

res.ca$eig #eigenvalues
res.ca$row$coord #coordinates
res.ca$row$contrib #contribution
res.ca$row$cos2 #cos2 <- how much is explaining each dimensions for each value

## Cloud of row profiles
?plot.CA
plot.CA(res.ca,axes = c(1,2)) # invisible="col", axes = c(1,3)) #you can switch the dimensions you wanna study

plot.CA(res.ca,invisible = c("col","col.sup"))
plot.CA(res.ca,invisible = c("row"))


###Correspondence Analysis: between ages and teams
tab_age_team <- table(df_clean$Tm, df_clean$Age_Group)

tab_pos_age
contingency <- addmargins(tab_pos_age)
contingency_tm <- addmargins(tab_age_team)


round(sweep(contingency_tm,1,apply(contingency,1,sum),"/")*100,1)
round(sweep(contingency_tm,2,apply(contingency,2,sum),"/")*100,1)

?CA
res.ca=CA(contingency_tm[1:31,1:3]) #dataset is the only compulsory parameter for the CA
res.ca

res.ca$eig #eigenvalues
res.ca$row$coord #coordinates
res.ca$row$contrib #contribution
res.ca$row$cos2 #cos2 <- how much is explaining each dimensions for each value


## Cloud of row profiles
?plot.CA
plot.CA(res.ca,axes = c(1,2)) # invisible="col", axes = c(1,3)) #you can switch the dimensions you wanna study



