df_clean <- readRDS("df_clean.rds")

# FACTOR ANALYSIS

library(psych)
library(corrplot)
library(FactoMineR)

# Select the core numerical metrics that capture player performance
fa_vars <- c("MP", "PTS", "AST", "TRB", "STL", "BLK", "TOV", "3PA", "FTA")

# Scale the data 
nba_fa <- df_clean[, fa_vars]
nba_fa_scaled <- scale(nba_fa)

# Check the correlation 
cor_matrix_fa <- cor(nba_fa_scaled, use = "complete.obs")
corrplot(cor_matrix_fa, method = "color", type = "upper", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, number.cex = 0.7, 
         title = "Correlation Matrix for Factor Analysis", mar = c(0, 0, 1, 0))

# Test that variables are a good fit for Factor Analysis
# Bartlett's test - we want a significant p-value (< 0.05) to reject H0 
cortest.bartlett(cor_matrix_fa, n = nrow(nba_fa_scaled))

# KMO test, ideally above 0.60 or 0.70
KMO(cor_matrix_fa)

# Run the Factor Analysis 
nba_fa_results <- PCA(nba_fa, scale.unit = TRUE, graph = FALSE)

# Check eigenvalues to see how much each dimension explains
print(nba_fa_results$eig)

# Compute the Scree Plot
plot(nba_fa_results$eig[, 1], type = "o", main = "Scree Plot", 
     xlab = "Component / Factor", ylab = "Eigenvalue", col = "blue", pch = 19)

# Variables Factor Map to see which variables group together
plot(nba_fa_results, choix = "var", title = "Variables Factor Map")

# Check the actual factor loadings to see how strongly each stat correlates with the dimensions
print(nba_fa_results$var$cor[, 1:3])

# Extract individual player coordinates for our dimensions
player_scores <- nba_fa_results$ind$coord
head(player_scores[, 1:2])

# Check communalities to see how variables are represented and contributions
print(nba_fa_results$var$cos2[, 1:2])
apply(nba_fa_results$var$cos2[, 1:2], 1, sum) 

print(nba_fa_results$var$contrib[, 1:2])
apply(nba_fa_results$var$contrib[, 1:2], 2, sum) 

# Plot the individuals
plot(nba_fa_results, choix = "ind", title = "Player Factor Map")

# Apply a new index based on the first 2 dimensions
df_clean$Performance_Index <- apply(nba_fa_results$ind$coord[, 1:2], 1, sum)

# Check which are the first 3 players
sort(df_clean$Performance_Index, decreasing = TRUE)[1:3]


# Adding Supplementary Variables - Pos and Age

fa_vars_extended <- c("Pos", "Age", "MP", "PTS", "AST", "TRB", "STL", "BLK", "TOV", "3PA", "FTA")
nba_fa_ext <- df_clean[, fa_vars_extended]

# Run FA again, treating Position as qualitative supplementary and Age as quantitative
nba_fa_final <- PCA(nba_fa_ext, scale.unit = TRUE, 
                    quali.sup = 1, quanti.sup = 2, 
                    graph = FALSE)

# Plot variables map with Age
plot(nba_fa_final, choix = "var", title = "Variables Factor Map (with Age)")

# Plot player mapcoded by their position
plot(nba_fa_final, choix = "ind", habillage = 1, label = "none", 
     title = "Player Factor Map by Position")


