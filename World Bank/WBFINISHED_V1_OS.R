library(haven)
library(stargazer)
library(MatchIt)
library(dplyr)
library(tidyr)
library(MASS)
library(lfe)

  ### DATA PREPREPERATION AND IMPORTATION

# Read the .sav file and create data frame - make sure to modify working directory.

setwd("/Users/swire/World Bank")
df <- read_sav("output_file.sav")

#filtering countries
#df <- df %>%
#filter(!(Country %in% c("ethiopia","south africa","mozambique","namibia","malawi","mauritius","zimbabwe","zimbabwe","botswana","zambia","uganda","lesotho","kenya","morocco","swaziland", "tanzania")))


# Rename variables to avoid conflicts
colnames(df)[colnames(df) == "Days_since_Disaster"] <- "DaysSinceDisaster"
colnames(df)[colnames(df) == "No__Affected"] <- "NoAffected"
colnames(df)[colnames(df) == "Total_Affected"] <- "TotalAffected"

# Ensure DATEINTR is in Date format
df$DATEINTR <- as.Date(df$DATEINTR)
# Extract the year
df$Year <- format(df$DATEINTR, "%Y")


# Replace missing values conditionally using updated names
df$DaysSinceDisaster[is.na(df$DaysSinceDisaster) & df$Occurrence == 0] <- 0
df$NoAffected[is.na(df$NoAffected) & df$Occurrence == 0] <- 0
df$TotalAffected[is.na(df$TotalAffected) & df$Occurrence == 0] <- 0
df$Magnitude[is.na(df$Magnitude) & df$Occurrence == 0] <- 0


# Create a new variable Log_TotalAffected to avoid log(0) issues
df$Log_TotalAffected <- log(df$TotalAffected + 1)
df$Log_Magnitude <- log(df$Magnitude + 1)

df = as.data.frame(df)

###             Index Creation

#Creating an index for social cohesion

#To start, let's clean the data and get rid of anyone who doesn't answer a question in a usable way:

#For all of them, there are 
# 8 = Refused
# 9 = Don't Know (in question 41, Don't Know/Haven't Heard)
# -1 = Missing
#For Q82s, we also have
# 7 = Not Applicable


df <- df[!(df$Q5 %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q8A %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q8B %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q40E %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q41C %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q41D %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q41G %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q41I %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q82A %in% c(7, 8, 9, -1, NA, 94)),] 
df <- df[!(df$Q82B %in% c(7, 8, 9, -1, NA, 94)),] 
df <- df[!(df$Q82C %in% c(7, 8, 9, -1, NA, 94)),]
df <- df[!(df$Q82D %in% c(7, 8, 9, -1, NA, 94)),] 
df <- df[!(df$Q83 %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q84A %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q84B %in% c(8, 9, -1, NA, 94)),]
df <- df[!(df$Q84C %in% c(8, 9, -1, NA, 94)),]
df <- df[!(df$Q86A %in% c(8, 9, -1, NA, 94)),]
df <- df[!(df$Q86B %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q86C %in% c(8, 9, -1, NA, 94)),] 


standardise_survey <- function(data, columns) {
  # Validate inputs
  if (!all(columns %in% colnames(data))) {
    stop("Some columns are not valid column names.")
  }
  
  # Print min and max for each column
  for (col in columns) {
    cat(sprintf("Column: %s, Min: %f, Max: %f\n", col, 
                min(data[[col]], na.rm = TRUE), 
                max(data[[col]], na.rm = TRUE)))
  }
  
  # Apply standardisation to each specified column
  data[columns] <- lapply(data[columns], function(col) {
    (col - min(col, na.rm = TRUE)) / (max(col, na.rm = TRUE) - min(col, na.rm = TRUE))
  })
  
  return(data)
}

columns_to_standardise <- c("Q5","Q8A","Q8B","Q40E","Q41C","Q41D","Q41G","Q41I","Q82A","Q82B","Q82C","Q82D","Q83","Q84A","Q84B","Q84C","Q86A","Q86B","Q86C")

# Standardise the data
df <- standardise_survey(df, columns_to_standardise)


# Function to compute inverse covariance weights
compute_inverse_cov_weights <- function(X) {
  # Compute the covariance matrix
  S <- cov(X, use = "complete.obs")
  
  # Invert the covariance matrix
  S_inv <- solve(S)
  
  # Create a vector of ones
  ones <- rep(1, ncol(X))
  
  # Compute the weights
  w_num <- S_inv %*% ones
  w_den <- as.numeric(t(ones) %*% S_inv %*% ones)
  w <- as.numeric(w_num / w_den)
  
  return(w)
}

# --- Index Intra ---
# Variables for index_intra
vars_intra <- c("Q83", "Q86C", "Q8A", "Q8B")

# Extract variables
X_intra <- df[, vars_intra]

# Compute weights
w_intra <- compute_inverse_cov_weights(X_intra)

# Print weights
print("Weights for index_intra:")
print(w_intra)

# Compute index_intra
index_intra <- as.matrix(X_intra) %*% w_intra
df$index_intra <- index_intra

# --- Index Ethreg ---
vars_ethreg <- c("Q82A", "Q82B", "Q82C", "Q82D", "Q84B", "Q84C", "Q86A", "Q86B")
X_ethreg <- df[, vars_ethreg]
w_ethreg <- compute_inverse_cov_weights(X_ethreg)
print("Weights for index_ethreg:")
print(w_ethreg)
index_ethreg <- as.matrix(X_ethreg) %*% w_ethreg
df$index_ethreg <- index_ethreg

# --- Index Economic ---
vars_economic <- c("Q84A", "Q5")
X_economic <- df[, vars_economic]
w_economic <- compute_inverse_cov_weights(X_economic)
print("Weights for index_economic:")
print(w_economic)
index_economic <- as.matrix(X_economic) %*% w_economic
df$index_economic <- index_economic

# --- Index Authority ---
vars_authority <- c("Q41C", "Q41D", "Q41G", "Q41I", "Q40E")
X_authority <- df[, vars_authority]
w_authority <- compute_inverse_cov_weights(X_authority)
print("Weights for index_authority:")
print(w_authority)
index_authority <- as.matrix(X_authority) %*% w_authority
df$index_authority <- index_authority

# --- Overall Cohesion Index ---
X_cohesion <- data.frame(index_intra, index_ethreg, index_economic, index_authority)
w_cohesion <- compute_inverse_cov_weights(X_cohesion)
print("Weights for cohesion_index:")
print(w_cohesion)
cohesion_index <- as.matrix(X_cohesion) %*% w_cohesion
df$cohesion_index <- cohesion_index



###       Naive regression, for comparison with SOTA

# Model 1: Basic regression with Occurrence
m1basic <- lm(cohesion_index ~ Occurrence, 
         data = df)

# Model 2: Bringing in Days since disaster
m2basic <- lm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster, data = df)

m3basic <- lm(cohesion_index ~ Occurrence + 
           Occurrence:DaysSinceDisaster + 
           Occurrence:Log_TotalAffected, 
         data = df)
m4basic <- lm(cohesion_index ~ Occurrence + 
           Occurrence:DaysSinceDisaster + 
           Occurrence:Log_TotalAffected + 
           Occurrence:Log_Magnitude, 
         data = df)

stargazer(m1basic,m2basic,m3basic,m4basic, title="OLS Results", align=TRUE, out="regression1.txt")




###       Matching

##    covariates
# education
covariate1 <- "Q97"
df <- subset(df, !(df[[covariate1]] %in% c(-1)))
# age
covariate2 <- "Q1"
df <- subset(df, !(df[[covariate2]] %in% c(-1,998,999)))
# gender
covariate3<- "Q101"

m.out <- matchit(
  Occurrence ~ Q97+Q1+Q101,  # Model for propensity score
  data = df,
  method = "nearest", # Nearest-neighbor matching
  distance = "mahalanobis", #Mahalanobis distance matching
  ratio = 3
)

# Check matching balance
summary(m.out)
df <- match.data(m.out)




###               Fixed Effects Linear Model

# Model 1 with fixed effects, occurrence only
m1felm <- felm(cohesion_index ~ Occurrence | Country + Year| 0 | Country, data = df)

# Model 2 with fixed effects, occurrence and days since disaster
m2felm <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster | Country + Year| 0 | Country, data = df)

# Model 3 with fixed effects, occurrence, DSD, and LogTA
m3felm <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected | Country + Year| 0 | Country, data = df)

# Model 6 with fixed effects, DSD, LogTA, and LogMag
m4felm <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)
stargazer(m1felm, m2felm, m3felm, m4felm, title = "Linear model with fixed affects for Country and Year", align = TRUE, out="regression2.txt")


### Export
#write_sav(df, "matched_indexed.sav")




### Visualisation


