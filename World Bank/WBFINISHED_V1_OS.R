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

#Standardisation and new column creation
Q83unit <- df$Q83
Q86Cunit <- (df$Q86C -1)/4
Q8Aunit <- (4 - df$Q8A)/4
Q8Bunit <- (4 - df$Q8B)/4
Q82Aunit <- (3 - df$Q82A)/3
#Q82B Let us suppose that you had to choose between being a [citizen of this country] and being a [member of respondent’s ethnic group]. Which of the following statements best expresses your feelings? This is coded between 1 and 5, with 5 indicating maximum cohesion. Minusing by 1, then dividing by 4, to make it binary. 
Q82Bunit <- (df$Q82B - 1)/4
#Q82C Do you feel comfortable: speaking your mother tongue in public? This is already a binary indicator, with the 1 indicating comfort/cohesion
Q82Cunit <- df$Q82C
#Q82D Do you feel comfortable: wearing your traditional or cultural dress in public? This is already a binary indicator, with the 1 indicating comfort/cohesion
Q82Dunit <- df$Q82D
#Q84B In the past year, how often, if ever, have you personally been treated unfairly by other [citizens] based on: Your Religion? This is coded from 0-3, with 0 indicating maximum cohesion
Q84Bunit <- (3 - df$Q84B)/3
#Q84C In the past year, how often, if ever, have you personally been treated unfairly by other [citizens] based on: Your Ethnicity? This is coded from 0-3, with 0 indicating maximum cohesion
Q84Cunit <- (3 - df$Q84C)/3
#Q86A For each of the following types of people, please tell me whether you would like having people from this group as neighbours, dislike it, or not care: People of a different religion? This is coded from 1-5, with 5 indicating most cohesive views.
Q86Aunit <- (df$Q86A-1)/4
#Q86B For each of the following types of people, please tell me whether you would like having people from this group as neighbours, dislike it, or not care: People of a different ethnicity? This is coded from 1-5, with 5 indicating most cohesive views.
Q86Bunit <- (df$Q86B-1)/4
#Q84A In the past year, how often, if ever, have you personally been treated unfairly by other [citizens] based on: Your economic status, that is, how rich or poor you are? This is coded from 0-3, with 0 reflecting the highest cohesion (never happened) and 3 reflecting the lowest (always)
Q84Aunit <- (3 - df$Q84A)/3
#Q5 How often, if ever, are people like you treated unfairly by the government based on your economic status, that is, how rich or poor you are? This is coded from 0-3, with 0 reflecting the highest cohesion (never happened) and 3 reflecting the lowest (always)
Q5unit <- (3-df$Q5)/3  
#Q41C How much do you trust each of the following, or haven’t you heard enough about them to say: The Electoral Commission? This is coded from 0-3, with 0 representing low trust, so no cohesion, 3 representing high cohesion/trust.
Q41Cunit <- df$Q41C/3
#Q41D How much do you trust each of the following, or haven’t you heard enough about them to say: Your [county assembly]? This is coded from 0-3, with 0 representing low trust, so no cohesion, 3 representing high cohesion/trust.
Q41Dunit <- df$Q41D/3
#Q41G How much do you trust each of the following, or haven’t you heard enough about them to say: The Police? This is coded from 0-3, with 0 representing low trust, so no cohesion, 3 representing high cohesion/trust.
Q41Gunit <- df$Q41G/3
#Q41I How much do you trust each of the following, or haven’t you heard enough about them to say: Courts of Law? This is coded from 0-3, with 0 representing low trust, so no cohesion, 3 representing high cohesion/trust.
Q41Iunit <- df$Q41I/3
#Q40E In your opinion, how often, in this country, are people treated unequally under the law? Coded from 0 (Never) to 3 (always). 0 represents highest cohesion, 3 represents lowest.
Q40Eunit <- (3 - df$Q40E)/3
units <- cbind(Q5unit, Q8Aunit, Q8Bunit, Q40Eunit, Q41Cunit, Q41Dunit, Q41Gunit, Q41Iunit, Q82Aunit, Q82Bunit, Q82Cunit, Q82Dunit, Q83unit, Q84Aunit, Q84Bunit, Q84Cunit, Q86Aunit, Q86Bunit, Q86Cunit)
df <- cbind(df, units)

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

# Function to flip columns with 0 and 1 values
flip_columns <- function(data, columns) {
  # Check if the columns exist in the data frame
  if (is.character(columns)) {
    missing_cols <- setdiff(columns, colnames(data))
    if (length(missing_cols) > 0) {
      stop(paste("The following columns are missing:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Iterate over the specified columns
  for (col in columns) {
    if (all(data[[col]] %in% c(0, 1), na.rm = TRUE)) {
      # Flip the 0 and 1 values
      data[[col]] <- 1 - data[[col]]
    } else {
      warning(paste("Column", col, "contains values outside 0 and 1. Skipping flip."))
    }
  }
  
  return(data)
}

# --- Index Intra ---
# Variables for index_intra
vars_intra <- c("Q83unit", "Q86Cunit", "Q8Aunit", "Q8Bunit")
X_intra <- df[, vars_intra]
w_intra <- compute_inverse_cov_weights(X_intra)
print("Weights for index_intra:")
print(w_intra)
index_intra <- as.matrix(X_intra) %*% w_intra
df$index_intra <- index_intra

# --- Index Ethreg ---
vars_ethreg <- c("Q82Aunit", "Q82Bunit", "Q82Cunit", "Q82Dunit", "Q84Bunit", "Q84Cunit", "Q86Aunit", "Q86Bunit")
X_ethreg <- df[, vars_ethreg]
w_ethreg <- compute_inverse_cov_weights(X_ethreg)
print("Weights for index_ethreg:")
print(w_ethreg)
index_ethreg <- as.matrix(X_ethreg) %*% w_ethreg
df$index_ethreg <- index_ethreg

# --- Index Economic ---
vars_economic <- c("Q84Aunit", "Q5unit")
X_economic <- df[, vars_economic]
w_economic <- compute_inverse_cov_weights(X_economic)
print("Weights for index_economic:")
print(w_economic)
index_economic <- as.matrix(X_economic) %*% w_economic
df$index_economic <- index_economic

# --- Index Authority ---
vars_authority <- c("Q41Cunit", "Q41Dunit", "Q41Gunit", "Q41Iunit", "Q40Eunit")
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

df_naive <- df

###       Naive regression, for comparison with SOTA

# Model 1: Basic regression with Occurrence
m1basic <- lm(cohesion_index ~ Occurrence, 
         data = df_naive)

# Model 2: Bringing in Days since disaster
m2basic <- lm(cohesion_index ~ Occurrence + 
            Occurrence:DaysSinceDisaster, 
          data = df_naive)

m3basic <- lm(cohesion_index ~ Occurrence + 
           Occurrence:DaysSinceDisaster + 
           Occurrence:Log_Magnitude, 
         data = df_naive)

stargazer(m1basic,m2basic,m3basic, title="OLS Results", align=TRUE, out="regression1.txt")




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

# Model 6 with fixed effects, DSD, LogTA, and LogMag
m3felm <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)
stargazer(m1felm, m2felm, m3felm, title = "Linear model with fixed affects for Country and Year", align = TRUE, out="regression2.txt")


###       SUBGROUP ANALYSIS



#Q101 Gender - 1=Male, 2=Female
dfmen <- subset(df, df$Q101 == 1)

dfwomen <- subset(df, df$Q101 == 2)

#Getting overall, men, women impacts for index.

cindexoverall <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)
cindexmen <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = dfmen)
cindexwomen <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = dfwomen)
stargazer(cindexoverall, cindexmen, cindexwomen, title = "Fixed Effects Models using felm", align = TRUE, out="cindexgender.txt")

#Doing this for subindices

cintramen <- felm(index_intra ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfmen)
cintrawomen <- felm(index_intra ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfwomen)

cethregmen <- felm(index_ethreg ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfmen)
cethregwomen <- felm(index_ethreg ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfwomen)

ceconomicmen <- felm(index_economic ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfmen)
ceconomicwomen <- felm(index_economic ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfwomen)

cauthoritymen <- felm(index_authority ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfmen)
cauthoritywomen <- felm(index_authority ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfwomen)

stargazer(cintramen, cintrawomen, cethregmen, cethregwomen, title = "Fixed Effects Models using felm", align = TRUE, out="gendersubindices1.txt")
stargazer(ceconomicmen, ceconomicwomen, cauthoritymen, cauthoritywomen, title = "Fixed Effects Models using felm", align = TRUE, out="gendersubindices2.txt")

#Doing this for rep questions:



#Q97 Education - 0=No formal schooling, 1=Informal schooling only (including Koranic schooling), 2=Some primary schooling, 3=Primary school completed, 4=Intermediate school or some secondary school/high school, 5=Secondary school/high school completed, 6=Post-secondary qualifications, other than university, 7=Some university, 8=University completed, 9=Post-graduate, 98=Refused, 99=Don’t know, -1=Missing

dfleduc <- subset(df, df$Q97 < 5)


dfheduc <- subset(df, df$Q97 > 4)

#Getting overall, high and low education impacts for index.

cindexoverall <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)
cindexheduc <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = dfheduc)
cindexleduc <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = dfleduc)
stargazer(cindexoverall, cindexheduc, cindexleduc, title = "Fixed Effects Models using felm", align = TRUE, out="cindexeducation.txt")

#Doing this for subindices

cintraheduc <- felm(index_intra ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfheduc)
cintraleduc <- felm(index_intra ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfleduc)

cethregheduc <- felm(index_ethreg ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfheduc)
cethregleduc <- felm(index_ethreg ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfleduc)

ceconomicheduc <- felm(index_economic ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfheduc)
ceconomicleduc <- felm(index_economic ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfleduc)

cauthorityheduc <- felm(index_authority ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfheduc)
cauthorityleduc <- felm(index_authority ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfleduc)

stargazer(cintraheduc, cintraleduc, cethregheduc, cethregleduc, title = "Fixed Effects Models using felm", align = TRUE, out="educationsubindices1.txt")
stargazer(ceconomicheduc, ceconomicleduc, cauthorityheduc, cauthorityleduc, title = "Fixed Effects Models using felm", align = TRUE, out="educationsubindices2.txt")


#Also subgroup based on familial assets. Afrobarometer paper about doing this.
#Question Number: Q92B Question: Which of these things do you personally own? [If no, ask:] Does anyone else in your household own one: Television? 

dfhinc <- subset(df, df$Q92B %in% c(1,2))
dflinc <- subset(df, df$Q92B == 0)
cindexoverall <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)
cindexhinc <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = dfhinc)
cindexlinc <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = dflinc)
stargazer(cindexoverall, cindexhinc, cindexlinc, title = "Fixed Effects Models using felm", align = TRUE, out="cindexincome.txt")

#Doing this for subindices

cintrahinc <- felm(index_intra ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfhinc)
cintralinc <- felm(index_intra ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dflinc)

cethreghinc <- felm(index_ethreg ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfhinc)
cethreglinc <- felm(index_ethreg ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dflinc)

ceconomichinc <- felm(index_economic ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfhinc)
ceconomiclinc <- felm(index_economic ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dflinc)

cauthorityhinc <- felm(index_authority ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfhinc)
cauthoritylinc <- felm(index_authority ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dflinc)

stargazer(cintrahinc, cintralinc, cethreghinc, cethreglinc, title = "Fixed Effects Models using felm", align = TRUE, out="incomesubindices1.txt")
stargazer(ceconomichinc, ceconomiclinc, cauthorityhinc, cauthoritylinc, title = "Fixed Effects Models using felm", align = TRUE, out="incomesubindices2.txt")

#Coastal vs Riverine floods
#Looking at Disaster_Subtype
table(df$Disaster_Subtype)
#Only gives us 63 Riverine floods.



#Subgroups' questions. Not necessarily the question with the strongest correlation. Value judgment about which one captures something interesting to include. Have looked at including ones that vaguely follow the trend.

#Q8B Do you fear crime in your home?
#Q86B Would you like to have a neighbour from a different ethnic group?
#Q84A How often have you been treated unfairly based on economic status
#Q41G How much do you trust the police?

#OR should we focus more on the authority area? 

Q8Bunitmen <- felm(Q8Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfmen)
Q8Bunitwomen <- felm(Q8Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfwomen)

Q86Bunitmen <- felm(Q86Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfmen)
Q86Bunitwomen <- felm(Q86Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfwomen)

Q84Aunitmen <- felm(Q84Aunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfmen)
Q84Aunitwomen <- felm(Q84Aunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfwomen)

Q41Gunitmen <- felm(Q41Gunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfmen)
Q41Gunitwomen <- felm(Q41Gunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfwomen)

stargazer(Q8Bunitmen, Q8Bunitwomen, Q86Bunitmen, Q86Bunitwomen,  title = "Fixed Effects Models using felm", align = TRUE, out="genderrepqs1.txt")
stargazer(Q84Aunitmen, Q84Aunitwomen, Q41Gunitmen, Q41Gunitwomen, title = "Fixed Effects Models using felm", align = TRUE, out="genderrepqs2.txt")



#Education

Q8Bunitheduc <- felm(Q8Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfheduc)
Q8Bunitleduc <- felm(Q8Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfleduc)

Q86Bunitheduc <- felm(Q86Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfheduc)
Q86Bunitleduc <- felm(Q86Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfleduc)

Q84Aunitheduc <- felm(Q84Aunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfheduc)
Q84Aunitleduc <- felm(Q84Aunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfleduc)

Q41Gunitheduc <- felm(Q41Gunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfheduc)
Q41Gunitleduc <- felm(Q41Gunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfleduc)

stargazer(Q8Bunitheduc, Q8Bunitleduc, Q86Bunitheduc, Q86Bunitleduc,  title = "Fixed Effects Models using felm", align = TRUE, out="educrepqs1.txt")
stargazer(Q84Aunitheduc, Q84Aunitleduc, Q41Gunitheduc, Q41Gunitleduc, title = "Fixed Effects Models using felm", align = TRUE, out="educrepqs2.txt")

#Income

Q8Bunithinc <- felm(Q8Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfhinc)
Q8Bunitlinc <- felm(Q8Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dflinc)

Q86Bunithinc <- felm(Q86Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfhinc)
Q86Bunitlinc <- felm(Q86Bunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dflinc)

Q84Aunithinc <- felm(Q84Aunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfhinc)
Q84Aunitlinc <- felm(Q84Aunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dflinc)

Q41Gunithinc <- felm(Q41Gunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dfhinc)
Q41Gunitlinc <- felm(Q41Gunit ~ Occurrence + Occurrence:DaysSinceDisaster +  Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = dflinc)

stargazer(Q8Bunithinc, Q8Bunitlinc, Q86Bunithinc, Q86Bunitlinc,  title = "Fixed Effects Models using felm", align = TRUE, out="increpqs1.txt")
stargazer(Q84Aunithinc, Q84Aunitlinc, Q41Gunithinc, Q41Gunitlinc, title = "Fixed Effects Models using felm", align = TRUE, out="increpqs2.txt")

### Export
write_sav(df, "matched_indexed.sav")




### Visualisation


