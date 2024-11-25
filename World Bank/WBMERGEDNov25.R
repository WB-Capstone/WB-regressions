library(haven)
library(stargazer)
library(MatchIt)
library(dplyr)
library(tidyr)
library(MASS)
library(lfe)


# Read the .sav file and create data frame - make sure to modify working directory.

setwd("~/Documents/GV343/Empirics")
df <- read_sav("~/Documents/GV343/Empirics/output_file.sav")

#filtering countries
#df <- df %>%
  #filter(!(Country %in% c("ethiopia","south africa","mozambique","namibia","malawi","mauritius","zimbabwe","zimbabwe","botswana","zambia","uganda","lesotho","kenya","morocco","swaziland", "tanzania")))

##  outcome variables
outcome_variable <- "Q83"
df <- subset(df, Q83 == 0 | Q83 == 1 | Q83 == 2 | Q83 == 3 | Q83 == 4)

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


#In many questions - Q41D, Q82A, Q82B, Q84C - there was bias from '94' - this is not in the codebook, but reflects a question not being asked in that country.

#Sub-indices - consider, are the variables coded similarly? If so, we could use an additive measure. Then we'll scale them so that they're all between 0 and 1, for ease. Also look at the degree of correlation between variables


#index_intra will include:
#question 83 (Generally speaking, would you say that most people can be trusted or that you must be very careful in dealing with people?). coded with higher values indicating more trust. For 83, it is binary, already a unit interval, 
#question 86C (For each of the following types of people, please tell me whether you would like having people from this group as neighbours, dislike it, or not care: Homosexuals). This takes values 1-5, with 5 indicting the highest tolerance.
#question 8A Over the past year, how often, if ever, have you or anyone in your family: Felt unsafe walking in your neighbourhood?
#question 8B Over the past year, how often, if ever, have you or anyone in your family: Feared crime in your own home?
#Questions 8A and 8B are coded with 0 being 'never' feeling unsafe, 4 being 'always'. There are therefore 5 categories, with a higher score indicating a lower safety perception. Unusable values coded as 8, 9, -1

Q83unit <- df$Q83

Q86Cunit <- (df$Q86C -1)/4

Q8Aunit <- (4 - df$Q8A)/4

Q8Bunit <- (4 - df$Q8B)/4

#NOTE - there are some bad results for Q86C - getting 9.
index_intra <- (Q83unit + Q86Cunit + Q8Aunit + Q8Bunit)/4
#Divided by 3, as each question is from 0-1 already, so this should lead to an overall binary indicator.
df$index_intra <- index_intra

#index_ethnoreligious will include:
#Q68 not included as doesn't easily lend itself to an index (1-4 goes up in increasing strength of view, but 5 is 'doesn't agree with either')
#Q82A How often, if ever, are [members of respondent’s ethnic group/religious group] treated unfairly by the government? 0 indicates 'Never', 3 indicates 'always' - so there are 4 options, with higher value indicating less cohesion.
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


index_ethnoreligious <- (Q82Aunit + Q82Bunit + Q82Cunit + Q82Dunit + Q84Bunit + Q84Cunit +Q86Aunit + Q86Bunit)/8
df$index_ethnoreligious <- index_ethnoreligious

#index_economic will include:
#Q84A In the past year, how often, if ever, have you personally been treated unfairly by other [citizens] based on: Your economic status, that is, how rich or poor you are? This is coded from 0-3, with 0 reflecting the highest cohesion (never happened) and 3 reflecting the lowest (always)
Q84Aunit <- (3 - df$Q84A)/3
#Q5 How often, if ever, are people like you treated unfairly by the government based on your economic status, that is, how rich or poor you are? This is coded from 0-3, with 0 reflecting the highest cohesion (never happened) and 3 reflecting the lowest (always)
Q5unit <- (3-df$Q5)/3  

index_economic <- (Q84Aunit + Q5unit)/2
df$index_economic <- index_economic

#index_authority will include:
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

index_authority <- (Q41Cunit + Q41Dunit + Q41Gunit + Q41Iunit + Q40Eunit)/5
df$index_authority <- index_authority



#Overall Index

#Equally weighting these different elements of social cohesion - can we back this up theoretically? Look into weighting by covariance.

cohesion_index <- (index_intra + index_ethnoreligious + index_economic + index_authority)/4
df$cohesion_index <- cohesion_index
hist(df$cohesion_index)

units <- cbind(Q5unit, Q8Aunit, Q8Bunit, Q40Eunit, Q41Cunit, Q41Dunit, Q41Gunit, Q41Iunit, Q82Aunit, Q82Bunit, Q82Cunit, Q82Dunit, Q83unit, Q84Aunit, Q84Bunit, Q84Cunit, Q86Aunit, Q86Bunit, Q86Cunit)
df <- cbind(df, units)

###                       NAIVE

# Model 1: Basic logistic regression with Occurrence
m1 <- lm(cohesion_index ~ Occurrence, 
         data = df)

# Model 2
m2 <- lm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster, data = df)

m5 <- lm(cohesion_index ~ Occurrence + 
           Occurrence:DaysSinceDisaster + 
           Occurrence:Log_TotalAffected, 
         data = df)
m6 <- lm(cohesion_index ~ Occurrence + 
           Occurrence:DaysSinceDisaster + 
           Occurrence:Log_TotalAffected + 
           Occurrence:Log_Magnitude, 
         data = df)

stargazer(m1,m2,m5,m6, title="OLS Results", align=TRUE, out="regression1.txt")

###                 MATCHING

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

# Model 1 with fixed effects
m1 <- felm(cohesion_index ~ Occurrence | Country + Year| 0 | Country, data = df)

# Model 2 with fixed effects
m2 <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster | Country + Year| 0 | Country, data = df)

# Model 5 with fixed effects
m5 <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected | Country + Year| 0 | Country, data = df)

# Model 6 with fixed effects
m6 <- felm(cohesion_index ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)
stargazer(m1, m2, m5, m6, title = "Fixed Effects Models using felm", align = TRUE, out="regression2.txt")

write_sav(df, "matched_indexed.sav")

unique(df$Country)



#Look at fixed linear effects, for different questions: for intra index


cintra <- felm(index_intra ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)


c83 <- felm(Q83unit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)


c86C <- felm(Q86Cunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)


c8A <- felm(Q8Aunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c8B <- felm(Q8Bunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

stargazer(cintra, c83, c86C, c8A, c8B, title = "Fixed Effects Models using felm", align = TRUE, out="choosing_intra.txt")


#For ethnoreligious
cethnoreligious <- felm(index_ethnoreligious ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)

c82A <- felm(Q82Aunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)


c82B <- felm(Q82Bunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)


c82C <- felm(Q82Cunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)


c82D <- felm(Q82Dunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c84B <- felm(Q84Bunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c84C <- felm(Q84Cunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c86A <- felm(Q86Aunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c86B <- felm(Q86Bunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

stargazer(c82A, c82B, c82C, c82D, c84B, c84C, c86A, c86B, title = "Fixed Effects Models using felm", align = TRUE, out="choosing_ethnoreligious.txt")

#For economic

ceconomic <- felm(index_economic ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)

c84A <- felm(Q84Aunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c5 <- felm(Q5unit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

stargazer(ceconomic, c84A, c5, title = "Fixed Effects Models using felm", align = TRUE, out="choosing_economic.txt")

#For authority

cauthority <- felm(index_authority ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude| Country + Year| 0 | Country, data = df)

c41C <- felm(Q41Cunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c41D <- felm(Q41Dunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c41G <- felm(Q41Gunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c41I <- felm(Q41Iunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

c40E <- felm(Q40Eunit ~ Occurrence + Occurrence:DaysSinceDisaster + Occurrence:Log_TotalAffected + Occurrence:Log_Magnitude | Country + Year| 0 | Country, data = df)

stargazer(cauthority, c41C, c41D, c41G, c41I, c40E, title = "Fixed Effects Models using felm", align = TRUE, out="choosing_authority.txt")


#Select the key questions. overall index, 8B, 



