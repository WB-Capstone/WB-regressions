library(haven)
library(stargazer)
library(MatchIt)
library(dplyr)
library(tidyr)

# Read the .sav file

setwd("~/Documents/GV343/Empirics")
df <- read_sav("~/Documents/GV343/Empirics/output_file.sav")


# Rename variables to avoid conflicts
colnames(df)[colnames(df) == "Days_since_Disaster"] <- "DaysSinceDisaster"
colnames(df)[colnames(df) == "No__Affected"] <- "NoAffected"
colnames(df)[colnames(df) == "Total_Affected"] <- "TotalAffected"


# Replace missing values conditionally using updated names
df$DaysSinceDisaster[is.na(df$DaysSinceDisaster) & df$Occurrence == 0] <- 0
df$NoAffected[is.na(df$NoAffected) & df$Occurrence == 0] <- 0
df$TotalAffected[is.na(df$TotalAffected) & df$Occurrence == 0] <- 0
df$Magnitude[is.na(df$Magnitude) & df$Occurrence == 0] <- 0


df = as.data.frame(df)

m.out0 <- matchit(Occurrence ~ Q97, data = df,
                  method = NULL, distance = "glm")

#covariates
# education
covariate1 <- "Q97"
df <- subset(df, !(df[[covariate1]] %in% c(-1)))
covariate2 <- ""

# Step 3: Check the structure to confirm the changes
str(df)


summary(df)

# Example: Replace covariate1, covariate2, etc., with actual covariates in your data
matched_data <- matchit(
  Occurrence ~ Q97,  # Model for propensity score
  data = df,
  method = "nearest", # Nearest-neighbor matching
  ratio = 3
)
plot.matchit(matched_data)

# Check matching balance
summary(matched_data)

length(df)

#REAL

df <- subset(df, Q8A == 0 | Q8A == 1 | Q8A == 2 | Q8A == 3 | Q8A == 4)


# Create a new variable Log_TotalAffected to avoid log(0) issues
df$Log_TotalAffected <- log(df$TotalAffected + 1)
df$Log_Magnitude <- log(df$Magnitude + 1)

str(matched_data)
#OLS
# Model 1: Basic logistic regression with Occurrence
m1 <- lm(Q8A ~ Occurrence, 
         data = df)

# Model 2
m2 <- lm(Q8A ~ Occurrence + Occurrence:DaysSinceDisaster, data = df)

m5 <- lm(Q8A ~ Occurrence + 
           Occurrence:DaysSinceDisaster + 
           Occurrence:Log_TotalAffected, 
         data = df)
m6 <- lm(Q8A ~ Occurrence + 
           Occurrence:DaysSinceDisaster + 
           Occurrence:Log_TotalAffected + 
           Occurrence:Log_Magnitude, 
         data = df)

stargazer(m1,m2,m5,m6, title="OLS Results", align=TRUE)

#LOGISTIC
# Model 1: Basic logistic regression with Occurrence
logit_model1 <- glm(Q8A ~ Occurrence, 
                    data = df, 
                    family = binomial)

# Model 2
logit_model2 <- glm(Q8A ~ Occurrence + Occurrence:DaysSinceDisaster, data = df, family=binomial)


# Model 5: Adding interaction terms
LLM1 <- glm(Q8A ~ Occurrence + 
              Occurrence:DaysSinceDisaster + 
              Occurrence:Log_TotalAffected, 
            data = df, 
            family = binomial)

# Model 6: Full model with log-transformed TotalAffected
LLM2 <- glm(Q8A ~ Occurrence + 
              Occurrence:DaysSinceDisaster + 
              Occurrence:Log_TotalAffected + 
              Occurrence:Log_Magnitude, 
            data = df, 
            family = binomial)

# Display results using stargazerLog_TotalAffected# Display results using stargazer
stargazer(logit_model1,logit_model2, LLM1, LLM2, title="Logistic Regression Results", align=TRUE, column.labels = c("Regular", "Log-Transformation"), column.separate = c(4, 2))

install.packages("MASS")
library(MASS)

# Fit the ordinal logistic regression model
om1 <- polr(as.factor(Q8A) ~ Occurrence, data = df, Hess = TRUE)
om2 <- polr(as.factor(Q8A) ~ Occurrence + Occurrence:DaysSinceDisaster, data = df, Hess = TRUE)
om3 <- polr(as.factor(Q8A) ~ Occurrence + Occurrence:DaysSinceDisaster + 
              Occurrence:TotalAffected, data = df, Hess = TRUE)
om4 <- polr(as.factor(Q8A) ~ Occurrence + 
              Occurrence:DaysSinceDisaster + 
              Occurrence:TotalAffected + 
              Occurrence:Magnitude, data = df, Hess = TRUE)
om5 <- polr(as.factor(Q8A) ~ Occurrence + Occurrence:DaysSinceDisaster + 
              Occurrence:Log_TotalAffected, data = df, Hess = TRUE)
om6 <- polr(as.factor(Q8A) ~ Occurrence + 
              Occurrence:DaysSinceDisaster + 
              Occurrence:Log_TotalAffected + 
              Occurrence:Log_Magnitude, data = df, Hess = TRUE)

stargazer(m1,m2,m5,m6, title="OLS Results for Q8A", align=TRUE,font.size = "tiny")

texreg(list(om1,om2,om5,om6),
       caption = "Ordinal Regression Results for Q8A",
       custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4"),
       dcolumn = TRUE,
       booktabs = TRUE,
       use.packages = FALSE)



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
df <- df[!(df$Q85 %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q86A %in% c(8, 9, -1, NA, 94)),]
df <- df[!(df$Q86B %in% c(8, 9, -1, NA, 94)),] 
df <- df[!(df$Q86C %in% c(8, 9, -1, NA, 94)),] 

#In many questions - Q41D, Q82A, Q82B, Q84C - there was bias from '94' - this is not in the codebook, but reflects a question not being asked in that country.

#Sub-indices - consider, are the variables coded similarly? If so, we could use an additive measure. Then we'll scale them so that they're all between 0 and 1, for ease. Also look at the degree of correlation between variables

#index_general will include 
#question 83 (Generally speaking, would you say that most people can be trusted or that you must be very careful in dealing with people?). coded with higher values indicating more trust. For 83, it is binary, already a unit interval, 

Q83unit <- df$Q83
#question 85 ([Citizens of this country] are very diverse. They come from different religions,ethnic groups, political parties, and economic and social backgrounds. Overall, would you say that there is more that unites all [citizens of this country] as one people, or more that divides them?)
# There are 4 values from 1-4, with 4 being the highest support for cohesion. Therefore, I have subtacted 1 and divided it by 3, so that it ranges from 0-1, and then can be compared with Q83.


Q85unit <- ((df$Q85-1)/3)
index_general <- (Q83unit + Q85unit)/2
df$index_general <- index_general


#index_intra will include:
#question 86C (For each of the following types of people, please tell me whether you would like having people from this group as neighbours, dislike it, or not care: Homosexuals). This takes values 1-5, with 5 indicting the highest tolerance.
#question 8A Over the past year, how often, if ever, have you or anyone in your family: Felt unsafe walking in your neighbourhood?
#question 8B Over the past year, how often, if ever, have you or anyone in your family: Feared crime in your own home?
#Questions 8A and 8B are coded with 0 being 'never' feeling unsafe, 4 being 'always'. There are therefore 5 categories, with a higher score indicating a lower safety perception. Unusable values coded as 8, 9, -1

Q86Cunit <- (df$Q86C -1)/4

Q8Aunit <- (4 - df$Q8A)/4

Q8Bunit <- (4 - df$Q8B)/4

#NOTE - there are some bad results for Q86C - getting 9.
index_intra <- (Q86Cunit + Q8Aunit + Q8Bunit)/3
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

cohesion_index <- (index_general + index_intra + index_ethnoreligious + index_economic + index_authority)/5
df$cohesion_index <- cohesion_index
hist(df$cohesion_index)
