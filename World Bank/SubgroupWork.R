#We can then run for subgroups:



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

