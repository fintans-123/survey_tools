### Fintan - script for identifying speeders from survey response data
### The method this script applies is as follows:
# 1. Remove variables that have inconsistent levels of NAs that are not question
# variables
# 2. Count the number of NA (skipped or not asked) responses for each case
# 3. Work out total response time for each case by taking starttime away from
# endtime.
# 4. Adjust total response time by adding 8 seconds to each case for every NA
# (skipped or not asked) response they have. The amount of time added for each
# question can be adjusted on line 48. 
# 5. Calculates a mean response time across all cases that took less than 20
# minutes (to discount survey returners from the calculation)
# 6. Codes anyone with a response time of two standard deviations below that as
# a 'speeder' - in a variable called 'speeder'. This can be adjusted on line 60
# to be more or less strict. 
# 7. Joins df_speeders with our original df and writes this as a new spss file
# called 'speeders'.

#Any questions, ask Fintan.




library(tidyverse)
library(sjlabelled)
library(lubridate)


#DEFINE YOUR WORKING DIRECTORY

setwd("WORKING DIRECTORY HERE")


#DEFINE FILE NAME
df<- sjlabelled::read_spss("FILE NAME.sav")

#Codes start and end time as time-date variables using lubridate
df$starttime <- as_datetime(ymd_hms(df$starttime))
df$endtime <- as_datetime(ymd_hms(df$endtime))

#creates a new dataframe excluding values where many respondents are coded as NA.
#10 seconds is added to each respondents total completion time for each question
#they are coded as NA for, which deals with individual cases' response times
#being effected by routing.
df_speeders <- df%>%
  select(-contains("_demog"))%>%
  mutate(numberqs_not_asked= apply(X = is.na(.), MARGIN = 1, FUN = sum))%>%
  mutate(time_adjustment= (numberqs_not_asked * 8))%>% 
  mutate(time_taken= interval(starttime, endtime)/seconds(1))%>%
  mutate(adjusted_time_taken= (time_taken + time_adjustment))%>%
  mutate(diff= adjusted_time_taken-time_taken)


# Excludes those who returned to the survey (defined here as taking more than 20 minutes)
#from calculation of mean response time overall
df_returners_excl <- filter(df_speeders, time_taken < (20*60))

#Defines the lowest allowed response time overall as 2 sd below the mean response time
lowest_allowed_response_time<- (mean(df_returners_excl$adjusted_time_taken) - 
                                  ((2*sd(df_returners_excl$adjusted_time_taken))))

#Count the number of NAs (to account for routing)
df_speeders<- df_speeders%>%
  mutate(speeder = ifelse(adjusted_time_taken < lowest_allowed_response_time, 1,2))%>%
  set_labels(speeder, labels = c(
    "1" = "Speeder",
    "2" = "Non-speeder"
  ))

df_speeders <- df_speeders%>%
  select(identity, contains("time_"),speeder)

df_final <- left_join(df,df_speeders,by="identity")

write_spss(df_final, path = "speeders.sav")





