### Low quality responder script V2
### Fintan
### The method this script applies is as follows:
# 1. Remove variables that have inconsistent levels of NAs that are not question
# variables
# 2. Count the number of NA (skipped or not asked) responses for each case
# 3. Work out total response time for each case by taking starttime away from
# endtime.
# 4. Adjust total response time by adding 8 seconds to each case for every NA
# (skipped or not asked) response they have. The amount of time added for each
# question can be adjusted on line 53. 
# 5. Calculates a mean response time across all cases that took less than 20
# minutes (to discount survey returners from the calculation)
# 6. Codes anyone with a response time of two standard deviations below that as
# a 'speeder' - in a variable called 'speeder'. This can be adjusted on line 86
# to be more or less strict. 
# 7. NEW FEATURE: Counts the number of 'don't know' responses respondents give
# excluding those from multiple and rank response format questions
# 8. Codes anyone as 2SD or above average don't know rate in the survey as a
# cause for concern. For now it is recommended that high DK people are not used
# as a marker for removal, but instead as a flag for checking open responses
# to see if they are bad responders that may have not been caught in 
# straightliner and speeder checks.
# 9. Anyone who is classed as a speeder or straightliner will be marked as an
# "overall cause for concern" in variable 'overall quality concern' - HOWEVER,
# this variable will NOT BE CREATED if you have not included the straightline script
# in the survey you are running data into this script from.
# 10. Joins df_speeders with our original df and writes this as a new spss file
# called 'speeders_dks_straightline.sav'.



#Any questions, ask Fintan.

#only run this line if you don't have the package 'pacman' installed
install.packages("pacman")

library(pacman)

p_load(dplyr,tidyverse,sjmisc,sjlabelled,lubridate,ggplot2)



#DEFINE YOUR WORKING DIRECTORY

setwd("A WORKING DIRECTORY")


#DEFINE FILE NAME
df<- sjlabelled::read_spss("df_name.sav")


#Filters out incompletes (in case you've accidentally downloaded with)
df <- filter(df, df$disposition==1)



#Codes start and end time as time-date variables using lubridate
df$starttime <- as_datetime(ymd_hms(df$starttime))
df$endtime <- as_datetime(ymd_hms(df$endtime))

#creates a new dataframe excluding values where many respondents are coded as NA.
#10 seconds is added to each respondents total completion time for each question
#they are coded as NA for, which deals with individual cases' response times
#being effected by routing.
df_speeders <- df%>%
  dplyr::select(-contains("vote"),-contains("profile")%>%
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

df_dk_flags<- df_speeders %>%
  dplyr::select(-contains("time"))%>%
  as_character(.)

df_speeders <- df_speeders%>%
  select(identity, contains("time_"),speeder)




df_dk_flags <- df_dk_flags %>%
  mutate( across(
    .cols = everything(),
    ~str_replace(., "Don't know", "97" )
  ) )%>%
  sjmisc::row_count(., count = 97,var = "Number_dks",append=T)%>%
  dplyr:: select(identity, Number_dks)

too_many_dks_threshold <- (mean(df_dk_flags$Number_dks) + (2*(sd(df_dk_flags$Number_dks))))

df_dk_flags <- df_dk_flags%>%
  mutate(too_many_dks= ifelse(Number_dks > too_many_dks_threshold,1,2))%>%
  as_numeric(identity)


df_final <- left_join(df,df_speeders,by="identity")
df_final <- left_join(df_final, df_dk_flags, by="identity")


df_final <- df_final%>%
  set_labels(too_many_dks,labels= c("1" = "Cause for concern", "2"= "No cause for concern"))

df_final <- df_final%>%
  mutate(overall_quality_concern = ifelse(speeder==1 | straight_liner_flag >= 2, 1,0))%>%
  set_labels(overall_quality_concern, labels= c("1" = "Overall cause for concern"))

df_final <- dplyr::select(df_final, identity, contains("overall_quality_concern"),contains("Number_DKs"), contains("too_many_dks"),contains("straight_lin"),contains("speeder"),
                          contains("time"))

#WRITE SPSS OF FINAL OUTPUT FILE CONTAINING THE NUMBER OF DON'T KNOW RESPONSES
#GIVEN BY EACH RESPONDENT, AND TIME TAKEN PLUS SPEEDER CLASS.
write_spss(df_final, path = "speeders_dks_straightline.sav")







################### VISUALISATIONS TO GET OVERALL PICTURE OF SAMPLE QUALITY
#Will save to your working directory


#plot number of don't knows distribution for whole sample
distribution_dks <-ggplot2::ggplot(df_dk_flags, aes(Number_dks))+
  geom_histogram(binwidth = 1, col= "red")+ 
  geom_vline(xintercept = mean(df_dk_flags$Number_dks), col= "blue", lwd=.5) +
  geom_vline(xintercept = (too_many_dks_threshold), col="green") +
  labs(title="Distribution of number of don't know answers provided per respondent", xlab="Number of don't knows")

#plot time taken for whole sampel
distribution_time <-ggplot2::ggplot(df_returners_excl, aes(adjusted_time_taken))+ 
  geom_histogram(binwidth = 10, col= "red")+
  geom_vline(xintercept = mean(df_returners_excl$adjusted_time_taken), col= "blue", lwd=.5) +
  geom_vline(xintercept = (lowest_allowed_response_time), col="green") +
  labs(title="Distribution of time taken to complete survey",subtitle = "Excluding those who took 20 minutes or longer to complete the survey", xlab="Time taken to complete survey (seconds)")


#SAVES PLOTS OF DISTRIBUTIONS OF TIME TAKEN + NUMBER OF DON'T KNOW RESPONSES GIVEN
ggsave("dk_numbers.png", plot = distribution_dks)
ggsave("time_taken_distribution.png", plot = distribution_time)




