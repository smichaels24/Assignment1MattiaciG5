##############################
##############################
## Spencer Michaels
## POSC 113
## Spring 2021
## Assignment 1
## Team 5 [myself, Samara, Lawrence, and Isabelle]
## Week #1
######################################
######################################

# Library and dependency imports. You may have to manually install via the Packages tab.
library(foreign)
library(dplR)
library(tidyverse)
library(mdsr)
library(magrittr)
library(dplyr)

## ALERT: CHANGE THE LINE BELOW TO YOUR WORKING DIRECTORY IF YOU GET ERROR, WHICH CAN BE FOUND WITH THE getwd() COMMAND BELOW ##

# setwd("D:/R/MAR05")

## I load the data from NMC.csv. NOTE: your data must be renamed to NMC.csv if you move the R file, which I do not reccomend.
NMC <- read.csv("NMC.csv")

# Some initial filter commands which will be used later. Only CINC, year, and state abbreviation is taken.

# Before the Cold War (before 1945)
preCW <- NMC %>%
  filter(year < 1945) %>%
  select(stateabb, year, cinc)

# After the Cold War (after 1991)
postCW <- NMC %>%
  filter(year > 1991) %>%
  select(stateabb, year, cinc)

# During the Cold War (between 1945 and 1991)
duringCW <- NMC %>%
  filter(year >= 1945 & year >= 1991) %>%
  select(stateabb, year, cinc)

# After WW2 (after 1945)
postWW2 <- NMC %>%
  filter(year > 1945) %>%
  select(stateabb, year, cinc)

# After 9/11 (after or equal to 2002)
post911 <- NMC %>%
  filter(year >= 2002) %>%
  select(stateabb, year, cinc)

### 1. Check out the Correlates of War National Military Capabilities dataset. 
#      Post 9-11, which are the ten most powerful countries in the world? Which are the 10 least powerful countries?
    ## NOTE: we took data from *only* the year 2002, a.k.a. directly post-9-11. 
      
# Take data from the year 2002, only the stateabb, year, and CNIC data
war_data_post <- NMC %>%
  filter(year == 2002) %>%
  select(stateabb, cinc)

# Take the top 10 highest CNIC scores
mostpwr <- war_data_post %>%
  arrange(desc(cinc)) %>%
  head(10) 

# Print the results
print("Top 10 most powerful countries in 2002, according to CINC scores") 
print(mostpwr)

# Take the bottom 10 highest CNIC scores
leastpwr <- war_data_post %>%
  arrange(desc(cinc)) %>%
  tail(10) 

# Print the results  
print("Least 10 most powerful countries in 2002, according to CINC scores") 
print(leastpwr)

### 2. What is the average level of countries’ power during the Cold War (1945-1991)? What is the median level of power during the Cold War?

# Calculate and print median and mean of CINC values for CW vector
meanCWpwr <- mean(duringCW$cinc) 
medianCWpwr <- median(duringCW$cinc) 
# Print results
cat("\nAverage CINC during CW:", meanCWpwr)
cat("\nMedian CINC during CW:", medianCWpwr)

## 3. Has China (CHN) become more powerful since the end of the Cold War?

# Calculate averages of pre and post CW CHN CINC scores.
CHN_preCW <- filter(preCW, stateabb == "CHN")
CHN_postCW <- filter(postCW, stateabb == "CHN")
meanCHN_preCW <- mean(CHN_preCW$cinc)
meanCHN_postCW <- mean(CHN_postCW$cinc)
# Print results
cat("\nAverage Chinese CINC scores before the Cold War:", meanCHN_preCW)
cat("\nAverage Chinese CINC scores after the Cold War:", meanCHN_postCW)

### 4. Were countries more or less spread out when it comes to power during the cold war (1945-1991) or after the Cold War (1992-2016)?

# Calculate standard deviation of CINC scores during and after
sdDur <- sd(duringCW$cinc)
sdAft <- sd(postCW$cinc)
sdBef <- sd(preCW$cinc)
# Print results
cat("\nStandard deviation of CINC before the Cold War:", sdBef)
cat("\nStandard deviation of CINC during the Cold War:", sdDur)
cat("\nStandard deviation of CINC after the Cold War:", sdAft)

### 5. What are the two boundaries of the interquartile range for power for the US since the end of the second world war? (since 1945) 
     # What about the interquartile range for power for the US since the end of the Cold War? 
     # What about the interquartile range for power for the US since 9/11?
US_postWW2 <- filter(postWW2, stateabb == "USA")
US_postCW <- filter(postCW, stateabb == "USA")
US_post911 <- filter(post911, stateabb == "USA")

IQR_US_postWW2 <- IQR(US_postWW2$cinc)
IQR_US_postCW <- IQR(US_postCW$cinc)
IQR_US_post911 <- IQR(US_post911$cinc)

# Print results:
cat("\nUS IQR after WW2:", IQR_US_postWW2)
cat("\nUS IQR after 911:", IQR_US_post911)
cat("\nUS IQR after the Cold War:", IQR_US_postCW)

### 6. Repeat question 5, substituting China for the US.
CHN_postWW2 <- filter(postWW2, stateabb == "CHN")
CHN_postCW <- filter(postCW, stateabb == "CHN")
CHN_post911 <- filter(post911, stateabb == "CHN")

IQR_CHN_postWW2 <- IQR(CHN_postWW2$cinc)
IQR_CHN_postCW <- IQR(CHN_postCW$cinc)
IQR_CHN_post911 <- IQR(CHN_post911$cinc)

# Print results:
cat("\nCHN IQR after WW2:", IQR_CHN_postWW2)
cat("\nCHN IQR after 911:", IQR_CHN_post911)
cat("\nCHN IQR after the Cold War:", IQR_CHN_postCW)

## 7.	Given the data analysis you have conducted so far, tell us the three things 
#     you have learnt about power with respect to the US, China, and the world. Feel free to conduct more data analysis to answer this question.

    ## 1. The Cold War started significant shifts in power. Chinese IQRs of 
#         CINC scores are increasing (0.034 after WW2, 0.036 after the Cold War, 0.037 after 9/11) while American CINC IRQs are decreasing (0.067, 0.0098, and 0.0059, respectivley).
    ## 2. The Cold War changed the way in which power was balanced across countries. 
#         SD before was 0.051, changing to 0.018 after. From my understanding, this means that the power in the world changed 
#         slightly from lots of powerful and weak countries to some weak, some powerful, and some in the middle. 
#         This makes logical sense, as you can see  once-third-world countries have evolved (India, many parts of Africa, South America).
    ## 3. China has been gaining power. Comparing IQRs between the US and China can give us a good relative look, 
#         but by looking at raw CINC scores, China's power increased 22.6% after the Cold War as compared to before.


## CLEAR all variables at end of run in case of changed values above!
rm(list=ls())
# or go via the GUI, the little broom in the Environment tab
