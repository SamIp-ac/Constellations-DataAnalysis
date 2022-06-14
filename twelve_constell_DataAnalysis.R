#Data from https://www.kaggle.com/aagghh/divorcemarriage-dataset-with-birth-dates
#Claim : Want to find out the relationship between the 12-constellation love-divination and 
#their data in reality.
#Is it really useful?
#The data describe the divorces information of the man and his wife (woman), including 
#their day of birth (their relative constellations)

library(datasets, drc, stringr)
if (!require("pacman")) install.packages("pacman") #pacman to load add-on packages as desired
pacman::p_load(pacman, tidyverse, psych, stats, lars, caret, MASS, standardize, drc, stringr) 

divorces_translated <- read.csv('~/Desktop/divorces/divorces_2000-2015_translated.csv', header = TRUE)
#In the following context, constell. == constellations
#descriptions_for_column <- read.csv('~/Desktop/descriptions_for_ column.csv', header = TRUE)
#Comp_matrix <- read.csv('~/Desktop/Comp_matrix.csv', header = TRUE)
#Date representation method (Unused)
trial <- (seq(as.Date('21/03',"%d/%m"),as.Date('21/04',"%d/%m"), by = 'day'))
trial
trial1 <- format(trial, "%d/%m/%Y")
trial1

# '+' means good with the following constellation.
# '-' means bad with the following constellation.
# The matchingness of these 12-constellation are of the view of woman.
#####Not need to insert, just for review####
#Girl                     Boy          Boy
Aries <- 21/3 -- 20/4 # + Sagittariu - Virgo
Taurus <- 21/4 -- 20/5 # + Capricornus - Libra
Gemini <- 21/5 -- 20/6 # + Aquarius - Scorpius
Cancer <- 21/6 -- 22/7 # + Pisces - Sagittarius
Leo <- 23/7 -- 22/8 # + Aries - Capricornus
Virgo <- 23/8 -- 22/9 # + Taurus - Aquarius
Libra <- 23/9 -- 22/10 # + Gemini - Pisces
23/10 #controversial day
Scorpius <- 24/10 -- 22/11 # + Cancer - Aries
Sagittarius <- 23/11 -- 21/12 # + Leo - Taurus
Capricornus <- 22/12 - 19/01 # + Virgo - Gemini
20/01 #controversial day
Aquarius <- 21/01 - 18/02 # +Libra - Cancer
19/02 #controversial day
Pisces <- 20/02 -- 20/03 # +Scorpius - Leo

#Start now
#Insert these data
#Modify the date value
sample_man <- as.Date(divorces_translated$DOB_partner_man,"%d/%m/%Y")
sample_man <- format(sample_man, '%d/%m/%Y')
sample_man <- substr(sample_man, 1, 5)
typeof(sample_man)
sample_man

#Cancel the birth of '29/02'
#it will not affect the result, too less
sample_man[sample_man=='29/02'] <- 0
sample_man[is.na(sample_man)] <- 0
sample_man

#Convert the birth of date into a day within a year
for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '01'){
  sample_man[i] <- as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '02'){
  sample_man[i] <- (31)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '03'){
  sample_man[i] <- (31+28)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '04'){
  sample_man[i] <- (31+28+31)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '05'){
  sample_man[i] <- (31+28+31+30)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '06'){
  sample_man[i] <- (31+28+31+30+31)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '07'){
  sample_man[i] <- (31+28+31+30+31+30)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '08'){
  sample_man[i] <- (31+28+31+30+31+30+31)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '09'){
  sample_man[i] <- (31+28+31+30+31+30+31+31)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '10'){
  sample_man[i] <- (31+28+31+30+31+30+31+31+30)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '11'){
  sample_man[i] <- (31+28+31+30+31+30+31+31+30+31)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}

for (i in c(1:4923)) { if (substr(sample_man[i], 4, 5) == '12'){
  sample_man[i] <- (31+28+31+30+31+30+31+31+30+31+30)+as.double(substr(sample_man[i], 1, 2))
} else sample_man[i] <- sample_man[i]}
sample_man

#Unused
#as.double(substr(sample_man, 1, 2))
#substr(sample_man, 1, 2)
#Similarly

sample_woman <- as.Date(divorces_translated$DOB_partner_woman, "%d/%m/%Y")
sample_woman <- format(sample_woman, '%d/%m/%Y')
sample_woman <- substr(sample_woman, 1, 5)
sample_woman[sample_woman=='29/02'] <- 0
sample_woman[is.na(sample_woman)] <- 0
sample_woman
for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '01'){
  sample_woman[i] <- as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '02'){
  sample_woman[i] <- (31)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '03'){
  sample_woman[i] <- (31+28)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '04'){
  sample_woman[i] <- (31+28+31)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '05'){
  sample_woman[i] <- (31+28+31+30)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '06'){
  sample_woman[i] <- (31+28+31+30+31)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '07'){
  sample_woman[i] <- (31+28+31+30+31+30)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '08'){
  sample_woman[i] <- (31+28+31+30+31+30+31)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '09'){
  sample_woman[i] <- (31+28+31+30+31+30+31+31)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '10'){
  sample_woman[i] <- (31+28+31+30+31+30+31+31+30)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '11'){
  sample_woman[i] <- (31+28.5+31+30+31+30+31+31+30+31)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}

for (i in c(1:4923)) { if (substr(sample_woman[i], 4, 5) == '12'){
  sample_woman[i] <- (31+28+31+30+31+30+31+31+30+31+30)+as.double(substr(sample_woman[i], 1, 2))
} else sample_woman[i] <- sample_woman[i]}
sample_woman
summary(divorces_translated)
result <- cbind.data.frame(sample_man, sample_woman)
head(result)
#Zero
x = 0
for (i in c(1:4923)) { if (result[i,1] == 0){
  x = x + 1
} else x = x + 0}
for (i in c(1:4923)) { if (result[i,2] == 0){
  x = x + 1
} else x = x + 0}
for (i in c(1:4923)) { if (result[i,1] == 0 && result[i,2] == 0){
  x = x - 1
} else x = x + 0}
useless_data <- x
Average_distri = (4923-useless_data)/(12**2) #Average distribution
Average_distri
#plot
plot(result, main = 'Relationship between 12-constell. and divorces',
     xlab = 'Day of Birth of Man', ylab = 'Day of Birth of Woman',
     cex = 0.35, pch = 16)
#invent square (love)
lines(297 + c(0 ,0, 29, 29, 0), 51 + c(0, 28, 28, 0, 0), col = 'red') #Pisces
lines(327 + c(0, 0, 28, 28, 0), 80 + c(0, 30, 30, 0, 0), col = 'red') #Aries
lines(356 + c(0, 0, 10, 10, 0), 111 + c(0, 29, 29, 0, 0), col = 'red') #Taurus
lines(0 + c(0, 0, 19, 19, 0), 111 + c(0, 29, 29, 0, 0), col = 'red') #Taurus
lines(21 + c(0, 0, 28, 28, 0), 141 + c(0, 30, 30, 0, 0), col = 'red') # Gemini
lines(51 + c(0, 0, 28, 28, 0), 172 + c(0, 31, 31, 0, 0), col = 'red') # Cancer
lines(80 + c(0, 0, 30, 30, 0), 204 + c(0, 30, 30, 0, 0), col = 'red') # Leo
lines(111 + c(0, 0, 29, 29, 0), 235 + c(0, 30, 30, 0, 0), col = 'red') # Virgo
lines(141 + c(0, 0, 30, 30, 0), 266 + c(0, 29, 29, 0, 0), col = 'red') # Libra
lines(172 + c(0, 0, 31, 31, 0), 297 + c(0, 29, 29, 0, 0), col = 'red') # Scorpius
lines(204 + c(0, 0, 30, 30, 0), 327 + c(0, 28, 28, 0, 0), col = 'red') # Sagittarius
lines(235 + c(0, 0, 30, 30, 0), 356 + c(0, 10, 10, 0, 0), col = 'red') # Capricornus
lines(235 + c(0, 0, 30, 30, 0), 0 + c(0, 19, 19, 0, 0), col = 'red') # Capricornus
lines(266 + c(0, 0, 29, 29, 0), 21 + c(0, 28, 28, 0, 0), col = 'red') # Aquarius

#invent square (hate)
lines(204 + c(0, 0, 30, 30, 0), 51 + c(0, 28, 28, 0, 0), col = 'blue') #Pisces
lines(235 + c(0, 0, 30, 30, 0), 80 + c(0, 30, 30, 0, 0), col = 'blue') #Aries
lines(266 + c(0, 0, 29, 29, 0), 111 + c(0, 29, 29, 0, 0), col = 'blue') #Taurus
lines( 297 + c(0, 0, 29, 29, 0), 141 + c(0, 30, 30, 0, 0), col = 'blue') # Gemini
lines( 327 + c(0, 0, 28, 28, 0), 172 + c(0, 31, 31, 0, 0), col = 'blue') # Cancer
lines(356 + c(0, 0, 10, 10, 0), 204 + c(0, 30, 30, 0, 0), col = 'blue') # Leo
lines( 0 + c(0, 0, 19, 19, 0), 204 + c(0, 30, 30, 0, 0), col = 'blue') # Leo
lines(21 + c(0, 0, 28, 28, 0), 235 + c(0, 30, 30, 0, 0), col = 'blue') # Virgo
lines(51 + c(0, 0, 28, 28, 0), 266 + c(0, 29, 29, 0, 0), col = 'blue') # Libra
lines(80 + c(0, 0, 30, 30, 0), 297 + c(0, 29, 29, 0, 0), col = 'blue') # Scorpius
lines(111 + c(0, 0, 29, 29, 0), 327 + c(0, 28, 28, 0, 0), col = 'blue') # Sagittarius
lines(141 + c(0, 0, 30, 30, 0), 356 + c(0, 10, 10, 0, 0), col = 'blue') # Capricornus
lines(141 + c(0, 0, 30, 30, 0), 0 + c(0, 19, 19, 0, 0), col = 'blue') # Capricornus
lines(172 + c(0, 0, 31, 31, 0), 21 + c(0, 28, 28, 0, 0), col = 'blue') # Aquarius

#Doubt days
abline(v = 20, h = 20)
abline(v = 50, h = 50)
abline(v = 296, h = 296)

legend(150, 250, legend=c('Not matching', 'Matching','Controversial'),
       col=c('blue', 'red', 'black'), lty=1)
#Agree
#Hate: more
#Love: less
#Disagree
#Hate: less
#Love: more
Average_distri
result_hate <- c(32,35,21,39,22,40,32,34,26,29,24,32) #Just calculate it by human
result_love <- c(33,16,33,29,33,33,19,16,26,34,27,22)
summary(result_hate)
summary(result_love)
barplot(result_hate, main = 'The divorces number of a pair of not mactching constellations ',
        xlab = 'A pair of constellation', ylab = 'Number of cases of divorces'
)
abline(h = 30)
barplot(result_love, main = 'The divorces number of a pair of mactching constellations ',
        xlab = 'A pair of constellation', ylab = 'Number of cases of divorces'
)
abline(h = 30)

h = 0
for (i in c(1:12)){
  h = result_hate[i] + h
}
mean_love = h/12

l = 0
for (i in c(1:12)){
  l = result_hate[i] + l
}
mean_hate = l/12

mean_love
mean_hate


#There does not exist an observable relationship between the 12-constell. and love
#but so odd to see that the mean of both 'hate' and 'love' are very near the 'Average_distru'
# It means that the love divination by using 12-constellations seems useless.
#Review: It is not fair by using the different years data
#Since the people birth at the same year may experience 12-kind of life because of 
#the climate, current situation, technology development and era characteristics.
#I actually think that 12-constellation not only consider as a occult, it may reflect
#12 kinds of characteristic.

head(divorces_translated)
pca_divorces <-prcomp(~ Age_partner_man + Monthly_income_partner_man_peso +
                        Age_partner_woman + Marriage_duration + Num_Children, 
                      data = divorces_translated, center = TRUE, scale = TRUE)
plot(pca_divorces)
summary(pca_divorces)
pca_divorces
predict(pca_divorces) %>% round(2)
biplot(pca_divorces)
#216, 3080, 848, 354, 2774, 1007, 3335 #100, 333, 75 #33,35,75 #335 #3035
reduce <- c(216, 3080, 848, 354, 2774, 3375, 1003, 2968, 3925, 3414, 4443, 371, 3075,
            360, 1214, 4407, 1567, 185, 951, 1590, 2017, 999, 1785, 3518, 1881, 2253, 
            3164, 3048, 4007, 3618, 3650, 722, 725, 723, 765, 4272)
divorces_translated_new <- divorces_translated[-reduce,]

pca_divorces_new <-prcomp(~ Age_partner_man + Monthly_income_partner_man_peso +
                            Age_partner_woman + Marriage_duration + Num_Children, 
                          data = divorces_translated_new, center = TRUE, scale = TRUE)
plot(pca_divorces_new)
summary(pca_divorces_new)
pca_divorces_new
predict(pca_divorces_new) %>% round(2)
biplot(pca_divorces_new)

pca_divorces_new <-prcomp(~ Monthly_income_partner_man_peso +
                            Marriage_duration + Num_Children, 
                          data = divorces_translated_new, center = TRUE, scale = TRUE)
plot(pca_divorces_new)
summary(pca_divorces_new)
pca_divorces_new
predict(pca_divorces_new) %>% round(2)
biplot(pca_divorces_new)
#The data component 'Monthly_income_partner_man_peso' and 'Num_Children' are nearly 
#perpendicular to each other, it make reflect that it may be a reason of the divorce.
#They do not have the a birth plan.

######
rm(list = ls())
cat("/014")
######
dev.off()