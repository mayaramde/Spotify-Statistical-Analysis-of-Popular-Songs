library(tidyverse)
library(gapminder)
library(ggplot2)
library(gridExtra)

load("312_project_Spotify.RData")
#contributed by Amity Tang
#data exploration: discover missed values and categorize data
n=sum(complete.cases(spotify))
sum(!complete.cases(spotify))
summary(spotify)

missing <- which(colSums(is.na(spotify))>0)
missing

#contributed by Amity Tang
#rename duration and change its unit to seconds preparing for further exploration
spotify$duration_ms=spotify$duration_ms/1000
names(spotify)[names(spotify) == 'duration_ms'] <- 'duration'

#contributed by Amity Tang
#mode exploration (but found it not relevent enough for analysis)
row.names(table(spotify$mode))
spotify$mode <- factor(spotify$mode)
mode.mo <- lm(streams~mode,data=spotify)
summary(mode.mo)$coef
contrasts(spotify$mode)

spotify$mode <- factor(spotify$mode,
                       levels = c("Minor","Major"),
                       labels = c(0,1))

#contributed by Amity Tang
#produce correlation matrix and discover the correlation between
#danceability, loudness, acousticness, instrumentalness, and streams
spotify.n=as.data.frame(lapply(spotify[,-c(1:4)],as.numeric))
cor(spotify.n)

#contributed by Amity Tang
#build model based on correlation matrix
mydata <- spotify.n[,c("danceability","loudness","acousticness","instrumentalness","streams")]
lm.reg <- lm(streams~.,data = mydata)
summary(lm.reg)

#contributed by Amity Tang
#residual analysis
lm.res=residuals(lm.reg)
plot(lm.res)
par(mfrow=c(2,2))
plot(lm.reg)

#contributed by Amity Tang
#further explore residuals by comparing them to normal values and quantilize skewness
qqnorm(lm.reg$residuals);qqline(lm.reg$residuals)
summarylm<-summary(lm.reg$residuals)
Q1<-summarylm[[2]]
Q2<-summarylm[[3]]
Q3<-summarylm[[5]]
skewness<-((Q1-Q2)+(Q3-Q2))/(Q3-Q1)
skewness

#contributed by Amity Tang
#use cook distance to determine abnormal data points (result: 1289)
modelmatrix<-model.matrix(lm.reg)
hatvalues<-hat(modelmatrix)
hatvalues<-lm.influence(lm.reg)$hat
sum(hatvalues)
cook=cooks.distance(lm.reg)
length(which(cook>4/n))

#contributed by Amity Tang
#data visualization to confirm their relationships
ggplot(data = spotify, mapping = aes(x = danceability, y = streams)) +  geom_point()
ggplot(data = spotify, mapping = aes(x = loudness, y = streams)) +  geom_point()
ggplot(data = spotify, mapping = aes(x = acousticness, y = streams)) +  geom_point()
ggplot(data = spotify, mapping = aes(x = instrumentalness, y = streams)) +  geom_point()

#contributed by Zhuolin Geng
cor(na.omit(mydata[,c(1:5,5)]))

#contributed by Sunny Liang

library(tidyverse)
library(moderndive)
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)

#clean up data by dropping irrelevant columns:track_id
spotify<-spotify%>%select(-c(track_id,track_name,artist_name))
summary(spotify)
#create correlation character
corr_spotify <- select(spotify, streams, danceability, energy, loudness, speechiness, 
                       acousticness, instrumentalness, liveness, valence, tempo)
#convert char to numeric data type
corr_spotify$acousticness <- as.numeric(corr_spotify$acousticness)
corr_spotify$instrumentalness <- as.numeric(corr_spotify$instrumentalness)
str(corr_spotify)
#build correlation plot
corrplot(cor(corr_spotify), type="lower")
corrplot(cor(corr_spotify), method="number")

# contributed by Maya Ramde
## We can also look at the interaction model between variables that 
## hold similar qualities, such as (1) energy and danceability and 
## (2) acousticness and instrumentalness
summary(lm(streams~.,spotify))
lm_energy_dance = lm(streams~enegry+instrumentalness, spotify)
summary(lm_energy_dance)
spotify %>% 
  ggplot(aes(x = energy, y = streams)) +
  geom_point() +
  geom_smooth(method = "lm")
# Build a basic regression model to predict streams using Income


