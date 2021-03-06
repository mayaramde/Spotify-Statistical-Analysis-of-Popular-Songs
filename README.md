# Spotify-Statistical-Analysis-of-Popular-Songs
Statistical analysis of top streamed Spotify songs, organized by genre.
Using a data set of over 80,000 songs, I analyzed popular songs by highlighting musical elements that significantly contribute to increrased song streams. 
This project is coded in R. 

PROJECT OBJECTIVES:
Despite the important role data now plays in the music industry, many companies are behind the “data curve.” 
To that end, Universal Music has hired you as a data analyst to help them understand how they can utilize Spotify to their benefit.
Below are some of the questions Universal Music would like answered: 
•	What can we learn about listeners’ music tastes at a high level from this data? Are there any discernable patterns in the data? 
•	What are the key factors related to large number of streams? Are there simple rules to select songs with potentials of high streams?
•	Based on the data, what direction is the Spotify ecosystem headed? Are the characteristics of popular songs consistent over time, or do they change in a predictable way from year to year?
•	What else can we learn from this data set?

PROJECT SUMMARY (collaborated with a team):
As we are focusing on understanding listeners’ music taste and looking for patterns, 
we consider the stream count to be a determining factor of whether listeners favor a song or not. 
Our hypothesis is that the reason why people listen to a song more might be because of its danceability, acountiscness, instrumentalness, loudness, etc. 
Thus, we generate a correlation table to find discernible patterns in the data. 
Since there are non-numeric variables in the Spotify data set, we extract the most useful and significant numeric variables to interpret their correlations with streams and analyze if they are the reason why listeners like or dislike a song which is numerically demonstrated by the stream count. We then use this code to make a correlation matrix:
spotify.n=as.data.frame(lapply(spotify[,-c(1:4)],as.numeric))
cor(spotify.n)

Based on the correlation matrix, we figured that danceability and loudness have the strongest positive correlation relationships with streams whereas acousticness and instrumentalness have the strongest negative correlation relationships with streams. 
According to our observation, as danceability and loudness increase, streams will increase accordingly. 
This finding indicates that people highly prefer music that is easier to dance with. 
To some extent, songs with high danceability are easier to spread because of their rhythm. 
In addition to danceability, loudness is also an important factor to increase streams, since the correlation between loudness and streams is relatively strong. 
“Volume is important because it will let the song take part in your body system, muscles and nerves, and this will affect the subconscious by reaching deeper into your system.”
We can tell that , in general, the listeners prefer songs with stronger danceability and loudness. 
The correlation analysis suggests danceability and loudness have strong positive correlation relationships with streams, while acousticness and instrumentalness have strong negative correlation with streams. 

Therefore, the regression model is built based on these 4 features：
mydata-< spotify.n[,c("danceability","loudness", "acousticness","instrumentalness","streams")]
lm.reg <- lm(streams~.,data = mydata)

The p-values in this model's summary are way below 0.05, further proving that the 4 characteristics significantly affect the value of streams. 
To assess the appropriateness of this linear regression model, a residual analysis is performed. 
With the residual normal QQ plot and the quantized skewness, it's clear that there are more abnormal values on the right side of the theoretical quantiles and part of the residuals is still related to the predictive variables. 
Furthermore, based on the Cook's distance, we can tell that there are 1289 values that have abnormal impact on the regression model.
{cook=cooks.distance(lm.reg) length(which(cook>4/n))}

With the scattered plots shown for danceability, loudness, acousticness, and instrumentalism in relationship to streams respectively, the conclusion could be drawn that danceability, loudness, acousticness, and instrumentalness are suitable features to predict streams. 
The more danceability and loudness a song has, the more streams it is likely to draw, the opposite for instrumentalness and acousticness. 
However, there are 1289 data points that are subjected to abnormal effects on the regression model and the non-randomized residuals suggest that some of the residuals are related to the predictive variables.
In order to corroborate these findings, we wrangled the given data to create a stream_cat variable, splitting stream count into two facets of analysis. 
These were crafted through visualizing the distribution of the stream count variable, which revealed a largely binary distribution, with roughly the top 2.5% of songs being separated from the rest of the pack. 
An analysis of these top songs yielded a result of an increased positive correlation with danceability and loudness, and an amplified negative correlation with acousticness, as illustrated through faceted scatter plots and interaction models. 
Therefore, the upper echelon of songs in the data set illustrates a significantly more accurate picture of the influence of the various metrics on music popularity.
Based on the Spotify data set, the most trending/most listened to music genre is Pop music. As Pop music usually has strong tempo and loudness, which are two positively correlated music elements to stream count. 
From a business perspective, to maximize profit and promote business, Universal Music should focus more on producing Pop music or music that has higher danceability and loudness.  
Although the Spotify ecosystem changes frequently, so far in 2021 and the near future, we can see that Pop music is the most successful music genre. 
We think people are more into Pop music nowadays because of the influence of social media, as social media is playing a huge role in leading people’s taste in music. 
Above is the real-time preferences of listeners and what Universal Music should know in order to utilize its resources and capitalize on the music market. 
Although we are not given data on a year to year basis, our prediction is that people’s music preferences will be consistent in recent years. 
One suggestion to Universal Music is that it can keep track or pay attention to what kind of music is trending or becoming popular on Tiktok, instagram, or other social media platforms. 
This is a way to dominate the dynamic Spotify ecosystem and be more prepared for any future changes in listeners’ overall music tastes. 
