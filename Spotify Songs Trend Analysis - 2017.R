###########################################################################
###########################################################################
### TITLE: SPOTIFY TRENDS BASED ON AUDIO FEATURES - YEAR 2021           ###
### AUTHOR: TEFFY ANNIE GEORGE                                          ###
### DATE : APRIL 8, 2022                                                ###
###                                                                     ###
###########################################################################
###########################################################################

# For data Manipulation
library(dplyr)
# For data visualization
library(ggplot2)
# To display the correlation between variables
library(corrplot)
# To display different properties of themes in ggplot
library(ggthemes) 
# plot the decision tree
  library(rpart)
library(rpart.plot)
library(rattle)

# STEP 1: IMPORT DATA TO R
# Get the current working directory
getwd()
# Set the working directory to file path location
setwd("C:\\Data Files")

spotify_df <- read.csv("featuresdf.csv")
daily_df <- read.csv("data.csv")

# Taking the summary of spotify data to check if there are any missing values
summary(spotify_df)
# Taking the summary of daily data to check if there are any missing values
summary(daily_df)

# Data has no NA's 
# This is good data that requires no cleaning for analysis purpose

# STEP 2: DATA ANALYSIS

# Find the top artist from the top 100 list
top_artists <- spotify_df %>% 
               count(artists)
top_artists <- top_artists %>%
               arrange(desc(n))
head(top_artists)

#Getting the class types of each variables
str(top_artists)

#setting the field 'n' as factor as the data can be in seen in descending order
top_artists$artists <-factor(top_artists$artists,
                             levels=top_artists$artists[order(top_artists$n)])

#fetching the top 10 artists
top_artists_highest_songs_top10 <- top_artists[1:10,]

#Plotting the top most artists from the top 100 list
ggplot(top_artists_highest_songs_top10, aes(x = artists, y = n)) +
  geom_bar(stat='identity',fill = "coral3",alpha = 0.8,width = 0.9) +
  geom_text(aes(label=n), color = 'white', hjust = 2, size = 4) +
  ggtitle("Top Artists of 2017") +
  labs(x = "Artist",y = "Total Songs Per Artist Appeared in Top 100") +
  theme(axis.title.x = element_text(colour = "DarkGreen",size= 8),
        axis.title.y = element_text(colour = "DarkGreen",size= 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(color = "Maroon",
                                  size = 10,
                                  family = "Arial",
                                  hjust = 0.5))+
  coord_flip()

#Chain Smokers and Ed Sheeran topped the list

# Based on streamed data in Spotify, 
# listing the top streamed artist and top streamed song
# Find the top streamed artist from the top 100 list

top_streamed_artists <- daily_df %>%
                        group_by(Artist)  %>% 
                        summarise(Total_Streamed_Time = sum(Streams)) %>%
                        arrange(desc(Total_Streamed_Time))

# Find the top streamed song from the top 100 list
top_streamed_songs <- daily_df %>%
                       group_by(Track.Name)  %>% 
                       summarise(Total_Streamed_Time = sum(Streams)) %>%
                       arrange(desc(Total_Streamed_Time))


head(top_streamed_artists)
head(top_streamed_songs)

# Plotting the top 10 of Top Streamed Artists
options(scipen = 99999999)
top_streamed_artists_top10 <- top_streamed_artists[1:10,]
#setting the field 'Total_Streamed_Time' as factor as the data can be in seen in descending order

top_streamed_artists_top10$Artist <-factor(top_streamed_artists_top10$Artist,levels=top_streamed_artists_top10$Artist[order(top_streamed_artists_top10$Total_Streamed_Time)])
ggplot(top_streamed_artists_top10, aes(x = Artist, y = Total_Streamed_Time)) +
  geom_bar(stat='identity',fill = c("turquoise4"),alpha = 0.8,width = 0.8) +
  geom_text(aes(label=Total_Streamed_Time), color = 'white', hjust = 2, size = 4) +
  ggtitle("Top 10 Artists with Highest Streamed Songs") +
  labs(x = "Artist",y = "Total Streamed Time") +
  theme(axis.title.x = element_text(colour = "DarkGreen",size= 8),
        axis.title.y = element_text(colour = "DarkGreen",size= 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(color = "Maroon",
                                  size = 10,
                                  family = "Arial",
                                  hjust = 0.5))+
  coord_flip()


# Plotting the top 10 of Top Streamed Songs
options(scipen = 99999999)
top_streamed_songs_top10 <- top_streamed_songs[1:10,]
#setting the field 'Total_Streamed_Time' as factor as the data can be in seen in descending order

top_streamed_songs_top10$Track.Name <-factor(top_streamed_songs_top10$Track.Name,levels=top_streamed_songs_top10$Track.Name[order(top_streamed_songs_top10$Total_Streamed_Time)])
str(top_streamed_songs_top10)
ggplot(top_streamed_songs_top10, aes(x = Track.Name, y = Total_Streamed_Time)) +
  geom_bar(stat='identity',fill = c("slateblue"),alpha = 0.8,width = 0.9) +
  geom_text(aes(label=Total_Streamed_Time), color = 'white', hjust = 2, size = 4) +
  ggtitle("Top 10 Streamed Songs") +
  labs(x = "Songs",y = "Total Streamed Time") +
  theme(axis.title.x = element_text(colour = "DarkGreen",size= 8),
        axis.title.y = element_text(colour = "DarkGreen",size= 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(color = "Maroon",
                                  size = 10,
                                  family = "Arial",
                                  hjust = 0.5))+
  coord_flip()


# DEPENDENCY OF FEATURES 
features_df <- spotify_df[,-c(1:3)]
cor_features <- cor(features_df,method = "pearson", use="na.or.complete")
corrplot(cor_features,use="na.or.complete",method = "color")

# Energy and Loudness most positively correlated, 
# therefore we will plot the density of these 2 variables
# with the top 100 songs

par(mfrow=c(1,2))

ggplot(spotify_df) +
  geom_density(aes(x = loudness,fill = "loudness") ) +
  theme_solarized_2(light = FALSE,base_size = 15, base_family = "serif")

ggplot(spotify_df) +
  geom_density(aes(x = energy,fill = "energy") ) +
  theme_solarized_2(light = FALSE,base_size = 15, base_family = "serif")

#Relation between Energy and Loudness in the Top 10 Songs
ggplot(spotify_df) + aes(x= energy,y=loudness) +
  geom_point(color = "Purple",alpha = 0.4) + theme_light() +
  labs( x= "Energy",
        y = "Loudness",
        title = "Relationship between Energy And Loudness")+
  theme(axis.title.x = element_text(colour = "DarkGreen",size= 10),
        axis.title.y = element_text(colour = "DarkBlue",size= 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(color = "Brown",
                                  size = 15,
                                  family = "Arial",
                                  hjust = 0.5))+
  stat_smooth(se = FALSE) +
  theme_solarized_2(light = FALSE,base_size = 15, base_family = "serif")

# CHECKING ON KEY VALUES
# Adding column based on other column:

spotify_df <- spotify_df %>%
  mutate(original_key = case_when(
    (key == "0") ~ "C",
    (key == "1") ~ "C#,Db",
    (key == "2") ~ "D",
    (key == "3") ~ "D#,Eb",
    (key == "4") ~ "E",
    (key == "5") ~ "F",
    (key == "6") ~ "F#,Gb",
    (key == "7") ~ "G",
    (key == "8") ~ "G#,A",
    (key == "9") ~ "A",
    (key == "10") ~ "A#,Bb",
    (key == "11") ~ "B"
  ))

# Checking which keys the top 100 songs use
# Find the top artist from the top 100 list
top_keys <- spotify_df %>% 
  count( original_key)
top_keys <- top_keys %>%
  arrange(desc(n))
head(top_keys)

str(top_keys)

#setting the field 'n' as factor as the data can be in seen in descending order
#top_artists$artists <- as.factor(top_artists$artists)
top_keys$original_key <-factor(top_keys$original_key,levels=top_keys$original_key[order(top_artists$n)])

ggplot(top_keys, aes(x = original_key, y = n)) +
  geom_bar(stat='identity',fill = c("springgreen4"),alpha = 0.8,width = 0.6) +
  geom_text(aes(label=n), color = 'black') +
  ggtitle("Top Keys Used in Top 100 Songs of 2017") +
  labs(y = "No of Keys",x = "Keys") +
  theme(axis.title.x = element_text(colour = "DarkGreen",size= 10),
        axis.title.y = element_text(colour = "DarkGreen",size= 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(color = "Maroon",
                                  size = 10,
                                  family = "Arial",
                                  hjust = 0.5))

features_df$rank <- c(1:100)
decision_tree <- rpart(rank ~ ., data = features_df)
prp(decision_tree,type = 1,extra = 1,varlen = 14, under = T,fallen.leaves = T,
split.col = c("red","blue"), box.col = c("green","yellow")[decision_tree$frame$yval])

fancyRpartPlot(decision_tree,cex= 0.45)
?fancyRpartPlot
s
