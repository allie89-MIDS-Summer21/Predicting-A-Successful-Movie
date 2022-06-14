#loading libraries
library(tidyverse)
library(dplyr)
library(scales)
library(readr)
library(ggplot2)
library(readxl)
library(MASS)
install.packages("patchwork")
library(patchwork)
install.packages("stargazer")
library(stargazer)
install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)
install.packages('caret',  repos='https://ftp.osuosl.org/pub/cran/')
library(caret)
install.packages("rcompanion")
library(rcompanion)
install.packages("ggpubr")
library(ggpubr)

#*************************************************DATA WRANGLING***********************************************************

#LOAD CSV FILES THAT WERE DOWNLOADED FROM KAGGLE
movies_df <- read.csv("IMDb movies.csv")

movie_ratings_df <- read.csv("IMDb ratings.csv")

#MERGE MOVIE DATA WITH RATING INFO 
movies_w_ratings_df <- merge(movies_df, movie_ratings_df, by = 'imdb_title_id')


#NARROW DATASET DOWN TO ONLY ROWS THAT HAVE ENGLISH AS ONE OF THE LANGUAGES 
movies_w_subset_df <- movies_w_ratings_df[grep("English", movies_w_ratings_df$language), ]

#NARROW DATASET DOWN TO ONLY ROWS THAT HAVE USA AS ONE OF THE COUNTRIES 
movies_w_subset_df <- movies_w_subset_df[grep("USA", movies_w_subset_df$country), ]

#REMOVE ROWS WERE BUDGET OR USA_GROSS_INCOME ARE BLANK 
movies_w_subset_df <- movies_w_subset_df[-which(movies_w_subset_df$budget == ""), ]
movies_w_subset_df <- movies_w_subset_df[-which(movies_w_subset_df$usa_gross_income == ""), ]

#REMOVE ROWS WHERE THE BUDGET IS NOT REPORTED IN US DOLLARS 
movies_w_subset_df <- movies_w_subset_df[-grep("AUD", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("BRL", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("CAD", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("DKK", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("ESP", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("EUR", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("FRF", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("GBP", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("HUF", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("INR", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("NOK", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("PLN", movies_w_subset_df$budget), ]
movies_w_subset_df <- movies_w_subset_df[-grep("THB", movies_w_subset_df$budget), ]

write.csv(movies_w_subset_df, "movies_w_ratings_filtered.csv")

#WRITE UP FOR ADDING OSCAR WINNER DIRECTER AND WRITER VARIABLES 
## 1)We pulled in a list of all oscar winning directors and writers
## 2)separated multiple directors from our data in separate columns, if a movie had more than one
## 3) Applied vlookup in excel and if any of the directors for each movie were in the oscar winning directors, I coded that as 1


#WRITE UP FOR ADDING STUDIO POPULARITY
## 1)In Excel.. counted the frequency of how often a studio appeared in our dataset
## 2)Added those counts as a column to our main dataset joining on production company 


#loading final data
data <- read_excel("data_v2.xlsx")

#creating a category variable called studio_pop_group from studio_popularity
data$studio_pop_group <- ifelse(data$studio_popularity <=10 , "low", ifelse((data$studio_popularity >10) & (data$studio_popularity <=200), "medium", "high"))
table(data$studio_pop_group)



#filtering to only movies post 1928
df <- data[data$year > 1928,]



#creating a date set that shows average income for each genre
df_grp_genre <- df %>%
  group_by(genre) %>%
  summarise(mean(usa_gross_income))
colnames(df_grp_genre) <- c("genre", "avg_income")
df_grp_genre

df_grp_genre <- df_grp_genre %>%
  mutate(action = if_else(grepl("Action",df_grp_genre$genre), 1, 0),
         adventure = if_else(grepl("Adventure",df_grp_genre$genre), 1, 0),
         comedy = if_else(grepl("Comedy",df_grp_genre$genre), 1, 0),
         crime = if_else(grepl("Crime",df_grp_genre$genre), 1, 0),
         drama = if_else(grepl("Drama",df_grp_genre$genre), 1, 0),
         family = if_else(grepl("Family",df_grp_genre$genre), 1, 0),
         fantasy = if_else(grepl("Fantasy",df_grp_genre$genre), 1, 0),
         history = if_else(grepl("History",df_grp_genre$genre), 1, 0),
         horror = if_else(grepl("Horror",df_grp_genre$genre), 1, 0), 
         mystery = if_else(grepl("Mystery",df_grp_genre$genre), 1, 0),
         romance = if_else(grepl("Romance",df_grp_genre$genre), 1, 0),
         scifi = if_else(grepl("Sci-Fi",df_grp_genre$genre), 1, 0),
         thriller = if_else(grepl("Thriller",df_grp_genre$genre), 1, 0),
         western = if_else(grepl("Western",df_grp_genre$genre), 1, 0),
         animation = if_else(grepl("Animation",df_grp_genre$genre), 1, 0),
         biography = if_else(grepl("Biography",df_grp_genre$genre), 1, 0),
         sport = if_else(grepl("Sport",df_grp_genre$genre), 1, 0),
         music = if_else(grepl("Music",df_grp_genre$genre), 1, 0),
         war = if_else(grepl("War",df_grp_genre$genre), 1, 0))



df_new <- df %>%
  mutate(action = if_else(grepl("Action",df$genre), 1, 0),
         adventure = if_else(grepl("Adventure",df$genre), 1, 0),
         comedy = if_else(grepl("Comedy",df$genre), 1, 0),
         crime = if_else(grepl("Crime",df$genre), 1, 0),
         drama = if_else(grepl("Drama",df$genre), 1, 0),
         family = if_else(grepl("Family",df$genre), 1, 0),
         fantasy = if_else(grepl("Fantasy",df$genre), 1, 0),
         history = if_else(grepl("History",df$genre), 1, 0),
         horror = if_else(grepl("Horror",df$genre), 1, 0), 
         mystery = if_else(grepl("Mystery",df$genre), 1, 0),
         romance = if_else(grepl("Romance",df$genre), 1, 0),
         scifi = if_else(grepl("Sci-Fi",df$genre), 1, 0),
         thriller = if_else(grepl("Thriller",df$genre), 1, 0),
         western = if_else(grepl("Western",df$genre), 1, 0),
         animation = if_else(grepl("Animation",df$genre), 1, 0),
         biography = if_else(grepl("Biography",df$genre), 1, 0),
         sport = if_else(grepl("Sport",df$genre), 1, 0),
         music = if_else(grepl("Music",df$genre), 1, 0),
         war = if_else(grepl("War",df$genre), 1, 0))

df_new <- df_new %>%
  mutate(genre_score = action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western + animation + biography + sport + music + war)
df_new

#creating a log transform usa income variable and saving it to our data

df_new$log_usa_income <- log(df_new$usa_gross_income)



#splitting data to training and testing sets-we will explore on the training set
set.seed(1234)
row.number <- sample(1:nrow(df_new), 0.7*nrow(df_new))
df_train <- df_new[row.number,]
df_test <- df_new[-row.number,]
dim(df_train)
dim(df_test)

#*************************************************EDA***********************************************************
#histograms of USA Gross Income
hist_income <- ggplot(data = df_train, aes(x = usa_gross_income)) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Movie USA Gross Income", x = "Income (in $)", y = "No. of Movies")
hist_log_income <- ggplot(data = df_train, aes(x = log(usa_gross_income))) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Movie USA Gross Income Logged", x = "Log of Income", y = "No. of Movies")
ggarrange(hist_income, hist_log_income  + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


#Histograms of Budget
hist_budget <- ggplot(data = df_train, aes(x = budget)) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Movie Budget", x = "Budget (in $)", y = "No. of Movies")
#histogram with log transformation to budget
hist_log_budget <- ggplot(data = df_train, aes(x = log(budget))) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Movie Budget Logged", x = "Log Budget", y = "No. of Movies")
ggarrange(hist_budget, hist_log_budget  + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


#Histograms of Duration
hist_duration <- ggplot(data = df_train, aes(x = duration)) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Movie Duration", x = "Duration (in minutes)", y = "No. of Movies")
hist_duration_log <- ggplot(data = df_train, aes(x = log(duration))) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Movie Duration Logged", x = "Log of Duration", y = "No. of Movies")
ggarrange(hist_duration, hist_duration_log  + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)



#Histograms of Studio Popularity
hist_studio_pop <- ggplot(data = df_train, aes(x = studio_popularity)) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Different Studios", x = "Studio Of Popularity", y = "No. of Movies")
hist_studio_pop_log <- ggplot(data = df_train, aes(x = log(studio_popularity))) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Studio Frequencies Logged", x = "Studio Of Popularity Logged", y = "No. of Movies")
ggarrange(hist_studio_pop, hist_studio_pop_log  + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


#Histograms of Movie Release Year
hist_year <- ggplot(data = df_train, aes(x = year)) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Movie Release Year", x = "Movie Release Year", y = "No. of Movies")
hist_log_year <- ggplot(data = df_train, aes(x = log(year))) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Movie Release Year Logged", x = "Log of Movie Release Year", y = "No. of Movies")
ggarrange(hist_year, hist_log_year  + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

#Histograms of US Voters Votes
hist_us_votes <- ggplot(data = df_train, aes(x = us_voters_votes)) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of US Votes", x = "US Votes", y = "No. of Movies")
hist_us_votes_log <- ggplot(data = df_train, aes(x = log(us_voters_votes))) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of US Votes Logged", x = "Log of US Votes", y = "No. of Movies")
ggarrange(hist_us_votes, hist_us_votes_log  + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)



#scatterplots of independent variables and dependent

par(mfrow=c(2,2))
#relationship between budget and income
sp_log_budg_income <- ggplot(df_train, aes(x=log(budget), y=log(usa_gross_income))) + geom_point()+
  labs(title = "Log of Budget vs Log of USA Gross Income", x = "Log of Budget", y = "Log of Income")
sp_log_budg_income 
cor.test(df_train$usa_gross_income, df_train$budget) #strong correlation- .68

#relationship between year and income
sp_log_income_year <- ggplot(df_train, aes(x=year, y=log(usa_gross_income))) + geom_point()+
  labs(title = "Year vs Log of USA Gross Income", x = "Movie Release Year", y = "Log of Income")
sp_log_income_year 
cor.test(df_train$usa_gross_income, df_train$year) #minimal- .12

#relationship between duration and income
sp_log_income_dur <- ggplot(df_train, aes(x=log(duration), y=log(usa_gross_income))) + geom_point()+
  labs(title = "Log of Duration vs Log of USA Gross Income", x = "Log of Movie Duration", y = "Log of Income")
sp_log_income_dur 
cor.test(df_train$usa_gross_income, df_train$duration) #minimal- .29

#relationship between studio_pop and income
sp_log_income_stud_pop <- ggplot(df_train, aes(x=log(studio_popularity), y=log(usa_gross_income))) + geom_point()+
  labs(title = "Log of Studio Popularity vs Log of USA Gross Income", x = "Log of Studio Populartiy", y = "Log of Income")
sp_log_income_stud_pop 
cor.test(df_train$usa_gross_income, df_train$studio_popularity) #minimal- .27

ggarrange(sp_log_budg_income, sp_log_income_year,sp_log_income_dur,sp_log_income_stud_pop  + rremove("x.text"), 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

#tables of mean usa_gross_income (our dependent variable) by director and writer
tapply(df_train$usa_gross_income, df_train$`Oscar winning director`, mean)
tapply(df_train$usa_gross_income, df_train$`Oscar winning writer`, mean)

#*************************************************MODEL BUILDING***********************************************************


##running models
options(max.print=999999)

#dependent plus our controls
mod1 <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
           + animation + biography + sport + music + war, data=df_train)

coeftest(mod1, vcov = vcovHC(mod1))
summary(mod1)


mod2 <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
           + animation + biography + sport + music + war+log(budget), data=df_train)
summary(mod2)
anova(mod1, mod2)




mod3 <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
           + animation + biography + sport + music + war+log(budget)+log(studio_popularity), data=df_train)
summary(mod3)
anova(mod2, mod3)



mod4 <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
           + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes), data=df_train)
summary(mod4)
anova(mod3, mod4)




mod4b <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
           + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+I(studio_popularity)^2
           +log(us_voters_votes), data=df_train)
summary(mod4)
anova(mod4, mod4b)




mod5 <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
           + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+factor(`Oscar winning writer`), data=df_train)
summary(mod5)
anova(mod4, mod5)




mod6 <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
           + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+ factor(`Oscar winning director`), data=df_train)
summary(mod6)
anova(mod4, mod6)


accuracy(list(mod1,mod2,mod3,mod4,mod5,mod6))


#mod4 is the best model

#choosing the best genre interactions, checking which combos occur most
as.data.frame(sort(table(df_train$genre), decreasing=TRUE))


#testing models with interaction

mod4_i <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
              + animation + biography + sport + music + war+log(budget)+log(studio_popularity)
              +log(us_voters_votes)+log(studio_popularity)*log(budget),data=df_train)
summary(mod4_i)
anova(mod4, mod4_i)



mod4_ii <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
             + animation + biography + sport + music + war+log(budget)+log(studio_popularity)
             +log(us_voters_votes)+log(studio_popularity)*log(budget)+comedy*drama,data=df_train)
summary(mod4_ii)
anova(mod4_i, mod4_ii)


mod4_iii <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
              + animation + biography + sport + music + war+log(budget)+log(studio_popularity)
              +log(us_voters_votes)+log(studio_popularity)*log(budget)+comedy*romance,data=df_train)
summary(mod4_iii)
anova(mod4_i, mod4_iii)

mod4_iv <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
               + animation + biography + sport + music + war+log(budget)+log(studio_popularity)
               +log(us_voters_votes)+log(studio_popularity)*log(budget)+drama*romance,data=df_train)
summary(mod4_iv)
anova(mod4_i, mod4_iv)



stargazer(
  mod4_iv, 
  title = "..............Regression Results for the Main Model............",
  align = FALSE,
  dep.var.labels = c("USA Gross Income"),
  single.row = TRUE,
  header = TRUE,
  type = "text",
  font.size = "tiny",
  star.cutoffs = c(0.05, 0.01, 0.001),
  out = "mainmodel.txt"
)


mod4_v <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
              + animation + biography + sport + music + war+log(budget)+log(studio_popularity)
              +log(us_voters_votes)+log(studio_popularity)*log(budget)+(drama*romance)+(comedy*drama*romance),data=df_train)
summary(mod4_v)
anova(mod4_iv, mod4_v)


accuracy(list(mod4, mod4_i, mod4_ii, mod4_iii, mod4_iv, mod4_v))


#checking our chose model's accuracy against the testing data


mod4_iv_test <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western 
              + animation + biography + sport + music + war+log(budget)+log(studio_popularity)
              +log(us_voters_votes)+log(studio_popularity)*log(budget)+drama*romance,data=df_test)
summary(mod4_iv_test)
predictions <- mod4_iv_test %>% predict(df_test)
data.frame(
  R2 =  R2(predictions, df_test$log_usa_income),
  RMSE = RMSE(predictions, df_test$log_usa_income),
  MAE = MAE(predictions, df_test$log_usa_income)
)
accuracy(list(mod4_iv_test))
#*************************************************GENRE SUBSETTING***********************************************************

#action
df_action <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                       fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_action <- df_action[!(df_action$action==0),]
df_action
modaction <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_action)
summary(modaction)

#adventure
df_adventure <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity,  `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                          fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_adventure <- df_adventure[!(df_adventure$adventure==0),]
df_adventure
modadventure <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                   + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_adventure)
summary(modadventure)

#animation
df_animation <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`,action, adventure, comedy, crime, drama, family,
                                          fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_animation <- df_animation[!(df_animation$animation==0),]
df_animation
modanimation <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                   + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_animation)
summary(modanimation)

#biography
df_biography <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                          fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_biography <- df_biography[!(df_biography$biography==0),]
df_biography
modbiography <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                   + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_biography)
summary(modbiography)

#comedy
df_comedy <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                       fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_comedy <- df_comedy[!(df_comedy$comedy==0),]
df_comedy
modcomedy <-lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
               + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget), data=df_comedy)
summary(modcomedy)

#crime
df_crime <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                      fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_crime <- df_crime[!(df_crime$crime==0),]
df_crime
modcrime <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
               + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_crime)
summary(modcrime)

#drama
df_drama <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                      fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_drama <- df_drama[!(df_drama$drama==0),]
df_drama
moddrama <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
               + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_drama)
summary(moddrama)

#family
df_family <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                       fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_family <- df_family[!(df_family$family==0),]
df_family
modfamily <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_family)
summary(modfamily)


#fantasy
df_fantasy <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                        fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_fantasy <- df_fantasy[!(df_fantasy$fantasy==0),]
df_fantasy
modfantasy <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                 + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_fantasy)
summary(modfantasy)

#history
df_history <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`,action, adventure, comedy, crime, drama, family,
                                        fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_history <- df_history[!(df_history$history==0),]
df_history
modhistory <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                 + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget), data=df_history)
summary(modhistory)

#horror
df_horror <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                       fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_horror <- df_horror[!(df_horror$horror==0),]
df_horror
modhorror <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_horror)
summary(modhorror)

#music
df_music <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                      fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_music <- df_music[!(df_music$music==0),]
df_music
modmusic <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
               + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_music)
summary(modmusic)

#mystery
df_mystery <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                        fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_mystery <- df_mystery[!(df_mystery$mystery==0),]
df_mystery
modmystery <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                 + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget), data=df_mystery)
summary(modmystery)

#romance
df_romance <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                        fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_romance <- df_romance[!(df_romance$romance==0),]
df_romance
modromance <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                 + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_romance)
summary(modromance)

#scifi
df_scifi <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                      fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_scifi <- df_scifi[!(df_scifi$scifi==0),]
df_scifi
modscifi <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
               + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_scifi)
summary(modscifi)

#sport
df_sport <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                      fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_sport <- df_sport[!(df_sport$sport==0),]
df_sport
modsport <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
               + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_sport)
summary(modsport)

#thriller
df_thriller <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                         fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_thriller <- df_thriller[!(df_thriller$thriller==0),]
df_thriller
modthriller <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                  + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_thriller)
summary(modthriller)

#war
df_war <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                    fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_war <- df_war[!(df_war$war==0),]
df_war
modwar <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
             + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget),  data=df_war)
summary(modwar)

#western
df_western <- subset(df_train, select = c(genre, usa_gross_income, year, budget, duration, studio_popularity, `Oscar winning director`, `Oscar winning writer`, action, adventure, comedy, crime, drama, family,
                                        fantasy, history, horror, mystery, romance, scifi, thriller, western, animation, biography, sport, music, war, us_voters_votes))
df_western <- df_western[!(df_western$western==0),]
df_western
modwestern <- lm(log(usa_gross_income) ~ duration+year+ action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western
                 + animation + biography + sport + music + war+log(budget)+log(studio_popularity)+log(us_voters_votes)+log(studio_popularity)*log(budget), data=df_western)
summary(modwestern)


summary(modwestern)
summary(modwar)
summary(modthriller)
summary(modsport)
summary(modscifi)
summary(modromance)
summary(modmystery)
summary(modmusic)
summary(modhorror)
summary(modhistory)
summary(modfantasy)
summary(modfamily)
summary(moddrama)
summary(modcrime)
summary(modcomedy)
summary(modbiography)
summary(modanimation)
summary(modadventure)
summary(modaction)


#create comedy dataframe
df_comedy <- df_train[!(df_train$comedy==0),]

#add new column for comedy only 
df_comedy$comedy_only  = ifelse(df_comedy$genre == 'Comedy',0,1)


mod_com <- lm(log(usa_gross_income) ~ duration + log(budget)+log(studio_popularity) + year +log(us_voters_votes)
              + factor(comedy_only), data=df_comedy)

mod_com_2 <- lm(log(usa_gross_income) ~ duration + log(budget)+log(studio_popularity) + year +log(us_voters_votes)
                + factor(genre), data=df_comedy)

summary(mod_com)
summary(mod_com_2)
#------------------------

#create horror dataframe
df_horror <- df_train[!(df_train$horror==0),]

#add new column for horror only 
df_horror$horror_only  = ifelse(df_horror$genre == 'Horror',0,1)


mod_horror <- lm(log(usa_gross_income) ~ duration + log(budget)+log(studio_popularity) + year+factor(`Oscar winning writer`)+factor(`Oscar winning director`) +log(us_voters_votes)
                 + factor(horror_only), data=df_horror)

mod_horror_2 <- lm(log(usa_gross_income) ~ duration + log(budget)+log(studio_popularity) + year+factor(`Oscar winning writer`)+factor(`Oscar winning director`) +log(us_voters_votes)
                   + factor(genre), data=df_horror)

summary(mod_horror)
summary(mod_horror_2)
#------------------------

#create action dataframe
df_action <- df_train[!(df_train$action==0),]

#add new column for action only 
df_action$action_only  = ifelse(df_action$genre == 'Action',0,1)


mod_action <- lm(log(usa_gross_income) ~ duration + log(budget)+log(studio_popularity) + year +log(us_voters_votes)
                 + factor(action_only), data=df_action)

mod_action_2 <- lm(log(usa_gross_income) ~ duration + log(budget)+log(studio_popularity) + year +log(us_voters_votes)
                   + factor(genre), data=df_action)

summary(mod_action)
summary(mod_action_2)
#------------------------

#create mystery dataframe
df_mystery <- df_train[!(df_train$mystery==0),]

#add new column for mystery only 
df_mystery$mystery_only  = ifelse(df_mystery$genre == 'Mystery',0,1)


mod_mystery <- lm(log(usa_gross_income) ~ duration + log(budget)+log(studio_popularity) + year+factor(`Oscar winning writer`)+factor(`Oscar winning director`) +log(us_voters_votes)
                  + factor(mystery_only), data=df_mystery)

mod_mystery_2 <- lm(log(usa_gross_income) ~ duration + log(budget)+log(studio_popularity) + year+factor(`Oscar winning writer`)+factor(`Oscar winning director`) +log(us_voters_votes)
                    + factor(genre), data=df_mystery)


summary(mod_mystery)
summary(mod_mystery_2)




df_action <- df_train %>%
  group_by(action) %>%
  summarise(mean(usa_gross_income))
colnames(df_action) <- c("action", "avg_income")
df_action <- df_action[(!df_action$action == 0),]

df_adventure <- df_train %>%
  group_by(adventure) %>%
  summarise(mean(usa_gross_income))
colnames(df_adventure) <- c("adventure", "avg_income")
df_adventure <- df_adventure[(!df_adventure$adventure == 0),]

df_genre <- merge(x = df_action, y = df_adventure, all = TRUE)

df_comedy <- df_train %>%
  group_by(comedy) %>%
  summarise(mean(usa_gross_income))
colnames(df_comedy) <- c("comedy", "avg_income")
df_comedy <- df_comedy[(!df_comedy$comedy == 0),]

df_genre <- merge(x = df_genre, y = df_comedy, all = TRUE)

df_crime <- df_train %>%
  group_by(crime) %>%
  summarise(mean(usa_gross_income))
colnames(df_crime) <- c("crime", "avg_income")
df_crime <- df_crime[(!df_crime$crime == 0),]

df_genre <- merge(x = df_genre, y = df_crime, all = TRUE)

df_drama <- df_train %>%
  group_by(drama) %>%
  summarise(mean(usa_gross_income))
colnames(df_drama) <- c("drama", "avg_income")
df_drama <- df_drama[(!df_drama$drama == 0),]

df_genre <- merge(x = df_genre, y = df_drama, all = TRUE)

df_family <- df_train %>%
  group_by(family) %>%
  summarise(mean(usa_gross_income))
colnames(df_family) <- c("family", "avg_income")
df_family <- df_family[(!df_family$family == 0),]

df_genre <- merge(x = df_genre, y = df_family, all = TRUE)

df_fantasy <- df_train %>%
  group_by(fantasy) %>%
  summarise(mean(usa_gross_income))
colnames(df_fantasy) <- c("fantasy", "avg_income")
df_fantasy <- df_fantasy[(!df_fantasy$fantasy == 0),]

df_genre <- merge(x = df_genre, y = df_fantasy, all = TRUE)

df_history <- df_train %>%
  group_by(history) %>%
  summarise(mean(usa_gross_income))
colnames(df_history) <- c("history", "avg_income")
df_history <- df_history[(!df_history$history == 0),]

df_genre <- merge(x = df_genre, y = df_history, all = TRUE)

df_horror <- df_train %>%
  group_by(horror) %>%
  summarise(mean(usa_gross_income))
colnames(df_horror) <- c("horror", "avg_income")
df_horror <- df_horror[(!df_horror$horror == 0),]

df_genre <- merge(x = df_genre, y = df_horror, all = TRUE)

df_mystery <- df_train %>%
  group_by(mystery) %>%
  summarise(mean(usa_gross_income))
colnames(df_mystery) <- c("mystery", "avg_income")
df_mystery <- df_mystery[(!df_mystery$mystery == 0),]

df_genre <- merge(x = df_genre, y = df_mystery, all = TRUE)

df_romance <- df_train %>%
  group_by(romance) %>%
  summarise(mean(usa_gross_income))
colnames(df_romance) <- c("romance", "avg_income")
df_romance <- df_romance[(!df_romance$romance == 0),]

df_genre <- merge(x = df_genre, y = df_romance, all = TRUE)

df_scifi <- df_train %>%
  group_by(scifi) %>%
  summarise(mean(usa_gross_income))
colnames(df_scifi) <- c("scifi", "avg_income")
df_scifi <- df_scifi[(!df_scifi$scifi == 0),]

df_genre <- merge(x = df_genre, y = df_scifi, all = TRUE)

df_thriller <- df_train %>%
  group_by(thriller) %>%
  summarise(mean(usa_gross_income))
colnames(df_thriller) <- c("thriller", "avg_income")
df_thriller <- df_thriller[(!df_thriller$thriller == 0),]

df_genre <- merge(x = df_genre, y = df_thriller, all = TRUE)

df_western <- df_train %>%
  group_by(western) %>%
  summarise(mean(usa_gross_income))
colnames(df_western) <- c("western", "avg_income")
df_western <- df_western[(!df_western$western == 0),]

df_genre <- merge(x = df_genre, y = df_western, all = TRUE)

df_animation <- df_train %>%
  group_by(animation) %>%
  summarise(mean(usa_gross_income))
colnames(df_animation) <- c("animation", "avg_income")
df_animation <- df_animation[(!df_animation$animation == 0),]

df_genre <- merge(x = df_genre, y = df_animation, all = TRUE)

df_biography <- df_train %>%
  group_by(biography) %>%
  summarise(mean(usa_gross_income))
colnames(df_biography) <- c("biography", "avg_income")
df_biography <- df_biography[(!df_biography$biography == 0),]

df_genre <- merge(x = df_genre, y = df_biography, all = TRUE)

df_sport <- df_train %>%
  group_by(sport) %>%
  summarise(mean(usa_gross_income))
colnames(df_sport) <- c("sport", "avg_income")
df_sport <- df_sport[(!df_sport$sport == 0),]

df_genre <- merge(x = df_genre, y = df_sport, all = TRUE)

df_music <- df_train %>%
  group_by(music) %>%
  summarise(mean(usa_gross_income))
colnames(df_music) <- c("music", "avg_income")
df_music <- df_music[(!df_music$music == 0),]

df_genre <- merge(x = df_genre, y = df_music, all = TRUE)

df_war <- df_train %>%
  group_by(war) %>%
  summarise(mean(usa_gross_income))
colnames(df_war) <- c("war", "avg_income")
df_war <- df_war[(!df_war$war == 0),]

df_genre <- merge(x = df_genre, y = df_war, all = TRUE)

df_genre[is.na(df_genre)] <- 0

df_genre <- df_genre %>%
  mutate(genre_type = action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western + animation + biography + sport + music + war)

df_genre <- df_genre[c("avg_income", "genre_type")]

df_genre_income <- df_genre %>%
  mutate(genre = case_when(
    genre_type == 1 ~ "action",
    genre_type == 2 ~ "adventure",
    genre_type == 3 ~ "comedy",
    genre_type == 4 ~ "crime",
    genre_type == 5 ~ "drama",
    genre_type == 6 ~ "family",
    genre_type == 7 ~ "fantasy",
    genre_type == 8 ~ "history",
    genre_type == 9 ~ "horror",
    genre_type == 10 ~ "mystery",
    genre_type == 11 ~ "romance",
    genre_type == 12 ~ "scifi",
    genre_type == 13 ~ "thriller",
    genre_type == 14 ~ "western",
    genre_type == 15 ~ "animation",
    genre_type == 16 ~ "biography",
    genre_type == 17 ~ "sport",
    genre_type == 18 ~ "music",
    genre_type == 19 ~ "war"
  ))


df_action <- df_train %>%
  group_by(action) %>%
  summarise(mean(budget))
colnames(df_action) <- c("action", "avg_budget")
df_action <- df_action[(!df_action$action == 0),]

df_adventure <- df_train %>%
  group_by(adventure) %>%
  summarise(mean(budget))
colnames(df_adventure) <- c("adventure", "avg_budget")
df_adventure <- df_adventure[(!df_adventure$adventure == 0),]

df_genre <- merge(x = df_action, y = df_adventure, all = TRUE)

df_comedy <- df_train %>%
  group_by(comedy) %>%
  summarise(mean(budget))
colnames(df_comedy) <- c("comedy", "avg_budget")
df_comedy <- df_comedy[(!df_comedy$comedy == 0),]

df_genre <- merge(x = df_genre, y = df_comedy, all = TRUE)

df_crime <- df_train %>%
  group_by(crime) %>%
  summarise(mean(budget))
colnames(df_crime) <- c("crime", "avg_budget")
df_crime <- df_crime[(!df_crime$crime == 0),]

df_genre <- merge(x = df_genre, y = df_crime, all = TRUE)

df_drama <- df_train %>%
  group_by(drama) %>%
  summarise(mean(budget))
colnames(df_drama) <- c("drama", "avg_budget")
df_drama <- df_drama[(!df_drama$drama == 0),]

df_genre <- merge(x = df_genre, y = df_drama, all = TRUE)

df_family <- df_train %>%
  group_by(family) %>%
  summarise(mean(budget))
colnames(df_family) <- c("family", "avg_budget")
df_family <- df_family[(!df_family$family == 0),]

df_genre <- merge(x = df_genre, y = df_family, all = TRUE)

df_fantasy <- df_train %>%
  group_by(fantasy) %>%
  summarise(mean(budget))
colnames(df_fantasy) <- c("fantasy", "avg_budget")
df_fantasy <- df_fantasy[(!df_fantasy$fantasy == 0),]

df_genre <- merge(x = df_genre, y = df_fantasy, all = TRUE)

df_history <- df_train %>%
  group_by(history) %>%
  summarise(mean(budget))
colnames(df_history) <- c("history", "avg_budget")
df_history <- df_history[(!df_history$history == 0),]

df_genre <- merge(x = df_genre, y = df_history, all = TRUE)

df_horror <- df_train %>%
  group_by(horror) %>%
  summarise(mean(budget))
colnames(df_horror) <- c("horror", "avg_budget")
df_horror <- df_horror[(!df_horror$horror == 0),]

df_genre <- merge(x = df_genre, y = df_horror, all = TRUE)

df_mystery <- df_train %>%
  group_by(mystery) %>%
  summarise(mean(budget))
colnames(df_mystery) <- c("mystery", "avg_budget")
df_mystery <- df_mystery[(!df_mystery$mystery == 0),]

df_genre <- merge(x = df_genre, y = df_mystery, all = TRUE)

df_romance <- df_train %>%
  group_by(romance) %>%
  summarise(mean(budget))
colnames(df_romance) <- c("romance", "avg_budget")
df_romance <- df_romance[(!df_romance$romance == 0),]

df_genre <- merge(x = df_genre, y = df_romance, all = TRUE)

df_scifi <- df_train %>%
  group_by(scifi) %>%
  summarise(mean(budget))
colnames(df_scifi) <- c("scifi", "avg_budget")
df_scifi <- df_scifi[(!df_scifi$scifi == 0),]

df_genre <- merge(x = df_genre, y = df_scifi, all = TRUE)

df_thriller <- df_train %>%
  group_by(thriller) %>%
  summarise(mean(budget))
colnames(df_thriller) <- c("thriller", "avg_budget")
df_thriller <- df_thriller[(!df_thriller$thriller == 0),]

df_genre <- merge(x = df_genre, y = df_thriller, all = TRUE)

df_western <- df_train %>%
  group_by(western) %>%
  summarise(mean(budget))
colnames(df_western) <- c("western", "avg_budget")
df_western <- df_western[(!df_western$western == 0),]

df_genre <- merge(x = df_genre, y = df_western, all = TRUE)

df_animation <- df_train %>%
  group_by(animation) %>%
  summarise(mean(budget))
colnames(df_animation) <- c("animation", "avg_budget")
df_animation <- df_animation[(!df_animation$animation == 0),]

df_genre <- merge(x = df_genre, y = df_animation, all = TRUE)

df_biography <- df_train %>%
  group_by(biography) %>%
  summarise(mean(budget))
colnames(df_biography) <- c("biography", "avg_budget")
df_biography <- df_biography[(!df_biography$biography == 0),]

df_genre <- merge(x = df_genre, y = df_biography, all = TRUE)

df_sport <- df_train %>%
  group_by(sport) %>%
  summarise(mean(budget))
colnames(df_sport) <- c("sport", "avg_budget")
df_sport <- df_sport[(!df_sport$sport == 0),]

df_genre <- merge(x = df_genre, y = df_sport, all = TRUE)

df_music <- df_train %>%
  group_by(music) %>%
  summarise(mean(budget))
colnames(df_music) <- c("music", "avg_budget")
df_music <- df_music[(!df_music$music == 0),]

df_genre <- merge(x = df_genre, y = df_music, all = TRUE)

df_war <- df_train %>%
  group_by(war) %>%
  summarise(mean(budget))
colnames(df_war) <- c("war", "avg_budget")
df_war <- df_war[(!df_war$war == 0),]

df_genre <- merge(x = df_genre, y = df_war, all = TRUE)

df_genre[is.na(df_genre)] <- 0

df_genre <- df_genre %>%
  mutate(genre_type = action + adventure + comedy + crime + drama + family + fantasy + history + horror + mystery + romance + scifi + thriller + western + animation + biography + sport + music + war)



df_genre_budget <- df_genre %>%
  mutate(genre = case_when(
    genre_type == 1 ~ "action",
    genre_type == 2 ~ "adventure",
    genre_type == 3 ~ "comedy",
    genre_type == 4 ~ "crime",
    genre_type == 5 ~ "drama",
    genre_type == 6 ~ "family",
    genre_type == 7 ~ "fantasy",
    genre_type == 8 ~ "history",
    genre_type == 9 ~ "horror",
    genre_type == 10 ~ "mystery",
    genre_type == 11 ~ "romance",
    genre_type == 12 ~ "scifi",
    genre_type == 13 ~ "thriller",
    genre_type == 14 ~ "western",
    genre_type == 15 ~ "animation",
    genre_type == 16 ~ "biography",
    genre_type == 17 ~ "sport",
    genre_type == 18 ~ "music",
    genre_type == 19 ~ "war"
  ))

plot_26 <- ggplot(data = df_genre_budget, aes(x = genre, y = avg_budget/1000000)) + 
  geom_point() +
  labs(x = "Genre", y = "Avg. Budget ($M)", las = 0) + 
  theme(axis.text.x = element_text(angle = 90))

df_genre_comb <- merge(x = df_genre_income, y = df_genre_budget, all = TRUE)
df_genre_comb <- df_genre_comb %>%
  mutate(rate_of_return = ((avg_income/avg_budget)*100))

plot_27 <- ggplot(data = df_genre_comb, aes(x = genre, y = rate_of_return)) + 
  geom_point() +
  labs(x = "Genre", y = "Rate of Return (%)", las = 0) + 
  theme(axis.text.x = element_text(angle = 90))



