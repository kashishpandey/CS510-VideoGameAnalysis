library(tidyverse)
library(ggplot2)
library(tree) 
library(caret) 
library(elasticnet)
library(corrplot) 

# na.strings is removing null/blank values within the dataset
vg_sales <- read.csv("dataset/Video_Games_Sales_as_at_22_Dec_2016.csv",
                        sep=",",na.strings=c(""," ","NA","N/A"))
vg_sales

# viewing the first 5 lines of the csv file 
head(vg_sales)

# general summary of dataset
summary(vg_sales)

# checking number of null values within the dataset
colSums(is.na(vg_sales))

# dropping NA values from dataset
# many missing values within this dataset
# it is the combination of 2 different datasets and many of the original observations
# did not match the data from the second set
vg_sales <- vg_sales[complete.cases(vg_sales), ]
colSums(is.na(vg_sales))

# getting internal structure of each feature
str(vg_sales)

# examining outlier data for sales 
summary(vg_sales$NA_Sales)
summary(vg_sales$EU_Sales)
summary(vg_sales$JP_Sales)
summary(vg_sales$Other_Sales)
summary(vg_sales$Global_Sales)

# examining outlier data for score/count
summary(vg_sales$Critic_Score)
summary(vg_sales$Critic_Count)
summary(vg_sales$User_Count)
summary(vg_sales$User_Score)

# critic score is int and user score is num 
# changing user score to int to keep it consistent 
vg_sales$User_Score <- as.integer(vg_sales$User_Score)
summary(vg_sales$User_Score)

# putting critic score and user score on the same scale
# user score was only out of 10; critic was out of 100
vg_sales$User_Score <- vg_sales$User_Score * 10

# rating variable 
# there is only 1 occurrence of AO, K-A, and RP
# going to add AO, K-A, and RP into Mature rating + Everyone rating 
vg_sales %>% count(Rating)

vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "AO", "M", Rating))
vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "K-A", "E", Rating))
vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "RP", "E", Rating))
vg_sales %>% count(Rating)

## data visualization ##

# number of games per rating
# teen games have the highest global sales
rating_games_bar <- ggplot(vg_sales, aes(x=Rating,fill =Rating)) + geom_bar() + 
      theme(text = element_text(size=10)) + xlab("Rating") + ylab("Global sales")
rating_games_bar

# sales for each platform
# biggest sales from playstation 2 and xbox360
vg_sales %>% group_by(Platform) %>% 
  summarise(vg_sales = sum(Global_Sales)) %>% ggplot() + 
  geom_bar(aes(reorder(Platform, vg_sales), vg_sales), stat = "identity", 
           fill = "#063970") + 
  xlab("Platform") + ylab("Global sales") + 
  coord_flip() + theme_minimal()

# sales by genre
# action, sports, shooters are the top genres
vg_sales %>% group_by(Genre) %>% 
  summarise(vg_sales = sum(Global_Sales)) %>% ggplot() + 
  geom_bar(aes(reorder(Genre, vg_sales), vg_sales), stat = "identity", 
           fill = "#063970") + 
  xlab("Genre") + ylab("Global sales") + 
  coord_flip() + theme_minimal()


# sales in North America, Europe, Japan, Other
# north america had the highest overall sales from 1990-2016
vg_sales %>% gather(area, vg_sales, NA_Sales:Other_Sales, 
                    factor_key = TRUE) %>% 
  group_by(area,Year_of_Release) %>% 
  summarise(vg_sales = sum(vg_sales)) %>% ggplot() + 
  xlab("Year of release") + ylab("Sales") + 
  geom_line(aes(Year_of_Release, vg_sales, group = area, color = area)) + 
  theme_minimal() + theme(legend.text = element_text(size = 7), 
                          legend.position = "bottom",
                          axis.text.x = element_text(angle = 90))+
  theme_minimal()

# top 10 best selling games globally 
# wii sports is the #1 game sold globally
vg_sales %>% select(Name,Global_Sales) %>% arrange(desc(Global_Sales))%>% head(10)%>%
  ggplot(aes(x=Name,y=Global_Sales,fill= Name))+geom_bar(stat="identity")+ 
  labs(x="Game Name",y="Global Sales",
       title="Top 10 best selling games")+
  theme(text = element_text(size=7),legend.position="right",
        axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=7))+
  scale_fill_brewer(name= "Game Titles",palette="RdYlGn") 

# bar plot of global sales
# extremely skewed plot, need to change x axis to log axis 
ggplot(vg_sales) + geom_histogram(aes(Global_Sales), fill = "#063970")

# fixing axis, better distribution - similar to gaussian distribution 
ggplot(vg_sales) + geom_histogram(aes(Global_Sales), fill = "#063970") + 
  scale_x_log10()

# number of titles released each year
# there seems to be a peak within the data 
vg_sales %>% group_by(Year_of_Release) %>% 
  count() %>% ggplot() + 
  geom_bar(aes(Year_of_Release, n), stat = "identity", 
           fill = "#063970") + theme(axis.text.x = element_text(angle = 90))

# sales each yr vs number of releases 
# more revenue when more titles are released
color <- c("Titles released" = "maroon4", "Global sales" = "royalblue")
vg_sales %>% group_by(Year_of_Release) %>% 
  summarise(vg_sales = sum(Global_Sales), count = n()) %>% 
  ggplot() + xlab("Year of Release") + ylab("Titles released") +
  geom_line(aes(Year_of_Release, count, group = 1, color = "Titles released")) + 
  geom_line(aes(Year_of_Release, vg_sales, group = 1, color = "Global sales")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  scale_color_manual(name="",values = color) + theme_minimal()

# combining platform by company - to simplify all these platforms
vg_sales <- vg_sales %>% mutate(platform2 = case_when(
  Platform %in% c("Wii", "DS", "3DS", "WiiU", "GC", "GBA") ~ "Nintendo",
  Platform %in% c("X360", "XB", "XOne") ~ "XBox",
  Platform %in% c("PS3", "PS4", "PS2", "PS", "PSP", "PSV") ~ "PS",
  Platform == "PC" ~ "PC",
  Platform == "DC" ~ "Sega"
))

# global sales each year for each platform
# nintendo and playstation both peaked around the same time
vg_sales %>% group_by(platform2, Year_of_Release) %>%
  summarise(vg_sales = sum(Global_Sales)) %>% 
  ggplot() + xlab("Year of release") + ylab("Global Sales") + 
  geom_line(aes(Year_of_Release, vg_sales, group = platform2, color = platform2)) +
  theme(legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5, size = 6))+
  theme_minimal() + labs(color='Platforms') 

# sales for each developer
# need to change individual bar colors
vg_sales %>% group_by(Developer) %>% 
  summarise(vg_sales = sum(Global_Sales)) %>% 
  arrange(desc(vg_sales)) %>% slice(1:10) %>% 
  ggplot() + xlab("Developer") + ylab("Global sales")+
  geom_bar(aes(reorder(Developer, vg_sales), vg_sales), 
           stat = "identity", fill = "#063970") +
  theme(axis.text.x = element_text(angle = 90)) 


# sales for each gaming genre
# need to change individual bar colors
vg_sales %>% group_by(Genre) %>%
  summarise(vg_sales = sum(Global_Sales)) %>%  
  ggplot() + 
  geom_bar(aes(reorder(Genre, vg_sales), vg_sales), stat = "identity", 
           fill = "#063970") + 
  ylab("Global Sales") + xlab("Genre") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5)) 

# sales for each platform in each genre
# xbox 360 - shooter
# ps3 - action
vg_sales %>% group_by(Platform, Genre) %>% 
  summarise(vg_sales = sum(Global_Sales)) %>% 
  ggplot() + geom_raster(aes(Genre, Platform, fill = vg_sales)) + 
  ylab("") + xlab("") + 
  scale_fill_gradient(low = "#e4dee5", high = "blue") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 5),
        legend.text = element_text(size = 7)) + labs(fill = "Global Sales")


# MODELS for results 

# Overall, the sales vary depending on the platform, release year, and 
# developer 

# The top developers had the highest sales 

# publishers is categorical but has many values
publishers_top <- (vg_sales %>% group_by(Publisher) %>%
                     summarise(vg_sales = sum(Global_Sales)) %>% arrange(desc(vg_sales)) %>% 
                     top_n(10) %>% distinct(Publisher))$Publisher

# developers is categorical but has many values
developers_top <- (vg_sales %>% group_by(Developer) %>%
                     summarise(vg_sales = sum(Global_Sales)) %>% arrange(desc(vg_sales)) %>% 
                     top_n(10) %>% distinct(Developer))$Developer



# creating new variable for whether a game is created by a top developer/publisher
vg_sales <- vg_sales %>% 
  mutate(publisher_top = ifelse(Publisher %in% publishers_top, TRUE, FALSE),
         developer_top = ifelse(Developer %in% developers_top, TRUE, FALSE))

# whether games are exclusively launched on a specific platform
vg_sales <- vg_sales %>% group_by(Name) %>% mutate(num_of_platforms = n()) %>% ungroup(Name)


# training and testing data sets
set.seed(2000, sample.kind = "Rounding")

test_index <- createDataPartition(vg_sales$Global_Sales, p = 0.9, list = FALSE)
train_set <- vg_sales[-test_index, ]
test_set <- vg_sales[test_index, ]


# including categorical data as well
totalData <- rbind(train_set, test_set)
for (f in 1:length(names(totalData))) {
  levels(train_set[, f]) <- levels(totalData[, f])
}


# creating RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# linear regression model 
# base line model 
model_lm <- train(log(Global_Sales) ~ Critic_Score + 
                    User_Score + Genre + 
                    Year_of_Release + Platform + Critic_Count +
                    User_Count + Rating + 
                    publisher_top + developer_top + 
                    num_of_platforms, method = "lm", data = train_set)

# predicted values and RMSE 
test_set$predicted_lm <- predict(model_lm, test_set)
rmse_results <- data.frame(Method = "Linear Regression", 
                           RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_lm))

# r^2: 0.5316 
summary(model_lm)

ggplot(test_set) + 
  geom_point(aes(log(Global_Sales), predicted_lm)) + 
  geom_line(aes(log(Global_Sales), log(Global_Sales))) + 
  xlab("Actual values") + ylab("Predicted values")

# residual plot (error vs predicted)
# errors are largest for larger values of global sales - heteroskedacity
ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_lm, Global_Sales)) + 
  xlab("Error") + ylab("Global sales")


# SVM Linear
model_svm_linear <- train(log(Global_Sales) ~ Critic_Score + 
                     User_Score + Genre + 
                     Year_of_Release + Platform + Critic_Count +
                     User_Count + Rating + 
                     publisher_top + developer_top + 
                     num_of_platforms, method = "svmLinear",
                   data = train_set)

# predicted value and RMSE 
test_set$predicted_svm_linear <- predict(model_svm_linear, test_set)
rmse_results <- rmse_results %>% 
  add_row(Method = "SVM Linear", 
          RMSE = RMSE(log(test_set$Global_Sales), 
                      test_set$predicted_svm_linear))

summary(model_svm_linear)

# SVM poly 
# might take several minutes to run because it is
# more mathematically complex (polynomial function)
model_svm_poly <- train(log(Global_Sales) ~ Critic_Score + 
                     User_Score + Genre + 
                     Year_of_Release + Platform + Critic_Count +
                     User_Count + Rating + 
                     publisher_top + developer_top + 
                     num_of_platforms, method = "svmPoly",
                   data = train_set)

# predicted value and RMSE 
test_set$predicted_svm_poly <- predict(model_svm_poly, test_set)
rmse_results <- rmse_results %>% 
  add_row(Method = "SVM Polynomial", 
          RMSE = RMSE(log(test_set$Global_Sales), 
                      test_set$predicted_svm_poly))

summary(model_svm_poly)

# SVM radial 
model_svm_rad <- train(log(Global_Sales) ~ Critic_Score + 
                          User_Score + Genre + 
                          Year_of_Release + Platform + Critic_Count +
                          User_Count + Rating + 
                          publisher_top + developer_top + 
                          num_of_platforms, method = "svmRadial",
                        data = train_set)

# predicted value and RMSE 
test_set$predicted_svm_rad<- predict(model_svm_rad, test_set)
rmse_results <- rmse_results %>% 
  add_row(Method = "SVM Radial", 
          RMSE = RMSE(log(test_set$Global_Sales), 
                      test_set$predicted_svm_rad))

summary(model_svm_rad)

rmse_results

# L1 - lasso model
model_l1 <- train(log(Global_Sales) ~ Critic_Score +
                    User_Score + Genre +
                    Year_of_Release + Platform + Critic_Count +
                    User_Count + Rating +
                    publisher_top + developer_top +
                    num_of_platforms, method = "lasso", data = train_set)

# predicted values and RMSE
test_set$predicted_l1 <- predict(model_l1, test_set)
rmse_results <- rmse_results %>% add_row(Method = "L1 Lasso",
                                         RMSE = RMSE(log(test_set$Global_Sales), 
                                                     test_set$predicted_l1))
summary(model_l1)

ggplot(test_set) +
  geom_point(aes(log(Global_Sales), predicted_l1)) +
  geom_line(aes(log(Global_Sales), log(Global_Sales))) +
  xlab("Actual values") + ylab("Predicted values")

ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_l1, Global_Sales)) +
  xlab("Error") + ylab("Global sales")

# L2 - ridge model
model_l2 <- train(log(Global_Sales) ~ Critic_Score +
                    User_Score + Genre +
                    Year_of_Release + Platform + Critic_Count +
                    User_Count + Rating +
                    publisher_top + developer_top +
                    num_of_platforms, method = "ridge", data = train_set)

# predicted values and RMSE
test_set$predicted_l2 <- predict(model_l2, test_set)
rmse_results <- rmse_results %>% add_row (Method = "L2 Ridge",
                           RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_l2))
summary(model_l2)

ggplot(test_set) +
  geom_point(aes(log(Global_Sales), predicted_l2)) +
  geom_line(aes(log(Global_Sales), log(Global_Sales))) +
  xlab("Actual values") + ylab("Predicted values")

ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_l2, Global_Sales)) +
  xlab("Error") + ylab("Global sales")


# random forest
# this one will also take a few minutes to run 
cntrl <- trainControl(method = "repeatedcv", number = 10,
                      repeats = 3)
tunegrid <- expand.grid(.mtry=c(1:5),
                        .min.node.size = seq(1, 5, 1),
                        .splitrule = c("extratrees", "variance"))
model_rf <- train(log(Global_Sales) ~ Critic_Score +
                    User_Score + Genre +
                    Year_of_Release + Platform + Critic_Count +
                    User_Count + Rating +
                    publisher_top + developer_top +
                    num_of_platforms, data = train_set,
                  method = "ranger", trControl = cntrl,
                  tuneGrid = tunegrid)

ggplot(test_set) +
  geom_point(aes(log(Global_Sales), predicted_rf)) +
  geom_line(aes(log(Global_Sales), log(Global_Sales))) +
  xlab("Actual values") + ylab("Predicted values") +
  labs(caption =
         paste("R-squared",
               format(model_rf$finalModel$r.squared,
                      digits = 2)))

ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_rf, Global_Sales)) +
  xlab("Error") + ylab("Global sales")

# predicted and RMSE
test_set$predicted_rf <- predict(model_rf, test_set)
rmse_results <- rmse_results %>% add_row(Method = "Random Forest",
                RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_rf))

# compare the RMSE values of each model
rmse_results


# plotting and comparing all the models RMSE's 
rmse_plot <- ggplot(rmse_results, aes(x=RMSE,y=Method, fill = Method))+geom_bar(stat="identity")+
  xlab("RMSE") + ylab("Model Type")
theme(text = element_text(size=10), 
      legend.position="right",
      axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=8))+
  geom_col(position = "dodge") +
  geom_text(aes(label = rmse_results$RMSE), vjust = -0.2, color = "black")
rmse_plot