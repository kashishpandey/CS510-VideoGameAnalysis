## --------------------------------------------------------------------------------------------------------
my_packages <- c("tidyverse","testthat","ggplot2", "tree","caret","elasticnet",
                 "corrplot","kernlab")      
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    
if(length(not_installed)) install.packages(not_installed)               
print(paste(length(not_installed), "packages had to be installed."))   

library(tidyverse)
library(testthat)
# For plotting  
library(ggplot2)
# Random Forest Model
library(tree)
# Regression Model
library(caret)
# Lasso and Ridge Models
library(elasticnet)
# Correlation Plot 
library(corrplot)
# SVM Models (linear,poly,radial)
library(kernlab)


## --------------------------------------------------------------------------------------------------------

vg_sales <- read.csv("dataset/Video_Games_Sales_as_at_22_Dec_2016.csv",
                        sep=",",na.strings=c(""," ","NA","N/A"))



## --------------------------------------------------------------------------------------------------------
head(vg_sales)



## --------------------------------------------------------------------------------------------------------
colSums(is.na(vg_sales))



## --------------------------------------------------------------------------------------------------------
vg_sales <- vg_sales[complete.cases(vg_sales), ]
colSums(is.na(vg_sales))



## --------------------------------------------------------------------------------------------------------
str(vg_sales)



## --------------------------------------------------------------------------------------------------------
summary(vg_sales$NA_Sales)
summary(vg_sales$EU_Sales)
summary(vg_sales$JP_Sales)
summary(vg_sales$Other_Sales)
summary(vg_sales$Global_Sales)



## --------------------------------------------------------------------------------------------------------
summary(vg_sales$Critic_Score)
summary(vg_sales$Critic_Count)
summary(vg_sales$User_Count)
summary(vg_sales$User_Score)



## --------------------------------------------------------------------------------------------------------
vg_sales$User_Score <- as.integer(vg_sales$User_Score)
summary(vg_sales$User_Score)



## --------------------------------------------------------------------------------------------------------
vg_sales$User_Score <- vg_sales$User_Score * 10



## --------------------------------------------------------------------------------------------------------
vg_sales %>% count(Rating)

vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "AO", "M", Rating))
vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "K-A", "E", Rating))
vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "RP", "E", Rating))
vg_sales %>% count(Rating)


## --------------------------------------------------------------------------------------------------------
rating_games_bar <- ggplot(vg_sales, aes(x = Rating,fill = Rating)) + geom_bar() + 
      theme(text = element_text(size=10)) + xlab("Rating") + ylab("Global sales")+
  theme_minimal() + ggtitle("Game Rating and Global Sales") 
 
rating_games_bar


## --------------------------------------------------------------------------------------------------------
vg_sales %>% group_by(Platform) %>% 
  summarise(vg_sales = sum(Global_Sales)) %>% ggplot() + 
  geom_bar(aes(reorder(Platform, vg_sales), vg_sales), stat = "identity", 
           fill = "#645188") + 
  xlab("Platform") + ylab("Global sales") + 
  coord_flip() + theme_minimal() + ggtitle("Game Platform and Global Sales") 


## --------------------------------------------------------------------------------------------------------
vg_sales %>% group_by(Genre) %>% 
  summarise(vg_sales = sum(Global_Sales)) %>% ggplot() + 
  geom_bar(aes(reorder(Genre, vg_sales), vg_sales), stat = "identity", 
           fill = "#317256") + 
  xlab("Genre") + ylab("Global sales") + 
  coord_flip() + theme_minimal() +
  ggtitle("Game Genre and Global Sales") 



## --------------------------------------------------------------------------------------------------------
vg_sales %>% gather(area, vg_sales, NA_Sales:Other_Sales, 
                    factor_key = TRUE) %>% 
  group_by(area,Year_of_Release) %>% 
  summarise(vg_sales = sum(vg_sales)) %>% ggplot() + 
  xlab("Year of Release") + ylab("Sales") + 
  geom_line(aes(Year_of_Release, vg_sales, group = area, color = area)) + 
  theme_minimal() + theme(legend.text = element_text(size = 7), 
                          legend.position = "bottom",
                          axis.text.x = element_text(angle = 90))+
  theme_minimal() + ggtitle("Release Year and Global Sales by Region") 


## --------------------------------------------------------------------------------------------------------
vg_sales %>% select(Name,Global_Sales) %>% arrange(desc(Global_Sales))%>% head(10)%>%
  ggplot(aes(x=Name,y=Global_Sales,fill= Name))+geom_bar(stat="identity")+ 
  labs(x="Game Title",y="Global Sales",
       title="Top 10 Best Selling Games")+
  theme(text = element_text(size=7),legend.position="right",
        axis.text.x=element_text(angle = 90,vjust = 0.5, hjust = 1,size=7))+
  scale_fill_brewer(name= "Game Titles", palette="Paired")



## --------------------------------------------------------------------------------------------------------
ggplot(vg_sales) + geom_histogram(aes(Global_Sales), fill = "#063970")+ ggtitle("Global Sales")+ theme_minimal()


## --------------------------------------------------------------------------------------------------------
ggplot(vg_sales) + geom_histogram(aes(Global_Sales), fill = "#063970") + 
  scale_x_log10() + ggtitle("Global Sales (with scaled axis)")+ theme_minimal()


## --------------------------------------------------------------------------------------------------------
vg_sales %>% group_by(Year_of_Release) %>% 
  count() %>% ggplot() + 
  geom_bar(aes(Year_of_Release, n), stat = "identity", 
           fill = "#063970") + theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Total Number of Titles Released Each Year")+
  theme_minimal()



## --------------------------------------------------------------------------------------------------------
color <- c("Titles released" = "maroon4", "Global sales" = "royalblue")
vg_sales %>% group_by(Year_of_Release) %>% 
  summarise(vg_sales = sum(Global_Sales), count = n()) %>% 
  ggplot() + xlab("Year of Release") + ylab("Titles released") +
  geom_line(aes(Year_of_Release, count, group = 1, color = "Titles released")) + 
  geom_line(aes(Year_of_Release, vg_sales, group = 1, color = "Global sales")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  scale_color_manual(name="",values = color) + theme_minimal() + ggtitle("Sales Each Year and Total Number of Titles Released")


## --------------------------------------------------------------------------------------------------------
vg_sales <- vg_sales %>% mutate(platform2 = case_when(
  Platform %in% c("Wii", "DS", "3DS", "WiiU", "GC", "GBA") ~ "Nintendo",
  Platform %in% c("X360", "XB", "XOne") ~ "XBox",
  Platform %in% c("PS3", "PS4", "PS2", "PS", "PSP", "PSV") ~ "PS",
  Platform == "PC" ~ "PC",
  Platform == "DC" ~ "Sega"
))



## --------------------------------------------------------------------------------------------------------
vg_sales %>% group_by(platform2, Year_of_Release) %>%
  summarise(vg_sales = sum(Global_Sales)) %>% 
  ggplot() + xlab("Year of release") + ylab("Global Sales") + 
  geom_line(aes(Year_of_Release, vg_sales, group = platform2, color = platform2)) +
  theme(legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5, size = 6))+
  theme_minimal() + labs(color='Platforms') + ggtitle("Global Sales Each Year Per Platform")


## --------------------------------------------------------------------------------------------------------
vg_sales %>% group_by(Developer) %>% 
  summarise(vg_sales = sum(Global_Sales)) %>% 
  arrange(desc(vg_sales)) %>% slice(1:10) %>% 
  ggplot() + xlab("Developer") + ylab("Global Sales")+
  geom_bar(aes(reorder(Developer, vg_sales), vg_sales), 
           stat = "identity", fill = "#0095B6") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Global Sales for Each Developer")


## --------------------------------------------------------------------------------------------------------
vg_sales %>% group_by(Genre) %>%
  summarise(vg_sales = sum(Global_Sales)) %>%  
  ggplot() + 
  geom_bar(aes(reorder(Genre, vg_sales), vg_sales), stat = "identity", 
           fill = "#BC4B4B") + 
  ylab("Global Sales") + xlab("Genre") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5)) +
  ggtitle("Genre and Global Sales")



## --------------------------------------------------------------------------------------------------------
vg_sales %>% group_by(Platform, Genre) %>% 
  summarise(vg_sales = sum(Global_Sales)) %>% 
  ggplot() + geom_raster(aes(Genre, Platform, fill = vg_sales)) + 
  ylab("") + xlab("") + 
  scale_fill_gradient(low = "#e4dee5", high = "blue") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 5),
        legend.text = element_text(size = 7)) + labs(fill = "Global Sales")+
  ggtitle("Global Sales For Each Platform and Genre")


## --------------------------------------------------------------------------------------------------------
publishers_top <- (vg_sales %>% group_by(Publisher) %>%
                     summarise(vg_sales = sum(Global_Sales)) %>% arrange(desc(vg_sales)) %>% 
                     top_n(10) %>% distinct(Publisher))$Publisher



## --------------------------------------------------------------------------------------------------------
developers_top <- (vg_sales %>% group_by(Developer) %>%
                     summarise(vg_sales = sum(Global_Sales)) %>% arrange(desc(vg_sales)) %>% 
                     top_n(10) %>% distinct(Developer))$Developer



## --------------------------------------------------------------------------------------------------------
vg_sales <- vg_sales %>% 
  mutate(publisher_top = ifelse(Publisher %in% publishers_top, TRUE, FALSE),
         developer_top = ifelse(Developer %in% developers_top, TRUE, FALSE))


## --------------------------------------------------------------------------------------------------------
vg_sales <- vg_sales %>% group_by(Name) %>% mutate(num_of_platforms = n()) %>% ungroup(Name)



## --------------------------------------------------------------------------------------------------------
set.seed(2000)


## --------------------------------------------------------------------------------------------------------
test_index <- createDataPartition(vg_sales$Global_Sales, p = 0.8, list = FALSE)
train_set <- vg_sales[-test_index, ]
test_set <- vg_sales[test_index, ]


## --------------------------------------------------------------------------------------------------------
totalData <- rbind(train_set, test_set)
for (f in 1:length(names(totalData))) {
  levels(train_set[, f]) <- levels(totalData[, f])
}



## --------------------------------------------------------------------------------------------------------
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



## --------------------------------------------------------------------------------------------------------
model_lm <- train(log(Global_Sales) ~ Critic_Score +
                     User_Score + Genre +
                     Year_of_Release + Critic_Count +
                     User_Count + Rating +
                     publisher_top + developer_top +
                     num_of_platforms, method = "lm", data = train_set)

# predicted values and RMSE
test_set$predicted_lm <- predict(model_lm, test_set)
rmse_results <- data.frame(Method = "Linear Regression",
                           RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_lm))



## --------------------------------------------------------------------------------------------------------
summary(model_lm)



## --------------------------------------------------------------------------------------------------------
ggplot(test_set) +
  geom_point(aes(log(Global_Sales), predicted_lm)) +
  geom_line(aes(log(Global_Sales), log(Global_Sales))) +
  xlab("Actual values") + ylab("Predicted values") + 
  ggtitle("Actual values vs Predicted values")


## --------------------------------------------------------------------------------------------------------
ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_lm, Global_Sales)) +
  xlab("Error") + ylab("Global sales")


## --------------------------------------------------------------------------------------------------------
model_svm_linear <- train(log(Global_Sales) ~ Critic_Score + 
                     User_Score + Genre + 
                     Year_of_Release +  Critic_Count +
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



## --------------------------------------------------------------------------------------------------------
summary(model_svm_linear)


## --------------------------------------------------------------------------------------------------------
model_svm_poly <- train(log(Global_Sales) ~ Critic_Score + 
                     User_Score + Genre + 
                     Year_of_Release + Critic_Count +
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



## --------------------------------------------------------------------------------------------------------
summary(model_svm_poly)


## --------------------------------------------------------------------------------------------------------
model_svm_rad <- train(log(Global_Sales) ~ Critic_Score + 
                          User_Score + Genre + 
                          Year_of_Release +  Critic_Count +
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


## --------------------------------------------------------------------------------------------------------
summary(model_svm_rad)


## --------------------------------------------------------------------------------------------------------
model_l1 <- train(log(Global_Sales) ~ Critic_Score +
                    User_Score + Genre +
                    Year_of_Release +  Critic_Count +
                    User_Count + Rating +
                    publisher_top + developer_top +
                    num_of_platforms, method = "lasso", data = train_set)

# predicted values and RMSE
test_set$predicted_l1 <- predict(model_l1, test_set)
rmse_results <- rmse_results %>% add_row(Method = "L1 Lasso",
                                         RMSE = RMSE(log(test_set$Global_Sales), 
                                                     test_set$predicted_l1))



## --------------------------------------------------------------------------------------------------------
summary(model_l1)


## --------------------------------------------------------------------------------------------------------
ggplot(test_set) +
  geom_point(aes(log(Global_Sales), predicted_l1)) +
  geom_line(aes(log(Global_Sales), log(Global_Sales))) +
  xlab("Actual values") + ylab("Predicted values")+
  ggtitle("Actual Values vs Predicted Values")


## --------------------------------------------------------------------------------------------------------
ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_l1, Global_Sales)) +
  xlab("Error") + ylab("Global sales")


## --------------------------------------------------------------------------------------------------------
model_l2 <- train(log(Global_Sales) ~ Critic_Score +
                    User_Score + Genre +
                    Year_of_Release +  Critic_Count +
                    User_Count + Rating +
                    publisher_top + developer_top +
                    num_of_platforms, method = "ridge", data = train_set)

# predicted values and RMSE
test_set$predicted_l2 <- predict(model_l2, test_set)
rmse_results <- rmse_results %>% add_row (Method = "L2 Ridge",
                           RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_l2))


## --------------------------------------------------------------------------------------------------------
summary(model_l2)


## --------------------------------------------------------------------------------------------------------
ggplot(test_set) +
  geom_point(aes(log(Global_Sales), predicted_l2)) +
  geom_line(aes(log(Global_Sales), log(Global_Sales))) +
  xlab("Actual values") + ylab("Predicted values")+
  ggtitle("Actual Values vs Predicted Values")


## --------------------------------------------------------------------------------------------------------
ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_l2, Global_Sales)) +
  xlab("Error") + ylab("Global sales")


## --------------------------------------------------------------------------------------------------------
cntrl <- trainControl(method = "repeatedcv", number = 10,
                      repeats = 3)
tunegrid <- expand.grid(.mtry=c(1:5),
                        .min.node.size = seq(1, 5, 1),
                        .splitrule = c("extratrees", "variance"))
model_rf <- train(log(Global_Sales) ~ Critic_Score +
                    User_Score + Genre +
                    Year_of_Release + Critic_Count +
                    User_Count + Rating +
                    publisher_top + developer_top +
                    num_of_platforms, data = train_set,
                  method = "ranger", trControl = cntrl,
                  tuneGrid = tunegrid)

# predicted and RMSE
test_set$predicted_rf <- predict(model_rf, test_set)
rmse_results <- rmse_results %>% add_row(Method = "Random Forest",
                RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_rf))


## --------------------------------------------------------------------------------------------------------
ggplot(test_set) +
  geom_point(aes(log(Global_Sales), predicted_rf)) +
  geom_line(aes(log(Global_Sales), log(Global_Sales))) +
  xlab("Actual values") + ylab("Predicted values") +
  labs(caption =
         paste("R-squared",
               format(model_rf$finalModel$r.squared,
                      digits = 2)))+ 
  ggtitle("Actual Values vs Predicted Values")


## --------------------------------------------------------------------------------------------------------
ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_rf, Global_Sales)) +
  xlab("Error") + ylab("Global Sales")


## --------------------------------------------------------------------------------------------------------
# testing linear regression RMSE 
linear_regression_test <- rmse_results[1,2]

test_that("double",{
  expect_lt(linear_regression_test,2)
})

# testing SVM linear RMSE 
SVM_Linear_test <- rmse_results[2,2]

test_that("double",{
  expect_lt(SVM_Linear_test,2)
})

# testing SVM poly RMSE 
SVM_Polynomial_test <- rmse_results[3,2]
test_that("double",{
  expect_lt(SVM_Polynomial_test,2)
})


# testing SVM radial RMSE 
SVM_Radial_test <- rmse_results[4,2]
test_that("double",{
  expect_lt(SVM_Radial_test,2)
})

# testing L1 RMSE
L1_test <- rmse_results[5,2]
test_that("double",{
  expect_lt(L1_test,2)
})

# testing L2 RMSE
L2_test <- rmse_results[6,2]
test_that("double",{
  expect_lt(L2_test,2)
})

# testing random forest RMSE
random_forest_test <- rmse_results[7,2]
test_that("double",{
  expect_lt(random_forest_test,2)
})



## --------------------------------------------------------------------------------------------------------
print(rmse_results)



## --------------------------------------------------------------------------------------------------------
rmse_plot <- ggplot(rmse_results, aes(x=RMSE,y=Method, fill =Method))+
  geom_bar(stat="identity")+
  xlab("RMSE") + ylab("Model Type")

theme(text = element_text(size=10), 
      legend.position="right",
      axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=8))

rmse_plot

