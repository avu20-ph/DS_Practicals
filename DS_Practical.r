#Task List Your written report should include both code, output and written text summaries of the following: 
#Data Validation: Describe validation and cleaning steps for every column in the data 
#Exploratory Analysis: Include two different graphics showing single variables only to demonstrate the characteristics of data 
#Include at least one graphic showing two or more variables to represent the relationship between features 
#Describe your findings Model Development Include your reasons for selecting the models you use as well as a statement of the problem type 
#Code to fit the baseline and comparison models Model Evaluation Describe the performance of the two models based on an appropriate metric 
#Business Metrics Define a way to compare your model performance to the business Describe how your models perform using this approach 
#Final summary including recommendations that the business should undertake

#KPI - achieve at least 80% prediction accuracy
#This is a binary classification problem since the outcome is categorical and only has two possible classes: high traffic and not high traffic.
#Importing packages
library(tidymodels)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(xgboost)
library(ranger)
#Reading csv file and converting it into a dataframe, checking first 6 rows, and checking # rows in dataset
recipes <- read.csv("recipe_site_traffic_2212.csv")
head(recipes)
nrow(recipes)

#Begin data validation and cleaning
#Checking if there are duplicate rows; there are no duplicate rows
sum(duplicated(recipes))
#Ensuring datatypes in the dataset matches the datatypes in the document; servings needs to be converted to a numeric variable
str(recipes)
#Checking if servings has any non-numeric characters; there are some that need to be removed
table(recipes$servings)
#Removing non-numeric characters from servings
recipes$servings <- gsub("[^0-9.]", "", recipes$servings)
table(recipes$servings)
#Converting servings to integer
recipes$servings <- as.integer(recipes$servings)
str(recipes)

#Checking values for category; there is an extra category that needs to be removed, "Chicken Breast"
table(recipes$category)
head(recipes)
str(recipes)
#Combining "Chicken" with "Chicken Breast" by removing " Breast" and checking it is removed
recipes$category <- gsub(" Breast", "", recipes$category)
table(recipes$category)

#Checking values for high_traffic
table(recipes$high_traffic)
head(recipes)
str(recipes)
#Replacing null values with "Not high" and checking values again
recipes$high_traffic[is.na(recipes$high_traffic)] <- "Not High"
table(recipes$high_traffic)
str(recipes)

#Checking if there are missing numbers for any of the columns; calories, carbohydryates, sugar, and protein are have missing numbers
colSums(is.na(recipes))
#Finding rows with missing values; there are 52 rows
recipes[!complete.cases(recipes), ]
#Removing 52 rows with missing values; they are being removed since they only make up 5.5% of all recipes and have multiple missing values
recipes <- recipes %>% drop_na()
#Ensuring the 52 columns were removed
colSums(is.na(recipes))
recipes[!complete.cases(recipes), ]
str(recipes)

#Begin exploratory data analysis
#Calculating descriptive statistics; min, max, quartiles
summary(recipes)
#Calculating descriptive statistics; sd
recipes %>% summarize(across(where(is.numeric), sd, na.rm=TRUE))
#Visually checking for outliers using pairplots; there appears to be some outliers
pairs(recipes[, c("calories", "carbohydrate", "sugar", "protein")])
#Creating histograms for calories, carbohydrates, protein, and sugar (ccps); the plots show that these variables have a right-skewed distribution and are unimodal
recipes %>%
select(calories, carbohydrate, sugar, protein) %>%
pivot_longer(cols=everything(), names_to="ccps", values_to="value") %>%
ggplot(aes(value))+geom_histogram(aes(y=after_stat(density)))+geom_density()+facet_wrap(~ccps, scales = "free")+
labs(title = "Distributions of Recipe Nutrients", x = "Nutrient Value", y = "Density") 
#Plotting ccps as boxplots; the boxplots further support there are outliers for each of these variables; may revisit later
recipes %>%
select(calories, carbohydrate, sugar, protein) %>%
pivot_longer(cols=everything(), names_to="ccps", values_to="value") %>%
ggplot(aes(ccps, value, fill=ccps))+geom_boxplot(staplewidth=0.2, show.legend=FALSE) +facet_wrap(~ccps, scales="free")+
labs(title = "Variation and Outliers in Recipe Nutrients", y = "Value per Recipe")+
theme(axis.title.x = element_blank(), axis.text.x  = element_blank(), axis.ticks.x = element_blank())
#The descriptive statistics and plots suggest there may be some outliers and that the distributions for ccps are unimodal and skewed towards smaller values. The data contains outliers, so median should be used over mean since it is better at representing the typical value in a skewed distribution. 

#Finding median ccps values for each category; as expected, ccps values vary by category
recipes %>% group_by(category) %>% 
summarize(median(calories), median(carbohydrate), median(protein), median(sugar))
#Plotting median ccps values for each category; the scales are too different so they should be normalized
medians <- recipes %>% group_by(category) %>% 
summarize(calories=median(calories), carbohydrate=median(carbohydrate), protein=median(protein), sugar=median(sugar)) %>%
pivot_longer(cols=calories:sugar, names_to="variable", values_to="median_value")
ggplot(medians, aes(category, median_value, fill=variable))+geom_col(show.legend = FALSE)+coord_flip()+facet_wrap(~variable)
#Plotting normalized median ccps values for each category; after normalization, we find that recipe categories have distinct nutritional profiles. For examples desserts are driven by sugar, meat-based dishes by protein, and vegetables and beverages having consistently low ccps values.
	#Normalizing values
medians_scaled <- medians %>% group_by(variable) %>% mutate(scaled_value=median_value/max(median_value))
	#Plotting 
ggplot(medians_scaled, aes(category, scaled_value, fill=variable))+geom_col(show.legend = FALSE)+coord_flip()+facet_wrap(~variable)+labs(title="Normalized Median Nutrient Values by Recipe Category",x="Category", y="Scale Value") 

#Plotting relationship between serving size and high-traffic, ordered by high-traffic=High; The top five categories in high-traffic count were potato, vegetable, chicken, pork, then meat. Beverages had the lowest high-traffic count. 
recipes %>% mutate(category=fct_reorder(category, high_traffic=="High", .fun=sum)) %>%
ggplot(aes(x = category, fill=high_traffic))+
geom_bar(position = "dodge")+labs(title="Categories Ranked by High Traffic Count", x="Category", y="Number of Recipes")+coord_flip()
#Investigating proportion of high-traffic recipes for each category; The top five categories in high-traffic rate were vegetable, potato, pork, one dish meal, and meat. Beverages had the lowest high-traffic count. 
recipes %>% group_by(category) %>% summarize(total=n(), high_traffic_count = sum(high_traffic=="High"), proportion_high_traffic=high_traffic_count/total)

#Investigating relationship between serving size and high-traffic; Recipes with four servings had the highest number of high-traffic recipes, followed by six-serving, one-serving, and two-serving recipes.
ggplot(recipes, aes(factor(servings), fill = high_traffic))+geom_bar(position = "dodge")+labs(title="High Traffic Recipes by Servings", x="Servings", y="Number of Recipes") 
#Investigating proportion of high-traffic recipes for each serving size; Recipes with six servings had the highest proportion of high-traffic recipes, followed by four-serving, one-serving, and two-serving recipes.
recipes %>% group_by(servings) %>% summarize(total=n(), high_traffic_count = sum(high_traffic=="High"), proportion_high_traffic=high_traffic_count/total)

#Creating a tentative/baseline model using logistic regression and assessing performance; the model has a roc_auc of 0.8366 and an accuracy of 0.7991 This means 84% of the time, a randomly chosen “High traffic” recipe will have a higher predicted probability than a randomly chosen “Low traffic” recipe and 80% of recipes were correctly classified as High or Low traffic. These metrics suggest this model performs well and is reliable for selecting content that drives high traffic. 
	#Log-transforming ccps and converting category, serving, and high-traffic into categorical variables to predict high_traffic; ccps is log-transformed since they are right-skewed
ccps_transformed <- recipes %>%
mutate(calories_log=log(calories+1), carbohydrate_log=log(carbohydrate+1), protein_log=log(protein+1), sugar_log=log(sugar+1),
category=as.factor(category), servings=as.factor(servings), high_traffic=as.factor(high_traffic))
	#Plotting log ccps
ccps_transformed %>%
select(calories_log, carbohydrate_log, sugar_log, protein_log) %>%
pivot_longer(cols=everything(), names_to="ccps", values_to="value") %>%
ggplot(aes(value))+geom_histogram(aes(y=after_stat(density)))+geom_density()+facet_wrap(~ccps, scales="free")+
labs(title="Distributions of Log-Transformed Recipe Nutrients", x="Nutrient Value", y="Density") 
	#Splitting data into train and test set
set.seed(1)
split <- ccps_transformed %>% initial_split(prop=3/4, strata=high_traffic)
train <- training(split) 
test <- testing(split)
train %>% select(high_traffic) %>% table() %>% prop.table()
test %>% select(high_traffic) %>% table() %>% prop.table()
	#Declaring model
lr_model <- logistic_reg() %>% set_engine("glm")
	#Building a recipe
lr_recipe <- recipe(high_traffic ~ calories_log+carbohydrate_log+protein_log+sugar_log+category+servings, data=train)
	#Bundling into a workflow object
lr_workflow <- workflow() %>% add_model(lr_model) %>% add_recipe (lr_recipe)
	#Fitting model to the training data
lr_fit <- lr_workflow %>% fit(data=train)
	#Summarize model
lr_fit %>% extract_fit_engine() %>% summary() 
	#Assess model performance
lr_aug <- lr_fit %>% augment(test)
bind_rows(lr_aug %>% roc_auc(truth=high_traffic, .pred_High), lr_aug %>% accuracy(truth=high_traffic, .pred_class))
lr_aug %>% roc_curve(truth=high_traffic, .pred_High) %>% autoplot()

#Creating a comparison model using random forest and assessing performance; The model has a roc_auc of 0.8071 and an accuracy of 0.7366 This means 81% of the time, a randomly chosen “High traffic” recipe will have a higher predicted probability than a randomly chosen “Low traffic” recipe and 74% of recipes were correctly classified as High or Low traffic. These metrics suggest this model performs slightly worse than the logistic regression model but is still decent for selecting content that drives high traffic. 
	#Declaring model
rf_model <- rand_forest(mtry = tune(), trees=500, min_n=tune()) %>%
set_engine("ranger") %>% set_mode("classification")
	#Building a recipe
rf_recipe <- lr_recipe
	#Bundling into a workflow object
rf_workflow <- workflow() %>% add_model(rf_model) %>% add_recipe(rf_recipe)
	#Cross-validating 
set.seed(1)
folds <- vfold_cv(train, v=5, strata=high_traffic)
	#Tuning the random forest
rf_tuned <- rf_workflow %>% tune_grid(resamples=folds, grid=20, metrics=metric_set(roc_auc, accuracy))
	#Selecting the best model 
rf_best <- rf_tuned %>% select_best(metric="roc_auc")
	#Finalize workflow
rf_final_wf <- rf_workflow %>% finalize_workflow(rf_best)
	#Fitting model to the training data
rf_fit <- rf_final_wf %>% fit(data=train) 
	#Assess model performance
rf_aug <- rf_fit %>% augment(test)
bind_rows(rf_aug %>% roc_auc(truth=high_traffic, .pred_High), rf_aug %>% accuracy(truth=high_traffic, .pred_class))
rf_aug %>% roc_curve(truth=high_traffic, .pred_High) %>% autoplot()
