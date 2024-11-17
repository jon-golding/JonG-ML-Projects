library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(timetk)
library(lubridate)

#Loading the dataset as a CSV file and assigning it to a tbl name:
food_data_set_top_20_features.tbl <- read_csv("/Users/jongolding/Desktop/My Glucose Project/top_26_features_9300 rows cleaned May 2024.csv")

# Save the data frame as an RDS file:
# Save the data frame as an RDS file
saveRDS(food_data_set_top_20_features.tbl, "/Users/jongolding/Desktop/My Glucose Project/top_20_features.rds")


# Load the RDS file into R
food_data_set_top_20_features_loaded.tbl <- readRDS("/Users/jongolding/Desktop/My Glucose Project/top_20_features.rds")

# Check the structure of the loaded data to verify everything is correct
glimpse(food_data_set_top_20_features_loaded.tbl)


#Pipe into Glimpse function to see the tbl:
food_data_set_top_20_features.tbl %>% glimpse()



# View column names
food_data_set_top_20_features.tbl %>% 
    colnames() -> col_names
print(col_names)

#create a pipeline that makes a correlation funnel for some initial EDA insights:
library(dplyr)
library(correlationfunnel)
food_data_set_top_20_features.tbl %>% 
    binarize() %>% 
    glimpse() %>% 
    
    #note that binarize function assigns bin ranges to continuous variables, and the resulting 
    #ranges are one-hot-encoded into binary values. In the case of the target 
    #variable, the following 4 bin ranges are created:
    #`Historic_Glucose_mmol_L__-Inf_6.3`
    #Historic_Glucose_mmol_L__6.3_7.4     
    #Historic_Glucose_mmol_L__7.4_9.5     
    #Historic_Glucose_mmol_L__9.5_Inf
    
    correlate(Historic_Glucose_mmol_L__9.5_Inf) %>%
    plot_correlation_funnel()

#The correlation funnel for the high glucose bin of 9.5 to infinity shows little 
#to no correlation for all food features beyond random noise. This in itself is useful 
#information in thinking about static glucose readings at the time of food intake: Glucose response  
#takes time to show an effect post food-intake. The addition of lagged food feature columns could be 
#of benefit when developing a model. 

#In order to prepare the dataset for date-feature extraction and lagged feature extraction, 
#the device time-stamp column will need to be converted to date-time format. Piping the data frame(tbl)into
#the glimpse function shows that it is currently in chr (character) format and will need to be converted. 

food_data_set_top_20_features.tbl %>% glimpse()

#pipeline to convert to datetime format:

food_data_set_top_20_features.tbl <- food_data_set_top_20_features.tbl %>%
    mutate(`Device Timestamp` = dmy_hm(`Device Timestamp`))
      
food_data_set_top_20_features.tbl %>% glimpse()


#Next, plot the timeseries for the full dataset (3+ months of time-stamped 
#rows to get a sense of of the variance over time. 
#***Note that the upper and lower bounds of a normative glucose range are shown 
#as the green lines at the y intercept. 

library(dplyr)

food_data_set_top_20_features.tbl %>%
    plot_time_series(
        .date_var = `Device Timestamp`,
        .value = Historic_Glucose_mmol_L,
        .y_intercept = c(4, 10), # Setting y-intercepts at 4 and 10
        .y_intercept_color = c("green", "green"), # Colors for the intercepts
        .line_color = "#2c3e50",
        .line_size = 0.5,
        .line_type = 1,
        .line_alpha = 1,
        .smooth = FALSE,
        .title = "Glucose Levels Over Time",
        .y_lab = "Historic Glucose (mmol/L)"
    )

#look at extracting lag(future lead) features for independent food variables:

leads_food.tbl <- food_data_set_top_20_features.tbl %>%
    tk_augment_leads(`lemon_cranberry_muffin`,.lags = c(-4,-5,-6,-7,-8,-9,-10,-11,-12)) %>% 
                    drop_na()

#due to its format, the date-stamp column must be dropped for the correlation
#funnel method 

leads_food_tbl_drop_timestamp.tbl <- leads_food.tbl %>% 
    select(-`Device Timestamp`)

    
    #note that binarize function assigns bin ranges to continuous variables, and the resulting 
    #ranges are one-hot-encoded into binary values. In the case of the target 
    #variable, the following 4 bin ranges are created:
    #`Historic_Glucose_mmol_L__-Inf_6.3`
    #Historic_Glucose_mmol_L__6.3_7.4     
    #Historic_Glucose_mmol_L__7.4_9.5     
    #Historic_Glucose_mmol_L__9.5_Inf
leads_food_tbl_drop_timestamp.tbl %>% 
    binarize() %>% 
    glimpse() %>% 
    
            correlate(Historic_Glucose_mmol_L__9.5_Inf) %>%
    plot_correlation_funnel()


#All food categories shown in the resulting correlation funnel showed little
#to no correlation with the target variable which was binned into the high 
#glucose range for this specific correlation funnel plot. Future lead versions 
#of the lemon_cranberry_muffin variable also showed little to no correlation. 
#Given the Author's specific domain knowledge (observations) that muffin consumption 
#seemed to cause a noticeable rise in glucose levels across multiple intake instances, 
#the lack of observed correlation with the lead muffin variables is surprising
#This could likely be caused by the sparseness of the food features in the 
#data set. Taking the lemon cranberry muffins as an example, their intake is not 
#constant! 

#A new strategy below is to take a stratified sample in a four-hour window post 
#lemon cranberry muffin consumption for all instances where the muffins are consumed:

# Identify muffin consumption points
muffin_consumption <- food_data_set_top_20_features.tbl %>%
    filter(lemon_cranberry_muffin == 1) %>%
    select(`Device Timestamp`) %>%
    mutate(end_time = `Device Timestamp` + hours(4))

# Generate a dataframe for each 4-hour window post-muffin consumption
muffin_windows.tbl <- map2_df(muffin_consumption$`Device Timestamp`, muffin_consumption$end_time, 
                          ~{food_data_set_top_20_features.tbl %>%
                            filter(`Device Timestamp` >= .x & `Device Timestamp` <= .y)
                    })
 

#the muffin windows tbl shows the same 20 or so food category features but sampled 
#to a much much smaller data-frame where the muffins were consumed and the subsequent 4-hour
#windows post consumption: 216 rows and 23 columns 

muffin_windows.tbl %>% glimpse() 

#below the stratified muffin sample data-frame is binarized and plotted as a correlation 
#funnel. Weak correlations can now be seen with the target variable (Glucose 9.425 to inf) which
#is the highest level glucose. 

binarized_data <- muffin_windows.tbl %>%
    select(-`Device Timestamp`) 

binarized_data %>%     
    binarize() %>%
    glimpse() %>% 

# Compute and plot the correlations
    correlate(Historic_Glucose_mmol_L__9.425_Inf) %>%
    plot_correlation_funnel()

###_____________________Preparation for data modeling through feature engineering:
###feature engineering: adding change-based features to the main data-frame:


food_data_set_change_features.tbl <- food_data_set_top_20_features.tbl %>%
    arrange(`Device Timestamp`) %>%
    mutate(
        glucose_change_1hr = coalesce(Historic_Glucose_mmol_L - lag(Historic_Glucose_mmol_L, 4), 0),  # 4 steps back for 1-hour change
        glucose_change_2hr = coalesce(Historic_Glucose_mmol_L - lag(Historic_Glucose_mmol_L, 8), 0),  # 8 steps back for 2-hour change
        glucose_change_3hr = coalesce(Historic_Glucose_mmol_L - lag(Historic_Glucose_mmol_L, 12), 0), # 12 steps back for 3-hour change
        glucose_change_4hr = coalesce(Historic_Glucose_mmol_L - lag(Historic_Glucose_mmol_L, 16), 0)  # 16 steps back for 4-hour change
    )

# View the updated dataframe
food_data_set_change_features.tbl %>% glimpse()

#_____________________________________________________________

#Creating a pre-processing Pipeline For Linear Regression Model: While linear regression will likely
#prove ineffective as a modelling strategy due to the non-linear and sparse nature of the 
#data, it is worth exploring to see if feature engineering, and non-linear 
#transformations can model the data in a way that will allow for feature-based insights from the model coefficients 

#apply a log transformation below to the target variable to reduce the effect of 
#outliers and to reduce overall variance. Note: The benefits of applying a log transformation
#are that it can linearize the relationship between variables in a non-linear dataset, making it 
#more appropriate for linear regression. A log transformation will also stabilize the variance, 
#reducing heteroscedasticity across residuals which is a key assumption that needs to be met 
#for linear regression
 
food_cleaned_1.tbl <- food_data_set_change_features.tbl %>% 
    mutate(Glucose_transformed = log1p(Historic_Glucose_mmol_L)) %>% 
    mutate(Glucose_transformed = standardize_vec(Glucose_transformed)) 
                                             
                            
food_cleaned_1.tbl %>% 
    plot_time_series(`Device Timestamp`, Glucose_transformed,
                     .title="log+1 transformed & 1_SD 0_mean standardised Glucose.png")

#Next let's look at a time series overlay of Glucose_transformed
#and one of the glucose change columns. The pivot longer function is used to create 
#a long form table in order to format for the plot_time_series function. This EDA cross-section shows 
#the scale of the glucose change is quite different from the glucose transformed scale:

food_cleaned_1.tbl %>%
    pivot_longer(cols = c(Glucose_transformed, glucose_change_2hr), names_to = "name", values_to = "value") %>%
    plot_time_series(`Device Timestamp`, value, .color_var = name, .smooth=FALSE, 
                     .title= "Cross section overlay of glucose transformed and glucose change")

#Below, similar scaling now applied to the glucose change columns:

food_cleaned_2.tbl <- food_cleaned_1.tbl %>% 
    mutate(glucose_change_1hr_stand = standardize_vec(glucose_change_1hr)) %>%  
    mutate(glucose_change_2hr_stand = standardize_vec(glucose_change_2hr)) %>% 
    mutate(glucose_change_3hr_stand = standardize_vec(glucose_change_3hr)) %>% 
    mutate(glucose_change_4hr_stand = standardize_vec(glucose_change_4hr)) 


food_cleaned_2.tbl %>% glimpse()
    
#Plotting the overlay with glucose change standardized to 0 mean 1 SD:

food_cleaned_2.tbl %>%
    pivot_longer(cols = c(Glucose_transformed, glucose_change_2hr_stand), names_to = "name", values_to = "value") %>%
    plot_time_series(`Device Timestamp`, value, .color_var = name, .smooth=FALSE, 
                     .title= "Cross section overlay of glucose transformed and 2_hr_glucose_change_standardised")

#with transformations/standardization complete, drop the original columns:

food_cleaned_3.tbl <- food_cleaned_2.tbl %>% 
    select(-Historic_Glucose_mmol_L, -glucose_change_1hr, -glucose_change_2hr, -glucose_change_3hr, -glucose_change_4hr) %>% 
    glimpse()

#Next create time-based features extracted from the Device Timestamp using the 
#TK_augment_timeseries_signature function: 


data_tk_signature.tbl <- food_cleaned_3.tbl %>% 
    tk_augment_timeseries_signature(`Device Timestamp`)

data_tk_signature.tbl %>% glimpse()

# 28 time-based features were added through the time series function. Not all are uselful
#for modeling time-based trends and patterns for this use-case, therefore, let's drop several 
#of the features below:

data_tk_sig_cleaned.tbl <- data_tk_signature.tbl %>% 
    select(-diff, -ends_with("iso"), -ends_with(".xts"), -contains("minute"), 
           -contains("second"), -contains("am.pm"), -contains("month"), -contains("quarter"),
           -contains("year"), -contains("half"), -contains(".lbl"),-contains("yday"))

data_tk_sig_cleaned.tbl %>% glimpse()  

#next, look at the linear trend using plot time series regression and the new
#index feature where Glucose_transformed is plotted as a function of index.num:

data_tk_sig_cleaned.tbl %>%
    plot_time_series_regression(
        `Device Timestamp`,
        .formula= Glucose_transformed ~ index.num,
        .title= "Linear Trend for Glucose Transformed as a function of Index.num"
        )
    
data_tk_sig_cleaned.tbl %>%
    plot_time_series_regression(
        `Device Timestamp`,
        .formula= Glucose_transformed ~ splines::bs(index.num, degree=6),
        .title= "Non-Linear Trend using basis-splines(6 degree Polynomial function) for Glucose Transformed",
        .show_summary=TRUE)

#Create a formula below that uses a couple seasonality features with polynomial basis splines:
model_formula_seasonality <- as.formula(
    Glucose_transformed ~splines::bs(index.num, degree=6)
    +wday + hour +.
    )  

        
data_tk_sig_cleaned.tbl %>% 
    plot_time_series_regression(
        `Device Timestamp`,
        .formula= model_formula_seasonality,
        .show_summary = TRUE,
        .title= "Polynomial reg model w/ basis spline, food, & seasonality features, .70 Adj R squared"
    )

####break-----next create non-recursive lag(lead) feature variables to see if the accuracy of the spline model
###can be even further improved:

data_lags_sig_food.tbl <- data_tk_sig_cleaned.tbl %>% 
    tk_augment_lags(Glucose_transformed, .lags=c(4,5,6,7,8,9,10,11,12,13,14,15,16)) %>% 
    drop_na() %>% 
    glimpse()


data_lags_sig_food.tbl %>% 
    plot_time_series_regression(
        `Device Timestamp`,
        .formula= model_formula_seasonality,
        .show_summary = TRUE,
    
    )

data_lags_sig_food.tbl %>% glimpse()


###Next proceed with train/test split:
splits <- time_series_split(data_lags_sig_food.tbl, assess=1870, cumulative = TRUE )

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(`Device Timestamp`, Glucose_transformed) 

##Create a modeltime recipe pipeline for different modeling iterations:

base_recipe_spec <- recipe(Glucose_transformed~ ., data=training(splits)) %>% 
    
    
    #apply step normalize which is actually standardization with mean 0 and 
    #SD1. Apply this function to the remaining features that could benefit from 
    #standardization:
    
    step_normalize("index.num", "day", "hour", "hour12", "wday", "mday", "qday","mweek","week",
                   "week2", "week3", "week4", "mday7")

#note that applying the recipe spec to the prep function trains the data;
#the juice function returns the training data with the recipe applied:

base_recipe_spec %>% prep() %>% juice() %>% glimpse()


#LM MODEL Spec:

model_spec_lm <- linear_reg() %>% 
    set_engine("lm")

#Spline RECIPE Spec:

base_recipe_spec %>% prep() %>% juice() %>% glimpse()

recipe_spec_1 <- base_recipe_spec %>% 
    #drop device time-stamp for regression modeling. We now have the index column
    #and datetime format will not work for the modeling 
    step_rm(`Device Timestamp`,index.num, mday) 
    #step_ns(ends_with ("index.num"), deg_free = 4) %>% 
    #standardize the added spline features:
    #step_normalize("index.num_ns_1","index.num_ns_2","index.num_ns_3", "index.num_ns_4")
    
recipe_spec_1 %>%  prep() %>% juice() %>% glimpse()

#create a model workflow:

workflow_fit_lm_1_spline <- workflow() %>%
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spec_1) %>% 
    fit(training(splits))

#Modeltime workflow for spline model:

calibration_tbl <- modeltime_table(
    workflow_fit_lm_1_spline) %>% 
    modeltime_calibrate(new_data=testing(splits))
        

#forecast using the fitted model:

calibration_tbl %>% 
    modeltime_forecast(new_data= testing(splits),
                       actual_data= data_lags_sig_food.tbl) %>% 
    plot_modeltime_forecast(.title="Linear Model Forecast Plot")
    
calibration_tbl %>% modeltime_accuracy()

###Comparing the calibration test set accuracy vs training set accuracy to assess
#for potential over-fitting: The modeltime forecast for the calibration tbl below
#shows an R squared of.683. The code below will extract the training set test accuracy 
#measures so they can be compared. Training data test accuracy showed an R squared of .7482
#showing that a relatively small amount of over-fitting occurred when training the model. 

workflow_fit_lm_1_spline %>% 
    pull_workflow_fit() %>% 
    pluck("fit") %>% 
    summary()

#______________ plotting the LM coefficients for feature importance below:   
library(ggplot2)
library(tidymodels)

# Extract the coefficients from the fitted model
coefficients <- workflow_fit_lm_1_spline %>% 
    pull_workflow_fit() %>% 
    tidy()  # This returns a tibble with the coefficients

# Plotting the coefficients
coefficients %>%
    ggplot(aes(x = reorder(term, estimate), y = estimate)) +
    geom_bar(stat = "identity", fill = "#2c3e50") +
    coord_flip() +  # Flip coordinates for better readability
    labs(title = "Feature Importance based on Linear Model Coefficients",
         x = "Features",
         y = "Coefficient (Weight)") +
    theme_minimal()

####__________________The resulting plot and food feature significance levels yielded 
###some interesting results for the linear model.Results were mostly congruent with the author's
###domain knowledge and other results were not consistent with domain knowledge of how food features 
###were expected to affect glucose response:

#1Fries was expected to have a upward contribution to glucose response. As shown in the feature importance plot,
#1Fries was considered the most important food feature category in this direction. 

#It was very surprising that kit_kit_bar demonstrated feature importance model contribution in a slightly negative direction of glucose 
#response. Putting this into context with the author's domain knowledge, kit_kat_bar was normally 
#conssumed as few as 8 times over the course of the 3 month time series data set and was usually consumed during instances of low 
#glucose levels at the time of consumption. This could be telling a telling aspect of the linear model's possible limitations in separating 
#prolonged extremes in glucose levels from food-based feature impacts. 

#Given previously mentioned domain knowledge and the author's personal observations over time: 
#coffee, almond butter, olive oil, cod and cinnamon, were all expected to have a downward feature importance 
#contributions on glucose response. This was shown to be the case the the feature importance plot.  


#__________________break_____________________________________________________
###Calculate the frequency of each food item consumed:


food_frequencies <- data_lags_sig_food.tbl %>%
    select(-`Device Timestamp`, -Glucose_transformed, -contains('lag'),-contains('day'), 
           -contains('change'),-contains('hour'),-contains('week'),-contains('index')) %>%  # Exclude non-food columns
    summarise(across(everything(), sum)) %>%  # Sum up occurrences of each food item
    pivot_longer(cols = everything(), names_to = "food_item", values_to = "frequency")  # Convert to long format


###Given the frequency of food features consumed tbl that was produced: below a strategy to re-distribute 
##model weights to address the imbalance of food features consumed:

# Calculate weights based on inverse frequency
food_frequencies <- food_frequencies %>%
    mutate(weight = 1 / frequency)

# Normalize weights to ensure they sum to 1
food_frequencies <- food_frequencies %>%
    mutate(weight = weight / sum(weight))

# View the calculated weights
print(food_frequencies)

    
#The process of assigning higher weights to less frequent features means the contribution of each food item is 
#re-balanced, ensuring that less frequent but potentially important features are not diluted.

#Next: Assign the re-weighted food categories to the previously modelled dataset:

library(tidymodels)
library(timetk)
library(dplyr)
library(ggplot2)
library(hardhat)

# Add scaled weights directly into the dataset using case_when
data_lags_sig_food_weighted <- data_lags_sig_food.tbl %>%
    mutate(
        total_weight = case_when(
            lemon_cranberry_muffin == 1 ~ 0.0546,  # Weight for 'lemon_cranberry_muffin'
            `1fries` == 1 ~ 0.0637,                # Weight for '1fries'
            `1cinnamon` == 1 ~ 0.00921,            # Weight for '1cinnamon'
            `Metformin Pill` == 1 ~ 0.00475,       # Weight for 'Metformin Pill'
            `1rosehip_tea` == 1 ~ 0.0449,          # Weight for '1rosehip_tea'
            `1almond_butter` == 1 ~ 0.0156,        # Weight for '1almond_butter'
            `1good_granola` == 1 ~ 0.0955,         # Weight for '1good_granola'
            `1peanuts` == 1 ~ 0.0332,              # Weight for '1peanuts'
            `1squash` == 1 ~ 0.0637,               # Weight for '1squash'
            `1pickles` == 1 ~ 0.0402,              # Weight for '1pickles'
            kit_kat_bar == 1 ~ 0.0955,             # Weight for 'kit_kat_bar'
            `1coffee` == 1 ~ 0.0382,               # Weight for '1coffee'
            `1olive_oil` == 1 ~ 0.0105,            # Weight for '1olive_oil'
            `1chicken_tikka` == 1 ~ 0.127,         # Weight for '1chicken_tikka'
            `1cashews` == 1 ~ 0.0764,              # Weight for '1cashews'
            `1brown_rice` == 1 ~ 0.0186,           # Weight for '1brown_rice'
            cod == 1 ~ 0.153,                      # Weight for 'cod'
            rye_sourdough_bread == 1 ~ 0.0546,     # Weight for 'rye_sourdough_bread'
            Walk == 1 ~ 0.000528,                  # Weight for 'Walk'
            None == 1 ~ 0.000101,                  # Weight for 'None'
            TRUE ~ 1                               # Default weight for other cases
        )
    ) %>%
    mutate(total_weight = round(total_weight * 1000))  # Scale weights

# View the modified dataset with scaled weights
data_lags_sig_food_weighted %>% glimpse()

# Proceed with the train/test split
splits <- time_series_split(data_lags_sig_food_weighted, assess = 1870, cumulative = TRUE)

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(`Device Timestamp`, Glucose_transformed)

# Define the recipe without update_role(), weights will be handled separately
base_recipe_spec <- recipe(Glucose_transformed ~ ., data = training(splits)) %>%
    step_rm(`Device Timestamp`, index.num, mday) %>%
    step_normalize("day", "hour", "hour12", "wday", "qday","mweek","week",
                   "week2", "week3", "week4", "mday7")

# Apply frequency weights using hardhat with scaled integer weights
training_data <- training(splits) %>%
    mutate(total_weight = hardhat::frequency_weights(as.integer(total_weight)))

# Prep and juice the recipe using the weighted data
prepped_data <- base_recipe_spec %>% prep(training_data) %>% juice()
prepped_data %>% glimpse()

# Define the linear regression model specification
model_spec_lm <- linear_reg() %>% 
    set_engine("lm")

# Fit the model with the updated recipe and weights
workflow_fit_lm_1_spline <- workflow() %>%
    add_model(model_spec_lm) %>%
    add_recipe(base_recipe_spec) %>%
    fit(training_data)

# Create a calibration table for the model
calibration_tbl <- modeltime_table(workflow_fit_lm_1_spline) %>% 
    modeltime_calibrate(new_data = testing(splits))

# Forecast using the fitted model and plot the results
calibration_tbl %>% 
    modeltime_forecast(new_data = testing(splits),
                       actual_data = data_lags_sig_food_weighted) %>% 
    plot_modeltime_forecast(.title="Linear Model Forecast Food Coefficients Re-weighted")

# Check the accuracy of the model
calibration_tbl %>% modeltime_accuracy()

# Summary and plot of coefficients for feature importance
workflow_fit_lm_1_spline %>% 
    pull_workflow_fit() %>% 
    pluck("fit") %>% 
    summary()

# Plot the linear model coefficients
coefficients <- workflow_fit_lm_1_spline %>% 
    pull_workflow_fit() %>% 
    tidy()

# Plotting the coefficients
coefficients %>%
    ggplot(aes(x = reorder(term, estimate), y = estimate)) +
    geom_bar(stat = "identity", fill = "#2c3e50") +
    coord_flip() +
    labs(title = "Feature Importance w/ Linear Model Food Coefficients Re-weighted",
         x = "Features",
         y = "Coefficient (Weight)") +
    theme_minimal()

##_________________________________________Break
####As observed in the latest feature importance plot, The Glucose_transformed_lead_4 and 
####Glucose_tansformed_lead_5 features show the highest importance for the existing model, and seem 
###to overshadow some of the importances for the food-based features. Below, these two lead based features 
###are dropped to see if the food feature importance is affected without reducing the adjusted r squared:


splits <- time_series_split(data_lags_sig_food_weighted, assess = 1870, cumulative = TRUE)

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(`Device Timestamp`, Glucose_transformed)

# Define the recipe without update_role(), weights will be handled separately
base_recipe_spec <- recipe(Glucose_transformed ~ ., data = training(splits)) %>%
    step_rm(`Device Timestamp`, index.num, mday,glucose_transformed_lag4) %>%
    step_normalize("day", "hour", "hour12", "wday", "qday","mweek","week",
                   "week2", "week3", "week4", "mday7")

# Apply frequency weights using hardhat with scaled integer weights
training_data <- training(splits) %>%
    mutate(total_weight = hardhat::frequency_weights(as.integer(total_weight)))

# Prep and juice the recipe using the weighted data
prepped_data <- base_recipe_spec %>% prep(training_data) %>% juice()
prepped_data %>% glimpse()

# Define the linear regression model specification
model_spec_lm <- linear_reg() %>% 
    set_engine("lm")

# Fit the model with the updated recipe and weights
workflow_fit_lm_1_spline <- workflow() %>%
    add_model(model_spec_lm) %>%
    add_recipe(base_recipe_spec) %>%
    fit(training_data)

# Create a calibration table for the model
calibration_tbl <- modeltime_table(workflow_fit_lm_1_spline) %>% 
    modeltime_calibrate(new_data = testing(splits))

# Forecast using the fitted model and plot the results
calibration_tbl %>% 
    modeltime_forecast(new_data = testing(splits),
                       actual_data = data_leads_sig_food_weighted) %>% 
    plot_modeltime_forecast(.title="Linear Model Forecast Food Coefficients Re-weighted, Glucose leads 4 & 5 removed")

# Check the accuracy of the model
calibration_tbl %>% modeltime_accuracy()

# Summary and plot of coefficients for feature importance
workflow_fit_lm_1_spline %>% 
    pull_workflow_fit() %>% 
    pluck("fit") %>% 
    summary()

# Plot the linear model coefficients
coefficients <- workflow_fit_lm_1_spline %>% 
    pull_workflow_fit() %>% 
    tidy()

# Plotting the coefficients
coefficients %>%
    ggplot(aes(x = reorder(term, estimate), y = estimate)) +
    geom_bar(stat = "identity", fill = "#2c3e50") +
    coord_flip() +
    labs(title = "Feature Importance w/ Linear Model Food Coefficients Re-weighted, Glucose leads 4 & 5 removed",
         x = "Features",
         y = "Coefficient (Weight)") +
    theme_minimal()

###________________Break____________________________________________________



###Break next iteration with food lags added_____________________________________________________

# Augment lagged features for Glucose_transformed with lags of 4 and 8
data_with_food_lags <- data_lags_sig_food.tbl %>%
    tk_augment_lags(`1fries`, .lags = c(4,8)) %>%
    tk_augment_lags(kit_kat_bar, .lags = c(4,8)) %>%
    tk_augment_lags(lemon_cranberry_muffin, .lags = c(4,8)) %>%
    tk_augment_lags(`1peanuts`, .lags = c(4,8)) %>%
    tk_augment_lags(`1cashews`, .lags = c(4,8)) %>%
    tk_augment_lags(`1cinnamon`, .lags = c(4, 8)) %>%
    tk_augment_lags(cod, .lags = c(4, 8)) %>%
    tk_augment_lags(`1good_granola`, .lags = c(4, 8)) %>%
    tk_augment_lags(`1rosehip_tea`, .lags = c(4,8)) %>%
    tk_augment_lags(`Metformin Pill`, .lags = c(4,8)) %>%
    tk_augment_lags(`1almond_butter`, .lags = c(4, 8)) %>%
    tk_augment_lags(`1chicken_tikka`, .lags = c(4,8)) %>%
    tk_augment_lags(`1olive_oil`, .lags = c(4,8)) %>%
    tk_augment_lags(`1pickles`, .lags = c(4, 8)) %>%
    tk_augment_lags(rye_sourdough_bread, .lags = c(4, 8)) %>%
    tk_augment_lags(Walk, .lags = c(4, 8)) %>%
    tk_augment_lags(`1squash`, .lags=c(4,8)) %>% 
    tk_augment_lags(`1brown_rice`, .lags=c(4,8)) %>% 
    tk_augment_lags(`1coffee`, .lags=c(4,8)) %>% 
    drop_na()

# Split the lagged dataset into training and testing sets
splits <- time_series_split(data_with_food_lags, assess = 1870, cumulative = TRUE)

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(`Device Timestamp`, Glucose_transformed)

# Define the new recipe for interaction terms and lag features
final_lm_recipe <- recipe(Glucose_transformed ~ ., data = training(splits)) %>%
    step_rm(`Device Timestamp`, mday, index.num, Glucose_transformed_lag4, 
            Glucose_transformed_lag8,  Glucose_transformed_lag12,  Glucose_transformed_lag16) %>%
    
    # Interaction terms with hour and specific food items
    #step_interact(terms = ~ hour:lemon_cranberry_muffin) %>%
    #step_interact(terms = ~ hour:`1fries`) %>%
    #step_interact(terms = ~ hour:kit_kat_bar) %>%
    #step_interact(terms = ~ hour:`1cashews`) %>%
    #step_interact(terms = ~ hour:cod) %>%
    #step_interact(terms = ~ hour:`1squash`) %>%
    #step_interact(terms = ~ hour:`1cinnamon`) %>%
    #step_interact(terms = ~ hour:`1peanuts`) %>%
    #step_interact(terms = ~ hour:`1pickles`) %>%
    #step_interact(terms = ~ hour:`1brown_rice`) %>%
    #step_interact(terms = ~ hour:`1good_granola`) %>%
    #step_interact(terms = ~ hour:rye_sourdough_bread) %>%
    #step_interact(terms = ~ hour:`1rosehip_tea`) %>%
    #step_interact(terms = ~ hour:`Metformin Pill`) %>%
    #step_interact(terms = ~ hour:`1coffee`) %>%
    
    # Normalization step
    step_normalize("day", "hour", "hour12", "wday", "qday", "mweek", "week", "week2", "week3", "week4", "mday7")

# Apply frequency weights using the same weights as food variables
training_data <- training(splits)
    #mutate(across(starts_with("hour_"), ~ . * total_weight)) %>%
    #mutate(total_weight = hardhat::frequency_weights(as.integer(total_weight)))

# Prep and juice the recipe using the weighted data
prepped_data <- final_lm_recipe %>% prep(training_data) %>% juice()
prepped_data %>% glimpse()

# Define the linear regression model specification
model_spec_lm <- linear_reg() %>% 
    set_engine("lm")

# Fit the model with the updated recipe and weights
workflow_fit_lm_interaction <- workflow() %>%
    add_model(model_spec_lm) %>%
    add_recipe(final_lm_recipe) %>%
    fit(training_data)

# Create a calibration table for the model
calibration_tbl <- modeltime_table(workflow_fit_lm_interaction) %>% 
    modeltime_calibrate(new_data = testing(splits))

# Forecast using the fitted model and plot the results
calibration_tbl %>% 
    modeltime_forecast(new_data = testing(splits),
                       actual_data = data_with_lags) %>% 
    plot_modeltime_forecast(.title="Linear Model Forecast with Lagged Food Features Added")

# Check the accuracy of the model
calibration_tbl %>% modeltime_accuracy()

# Summary and plot of coefficients for feature importance
workflow_fit_lm_interaction %>% 
    pull_workflow_fit() %>% 
    pluck("fit") %>% 
    summary()

# Plot the linear model coefficients
coefficients <- workflow_fit_lm_interaction %>% 
    pull_workflow_fit() %>% 
    tidy()

coefficients %>%
    ggplot(aes(x = reorder(term, estimate), y = estimate)) +
    geom_bar(stat = "identity", fill = "#2c3e50") +
    coord_flip() +
    labs(title = "Feature Importance For Linear Model w/ Lagged Food Features Added",
         x = "Features",
         y = "Coefficient (Weight)") +
    theme_minimal()

# Plotting the coefficients

####_______________________________________________________________

  

######## XG Boost Model
# Set up XGBoost model specification
model_spec_boost <- boost_tree(
    mode = "regression"
) %>% 
    set_engine("xgboost")

# Set seed for reproducibility
set.seed(123)


# Define workflow with the model and recipe
workflow_fit_xgboost <- workflow() %>% 
    add_model(model_spec_boost) %>% 
    add_recipe(final_lm_recipe) %>% 
    fit(training(splits))

# Calibrate the model with testing data
XGboost_calibration_tbl <- modeltime_table(workflow_fit_xgboost) %>% 
    modeltime_calibrate(new_data = testing(splits))

# Forecast using the fitted model and plot the results
XGboost_calibration_tbl %>% 
    modeltime_forecast(new_data = testing(splits),
                       actual_data = data_with_lags) %>% 
    plot_modeltime_forecast(.title = "XGBoost Forecast") 

# Check the accuracy of the model
XGboost_calibration_tbl %>% modeltime_accuracy()


#FEATURE ANALYSIS BELOW USING SHAP:
#___________

# Load necessary libraries

library(farff)
library(OpenML)
library(dplyr)
library(xgboost)
library(ggplot2)
library(SHAPforxgboost)


Glucose_XGboost_data <- read_csv("/Users/jongolding/Desktop/R Glucose Project/R Glucose Project.glucose_data_XGboost.csv")



#Create glucose lag columns for 1, 2, 3, and 4 hours (1 hour = 4 time steps)

Glucose_XGboost_data <- Glucose_XGboost_data %>%
    mutate(
       Glucose_lag_1hr = lag(Glucose_transformed, n = 4),  # 1 hour = 4 time steps
       Glucose_lag_2hr = lag(Glucose_transformed, n = 8),  # 2 hours = 8 time steps
       Glucose_lag_3hr = lag(Glucose_transformed, n = 12), # 3 hours = 12 time steps
       Glucose_lag_4hr = lag(Glucose_transformed, n = 16)  # 4 hours = 16 time steps
    ) %>%
    drop_na()  # Remove rows with NA values caused by lagging



head(Glucose_XGboost_data)

Glucose_XGboost_data %>% glimpse()


# Define response and features
y <- "Glucose_transformed"

X <- Glucose_XGboost_data %>% 
    select(-`Device Timestamp`, -Glucose_transformed)

head(X)
X %>% glimpse()

#Create the train test split:

n <- nrow(Glucose_XGboost_data)

# Index for 80% split (train)
train_ix <- 1:floor(0.8 * n)

# Prepare training and validation sets
dtrain <- xgb.DMatrix(data.matrix(X[train_ix, ]),
                      label = Glucose_XGboost_data$Glucose_transformed[train_ix])

dvalid <- xgb.DMatrix(data.matrix(X[-train_ix, ]),
                      label = Glucose_XGboost_data$Glucose_transformed[-train_ix])

##Train the XGBoost Model:

params <- list(
    objective = "reg:squarederror",
    learning_rate = 0.05,
    subsample = 0.9,
    colsample_bynode = 1,
    reg_lambda = 10,
    max_depth = 5
)
fit_xgb <- xgb.train(
    params,
    data = dtrain,
    watchlist = list(valid = dvalid),
    early_stopping_rounds = 20,
    print_every_n = 100,
    nrounds = 10000 # early stopping
)


#Calulate eval metrics for the test set:
# Load required library for MASE calculation
library(Metrics)

# Step 1: Make predictions on the test set
preds <- predict(fit_xgb, dvalid)

# Step 2: Get the actual values for the test set
actuals <- Glucose_XGboost_data$Glucose_transformed[-train_ix]

# Step 3: Calculate R-squared
r_squared <- 1 - sum((actuals - preds) ^ 2) / sum((actuals - mean(actuals)) ^ 2)

# Step 4: Calculate MAE
mae <- mean(abs(actuals - preds))

# Step 5: Calculate MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((actuals - preds) / actuals)) * 100

# Step 6: Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((actuals - preds) ^ 2))

# Step 7: Calculate MASE (Mean Absolute Scaled Error)
# First, calculate naive forecast error (lag 1)
naive_error <- mean(abs(actuals[-1] - actuals[-length(actuals)]))
mase <- mae / naive_error

# Print all the metrics
metrics <- data.frame(
    R_squared = round(r_squared, 4),
    MAE = round(mae, 4),
    MAPE = round(mape, 4),
    RMSE = round(rmse, 4),
    MASE = round(mase, 4)
)

print(metrics)



####SHAP plots:

# Assume you have the XGBoost model `fit_xgb` already trained

# Step 1: Select some observations from the training data (or validation, as desired)
X_sample <- data.matrix(X[train_ix, ])[sample(train_ix, 1000), ]

# Step 2: Compute SHAP values using the trained XGBoost model
shap_values <- shap.prep(fit_xgb, X_train = X_sample)

# Step 3: Plot SHAP summary for the feature importance
shap.plot.summary(shap_values)

# Step 4: Loop over dependence plots in decreasing importance
for (feature in shap.importance(shap_values, names_only = TRUE)) {
    p <- shap.plot.dependence(shap_values, feature, color_feature = "auto", 
                              alpha = 0.5, jitter_width = 0.1) +
        ggtitle(feature)
    print(p)
}


####____The XGBoost model showed a significant r squared improvement (test set). 
#R squared is now .7973 using XGBoost without any hyperparameter tuning. Food feature 
#explainability as seen on the SHAP plots was not as insightful as with the model coefficients 
#in the linear model. Glucose change features and time-based features dominated the SHAP feature 
#importance plot. 

###Next strategy: remodel using XGBoost with some glucose lead features removed, most time features
#removed, glucose_change columns removed except for 1 & 3 hour glucose change:

library(dplyr)
library(xgboost)
library(ggplot2)
library(SHAPforxgboost)
library(Metrics)

# Step 1: Filter out the specified columns


X_filtered <- X %>%
    select(
        -contains("lag8"),
        -contains("lead"),
        -contains("mday"),  # Ensure the column exists
        -glucose_change_3hr_stand,
        -glucose_change_4hr_stand,
        -wday, -qday, -mweek, -week2, -week3, -week4, -hour12,-week,
        -total_weight, -day 
        
    )

X_filtered %>% glimpse()



# Step 2: Create the train/test split (80/20)
n <- nrow(Glucose_XGboost_data)
train_ix <- 1:floor(0.8 * n)

# Step 3: Prepare training and validation sets with the filtered dataset
dtrain_filtered <- xgb.DMatrix(data.matrix(X_filtered[train_ix, ]), 
                               label = Glucose_XGboost_data$Glucose_transformed[train_ix])
dvalid_filtered <- xgb.DMatrix(data.matrix(X_filtered[-train_ix, ]), 
                               label = Glucose_XGboost_data$Glucose_transformed[-train_ix])

# Step 4: Define XGBoost parameters
params <- list(
    objective = "reg:squarederror",
    learning_rate = 0.05,
    subsample = 1,
    colsample_bynode = 1,
    reg_lambda = 10,
    reg_alpha = 0.5,
    max_depth = 6
)


# Step 5: Train the XGBoost model with tracking of both training and validation RMSE
fit_xgb_filtered <- xgb.train(
    params,
    data = dtrain_filtered,
    watchlist = list(train = dtrain_filtered, valid = dvalid_filtered),  # Track both training and validation sets
    early_stopping_rounds = 20,
    print_every_n = 100,
    nrounds = 10000
)

# Extract evaluation history for learning curves
evaluation_log <- as.data.frame(fit_xgb_filtered$evaluation_log)

# Plot learning curves
ggplot(evaluation_log, aes(x = iter)) +
    geom_line(aes(y = train_rmse, color = "Training RMSE")) +
    geom_line(aes(y = valid_rmse, color = "Validation RMSE")) +
    labs(
        title = "Learning Curves (Training vs. Validation RMSE)",
        x = "Boosting Rounds",
        y = "RMSE"
    ) +
    scale_color_manual(values = c("Training RMSE" = "blue", "Validation RMSE" = "red")) +
    theme_minimal()



# Step 6: Compute SHAP values for the filtered model for all features
shap_values_filtered <- shap.prep(fit_xgb_filtered, X_train = data.matrix(X_filtered))

# Step 7: SHAP summary plot for all features in the filtered model
shap.plot.summary(shap_values_filtered)

# Step 8: Calculate evaluation metrics for the test set
# Make predictions on the test set
preds <- predict(fit_xgb_filtered, dvalid_filtered)

# Get the actual values for the test set
actuals <- Glucose_XGboost_data$Glucose_transformed[-train_ix]

# Calculate R-squared
r_squared <- 1 - sum((actuals - preds) ^ 2) / sum((actuals - mean(actuals)) ^ 2)

# Calculate MAE
mae <- mean(abs(actuals - preds))

# Calculate MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((actuals - preds) / actuals)) * 100

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((actuals - preds) ^ 2))

# Calculate MASE (Mean Absolute Scaled Error)
# First, calculate naive forecast error (lag 1)
naive_error <- mean(abs(actuals[-1] - actuals[-length(actuals)]))
mase <- mae / naive_error

# Print all the metrics
metrics <- data.frame(
    R_squared = round(r_squared, 4),
    MAE = round(mae, 4),
    MAPE = round(mape, 4),
    RMSE = round(rmse, 4),
    MASE = round(mase, 4))

print(metrics)

##________________________

library(dplyr)
library(xgboost)
library(ggplot2)
library(SHAPforxgboost)
library(Metrics)

# Step 1: Filter out the specified columns
X_filtered <- X %>%
    select(
        -contains("lag8"),
        -contains("lead"),
        -contains("mday"),  # Ensure the column exists
        -glucose_change_3hr_stand,
        -glucose_change_4hr_stand,
        -wday, -qday, -mweek, -week2, -week3, -week4, -hour12, -week,
        -total_weight, -day
    )

X_filtered %>% glimpse()

# Step 2: Create the train/test split (80/20)
n <- nrow(Glucose_XGboost_data)
train_ix <- 1:floor(0.8 * n)

# Step 3: Prepare training and validation sets with the filtered dataset
dtrain_filtered <- xgb.DMatrix(data.matrix(X_filtered[train_ix, ]), 
                               label = Glucose_XGboost_data$Glucose_transformed[train_ix])
dvalid_filtered <- xgb.DMatrix(data.matrix(X_filtered[-train_ix, ]), 
                               label = Glucose_XGboost_data$Glucose_transformed[-train_ix])

# Step 4: Define adjusted XGBoost parameters to reduce overfitting
params <- list(
    objective = "reg:squarederror",
    learning_rate = 0.05,
    subsample = 0.7,  # Use 70% of data for each tree
    colsample_bynode = 0.7,  # Use 70% of features for each split
    reg_lambda = 20,  # L2 regularization
    reg_alpha = 1.0,  # L1 regularization
    max_depth = 4  # Reduce tree depth to simplify the model
)

# Step 5: Train the XGBoost model with adjusted parameters and early stopping
fit_xgb_filtered <- xgb.train(
    params,
    data = dtrain_filtered,
    watchlist = list(train = dtrain_filtered, valid = dvalid_filtered),  # Track both training and validation sets
    early_stopping_rounds = 20,  # Stop if validation RMSE doesn’t improve for 20 rounds
    print_every_n = 100,
    nrounds = 500  # Reduce number of rounds to avoid overfitting
)

# Step 6: Extract evaluation history for learning curves
evaluation_log <- as.data.frame(fit_xgb_filtered$evaluation_log)

# Step 7: Plot learning curves
ggplot(evaluation_log, aes(x = iter)) +
    geom_line(aes(y = train_rmse, color = "Training RMSE")) +
    geom_line(aes(y = valid_rmse, color = "Validation RMSE")) +
    labs(
        title = "Learning Curves (Training vs. Validation RMSE)",
        x = "Boosting Rounds",
        y = "RMSE"
    ) +
    scale_color_manual(values = c("Training RMSE" = "blue", "Validation RMSE" = "red")) +
    theme_minimal()

# Step 8: Compute SHAP values for the filtered model for all features
shap_values_filtered <- shap.prep(fit_xgb_filtered, X_train = data.matrix(X_filtered))

# Step 9: SHAP summary plot for all features in the filtered model
shap.plot.summary(shap_values_filtered)

# Step 10: Calculate evaluation metrics for the test set
# Make predictions on the test set
preds <- predict(fit_xgb_filtered, dvalid_filtered)

# Get the actual values for the test set
actuals <- Glucose_XGboost_data$Glucose_transformed[-train_ix]

# Calculate R-squared
r_squared <- 1 - sum((actuals - preds) ^ 2) / sum((actuals - mean(actuals)) ^ 2)

# Calculate MAE
mae <- mean(abs(actuals - preds))

# Calculate MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((actuals - preds) / actuals)) * 100

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((actuals - preds) ^ 2))

# Calculate MASE (Mean Absolute Scaled Error)
# First, calculate naive forecast error (lag 1)
naive_error <- mean(abs(actuals[-1] - actuals[-length(actuals)]))
mase <- mae / naive_error

# Print all the metrics
metrics <- data.frame(
    R_squared = round(r_squared, 4),
    MAE = round(mae, 4),
    MAPE = round(mape, 4),
    RMSE = round(rmse, 4),
    MASE = round(mase, 4)
)

print(metrics)



