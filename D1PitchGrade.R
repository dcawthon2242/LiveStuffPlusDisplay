# ==============================================================================
# FINDING MEAN DELTA_RUN_EXPECTANCY BY COUNT AND EVENT
# ============================================================================================================================================================


D1TM24$decision <- with(D1TM24, 
                        ifelse(PitchCall %in% c("BallCalled", "BallInDirt", "BallIntentional"), "ball",
                               ifelse(PitchCall == "InPlay", "hit_into_play",
                                      ifelse(PitchCall %in% c("StrikeSwinging", "StrkeSwinging"), "swinging_strike",
                                             ifelse(PitchCall == c("StrikeCalled", "StreikC"), "called_strike",
                                                    ifelse(PitchCall == "HitByPitch", "hit_by_pitch",
                                                           ifelse(PitchCall %in% c("FoulBallNotFieldable ", "FouldBallNotFieldable"), "foul", NA)))))))


D1TM24$event <- with(D1TM24,
                     ifelse(KorBB %in% c("Strikeout", "StrikeOut"), "strikeout",
                            ifelse(PlayResult %in% c("Out", "FieldersChoice", "Sacrifice"), "field_out",
                                   ifelse(PlayResult == "Double", "double",
                                          ifelse(PlayResult == c("Single", "SIngle"), "single",
                                                 ifelse(PlayResult == "Triple", "triple",
                                                        ifelse(PlayResult == c("HomeRun", "Homerun"), "home_run",
                                                               ifelse(KorBB == c("walk", "Walk"), "walk", NA))))))))
D1TM24$swing <- ifelse(D1TM24$decision %in% c("swinging_strike", "hit_into_play", "foul"), 1, 0)

library(dplyr)
# Script to add delta_run_exp column to D1TM24 dataset

# Define the run values for different counts
run_values <- c(
  "0-0" = 0.001695997,
  "1-0" = 0.039248042,
  "0-1" = -0.043581338,
  "2-0" = -0.043581338,
  "1-1" = -0.015277684,
  "0-2" = -0.103242476,
  "3-0" = 0.200960731,
  "2-1" = 0.034545018,
  "1-2" = -0.080485991,
  "3-1" = 0.138254876,
  "2-2" = -0.039716495,
  "3-2" = 0.048505049,
  "Walk" = 0.325,
  "Strikeout" = -0.284
)

# Define run values for different outcomes
outcome_values <- c(
  "walk" = 0.31,
  "hit_by_pitch" = 0.33,
  "single" = 0.46,
  "double" = 0.79,
  "triple" = 1.07,
  "home_run" = 1.41,
  "field_out" = -0.33,
  "strikeout" = -0.284
)

# Function to calculate delta run expectancy
calculate_delta_run_exp <- function(row) {
  # Get the count before the event
  balls <- row$balls
  strikes <- row$strikes
  
  # Form the count string
  count <- paste(balls, strikes, sep = "-")
  
  # Get the run expectancy for the current count
  current_re <- run_values[count]
  
  # Get the run expectancy after the event
  event <- row$event
  
  # Determine final run expectancy based on the event
  if (is.na(event)) {
    # If event is NA, assume no change
    final_re <- current_re
  } else if (event %in% names(outcome_values)) {
    # If it's a terminal event (hit, out, walk), use the outcome value
    final_re <- outcome_values[event]
  } else {
    # For other events, calculate the new count
    new_balls <- balls
    new_strikes <- strikes
    
    # Logic for new count based on event type would go here
    # For now, assuming no change
    
    new_count <- paste(new_balls, new_strikes, sep = "-")
    final_re <- run_values[new_count]
  }
  
  # Calculate delta run expectancy
  delta_re <- final_re - current_re
  
  return(delta_re)
}

# Apply the function to each row in the dataset
D1TM24 <- D1TM24 %>%
  mutate(delta_run_exp = sapply(1:nrow(D1TM24), function(i) {
    calculate_delta_run_exp(D1TM24[i, ])
  }))

# Reset PA value at the beginning of each inning
D1TM24 <- D1TM24 %>%
  group_by(PAofInning) %>%
  mutate(
    PA_reset = PAofInning == 1,  # TRUE for first PA of inning
    # Additional logic for resetting PA-related values could go here
  ) %>%
  ungroup()

# Print the first few rows to verify the new column
head(D1TM24)
#===============================================================================
# DATA PREP
#===============================================================================
# Creating vertical acceleration adjusted for arm angle
model <- lm(az ~ arm_angle, data = D1TM24)

D1TM24 <- D1TM24 %>%
  mutate(az_AA = az - predict(model, newdata = D1TM24) + mean(az))

# Creating horizontal acceleration adjusted for arm angle
model <- lm(ax ~ arm_angle, data = D1TM24)

D1TM24 <- D1TM24 %>%
  mutate(ax_AA = ax - predict(model, newdata = D1TM24) + mean(ax))

#===============================================================================
#Pitch Grade Model v2 04/26/2025
#===============================================================================

# Load necessary libraries
library(dplyr)
library(lightgbm)
library(rBayesianOptimization)

# Combine specified pitch types
D1TM24 <- D1TM24 %>%
  mutate(pitch_type = case_when(
    pitch_type %in% c("CU", "KC") ~ "CU",
    pitch_type %in% c("SL", "SV") ~ "SL",
    pitch_type %in% c("SC", "CH") ~ "CH",
    pitch_type %in% c("FA", "FF") ~ "FF",
    TRUE ~ pitch_type
  ))

# Define pitch types for models
pitch_types <- c("FF", "CU", "CH", "SL", "SI", "FC", "ST", "FS")

# Function to train and predict for each pitch type
train_predict_pitch_type <- function(pitch_type) {
  # Filter data for the current pitch type
  data_pitch <- D1TM24 %>% filter(pitch_type == !!pitch_type)
  
  # Prepare features for training
  one_hot_pbp <- data_pitch %>%
    select(release_speed, release_spin_rate, spin_axis, release_pos_x, release_pos_z, az, ax, avg_delta_run_exp_mapped)
  
  X_train <- one_hot_pbp %>% select(-avg_delta_run_exp_mapped)
  y_train <- one_hot_pbp$avg_delta_run_exp_mapped
  
  # Convert to matrix format
  X_train_matrix <- as.matrix(X_train)
  
  # Create LightGBM dataset for training
  lgb_train <- lgb.Dataset(data = X_train_matrix, label = y_train)
  
  # Perform Bayesian Optimization
  opt_results <- BayesianOptimization(
    FUN = function(num_leaves, max_depth, learning_rate) {
      params <- list(
        objective = "regression",
        metric = "rmse",
        num_leaves = round(num_leaves),
        max_depth = round(max_depth),
        learning_rate = learning_rate,
        min_data_in_leaf = 30,
        feature_pre_filter = FALSE,
        feature_fraction = 0.8,
        bagging_fraction = 0.8,
        bagging_freq = 1
      )
      
      cv_results <- lgb.cv(
        params = params,
        data = lgb_train,
        nrounds = 1000,
        nfold = 5,
        early_stopping_rounds = 50,
        verbose = -1
      )
      
      best_score <- min(cv_results$best_score)
      return(list(Score = -best_score))
    },
    bounds = list(
      num_leaves = c(20L, 100L),
      max_depth = c(3L, 15L),
      learning_rate = c(0.01, 0.3)
    ),
    init_points = 5,
    n_iter = 10,
    acq = "ucb"
  )
  
  # Extract best parameters
  best_params <- list(
    objective = "regression",
    metric = "rmse",
    num_leaves = round(opt_results$Best_Par["num_leaves"]),
    max_depth = round(opt_results$Best_Par["max_depth"]),
    learning_rate = opt_results$Best_Par["learning_rate"],
    min_data_in_leaf = 30,
    feature_pre_filter = FALSE,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 1
  )
  
  # Train final model with optimized parameters
  final_model <- lgb.train(
    params = best_params,
    data = lgb_train,
    nrounds = 1000,
    verbose = 0
  )
  
  # Make predictions
  data_pitch$xRV_3 <- predict(final_model, X_train_matrix)
  
  return(data_pitch)
}

# Apply the function to each pitch type and combine results
results <- lapply(pitch_types, train_predict_pitch_type)
combined_results <- bind_rows(results)

# Add predictions back to the original dataset
D1TM24 <- D1TM24 %>%
  left_join(combined_results %>% select(row_index, xRV_3), by = "row_index")

# Calculate RMSE for the combined model
rmse_v3 <- rmse(D1TM24$xRV_3, D1TM24$avg_delta_run_exp_mapped)

# Load necessary library
library(dplyr)

# Calculate leaderboard with pitch_grade compared to the mean and sd of individual pitch types
leaderboard <- D1TM24 %>%
  group_by(player_name, game_year, pitch_type) %>%
  summarise(
    total_pitches = n(),
    avg_release_speed = mean(release_speed, na.rm = TRUE),
    avg_release_pos_z = mean(release_pos_z, na.rm = TRUE),
    avg_release_pos_x = mean(release_pos_x, na.rm = TRUE),
    avg_pfx_x = mean(pfx_x, na.rm = TRUE),
    avg_pfx_z = mean(pfx_z, na.rm = TRUE),
    xRV_per_100 = sum(xRV_2, na.rm = TRUE) / total_pitches * 100
  ) %>%
  filter(total_pitches >= 50) %>%  # Filter pitchers with at least 50 pitches
  group_by(pitch_type) %>%  # Group by pitch type for mean and sd calculations
  mutate(
    pitch_grade = 100 + 5 * (-xRV_per_100 - mean(-xRV_per_100, na.rm = TRUE)) / sd(-xRV_per_100, na.rm = TRUE)
  ) %>%
  ungroup()


# Print RMSE
print(paste("RMSE for the combined model:", rmse_v3))


