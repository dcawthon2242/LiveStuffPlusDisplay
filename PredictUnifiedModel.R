# Load required library
library(lightgbm)

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)

release_speed <- as.numeric(args[1])
release_spin_rate <- as.numeric(args[2])
spin_axis <- as.numeric(args[3])
release_pos_x <- as.numeric(args[4])
release_pos_z <- as.numeric(args[5])
pfx_z <- as.numeric(args[6])
pfx_x <- as.numeric(args[7])

# Prepare input data
input_data <- data.frame(
  release_speed = release_speed,
  release_spin_rate = release_spin_rate,
  spin_axis = spin_axis,
  release_pos_x = release_pos_x,
  release_pos_z = release_pos_z,
  pfx_z = pfx_z,
  pfx_x = pfx_x
)

# Load trained LightGBM model
model <- readRDS("models/unified_model.rds")

# Predict xRV
prediction <- predict(model, as.matrix(input_data))

# Hardcoded mean and SD from training data
stuff_mean <- -0.0007056827
stuff_sd <- 0.02020723

# Calculate Stuff+ score
stuff_plus <- 100 + 5 * (prediction - stuff_mean) / stuff_sd

# Output result
cat(stuff_plus, "\n")
