# 1. Required Libraries
library(tidyverse)
library(lubridate)
library(scales)

# 2. Read and Clean the Data
file_path <- "F:/Kurtun_heyelan/Makale/ilk_yuklenen/Red_sonrasi/Data/ERA5_Temperature_Air_Skin_2025.csv"

data <- read_delim(file_path, delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(
    # Set date format (GEE typically outputs YYYY-MM-DD)
    date = as.Date(date),
    # Replace comma decimals with dots and convert to numeric
    AirTemp = as.numeric(gsub(",", ".", AirTemp)),
    SkinTemp = as.numeric(gsub(",", ".", SkinTemp))
  ) %>%
  filter(!is.na(date))

# 3. Define Parameters
landslide_date <- as.Date("2025-04-22") # Landslide date
thresholds <- c(-2, 0, 2)              # Thresholds requested by the reviewer
windows <- c(90, 105, 120)             # Time windows requested by the reviewer
variables <- c("AirTemp", "SkinTemp")  # Variables

# 4. Analysis Function (Corrected)
calculate_robustness <- function(var_name, thresh, win_len) {
  # Ensure var_name is a character string
  var_name <- as.character(var_name)
  
  start_date <- landslide_date - days(win_len)
  
  # Filter data within the selected time window
  sub_data <- data %>%
    filter(date >= start_date & date <= landslide_date) %>%
    # Using !!sym() safely because var_name is explicitly character
    rename(temp_val = !!sym(var_name))
  
  # Find the first day when the threshold is exceeded
  trigger_day <- sub_data %>%
    filter(temp_val > thresh) %>%
    arrange(date) %>%
    slice(1)
  
  if(nrow(trigger_day) > 0) {
    lag_days <- as.numeric(landslide_date - trigger_day$date)
    return(data.frame(Variable=var_name, Threshold=thresh, Window=win_len, 
                      TriggerDate=trigger_day$date, Lag=lag_days))
  } else {
    return(data.frame(Variable=var_name, Threshold=thresh, Window=win_len, 
                      TriggerDate=as.Date(NA), Lag=NA))
  }
}

# 5. Run All Scenarios (expand.grid corrected)
results <- expand.grid(v = variables, t = thresholds, w = windows, stringsAsFactors = FALSE) %>%
  pmap_df(~calculate_robustness(..1, ..2, ..3))

# Inspect results
print(results)

# Use this code to visualize the results
library(ggplot2)

ggplot(results, aes(x = as.factor(Threshold), y = Lag, color = Variable)) +
  geom_point(aes(shape = as.factor(Window)), size = 4, position = position_jitter(width = 0.2)) +
  geom_hline(yintercept = c(35, 50), linetype = "dashed", color = "red", alpha = 0.3) +
  annotate("text", x = 2, y = 55, label = "Main Spring Thaw Zone", color = "red") +
  labs(
    title = "Robustness of Thaw-to-Landslide Lag Time",
    x = "Temperature Threshold (??C)",
    y = "Lag Time (Days)",
    shape = "Time Window (Days)",
    color = "Data Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("AirTemp" = "steelblue", "SkinTemp" = "darkorange"))




#=====Sliding-window temperature analysis========
library(ggplot2)
library(tidyverse)
library(lubridate)

# 1. Set locale to English (for month labels such as Nov, Dec, Jan)
Sys.setlocale("LC_TIME", "English")

# 2. Fit regression models (for equation annotations)
model_air <- lm(AirTemp ~ as.numeric(date), data = data)
model_skin <- lm(SkinTemp ~ as.numeric(date), data = data)

# Build equation strings
eq_air <- paste0("Air: y = ", round(coef(model_air)[2], 3), "x + ", round(coef(model_air)[1], 1), 
                 " (R?? = ", round(summary(model_air)$r.squared, 3), ")")
eq_skin <- paste0("Skin: y = ", round(coef(model_skin)[2], 3), "x + ", round(coef(model_skin)[1], 1), 
                  " (R?? = ", round(summary(model_skin)$r.squared, 3), ")")

# 3. Plot
ggplot(data, aes(x = date)) +
  # Trend lines (thin dashed lines)
  geom_smooth(aes(y = AirTemp), method = "lm", color = "steelblue", linetype = "dashed", se = FALSE, size = 0.5) +
  geom_smooth(aes(y = SkinTemp), method = "lm", color = "darkorange3", linetype = "dashed", se = FALSE, size = 0.5) +
  
  # Main time series lines
  geom_line(aes(y = AirTemp, color = "2m Air Temperature"), size = 0.8, alpha = 0.8) +
  geom_line(aes(y = SkinTemp, color = "Skin Temperature"), size = 0.8) +
  
  # 0??C threshold
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40", size = 0.5) +
  
  # Key dates (vertical reference lines)
  geom_vline(xintercept = as.Date("2025-03-08"), color = "blue", linetype = "dotted", size = 0.8) + 
  geom_vline(xintercept = as.Date("2025-04-08"), color = "darkgreen", linetype = "dotted", size = 0.8) + 
  geom_vline(xintercept = as.Date("2025-04-22"), color = "red", linetype = "solid", size = 1) + 
  
  # Text annotations (y-values adjusted; controlled via vjust)
  annotate("text", x = as.Date("2025-03-08"), y = 5, label = "Thaw Onset", 
           angle = 90, vjust = -0.5, size = 3.5, color = "blue") +
  annotate("text", x = as.Date("2025-04-08"), y = 5, label = "Peak Rainfall", 
           angle = 90, vjust = -0.5, size = 3.5, color = "darkgreen") +
  annotate("text", x = as.Date("2025-04-22"), y = 5, label = "Landslide Event", 
           color = "red", angle = 90, vjust = -0.5, size = 3.8, fontface = "bold") +
  
  # Regression equation label box
  annotate("label", x = min(data$date) + days(5), y = max(data$AirTemp) - 0.1, 
           label = paste(eq_air, eq_skin, sep = "\n"), 
           hjust = 0, size = 3.5, fill = "white", alpha = 0.8) +
  
  # Aesthetic settings
  scale_color_manual(values = c("2m Air Temperature" = "steelblue", "Skin Temperature" = "darkorange3")) +
  scale_x_date(date_breaks = "15 days", date_labels = "%d %b") +
  scale_y_continuous(breaks = seq(-20, 20, 5)) +
  labs(
    title = "Thermal Evolution and Triggering Factors",
    subtitle = "Analysis of ERA5-Land Temperature Profiles and Trend Slopes",
    x = "Date",
    y = "Temperature (??C)",
    color = "Data Source"
  ) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )
