# Load libraries
install.packages("ggplot2")   # only run once
library(ggplot2)
library(graphics)

# Read dataset
crime_data <- read.csv("C:/Users/Kasturi/Downloads/crime_safety_dataset.csv")

# --- BOX PLOT (Victim Age by Crime Type) ---
boxplot(victim_age ~ crime_type, data = crime_data,
        xlab = "Crime Type",
        ylab = "Victim Age",
        main = "Victim Age by Crime Type",
        col = c("skyblue", "lightgreen", "pink", "orange", "yellow", "purple"),
        las = 2)  # rotate labels

# --- HISTOGRAM (Victim Age) ---
hist(crime_data$victim_age,
     xlab = "Victim Age",
     col = "blue",
     border = "green",
     main = "Histogram of Victim Ages")

# --- PIE CHART (Crime Type Distribution) ---
crime_counts <- table(crime_data$crime_type)
pie(crime_counts,
    labels = paste(names(crime_counts), "\n", round(100*crime_counts/sum(crime_counts),1), "%"),
    col = rainbow(length(crime_counts)),
    main = "Crime Type Distribution")

# --- BAR CHART (Crime Counts by Type) ---
barplot(crime_counts,
        col = "lightblue",
        main = "Bar Chart of Crime Types",
        xlab = "Crime Type",
        ylab = "Count",
        las = 2)

# --- OUTLIERS DETECTION ---
outlier_values <- boxplot.stats(crime_data$victim_age)$out
print("Outliers in Victim Age:")
print(outlier_values)

# Optional: mark outliers on a plot
boxplot(crime_data$victim_age,
        main = "Boxplot of Victim Age (with Outliers)",
        col = "lightcoral")
