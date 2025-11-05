install.packages("XML")

data <- read.csv("C:/Users/Kasturi/Downloads/crime_safety_dataset.csv")

# --- Explore the data ---
print(head(data))           
summary(data)            
print(is.data.frame(data))  
print(ncol(data))    
print(nrow(data))
print(names(data))    

if ("crime_rate" %in% names(data)) {
  max_crime <- max(data$crime_rate, na.rm = TRUE)
  retval <- subset(data, crime_rate == max_crime)
  print(retval)
  
  high_crime <- subset(data, crime_rate > 70)  
  write.csv(high_crime, "C:/Users/Kasturi/Downloads/high_crime_output.csv")
  
  newdata <- read.csv("C:/Users/Kasturi/Downloads/high_crime_output.csv")
  print(head(newdata))
}

install.packages("xlsx")
library("xlsx")
excel_data <- read.xlsx("C:/Users/Kasturi/Downloads/input.xlsx", sheetIndex = 1)
print(head(excel_data))

library("XML")
library("methods")
#result <- xmlParse(file = "C:/Users/Kasturi/Downloads/input.xml")
#print(result)
