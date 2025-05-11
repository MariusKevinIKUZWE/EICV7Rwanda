install.packages("readxl")
library(readxl)

poverty_data <- read_excel("EICV7Rwanda.xlsx", sheet = 1)

View(poverty_data)
