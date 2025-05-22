source(".Rprofile")

# Load the data
data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Contributing_Mortality_Trend.xlsx", 
                   sheet = "Analyzable")

# Subset the data
data_1999_2015 <- subset(data, Year >= 1999 & Year <= 2015)
data_2016_2020 <- subset(data, Year >= 2016 & Year <= 2020)

# Linear model for 1999–2015
model_1999_2015 <- lm(log(AAMR) ~ Year, data = data_1999_2015)
beta1_1999_2015 <- coef(model_1999_2015)["Year"]
APC_1999_2015 <- (exp(beta1_1999_2015) - 1) * 100
se_1999_2015 <- summary(model_1999_2015)$coefficients["Year", "Std. Error"]
lower_1999_2015 <- (exp(beta1_1999_2015 - 1.96 * se_1999_2015) - 1) * 100
upper_1999_2015 <- (exp(beta1_1999_2015 + 1.96 * se_1999_2015) - 1) * 100

tidy(model_1999_2015)

# Linear model for 2016–2020
model_2016_2020 <- lm(log(AAMR) ~ Year, data = data_2016_2020)
beta1_2016_2020 <- coef(model_2016_2020)["Year"]
APC_2016_2020 <- (exp(beta1_2016_2020) - 1) * 100
se_2016_2020 <- summary(model_2016_2020)$coefficients["Year", "Std. Error"]
lower_2016_2020 <- (exp(beta1_2016_2020 - 1.96 * se_2016_2020) - 1) * 100
upper_2016_2020 <- (exp(beta1_2016_2020 + 1.96 * se_2016_2020) - 1) * 100

tidy(model_2016_2020)

# Print APCs
cat("APC 1999–2015:", round(APC_1999_2015, 2), "% (95% CI:", round(lower_1999_2015, 2), "to", round(upper_1999_2015, 2), ")\n")
cat("APC 2016–2020:", round(APC_2016_2020, 2), "% (95% CI:", round(lower_2016_2020, 2), "to", round(upper_2016_2020, 2), ")\n")

rm(list = ls())
