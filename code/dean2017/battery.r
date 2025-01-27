# Install and load required packages
if (!require("pacman")) install.packages("pacman")

# Load necessary packages for analysis
pacman::p_load(
  emmeans, # For estimated marginal means
  car, # For regression diagnostics
  ggplot2, # For plotting
  dplyr, # For data manipulation
  readr, # For reading data
  here # For file path management
)

## "Here" package is used to make the file path relative to the project directory
# Load data
file_path <- here::here("data", "dean2017", "battery.txt") # File path to data file

battery.data <- read.table(file_path, header = TRUE)
battery.data$fType <- factor(battery.data$Type)
head(battery.data, 3) # Display first 3 rows of data
levels(battery.data$fType) # Display levels of fType

model1 <- aov(LPUC ~ fType, data = battery.data) # Fit aov model
anova(model1) # Display 1-way ANOVA

## Post-hoc analysis

## Compute and save emmeans
lsmType <- emmeans::emmeans(model1, ~fType)

lsmType # Display estimated marginal means

## Individual contrasts: estimates, CIs, tests with 90% confidence interval

## Pairwise comparisons

summary(
  contrast(lsmType, list(
    Duty = c(1, 1, -1, -1) / 2,
    Brand = c(1, -1, 1, -1) / 2,
    DB = c(1, -1, -1, 1) / 2
  )),
  infer = c(TRUE, TRUE),
  level = 0.95,
  side = "two-sided"
)

# Multiple comparisons
confint(lsmType, level = 0.90) # Display lsmeans and 90% confidence intervals

# Tukey’s method
summary(contrast(lsmType, method = "pairwise", adjust = "tukey"),
  infer = c(TRUE, TRUE), level = 0.99, side = "two-sided"
)

# Dunnett’s method for comparing treatments to a control
summary(contrast(lsmType, method = "trt.vs.ctrl", adjust = "mvt", ref = 1),
  infer = c(TRUE, TRUE), level = 0.99, side = "two-sided"
)
