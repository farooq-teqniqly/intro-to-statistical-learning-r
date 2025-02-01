library(ISLR2)
library(tidyverse)

default_dataset = ISLR2::Default
glimpse(default_dataset)

default_dataset$default <- as.factor(default_dataset$default)

model <- glm(default ~ balance, data = default_dataset, family = "binomial")
summary(model)

rounded_coef <- round(coef(model), 3)

round(exp(rounded_coef), 3)
