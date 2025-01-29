library(MASS)
library(ISLR2)
library(tidyverse)
library(ggthemes)
library(viridis)
glimpse(ISLR2::Boston)

boston <- ISLR2::Boston

view(boston)

rounded <- function(n, digits = 2) {
  return(round(n, digits))
}

# Predict median housing value from percentage of population with low socioeconmic status

model <- glm(medv ~ lstat, data = boston, family = "gaussian")
rounded(coef(model))
rounded(confint(model))

plot(boston$lstat, boston$medv)
abline(model)

boston |> ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "#D64550", linewidth = 1.2) +
  labs(
    title = "Median housing value as a function of low socio-economic status",
    x = "Low socio-economic status (%)",
    y = "Median home value (thousands of $)"
  ) +
  scale_color_viridis_c(option = "C", direction = -1) +
  theme_minimal()

summary(model)


model2 <- glm(medv ~ lstat + age, data = boston, family = "gaussian")
summary(model2)

model3 <- glm(medv ~ ., data = boston, family = "gaussian")
summary(model3)

model3_reduced <- glm(medv ~ 1, data = boston, family = "gaussian")
summary(model3_reduced)

anova(model3_reduced, model3)

model4 <- glm(medv ~ . - age - indus, data = boston, family = "gaussian")
summary(model4)
anova(model3_reduced, model4)


predict(model4)

boston <- boston |>
  mutate(medv_predicted_m1 = predict(model)) |>
  mutate(medv_predicted_m2 = predict(model2)) |>
  mutate(medv_predicted_m3 = predict(model3)) |>
  mutate(medv_predicted_m4 = predict(model4)) |>
  relocate(medv, .before = crim) |>
  relocate(medv_predicted_m1, .after = medv) |>
  relocate(medv_predicted_m2, .after = medv_predicted_m1) |>
  relocate(medv_predicted_m3, .after = medv_predicted_m2) |>
  relocate(medv_predicted_m4, .after = medv_predicted_m3) |>
  select(-medv_predicted)

boston |>
  select(1:5) |>
  head(3)

library(glmtoolbox)

models <- list(model, model2, model3, model4)
model_names <- c("m1", "m2", "m3", "m4")

r_squared_df <- data.frame(
  model = model_names,
  adj_r2 = sapply(models, adjR2)
)


adjR2(model)
adjR2(model2)
adjR2(model3)
adjR2(model4)

library(car)
car::vif(model4)

cor(boston$rad, boston$tax)

model5 <- glm(medv ~ . - age - indus - rad, data = boston, family = "gaussian")
summary(model5)

boston_orig <- ISLR2::Boston

model5 <- glm(medv ~ . - age - indus - tax, data = boston_orig, family = "gaussian")
summary(model5)
adjR2(model5)
car::vif(model5)

library(classpackage)
anova_check(model5)
