library(viridis)

x_values <- seq.int(-10, 10, by = 1)
y_values <- x_values^3 + 3 * x_values^2 - 8 * x_values - 10

df <- data.frame(x = x_values, y = y_values)

df |>
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  labs(
    x = "x",
    y = "y"
  ) +
  scale_color_viridis_c() +
  scale_color_viridis_c(option = "C", direction = -1) +
  theme_minimal()
