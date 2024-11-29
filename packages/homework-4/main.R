# install.packages("MASS")
# install.packages("gridExtra")
# install.packages("corrplot")
# install.packages("ggplot2")
# install.packages("lmtest")
# install.packages("neuralnet")
# install.packages("randomForest")

library(MASS)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(lmtest)
library(neuralnet)
library(randomForest)

# Загрузка данных
data("Boston")

# Проверка нормальности с помощью теста Шапиро-Уилка
shapiro_results <- sapply(Boston, function(x) shapiro.test(x)$p.value)

# Фильтрация только числовых переменных
shapiro_results <- shapiro_results[sapply(Boston, is.numeric)]

# Вывод результатов
print(shapiro_results)

# Визуализация распределения некоторых числовых переменных
plots <- lapply(names(Boston)[1:3], function(var) {
  ggplot(Boston, aes_string(x = var)) +
    geom_histogram(bins = 30, aes(y = ..density..), fill = "steelblue", alpha = 0.7) +
    geom_density(color = "red") +
    ggtitle(paste("Распределение", var))
})

grid.arrange(grobs = plots, ncol = 3)

# Расчет корреляционной матрицы
cor_matrix <- cor(Boston)

# Визуализация корреляционной матрицы


# Построение корреляционного графика
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Построение модели линейной регрессии
model <- lm(medv ~ rm, data = Boston)

# Вывод результатов модели
summary(model)

dw_test <- dwtest(model)
print(dw_test)

shapiro_test_residuals <- shapiro.test(residuals(model))
print(shapiro_test_residuals)

r_squared <- summary(model)$r.squared
print(paste("Коэффициент детерминации R²:", r_squared))

# Построение множественной линейной регрессии
multi_model <- lm(medv ~ rm + lstat + age, data = Boston)

# Вывод результатов модели
summary(multi_model)

# Например, если lstat оказался незначимым
updated_model <- lm(medv ~ rm + age, data = Boston)
summary(updated_model)

r_squared <- summary(updated_model)$r.squared
adjusted_r_squared <- summary(updated_model)$adj.r.squared

print(paste("Коэффициент детерминации R²:", r_squared))
print(paste("Скорректированный R²:", adjusted_r_squared))

r_squared <- summary(updated_model)$r.squared
adjusted_r_squared <- summary(updated_model)$adj.r.squared

print(paste("Коэффициент детерминации R²:", r_squared))
print(paste("Скорректированный R²:", adjusted_r_squared))

# График остатков
residuals_data <- data.frame(Actual = Boston$medv, Predicted = predict(updated_model), Residuals = residuals(updated_model))

ggplot(residuals_data, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("График остатков") +
  xlab("Предсказанные значения") +
  ylab("Остатки")


ggplot(residuals_data, aes(x = Residuals)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red") +
  ggtitle("Распределение остатков") +
  xlab("Остатки") +
  ylab("Плотность")

# Квадратичная модель
quadratic_model <- lm(medv ~ poly(age, 2), data = Boston)
summary(quadratic_model)

# Кубическая модель
cubic_model <- lm(medv ~ poly(age, 3), data = Boston)
summary(cubic_model)

# Экспоненциальная модель
exponential_model <- lm(log(medv) ~ age, data = Boston)
summary(exponential_model)

# Сравнение моделей
results <- data.frame(
  Model = c("Квадратичная", "Кубическая", "Экспоненциальная"),
  AIC = c(AIC(quadratic_model), AIC(cubic_model), AIC(exponential_model)),
  BIC = c(BIC(quadratic_model), BIC(cubic_model), BIC(exponential_model)),
  R2 = c(
    summary(quadratic_model)$r.squared,
    summary(cubic_model)$r.squared,
    summary(exponential_model)$r.squared
  )
)

print(results)

# Нормализация данных
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Нормализуем данные
Boston_norm <- as.data.frame(lapply(Boston, normalize))

# Обучаем модель
set.seed(123) # Для воспроизводимости
nn_model <- neuralnet(medv ~ ., data = Boston_norm, hidden = c(5, 3), linear.output = TRUE)

# Предсказания
predicted <- predict(nn_model, newdata = Boston_norm)
actual <- Boston_norm$medv

# Сравнение результатов
results <- data.frame(Actual = actual, Predicted = predicted)
print(head(results))

# Визуализация кривой обучения
plot(nn_model)

mse_nn <- mean((actual - predicted)^2)
print(mse_nn)

# График предсказаний
plot(actual, predicted,
  main = "Нейронная сеть: фактические vs предсказанные значения",
  xlab = "Фактические значения", ylab = "Предсказанные значения", col = "blue", pch = 20
)
abline(0, 1, col = "red") # Линия y=x для удобства сравнения

# Используем нормализованные данные
Boston_norm <- Boston_norm # Если данные уже нормализованы

# Обучаем модель случайного леса
set.seed(123) # Для воспроизводимости
rf_model <- randomForest(medv ~ ., data = Boston_norm, importance = TRUE, ntree = 100)

# Важность признаков
importance_rf <- importance(rf_model)

# Построение графика значимости переменных
varImpPlot(rf_model, main = "Важность признаков в модели случайного леса")

# Предсказания модели
predicted_rf <- predict(rf_model, newdata = Boston_norm)
actual <- Boston_norm$medv

# Рассчитываем MSE
mse_rf <- mean((actual - predicted_rf)^2)
# Рассчитываем RMSE
rmse_rf <- sqrt(mse_rf)

# Выводим результаты
cat("MSE случайного леса:", mse_rf, "\n")
cat("RMSE случайного леса:", rmse_rf, "\n")
