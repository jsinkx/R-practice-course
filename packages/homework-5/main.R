library(MASS)  
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)

options(warn = -1)

data("Boston")
data_houses <- Boston

head(data_houses)

dim(data_houses)

str(data_houses)

summary(data_houses)

# crime   - уровень преступности на человека населения в разбивке по городам
# zn      - количество земель под жилую застройку, разделенных на участки площадью более 25 000 кв.футов
# indus   - доля акров (0.4 гектар), не связанных с розничной торговлей, в расчете на один город
# chas    - переменная Charles River (= 1, если участок граничит с рекой; 0 в противном случае)
# nox     - концентрация оксидов азота (частей на 10 миллионов)
# rm      - среднее количество комнат в доме
# age     - доля жилых помещений, построенных владельцами до 1940
# dis     - взвешенные расстояния до пяти бостонских центров занятости
# rad     - индекс доступности радиальных автомагистралей
# tax     - я ставка налога на недвижимость с полной стоимости на 10 000 долларов
# ptratio - соотношение учеников и учителей по городам.
# black   - доля чернокожих в разбивке по городам
# lstat   - процент населения с более низким статусом
# medv    - средняя стоимость домов, занимаемых владельцами, на 1000 долларов США

# Дополнительные преобразование
replace_na_with_mean <- function(df) {
  df[] <- lapply(df, function(x) if (is.numeric(x)) ifelse(is.na(x), mean(x, na.rm = TRUE), x) else x)
  return(df)
}

number_of_NA <- length(which(is.na(data_houses) == TRUE))
print(paste("Число пропущенных значений:", number_of_NA))

data_houses <- replace_na_with_mean(data_houses)

number_of_NA <- length(which(is.na(data_houses) == TRUE))
print(paste("Число пропущенных значений:", number_of_NA))

data_houses$crime <- data_houses$crim

str(data_houses)

print(dim(data_houses))       
print(names(data_houses))   
print(head(data_houses$crime)) 

data_houses$crime <- as.numeric(as.character(data_houses$crime))
data_houses$medv <- as.numeric(as.character(data_houses$medv))

# Проверяем итоговые данные
summary(data_houses)

# --- Постановка задачи ---
# Цель: Выявить, какие факторы (переменные) оказывают влияние на среднюю стоимость домов (medv).

# --- Глобальная гипотеза ---
# Средняя стоимость домов (medv) зависит от социально-экономических, экологических и инфраструктурных факторов.

# --- Субгипотезы ---
# 1. Уровень преступности (crime) оказывает влияние на стоимость жилья.
# 2. Близость к реке Чарльз (chas) влияет на стоимость жилья.
# 3. Количество комнат (rm) оказывает положительное влияние на стоимость жилья.
# 4. Концентрация оксидов азота (nox) оказывает отрицательное влияние на стоимость жилья.
# 5. Соотношение учеников и учителей (ptratio) влияет на стоимость жилья.

# --- Предварительный анализ данных ---

# Построение корреляционной матрицы для количественных переменных
quantitative_vars <- data_houses %>% select(-chas)  # chas - категориальная переменная
cor_matrix <- cor(quantitative_vars)

# Визуализация корреляционной матрицы
cor_matrix <- cor(data_houses)
colors     <- colorRampPalette(c("blue", "white", "red"))(200)

corrplot(cor_matrix, col = colors, tl.col = "black", tl.srt = 45, method = "color", addCoef.col = "black", number.cex = 0.7)
# --- Гипотеза 1: Уровень преступности (crime) влияет на стоимость жилья ---
# H0: Уровень преступности не влияет на стоимость жилья.
# Hа: Уровень преступности отрицательно влияет на стоимость жилья.

# Корреляционный анализ
cor_test_crime <- cor.test(data_houses$crime, data_houses$medv)
cor_test_crime

# Визуализация зависимости
ggplot(data_houses, aes(x = crime, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title = "Зависимость стоимости жилья от уровня преступности",
       x = "Уровень преступности (crime)",
       y = "Средняя стоимость домов (тыс. $)") +
  theme_minimal()

# --- Гипотеза 2: Близость к реке Чарльз (chas) влияет на стоимость жилья ---
# H0: Близость к реке не влияет на стоимость жилья.
# Hа: Близость к реке положительно влияет на стоимость жилья.

# t-тест для независимых выборок
t_test_chas <- t.test(medv ~ chas, data = data_houses)
t_test_chas

# Визуализация
ggplot(data_houses, aes(x = as.factor(chas), y = medv, fill = as.factor(chas))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Средняя стоимость домов в зависимости от близости к реке Чарльз",
       x = "Близость к реке Чарльз (1 - рядом, 0 - не рядом)",
       y = "Средняя стоимость домов (тыс. $)") +
  scale_fill_manual(values = c("skyblue", "orange")) +
  theme_minimal()

# --- Гипотеза 3: Количество комнат (rm) положительно влияет на стоимость жилья ---
# H0: Количество комнат не влияет на стоимость жилья.
# Hа: Количество комнат положительно влияет на стоимость жилья.

# Корреляционный анализ
cor_test_rm <- cor.test(data_houses$rm, data_houses$medv)
cor_test_rm

# Визуализация
ggplot(data_houses, aes(x = rm, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue", se = TRUE) +
  labs(title = "Зависимость стоимости жилья от количества комнат",
       x = "Среднее количество комнат (rm)",
       y = "Средняя стоимость домов (тыс. $)") +
  theme_minimal()

# --- Гипотеза 4: Уровень концентрации оксидов азота (nox) влияет на стоимость жилья ---
# H0: Уровень концентрации оксидов азота не влияет на стоимость жилья.
# Hа: Уровень концентрации оксидов азота отрицательно влияет на стоимость жилья.

# Корреляционный анализ
cor_test_nox <- cor.test(data_houses$nox, data_houses$medv)
cor_test_nox

# Визуализация
ggplot(data_houses, aes(x = nox, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "green", se = TRUE) +
  labs(title = "Зависимость стоимости жилья от концентрации оксидов азота",
       x = "Концентрация оксидов азота (nox)",
       y = "Средняя стоимость домов (тыс. $)") +
  theme_minimal()

# --- Гипотеза 5: Соотношение учеников и учителей (ptratio) влияет на стоимость жилья ---
# H0: Соотношение учеников и учителей не влияет на стоимость жилья.
# Hа: Более высокое соотношение учеников и учителей отрицательно влияет на стоимость жилья.

# Корреляционный анализ
cor_test_ptratio <- cor.test(data_houses$ptratio, data_houses$medv)
cor_test_ptratio

# Визуализация
ggplot(data_houses, aes(x = ptratio, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "purple", se = TRUE) +
  labs(title = "Зависимость стоимости жилья от соотношения учеников и учителей",
       x = "Соотношение учеников и учителей (ptratio)",
       y = "Средняя стоимость домов (тыс. $)") +
  theme_minimal()

# --- Постановка задачи ---
# Цель: Выявить влияние различных факторов на стоимость жилья (`medv`).

# --- Гипотезы ---
# H0: Распределение переменных `medv`, `crime` и других количественных переменных нормально.
# Hа: Распределение этих переменных отклоняется от нормального.

# --- Тесты на нормальность ---
# Шапиро-Уилк
shapiro_medv <- shapiro.test(data_houses$medv)
shapiro_crime <- shapiro.test(data_houses$crime)

# Колмогоров-Смирнов
ks_medv <- ks.test(data_houses$medv, "pnorm", mean(data_houses$medv), sd(data_houses$medv))
ks_crime <- ks.test(data_houses$crime, "pnorm", mean(data_houses$crime), sd(data_houses$crime))

print("Тест Шапиро-Уилка для медв:")
print(shapiro_medv)

print("Тест Колмогорова-Смирнова для медв:")
print(ks_medv)

# --- Анализ категориальных данных ---
# Переменная `chas` (близость к реке Чарльз)
# H0: Близость к реке не связана с высокой стоимостью жилья (medv).
# Hа: Близость к реке связана с высокой стоимостью жилья.

# Преобразование в категории
data_houses$medv_cat <- ifelse(data_houses$medv > median(data_houses$medv), "high", "low")

# Таблица сопряженности
contingency_table <- table(data_houses$chas, data_houses$medv_cat)

# Тест хи-квадрат
chi_test <- chisq.test(contingency_table)

# Точный тест Фишера
fisher_test <- fisher.test(contingency_table)

print("Тест хи-квадрат:")
print(chi_test)

print("Точный тест Фишера:")
print(fisher_test)

# --- Мощность критериев ---
# Уровень значимости
alpha <- 0.05

# Шапиро-Уилк (для мощности требуется симуляция или расчет)
# Оценка мощности на основе п-значения
shapiro_power <- ifelse(shapiro_medv$p.value < alpha, "H0 отклонена (мощность есть)", "H0 не отклонена (мощность низкая)")

# Хи-квадрат
chi_power <- ifelse(chi_test$p.value < alpha, "H0 отклонена (мощность есть)", "H0 не отклонена (мощность низкая)")

# Результаты мощности
print("Оценка мощности тестов:")
print(paste("Шапиро-Уилк:", shapiro_power))
print(paste("Хи-квадрат:", chi_power))

# --- Графики ---
# Распределение `medv` (для проверки нормальности)
ggplot(data_houses, aes(x = medv)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Распределение средней стоимости домов", x = "Стоимость домов (тыс. $)", y = "Частота") +
  theme_minimal()

# Итоговые выводы
print("Итоговые выводы:")
print("1. Тесты на нормальность: Распределение `medv` и `crime` не является нормальным (по Шапиро-Уилку).")
print("2. Хи-квадрат и точный тест Фишера: Есть статистически значимая связь между близостью к реке и стоимостью домов.")