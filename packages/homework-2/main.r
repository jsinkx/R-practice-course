###
# Датасет mtcars, встроенный в R
# Доку можно почитать здесь
# https://cran.r-project.org/web/packages/explore/vignettes/explore-mtcars.html
# https://www.w3schools.com/r/r_stat_data_set.asp
# https://www.youtube.com/watch?v=5G3GUciLRDc
#
# Лее ежжи, молодой html 🇸 🇰 🇺 - 🇫 4ever
# Эх, сейчас бы покодить фронтенд на R
# Бэкенд на html, ML на css
# Аналитику на Docker + Ubuntu 8.04 LTS
###

###
# mpg - расход топлива
# hp - мощность
# wt - вес
# cyl - число цилиндров
# gear - число передач
# am - тип коробки передач (0 = автомат, 1 = ручная)
# vs - тип двигателя (0 = V-образный, 1 = рядный)
###

library(ggplot2)

car_list <- list(
  mpg = mtcars$mpg,
  hp = mtcars$hp,
  wt = mtcars$wt,
  cyl = mtcars$cyl,
  gear = mtcars$gear,
  am = mtcars$am,
  vs = mtcars$vs
)

car_list$hp_per_wt <- car_list$hp / car_list$wt # Добавляем новый столбец с расчетом мощности на вес
car_list$transmission <- list(automatic = car_list$am == 0, manual = car_list$am == 1) # Добавляем новый столбец с типом коробки передач

# * Расчеты основных статистик для числовых переменных

means <- sapply(car_list, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE)) # Расчет средних значений по каждому столбцу
print("Средние значения по каждому столбцу")
print(means) # Вывод значений

normalized <- lapply(car_list, function(x) if (is.numeric(x)) (x - min(x)) / (max(x) - min(x)))
print("Нормализованные значения по каждому столбцу")
print(normalized$mpg[1:5])

# * Подготовка датасета
df_car_list <- as.data.frame(car_list)

df_car_list$am <- factor(df_car_list$am, labels = c("Automatic", "Manual")) # Преобразование переменной am в фактор
df_car_list$hp_per_cyl <- df_car_list$hp / df_car_list$cyl # Мощность на цилиндр
df_car_list$log_wt <- log(df_car_list$wt) # Логарифм веса
# Категории расхода топлива
df_car_list$mpg_class <- cut(
  df_car_list$mpg,
  breaks = c(-Inf, 15, 25, Inf),
  labels = c("Low", "Medium", "High")
)

head(df_car_list)

# * Визуализация зависимости расхода топлива от мощности

ggplot(df_car_list, aes(x = hp, y = mpg, color = am)) +
  geom_point(size = 3) + # Поинты на чарте
  geom_smooth(method = "lm", se = FALSE) + # Линия тренда
  labs(
    title = "Зависимость расхода топлива от мощности",
    x = "Мощность (hp)",
    y = "Расход топлива (mpg)",
    color = "Коробка передач"
  ) +
  theme_minimal()

# * Визуализация гистограмма расхода топлива
ggplot(df_car_list, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(
    title = "Распределение расхода топлива",
    x = "Расход топлива (mpg)",
    y = "Частота"
  ) +
  theme_minimal()

# * Визуализация категория расхода топлива (mpg_class)
ggplot(df_car_list, aes(x = mpg_class, fill = mpg_class)) +
  geom_bar() +
  labs(
    title = "Количество автомобилей по классам расхода топлива",
    x = "Класс расхода топлива",
    y = "Количество автомобилей"
  ) +
  scale_fill_manual(values = c("Low" = "red", "Medium" = "yellow", "High" = "green")) +
  theme_minimal()

# * Сохранение в csv
write.csv(df_car_list, "df_car_list.csv", row.names = FALSE)
