# * Статистика

# Биноминальное распредление

k <- 6  # мой порядковый номер в группе
n <- 30 # количество студентов в группе

p    <- k / n
size <- n

binom_data <- rbinom(10000, size = size, prob = p)

binom_mean   <- mean(binom_data)
binom_var    <- var(binom_data)
binom_sd     <- sd(binom_data)
binom_mode   <- as.numeric(names(sort(table(binom_data), decreasing = TRUE)[1]))
binom_median <- median(binom_data)

cat("Биномиальное распределение:\n")
cat("Мат. ожидание:", binom_mean, "\nДисперсия:", binom_var, "\nСтандартное отклонение:", binom_sd, 
    "\nМода:", binom_mode, "\nМедиана:", binom_median, "\n\n")

hist(binom_data, probability = TRUE, main = "Биномиальное распределение", xlab = "Значение", col = "lightblue")
lines(density(binom_data), col = "red")

'---'

# Хи-квадрат распределение
df       <- (n - 1) + k
chi_data <- rchisq(10000, df = df)

chi_mean   <- mean(chi_data)
chi_var    <- var(chi_data)
chi_sd     <- sd(chi_data)
chi_mode   <- df - 2
chi_median <- qchisq(0.5, df)

cat("Хи-квадрат распределение:\n")
cat("Мат. ожидание:", chi_mean, "\nДисперсия:", chi_var, "\nСтандартное отклонение:", chi_sd, 
    "\nМода:", chi_mode, "\nМедиана:", chi_median, "\n\n")

hist(chi_data, probability = TRUE, main = "Хи-квадрат распределение", xlab = "Значение", col = "lightblue")
lines(density(chi_data), col = "red")

'---'

# Нормальное распределение
m     <- k * 100
sigma <- sqrt(n + k)

norm_data   <- rnorm(10000, mean = m, sd = sigma)
norm_mean   <- mean(norm_data)
norm_var    <- var(norm_data)
norm_sd     <- sd(norm_data)
norm_mode   <- m # У нормального распрделения мода совпадает математичексим ожиданием
norm_median <- median(norm_data)

cat("Нормальное распределение:\n")
cat("Мат. ожидание:", norm_mean, "\nДисперсия:", norm_var, "\nСтандартное отклонение:", norm_sd, 
    "\nМода:", norm_mode, "\nМедиана:", norm_median, "\n\n")

hist(norm_data, probability = TRUE, main = "Нормальное распределение", xlab = "Значение", col = "lightblue")
lines(density(norm_data), col = "red")

'---'

# * Теория вероятности (задачки)

# Компания, сдающая машины в прокат, владеет 60 автомобилями, 35 из
# них фирменной расцветки. Известно, что 5 автомобилей в данный момент
# находятся в ремонте. Найти вероятность того, что

total_cars   <- 60       # Общее количество автомобилей
branded_cars <- 35       # Число фирменных автомобилей
repair_cars  <- 5        # Число автомобилей в ремонте

p <- branded_cars / total_cars # Вероятность, что автомобиль фирменной расцветки

# Вероятность того, что ровно два автомобиля в ремонте фирменной расцветки
x                <- 2
prob_two_branded <- dbinom(x, size = repair_cars, prob = p)

cat("Вероятность того, что ровно два автомобиля из ремонтирующихся — фирменной расцветки:", prob_two_branded, "\n")

# Вероятность того, что хотя бы один из ремонтирующихся автомобилей фирменной расцветки
prob_at_least_one <- 1 - dbinom(0, size = repair_cars, prob = p)

cat("Вероятность того, что хотя бы один из ремонтирующихся автомобилей — фирменной расцветки:", prob_at_least_one, "\n")

'---'

# Вы решили узнать, есть ли среди ваших однокурсников кто-нибудь, у
# кого день рождения (без учета года) совпадает с вашим.
# Понятно, что если вы узнаете дату др только одного однокурсника, то
# вероятность того, что вы сразу нашли нужного, довольно мала.
# Если вы узнаете дату др двоих однокурсников, то вероятность того, что
# среди них есть нужный, чуть выше. Если бы на курсе училось 100-200-300 человек, 
# то вероятность того, что среди них есть хотя бы один с нужной датой
# ДР, должна быть довольно большой.

# Задача связана с парадоксом дней рождения =)
# https://en.wikipedia.org/wiki/Birthday_problem

# Вероятность хотя бы одного совпадения среди 390 человек
n_a               <- 390 # Количество человек
prob_no_match     <- (1 - 1/365) ^ n_a
prob_at_least_one <- 1 - prob_no_match

cat("Вероятность того, что среди 390 человек есть хотя бы один с вашей датой рождения:", prob_at_least_one, "\n")

# Нужно определить минимальное n, чтобы вероятность была больше 0.5
n_b <- 1

while ((1 - (1 - 1/365)^n_b) <= 0.5) {
  n_b <- n_b + 1
}

cat("Минимальное количество человек, чтобы вероятность была больше 0.5:", n_b, "\n")

'---'

# * Покер
# В карточной игре покер игрок получает 5 карт из колоды в 52 карты.
# Задача игрока собрать наиболее сильную комбинацию карт. Комбинации бывают следующие:
# а) пара - две карты одного номинала;
# б) две пары - две карты одного номинала, две карты другого;
# в) тройка - три карты одного номинала;
# г) стрит - пять последовательных по номиналу карт (предполагается, что за тузом по номиналу следует двойка);
# д) флэш - все карты одной масти;
# е) три+два - три карты одного номинала, две карты другого;
# ж) каре - четыре карты одного номинала;
# з) стрит-флэш - пять последовательных по номиналу карт одной масти;
# b) ройал-флэш - туз, король, дама, валет и десятка одной и той же масти

# Найдите вероятность получения каждой из перечисленных комбинаций
# при случайной сдаче карт. Вычислите вероятность того, что не выпадет ни
# одна из вышеперечисленных комбинаций.

# Функция для подсчета факториала
factorial <- function(x) {
  if (x == 0) return(1)
  return(prod(1:x))
}

# Функция для подсчета числа сочетаний C(n, k)
choose <- function(n, k) {
  factorial(n) / (factorial(k) * factorial(n - k))
}

# Общее количество комбинаций
total_combinations <- choose(52, 5)

# Пара (одна пара и три остальные карты разные по номиналу)
pairs <- choose(13, 1) * choose(4, 2) * choose(12, 3) * (choose(4, 1)^3)
prob_pair <- pairs / total_combinations

# Две пары
two_pairs <- choose(13, 2) * (choose(4, 2)^2) * choose(11, 1) * choose(4, 1)
prob_two_pairs <- two_pairs / total_combinations

# Тройка
three_of_a_kind <- choose(13, 1) * choose(4, 3) * choose(12, 2) * (choose(4, 1)^2)
prob_three_of_a_kind <- three_of_a_kind / total_combinations

# Стрит
straights     <- 10 * (choose(4, 1)^5) - 10 * 4  # вычитаем стрит-флэши
prob_straight <- straights / total_combinations

# Флэш
flushes    <- choose(4, 1) * choose(13, 5) - 10  # вычитаем стрит-флэши
prob_flush <- flushes / total_combinations

# Фулл-хаус (три+два)
full_house      <- choose(13, 1) * choose(4, 3) * choose(12, 1) * choose(4, 2)
prob_full_house <- full_house / total_combinations

# Каре
four_of_a_kind      <- choose(13, 1) * choose(4, 4) * choose(12, 1) * choose(4, 1)
prob_four_of_a_kind <- four_of_a_kind / total_combinations

# Стрит-флэш
straight_flushes    <- 10 * choose(4, 1)
prob_straight_flush <- straight_flushes / total_combinations

# Ройал-флэш
royal_flushes    <- 1 * choose(4, 1)
prob_royal_flush <- royal_flushes / total_combinations

prob_none <- 1 - (prob_pair + prob_two_pairs + prob_three_of_a_kind + 
                    prob_straight + prob_flush + prob_full_house + 
                    prob_four_of_a_kind + prob_straight_flush + prob_royal_flush)

cat("Пара:", prob_pair, "\n")
cat("Две пары:", prob_two_pairs, "\n")
cat("Тройка:", prob_three_of_a_kind, "\n")
cat("Стрит:", prob_straight, "\n")
cat("Флэш:", prob_flush, "\n")
cat("Фулл-хаус:", prob_full_house, "\n")
cat("Каре:", prob_four_of_a_kind, "\n")
cat("Стрит-флэш:", prob_straight_flush, "\n")
cat("Ройал-флэш:", prob_royal_flush, "\n")
cat("Вероятность того, что не выпадет ни одна комбинация:", prob_none, "\n")
