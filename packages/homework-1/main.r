library(jsonlite)

library(RColorBrewer)
library(wordcloud)
library(ggplot2)

library(textstem)
library(stringr)
library(tm)

options(warn = -1)

# * Опционально

getwd()
setwd("/Users/artemiy/Library/CloudStorage/GoogleDrive-andrikv@gmail.com/My Drive/University/3 year/R/Homework/packages/homework-1")

# ---

FILE_PATH <- "./parser/data/data.json"
data <- fromJSON(FILE_PATH)

channel_name <- "ТОПОР - Горячие новости"
df <- data.frame(text = data[[channel_name]], stringsAsFactors = FALSE)

head(df, 5)

# Функция для очистки текста
additional_stop_words <- c("изза", "а", "к", "он", "его", "у", "о", "её", "ее", "до", "так", "они", "и", "в", "не", "на", "с", "как", "что", "по", "из", "за", "то", "да", "это", "же", "для", "или", "но")
stop_words <- stopwords("ru")
stop_words <- c(stop_words, additional_stop_words)
stop_words <- unique(stop_words)

stop_words

clean_text <- function(text) {
  text <- str_to_lower(text)
  text <- str_replace_all(text, "(http|https|www)\\S+", "")
  text <- str_replace_all(text, "[^\\w\\s]", "")
  text <- str_replace_all(text, "\\d+", "")
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_squish(text)

  if (str_trim(text) == "") {
    return(NA)
  }

  words <- str_split(text, " ", simplify = TRUE)
  cleaned_words <- words[!words %in% stop_words]
  cleaned_text <- str_c(cleaned_words, collapse = " ")

  return(cleaned_text)
}

# Применяем очистку текста и удаляем NA
df$text <- sapply(df$text, clean_text)
df <- na.omit(df)

head(df, 5)

# Объединение текста в одну строку для дальнейшего преобразования в отдельные слова
text_vector <- as.vector(df$text)
text <- paste(text_vector, collapse = " ")

text_lower <- tolower(text)
text_clean <- gsub("[[:punct:]]", "", text_lower)

words <- unlist(strsplit(text_clean, "\\s+"))
words_lemmatize <- lemmatize_words(words)

# * Подсчет частоты слов
word_frequency <- table(words_lemmatize)
word_frequency <- sort(word_frequency, decreasing = TRUE)

word_frequency[1:30]

df_word_frequencies <- as.data.frame(word_frequency)
colnames(df_word_frequencies) <- c("word", "frequency")

head(df_word_frequencies, 5)

# ---
# * Визуализации
# ---

# * Топ N слов по частоте

TOP_WORDS_COUNT <- 15

ggplot(df_word_frequencies[1:TOP_WORDS_COUNT, ], aes(x = reorder(word, -frequency), y = frequency, fill = word)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = str_glue("Топ {TOP_WORDS_COUNT} слов по частоте"),
    x = "Слова",
    y = "Частота"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_cartesian(clip = "off") +
  geom_text(aes(label = frequency), angle = 90, vjust = -0.3, size = 5, position = position_dodge(width = 1))

# * Облако слов

MIN_FREQUENCY <- 50
MAX_WORDS <- 350

set.seed(3)
wordcloud(
  words = df_word_frequencies$word,
  freq = df_word_frequencies$frequency,
  min.freq = MIN_FREQUENCY,
  max.words = MAX_WORDS,
  random.order = FALSE,
  rot.per = 0.2,
  scale = c(2, 1),
  colors = brewer.pal(8, "Set2"),
  background = "lightgray"
)
