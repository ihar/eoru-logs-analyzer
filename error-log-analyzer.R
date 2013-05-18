# На сайте http://eoru.ru ведутся логи ошибочных запросов пользователей.
# Ошибки случаются при неверном запросе слова (фразы) или при попытке взломать сайт.
# Все подобные обращения к сайту сохраняются в текстовый файл, по одному файлу на день.
# 
# Анализ логов поможет узнать, как можно улучшить систему поиска на сайте.

data_dir <- "d:/webservers/other/eoru_logs/2012/"
# data_dir <- "./data/"
data_files <- list.files(data_dir, "*.txt")

# Вспомогательные функции
source("functions.r")

# Формат строчки в текстовом файле
# ERROR - 2012-01-01 01:12:04 --> Wrong word requested: 'соорудение'
# Каждую строчку надо уметь разбирать по частям

# Для тестов
txt <- readLines(paste0(data_dir,data_files[1]))[6]

files_list <- paste0(data_dir, data_files)
df <- data.frame()
for (fname in files_list) {
  df <- rbind(df, ProcessFile(fname))
}

Sys.timezone()
df <- CleanData(df)

# Все записи имеют тип ERROR, можно эту колонку дальше не тянуть
unique(df$type)
df$type <- NULL

table(df$message)

df_wrong_word <- subset(df, df$message == "Wrong word requested")
df_wrong_word$message <- NULL

df_wrong_page <- subset(df, df$message == "Page requested")
df_wrong_page$message <- NULL

df_wrong_query <- subset(df, df$message == "Query error")
df_wrong_query$message <- NULL

df_wrong_symbols <- subset(df, df$message == "Severity")
df_wrong_symbols$message <- NULL

# Тут ещё в text повторяется "Illegal character". 
# Тоже бесполезная информация, если повторяется в каждой строчке
df_wrong_symbols$text <- NULL

nrow(df) == nrow(df_wrong_page) + nrow(df_wrong_query) +
            nrow(df_wrong_symbols) + nrow(df_wrong_word)

# Собрать статистику по словам с нулевым результатом поиска
wrong_words_decreasing <- sort(table(df_wrong_word$text), decreasing=TRUE)
word_limit <- 25
head(wrong_words_decreasing, word_limit) # как  львЁнок и черепаха пельи песьню  
wrong_words_top <- wrong_words_decreasing[1:word_limit]
wword <- names(wrong_words_top)[1]
wword_dates <- df_wrong_word[df_wrong_word$text==wword,]

