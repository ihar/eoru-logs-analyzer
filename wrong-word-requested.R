# Запросы Wrong word requested

data_dir <- "d:/webservers/other/eoru_logs/2012/"
# data_dir <- "./data/"
data_files <- list.files(data_dir, "*.txt")

# Вспомогательные функции
source("functions.r")

# Формат строчки в текстовом файле
# ERROR - 2012-01-01 01:12:04 --> Wrong word requested: 'соорудение'
# Каждую строчку надо уметь разбирать по частям

files_list <- paste0(data_dir, data_files)
df <- data.frame()
for (fname in files_list) {
  df <- rbind(df, ProcessFile(fname))
}

df <- CleanData(df)

# Все записи имеют тип ERROR, можно эту колонку дальше не тянуть
unique(df$type)
df$type <- NULL

table(df$message)

df_wrong_word <- subset(df, df$message == "Wrong word requested")
df_wrong_word$message <- NULL

# Частоты встречаемости каждого слова
word_df <- as.data.frame(table(df_wrong_word$text), stringsAsFactors = FALSE)

# Всего запросов
sum(word_df$Freq)
# 27568
# Всего уникальных слов
nrow(word_df)
# 20659

names(word_df) <- c("Word", "Freq")
word_df$Word <- Entity2Txt(word_df$Word)

# Слова по убыванию частоты запроса
word_df_by_freq <- word_df[order(-word_df$Freq),]
# Сохранить в отдельный файл весь список для детального анализа
write.table(word_df_by_freq, 
           file = "wrong-words-by-frequency.csv", 
            sep = ";",
           row.names = FALSE)
# Слова по алфавиту
word_df_by_alphabet <- word_df[order(word_df$Word),]
write.table(word_df_by_alphabet, 
            file = "wrong-words-by-alphabet.csv", 
            sep = ";",
            row.names = FALSE)

word_limit  <-  25
# как  львЁнок и черепаха пельи песьню, ага
head(word_df_by_freq, word_limit)  
