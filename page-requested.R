# Запросы Page requested

data_dir <- "d:/webservers/other/eoru_logs/2012/"
# data_dir <- "./data/"
data_files <- list.files(data_dir, "*.txt")

# Вспомогательные функции
source("functions.r")

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

wrong_pages <- subset(df, df$message == "Page requested")
wrong_pages$message <- NULL

# Запросы несуществующих страниц, всего за год
nrow(wrong_pages)
# 4939

df_wrong_pages <- as.data.frame(table(wrong_pages$text), stringsAsFactors = FALSE)
names(df_wrong_pages) <- c("Page", "Freq")
# Уникальных запросов несуществующих страниц
nrow(df_wrong_pages)
# 1347

# Страницы по убыванию частоты запроса
df_wrong_pages_by_freq <- df_wrong_pages[order(-df_wrong_pages$Freq),]
page_limit <- 25
head(df_wrong_pages_by_freq, page_limit)

# Надо сохранить полный список несуществующих страниц в отдельный файл.
# Для удобства первый столбец будет - количество запросов страницы, 
# второй - адрес страницы
write.table(data.frame(cbind(Freq = df_wrong_pages_by_freq$Freq, 
                             Page = df_wrong_pages_by_freq$Page)), 
            file = "wrong-pages-by-frequency.csv", 
            sep = ";",
            quote = FALSE,
            row.names = FALSE)

