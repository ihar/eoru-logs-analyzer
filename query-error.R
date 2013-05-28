# Запросы Query error

data_dir <- "d:/webservers/other/eoru_logs/2012/"
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

df_query_error <- subset(df, df$message == "Query error")
df_query_error$message <- NULL
str(df_query_error)

df_queries <- as.data.frame(table(df_query_error$text), stringsAsFactors=FALSE)
df_queries
