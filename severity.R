# Запросы Severity

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
df$type <- NULL

df_severity <- subset(df, df$message == "Severity")
df_severity$message <- NULL
str(df_severity)

df_severities <- as.data.frame(table(df_query_error$text), stringsAsFactors=FALSE)
# Illegal character  962
# Только такой текст у ошибок типа Severity.
# Другого нельзя было ожидать, так как функция CelanData на 15 строчке
# как раз все описания в сообщении типа Severity сокращает до Illegal character
df_severities

dates_to_plot <- format(df_severity$date, format="%m-%d")
d_df <- as.data.frame(table(dates_to_plot))
library("ggplot2")
p <- ggplot(d_df, aes(x=dates_to_plot, y=Freq)) + 
  geom_bar(stat="identity") +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Дата") + ylab("Количество запросов")
p
ggsave("severities-by-days.png", plot = p, path = "./img/")
