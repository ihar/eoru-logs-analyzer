# Запросы Page requested
rm(list=ls(all=TRUE))
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

# Распределение количества запросов по дням
dates_to_plot <- format(wrong_pages$date, format="%m-%d")
d_df <- as.data.frame(table(dates_to_plot))
# Отображать в подписи на оси асбцисс каждый седьмой день
axes_dates <- d_df$dates_to_plot[seq(1, nrow(d_df), 7)]
library("ggplot2")
threshold <- 100
p <- ggplot(d_df, aes(x=dates_to_plot, y=Freq)) + 
  geom_bar(stat="identity") +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = axes_dates, labels=axes_dates) +
  xlab("Дата") + ylab("Количество запросов")
p2 <- p +  geom_hline(yintercept = threshold, colour = "red")
ggsave("wrong-pages-by-days.png", plot = p2, path = "./img/")
# Дни с большим количеством ошибочных запросов
d_df[d_df$Freq>threshold,]
