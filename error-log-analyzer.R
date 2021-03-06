# На сайте http://eoru.ru ведутся логи ошибочных запросов пользователей.
# Ошибки случаются при неверном запросе слова (фразы) или при попытке взломать сайт.
# Все подобные обращения к сайту сохраняются в текстовый файл, по одному файлу на день.
# 
# Анализ логов поможет узнать, как можно улучшить систему поиска на сайте.
rm(list=ls(all=TRUE))
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

df <- CleanData(df)

# Все записи имеют тип ERROR, можно эту колонку дальше не тянуть
unique(df$type)
df$type <- NULL

table(df$message)

#Все типы ошибок на одном графике
library("ggplot2")
df2 <- data.frame(date = format(df$date, format="%m-%d"), message = df$message)
p <- ggplot(df2, aes(date)) + 
      geom_bar() + 
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())  +
      facet_wrap(~message)
p
ggsave("by-error-type.png", plot = p, path = "./img/")

unique_dates <- unique(df2$date)
axes_dates <- unique_dates[seq(1, length(unique_dates), 7)]
p <- ggplot(df2, aes(date)) +
      geom_freqpoly(aes(group=message, colour=message, )) +
      theme_bw() +  
      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_discrete(breaks = axes_dates, labels=axes_dates) +
      xlab("Дата") + ylab("Количество ошибочных запросов")
p
ggsave("by-error-type-frequency.png", plot = p, path = "./img/")

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

# Целое равно сумме непересекающихся частей?
nrow(df) == nrow(df_wrong_page) + nrow(df_wrong_query) +
            nrow(df_wrong_symbols) + nrow(df_wrong_word)

# Частоты встречаемости каждого слова
word_table <- table(df_wrong_word$text)

# Общее количество неверных запросов
wrong_req_sum  <- sum(word_table) 
# 27568

# Сводная информация по датам в лог-файлах
date_summary <- summary(df$date)
date_summary
# Min.                    1st Qu.                Median                  Mean 
# "2012-01-01 01:12:04" "2012-04-13 10:53:23" "2012-07-18 08:24:10" "2012-07-17 10:05:42" 
# 3rd Qu.                  Max.                  NA's 
# "2012-10-26 12:14:59" "2012-12-31 22:48:19"     "1" 
# Видно, что есть одна плохая дата

min_date <- date_summary["Min."]
max_date <- date_summary["Max."]
days_interval <- as.numeric(difftime(max_date, min_date, units="d"))
weeks_interval <- as.numeric(difftime(max_date, min_date, units="w"))
hours_interval <- as.numeric(difftime(max_date, min_date, units="h"))

# Среднее количество запросов отсутствующих слов, в день, неделю, в час
wrong_req_sum / c(days_interval, weeks_interval, hours_interval)
# в день - 75.34295, в неделю - 527.40068, в час - 3.13929

# Среднее количество плохих запросов всех 4 категорий вместе
# (в этом случае плохой запрос = одной записи в лог-файле)
nrow(df) / c(days_interval, weeks_interval, hours_interval)
# 91.653414 641.573896   3.818892

# Так что там с плохой датой?
df[is.na(df$date),]
#       date  message              text
# 35234 <NA> Severity Illegal character
# В датафрейме df это строчка номер...
na_row_num <- which(rownames(df)=="35234")
# Локализуем, где в оригинальных файлах это записано
df[na_row_num-1,]
df[na_row_num+1,]
# Такая запись:
#  ERROR - 2ERROR - 2012-12-11 21:19:54 --> Severity: Notice  --> iconv() ....
# Ошибка на сервере привела к сбою записи в лог-файл
df <- df[-na_row_num,]

# Распределение количества запросов по дням
dates_to_plot <- format(df$date, format="%m-%d")
d_df <- as.data.frame(table(dates_to_plot))
axes_dates <- d_df$dates_to_plot[seq(1, nrow(d_df), 7)]

threshold  <-  250 # дни с количеством запросов, больше данного
p <- ggplot(d_df, aes(x=dates_to_plot, y=Freq)) + 
  geom_bar(stat="identity") +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = axes_dates, labels=axes_dates) +
  xlab("Дата") + ylab("Количество ошибочных запросов")

p2 <- p + geom_hline(yintercept = threshold, colour = "red")
p2
ggsave("requests-by-days.png", plot = p2, path = "./img/")
# Дни с большим количеством ошибочных запросов
d_df[d_df$Freq>threshold,]
# 01-20  356
# 04-02  344
# 04-11  265
# 04-26  254
# 12-20  336

# Слова по убыванию частоты запроса
wrong_words_decreasing <- sort(word_table, decreasing=TRUE)
word_limit  <-  10
# как  львЁнок и черепаха пельи песьню, ага
head(wrong_words_decreasing, word_limit)  
tail(wrong_words_decreasing, word_limit)

