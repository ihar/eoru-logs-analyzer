SplitTextLine <- function(txt) {
  # Разбирает строчку из лог-файла на части
  #   
  # Args:
  #  txt:  строка текста из лог-файла
  #   
  # Returns:
  #  Датафрейм с полями type, date, message, text.
  #  Поля сохраняются без преобразований, все типа character  
  
  # Отделить тип сообщения с датой и временем от фактического текста ошибки
  main_parts <- unlist(strsplit(txt, " --> ", fixed = TRUE))
  if (length(main_parts)<2) return(data.frame())
  # type, date, time
  first_part <- unlist(strsplit(main_parts[1], " ", fixed = TRUE))
  txt_type <- first_part[1]
  txt_date <- paste(first_part[3], first_part[4])
  # message, text
  second_part <- unlist(strsplit(main_parts[2], ": ", fixed = TRUE))
  txt_message <- second_part[1]
  txt_text <- second_part[2]
  # Detected an illegal character in input string
  if("Severity" == txt_message) txt_text <- "Illegal character"
  df <- data.frame(txt_type, 
                   txt_date, 
                   txt_message, 
                   txt_text, 
                   stringsAsFactors = FALSE)
  names(df) <- c("type", "date", "message", "text")
  return(df)
}

ProcessFile <- function(fname) {
  # Сбор данных из одного лог-файла
  #   
  # Args:
  #  fname: Полный путь к файлу
  #  
  # Returns:
  #  Датафрейм с полями type, date, time, message, text. 
  #  Количество строчек соответствует количеству валидных сообщений в лог-файле
  
  file_lines <- readLines(fname)  
  data_frames_list <- sapply(file_lines, SplitTextLine, USE.NAMES=FALSE)
  return(do.call(rbind, data_frames_list))
}

CleanData <- function(df) {
  # Форматирование полей, удаление строчек с пустыми значениями
  # 
  # Args:
  #   df: Датафрейм со значениями  type, date, time, message, text
  # 
  # Returns:
  #   Датафрейм с форматированными датой и временем, без пустых полей,
  #   без одинарных кавычек в поле text 
  df <- na.omit(df)
  df$date <- strptime(df$date, "%Y-%m-%d %H:%M:%S")
  df$text <- gsub("'", "", df$text)
  return(df)
}

