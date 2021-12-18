#Максимальное количество итерация для подроба оптимальных данных соответствующих уровню продаж, чтобы избежать зацикливание
mx_iter = 500000
#Вектор уровней продаж для магазинов
SALE_LEVELS <-
  sample(c(40:80), 10)   #рандомная генерация требуемых уровней продаж
#SALE_LEVELS <-  c(50, 75, 20, 10, 86, 65, 95, 64, 45, 90)
#Продукты
SALE_PRODUCTS <- c("Чай", "Гречка", "Говядина")
#Дни недели
days <- c(1:7)

#Функция создающая 10 папок для десяти магазинов
create.dir <- function(way = '', i) {
  folder <- paste0(way, "Магазин ", as.integer(i))
  
  if (file.exists(folder)) {
    cat("The folder already exists")
    
  } else {
    dir.create(folder)
    
  }
}

#Генерация поставок для 1 товара
generate.in <- function(nDays = 7,
                        min = 50,
                        max = 120) {
  return(floor(runif(nDays, min, max)))
}


#Генерация продаж
generate.out <- function(data.in, saleLevel = 50) {
  n <- 0
  data.out <- c()
  tmp.data.out <- c()
  temp.ratio <- 200
  repeat {
    #Генерируем данные
    for (i in 1:length(data.in)) {
      data.out[i] <- as.integer(runif(1, 1, data.in[i]))
    }
    
    #Текущий уровень продаж
    ratio = (sum(data.out) / sum(data.in)) * 100
    
    #Если попадает в погрешность, то выходим из цикла
    if ((ratio >= saleLevel * 0.9975) &&
        (ratio <= saleLevel * 1.0025)) {
      tmp.data.out = data.out
      temp.ratio = ratio
      break
      
      #если подобрались неудачные значения, не подходящие под уровень продаж то сначала проверяем количество итераций, чтобы изюежать зацикливания
    } else if (n == mx_iter) {
      break
    } else if (abs(ratio - saleLevel) <= abs(temp.ratio - saleLevel)) {
      tmp.data.out = data.out
      temp.ratio = ratio
      n <- n + 1
    } else {
      n <- n + 1
    }
    
  }
  #после завершения цикла по генерации товара сообщаем что успешно сгенерированы данные
  print(paste0(
    "Итерации:",
    n,
    ', уровень продаж -> ',
    temp.ratio,
    ", требуемый -> ",
    saleLevel
  ))
  return (tmp.data.out)
}


#начало основной программы

dir_shops = '/Users/vladimirmarkovich/Documents/Shops/'
#в рабочей директории создаем 10 папок с магазинами
for (i in (1:10)) {
  create.dir(way = dir_shops, i)
}
{
  #Цикл по каждому магазину
  for (i in 1:10) {
    #Первый столбец каждой таблицы поставок и продаж это дни недели, устанавливаем сразу
    in.tabl <- data.frame("День недели" = days)
    out.tabl <- data.frame("День недели" = days)
    
    #Цикл по каждому товару
    for (good in SALE_PRODUCTS) {
      #генерируем поставки по товару
      dataIn <- generate.in(nDays = 7,
                            min = 40,
                            max = 150)
      #генерируем продажи по товару
      dataOut <- generate.out(dataIn, saleLevel = SALE_LEVELS[i])
      
      #Записываем в соответствующую таблицу
      in.tabl <- data.frame(in.tabl, dataIn)
      out.tabl <- data.frame(out.tabl, dataOut)
      
      #сообщаем о завершении генерации данных по магазину для определённого товара
      print(paste(
        "Генерация данных по товару",
        good,
        "для магазина",
        i,
        "успешна"
      ))
    }
    
    #Выставляем заголовки
    colnames(in.tabl) <- c("День недели", SALE_PRODUCTS)
    colnames(out.tabl) <- c("День недели", SALE_PRODUCTS)
    
    #Записываем в файл поставок
    write.table(
      in.tabl,
      file = paste0(dir_shops, "/Магазин ", as.integer(i), '/in.txt'),
      col.names = TRUE,
      row.names = FALSE,
      sep = ' ',
      dec = ','
    )
    #записываем в файл продаж
    write.table(
      out.tabl,
      file = paste0(dir_shops, "/Магазин ", as.integer(i), '/out.txt'),
      col.names = TRUE,
      row.names = FALSE,
      sep = ' ',
      dec = ','
    )
    
    
  }
  
  print("Завершение работы генератора данных")
}

#Копирование магазинов в папку "Аналитика"
work_dir <- '/Users/vladimirmarkovich/Documents/Shops/'
for (i in 1:10) {
  from_in <- paste0(work_dir, "Магазин ", as.integer(i), "/in.txt")
  from_out <-
    paste0(work_dir, "Магазин ", as.integer(i), "/out.txt")
  to_in <-
    paste0(work_dir, "Аналитика/store", as.integer(i), "_in.txt")
  to_out <-
    paste0(work_dir, "Аналитика/store", as.integer(i), "_out.txt")
  file.copy(from_in, to_in)
  file.copy(from_out, to_out)
}
