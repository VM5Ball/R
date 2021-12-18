#начинаем с папки, где у нас лежат данные по магазинам
setwd("/Users/vladimirmarkovich/Documents/Shops/Аналитика")

#Функция подсчёта максимальной продажи в магазине
max_sale <- function(my_out) {
  mx <- 0
  for (i in 1:7) {
    sale <- sum(my_out[i, 2:4])
    if (sale > mx) {
      mx <- sale
    }
  }
  return(mx)
}

#Функция подсчёта минимальной продажи в магазине
min_sale <- function(my_out) {
  mn <- 100000
  for (i in 1:7) {
    sale <- sum(my_out[i, 2:4])
    if (sale < mn) {
      mn <- sale
    }
  }
  return(mn)
}

#Функция находящая день максимальной продажи в магазине
find_max_day <- function(mx, my_out) {
  for (i in 1:7) {
    curr_mx_sale <- sum(my_out[i, 2:4])
    if (curr_mx_sale == mx) {
      day <- i
    }
  }
  return(day)
}

#Функция находящая день минимальной продажи в магазине
find_min_day <- function(mn, my_out) {
  for (i in 1:7) {
    curr_mx_sale <- sum(my_out[i, 2:4])
    if (curr_mx_sale == mn) {
      day <- i
    }
  }
  return(day)
}

#Функция подсчёта максимального списания в магазине
max_spis <- function(my_in, my_out) {
  mx <- 0
  for (i in 1:7) {
    spis <- sum(my_in[i, 2:4]) - sum(my_out[i, 2:4])
    if (spis > mx) {
      mx <- spis
    }
  }
  return(mx)
}

#Функция нахождения дня максимального списания в магазине
find_max_day_spis <- function(mx, my_in, my_out) {
  for (i in 1:7) {
    curr_spis <- sum(my_in[i, 2:4]) - sum(my_out[i, 2:4])
    if (curr_spis == mx) {
      day <- i
    }
  }
  return(day)
}



#создаём переменные по магазинам
# in{i} <- read.table(store{i}_in.txt)
for (i in 1:10) {
  assign(paste0("in", as.character(i)),
         read.table(
           paste0("store", as.character(i), "_in.txt"),
           head = TRUE,
           encoding = "UTF-8"
         ))
  assign(paste0("out", as.character(i)),
         read.table(
           paste0("store", as.character(i), "_out.txt"),
           head = TRUE,
           encoding = "UTF-8"
         ))
}

number_of_products <- length(names(in1)) - 1 # количество продуктов
name_of_products <- names(in1)[-1] #названия продуктов

#print(number_of_products)
#print(name_of_products)

#итоговая таблица
my_tab <- c()

#Первая колонка, создержащая названия строк (магазины)
first_col <-
  c(
    'Магазин1',
    'Магазин2',
    'Магазин3',
    'Магазин4',
    'Магазин5',
    'Магазин6',
    'Магазин7',
    'Магазин8',
    'Магазин9',
    'Магазин10',
    'Итого',
    'Среднее'
  )

final_tab <- data.frame('Параметр' = first_col)
final_tab$"Выручка" <- 0
final_tab$"Прибыль" <- 0
final_tab$"Реализация" <- 0
final_tab$"Списание" <- 0
final_tab$"Равномерность продаж" <- 0
final_tab$"Продажи макс" <- 0
final_tab$"День макс" <- 0
final_tab$"Продажи мин" <- 0
final_tab$"День мин" <- 0
final_tab$"Списание макс" <- 0
final_tab$"День" <- 0

#цикл, генерирующий данные по одному товару во всех магазинах
for (i in 1:number_of_products) {
  #создаём дата фрейм (таблицу) где первая колонка - параметры, а последующие создаём вручную и изначально заполняем нулями
  res.tab <- data.frame('Параметр' = first_col)
  res.tab$"Выручка" <- 0
  res.tab$"Прибыль" <- 0
  res.tab$"Реализация" <- 0
  res.tab$"Списание" <- 0
  res.tab$"Равномерность продаж" <- 0
  res.tab$"Продажи макс" <- 0
  res.tab$"День макс" <- 0
  res.tab$"Продажи мин" <- 0
  res.tab$"День мин" <- 0
  res.tab$"Списание макс" <- 0
  res.tab$"День" <- 0
  
  #константы, необходимые для расчётов, взяты из файла сбора данных страница 2 (внизу)
  P_supply = 5500  #оптовая цена
  P_sale = 8000    #цена по которой продает магазин
  P_util = 400   #потери от списания и утилизации
  
  # Первый столбец выручки от продажи
  # Total revenue. TR = Q_sale*Price
  # Q_sale = sum(out{j}[i+1])
  tr <- c()
  for (j in 1:10) {
    tr <-
      c(tr, sum(eval(parse(
        text = paste0("out", as.character(j))
      ))[i + 1]) * P_sale)
    res.tab[j, 2] <- tr[j]
  }
  res.tab[11, 2] = sum(tr)
  res.tab[12, 2] = mean(tr)
  
  #Второй солбец прибыли
  # tr{j} = sum(out{j}[i+1])*8000
  # tc{j} = sum(in{j}[i+1])*5500 + (sum(in{j}[i+1])-sum(out{j}[i+1]))*400
  # pr{j} = tr{j}-tc{j}
  tr <- c()
  tc <- c()
  pr <- c()
  for (j in 1:10) {
    tr <-
      c(tr, sum(eval(parse(
        text = paste0("out", as.character(j))
      ))[i + 1]) * P_sale)
    tc <-
      c(tc, sum(eval(parse(
        text = paste0("in", as.character(j))
      ))[i + 1]) * P_supply + (sum(eval(
        parse(text = paste0("in", as.character(j)))
      )[i + 1]) - sum(eval(
        parse(text = paste0("out", as.character(j)))
      )[i + 1])) * P_util)
    pr <- c(pr, tr[j] - tc[j])
    res.tab[j, 3] <- pr[j]
  }
  res.tab[11, 3] <- sum(pr)
  res.tab[12, 3] <- mean(pr)
  
  #Реализация
  #real{j} <- sum(in{j}[i+1])
  real <- c()
  for (j in 1:10) {
    real <-
      c(real, sum(eval(parse(
        text = paste0("in", as.character(j))
      ))[i + 1]))
    res.tab[j, 4] <- real[j]
  }
  res.tab[11, 4] <- sum(real)
  res.tab[12, 4] <- mean(real)
  
  #Cписание
  # sp <- sum(in{j}[i+1]) - sum(out{j}[i+1])
  sp <- c()
  for (j in 1:10) {
    sp <-
      c(sp, sum(eval(parse(
        text = paste0("in", as.character(j))
      ))[i + 1]) - sum(eval(parse(
        text = paste0("out", as.character(j))
      ))[i + 1]))
    res.tab[j, 5] <- sp[j]
  }
  res.tab[11, 5] <- sum(sp)
  res.tab[12, 5] <- mean(sp)
  
  #Pазмерность продаж
  #Формула взята со страниц 11-12 файла по КР
  # razm <- sd(out{j}[1:7,i+1])
  razm = c()
  for (j in 1:10) {
    razm <-
      c(razm, sd(eval(parse(
        text = paste0("out", as.character(j))
      ))[1:7, i + 1]))
    res.tab[j, 6] <- razm[j]
  }
  res.tab[11, 6] <- sum(razm)
  res.tab[12, 6] <- mean(razm)
  
  #Продажи макс и мин
  # max(out{j}[i+1])
  # min(out{j}[i+1])
  prod_mx <- c()
  prod_min <- c()
  for (j in 1:10) {
    prod_mx <-
      c(prod_mx, max(eval(parse(
        text = paste0("out", as.character(j))
      ))[i + 1]))
    res.tab[j, 7] <- prod_mx[j]
    prod_min <-
      c(prod_min, min(eval(parse(
        text = paste0("out", as.character(j))
      ))[i + 1]))
    res.tab[j, 9] <- prod_min[j]
  }
  
  #День макс продажи и мин продажи
  #which.max(unlist(out{j}[i+1]))
  #which.min(unlist(out{j}[i+1]))
  days_prod_mx <- c()
  days_prod_min <- c()
  for (j in 1:10) {
    days_prod_mx <-
      c(days_prod_mx, which.max(unlist(eval(
        parse(text = paste0("out", as.character(j)))
      )[i + 1])))
    res.tab[j, 8] <- days_prod_mx[j]
    days_prod_min <-
      c(days_prod_min, which.min(unlist(eval(
        parse(text = paste0("out", as.character(j)))
      )[i + 1])))
    res.tab[j, 10] <- days_prod_min[j]
  }
  
  #Списание макс и день списания макс
  # Макс списание: max(in{j}[1:7,i+1]-out{j}[1:7,i+1])
  spisan_mx <- c()
  day_sp_mx <- c()
  for (j in 1:10) {
    spisan_mx <-
      c(spisan_mx, max(eval(parse(
        text = paste0("in", as.character(j))
      ))[1:7, i + 1] - eval(parse(
        text = paste0("out", as.character(j))
      ))[1:7, i + 1]))
    res.tab[j, 11] <- spisan_mx[j]
    day_sp_mx <-
      c(day_sp_mx, which.max(eval(parse(
        text = paste0("in", as.character(j))
      ))[1:7, i + 1] - eval(parse(
        text = paste0("out", as.character(j))
      ))[1:7, i + 1]))
    res.tab[j, 12] <- day_sp_mx[j]
  }
  
  #дописываем 12 столбцов очередной таблицы в итоговую
  my_tab <- c(my_tab, res.tab)
  
  
  for (j in 2:6) {
    final_tab[j] <- final_tab[j] + res.tab[j]
  }
  if (i == 1) {
    final_tab[9] <- res.tab[9]
  }
  
  for (j in 1:10) {
    final_tab[j, 7] <-
      max_sale(eval(parse(text = paste0(
        "out", as.character(j)
      ))))
    final_tab[j, 8] <-
      find_max_day(final_tab[j, 7]  , eval(parse(text = paste0(
        "out", as.character(j)
      ))))
    
    final_tab[j, 9] <-
      min_sale(eval(parse(text = paste0(
        "out", as.character(j)
      ))))
    final_tab[j, 10] <-
      find_max_day(final_tab[j, 9]  , eval(parse(text = paste0(
        "out", as.character(j)
      ))))
    
    
    final_tab[j, 11] <-
      max_spis(eval(parse(text = paste0(
        "in", as.character(j)
      ))), eval(parse(text = paste0(
        "out", as.character(j)
      ))))
    final_tab[j, 12] <-
      find_max_day_spis(final_tab[j, 11] , eval(parse(text = paste0(
        "in", as.character(j)
      ))), eval(parse(text = paste0(
        "out", as.character(j)
      ))))
  }
  
}
final_tab[6] <- final_tab[6] / number_of_products

#print(final_tab)

#переходим в папку, где будут храниться таблицы с результатами
setwd("/Users/vladimirmarkovich/Documents/Shops/Result")

#Запись таблицы по всем продуктам сразу
write.table(
  final_tab,
  file = paste0("res.tab_Вместе.csv"),
  col.names = TRUE,
  row.names = FALSE,
  sep = ";",
  dec = ",",
  fileEncoding = "utf-8"
)

#циклом длиной в количество товаров идём
for (i in 1:number_of_products) {
  #еслив итоговой таблице пусто, то выходим из цикла
  if (length(my_tab) < 1) {
    break
  }
  #записываем в соответсвующий файл .csv с именем товара данные по нему
  write.table(
    my_tab[1:12],
    file = paste0("res.tab_", as.character(name_of_products[i]), ".csv"),
    col.names = TRUE,
    row.names = FALSE,
    sep = ";",
    dec = ",",
    fileEncoding = "utf-8"
  )
  #укорачиваем таблицу на уже использованные 12 столбцов
  if (length(my_tab) > 0) {
    my_tab <- my_tab[13:length(my_tab)]
  }
}
