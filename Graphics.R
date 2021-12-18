#начинаем с папки, где у нас лежат данные по магазинам
setwd("/Users/vladimirmarkovich/Documents/Shops/Аналитика")

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

#константы, необходимые для расчётов, взяты из файла сбора данных страница 2 (внизу)
P_supply = 5500  #оптовая цена
P_sale = 8000    #цена по которой продает магазин
P_util = 400   #потери от списания и утилизации
PRODUCTS <- c("Чай", "Гречка", "Говядина")
dir_shops = '/Users/vladimirmarkovich/Documents/Shops/Графика/'
#Функция создающая 10 папок для десяти магазинов
create.dir <- function(way = '', i) {
  folder <- paste0(way, "Магазин ", as.integer(i))
  
  if (file.exists(folder)) {
    cat("The folder already exists")
    
  } else {
    dir.create(folder)
    
  }
}
for (i in (1:10)) {
  create.dir(way = dir_shops, i)
}


number_of_products <- length(names(in1)) - 1 # количество продуктов
name_of_products <- names(in1)[-1] #названия продуктов


#--------------------Графики с одним параметром для задания 1----------------------

#Объём продаж по каждому товару в магазине
for (i in 1:10) {
  for (prod in PRODUCTS) {
    index <- which(PRODUCTS == prod)
    x  <-
      c(eval(parse(text = paste0(
        "out", as.character(i)
      )))[1:7, index + 1])
    #print(x)
    png(
      filename = paste0(
        "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
        as.character(i),
        "/Объём_продаж_",
        prod,
        ".png"
      ),
      width = 600,
      height = 450
    )
    plot(
      x,
      col = "Blue",
      xlab = "Дни",
      ylab = "Объём продаж",
      type = 'o',
      main = paste0('Объём продаж в Магазине ', as.character(i), " по товару ", prod)
    )
    dev.off()
  }
}


#Выручка по каждому товару в магазине
for (i in 1:10) {
  for (prod in PRODUCTS) {
    index <- which(PRODUCTS == prod)
    x  <-
      P_sale * eval(parse(text = paste0("out", as.character(i))))[1:7, index +
                                                                    1]
    #print(x)
    png(
      filename = paste0(
        "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
        as.character(i),
        "/Выручка_",
        prod,
        ".png"
      ),
      width = 600,
      height = 450
    )
    plot(
      x,
      col = "Blue",
      xlab = "Дни",
      ylab = "Выручка",
      type = 'o',
      main = paste0(
        'Выручка от продаж Магазине ',
        as.character(i),
        " по товару ",
        prod
      )
    )
    dev.off()
  }
}

#Прибыль по каждому товару в магазине
for (i in 1:10) {
  for (prod in PRODUCTS) {
    index <- which(PRODUCTS == prod)
    tr <-
      eval(parse(text = paste0("out", as.character(i))))[1:7, index + 1] * P_sale
    tc <-
      eval(parse(text = paste0("in", as.character(i))))[1:7, index + 1] * P_supply + eval(parse(text = paste0("in", as.character(i))))[1:7, index +
                                                                                                                                         1] - eval(parse(text = paste0("out", as.character(i))))[1:7, index + 1] *
      P_util
    pr <- tr - tc
    x  <- pr
    #print(x)
    png(
      filename = paste0(
        "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
        as.character(i),
        "/Прибыль_",
        prod,
        ".png"
      ),
      width = 600,
      height = 450
    )
    plot(
      x,
      col = "Blue",
      xlab = "Дни",
      ylab = "Прибыль",
      type = 'o',
      main = paste0(
        'Прибыль от продаж Магазине ',
        as.character(i),
        " по товару ",
        prod
      )
    )
    dev.off()
  }
}



#Списание по каждому товару в магазине
for (i in 1:10) {
  for (prod in PRODUCTS) {
    index <- which(PRODUCTS == prod)
    x  <-
      eval(parse(text = paste0("in", as.character(i))))[1:7, index + 1] - eval(parse(text = paste0("out", as.character(i))))[1:7, index +
                                                                                                                               1]
    #print(x)
    png(
      filename = paste0(
        "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
        as.character(i),
        "/Списание_",
        prod,
        ".png"
      ),
      width = 600,
      height = 450
    )
    plot(
      x,
      col = "Blue",
      xlab = "Дни",
      ylab = "Списание",
      type = 'o',
      main = paste0('Списания в Магазине ', as.character(i), " по товару ", prod)
    )
    dev.off()
  }
}

#Рентабельность по каждому товару в магазине
for (i in 1:10) {
  for (prod in PRODUCTS) {
    index <- which(PRODUCTS == prod)
    x  <-
      (c(eval(parse(
        text = paste0("out", as.character(i))
      ))[1:7, index + 1]) * P_sale - (c(eval(
        parse(text = paste0("in", as.character(i)))
      )[1:7, index + 1]) * P_supply)) / (c(eval(parse(
        text = paste0("out", as.character(i))
      ))[1:7, index + 1]) * P_sale) * 100
    #print(x)
    png(
      filename = paste0(
        "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
        as.character(i),
        "/Рентабельность",
        prod,
        ".png"
      ),
      width = 600,
      height = 450
    )
    plot(
      x,
      col = "Blue",
      xlab = "Дни",
      ylab = "Рентабельность",
      type = 'o',
      main = paste0(
        'Рентабельность в Магазине ',
        as.character(i),
        " по товару ",
        prod
      )
    )
    dev.off()
  }
}

#--------------------СравнительныеГрафики с тремя параметромами----------------------
tab_colors <- c("black", "red", "blue")

#Посторение графика в каждом магазине по трём продуктам (сравнение) по объёму продаж
for (i in 1:10) {
  data <- eval(parse(text = paste0("out", as.character(i))))
  png(
    filename = paste0(
      "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
      as.character(i),
      "/Общий_Объём_Продаж.png"
    ),
    width = 600,
    height = 450
  )
  xrange <- range(seq(1, 7))
  yrange <- range(seq(1, max(data)))
  graph <- plot(
    xrange,
    yrange,
    main = paste0('Общий объём продаж магазине ', as.character(i), " по товарам"),
    xlab = "День недели",
    ylab = "Продажи, шт",
    type = "n",
  )
  for (j in 2:4) {
    points(seq(1, 7), eval(parse(text = paste0(
      "out", as.character(i)
    )))[1:7, j], pch = 1, col = tab_colors[j - 1])
    lines(seq(1, 7), eval(parse(text = paste0(
      "out", as.character(i)
    )))[1:7, j], pch = 1, col = tab_colors[j - 1])
  }
  legend("topleft",
         legend = PRODUCTS,
         col = tab_colors,
         pch = 1)
  dev.off()
}



#Посторение графика в каждом магазине по трём продуктам (сравнение) по прибыли
for (i in 1:10) {
  data <- c()
  df1 <- eval(parse(text = paste0("out", as.character(i)))) * P_sale
  df2 <-
    eval(parse(text = paste0("in", as.character(i)))) * P_supply + eval(parse(text = paste0("in", as.character(i)))) - eval(parse(text = paste0("out", as.character(i)))) *
    P_util
  data <- df1 - df2
  png(
    filename = paste0(
      "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
      as.character(i),
      "/Общая_Прибыль.png"
    ),
    width = 600,
    height = 450
  )
  xrange <- range(seq(1, 7))
  yrange <- range(seq(min(data), max(data)))
  graph <- plot(
    xrange,
    yrange,
    main = paste0('Прибыль от продаж магазине ', as.character(i), " по товарам"),
    xlab = "День недели",
    ylab = "Прибыль, руб.",
    type = "n",
  )
  for (j in 2:4) {
    points(seq(1, 7), data[1:7, j], pch = 1, col = tab_colors[j - 1])
    lines(seq(1, 7), data[1:7, j], pch = 1, col = tab_colors[j - 1])
  }
  legend("topleft",
         legend = PRODUCTS,
         col = tab_colors,
         pch = 1)
  dev.off()
}

#Посторение графика в каждом магазине по трём продуктам (сравнение) по списанию
for (i in 1:10) {
  data <-
    eval(parse(text = paste0("in", as.character(i)))) - eval(parse(text = paste0("out", as.character(i))))
  png(
    filename = paste0(
      "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
      as.character(i),
      "/Общее_Списание.png"
    ),
    width = 600,
    height = 450
  )
  xrange <- range(seq(1, 7))
  yrange <- range(seq(min(data), max(data)))
  graph <- plot(
    xrange,
    yrange,
    main = paste0('Списание в магазине ', as.character(i), " по товарам"),
    xlab = "День недели",
    ylab = "Списание, руб.",
    type = "n",
  )
  for (j in 2:4) {
    points(seq(1, 7), data[1:7, j], pch = 1, col = tab_colors[j - 1])
    lines(seq(1, 7), data[1:7, j], pch = 1, col = tab_colors[j - 1])
  }
  legend("topleft",
         legend = PRODUCTS,
         col = tab_colors,
         pch = 1)
  dev.off()
}


#Посторение графика в каждом магазине по трём продуктам (сравнение) по рентабельности
for (i in 1:10) {
  data <- c()
  data <-
    ((eval(parse(
      text = paste0("out", as.character(i))
    )) * P_sale - eval(parse(
      text = paste0("in", as.character(i))
    )) * P_supply) / (eval(parse(
      text = paste0("out", as.character(i))
    )) * P_sale)) * 100
  png(
    filename = paste0(
      "/Users/vladimirmarkovich/Documents/Shops/Графика/Магазин ",
      as.character(i),
      "/Общая_Рентабельность.png"
    ),
    width = 600,
    height = 450
  )
  xrange <- range(seq(1, 7))
  yrange <- range(seq(min(data), max(data)))
  graph <- plot(
    xrange,
    yrange,
    main = paste0('Рентабельность в магазине ', as.character(i), " по товарам"),
    xlab = "День недели",
    ylab = "Рентабельность, %",
    type = "n",
  )
  for (j in 2:4) {
    points(seq(1, 7), data[1:7, j], pch = 1, col = tab_colors[j - 1])
    lines(seq(1, 7), data[1:7, j], pch = 1, col = tab_colors[j - 1])
  }
  legend("topleft",
         legend = PRODUCTS,
         col = tab_colors,
         pch = 1)
  dev.off()
}

#--------------------Общие графики по 10 магазинам----------------------
rev <- rep(0, 10)
tablica <- data.frame('магазин' = rev, 'объём_продаж' = rev)
for (j in (1:10)) {
  tablica[j, 1] <- as.character(j)
}
colorr <-
  c("black",
    "red",
    "yellow",
    "blue",
    "grey",
    "pink",
    "brown",
    "purple",
    "orange",
    "green")
for (product in 2:4) {
  for (shop in 1:10) {
    tablica[shop, 2] <-
      sum(eval(parse(text = paste0(
        "out", as.character(shop)
      )))[, product])
  }
  png(
    file = paste0(
      "/Users/vladimirmarkovich/Documents/Shops/Графика/Объём продаж ",
      PRODUCTS[product - 1],
      ".png"
    ),
    width = 650,
    height = 500
  )
  barplot(
    tablica$"объём_продаж",
    names = seq(1, 10),
    main = paste0('Объём продаж товара ', PRODUCTS[product - 1], " по магазинам"),
    col = colorr,
    xlab = "№ магазина",
    ylab = "Объём продаж, шт"
  )
  dev.off()
}

#install.packages("ggplot2")
library("ggplot2")

# create a dataset
shop <-
  c(
    rep("1" , 3) ,
    rep("2" , 3) ,
    rep("3" , 3) ,
    rep("4" , 3),
    rep("5" , 3),
    rep("6" , 3),
    rep("7" , 3),
    rep("8" , 3),
    rep("9" , 3),
    rep("10" , 3)
  )
prods <- rep(PRODUCTS , 10)
value <- c()
for (i in 1:10) {
  value <-
    c(value, sum(eval(parse(
      text = paste0("out", as.character(i))
    ))[, 2]))
  value <-
    c(value, sum(eval(parse(
      text = paste0("out", as.character(i))
    ))[, 3]))
  value <-
    c(value, sum(eval(parse(
      text = paste0("out", as.character(i))
    ))[, 4]))
}

#print(value)
data <- data.frame(shop, prods, value)
#print(data)

# Stacked
png(
  file = paste0(
    "/Users/vladimirmarkovich/Documents/Shops/Графика/Объём_продаж_Вместе.png"
  ),
  width = 650,
  height = 500
)
ggplot(data, aes(fill = prods, y = value, x = shop)) + geom_bar(position =
                                                                  "stack", stat = "identity")
dev.off()