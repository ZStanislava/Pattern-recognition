library(ggplot2)

draw_lines = function(X, Y, k, D_min, z) { #отрисовка линий 
  if (k < 2 || k > 3) {
    return
  }
  
  if (k == 2) {
    if (z[1,1] != z[2,1]) {
      x_f = seq(from = -10, to = 15, by = 0.1)
      y_f = seq(along.with = x_f)
      
      for (i in 1:length(y_f)) { # построение перпендикуляра- Разделяющая граница-y выражаем через x
        y_f[i] = (x_f[i]*(z[2,1] - z[1,1]) - 0.5*z[2,1]*z[2,1] - 0.5*z[2,2]*z[2,2] + 0.5*z[1,1]*z[1,1] + 0.5*z[1,2]*z[1,2]) / (z[1,2]-z[2,2])
      }
    }
    else {
      x_f = seq((z[1,1] - z[2,1]) / 2, lenght.out = 20)
      y_f = seq(from = -1, to = 11, length.out = length(x_f))
    }
    
    lines(data.frame(x_f, y_f), col = 1)
    
    return
  }
  
  if (k == 3) {
    # Между первым и вторым кластерами
    if (z[1,1] != z[2,1]) {
      x_f1 = seq(from = -5, to = 8, by = 0.005)
      y_f1 = seq(along.with = x_f1)
      
      for (i in 1:length(y_f1)) {
        y_f1[i] = (x_f1[i]*(z[2,1] - z[1,1]) - 0.5*z[2,1]*z[2,1] - 0.5*z[2,2]*z[2,2] + 0.5*z[1,1]*z[1,1] + 0.5*z[1,2]*z[1,2]) / (z[1,2]-z[2,2])
      }
    }
    else {
      x_f1 = seq((z[1,1] - z[2,1]) / 2, lenght.out = 50)
      y_f1 = seq(from = -1, to = 11, length.out = length(x_f1))
    }
    
    # Между вторым и третьим кластерами
    if (z[3,1] != z[2,1]) {
      x_f2 = seq(from = -5, to = 8, by = 0.005)
      y_f2 = seq(along.with = x_f2)
      
      for (i in 1:length(y_f2)) {
        y_f2[i] = (x_f2[i]*(z[2,1] - z[3,1]) - 0.5*z[2,1]*z[2,1] - 0.5*z[2,2]*z[2,2] + 0.5*z[3,1]*z[3,1] + 0.5*z[3,2]*z[3,2]) / (z[3,2]-z[2,2])
      }
    }
    else {
      x_f2 = seq((z[3,1] - z[2,1]) / 2, lenght.out = 50)
      y_f2 = seq(from = -1, to = 25, length.out = length(x_f2))
    }
    
    # Между первым и третьим кластерами
    if (z[1,1] != z[3,1]) {
      x_f3 = seq(from = -5, to = 8, by = 0.005)
      y_f3 = seq(along.with = x_f3)
      
      for (i in 1:length(y_f3)) {
        y_f3[i] = (x_f3[i]*(z[3,1] - z[1,1]) - 0.5*z[3,1]*z[3,1] - 0.5*z[3,2]*z[3,2] + 0.5*z[1,1]*z[1,1] + 0.5*z[1,2]*z[1,2]) / (z[1,2]-z[3,2])
      }
    }
    else {
      x_f3 = seq((z[1,1] - z[3,1]) / 2, lenght.out = 50)
      y_f3 = seq(from = -1, to = 11, length.out = length(x_f3))
    }
    
    y_f1_p = 0
    y_f2_p = 0
    for (i in 1:length(y_f1)) {
      for (j in 1:length(y_f2)) {
        if (sqrt((y_f1[i] - y_f2[j])*(y_f1[i] - y_f2[j]) + (x_f1[i] - x_f2[j])*(x_f1[i] - x_f2[j])) < 0.01) {
          y_f1_p = i
          y_f2_p = j
        }
      }
    }
    
    y_f3_p = 0
    for (i in 1:length(y_f1)) {
      for (j in 1:length(y_f3)) {
        if (sqrt((y_f1[i] - y_f3[j])*(y_f1[i] - y_f3[j]) + (x_f1[i] - x_f3[j])*(x_f1[i] - x_f3[j])) < 0.01) {
          y_f3_p = j
        }
      }
    }
    
    segments(x_f1[1], y_f1[1], x_f1[y_f1_p], y_f1[y_f1_p])
    segments(x_f2[y_f2_p], y_f2[y_f2_p], x_f2[length(x_f2)], y_f2[length(y_f2)])
    segments(x_f3[1], y_f3[1], x_f3[y_f3_p], y_f3[y_f3_p])
    
    #lines(data.frame(x_f1, y_f1), col = 1)
    #lines(data.frame(x_f2, y_f2), col = 1)
    #lines(data.frame(x_f3, y_f3), col = 1)
    
    return
  }
}

step3 = function(X, Y, k, z, T, pt0) {
  D = matrix(-1, nrow = length(X), ncol = k) # по строкам номера точек, по столбцам номера кластеров
  for (i in 1:length(X)) { #номера точек
    for (j in 1:k) { #номера кластеров
      D[i, j] = sqrt((z[j,1] - X[i])*(z[j,1] - X[i]) + (z[j,2] - Y[i])*(z[j,2] - Y[i]))
    } # матрица Расстояния от точек до центров кластеров
  }
  
  print("3 шаг:")
  print("Расстояния от точек до центров кластеров (по строкам - точки, по столбцам - кластера):")
  print(D)
  
  # 1 столбец - кластер точки
  # 2 столбец - минимальное расстояние до кластера
  #сравниваем рассояние от нее до центров кластеров и выбираем минимальное
  D_min = matrix(-1, nrow = length(X), ncol = 2)
  for (i in 1:length(X)) { #номера точек
    D_min[i,2] = -1
    for (j in 1:k) {#номера кластеров
      if (D_min[i,2] == -1) {
        D_min[i,1] = j #кластер точки
        D_min[i,2] = D[i, j] #минимальное расстояние до центра кластера
      }
      else if (D_min[i,2] > D[i, j]) {
        D_min[i,1] = j
        D_min[i,2] = D[i, j]
      }
    }
  }
  print("D_min:")
  print(D_min)
  
  # Максимальное D (d_max) из всех D_min
  d_max = -1
  d_max_i = -1
  for (i in 1:length(X)) {   #номера точек
    if (d_max < D_min[i,2]) {
      d_max = D_min[i,2]
      d_max_i = i
    }
  }
  print("D:")
  print(d_max)
  
  # Если d_max > 0.5*T (например, альфа = 0.5), то х - новый центр кластера
  alpha = 0.15 # можно менять #при уменьшении альфы увеличивается число кластеров
  k_old = k #номера кластеров
    if (d_max > alpha*T) {
      c_k = factorial(k) / (factorial(2) * factorial(k - 2))
      
      sum_z = 0
      for (i in 1:(k-1)) {
        for (j in (i+1):k) {
          sum_z = sqrt((z[i,1] - z[j,1])*(z[i,1] - z[j,1]) + (z[i,2] - z[j,2])*(z[i,2] - z[j,2]))
        }
      }
      
      T = (1 / c_k) * sum_z
    
    k = k + 1
  }
  
  # Если число кластеров не изменилось, то алгоритм завершён
  if (k == k_old) {
    print ("Число кластеров не изменилось, алгоритм завершён")
    
    # Построение графика
    plot(data.frame(X, Y), pch = 16, col = D_min+1, xlab="x", ylab="y", xlim=c(-11,27), ylim=c(-1,21))
    
    # Построение сетки
    abline(v=(seq(-50,50,1)), col="lightgray", lty="dotted")
    abline(h=(seq(-50,50,1)), col="lightgray", lty="dotted")
    
    # Построение точки
    D_pt0 = seq(along.with = k) # Расстояния от точки до центров кластеров
    for (i in 1:k) {
      D_pt0[i] = sqrt((z[i,1] - pt0[1])*(z[i,1] - pt0[1]) + (z[i,2] - pt0[2])*(z[i,2] - pt0[2]))
    }
    # Нахождение, к какому кластеру точка принадлежит
    d_min_pt0 = D_pt0[1]
    d_min_pt0_i = 1
    for (i in 2:k) {
      if (d_min_pt0 > D_pt0[i]) {
        d_min_pt0 = D_pt0[i]
        d_min_pt0_i = i
      }
    }
    points(data.frame(pt0[1], pt0[2]), pch = 16, col = d_min_pt0_i+1)
    
    # Построение линий
    print("Идёт построение линий...")
    draw_lines(X, Y, k, D_min, z)
    print("Построение линий закончено. Программа завершила работу")
    
    return
  }
  
  if (k != k_old) {
    print("Добавляется новый кластер")
    z[k,1] = X[d_max_i]
    z[k,2] = Y[d_max_i]
    points(data.frame(z[k,1], z[k,2]), pch = 16, col = 4)
    
    step3(X, Y, k, z, T, pt0)
  }
}

laba2 = function(pt0_x, pt0_y) {
  # Введённая точка
  pt0 = c(pt0_x, pt0_y)
  print("Введённая точка:")
  print(pt0)
  
  # Начальные данные
  X = read.table( file = "D:/Новая папка/иатэ/3 курс/6 семестр/РО/мои лабы/2 лаба/X.txt" ) 
  Y = read.table( file = "D:/Новая папка/иатэ/3 курс/6 семестр/РО/мои лабы/2 лаба/Y.txt") 
  X = unlist(X, recursive = TRUE, use.names = FALSE) 
  Y = unlist(Y, recursive = TRUE, use.names = FALSE) 
  X = c(X); Y = c(Y);
  
  print("Введённые точки:")
  print(data.frame(X, Y))
  
  # Построение графика
  plot(data.frame(X, Y), pch = 16, col = 1, xlab="x", ylab="y")

  # Построение сетки
  abline(v=(seq(-50,50,1)), col="lightgray", lty="dotted")
  abline(h=(seq(-50,50,1)), col="lightgray", lty="dotted")
  
  # 1 шаг-центр 1 кластера выбираем произвольно
  z = matrix(-1, nrow = 10, ncol = 2)
  z[1, 1] = X[1]
  z[1, 2] = Y[1]
  k = 1 # количество кластеров
  print("1 шаг:")
  print("z1:")
  print(c(z[1,1], z[1,2]))
  points(data.frame(z[1,1], z[1,2]), pch = 16, col = 2)
  
  # 2 шаг-находим наиболее удаленный образ от z1
  T = 0 #расстояние между кластерами
  z2_i = -1 # индекс точки максимально удаленной
  for (i in 2:length(X)) {
    d = sqrt((z[1,1] - X[i])*(z[1,1] - X[i]) + (z[1,2] - Y[i])*(z[1,2] - Y[i]))
    
    if (d > T) {
      T = d
      z2_i = i
    }
  }
  z[2, 1] = X[z2_i]
  z[2, 2] = Y[z2_i]
  k = k + 1 #кол-во кластеров 
  
  print("2 шаг:")
  print("z2:")
  print(c(z[2,1], z[2,2]))
  print("T:")
  print(T)
  points(data.frame(z[2,1], z[2,2]), pch = 16, col = 3)
  
  
  step3(X, Y, k, z, T, pt0)
}

laba2(8, 1)