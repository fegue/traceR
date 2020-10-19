set.seed(10)

  
  n <-  100
  x <- cumsum(sample(c(-1,1), n, TRUE))
  y <- cumsum(sample(c(-1,1), n, TRUE))
  plot(x, y, type = "l")

  
  n <-  100
  x <- cumsum(sample(rnorm(n, sd = 10, mean = 3), n, TRUE))
  y <- cumsum(sample(rnorm(n, sd = 10, mean = 0), n, TRUE))
  plot(x, y, type = "l")
  # showcase of dist
  hist(sample(rnorm(n, sd = 1, mean = 3), n, TRUE))

  
  n <- 100
  dats_raw <- data.frame(
    step = seq_len(n),
    x = cumsum(c(sample(-10:10, n, TRUE, prob = dnorm(c(-10:10), mean = 0, sd = 4)))),
    y = cumsum(c(sample(-10:10, n, TRUE, prob = dnorm(c(-10:10), mean = 0, sd = 4))))
  )

   # plot(x,y, type = "l") #, ylim = c(-100, 100)
   # points(x[1],y[1], col = "red")
  # # showcase of dist
  # hist(sample(-10:10, n, TRUE, prob = dnorm(c(-10:10), mean = 0, sd = 4)), xlim = c(-12,12))
  # # thought: inverse normal distribution
  
  ## Loess
  xy.l <- data.frame(
    x = predict(loess(x~step, data = dats_raw, span = 0.1, degree = 2)),
    y = predict(loess(y~step, data = dats_raw, span = 0.1, degree = 2))
  )
  plot(dats_raw$x, dats_raw$y, type = "l") # , ylim = c(-100,100)
  lines(xy.l$x, xy.l$y, col = "red")
  plot(xy.l, type = "l")
  
  xy.l_scaled <- data.frame(
    x = scales::rescale(xy.l$x, to = c(0,100)),
    y = xy.l$y*(100/max(xy.l$x))
  )
  plot(xy.l_scaled, type = "l") #, xlim = c(0,100), ylim = c(0,100)
  
  
  plot(scale(xy.l)*10, type = "l")
  x_scaled <- scales::rescale(xy.l$x, to = c(0,100))
  y_scaled <- xy.l$y*(max(x_scaled)/max(xy.l$x))
  plot(x_scaled, y_scaled, type = "l")
  
  
  my_scale <- function(x, min_new, max_new){
    (max_new - min_new)/(max(x) - min(x)) * (x - max(x)) + max_new
  }
  
  x_my_scale <- my_scale(xy.l$x, 0, 100)
  plot(x_my_scale)
  plot(x_scaled)
  
 
  ## Loess on new data
    new_dat = data.frame(
    step = 1:100,
    x = 1:100,
    y = 1:100
  )
    lFun.x <- loess(step~x, data = dats_raw, span = 0.5, degree = 2, model = TRUE)
    lFun.y <- loess(step~y, data = dats_raw, span = 0.5, degree = 2, model = TRUE)
    lFun <- loess(y~x, dats_raw)
    
  new_xy <- data.frame(
    x = predict(lFun.x, newdata = new_dat),
    y = predict(lFun.y, newdata = new_dat)
  )
  plot(new_xy)

  new_xy <- predict(lFun, newdata = new_dat)

  plot(new_xy)
  # Vorteil von loess, kann auf neuen datensatz vorhergesagt werden mit predict()
  
  ## Splie
  xy.s <- data.frame(
    x = spline(x)["y"],
    y = spline(y)["y"]
  )
  plot(x,y, type = "l")
  lines(xy.s, col = "red")
  # n?her an Bewegungsmuster
  
  
  
  
  # richtungs-patter bestimmen ?ber vektor-matrix
  # 1) vektoren zwischen jedem konekutiven punkt, die wahrscheinlich zusammen geh?ren
  # 2) vektoren zwichen jedem koneskutiven punkt einer flugbahn 
  # 3) vektor zischen dem ersten und letzten punkt einer flugbahn
  # 4) durchnittsvektor aller vektoren einer flugbahn
  # 5) flugbahn aufteilen (z.B. dritteln) --> drei vektoren eine rbahn
  
  # r?umliche verteilung verteilung 
  # 1) heatmap ?ber den gesmten zeitraum oder teil-zeiten
  
  
  