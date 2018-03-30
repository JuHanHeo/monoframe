
create.func <- function(order = 0) {
  library(ggplot2)
  
  tdf = data.frame(tdata)
  sdf = data.frame(sdata)
  
  tar1_x_vector = c()
  tar2_x_vector = c()
  tar3_x_vector = c()
  tar1_y_vector = c()
  tar2_y_vector = c()
  tar3_y_vector = c()
  std1_x_vector = c()
  std2_x_vector = c()
  std3_x_vector = c()
  std1_y_vector = c()
  std2_y_vector = c()
  std3_y_vector = c()
  
  # 시작점 (0,0) 으로 변경 ()
  for(i in 2:ncol(tdf)) {
    #df[order, i] plus minus x값
    #df[order, i+1] plus minus y값
  }
  
  #측정값 벡터, 표준값 벡터에 insert input data
  for(i in 2:ncol(tdf)) {
    if((i>= 2) & (i<=17)){
      tar1_x_vector <- c(tar1_x_vector, tdf[order,i])
      tar1_y_vector <- c(tar1_y_vector, tdf[order+1,i])
      std1_x_vector <- c(std1_x_vector, sdf[1,i])
      std1_y_vector <- c(std1_y_vector, sdf[2,i])
    } else if((i>= 18) & (i<=35)){
      tar2_x_vector <- c(tar2_x_vector, tdf[order,i])
      tar2_y_vector <- c(tar2_y_vector, tdf[order+1,i])
      std2_x_vector <- c(std2_x_vector, sdf[1,i])
      std2_y_vector <- c(std2_y_vector, sdf[2,i])
    } else if((i>= 36) & (i<=51)) {
      tar3_x_vector <- c(tar3_x_vector, tdf[order,i])
      tar3_y_vector <- c(tar3_y_vector, tdf[order+1,i])
      std3_x_vector <- c(std3_x_vector, sdf[1,i])
      std3_y_vector <- c(std3_y_vector, sdf[2,i])
    }
  }
  
  
  
  x_set = c(0, 0.5, 1.0, 1.5, 2, 2, 2, 2, 2, 1.6, 1.2, 0.8, 0.4, 0, 0, 0, 0)
  y_set = c(0, 0, 0, 0, 0, 0.5, 1, 1.5, 2, 2, 2, 2, 2, 2, 1.5, 1, 0.5)
  tx_set = c(0, 0.4, 0.8, 1.2, 1.6, 2, 2, 2, 2, 2, 1.6, 1.2, 0.8, 0.4, 0, 0, 0, 0)
  ty_set = c(0, 0, -0.05, -0.04, 0, 0, 0.5, 1, 1.5, 2, 2, 1.90, 1.95, 2, 2, 1.5, 1, 0.5)
  
  ty_set2 = c(0, 0, -0.05, -0.04, 0, 0, 0.5, 1, 1.5, 2, 2, 2.15, 2.1, 2, 2, 1.5, 1, 0.5)
  group_a <- data.frame(x = std1_x_vector,
                        y = std1_y_vector,
                        zone = "s")
  group_b <- data.frame(x = tar1_x_vector,
                        y = tar1_y_vector,
                        zone = "t")
  group_c <- data.frame(x = std2_x_vector,
                        y = std2_y_vector,
                        zone = "s1")
  
  group_d <- data.frame(x = tar2_x_vector,
                        y = tar2_y_vector,
                        zone = "t1")
  group_e <- data.frame(x = std3_x_vector,
                        y = std3_y_vector,
                        zone = "s2")
  
  group_f <- data.frame(x = tar3_x_vector,
                        y = tar3_y_vector,
                        zone = "t2")
  
  dat1 <- rbind(group_a, group_b)
  
  dat2 <- rbind(group_c, group_d)
  
  dat3 <- rbind(group_e, group_f)
  
  dat4 <- rbind(dat1, dat2, dat3)
  
  d=data.frame(a=c("a","b","c","d","e","f","g","h", "i", "j", "k", "l"))

  
  ggplot(dat4, aes(x = x, y = y, col = zone)) + geom_polygon(alpha = 0) + 
    geom_point(size=1) +
    scale_color_manual(breaks = c("s", "t", "s1", "t1", "s2", "t2"), values=c("red", "black", "red", "black", "red", "black")) +
    geom_segment(aes(x = group_a[13,1], y = group_a[13,2], xend = group_e[13,1], yend = group_e[13,2])) + geom_segment(aes(x = group_a[9,1], y = group_a[9,2], xend = group_e[9,1], yend = group_e[9,2])) + 
    geom_segment(aes(x = group_a[5,1], y = group_a[5,2], xend = group_e[5,1], yend = group_e[5,2])) + geom_segment(x = group_a[1,1], y = group_a[1,2], xend = group_e[1,1], yend = group_e[1,2], linetype = "dotted") +
    scale_x_continuous(breaks = seq(0, 5, by = 0.1)) + scale_y_continuous(breaks = seq(0, 7, by = 0.1))
  
  
    

}

path <- "/Users/jhheo/"

ext <- ".png"
j <- 1
for (i in seq(1, 20, 2)) {
  
  print(create.func(i))
  fullPath <- paste0(path,(paste0(j,ext))) 
  ggsave(file=fullPath)
  j <- j + 1
}
  
  

