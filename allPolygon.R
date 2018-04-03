
# data frame transpose (행, 열 교환)
# rowdata50 자리에 import한 엑셀 파일 이름
tmpDf <- data.frame(rowdata50)
finalDf <- as.data.frame(t(tmpDf))
finalDf <- finalDf[-c(1),]
cnt = ncol(finalDf)
cnt2 = cnt-1  

# 누적 그래프 사용 여부 (누적 그래프 출력 시 TRUE / 개별 사진 및 동영상 출력 시 FALSE)
isAll = FALSE

# 오차 증폭값 (n배 증폭)
multi = 10

# 사진 파일 저장할 경로 및 확장자 지정
path = "/Users/jhheo/"
ext = ".jpg"
allImageName = "all" #누적 그래프 출려시에만 사용


create.func <- function(order = 0) {

  
  if(isAll == FALSE) {
    cnt = 1
    cnt2 = 0
  }
  
    
  # ggplot2 패키치 설치 필요
  library(ggplot2)
  
  # 단면별 벡터 선언
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
  
  
  #측정값 벡터, 표준값 벡터에 insert input data
  for(o in seq(1:cnt)) {
    for(i in seq(1,114,3)) {
      if((i>=1) & (i<=36)) {
        tar1_x_vector <- c(tar1_x_vector, finalDf[i,o])
        tar1_y_vector <- c(tar1_y_vector, finalDf[i+1,o])
      } else if((i>=37) & (i<=78)) {
        tar2_x_vector <- c(tar2_x_vector, finalDf[i,o])
        tar2_y_vector <- c(tar2_y_vector, finalDf[i+1,o])
      } else if((i>=79) & (i<=114)) {
        tar3_x_vector <- c(tar3_x_vector, finalDf[i,o])
        tar3_y_vector <- c(tar3_y_vector, finalDf[i+1,o])
      }
    }
  }
  
  
  # 면1, 3 오차율 증폭
  for(l in 0:cnt2) {
    for(k in seq(1:12)) {
      if((k%%12>=1 & k%%12<=3)) {
        tar1_y_vector[k+(l*12)] = tar1_y_vector[k+(l*12)] * multi
        tar3_y_vector[k+(l*12)] = tar3_y_vector[k+(l*12)] * multi 
      } else if((k%%12>=4 & k%%12<=6)) {
        tar1_x_vector[k+(l*12)] = (tar1_x_vector[k+(l*12)] - 151.63) * multi +151.63 
        tar3_x_vector[k+(l*12)] = (tar3_x_vector[k+(l*12)] - 151.63) * multi +151.63
      } else if((k%%12>=7 & k%%12<=9)) {
        tar1_y_vector[k+(l*12)] = (tar1_y_vector[k+(l*12)] -107.45) * multi + 107.45 
        tar3_y_vector[k+(l*12)] = (tar3_y_vector[k+(l*12)] -107.45) * multi + 107.45
      } else if((k%%12>=10 | k%%12==0)) {
        tar1_x_vector[k+(l*12)] = tar1_x_vector[k+(l*12)] * multi
        tar3_x_vector[k+(l*12)] = tar3_x_vector[k+(l*12)] * multi
      }
    }
  }

  
  # 면2 오차율 증폭
  for(l in 0:cnt2) {
    for(k in 1:14) {
      if((k%%14>=1 & k%%14<=4)) {
        tar2_y_vector[k+(l*14)] = tar2_y_vector[k+(l*14)] * multi
      } else if((k%%14>=5 & k%%14<=7)) {
        tar2_x_vector[k+(l*14)] = (tar2_x_vector[k+(l*14)] - 151.63) * multi +151.63
      } else if((k%%14>=8 & k%%14<=10)) {
        tar2_y_vector[k+(l*14)] = (tar2_y_vector[k+(l*14)] -107.545)  * multi + 107.545 
      } else if((k%%14>=11 | k%%14==0)) {
        tar2_x_vector[k+(l*14)] = tar2_x_vector[k+(l*14)] * multi
      }
    }
  }
  
  
  # 면 1, 2, 3 측정값 벡터에 꼭지점 4개 씩 삽입
  tmp <- 0
  for(i in 1:cnt) {
    
    tar1_x_vector <- append(tar1_x_vector, 0, after=(0 + tmp))
    tar1_y_vector <- append(tar1_y_vector, 0, after=(0 + tmp))
    tar3_x_vector <- append(tar3_x_vector, 0, after=(0 + tmp))
    tar3_y_vector <- append(tar3_y_vector, 0, after=(0 + tmp))
    
    tar1_x_vector <- append(tar1_x_vector, 151.63, after=(4 + tmp))
    tar1_y_vector <- append(tar1_y_vector, 0, after=(4 + tmp))
    tar3_x_vector <- append(tar3_x_vector, 151.63, after=(4 + tmp))
    tar3_y_vector <- append(tar3_y_vector, 0, after=(4 + tmp))
    
    tar1_x_vector <- append(tar1_x_vector, 151.63, after=(8 + tmp))
    tar3_x_vector <- append(tar3_x_vector, 151.63, after=(8 + tmp))
    tar1_y_vector <- append(tar1_y_vector, 107.545, after=(8 + tmp))
    tar3_y_vector <- append(tar3_y_vector, 107.545, after=(8 + tmp))
    
    tar1_x_vector <- append(tar1_x_vector, 0, after=(12 + tmp))
    tar3_x_vector <- append(tar3_x_vector, 0, after=(12 + tmp))
    tar1_y_vector <- append(tar1_y_vector, 107.545, after=(12 + tmp))
    tar3_y_vector <- append(tar3_y_vector, 107.545, after=(12 + tmp))
    
    tmp <- tmp + 16
  }
  
  
  tmp2 <- 0
  
  for(i in 1:cnt) {
    
    tar2_x_vector <- append(tar2_x_vector, 0, after=(0+tmp2))
    tar2_y_vector <- append(tar2_y_vector, 0, after=(0+tmp2))
    tar2_x_vector <- append(tar2_x_vector, 151.63, after=(5+tmp2))
    tar2_y_vector <- append(tar2_y_vector, 0, after=(5+tmp2))
    tar2_x_vector <- append(tar2_x_vector, 151.63, after=(9+tmp2))
    tar2_y_vector <- append(tar2_y_vector, 107.545, after=(9+tmp2))
    tar2_x_vector <- append(tar2_x_vector, 0, after=(14+tmp2))
    tar2_y_vector <- append(tar2_y_vector, 107.545, after=(14+tmp2))
    
    tmp2 <- tmp2 + 18
  }

  
  
  
  # 기준면 1, 2, 3 (오차가 0인 가상의 면) 추가
  std1_x_vector = c(0, 10, 75.8, 141.6, 151.63, 151.63, 151.63, 151.63, 151.63, 141.6, 75.8,10, 0, 0, 0, 0)
  std1_y_vector = c(0, 0, 0, 0, 0, 10, 53.75, 97.5, 107.545, 107.545, 107.545, 107.545, 107.545, 97.5, 53.75, 10)
  std2_x_vector = c(0, 10, 53.9, 97.7, 141.6, 151.63, 151.63, 151.63, 151.63, 151.63, 141.6, 97.7, 53.9, 10, 0, 0, 0, 0)
  std2_y_vector = c(0, 0, 0, 0, 0, 0, 10, 53.75, 97.5, 107.545, 107.545, 107.545, 107.545, 107.545, 107.545, 97.5, 53.75, 10)
  std3_x_vector <- std1_x_vector
  std3_y_vector <- std1_y_vector
  
  dotted_x_vector1 = c(0)
  dotted_y_vector1 = c(0)
  dotted_x_vector2 = c(0)
  dotted_y_vector2 = c(0)
  
  
  
  
  
  # 기준면, 측정면 별 zone 추가 및 평행이동
  group_a <- data.frame(x = std1_x_vector,
                        y = std1_y_vector,
                        width = 1.5,
                        zone = "s")
  group_b <- data.frame(x = tar1_x_vector,
                        y = tar1_y_vector,
                        width = 1,
                        zone = "t")
  group_c <- data.frame(x = std2_x_vector+100,
                        y = std2_y_vector+100,
                        width = 1.5,
                        zone = "s1")
  group_d <- data.frame(x = tar2_x_vector+100,
                        y = tar2_y_vector+100,
                        width = 1,
                        zone = "t1")
  group_e <- data.frame(x = std3_x_vector+200,
                        y = std3_y_vector+200,
                        width = 1.5,
                        zone = "s2")
  group_f <- data.frame(x = tar3_x_vector+200,
                        y = tar3_y_vector+200,
                        width = 1,
                        zone = "t2")
  
  
  # 면 1, 2, 3 별로 기준면, 측정면 grouping
  dat1 <- rbind(group_a, group_b)
  
  dat2 <- rbind(group_c, group_d)
  
  dat3 <- rbind(group_e, group_f)
  
  dat4 <- rbind(dat1, dat2, dat3)
  
  
  ggplot(dat4, aes(x = x, y = y, col = zone, size=factor(width))) + geom_polygon(alpha = 0) + 
    geom_point(size=0.5) + scale_size_manual(breaks = c("s", "t", "s1", "t1", "s2", "t2"), values = c(0.1,1.5,0.1,1.5,0.1,1.5)) +
    scale_color_manual(breaks = c("s", "t", "s1", "t1", "s2", "t2"), values=c("#525252", "red", "#525252", "red", "#525252", "red")) +
    geom_segment(aes(x = group_a[13,1], y = group_a[13,2], xend = group_e[13,1], yend = group_e[13,2])) + geom_segment(aes(x = group_a[9,1], y = group_a[9,2], xend = group_e[9,1], yend = group_e[9,2])) + 
    geom_segment(aes(x = group_a[5,1], y = group_a[5,2], xend = group_e[5,1], yend = group_e[5,2])) + geom_segment(x = group_a[1,1], y = group_a[1,2], xend = group_e[1,1], yend = group_e[1,2], linetype = "dotted") +
    scale_y_continuous(breaks = seq(-10, 500, by=5)) +scale_x_continuous(breaks = seq(-10, 500, by=5))
  #scale_x_continuous(breaks = seq(0, 2, by = 0.1)) + scale_y_continuous(breaks = seq(0, 2, by = 0.1))
}




if(isAll == TRUE) {
  print(create.func(i))
  fullPath <- paste0(path,(paste0(allImageName,ext))) 
  ggsave(file=fullPath)
} else {
  j <- 1
  for (i in 1:3) {
    print(create.func(i))
    fullPath <- paste0(path,(paste0(j,ext))) 
    ggsave(file=fullPath)
    j <- j + 1
  }  
}




#system("ffmpeg -framerate 1 -i /Users/jhheo/%d.jpg -vcodec libx264 -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" -b 6400k -acodec aac -strict experimental -ab 6400k -threads 0 -map_metadata -1 -y /Users/jhheo/Desktop/output-test.mp4")