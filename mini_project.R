#정답율의 비율을 처리한다.
library(dplyr)
getwd()
setwd("C:/mini_project")
exam <- read.csv("data/1_수학.csv")
head(exam)

# answer_percent의 %제거, 숫자로 변환
exam$answer_percent=as.numeric(gsub("%","",exam$answer_percent))
head(exam)

a = exam %>% 
  filter(answer_percent >= 90) %>% 
  tally()*100/exam$problem_num 
b = exam %>% 
  filter(answer_percent >= 80 & answer_percent < 90) %>% 
  tally()*100/exam$problem_num
c = exam %>% 
  filter(answer_percent >= 70 & answer_percent < 80) %>% 
  tally()*100/exam$problem_num
d = exam %>% 
  filter(answer_percent >= 60 & answer_percent < 70) %>% 
  tally()*100/exam$problem_num
e = exam %>% 
  filter(answer_percent >= 50 & answer_percent < 60) %>% 
  tally()*100/exam$problem_num
f = exam %>% 
  filter(answer_percent >= 40 & answer_percent < 50) %>% 
  tally()*100/exam$problem_num
g = exam %>% 
  filter(answer_percent >= 30 & answer_percent < 40) %>% 
  tally()*100/exam$problem_num
h = exam %>% 
  filter(answer_percent >= 20 & answer_percent < 30) %>% 
  tally()*100/exam$problem_num
i = exam %>% 
  filter(answer_percent >= 10 & answer_percent < 20) %>% 
  tally()*100/exam$problem_num
j = exam %>% 
  filter(answer_percent >= 0 & answer_percent < 10) %>% 
  tally()*100/exam$problem_num


tab <- data.frame(tag = c('90~100','80~90','70~80',
                          '60~70','50~60','40~50',
                          '30~40','20~30','10~20',
                          '0~10'),  
                    percent = rbind(a,b,c,d,e,f,g,h,i,j))
names(tab) <- c("tag", "percent")
View(tab)

library(ggplot2)
tab
baekjoon <- ggplot(tab, aes(x=tag,y=percent,group=1))+geom_point(color="steelblue",stroke=1)+
  geom_line(color="steelblue")+geom_label(aes(label=percent), nudge_y=1)+
  labs(x="정답율(%)", y="문제 비율(%)", title="수학")
baekjoon


