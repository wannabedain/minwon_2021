'''
data <- read.csv("c:/data/0730 생활인구 test.csv", header = T, encoding = "UTF-8")
head(data)



data1 <-aggregate(총생활인구수~X.U.FEFF.집계구코드,data, sum)

write.csv(data1,"c:/data/생활인구집계 test.csv")
'''
library(dplyr)
library(corrplot)

data2 <- read.csv("D:/인천시청/최최최종가중치.csv", header = T, encoding = "EUC-KR")
head(data)
colnames(data)

dataa <- read.csv("D:/인천시청/승차건수.csv", header = T, encoding = "EUC-KR")
head(dataa)

dataaz <- read.csv("D:/인천시청/하차건수.csv", header = T, encoding = "EUC-KR")
head(dataaz)


data <- left_join(data2, dataa, by="id")
head(data)

data_final <- left_join(data, dataaz, by="id")
head(data_final)
data_final$승차건수 <- data_final$NUMPOINTS


data_real <- subset(data_final, select=-c(시설가중치, 교차, 고령인구_minmax,교차_가중치, SGG_NM, SGG_NM_가중치, 최종가중치, NUMPOINTS, left, top, right, bottom))
str(data_real)

coldata <- subset(data_real, select=-c(id))

'''
coldata %>% na.omit%>%cor %>% corrplot.mixed(p.mat=p.value[[1]], sig.level=.05, lower = 'number', upper='pie', tl.cex=.6, tl.col='black', order='hclust')
'''


library(corrplot) 
mcor <- cor(coldata) 
round(mcor, 2) 
corrplot(mcor, method='shade', shade.col=NA, tl.col='black', tl.srt=45)

corrplot(mcor, method="circle")
corrplot(mcor, method="number")

cor.test(data$의료기관,data$복지시설, method = 'pearson')
cor.test(data$의료기관,data$대규모점포, method = 'pearson')
cor.test(data$복지시설,data$대규모점포, method = 'pearson')



cor(data1)

cor.test(data1)
