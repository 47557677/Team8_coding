library(sp)
library(lctools)
library(maptools)
library(spdep)
#定义变量
xnum<-10#每行有多少个座位
date<-"X4月11日"#定义截取的日期

#读取数据
data=read.csv("E:/大学/大三下/空间数据/最新课件/实验内容/seat.csv",header=TRUE)#读取数据
fdata=data[c("uid","gpa.all",date)]#选取4月11日的数据
fdata=fdata[complete.cases(fdata),]#过滤数据为NA的行

#创建SpatialPoints
seat<-fdata[3]#获取座位编号
num<-nrow(seat)#获取需要创建的座位个数
seat1<-coordinates(cbind(x=seat[(1:num),1]%%xnum+1,y=seat[(1:num),1]%/%xnum+1))#生成座位
seat2<-SpatialPoints(seat1)


seat100<-cbind(x=seat[(1:num),1]%%xnum+1,y=seat[(1:num),1]%/%xnum+1)
nbk1 <- knn2nb(knearneigh(seat2))
all <- max(unlist(nbdists(nbk1,seat2)))   #获取最大值
all.0.all<- dnearneigh(seat2,0,all)     #找到最匹配的点
snbk1 <- make.sym.nb(all.0.all)
dev.new()
plot(nb2listw(snbk1), seat100)
moran.test(fdata[,2], nb2listw(snbk1))
