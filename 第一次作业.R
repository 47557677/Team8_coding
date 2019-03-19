library(sp)
#定义变量
xnum<-10#每行有多少个座位
bestgpa<-3#定义多少gpa为优秀学生
date<-"X4月11日"#定义截取的日期

#读取数据
data=read.csv("E:/大学/大三下/空间数据/最新课件/实验内容/seat.csv",header=TRUE)#读取数据
fdata=data[c("uid","gpa.all",date)]#选取4月9日的数据
fdata=fdata[complete.cases(fdata),]#过滤数据为NA的行

#创建SpatialPoints
seat<-fdata[3]#获取座位编号
num<-nrow(seat)#获取需要创建的座位个数
seat1<-coordinates(cbind(x=seat[(1:num),1]%%xnum+1,y=seat[(1:num),1]%/%xnum+1))#生成座位
seat2<-SpatialPoints(seat1)
seat3<-SpatialPointsDataFrame(seat2,fdata[c("gpa.all")])
seat4<-as(seat3,"SpatialPixelsDataFrame")#强制转化为SpatialPixelsDataFrame
rw.colors<-colorRampPalette(c("grey","red"))
dev.new()
spplot(subset(seat4,gpa.all>=bestgpa),col.regions=rw.colors(17),scales = list(draw = TRUE),xlab="讲台",main="学生座次表")

#样方法的计算，样方大小为2X2,本次实验采取手工读数，不采取包的方式
area<-c(2,1,0,1,0,0,0,1,0,1,2,0,2,0,1,2,1,2,2,1)#每个2x2方格中优秀的个数
mean <- mean(area)#平均值
var<-var(area)#方差
result.area<-var/mean


#核密度方法

cd<-merge(1:10,1:8)
cd<-cbind(cd,v=0)#添加一行存储该座位上是不是好学生，初始为0，不是好学生

seat5<-cbind(x=seat[(1:num),1]%%xnum+1,y=seat[(1:num),1]%/%xnum+1)
seat6<-cbind(seat5,fdata[2])#生成数据框
for(i in 1:num){
  #print(seat6[i,])
  for(p in 1:80){
    if(seat6[i,][,1]==cd[p,][,1]&seat6[i,][,2]==cd[p,][,2]&seat6[i,][,3]>=bestgpa)
    {
      cd[p,][,3]=1
    }
  }
}

cd<-cbind(cd,n=0)#存储带宽为一
cd<-cbind(cd,n1=0)#存储带宽为二
cd<-cbind(cd,n2=0)#存储带宽为三

######以下代码需执行三次，并修改dk

dk<-3#定义带宽
numm<-3
#进行核密度算法，获取指定带宽周围有多少个好学生
for(p in 1:80){
  x=cd[p,][,1]
  y=cd[p,][,2]
  v=cd[p,][,numm]
  for(i in 1:80){
    if(x==cd[i,][,1]&y+dk==cd[i,][,2]){
      v=v+cd[i,][,numm]
    }
    if(x-dk==cd[i,][,1]&y==cd[i,][,2]){
      v=v+cd[i,][,numm]
    }
    if(x+dk==cd[i,][,1]&y-dk==cd[i,][,2]){
      v=v+cd[i,][,numm]
    }
    if(x+dk==cd[i,][,1]&y==cd[i,][,2]){
      v=v+cd[i,][,3]
    }
    if(x==cd[i,][,1]&y-dk==cd[i,][,2]){
      v=v+cd[i,][,numm]
    }
    if(x-dk==cd[i,][,1]&y+dk==cd[i,][,2]){
      v=v+cd[i,][,numm]
    }
    if(x+dk==cd[i,][,1]&y+dk==cd[i,][,2]){
      v=v+cd[i,][,numm]
    }
    if(x-dk==cd[i,][,1]&y-dk==cd[i,][,2]){
      v=v+cd[i,][,numm]
    }
    cd[p,][,6]=v
  }
}
#将cd转化为空间点或网格并展示
cds<-coordinates(cd[1:2])
sp<-SpatialPointsDataFrame(cds,cd[6]/49)
sp1<-as(sp,"SpatialPixelsDataFrame")#强制转化为SpatialPixelsDataFrame
rw.colors<-colorRampPalette(c("grey","red"))
title<-paste("核密度分析结果：带宽为",dk)
dev.new()
spplot(sp1,col.regions=rw.colors(17),scales = list(draw = TRUE),xlab='讲台',main=title)#绘制核密度图形
