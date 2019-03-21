
# 第一次作业代码及结果说明:

## (数据说明：本次作业选取的数据均为4月11日、gpa>=3 的学生的座次数据)

# 结论VMR=0.7174515，学生座次呈现  随机分布


## 1.数据处理：选择4月11日的座次数据，将数据填图在10x8的表格中

```R
#读取数据
data=read.csv("E:/大学/大三下/空间数据/最新课件/实验内容/seat.csv",header=TRUE)
fdata=data[c("uid","gpa.all",date)]#选取4月11日的数据
fdata=fdata[complete.cases(fdata),]#过滤数据为NA的行

#创建SpatialPoints  生成学生座次表
seat<-fdata[3]#获取座位编号
num<-nrow(seat)#获取需要创建的座位个数
seat1<-coordinates(cbind(x=seat[(1:num),1]%%xnum+1,y=seat[(1:num),1]%/%xnum+1))#生成座位
seat2<-SpatialPoints(seat1)
seat3<-SpatialPointsDataFrame(seat2,fdata[c("gpa.all")])
seat4<-as(seat3,"SpatialPixelsDataFrame")#强制转化为SpatialPixelsDataFrame
rw.colors<-colorRampPalette(c("grey","red"))
dev.new()  
spplot(subset(seat4,gpa.all>=bestgpa),col.regions=rw.colors(17),scales = list(draw = TRUE),xlab="讲台",main="学生座次表")
```

![Image text](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E5%AD%A6%E7%94%9F%E5%BA%A7%E6%AC%A1%E8%A1%A8.png)
      
## 2.选取样方：选择样方大小为2x2 
```R
#样方法的计算，样方大小为2X2,本次实验采取手工读数，不采取包的方式
area<-c(2,1,0,1,0,0,0,1,0,1,2,0,2,0,1,2,1,2,2,1)#每个2x2方格中优秀的个数
```

## 3.计算：计算VMR=Var/Mean，得出结论
```R
mean <- mean(area)#平均值
var<-var(area)#方差
result.area<-var/mean
```
平均值 var = 0.6815789<br>
方差   mean = 0.95<br>
得到结果：VMR=Var/Mean = 0.7174515<br>


![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/%E5%AE%9E%E9%AA%8C%E4%B8%80/folder/%E8%BF%87%E7%A8%8B%E5%9B%BE.png)


由图
      
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/%E5%AE%9E%E9%AA%8C%E4%B8%80/folder/%E5%88%86%E5%B8%83%E5%9B%BE.png)

可知，介于均匀分布和随机分布之间,可以归属于:
# 随机分布
      
## 4.计算核密度
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/%E5%AE%9E%E9%AA%8C%E4%B8%80/folder/%E6%A0%B8%E5%AF%86%E5%BA%A6%E5%88%86%E6%9E%90%E8%B5%84%E6%96%991.jpg)
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/%E5%AE%9E%E9%AA%8C%E4%B8%80/folder/%E6%A0%B8%E5%AF%86%E5%BA%A6%E5%88%86%E6%9E%90%E8%B5%84%E6%96%992.jpg)

```R
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
```
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/%E5%AE%9E%E9%AA%8C%E4%B8%80/folder/%E5%AF%B9%E6%AF%94%E5%9B%BE.png)

![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/%E5%AE%9E%E9%AA%8C%E4%B8%80/folder/%E5%AF%B9%E6%AF%94%E5%9B%BE1.png)
    
选择三个不同大小的带宽，通过核密度结果分析出：

# 成绩好的同学喜欢坐在教室的中前部分

