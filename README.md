
# 第一次作业代码及结果说明:


# 结论VMR=0.7174515，学生座次在4月11日呈现  随机分布


## 1.数据处理：选择4月11日的座次数据，将数据（gpa>=3）填图在10x8的表格中

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

## 3.计算：计算VMR=Var/Mean
      
      平均值 var = 0.6815789
      方差   mean = 0.95
      得到结果：VMR=Var/Mean = 0.7174515
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E8%BF%87%E7%A8%8B%E5%9B%BE.png)

      由图
      
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E5%88%86%E5%B8%83%E5%9B%BE.png)

      可知，介于均匀分布和随机分布之间,可以归属于随机分布
      
## 4.计算核密度

     带宽=1
     
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E5%B8%A6%E5%AE%BD%E4%B8%BA%EF%BC%88%E4%B8%80%EF%BC%89.jpeg)![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E5%B8%A6%E5%AE%BD%E4%B8%BA%EF%BC%88%E4%B8%80%EF%BC%89%E6%A0%87%E8%AE%B0.jpeg)

     带宽=2
     
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E5%B8%A6%E5%AE%BD%E4%B8%BA%EF%BC%88%E4%BA%8C%EF%BC%89.jpeg)![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E5%B8%A6%E5%AE%BD%E4%B8%BA%EF%BC%88%E4%BA%8C%EF%BC%89%E6%A0%87%E8%AE%B0.jpeg)

     带宽=3
     
![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E5%B8%A6%E5%AE%BD%E4%B8%BA%EF%BC%88%E4%B8%89%EF%BC%89.jpeg)![Image text ](https://github.com/cuit201608/Team8_coding/blob/master/folder/%E5%B8%A6%E5%AE%BD%E4%B8%BA%EF%BC%88%E4%B8%89%EF%BC%89%E6%A0%87%E8%AE%B0.jpeg)


