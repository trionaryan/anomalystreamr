

# Rod 1
results_high<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod1MostOf/stray_results01-May-2020-12.27.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))
results_control<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod1MostOf/stray_results01-May-2020-12.30.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))
results_low<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod1MostOf/stray_results01-May-2020-12.35.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))

# Rod 2
results_high<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod2MostOf/stray_results01-May-2020-20.05.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))
results_control<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod2MostOf/stray_results01-May-2020-20.17.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))
results_low<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod2MostOf/stray_results01-May-2020-20.29.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))

results_control=results_control[results_control$V2<78,]
results_low=results_low[results_low$V2<78,]
results_high=results_high[results_high$V2<78,]

# Rod 3
results_high<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod3MostOf/stray_results01-May-2020-21.00.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))
results_control<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod3MostOf/stray_results01-May-2020-20.52.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))
results_low<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod3MostOf/stray_results01-May-2020-20.43.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))

results_control=results_control[results_control$V2<40,]
results_low=results_low[results_low$V2>15,]
results_high=results_high[results_high$V2<40,]
results_high=results_high[results_high$V2>15,]

# Rod 4
results_high<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod4MostOf/stray_results01-May-2020-21.18.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))
results_control<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod4MostOf/stray_results01-May-2020-21.23.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))
results_low<-read.csv("~/Documents/Maynooth/IMR/AoifeCode/Results/Rod4MostOf/stray_results01-May-2020-21.28.csv",header=FALSE,sep="",col.names<-c("x positions, y positions, layer number"))

results_control=results_control[results_control$V2<20,]
results_high=results_high[results_high$V2>-20,]

library(ggplot2)
library(stray)
require(gridExtra)
library(scatterplot3d)
library(rgl)

plot3d(results_low,col="red",xlab="x position",ylab="y position",zlab="layer number")
plot3d(results_control,col="blue",add=TRUE)#,xlab="x position",ylab="y position",zlab="layer number")
plot3d(results_high,col="green",add=TRUE)#,xlab="x position",ylab="y position",zlab="layer number"))


# Rod 1 
legend3d("topright", legend = paste('Type', c('Low gas flow - 283 anomalies', 'High gas flow - 392 anomalies','Control ?medium? gas flow - 333 anomalies')), pch = 16, col = rainbow(3), cex=1, inset=c(0.02))
# Rod 2
legend3d("topright", legend = paste('Type', c('Low gas flow - 342 anomalies', 'High gas flow - 337 anomalies','Control ?medium? gas flow - 330 anomalies')), pch = 16, col = rainbow(3), cex=1, inset=c(0.02))

# Rod 3
legend3d("topright", legend = paste('Type', c('Low gas flow - 288 anomalies', 'High gas flow - 256 anomalies','Control ?medium? gas flow - 219 anomalies')), pch = 16, col = rainbow(3), cex=1, inset=c(0.02))

# Rod 4s
legend3d("topright", legend = paste('Type', c('Low gas flow - 233 anomalies', 'High gas flow - 247 anomalies','Control ?medium? gas flow - 252 anomalies')), pch = 16, col = rainbow(3), cex=1, inset=c(0.02))


dim(results_high)
dim(results_control)
dim(results_low)

snapshot3d(filename = 'fourthrod3dplot.png', fmt = 'png')
