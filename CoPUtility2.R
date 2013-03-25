# for total of the cvs file.

library(ggplot2);

#setting dataset path
source("setting.R");

#get argument from console mode
args = commandArgs(trailingOnly=T);

file_name = "total.csv";

all_data = read.csv(file=file_name,head=F,sep=",");

names(all_data) = c("Seq","CoPx","CoPy","Class");


#Draw x=CoPx,y=CoPy graphic.
ggplot(all_data,aes(x=CoPx,y=CoPy,group=Class))+geom_line(aes(colour=Class))+ scale_colour_gradientn(colour=rainbow(7));

#Draw x=Seq,y=CoPx grphic.
ggplot(all_data,aes(x=Seq,y=CoPx,group=Class))+geom_line(aes(colour=Class))+ scale_colour_gradientn(colours=rainbow(7));

#Draw x=Seq,y=CoPy grphic.
ggplot(all_data,aes(x=Seq,y=CoPy,group=Class))+geom_line(aes(colour=Class))+ scale_colour_gradientn(colours=rainbow(7));

All_Seq_Max = sapply(1:50,function(i) max(all_data$Seq[all_data$Class==i]));

Seq_min = min(All_Seq_Max);

CoPx_mean = sapply(1:Seq_min,function(i) mean(all_data$CoPx[all_data$Seq==i]));

CoPy_mean = sapply(1:Seq_min,function(i) mean(all_data$CoPy[all_data$Seq==i]));

CoPx_MeanMat = mapply(1:50,function(i) all_data$CoPx[all_data$Class==i]-CoPx_mean);
CoPy_MeanMat = mapply(1:50,function(i) all_data$CoPy[all_data$Class==i]-CoPy_mean);
