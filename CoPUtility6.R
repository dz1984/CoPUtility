#Distance
library(ggplot2);

#setting dataset path
source("setting.R");

#get argument from console mode
args = commandArgs(trailingOnly=T);

file_name = "030.csv";

all_data = read.csv(file=file_name,head=F,sep=",");

names(all_data) = c("Seq","CoPx","CoPy","Class");

MyDist = function (x0,y0,x1,y1){
  return (sqrt((x1-x0)^2+(y1-y0)^2));
}

inSet = function(i,temp){
  N = length(temp);
  if (N==0)
    return (FALSE);
  for (j in 1:N){
    d = MyDist(all_data$CoPx[i],all_data$CoPy[i],all_data$CoPx[temp[j]],all_data$CoPy[temp[j]]);
    #d = min(sqrt((all_data$CoPx[i]-all_data$CoPx[temp])^2+(all_data$CoPy[i]-all_data$CoPy[temp])^2));
    if (d<=0.003)
      return (TRUE);   
  }
  return (FALSE);
}
#N = nrow(all_data)/2;
N = 5000;
temp = c();
index = c();

for (i in 1:N){
  if (inSet(i,temp)){
    index = c(index,i);
    temp = c();
  }else{
    temp = c(temp,i);
  }
}

all_data$CoPx[index];
all_data$CoPy[index];

g = qplot(CoPx,CoPy,data=all_data);
g+geom_point(aes(x=CoPx,y=CoPy,colour=(Seq/1000)))+
  scale_alpha_discrete(range=c(0.1,1))+
  geom_point(data=all_data[index,],aes(x=CoPx,y=CoPy),colour="red")+
  geom_point(data=head(all_data,1),aes(x=CoPx,y=CoPy),colour="green")+
  geom_point(data=tail(all_data,1),aes(x=CoPx,y=CoPy),colour="blue");