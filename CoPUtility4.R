library(ggplot2);

#setting dataset path
source("setting.R");

#get argument from console mode
args = commandArgs(trailingOnly=T);

file_name = "030.csv";

all_data = read.csv(file=file_name,head=F,sep=",");

names(all_data) = c("Seq","CoPx","CoPy","Class");

N = nrow(all_data);

CoPx_Y = fft(all_data$CoPx-mean(all_data$CoPx));
CoPy_Y = fft(all_data$CoPy-mean(all_data$CoPy));

CoPx_r = Re(CoPx_Y);
CoPy_r = Re(CoPy_Y);

CoPx_i = Im(CoPx_Y);
CoPy_i = Im(CoPy_Y);

CoPx_a = sqrt(CoPx_r^2+CoPx_i^2)*2/N;
CoPy_a = sqrt(CoPy_r^2+CoPy_i^2)*2/N;

CoPx_p = Arg(CoPx_Y)*180/pi;
CoPy_p = Arg(CoPy_Y)*180/pi;

#f = (1/(N*0.0001))*1:N/10;
f = (1:N)/10;
t = 1/f;

M = data.frame(cbind(f,t,CoPx_r,CoPy_r,CoPx_i,CoPy_i,CoPx_a,CoPy_a,CoPx_p,CoPy_p));

ggplot(data=all_data) +
  geom_path(aes(x=Seq,y=CoPx),colour="red")+
  geom_path(aes(x=Seq,y=CoPy),colour="blue")+
  labs(x="Seq",y="CoP");


#ggplot(data=M) + geom_path(aes(x=t,y=CoPx_r));
#ggplot(data=M) + geom_path(aes(x=t,y=CoPx_i));

ggplot(data=M[1:N/2,]) + geom_path(aes(x=f,y=CoPx_a))+labs(x="f",y="a")+opts(title="CoPx a-f");
ggplot(data=M[1:N/2,]) + geom_path(aes(x=f,y=CoPy_a))+labs(x="f",y="a")+opts(title="CoPy a-f");
ggplot(data=M[1:N/2,]) + geom_path(aes(x=f,y=CoPx_p))+labs(x="f",y="p")+opts(title="CoPx p-f");
ggplot(data=M[1:N/2,]) + geom_path(aes(x=f,y=CoPy_p))+labs(x="f",y="p")+opts(title="CoPy p-f");

ggplot(data=M) + geom_path(aes(x=t,y=CoPx_a))+labs(x="t",y="a")+opts(title="CoPx a-t");
ggplot(data=M) + geom_path(aes(x=t,y=CoPy_a))+labs(x="t",y="a")+opts(title="CoPy a-t");
ggplot(data=M) + geom_path(aes(x=t,y=CoPx_p))+labs(x="t",y="p")+opts(title="CoPx p-t");
ggplot(data=M) + geom_path(aes(x=t,y=CoPy_p))+labs(x="t",y="p")+opts(title="CoPy p-t");

ggplot(data=all_data) +
  geom_path(aes(x=CoPx,y=CoPy,colour=(Seq/1000)))+
  #scale_colour_gradientn(colours=rainbow(7))+
  scale_alpha_discrete(range=c(0.1,1))+
  geom_point(data=head(all_data,1),aes(x=CoPx,y=CoPy),colour="red")+
  geom_point(data=tail(all_data,1),aes(x=CoPx,y=CoPy),colour="blue");
