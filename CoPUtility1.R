#for normal of the cvs file
library(ggplot2);

#setting dataset path
source("setting.R");

#get argument from console mode
args = commandArgs(trailingOnly=T);


file_name = '035.csv';

all_data = read.csv(file=file_name,head=F,sep=',');

names(all_data) = c("Seq","CoPx","CoPy","Class");

N = nrow(all_data);

old_par = par();

#layout(matrix(c(2,0,1,3),2,2,byrow=T),widths=c(4,1),heights=c(1,2));


plot(all_data$CoPx,all_data$CoPy);
plot(all_data$Seq,all_data$CoPx);
plot(all_data$Seq,all_data$CoPy);

ML_mean = mean(all_data$CoPx); ML_mean;

AP_mean = mean(all_data$CoPy); AP_mean;

ML = all_data$CoPx-ML_mean;

AP = all_data$CoPy-AP_mean;

RD = sqrt(AP^2+ML^2); RD;


plot(ML,AP);
plot(ML);
plot(AP);

par(old_par);

plot(RD);
MDIST = sum(RD)/N; MDIST;

MDIST_ML = sum(abs(ML))/N; MDIST_ML;

MDIST_AP = sum(abs(AP))/N; MDIST_AP;

RDIST = sqrt(sum(RD^2)/N); RDIST;

RDIST_ML = sqrt(sum(ML^2)/N); RDIST_ML;

RDIST_AP = sqrt(sum(AP^2)/N); RDIST_AP;

AP_N1 = AP[2:length(AP)];
AP_N = AP[1:length(AP)-1];
ML_N1 = ML[2:length(ML)];
ML_N = ML[1:length(ML)-1];

TOTEX = sum(sqrt((AP_N1-AP_N)^2+(ML_N1-ML_N)^2)); TOTEX;
TOTEX_AP = sum(abs(AP_N1-AP_N)); TOTEX_AP;
TOTEX_ML = sum(abs(ML_N1-ML_N)); TOTEX_ML;

MVELO = TOTEX/10.0; MVELO;
MVELO_AP = TOTEX_AP/10.0; MVELO_AP;
MVELO_ML = TOTEX_ML/10.0; MVELO_ML;

#layout(matrix(c(2,0,1,3),2,2,byrow=T),widths=c(4,1),heights=c(1,2));
plot(ML,AP);

S_AP = sd(AP); S_AP;
S_ML = sd(ML); S_ML;
S_RD=sd(RD); S_RD;

AREA_R = MDIST+1.645*S_RD; AREA_R;
AREA_CC = pi*AREA_R^2; AREA_CC;

#D = sqrt(as.complex((S_AP^2+S_ML^2)-4*(S_AP^2*S_ML^2-cov(AP,ML)^2))); D;
Fd = qf(0.95,2,N-2); Fd;

#AREA_CE_ALPHA = sqrt(Fd*(S_AP^2+S_ML^2+D)); AREA_CE_ALPHA;
#AREA_CE_BETA = sqrt(Fd*(S_AP^2+S_ML^2-D)); AREA_CE_BETA;
#AREA_CE = pi*AREA_CE_ALPHA*AREA_CE_BETA; AREA_CE;
AREA_CE = 2*pi*Fd*sqrt(S_AP^2*S_ML^2-cov(AP,ML)^2); AREA_CE;

AREA_SW = sum(abs(AP_N1*ML_N-AP_N*ML_N1))/20; AREA_SW;

MFREQ = MVELO/(2*pi*MDIST);  MFREQ;

MFREQ_AP = MVELO_AP/(4*sqrt(2)*MDIST_AP); MFREQ_AP;
MFREQ_ML = MVELO_ML/(4*sqrt(2)*MDIST_ML); MFREQ_ML;

dFD_CC = 2*(MDIST+1.645*S_RD); dFD_CC;
FD_CC = log(N)/log(N*dFD_CC/TOTEX); FD_CC;