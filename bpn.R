library(neuralnet);

#setting dataset path
source("setting.R");

dat = read.csv(file="data.csv",head=F,sep=",");
head = names(dat);

feature_range = 1:16;
category_range = 17:21;

test_bpn = function (n){
  category = paste(head[category_range],collapse='+');
  feature = paste(head[feature_range],collapse='+');
  formula = as.formula(paste(category,'~',feature));
  training = sample(1:nrow(dat),nrow(dat)*0.7);
  test = which(1:nrow(dat)%in%training == F);
  net = neuralnet(formula,dat[training,],hidden=n,rep=10,algorithm="backprop",err.fct="sse",linear.output=F,learningrate=0.2,stepmax=1e+8);
  
  #result = prediction(net);
  
  #apply(dat[order(dat[,2]),]==result$data[order(result$data[,2]),],1,FUN=all);
  
  result = compute(net,dat[test,feature_range]);
  
  predict_result = round(result$net.result);
  
  fail_predict = names(which(apply(predict_result==dat[test,category_range],1,FUN=all)==F));
  
  return (1-(length(fail_predict)/length(test)));
};

wrapper = function (n) {
  score = sum(rep(test_bpn(n),100));
  write(sprintf("[%d]:%f",n,score),"result.tmp",append=T);
  return(score);
}

#result = lapply(0:10,wrapper);
result = wrapper(6);


