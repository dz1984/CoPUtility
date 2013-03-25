library(grid);
library(ggplot2);


#setting dataset path
source("setting.R");

#get argument from console mode
args = commandArgs(trailingOnly=T);

START = as.integer(args[1]);
END = as.integer(args[2]);

if (is.na(START)) START = 1;
if (is.na(END)) END = 50;
COLS = 1;
COUNT = END - START;

layout = matrix(seq(1,COLS*ceiling(COUNT/COLS)),ncol=COLS,nrow=ceiling(COUNT/COLS));
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))));

e_dist = function (x0,y0,x1,y1){
  return (sqrt((x1-x0)^2+(y1-y0)^2));
}

catch_point = function(arrs){
  N = length(arrs);
  state = arrs[1];
  result = c();
  for (i in 2:N){
    if (state!=arrs[i] && arrs[i]!=0){
      result=c(result,i);
      state=arrs[i];
    }
    
  }
  return (result);
}

slope = function(a,b){
  temp = b-a;
  return (temp[1]/temp[2]);
}

intersect = function(p1,p2,p3,p4){
  s = slope(p1,p2);
  t = slope(p3,p4);
  n = ((s*p1[2]-t*p3[2])-p1[1]+p3[1])/(s-t); 
  e = (n-p1[2])*s+p1[1];
  x1 = round(c(p1[1],p2[1]),7);
  y1 = round(c(p1[2],p2[2]),7);
  x2 = round(c(p3[1],p4[1]),7);
  y2 = round(c(p3[2],p4[2]),7);
  e = round(e,7);
  n = round(n,7);
  if (is.na(e) || is.na(n)) return(F)
  else if (e<max(x1) && e<max(x2) && e> min(x1) && e>min(x2) && n<max(y1) && n<max(y2) && n>min(y1) && n>min(y2)) return(T) 
  else return(F)
}
intersect_debug = function(data,p1,p2,p3,p4){
  p1 = get_point(data,p1);
  p2 = get_point(data,p2);
  p3 = get_point(data,p3);
  p4 = get_point(data,p4);
  s = slope(p1,p2);
  t = slope(p3,p4);
  n = ((s*p1[2]-t*p3[2])-p1[1]+p3[1])/(s-t); 
  e = (n-p1[2])*s+p1[1];
  x = round(c(p1[1],p2[1]),5);
  y = round(c(p1[2],p2[2]),5);
  e = round(e,5);
  n = round(n,5);  
  return(c(e,n,list(x),list(y)));
}
get_point = function(data,i){
  
  return (c(data$CoPx[i],data$CoPy[i]));
}

start = proc.time();

for( i in START:END){
  
  file_name = sprintf('%03d',i);
  
  csv_file_name = sprintf('%s.csv',file_name);
  
  png_file_name = sprintf('m%s.png',file_name);
  
  all_data = read.csv(file=csv_file_name,head=F,sep=',');
  
  names(all_data) = c("Seq","CoPx","CoPy","Class");
  
  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
  
  N = nrow(all_data);
  group = 0;
  temp = c(1,2);
  index = c();
  path = data.frame();
  for (i in 3:N){
    temp_len = length(temp);
    if (temp_len>2){
      j = 1;
      while (j > 0 && j <= temp_len-1){
        p1 = get_point(all_data,temp[j]);
        p2 = get_point(all_data,temp[j+1]);
        p3 = get_point(all_data,i-1);
        p4 = get_point(all_data,i)
        if (intersect(p1,p2,p3,p4)){
          index = c(index,i);
          path = rbind(path,cbind(all_data[temp[j]:i,],group=group));
          temp = c();
          group = group + 1;
          j=-1;
        } 
        j=j+1;
      } 
    }
    temp = c(temp,i);
  }
  
  g = ggplot(all_data,aes(x=CoPx,y=CoPy))+
    geom_point(aes(x=CoPx,y=CoPy,colour=(Seq/1000)))+
    scale_alpha_discrete(range=c(0.1,1))+
    geom_point(data=path,aes(x=CoPx,y=CoPy),colour="yellow",linetype=2,group=group)+
    geom_point(data=all_data[index,],aes(x=CoPx,y=CoPy),colour="red")+
    geom_point(data=head(all_data,1),aes(x=CoPx,y=CoPy),colour="green")+
    geom_point(data=tail(all_data,1),aes(x=CoPx,y=CoPy),colour="blue");
  
  #print(g+opts(title=file_name), vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
  ggsave(g,file=png_file_name,width=8,height=6);
}

end = proc.time();

print(end-start);


