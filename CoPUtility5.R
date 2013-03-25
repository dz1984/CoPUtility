library(grid);
library(ggplot2);

#setting dataset path
source("setting.R");

#get argument from console mode
args = commandArgs(trailingOnly=T);


DIRECTION = array(c(5,6,7,4,-1,0,3,2,1),dim=c(3,3));

COUNT = 1;
COLS = 1;

layout = matrix(seq(1,COLS*ceiling(COUNT/COLS)),ncol=COLS,nrow=ceiling(COUNT/COLS));
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))));


#FUNCTION AREA
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

map_direction = function(x,y){
  return (DIRECTION[x+2,y+2]);
}

parse_path = function(arrs){
  N = nrow(arrs);
  result = c();
  for (i in 1:N-1){
    dx = sign(arrs[i+1,]$CoPx - arrs[i,]$CoPx);
    dy = sign(arrs[i+1,]$CoPy - arrs[i,]$CoPy);
    result = c(result,map_direction(dx,dy));
  }
  return(result);
  
}


#MAIN AREA

for( i in 1:COUNT){
  
  file_name = sprintf('%03d',i);
  
  csv_file_name = sprintf('%s.csv',file_name);
  
  png_file_name = sprintf('l%s.png',file_name);
  
  out_file_name = sprintf('r%s.csv',file_name);
  
  all_data = read.csv(file=csv_file_name,head=F,sep=',');
  
  names(all_data) = c("Seq","CoPx","CoPy","Class");
  
  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
  
  N = nrow(all_data);
  
  CoPx_n0 = round(all_data$CoPx[1:N-1],5);
  CoPx_n1 = round(all_data$CoPx[2:N],5);
  
  CoPy_n0 = round(all_data$CoPy[1:N-1],5);
  CoPy_n1 = round(all_data$CoPy[2:N],5);
  
  result = (CoPy_n1-CoPy_n0)/(CoPx_n1-CoPx_n0);
  
  #result = result[!is.na(result) & !is.infinite(result)];
  index = catch_point (sign(result));
  index = c(1,index);
  index = c(index,N);
  #index = c(which(result>=10),which(result<=-10));
  
  direction=c(parse_path(all_data[index,]),0);
  
  g = ggplot(all_data,aes(x=CoPx,y=CoPy))+
    geom_point(aes(x=CoPx,y=CoPy,colour=(Seq/1000)))+
    scale_alpha_discrete(range=c(0.1,1))+
    geom_point(data=all_data[index,],aes(x=CoPx,y=CoPy),colour="red")+
    geom_path(data=all_data[index,],aes(x=CoPx,y=CoPy),alpha=0.5,linetype=2)+
    geom_text(data=all_data[index,],aes(x=CoPx,y=CoPy,hjust=-0.2,vjust=0.2,label=as.character(direction)))+
    geom_point(data=head(all_data,1),aes(x=CoPx,y=CoPy),colour="green")+
    geom_point(data=tail(all_data,1),aes(x=CoPx,y=CoPy),colour="blue");
  
  #print(g+opts(title=file_name), vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
  ggsave(g,file=png_file_name,width=8,height=6);
  
  write.csv(direction,out_file_name,row.names=F)
  
}

