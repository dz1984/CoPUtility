library(grid);
library(ggplot2);

#setting dataset path
source("setting.R");

#get argument from console mode
args = commandArgs(trailingOnly=T);

N = 10;
COLS = 4;

layout = matrix(seq(1,COLS*ceiling(N/COLS)),ncol=COLS,nrow=ceiling(N/COLS));
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))));


for( i in 1:N){
  
  file_name = sprintf('%03d',i+40);
  
  csv_file_name = sprintf('%s.csv',file_name);
  
  png_file_name = sprintf('%s.png',file_name);
  
  all_data = read.csv(file=csv_file_name,head=F,sep=',');
  
  names(all_data) = c("Seq","CoPx","CoPy","Class");
  
  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
  
  
  #temp_graphic=ggplot(all_data,aes(x=CoPx,y=CoPy))+geom_step(size=2,colour="grey30")+geom_point(colour="white");
  temp_graphic=ggplot(all_data,aes(x=CoPx,y=CoPy))+geom_path();
  print(temp_graphic+opts(title=file_name), vp = viewport(layout.pos.row = matchidx$row,
                                                          layout.pos.col = matchidx$col))
  ggsave(temp_graphic,file=png_file_name);
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}