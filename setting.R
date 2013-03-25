library(XML);


dataset = xmlValue(xmlRoot(xmlTreeParse("setting.xml")));

setwd(dataset);