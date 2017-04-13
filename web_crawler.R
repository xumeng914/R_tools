
# install.packages("rvest")
library(rvest)
url<-"http://www.weather.com.cn/weather/101210101.shtml"
# 解析网页
web<-read_html(url,encoding = "utf8")
# 天气数据抓取
weather<-web%>%
  html_nodes("div")%>%
  html_nodes("ul.t.clearfix")%>%
  html_nodes("li")%>%
  html_text()%>%
  strsplit(split="[\n]+")%>%
  as.data.frame(fix.empty.names = FALSE,row.names=c("无","date","weather","temperature","wind"))%>%
  t()
weather<-weather[,c(-1,-2)]
# 标示实时日期
date<-c(Sys.Date(),Sys.Date()+1,Sys.Date()+2,Sys.Date()+3,Sys.Date()+4,Sys.Date()+5,Sys.Date()+6)
weather<-data.frame(date,weather)
