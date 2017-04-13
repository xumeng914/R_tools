
# install.packages("rvest")
library(rvest)

city<- "101210101"
url<-paste('http://www.weather.com.cn/weather/',city,'.shtml',sep="")

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

####################################################################

page <- readLines(paste("http://lishi.tianqi.com/","shaoxing","/20160",6, ".html", sep = ""))
str(page)

grep(paste("绍兴2016年",6, "月份天气详情", sep = ""), page)
page[347:348]

# 找出包含最高气温的行及内容
max_char <- page[seq(from = 347+12, by = 8, length.out = 31)]
# 提取出最高气温的数据，转换为数值型
max_value <- as.numeric(unlist(lapply(max_char, function(x) strsplit(x, ">|<")[[1]][3])))

min_char <- page[seq(from = 291, by = 8, length.out = 31)]
min_value <- as.numeric(unlist(lapply(min_char, function(x) strsplit(x, ">|<")[[1]][3])))


weather_char <- page[seq(from = 347+14, by = 8, length.out = 31)]
weather_value <- as.character(unlist(lapply(weather_char, function(x) strsplit(x, ">|<")[[1]][3])))


df <- data.frame(month = 7, max = max_value, min = min_value)



####################################################################
find_data <- function(x, gps, length) {
  x_char <- paste("0",x,sep="")
  
  page <- readLines(paste("http://lishi.tianqi.com/",gps,"/2016", x_char, ".html", sep = ""))
  
  point <- grep(paste("绍兴2016年", x, "月份天气详情", sep = ""), page)
  
  max_char <- page[seq(from = point + 12, by = 8, length.out = length)]
  
  max_value <- as.numeric(unlist(lapply(max_char, function(x) strsplit(x, ">|<")[[1]][3])))
  
  min_char <- page[seq(from = point + 13, by = 8, length.out = length)]
  
  min_value <- as.numeric(unlist(lapply(min_char, function(x) strsplit(x, ">|<")[[1]][3])))
  
  weather_char <- page[seq(from = point + 14, by = 8, length.out = length)]
  
  weather_value <- as.character(unlist(lapply(weather_char, function(x) strsplit(x, ">|<")[[1]][3])))
  
  wind_char <- page[seq(from = point + 16, by = 8, length.out = length)]
  
  wind_value <- as.character(unlist(lapply(wind_char, function(x) strsplit(x, ">|<")[[1]][3])))
  
  df <- data.frame(month = x, max = max_value, min = min_value, weather = weather_value, wind = wind_value)
  
  df
}

# 返回2015年6月的气温数据
df_6 <- find_data(x = 6, gps = "shaoxing", length = 30)

