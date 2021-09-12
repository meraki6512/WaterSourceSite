#Setting

library(readxl)
library(xlsx)
library(dplyr)
library(rvest)
library(httr)
library(RSelenium)
library(wdman)
library(stringr)
library(ggplot2)
library(gridExtra)

options(max.print=100000)





## Crit1. Sea water

rm(list = ls())
setwd("C:/Users/username/Documents")
# 기상청 날씨누리 해양기상부이 통계자료
file10 <- read_excel("2010.xlsx")
file11 <- read_excel("2011.xlsx")
file12 <- read_excel("2012.xlsx")
file13 <- read_excel("2013.xlsx")
file14 <- read_excel("2014.xlsx")
file15 <- read_excel("2015.xlsx")
file16 <- read_excel("2016.xlsx")
file17 <- read_excel("2017.xlsx")
file18 <- read_excel("2018.xlsx")
file19 <- read_excel("2019.xlsx")
file20 <- read_excel("2020.xlsx")
infoPlace <- read_excel("관측지점정보.xlsx")

# 월별 평균으로 분류된 파일만들기
make_month_file <- function(a,b){
    a[,"difference"] <- abs(a$`평균 기온(°C)` - a$`평균 수온(°C)`)
    a <- filter(a,!is.na(a$difference))
    data <- data.frame(do.call('rbind', 
                               strsplit(as.character(a$일시), 
                                        split = '-', fixed = TRUE)))
    data
    a <- cbind(a, data)
    a <- a[,c(1,2,6,7,8,3,4,5)]
    names(a)[3]="year"
    names(a)[4]="month"
    names(a)[5]="day"
    a <- a %>%
        group_by(지점, month) %>%
        summarize(meanmonth = mean(difference, na.rm = TRUE),year=min(year))
    a<- merge(a, b, by= "지점")
    a <- a[,c(1:4,7)]
    return(a)
}

# 연도별 평균으로 분류된 파일 만들기
make_year_file <- function(a,b){
    a[,"difference"] <- abs(a$`평균 기온(°C)` - a$`평균 수온(°C)`)
    a <- filter(a,!is.na(a$difference))
    data <- data.frame(do.call('rbind', 
                               strsplit(as.character(a$일시), 
                                        split = '-', fixed = TRUE)))
    a <- cbind(a, data)
    a <- a[,c(1,2,6,3,4,5)]
    names(a)[3]="year"
    a <- a %>%
        group_by(지점) %>%
        summarize(meancountry = mean(difference, na.rm = TRUE),year = min(year))
    a<- merge(a, b, by= "지점")
    a <- a[,c(1,2,3,6)]
    return(a)
}

# 연도별 월평균 데이터프레임
months10 <- make_month_file(file10,infoPlace)
months11 <- make_month_file(file11,infoPlace)
months12 <- make_month_file(file12,infoPlace)
months13 <- make_month_file(file13,infoPlace)
months14 <- make_month_file(file14,infoPlace)
months15 <- make_month_file(file15,infoPlace)
months16 <- make_month_file(file16,infoPlace)
months17 <- make_month_file(file17,infoPlace)
months18 <- make_month_file(file18,infoPlace)
months19 <- make_month_file(file19,infoPlace)
months20 <- make_month_file(file20,infoPlace)

# 연도별 연평균 데이터 프레임
country10 <- make_year_file(file10,infoPlace)
country11 <- make_year_file(file11,infoPlace)
country12 <- make_year_file(file12,infoPlace)
country13 <- make_year_file(file13,infoPlace)
country14 <- make_year_file(file14,infoPlace)
country15 <- make_year_file(file15,infoPlace)
country16 <- make_year_file(file16,infoPlace)
country17 <- make_year_file(file17,infoPlace)
country18 <- make_year_file(file18,infoPlace)
country19 <- make_year_file(file19,infoPlace)
country20 <- make_year_file(file20,infoPlace)

#파일 합치기
month_bind <- rbind(months10,months11,months12,months13,months14,months15,months16,months17,months18,months19, months20)
country_bind <- rbind(country10,country11, country12,country13, country14, country15, country16,country17,country18,country19,country20)
month_bind$month <- as.numeric(month_bind$month)


# 계절별 온도차 평균
springs <- subset(month_bind, month == 3 | month == 4 | month == 5) %>%
    group_by(지점명) %>%
    summarize(mean = mean(meanmonth))
summers <- subset(month_bind, month == 6 | month == 7 | month == 8) %>%
    group_by(지점명) %>%
    summarize(mean = mean(meanmonth))
autumns <- subset(month_bind, month == 9 | month == 10 | month == 11) %>%
    group_by(지점명) %>%
    summarize(mean = mean(meanmonth))
winters <- subset(month_bind, month == 1 | month == 2 | month == 12) %>%
    group_by(지점명) %>%
    summarize(mean = mean(meanmonth))
    
#오름차순 정렬
springs[,"계절"] <- c("봄")
springs <- springs[c(order(springs$mean)),]
summers[,"계절"] <- c("여름")
summers <- summers[c(order(summers$mean)),]
autumns[,"계절"] <- c("가을")
autumns <- autumns[c(order(autumns$mean)),]
winters[,"계절"] <- c("겨울")
winters <- winters[c(order(winters$mean)),]
renew_bind <- rbind(springs, summers, autumns, winters) 
renew_bind$지점명 <- as.factor(renew_bind$지점명)

# 계절별 지점별 평균 그래프
ggplot(data = springs, aes(x= mean, y=reorder(지점명,mean), group = 지점명, color = 지점명,fill = 지점명)) + geom_col() + ggtitle("spring") + labs(x ="수온기온차", y = "지점명")
ggplot(data = summers, aes(x= mean, y=reorder(지점명,mean), ylab(평균),group = 지점명, color = 지점명, fill = 지점명)) + geom_col() + ggtitle("summer") + labs(x ="수온기온차", y = "지점명")
ggplot(data = autumns, aes(x= mean, y=reorder(지점명,mean), group = 지점명, color = 지점명,fill = 지점명)) + geom_col() + ggtitle("autumn") + labs(x ="수온기온차", y = "지점명")
ggplot(data = winters, aes(x= mean, y=reorder(지점명,mean), group = 지점명, color = 지점명,fill = 지점명)) + geom_col() + ggtitle("winter") + labs(x ="수온기온차", y = "지점명")

# 연도별 평균 묘사 그래프
month_bind$month <- as.factor(month_bind$month)
ggplot(data = country_bind, aes(x=meancountry, y =지점명 , color = 지점명, group = 지점명,fill = 지점명)) + geom_col(aes(color=지점명)) + facet_wrap(~year) + ggtitle("지점별 년평균 수온기온차") + labs(x ="수온기온차", y = "지점명")

# 월별 평균 묘사 그래프
ggplot(data = month_bind, aes(x=meanmonth, y = month , color = year, group = 지점명,fill = 지점명)) + geom_point() + facet_wrap(~지점명) + ggtitle("년별 월평균 수온기온차") + labs(x ="수온기온차", y = "month")






## Crit2. River water

# 하천 유량 데이터파일
##excel파일 읽기
dir <- "C:/Users/username/Documents/유량"

#연도별 excel을 하나의 dataframe으로
file_list <- list.files(dir) #디렉토리 내 모든 파일이름을 불러옵니다.(string vector)
data <- data.frame()
for(file in file_list){
    print(file)
    temp <- read_excel(paste(dir, file, sep="/"),sk=2)
    data <- rbind( data, temp )
}

#데이터프레임 다듬기
df <- data[,c(2,3,4,14)] #측정소명, 날짜, 수온, 유량
colnames(df) <- c("loc","date","temp","fluid") #column명 수정

#자료형변환
df$temp <- as.numeric(df$temp) 
df$fluid <- as.numeric(df$fluid)
df$date <- gsub("[:.:]","-",df$date)
df$date <- as.Date(df$date)
df <- df[complete.cases(df),]

#한글 기준으로 통일 (뒤에 A,B-1같은거 무시)
df$group <- gsub("[[:upper:]]","",df$loc)
df$group <- gsub("[[:digit:]]","",df$group)
df$group <- gsub("[[:punct:]]","",df$group)
df$group <- as.factor(df$group)

#전구역 graph(전체)
ggplot(data = df, aes(x=date, y=fluid, color = group)) + 
    geom_line()

#부지 선정(View로 평균, 분산, 최소, 최대 확인가능)
df_mean <- df[(df$fluid>0),]
df_mean <- df_mean %>%
    group_by(group) %>%
    summarize(mean = mean(fluid), var=var(fluid),min=min(fluid),max=max(fluid))
df_mean <- df_mean[order(df_mean$mean, decreasing=T),]

#순위권 지점의 월별 유량
#표준점수 차로 순위 정해보면
#(평균은 클수록 분산은 작을수록 좋음)
mean_M <- mean(df_mean$mean)
mean_V <- var(df_mean$mean)
df_mean$m_score <- (df_mean$mean-mean_M)/mean_V #평균 표준점수

var_M <- mean(df_mean$var)
var_V <- var(df_mean$var)
df_mean$v_score <- (df_mean$var-var_M)/var_V #분산 표준점수

df_mean$score <- df_mean$m_score - df_mean$v_score #표준점수 차

View(df_mean) #결과 확인 
library(openxlsx)
write.xlsx(df_mean, file="유량결과.xlsx")


#상위6개 구역 graph(그래프6개 따로)
p <- list()
for(i in (1:6)){
    p[[i]] <- ggplot(data = df[df$group==head(df_mean)$group[i],], aes(x=date, y=fluid, color = group)) + 
        geom_line()+
        ylim(0,9000)
}
grid.arrange(grobs=p, nrow=2 ,ncol = 3) #(2,3) 총6구역으로 ggplot 화면분할






## Hydrothermal source site selection (In Seoul)
## (Criteria 3: Runoff amounts && Criteria 4: Energy usage)

#1. PreProcessing Runoff

#(1) Open
setwd("C:/Users/username/Documents/Runoff")
runoff_all <- as.data.frame(readxl::read_excel("서울특별시 건축물 유출지하수 현황.xlsx", col_names = T, col_types = "guess", na = "NA"))

#(2) Data needed
runoff_need <- runoff_all[,c(1:2,4:7,13)]
colnames(runoff_need) <- c("name", "location", "area", "year", "total", "daily", "unused")
runoff_need <- runoff_need[!(runoff_need$year<2018),]
runoff_need <- runoff_need[!is.na(runoff_need$total),]
runoff_need <- runoff_need[!duplicated(runoff_need),]
rownames(runoff_need) <- NULL

#save_runoff_need<-runoff_need

#(3) Standardize address (crawling)
for (i in 1:length(runoff_need$location)){
    #blanks
    if (length(grep('[[:blank:]]',runoff_need$location[i]))!=0){
        runoff_need$location[i] <- gsub(" ", "", runoff_need$location[i])
    }
    #parentheses
    if (length(grep('[(]',runoff_need$location[i]))!=0){
        runoff_need$location[i] <- gsub("[(].*", "", runoff_need$location[i])
    }
}

urls <- list()
for (i in 1:length(runoff_need$location)){
    Query <- runoff_need$location[i]
    url = paste0("https://search.naver.com/search.naver?&query=", Query)
    urls <- c(urls, url)
}
Loc <- runoff_need[,1:2]
Loc$region <- ""


#Way1: R Selenium  >>>>> takes too long (1.5h)
RS_connect = function(){
    tryCatch(expr = {
        port_no = as.integer(sample(2000:8000, 1))
        rs = chrome(port = port_no)
        remote <<- remoteDriver(remoteServerAddr = "localhost",
                                port = port_no,
                                browserName = "chrome")
        remote$open()
    }, finally = {
        port_no = as.integer(sample(2000:8000, 1))
        browser_ver = binman::list_versions(appname = "chromedriver")
        browser_ver = browser_ver[[1]][length(browser_ver[[1]]) - 2]
        rs = chrome(port = port_no, version = browser_ver)
        remote <<- remoteDriver(remoteServerAddr = "localhost",
                                port = port_no,
                                browserName = "chrome")
        remote$open()
    })
}
RS_connect()

for (i in 1:length(urls)){
    remote$navigate(urls[[i]])
    source = remote$getPageSource()
    html = read_html(source[[1]])
    addr <- html %>%
        html_nodes(xpath = '//*[@id="loc-main-section-root"]/section/div/div[2]/div[2]/div/div[1]/div[1]') %>%
        html_text() %>%
        unique()
    
    if (length(addr)>0){
        Loc$region[i] <- addr
    }
}

# Way2: Requests  >>>>> takes quite a while (1h) + Many failures
# f<-0
# fail <- list()
# for (i in 1:length(urls)){
#  html <- read_html(urls[[i]])
#  addr <- unique(html_text(html_nodes(html, xpath = '//*[@id="loc-main-section-root"]/section/div/div[2]/div[2]/div/div[1]/div[1]')))
#  if (length(addr)>0){
#    Loc$region[i] <- addr
#  }
#  else{
#    fail[f]<-i
#    f=f+1
#  }
#  print(i)
# }

##(3-2) fail on searching in way1 () >>> NA | omit

#(4) address - extract "gu"

Loc$region<-strsplit(Loc$region, split=" ")
for (i in 1:length(urls)){
    Loc$gu[i] <- Loc$region[[i]][2] 
}
runoff_new <- runoff_need
runoff_new$gu <- Loc$gu
runoff_new<-runoff_new[!is.na(runoff_new$gu),]
runoff_new<-runoff_new[runoff_new$total!=0,]

which(runoff_new$gu=="고양시") #1088 2486 2795 2831 3309
runoff_new$gu[1088]<-"송파구"
runoff_new$gu[2486]<-"송파구"
runoff_new$gu[2795]<-"동대문구"
runoff_new$gu[2831]<-"동대문구"
runoff_new$gu[3309]<-"동대문구"
which(runoff_new$gu=="당진시") #642 650
runoff_new$gu[642]<-"성북구"
runoff_new$gu[650]<-"성북구"
which(runoff_new$gu=="연수구") #2084 2195 3094
runoff_new$gu[2084]<-"금천구"
runoff_new$gu[2195]<-"금천구"
runoff_new$gu[3094]<-"금천구"

runoff_18<-runoff_new[(runoff_new$year==2018),]
runoff_19<-runoff_new[(runoff_new$year==2019),]
runoff_20<-runoff_new[(runoff_new$year==2020),]
runoff_18$gu <- as.factor(runoff_18$gu)
runoff_19$gu <- as.factor(runoff_19$gu)
runoff_20$gu <- as.factor(runoff_20$gu)


#2. PreProcessing Energy Use

#(1) Open
energy_use_18 <- as.data.frame(readxl::read_excel("2018년_건물에너지_사용량_현황.xls", col_names = T, col_types = "guess", na = "NA"))
energy_use_19 <- as.data.frame(readxl::read_excel("2019년_건물에너지_사용량_현황.xls", col_names = T, col_types = "guess", na = "NA"))
energy_use_20 <- as.data.frame(readxl::read_excel("2020년_건물에너지_사용량_현황.xls", col_names = T, col_types = "guess", na = "NA"))

#(2) Data needed
colnames(energy_use_18) <- c("gu", "total_num", "area", "elec_E", "gas_E", "heat_E", "total_E")
energy_use_18 <- energy_use_18[c(4:28),c(1:3,6:7)]
rownames(energy_use_18) <- NULL
colnames(energy_use_19) <- c("gu", "total_num", "area", "elec_E", "gas_E", "heat_E", "total_E")
energy_use_19 <- energy_use_19[c(4:28),c(1:3,6:7)]
rownames(energy_use_19) <- NULL
colnames(energy_use_20) <- c("gu", "total_num", "area", "elec_E", "gas_E", "heat_E", "total_E")
energy_use_20 <- energy_use_20[c(4:28),c(1:3,6:7)]
rownames(energy_use_20) <- NULL


#3. Merge

#(1) group runoff by "gu" (2018)

#1) get data : num, sum area, sum total, number, average (daily, unused)
new_18 <- energy_use_18[,(1:2)]  #just for new df
colnames(new_18) <- c("gu", "num")
new_18$num <- 0
new_18$sum_area <- 0  
new_18$sum_total <- 0
new_18$avg_daily <- 0
new_18$avg_unused <- 0
gu <- new_18$gu
rownames(runoff_18)<-NULL   
runoff_18 <- runoff_18[which(runoff_18$area!="터널"),]   
runoff_18 <- runoff_18[!is.na(runoff_18$area),]   
runoff_18$area<-as.numeric(runoff_18$area)   
for (i in 1:length(runoff_18$gu)){
    for (j in 1:length(gu)){
        if (runoff_18$gu[i]==new_18$gu[j]){
            new_18$num[j] <- new_18$num[j] + 1 
            new_18$sum_area[j] <- new_18$sum_area[j] + runoff_18$area[i]    
            new_18$sum_total[j] <- new_18$sum_total[j] + runoff_18$total[i]
            new_18$avg_daily[j] <- new_18$avg_daily[j] + runoff_18$daily[i]
            new_18$avg_unused[j] <- new_18$avg_unused[j] + runoff_18$unused[i]
        }
    }
}
tmp<-new_18$sum_area/new_18$num    
new_18$avg_daily <- new_18$avg_daily/tmp    
new_18$avg_unused <- new_18$avg_unused/tmp    

#2) merge data frames
colnames(new_18)<-c("gu", "num_B", "sum_area", "sum_total_runoff", "avg_daily_runoff", "avg_daily_unused")     
hydro_18 <- inner_join(energy_use_18, new_18, by="gu")
hydro_18 <- hydro_18[,c(1,3,7,2,6,8:10,5,4)]    
colnames(hydro_18) <- c("gu", "total_area", "area", "total_num_B",  "num_B", "total_runoff", "daily_runoff", "daily_unused", "total_E", "heat_E")     
rownames(hydro_18) <- hydro_18$gu

#(2) group runoff by "gu" (2019)

#1) get data : sum total, number, average (daily, unused)
new_19 <- energy_use_18[,(1:2)]   #just for new df
colnames(new_19) <- c("gu", "num")
new_19$num <- 0
new_19$sum_area <- 0  
new_19$sum_total <- 0
new_19$avg_daily <- 0
new_19$avg_unused <- 0

rownames(runoff_19)<-NULL   
#str(runoff_19)     
runoff_19 <- runoff_19[!is.na(runoff_19$area),]   
runoff_19$area<-as.numeric(runoff_19$area)   
for (i in 1:length(runoff_19$gu)){
    for (j in 1:length(gu)){
        if (runoff_19$gu[i]==new_19$gu[j]){
            new_19$num[j] <- new_19$num[j] + 1 
            new_19$sum_area[j] <- new_19$sum_area[j] + runoff_19$area[i]    
            new_19$sum_total[j] <- new_19$sum_total[j] + runoff_19$total[i]
            new_19$avg_daily[j] <- new_19$avg_daily[j] + runoff_19$daily[i]
            new_19$avg_unused[j] <- new_19$avg_unused[j] + runoff_19$unused[i]
        }
    }
}
tmp<-new_19$sum_area/new_19$num    
new_19$avg_daily <- new_19$avg_daily/tmp    
new_19$avg_unused <- new_19$avg_unused/tmp    

#2) merge data frames
colnames(new_19)<-c("gu","num_B","sum_area", "sum_total_runoff","avg_daily_runoff","avg_daily_unused")    
hydro_19 <- inner_join(energy_use_19, new_19, by="gu")
hydro_19 <- hydro_19[,c(1,3,7,2,6,8:10,5,4)]    
colnames(hydro_19) <- c("gu", "total_area", "area", "total_num_B", "num_B", "total_runoff", "daily_runoff", "daily_unused", "total_E", "heat_E")    
rownames(hydro_19) <- hydro_19$gu

#(3) group runoff by "gu" (2020)

#1) get data : sum total, number, average (daily, unused)
new_20 <- energy_use_18[,(1:2)]  #just for new df
colnames(new_20) <- c("gu", "num")
new_20$num <- 0
new_20$sum_area <- 0  
new_20$sum_total <- 0
new_20$avg_daily <- 0
new_20$avg_unused <- 0

rownames(runoff_20)<-NULL   
#str(runoff_20)     
runoff_20 <- runoff_20[!is.na(runoff_20$area),]   
runoff_20$area<-as.numeric(runoff_20$area)   

for (i in 1:length(runoff_20$gu)){
    for (j in 1:length(gu)){
        if (runoff_20$gu[i]==new_20$gu[j]){
            new_20$num[j] <- new_20$num[j] + 1 
            new_20$sum_area[j] <- new_20$sum_area[j] + runoff_20$area[i]    
            new_20$sum_total[j] <- new_20$sum_total[j] + runoff_20$total[i]
            new_20$avg_daily[j] <- new_20$avg_daily[j] + runoff_20$daily[i]
            new_20$avg_unused[j] <- new_20$avg_unused[j] + runoff_20$unused[i]
        }
    }
}
tmp<-new_20$sum_area/new_20$num    
new_20$avg_daily <- new_20$avg_daily/tmp    
new_20$avg_unused <- new_20$avg_unused/tmp   

#2) merge data frames
colnames(new_20)<-c("gu","num_B","sum_area", "sum_total_runoff","avg_daily_runoff","avg_daily_unused")
hydro_20 <- inner_join(energy_use_20, new_20, by="gu")
hydro_20 <- hydro_20[,c(1,3,7,2,6,8:10,5,4)] 
colnames(hydro_20) <- c("gu", "total_area", "area", "total_num_B", "num_B", "total_runoff", "daily_runoff", "daily_unused", "total_E", "heat_E")
rownames(hydro_20) <- hydro_20$gu


#4. Analysis

#(1)  preprocess
hydro_18$year <- 2018
hydro_19$year <- 2019
hydro_20$year <- 2020
hydro_1819 <- rbind(hydro_18, hydro_19)
hydro_all <- rbind(hydro_1819, hydro_20)

hydro_all$total_area<-as.numeric(gsub(",","",hydro_all$total_area,fixed=TRUE))
hydro_all$total_num_B<-as.numeric(gsub(",","",hydro_all$total_num_B,fixed=TRUE))
hydro_all$total_E<-as.numeric(gsub(",","",hydro_all$total_E,fixed=TRUE))
hydro_all$heat_E<-as.numeric(gsub(",","",hydro_all$heat_E,fixed=TRUE))
hydro_all$year<-as.factor(hydro_all$year)


#(2) Graphs : key = year, group = gu (For reference only)

#1) total runoff (y)  
ggplot(hydro_all, aes(x=year)) + 
    geom_point(aes(y=total_runoff, color=gu)) +
    ggtitle("Total Runoff ~ Gu") + xlab("gu") + ylab("total runoff") + 
    theme_bw() + theme(text=element_text(size=8, face="bold")) 

#2) daily runoff (y)
ggplot(hydro_all, aes(x=year)) + 
    geom_point(aes(y=daily_runoff, color=gu)) +
    ggtitle("Daily Runoff ~ Gu") + xlab("gu") + ylab("daily runoff") + 
    theme_bw() + theme(text=element_text(size=8, face="bold")) 

#3) unused runoff (y)
ggplot(hydro_all, aes(x=year)) + 
    geom_point(aes(y=daily_unused, color=gu)) +
    ggtitle("Unused Runoff ~ Gu") + xlab("gu") + ylab("unused runoff") + 
    theme_bw() + theme(text=element_text(size=8, face="bold")) 

#4) total E (y)
ggplot(hydro_all, aes(x=year)) + 
    geom_point(aes(y=total_E, color=gu)) +
    ggtitle("Total E ~ Gu") + xlab("gu") + ylab("total E") + 
    theme_bw() + theme(text=element_text(size=8, face="bold")) 

#5) heat E (y)
ggplot(hydro_all, aes(x=year)) + 
    geom_point(aes(y=heat_E, color=gu)) +
    ggtitle("Heat E ~ Gu") + xlab("gu") + ylab("heat E") + 
    theme_bw() + theme(text=element_text(size=8, face="bold")) 

#(3) Order (Rank: Pointing)
#runoff: 수열 에너지 자원이 많을 구역 (높을수록 좋다)
#E use: 수열 에너지 수요가 있을 구역 (높을수록 좋다)

#1) key = runoff
#1)) total runoff
tr18 <- hydro_18[order(hydro_18$total_runoff),c(1,11)]
tr18$point <- c(1:length(tr18$gu))
tr19 <- hydro_19[order(hydro_19$total_runoff),c(1,11)]
tr19$point <- c(1:length(tr18$gu))
tr20 <- hydro_20[order(hydro_20$total_runoff),c(1,11)]
tr20$point <- c(1:length(tr18$gu))
tr18<-tr18[,c(1,3)]
colnames(tr18) <- c("gu","2018")
tr19<-tr19[,c(1,3)]
colnames(tr19) <- c("gu","2019")
tr20<-tr20[,c(1,3)]
colnames(tr20) <- c("gu","2020")

#2)) daily runoff
dr18 <- hydro_18[order(hydro_18$daily_runoff),c(1,11)]
dr18$point <- c(1:length(dr18$gu))
dr19 <- hydro_19[order(hydro_19$daily_runoff),c(1,11)]
dr19$point <- c(1:length(dr18$gu))
dr20 <- hydro_20[order(hydro_20$daily_runoff),c(1,11)]
dr20$point <- c(1:length(dr18$gu))
dr18<-dr18[,c(1,3)]
colnames(dr18) <- c("gu","2018")
dr19<-dr19[,c(1,3)]
colnames(dr19) <- c("gu","2019")
dr20<-dr20[,c(1,3)]
colnames(dr20) <- c("gu","2020")

#3)) unused runoff
un18 <- hydro_18[order(hydro_18$daily_unused),c(1,11)]
un18$point <- c(1:length(un18$gu))
un19 <- hydro_19[order(hydro_19$daily_unused),c(1,11)]
un19$point <- c(1:length(un18$gu))
un20 <- hydro_20[order(hydro_20$daily_unused),c(1,11)]
un20$point <- c(1:length(un18$gu))
un18<-un18[,c(1,3)]
colnames(un18) <- c("gu","2018")
un19<-un19[,c(1,3)]
colnames(un19) <- c("gu","2019")
un20<-un20[,c(1,3)]
colnames(un20) <- c("gu","2020")

#4)) merge + order (by gu)
tr <- merge(merge(tr18[order(tr18$gu),],tr19[order(tr19$gu),], by='gu'), tr20[order(tr20$gu),], by='gu')
dr <- merge(merge(dr18[order(dr18$gu),],dr19[order(dr19$gu),], by='gu'), dr20[order(dr20$gu),], by='gu')
un <- merge(merge(un18[order(un18$gu),],un19[order(un19$gu),], by='gu'), un20[order(un20$gu),], by='gu')
tr$point <- tr$`2018` + tr$`2019` + tr$`2020`
dr$point <- dr$`2018` + dr$`2019` + dr$`2020`
un$point <- un$`2018` + un$`2019` + un$`2020`
runoff <- tr[,c(1,5)] #just for making df
runoff$point <- tr$point + dr$point + un$point

#2) key = Energy use
#1)) total E
te18 <- hydro_18[order(hydro_18$total_E),c(1,11)]
te18$`2018` <- c(1:length(te18$gu))
te19 <- hydro_19[order(hydro_19$total_E),c(1,11)]
te19$`2019` <- c(1:length(te19$gu))
te20 <- hydro_20[order(hydro_20$total_E),c(1,11)]
te20$`2020` <- c(1:length(te20$gu))

#2)) heat E
he18 <- hydro_18[order(hydro_18$heat_E),c(1,11)]
he18$`2018` <- c(1:length(he18$gu))
he19 <- hydro_19[order(hydro_19$heat_E),c(1,11)]
he19$`2019` <- c(1:length(he19$gu))
he20 <- hydro_20[order(hydro_20$heat_E),c(1,11)]
he20$`2020` <- c(1:length(he20$gu))

#3)) merge + order (by gu)
te <- merge(merge(te18[order(te18$gu),c(1,3)],te19[order(te19$gu),c(1,3)], by='gu'),te20[order(te20$gu),c(1,3)], by='gu')
he <- merge(merge(he18[order(he18$gu),c(1,3)],he19[order(he19$gu),c(1,3)], by='gu'),he20[order(he20$gu),c(1,3)], by='gu')
te$point <- te$`2018` + te$`2019` + te$`2020`
he$point <- he$`2018` + he$`2019` + he$`2020`
Euse <- tr[,c(1,5)] #just for making df
Euse$point <- te$point + he$point

colnames(runoff)<-c("gu", "runoff")
colnames(Euse)<-c("gu", "Euse")
hydro <- merge(runoff, Euse, by='gu')

#(4) Graphs (final) : x = gu, y = runoff | Euse
#1) runoff
r <- ggplot(data=hydro, aes(x=reorder(gu, runoff), y=runoff)) +
    xlab("gu") + ylab("runoff") +
    theme(text=element_text(size=8, face="bold")) 
rp <- r + geom_point(aes(size=runoff, color=gu)) +
    geom_text(aes(label=runoff), size=2, hjust=0, vjust=1.75 ,  color='black')
rb <- r + geom_hline(yintercept=mean(hydro$runoff), linetype='dashed', size = 0.25, color = "grey60") +
    geom_text(aes(label=runoff), size=2, hjust=-1.25,  color='black') +
    geom_bar(stat='identity', width = 0.3, fill="steelblue", colour="steelblue") +
    coord_flip()

#2) Euse
e <- ggplot(data=hydro, aes(x=reorder(gu, Euse), y=Euse)) +
    xlab("gu") + ylab("Euse") +
    theme(text=element_text(size=8, face="bold"))
ep <- e + geom_point(aes(size=Euse, color=gu)) +
    geom_text(aes(label=Euse), size=2, hjust=0, vjust=1.75 ,  color='black')
eb <- e + geom_hline(yintercept=mean(hydro$Euse), linetype='dashed') +
    geom_text(aes(label=Euse), size=2, hjust=-1.25,  color='black') +
    geom_bar(stat='identity', width = 0.3, fill="steelblue", colour="grey") +
    coord_flip()

#3) runoff + Euse
rp
rb
ep
eb
