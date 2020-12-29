library("rvest")
library('ggplot2')

Rates <- function(BaseURL, BegDate, EndDate, Curr){
  bd <- as.Date(BegDate)
  ed <- as.Date(EndDate)
  vDate <- seq.Date(bd,ed,1)
  Currency <- toupper(Curr)
  fCourse = NULL
  
  len <- length(vDate)
  for (dd in 1:len) {
    locURL <- paste0(BaseURL,vDate[dd])
    docSource <- read_html(locURL)
    table <- html_table(docSource)
    tab <- table[[1]]
    fCourse <- rbind(fCourse,cbind(subset(tab,tab[2]==Currency),
                                   dDate=vDate[dd]))
    Sys.sleep(3)
  }
  fCourse
}

currentDate <- as.Date('2020-12-29')
usd <- Rates('http://cbr.ru/currency_base/daily.aspx?date_req=',as.character(currentDate-7),as.character(currentDate),'usd')
eur<-Rates('http://cbr.ru/currency_base/daily.aspx?date_req=',as.character(currentDate-7),as.character(currentDate),'eur')
dkk<-Rates('http://cbr.ru/currency_base/daily.aspx?date_req=',as.character(currentDate-7),as.character(currentDate),'dkk')


moneyChange <- data.frame(dates=usd$dDate,usd=usd$Rate , eur=eur$Rate, dkk= dkk$Rate)

ggplot(moneyChange, aes(dates)) + 
  geom_point(aes(y=moneyChange$usd, colour = 'Dollar')) + 
  geom_point(aes(y=moneyChange$eur, colour = 'Euro'))+ 
  geom_point(aes(y=moneyChange$dkk, colour = 'Danish Krone')) +
  scale_color_manual(name = "Currencies", 
                     values = c("Dollar" = "blue", "Euro" = "red", 'Danish Krone' = 'green')) +
  labs(title = "Currency dates:", 
       x = "Date", y = "Rate")


