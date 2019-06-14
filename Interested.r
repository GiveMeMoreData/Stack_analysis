options(stringsAsFactors = FALSE)
library('dplyr')
library('stringi')
library(ggplot2)
Users<-read.csv('Users.csv')


select(Users, CreationDate, LastAccessDate)%>%filter(!is.na(CreationDate))->df1


stri_replace_all_regex(df1$CreationDate, pattern = "\\.[0-9]{3}", replacement = "")%>%
  stri_replace_all(replacement = " ", fixed="T")%>%
  stri_datetime_parse()->FormattedCreationDate


stri_replace_all_regex(df1$LastAccessDate, pattern = "\\.[0-9]{3}", replacement = "")%>%
  stri_replace_all(replacement = " ", fixed="T")%>%
  stri_datetime_parse()->FormattedLastAccessDate


min(FormattedCreationDate, na.rm=TRUE)->start
max(FormattedLastAccessDate, na.rm = TRUE)->ending


monthFloor<-function(x){
  #Funkcja zmienia w dacie: dzień na 1, godzinę na 12:00:00
  stri_datetime_create(year=stri_datetime_fields(x)$Year, 
                       month = stri_datetime_fields(x)$Month,
                       day=1)
}


monthFloor(start)->cut_start

monthFloor(ending)%>%stri_datetime_add(units = "months")->cut_ending
seq(cut_start, cut_ending, by="month")->checkPoints

stri_datetime_add(FormattedLastAccessDate,value = 3,
                  units = "months")->ExpireDates


sapply(checkPoints, FUN=function(x){
  sum(FormattedCreationDate<=x & 
        ExpireDates>= x, na.rm = TRUE)
})->InterestedPerMonth

plot(checkPoints, InterestedPerMonth, type = "l", las=2 ,main="Gaming",col="#54aee5")


ggplot()+
  geom_line(aes(x=checkPoints,y=InterestedPerMonth),colour="#54aee5",size=3,show.legend = FALSE)+
  labs(y="Active since x",x="Date")+
  theme_minimal()

checkPoints[which.max(InterestedPerMonth)]


sapply(checkPoints, FUN=function(x){
  sum(FormattedCreationDate>=x & 
        FormattedCreationDate < stri_datetime_add(x, units = "months"), na.rm = TRUE)
})->NewUsersPerMonth

plot(checkPoints, NewUsersPerMonth, type = "l", las=2)


ggplot()+
  geom_line(aes(x=checkPoints,y=NewUsersPerMonth),colour="#54aee5",size=3,show.legend = FALSE)+
  labs(y="New users",x="Date")+
  theme_minimal()


(ExpireDates-FormattedCreationDate)%>%as.double()/365->YearsOfActivity

hist(YearsOfActivity, right=FALSE, las=1)


?geom_histogram
ggplot()+
  geom_histogram(aes(YearsOfActivity),fill="#54aee5",col="#FFFFFF",size=0.5,show.legend = FALSE,bins=25)+
  labs(y="Quantity",x="Number of years")+
  theme_minimal()
