options(stringsAsFactors = FALSE)
library('dplyr')
library('stringi')
Tags<-read.csv('Tags.csv')
Posts<-read.csv("Posts.csv")

arrange(Tags, desc(Count))%>%select(TagName)->TagNames

write.csv(TagNames,"TagNames.csv")

head(TagNames)->PopularTags

#Zamierzamy porównać popularność określonych tagów w kolejnych miesiącach.
filter(Posts, PostTypeId==1)%>%select(CreationDate)%>%unlist()%>%
  stri_replace_all_regex(pattern = "[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}$", 
                         replacement = "01 12:00:00")%>%
  stri_datetime_parse()->AllPostDates


seq(min(AllPostDates, na.rm = TRUE), max(AllPostDates, na.rm = TRUE), by="month")->checkPoints

sapply(checkPoints, FUN = function(x){
  sum(AllPostDates>=x & AllPostDates < stri_datetime_add(x, units = "months"), na.rm = TRUE)
})->AllPostsPerMonth

write.csv(checkPoints,"checkPoints.csv",row.names = FALSE)



sapply(unlist(TagNames), FUN=function(s){
  print(which(TagNames==s)/length(TagNames))
  string<-paste("<", s, ">", sep="")
  filter(Posts, stri_detect_fixed(Tags, pattern = string))%>%
    select(CreationDate)%>%unlist()%>%
    stri_replace_all_regex(pattern = "\\.[0-9]{3}", replacement = "")%>%
    stri_replace_all(replacement = " ", fixed="T")%>%
    stri_datetime_parse()->Dates
  sapply(checkPoints, FUN = function(x){
    sum(Dates>=x & Dates < stri_datetime_add(x, units = "months"), na.rm = TRUE)
  })
})%>%as.data.frame()->PostsPerMonth


## my
copy <- PostsPerMonth
##

colnames(PostsPerMonth)<-TagNames$TagName
rownames(PostsPerMonth)<-stri_datetime_format(checkPoints, format = "uuuu-MM")
apply(PostsPerMonth, 2, sum)->sums
which(sums==0)->Zeroes
PostsPerMonth[,-Zeroes]->PostsPerMonth


write.csv(PostsPerMonth, "PostsPerMonth.csv",row.names = FALSE)
read.csv("PostsPerMonth.csv")->PostsPerMonth


rownames(PostsPerMonth)<-PostsPerMonth$X
PostsPerMonth[,-1]->PostsPerMonth



apply(PostsPerMonth,MARGIN = 2, FUN=function(x){
  #Liczymy korelację od miesiąca, w którym mamy pierszą niezerową obserwację.
  cor.test(x, 
           AllPostsPerMonth,
           method = "pearson")$estimate
})%>%as.data.frame()%>%
mutate(TagName=TagNames$TagName)%>%
  arrange(desc(.))->CorTestsPearson


write.csv(CorTestsPearson,"CorTestPearson.csv")

for(i in 1:5){
  #Rysujemy wykresy "klasyków".
  j<-which(TagNames==CorTestsPearson$TagName[i])
  plot(checkPoints, PostsPerMonth[[j]],type = "l", 
       main=colnames(PostsPerMonth)[j], las=2,
       ylab = "PostsCount")
  bestIndex<-which.max(PostsPerMonth[[j]])
  text(x=checkPoints[bestIndex], 
       y=PostsPerMonth[[j]][bestIndex], 
       stri_datetime_format(checkPoints[bestIndex], 
                            format = "uuuu-MM"))
}

#Drążymy dalej. Chcemy klasycznych gier, a nie "technical issues".
#Wyniki: minecraft, dota-2, dark-souls.
classicGames<-c("minecraft","dota.2","dark.souls")
for(s in classicGames){
  plot(checkPoints, PostsPerMonth[[s]],type = "l", 
       main=s, las=2,
       ylab = "PostsCount",
       xlab = "")
  bestIndex<-which.max(PostsPerMonth[[s]])
  text(x=checkPoints[bestIndex], 
       y=PostsPerMonth[[s]][bestIndex], 
       stri_datetime_format(checkPoints[bestIndex], 
                            format = "uuuu-MM"))
}

for(i in 1:5){
  #Rysujemy wykresy najpopularniejszych
  plot(checkPoints, PostsPerMonth[[i]],type = "l", 
       main=colnames(PostsPerMonth)[i], las=2,
       ylab = "PostsCount")
  bestIndex<-which.max(PostsPerMonth[[i]])
  text(x=checkPoints[bestIndex], 
       y=PostsPerMonth[[i]][bestIndex], 
       stri_datetime_format(checkPoints[bestIndex], 
                            format = "uuuu-MM"))
}
#Podsumowanie. Najlepszy miesiąc danego taga pod względem ilości użyć
data.frame(bestMonth=apply(PostsPerMonth,
                 MARGIN=2, FUN=function(x){
                   rownames(PostsPerMonth)[which.max(x)]
                 }),
           bestCount=apply(PostsPerMonth, 
                 MARGIN=2, FUN=max))->Summary



