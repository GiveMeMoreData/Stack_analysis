options(stringsAsFactors = FALSE)
library('dplyr')
library('stringi')
library('ggplot2')
Posts<-read.csv('Posts.csv')
Users<-read.csv('Users.csv')

#Będziemy patrzeć, czy (i którzy) użytkownicy Stacka z czasem odpowiadają na trudniejsze pytania.

#Jak ustaliliśmy w Experts.R, Score jest dobrą oceną wartości pytania.
#Niestety, tu musimy to dostosować do roku (może się np zdarzyć, że w danym roku nie było
#pytań o wyższym Score niż w poprzednich latach).
#Rozważymy też wariant drugi, bez dostosowania wyników, EDIT: nie wyszło najlepiej. Wracamy do pierwszej koncepcji 
#Dlatego zamiast na sam Score, będziemy patrzeć, w jakim CENTYLU znajduje się dany Score
#wśród Scorów w danym roku. EDIT: koniec końców korzystam z metody poniżej.
#Trudności pytań podlegają rozkładowi Pareto (duużo łatwych pytań, bardzo mało trudnych),
#Więc za dużo gubimy po drodze.
#Alternatywnie możemy każdy Score przemnożyć przez 100/max(ScoreInYear). EDIT:Tak będziemy robić.
#Wtedy w każdym roku maksymalna wartość nowego Score będzie wynosić 100. Taka standaryzacja.
#UWAGA: musimy brać czasy nadsyłania odpowiedzi, a nie czasy zapostowania pytania.
#Ponadto lepiej dla współczynnika Pearsona będzie, jeśli będziemy grupować odpowiedzi po miesiącach.
#Więcej obserwacji - dokładniejszy wynik.

filter(Posts, PostTypeId==1, !is.na(AcceptedAnswerId), !is.na(OwnerUserId),
       !is.na(Score))%>%
  select(Id,AcceptedAnswerId, Title, Tags, CreationDate, Score)%>%
  mutate(QuestionYear=stri_sub(CreationDate, from=1, to=4))%>%
  group_by(QuestionYear)%>%
  mutate(AdjustedScore=Score*100/max(Score))->Questions


FindPercentile<-function(x, vector){
  #Zakładamy, że vector jest wektorem liczb zawierającym x.
  #Funkcja mówi, w jakim centylu wektora vector jest x.
  vector<-sort(vector)
  sapply(x, FUN=function(y)  which(vector==y)[length(which(vector==y))] / length(vector) * 100
)}

#Ponownie, jak u ekspertów, dodajemy odpowiedzi, które nie były AcceptedAnswer,
#ale miały nie mniejszy Score

inner_join(Questions, select(Posts, Id, AcceptedScore=Score),
           by=c("AcceptedAnswerId"="Id"))->AcceptedScores

filter(Posts,PostTypeId==2)%>%
  select(ParentId, AnswerScore=Score, OwnerUserId, 
         AnswerCreationDate=CreationDate)%>%
  inner_join(AcceptedScores, by=c("ParentId"="Id"))%>%
  filter(AnswerScore>=AcceptedScore)%>%
  mutate(AnswerMonth=stri_sub(CreationDate, from=1, to=7))->Results1

#Ponownie chcemy faworyzować wysokie wyniki

select(Results1, OwnerUserId, AnswerMonth, AdjustedScore)%>%
  group_by(OwnerUserId, AnswerMonth)%>%
  summarise(Rating=sum(AdjustedScore**3)**0.33)->Results2

#Opcja 2
select(Results1, OwnerUserId, AnswerMonth, Score)%>%
  group_by(OwnerUserId, AnswerMonth)%>%
  summarise(Rating=sum(Score**3)**0.33)->Results2.1


#Z doświadczenia wiem, że cor.test nie lubi małych próbek. dla mniejszych od 5 zwracał błąd.
#Zresztą interesuje nas progres w czasie, nieprawdaż?

group_by(Results2, OwnerUserId)%>%filter(n()>=6)%>%
  summarise(
  Pearson=cor.test(Rating, 1:length(AnswerMonth))$estimate,
  Kendall=cor.test(Rating, 1:length(AnswerMonth),
                    method = "kendall")$estimate,
  Spearman=cor.test(Rating, 1:length(AnswerMonth),
                    method = "spearman")$estimate)%>%
  arrange(desc(Pearson))->Results_Cor_1

group_by(Results2.1, OwnerUserId)%>%filter(n()>=6)%>%
  summarise(
    Pearson=cor.test(Rating, 1:length(AnswerMonth))$estimate)%>%
  arrange(desc(Pearson))->Results_Cor_1.1

hist(Results_Cor_1$Pearson)
hist(Results_Cor_1$Spearman)
hist(Results_Cor_1$Kendall)

hist(Results_Cor_1.1$Pearson)

#Wyszło... dziwnie? Z jednej strony otrzymane współcznniki idealnie się układają w rozkład normalny,
#co ma sens.
#Ale z drugiej strony to trochę głupieje, gdy ktoś miał kilka odpowiedzi na pytania ciężkie i duuużo odpowiedzi na pytania łatwe.
#Sprawdzimy korelację liniową tylko tych najlepszych. W sumie takie jest nasze zagadnienie - 
#Czy ktoś z czasem odpowiadał na coraz trudniejsze pytania. To jest nasza miara progresu.
#Jak wyznaczyć "najlepsze" dla każdego usera z osobna?
#Może najlepsze 40% obserwacji? 
#Jak tak, to bierzemy stan przed policzeniem Rating.

group_by(Results1, OwnerUserId)%>%
  filter(n()>=6, FindPercentile(AdjustedScore, AdjustedScore)>=60 | n()<=15)%>%
  summarise(
    Pearson=cor.test(AdjustedScore, 1:length(AnswerMonth))$estimate,
    Kendall=cor.test(AdjustedScore, 1:length(AnswerMonth),
                     method = "kendall")$estimate,
    Spearman=cor.test(AdjustedScore, 1:length(AnswerMonth),
                      method = "spearman")$estimate)%>%
  arrange(desc(Pearson))->BestResults_Cor

hist(BestResults_Cor$Pearson)
hist(BestResults_Cor$Spearman)
hist(BestResults_Cor$Kendall)
mean(BestResults_Cor$Pearson)

#Już jest lepiej. Histogramy każdego z współczynników są bardzo podobne i wciąż przypominają rozkład normalny,
#ale te wyniki są bardziej miarodajne.

#Weźmy teraz pod uwagę czas między założeniem konta a pierwszym "osiągnięciem" - zaakceptowaną odpowiedzią.
#Możemy ustawić dla miesięcy z tego przedziału czasowego 0 i udawać, że w tym czasie nasi użytkownicy "uczyli się".
#OK, nie będzie to konieczne. Wystarczy, że dodamy do "osiągnięć" obserwację równą 0 odpowiadającą dacie założenia konta
#i będziemy liczyć współczynnik po liczbie minionych miesięcy, a nie po kolejnych liczbach naturalnych jak debile.
group_by(Results1, OwnerUserId)%>%
  filter(n()>=6, FindPercentile(AdjustedScore, AdjustedScore)>=60 | n()<=15)%>%
inner_join(select(Users, Id, UserCreationDate=CreationDate), by=c("OwnerUserId"="Id"))->Cut_Results  
stri_replace_all_regex(Cut_Results$UserCreationDate, 
                       pattern = "T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}$", 
                       replacement = " 12:00:00")%>%
  stri_datetime_parse()->Cut_Results$UserCreationDate
stri_replace_all_regex(Cut_Results$AnswerCreationDate, 
                       pattern = "T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}$", 
                       replacement = " 12:00:00")%>%
  stri_datetime_parse()->Cut_Results$AnswerCreationDate
mutate(Cut_Results, Latency=as.double(difftime(AnswerCreationDate, UserCreationDate, 
                                               units="days"))/30)%>%
  summarise(Pearson=cor.test(c(0,AdjustedScore), c(0,Latency))$estimate,
            Kendall=cor.test(c(0,AdjustedScore), c(0,Latency),
                             method = "kendall")$estimate,
            Spearman=cor.test(c(0,AdjustedScore), c(0,Latency),
                              method = "spearman")$estimate)%>%
  arrange(desc(Pearson))->BestResults2_Cor




hist(BestResults2_Cor$Pearson)
hist(BestResults2_Cor$Spearman)
hist(BestResults2_Cor$Kendall)




write.csv(Results_Cor_1,"Resoults_Cor.csv",row.names = FALSE)
write.csv(BestResults_Cor,"BestResoults_Cor.csv",row.names = FALSE)
write.csv(BestResults2_Cor,"BestResoults_2Cor.csv",row.names = FALSE)






#Rewelacja. Całe wykresy wyraźnie przesunięte w prawo.

BestResults2_Cor%>%top_n(10)%>%
inner_join(Users, by=c("OwnerUserId"="Id"))->GoodStudents
stri_replace_all_regex(GoodStudents$CreationDate, 
                       pattern = "T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}$", 
                       replacement = " 12:00:00")%>%
  stri_datetime_parse()->GoodStudents$CreationDate
stri_replace_all_regex(GoodStudents$LastAccessDate, 
                       pattern = "T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}$", 
                       replacement = " 12:00:00")%>%
  stri_datetime_parse()->GoodStudents$LastAccessDate
mutate(GoodStudents, 
       ActivityTime=difftime(LastAccessDate,
                             CreationDate,
                             units="weeks"))%>%
  select(Pearson, Reputation, ActivityTime, 
         DisplayName, AboutMe, Location)->GoodStudents2

#Wykresiki trudności obalonych pytań od czasu. Możemy wziąć wszystkie pytania: 

for(i in GoodStudents$OwnerUserId){
  filter(Results1, OwnerUserId==i)->x
  plot(as.integer(as.factor(x$AnswerMonth)), 
       x$AdjustedScore,
       xlab="AnswerMonth",
       main=as.character(GoodStudents[GoodStudents$OwnerUserId==i,
                                      "DisplayName"]))
}

#Lub tylko "osiągnięcia".

for(i in GoodStudents$OwnerUserId){
  filter(Cut_Results, OwnerUserId==i)->x
  plot(x$AnswerCreationDate, 
       x$AdjustedScore,
       main=as.character(GoodStudents[GoodStudents$OwnerUserId==i,
                                      "DisplayName"]),
       sub = "Achievements")
}










#Poligon. Nie wchodzić, nie pytać

#Potrzebne do liczenia centyli. EDIT: da się szybciej w dplyr.:
sapply(unique(Questions$QuestionYear), FUN=function(y){
  filter(Questions, QuestionYear==y)%>%select(Score)%>%
    arrange(Score)%>%unlist()
})->ScoresByYear


hist(Cut_Results_Cor$Pearson)
hist(Cut_Results_Cor_2$Pearson)
hist(Cut_Results_Cor_3$Pearson)


inner_join(Results2, Results_Cor)->Results4
group_by(Results4, OwnerUserId)%>%mutate(Best=max(Rating))%>%
  arrange(desc(Best))%>%View()

inner_join(Users, by=c("OwnerUserId"="Id"))%>%
  select(OwnerUserId, Title, Tags, QuestionScore, Reputation, DisplayName)->RawValued

