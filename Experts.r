options(stringsAsFactors = FALSE)
library('dplyr')
library('stringi')
Posts<-read.csv('Posts.csv')
Comments<-read.csv('Comments.csv')
Users<-read.csv('Users.csv')

#Część pierwsza. Kreatywne/złożone pytania - najwięcej komentarzy i odpowiedzi
filter(Posts, PostTypeId==1)%>%
  mutate(ReactionCount=AnswerCount+CommentCount)%>%
  select(Id, Title, ReactionCount, AcceptedAnswerId, Score, Tags)%>%
  arrange(desc(ReactionCount, Score))->CreativeQuestions

cor.test(CreativeQuestions$ReactionCount, 
         CreativeQuestions$Score, 
         method = "spearman")->Spearman_ReactionToScore

#Wyszło 0.19. Prawie brak korelacji.

#Pierwsza opcja. ReactionCount będzie bezpośrednią miarą kreatywności odpowiedzi.
#Druga opcja: robimy nasz własny, autorski CreativeRating.
#Będzie to Score odpowiedzi pomnożony przez wartość ReactionCount?
#Niestety, wartości ReactionCount się nie różnią zbytnio od siebie.
#Maksymalna to 45.
#Zobaczymy późńiej.

#Tymczasem: Praca domowa nr 1 uczy, że AcceptedAnswer nie zawsze ma najwyższy Score.
inner_join(select(CreativeQuestions, -Score), select(Posts, Id, AcceptedScore=Score),
           by=c("AcceptedAnswerId"="Id"))->AcceptedScores
#AcceptedScores[AcceptedScores$AcceptedScore==0,]<-1
filter(Posts,PostTypeId==2)%>%select(ParentId, AnswerScore=Score, OwnerUserId)%>%
  inner_join(AcceptedScores, by=c("ParentId"="Id"))%>%
  filter(AnswerScore>=AcceptedScore)%>%
inner_join(Users, by=c("OwnerUserId"="Id"))%>%
  select(OwnerUserId, Title, Tags, ReactionCount,Reputation,DisplayName)->RawCreatives

#Oczywiście możliwe, że ten sam użytkownik odpowiedział na kilka złożonych pytań.
#Musimy jakoś uwzględnić. Niestety, bywa tak, że użytkownicy odpowiadają na sporo "niełożonych" pytań
#i 0 złożonych, więc zwykłe sumowanie wartości ReactionCount nas nie zadowala. 
#Chcemy promować odpowiadających na złożone pytania.
#Moja propozycja: policzyć sumę czwartych potęg i wyciągnąć pierwiastek czwartego stopnia.
#W ten sposób niskie wyniki giną wśród pojedynczych wyników wysokich.
#Jak jakiś user miał tylko jedną odpowiedź, to nie robi różnicy.
#Być może należy przyjrzeć bliżej, jak wartość branego wykładnika (tu:4) wpływa na korelację z reputacją.
#Nie może być za mały, bo wtedy zyskują ci, którzy udzielili dużo odpowiedzi na niezłożone pytania.
#Nie może być za duży, bo wtedy karzemy osoby, które udzieliły kilka odpowiedzi na złożone pytania.

select(RawCreatives, OwnerUserId, ReactionCount)%>%
group_by(OwnerUserId)%>%summarise(Rating=sum(ReactionCount**4)**0.25)%>%
  inner_join(RawCreatives)->FullResults
select(FullResults, Rating, Reputation)%>%unique()%>%arrange(desc(Rating))->CreativeToCompare

#Interlude. Szukamy dobrego wykładnika
sapply(seq(1.5, 5, by=0.5), FUN=function(x){
select(RawCreatives, OwnerUserId, ReactionCount)%>%
  group_by(OwnerUserId)%>%summarise(Rating=sum(ReactionCount**x)**(1/x))%>%
  inner_join(RawCreatives)%>%
select(Rating, Reputation)%>%unique()%>%arrange(desc(Rating))->CreativeToCompare
  cor.test(CreativeToCompare$Rating, CreativeToCompare$Reputation, 
           method="spearman")$estimate
})->TestsByExponent
barplot(TestsByExponent)
#Wygląda na to, że Reputation po prostu faworyzuje tych, co się dużo udzielają,
#Niekoniecznie tych, którzy odpowiadają na "złożone" pytania.


#Część druga. "Trudne pytania" - najdłużej czasu między postem pytania a zaakceptowaną odpowiedzią
filter(Posts, PostTypeId==1)%>%
  select(QId=Id, AcceptedAnswerId, QuestionDate=CreationDate, 
         QTitle=Title, QTags=Tags)%>%
  inner_join(filter(Posts, PostTypeId==2), by=c("AcceptedAnswerId"="Id"))%>%
  select(QId, Score, Title=QTitle, Tags=QTags,QuestionDate, 
         AnswerDate=CreationDate, AcceptedAnswerId)->Dates
stri_replace_all_regex(Dates$QuestionDate, pattern = "\\.[0-9]{3}", replacement = "")%>%
  stri_replace_all(replacement = " ", fixed="T")%>%
  stri_datetime_parse()->Dates$QuestionDate
stri_replace_all_regex(Dates$AnswerDate, pattern = "\\.[0-9]{3}", replacement = "")%>%
  stri_replace_all(replacement = " ", fixed="T")%>%
  stri_datetime_parse()->Dates$AnswerDate
mutate(Dates, LatencyInDays=as.double(difftime(AnswerDate, QuestionDate, units="days")))%>%
         arrange(desc(LatencyInDays))->HardQuestions
#Ponownie, jak wcześniej, szukamy odpowiedzi nieuwzględnionych
inner_join(select(HardQuestions, -Score, -QuestionDate, -AnswerDate), 
           select(Posts, Id, AcceptedScore=Score),
           by=c("AcceptedAnswerId"="Id"))->AcceptedScores
filter(Posts,PostTypeId==2)%>%select(ParentId, AnswerScore=Score, OwnerUserId)%>%
  inner_join(AcceptedScores, by=c("ParentId"="QId"))%>%
  filter(AnswerScore>=AcceptedScore)%>%
  inner_join(Users, by=c("OwnerUserId"="Id"))%>%
  select(OwnerUserId, Title, Tags, LatencyInDays,Reputation,
         DisplayName)->RawPatients

#Podobnie, jak wyżej, wyliczamy Rating jako pierwiastek sumy kwadratów.
#Różnica w wykładniku jest spowodowana różnicą w wartościach danych
#(wyżej były liczby z przedziału 0-45, tu 0- ~300)

select(RawPatients, OwnerUserId, LatencyInDays)%>%
  group_by(OwnerUserId)%>%summarise(Rating=sum(LatencyInDays**2)**0.5)%>%
  inner_join(RawPatients)->FullPatientResults
select(FullPatientResults, Rating, Reputation)%>%
  unique()%>%arrange(desc(Rating))->PatientToCompare

#Część 3. Czas istnienia konta.
#Z powodów, o których mówię w issue na Gicie, interesować nas będą tylko dni.
select(Users, Reputation, CreationDate, LastAccessDate)->Users2
stri_replace_all_regex(Users2$CreationDate, 
                       pattern = "T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}$", 
                       replacement = " 12:00:00")%>%
  stri_datetime_parse()->Users2$CreationDate
stri_replace_all_regex(Users2$LastAccessDate, 
                       pattern = "T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}", 
                       replacement = " 12:00:00")%>%
  stri_datetime_parse()->Users2$LastAccessDate
mutate(Users2, ActivityTime=as.double(difftime(LastAccessDate, CreationDate, 
                                               units="days")))%>%
  arrange(desc(ActivityTime))->LongestActive

#Część czwarta. 
#Pozwólmy społeczności wybrać wartościowe pytania. Ekspert powinien wychodzić im naprzeciw.
#W gruncie rzeczy będzie to podobne do części pierwszej, tylko zamiast ReactionCount będziemy brać po prostu Score.

filter(Posts, PostTypeId==1)%>%
  select(Id,AcceptedAnswerId, Title, Tags, QuestionScore=Score)%>%
  arrange(desc(QuestionScore))->ValuedQuestions

inner_join(ValuedQuestions, select(Posts, Id, AcceptedScore=Score),
           by=c("AcceptedAnswerId"="Id"))->AcceptedScores

filter(Posts,PostTypeId==2)%>%select(ParentId, AnswerScore=Score, OwnerUserId)%>%
  inner_join(AcceptedScores, by=c("ParentId"="Id"))%>%
  filter(AnswerScore>=AcceptedScore)%>%
  inner_join(Users, by=c("OwnerUserId"="Id"))%>%
  select(OwnerUserId, Title, Tags, QuestionScore, Reputation, DisplayName)->RawValued

select(RawValued, OwnerUserId, QuestionScore)%>%
  group_by(OwnerUserId)%>%summarise(Rating=sum(QuestionScore**2)**0.5)%>%
  inner_join(RawCreatives)->FullValuedResults
select(FullValuedResults, Rating, Reputation)%>%unique()%>%
  arrange(desc(Rating))->ValuedToCompare

#Ponownie możemy się pobawić w znajdowanie dobrego wykładnika.
sapply(seq(1.5, 5, by=0.25), FUN=function(x){
  select(RawValued, OwnerUserId, QuestionScore)%>%
    group_by(OwnerUserId)%>%summarise(Rating=sum(QuestionScore**x)**(1/x))%>%
    inner_join(RawCreatives)%>%
    select(Rating, Reputation)%>%unique()%>%arrange(desc(Rating))->CreativeToCompare
  cor.test(CreativeToCompare$Rating, CreativeToCompare$Reputation, 
           method="spearman")$estimate
})->ValuedTestsByExponent
names(ValuedTestsByExponent)<-as.character(seq(1.5, 5, by=0.25))
barplot(ValuedTestsByExponent)

#Finał. Testy korelacji i wykresiki
cor.test(CreativeToCompare$Rating, CreativeToCompare$Reputation, 
         method="spearman")->Spearman_Creative
plot(CreativeToCompare$Rating, 
     CreativeToCompare$Reputation,
     xlab="Rating",
     ylab="Reputation")
#Wynik: 0.38. Nie jest źle.

cor.test(PatientToCompare$Rating, PatientToCompare$Reputation, 
         method="spearman")->Spearman_Patient
plot(PatientToCompare$Rating, 
     PatientToCompare$Reputation,
     xlab="Rating",
     ylab="Reputation")
#Wynik: 0.18 . Nie urywa.

cor.test(LongestActive$ActivityTime, 
         LongestActive$Reputation,
         method = "spearman")->Spearman_Longest_Active

plot(LongestActive$ActivityTime, 
     LongestActive$Reputation,
     xlab="ActivityTime",
     ylab="Reputation")

#Trafiony.
cor.test(ValuedToCompare$Rating, 
         ValuedToCompare$Reputation,
         method = "spearman")->Spearman_Valued

plot(ValuedToCompare$Rating, 
     ValuedToCompare$Reputation,
     xlab="Rating",
     ylab="Reputation")
#0.64. Jest OK.

#Poligon. Nie wnikać, nie zaglądać
Linearize<-function(x){
  #Funkcja przerabia wektor liczb tak, by najwyższa była 10, a najniższa - 0
  x<-as.double(x)
  a<-min(x)
  x<-x-a
  x*(10/max(x))
}
x<-LongestActive$ActivityTime

is.na(NewCreationDate)%>%sum()
is.na(Users2$CreationDate)%>%sum()
filter(Users, is.na(NewCreationDate))%>%View()
filter(Users, is.na(Users2$CreationDate), is.na(NewCreationDate))%>%View()