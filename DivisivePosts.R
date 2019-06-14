options(stringsAsFactors = FALSE)
library('dplyr')
Posts<-read.csv('Posts.csv')
Votes<-read.csv('Votes.csv')

#Liczymy w tabelce Votes, dla jakich postów ilość
#DownVotes i Upvotes była w przybliżeniu taka sama.

filter(Votes, VoteTypeId==2 | VoteTypeId==3)%>%
  select(PostId, VoteTypeId)%>%group_by(PostId)%>%
  summarise(UpVotes=sum(VoteTypeId==2),
            DownVotes=sum(VoteTypeId==3),
            AllVotes=n(),
            Ratio=UpVotes/AllVotes)%>%
filter(Ratio >= 0.4 & 
         Ratio <= 0.5)%>%arrange(desc(AllVotes))->SummarisedVotes
inner_join(SummarisedVotes, Posts, by=c("PostId"="Id"))%>%
  select(Title, Tags, Ratio, AllVotes)->DivisivePosts
