install.packages('rvest')
library(rvest)
library(dplyr)
library(ggplot2)



filmweb<-read_html("http://www.filmweb.pl/rankings/film/world")


filmweb%>%
  html_nodes(".vertical-align")%>%
  html_text()->oceny

length(oceny)
oceny2<-c(1:length(oceny))

for(i in 1:length(oceny)){
  oceny2[i]<-oceny[2*i]
}
  
oceny<-oceny2[!is.na(oceny2)]


oceny<-scan(text=oceny, dec=",", sep=".")

oceny<-as.numeric(oceny)



filmweb%>%
  html_nodes(".s-20")%>%
  html_text->titles


titles<-as.data.frame(titles)

titles%>%mutate(titles=replace(titles, titles=="", NA))%>%na.omit->titles

titles$rank<-c(1:nrow(titles))
  
oceny<-as.data.frame(oceny)
  
oceny$rank<-c(1:nrow(titles))


ranking<-left_join(titles, oceny, by="rank")



glosy<-filmweb%>%html_nodes(".top-3")%>%html_text

class(glosy)


a<-strsplit(glosy, split = " głosów")
a<-as.character(a)

a<-strsplit(a, split=" głosy")
a<-gsub(" ", "", a, fixed = TRUE)
a<-as.numeric(a)
a<-as.data.frame(a)
a$rank<-c(1:500)


ranking<-left_join(ranking, a, by="rank")



ranking<-ranking%>%mutate(glosy=a)%>%select(-a)








ggplot(data=ranking, aes(x=rank, y=glosy))+geom_point(aes(color=rank))+
  geom_smooth()
  

