library(runjags)
##parameters##
n.subjects <- 200
P <- .6 #probability 
prime <- 170
noise.sd <- 5
song.bank.mean <- 120
song.bank.sd <- 15
##############

pick.song <- function(songs, prime.song){ #function that generates the tempo for one subject
  songs <- rnorm(1,song.bank.mean,song.bank.sd) #tempo in head without prime
  prime.song <- prime #primed tempo
  noise <- rnorm(1,0,noise.sd)
  decision <- runif(1,0,1)
  if(decision <= P){
    return(prime.song + noise)}
  else{
    return(songs)}
}
Subject = 1:n.subjects
Prime.tempo = rep(prime, n.subjects)
Tempo = sapply(1:n.subjects, pick.song)

jags.data <- list(
  tempo.produced=Tempo,
  primed.tempo=Prime.tempo,
  subjects=n.subjects
)

result <- run.jags('Preferred Tempo Model.txt', monitor=c('theta'), data=jags.data, n.chains=3, burnin=1000, sample=10000)
plot(result)
summary(result)

library(ggplot2)
plot.data <- data.frame(Subject, Prime.tempo, Tempo)

ggplot(plot.data, aes(x=Subject, y=Tempo))+
  geom_point()
  