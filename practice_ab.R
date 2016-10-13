x=sample(c(-5,5),size = 50,replace = TRUE)
sum(x)
cumsum(x)
#print it as four different plots
par(mfrow=c(2,2))
for (j in 1:4){
  x=sample(c(-5,5),size=50,replace=TRUE)
  plot(cumsum(x),type="l",ylim=c(-50,50))
  abline(h=0)
         
}

#this can be put as a function
peterpaul=function(n=50){
  
  x=sample(c(-5,5),size=n,replace=TRUE)
  sum(x)
}

peterpaul(100)

y=replicate(1000,peterpaul(100))
table(y)

par(mfrow=c(1,1))
plot(table(y))
# chances of breaking even can also be computed very accurately through binomial distribution
#getting exactly 25 heads in a row of 50 trials
dbinom(25,50,0.5)

#prob of getting atleast 25 heads
sum(dbinom(0:25,50,0.5))

# we find total fortune, sum(x), times when peter is in lead, and max cum sum can also be calculated
peterpaul=function(n=50){
  x=sample(c(-5,5),size=n,replace=TRUE)
  c(F=sum(x),L=sum(cumsum(x)>0),M=max(cumsum(x)))
}

peterpaul(100)
#replicate for 1000 times

n=replicate(1000,peterpaul())
head(n)
#how many times was Peter in lead

times_lead=n["L",]
plot(prop.table(table(times_lead)))

#Peter's maximum winning saved in M variable
m=n["M",]
plot(prop.table(table(m)))
