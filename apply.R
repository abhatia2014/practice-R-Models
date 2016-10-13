#creating this on github as well
#using apply functions

#1. apply works on arrays or matrices

#syntax is apply(arrayname,MARGIN, FUN,...)
# where MARGIN=1 applies over rows, MARGIN=2 applies over columns, MARGIN= c(1,2) applies over both rows and columns

#example
x=matrix(rnorm(30),nrow=5,ncol=6)
x
# if we want to sum the values in each column

apply(x,2,sum)
# or sum the values in each rows
apply(x,1,sum)

#using lapply in R

#when we wish to apply the function to every element of a list

#it can be used for other objects like dataframe, lists or vectors

#create a list of matrices

A<-matrix(1:9, 3,3)
B<-matrix(4:15, 4,3)
C<-matrix(8:10, 3,2)
mylist=list(A,B,C)
mylist

#extract the second column from the list of matrices using the operator '[]

lapply(mylist,"[",,2)

#we use the [ selection operator and have 2 comma to indicate we omit the first parameter which indicates 'any'

#to extract the first row from the list of matrices

lapply(mylist,"[",1,)

#extract the second row

lapply(mylist,"[",2,)
#to extract the first row and second column (single element from each row)

lapply(mylist,"[",1,2)

#using sapply

#it works as lapply but it tries to simplify the output to the 
#most elementary data structure as possible

#extracting 2nd row, 1 column
lapply(mylist,"[",2,1)
sapply(mylist,"[",2,1)
#while lapply gives a list, sapply gives a vector
#if we give simplify=FALSE to sapply, it will give it out as a list

#conversely function unlist can tell lapply to give us a vector

unlist(lapply(mylist,"[",2,1))

#using mapply

#multivariate apply- to be able to vectorize arguments to a function that does not accept vectors as arguments

#mapply applies a function to multiple lists or multiple vector arguments


# Apply R Exercises -------------------------------------------------------

# http://r-exercises.com/2016/09/08/efficient-processing-with-apply-exercises/

dataset1 <- cbind(observationA = 16:8, observationB = c(20:19, 6:12))

#Exercise 1 - find rowmeans of dataset

mean.m=apply(dataset1,1,mean)

# Exercise 2 column sums of datasets

mean.c=apply(dataset1,2,sum)

# Exercise 3 sort columns of a dataset
sort.c=apply(dataset1,2,sort)

# Exercise 4 find product of dataset rows

product.r=apply(dataset1,1,prod)

#Exercise 5 apply the function to all rows of the dataset

DerivativeFunction <- function(x) { log10(x) + 1 }

apply.fun=apply(dataset1,1,DerivativeFunction)

#Exercise 6 define the Derevative function inside the apply function

apply.fun2=apply(dataset1,1,function(x) {log10(x)+1})

#Exercise 7 round the output of previous to 2 places

apply.fun3=apply(dataset1,1,function(x) {round(log10(x)+1,2)})

#Exercise 8 print columns 

print.c=apply(dataset1,2,print)

#Exercise 9 find length of columns

length.c=apply(dataset1,2,length)

#Exercise 10 find range of numbers

range.c=apply(dataset1,2,range)
