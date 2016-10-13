getwd()
traindata=read.csv("train.csv",header=TRUE,stringsAsFactors=FALSE)
str(traindata)
testdata=read.csv("test.csv",header=TRUE,stringsAsFactors=FALSE)
head(traindata)
#make basic data visualizations- x-y plots
attach(traindata)
plot(density(Age,na.rm=TRUE))
boxplot(Age, na.rm=TRUE,range=0, horizontal=TRUE)
names(traindata)
table(Survived)
##Trevor Stephens- Kaggle competition

boxplot(Age~Survived,range=0, na.rm=TRUE)
plot(density(Fare,na.rm=TRUE))
boxplot(Fare~Survived,na.rm=TRUE,range=0)
table(Pclass)
Uclass=subset(traindata,Pclass=="1")
Uclass
boxplot(Uclass$Age~Uclass$Survived, na.rm=TRUE,range=0)
sur=subset(traindata,Survived=="1")
boxplot(sur$Pclass,na.rm=TRUE, range=0)
boxplot(sur$Fare~sur$Pclass, na.rm=TRUE, range=0)
plot(table(sur$Pclass))
table(sur$Pclass)
nsur=subset(traindata,Survived=="0")
table(nsur$Pclass)
plot(table(nsur$Pclass),na.rm=TRUE)
#survival rate by sex- not survived
plot(table(nsur$Sex),na.rm=TRUE)
# % men and women not survived
a=table(nsur$Sex)/length(nsur$Sex)
b=table(sur$Sex)/length(sur$Sex)
survival= cbind(a,b)
colnames(survival)=c("Not Survived","Survived")
survival
barplot(survival)
counts=table(Survived, Sex)
counts
barplot(counts,xlab="Gender",ylab="# of People",main="survived and deceased male and females")
table(sur$Sex)
table(sur$Sex)/length(sur$Sex)
#survival rate by passenger class barplot
survivalclass=table(Survived,Pclass)
survivalclass
barplot(survivalclass,xlab="cabin class",ylab="# people",main="survived and deceased")
# survival rate 
a=table(Pclass,Survived)
class1=a[4]/(a[1]+a[4]);class1
class2=a[5]/(a[2]+a[5]);class2
class3=a[6]/(a[3]+a[6]);class3
summary(Age)
# we remove the data which is not needed in the dataset
traindata1=traindata[-c(1,9:12)]
str(traindata1)
head(traindata)
summary(traindata)
# we have to replace qualitative variables like gender (Male, Female) with quantitative variable
head(traindata1)
table(Sex)
# use gsub() function to replace any text with a value of our chosing
#replacing gender variables with dummy variable(0/1)
attach(traindata1)
Sex=gsub("female",replacement = 1,Sex)
table(Sex)
Sex=gsub("^male",replacement=0,Sex)
table(Sex)
# missing age values- evaluation
traindata1$Name[1:20]
# use the grep() function to give a vector of row numbers with a specified surname
master_v=grep("Master.",traindata1$Name,fixed = TRUE)
length(master_v)
master_V
miss_v=grep("Miss.",traindata1$Name,fixed=TRUE)
length(miss_v)
mrs_v=grep("Mrs.",traindata1$Name,fixed=TRUE)
mr_v=grep("Mr.",traindata1$Name,fixed=TRUE)
dr_v=grep("Dr.",traindata1$Name,fixed=TRUE)
length(master_v)+length(miss_v)+length(mrs_v)+length(mr_v)+length(dr_v)
# we rename each name with a shortened name
for (i in master_v) {
  traindata1$Name[i]="Master"
}
for (i in miss_v) {
  traindata1$Name[i]="Miss"
}
for (i in mrs_v) {
  traindata1$Name[i]="Mrs"
}
for (i in mr_v) {
  traindata1$Name[i]="Mr"
}
for (i in dr_v) {
    traindata1$Name[i]="Dr"
}
table(traindata1$Name)
head(traindata1$Name)
# we replace the missing ages with the title group averages
master_avg=round(mean(traindata1$Age[traindata1$Name=="Master"],na.rm=TRUE),digits = 2)
master_avg
miss_avg=round(mean(traindata1$Age[traindata1$Name=="Miss"],na.rm=TRUE),digits = 2)
miss_avg
mr_avg=round(mean(traindata1$Age[traindata1$Name=="Mr"],na.rm=TRUE),digits = 2)
mr_avg
mrs_avg=round(mean(traindata1$Age[traindata1$Name=="Mrs"],na.rm=TRUE),digits = 2)
dr_avg=round(mean(traindata1$Age[traindata1$Name=="Dr"],na.rm=TRUE),digits = 2)
# go through a loop through all names and replace age if NA with the average age of the group
attach(traindata1)
for (i in 1:nrow(traindata1)){
  if (is.na(traindata1[i,5])){
    if(Name[i]=="Master"){
      Age[i]=master_avg
      }
    else if (Name[i]=="Miss"){
      Age[i]=miss_avg
    }
    else if (Name[i]=="Mrs"){
      Age[i]=mrs_avg
    }
    else if (Name[i]=="Mr"){
      Age[i]=mr_avg
    }
    else if (Name[i]=="Dr"){
      Age[i]=dr_avg
    }
    else {
      print("Uncaught Title")
    }
  }
}
summary(Age)
boxplot(Age,range=0)
# we will create additional variables which can help us to predict the survival of passengers more closely
# the additional variable is created called child which adds a 1 if the age <12 and 2 otherwise
#add a new column to the dataset
traindata1["Child"]=NA
names(traindata1)
summary(traindata1$Child)
attach(traindata1)
names(traindata1)
summary(Age)
for (i in 1:nrow(traindata1)) {
  if (Age[i] <= 12) {
    Child[i] = 1
  } else {
    Child[i] = 2
  }
}
table(Child)
# define another new variable- Family (column) in the dataset
traindata1["Family"]=NA
attach(traindata1)
# add the total number of family members for each person
for (i in 1:nrow(traindata1)){
  x=SibSp[i]
  y=Parch[i]
  Family[i]=x+y+1
}
table(Family)
# to see if the passenger was a mother, add another variable called Mother
traindata1["Mother"]=NA
attach(traindata1)
names(traindata1)
# if title = Mrs and no of childern >0, then mother
for (i in 1:nrow(traindata1)){
  if (Name[i]=="Mrs" & Parch[i]>0){
    Mother[i]=1
  }
  else {
    Mother[i]=2
  }
}
table(Mother)
# we repeat the exact same process on test data
testdata
testdata1=testdata[-c(1,8:11)]
testdata1$Sex=gsub("female",1,testdata1$Sex)
testdata1$Sex=gsub("male",0,testdata1$Sex)
test_master_vector = grep("Master.",testdata1$Name)
test_miss_vector = grep("Miss.", testdata1$Name)
test_mrs_vector = grep("Mrs.", testdata1$Name)
test_mr_vector = grep("Mr.", testdata1$Name)
test_dr_vector = grep("Dr.", testdata1$Name)
for(i in test_master_vector) {
  testdata1[i, 2] = "Master"
}
for(i in test_miss_vector) {
  testdata1[i, 2] = "Miss"
}
for(i in test_mrs_vector) {
  testdata1[i, 2] = "Mrs"
}
for(i in test_mr_vector) {
  testdata1[i, 2] = "Mr"
}
for(i in test_dr_vector) {
  testdata1[i, 2] = "Dr"
}
test_master_age = round(mean(testdata1$Age[testdata1$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(testdata1$Age[testdata1$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(testdata1$Age[testdata1$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(testdata1$Age[testdata1$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(testdata1$Age[testdata1$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(testdata1)) {
  if (is.na(testdata1[i,4])) {
    if (testdata1[i, 2] == "Master") {
      testdata1[i, 4] = test_master_age
    } else if (testdata1[i, 2] == "Miss") {
      testdata1[i, 4] = test_miss_age
    } else if (testdata1[i, 2] == "Mrs") {
      testdata1[i, 4] = test_mrs_age
    } else if (testdata1[i, 2] == "Mr") {
      testdata1[i, 4] = test_mr_age
    } else if (testdata1[i, 2] == "Dr") {
      testdata1[i, 4] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", testdata1[i,2], sep=""))
    }
  }
}
testdata1["Child"] = NA

for (i in 1:nrow(testdata1)) {
  if (testdata1[i, 4] <= 12) {
    testdata1[i, 7] = 1
  } else {
    testdata1[i, 7] = 1
  }
}

testdata1["Family"] = NA

for(i in 1:nrow(testdata1)) {
  testdata1[i, 8] = testdata1[i, 5] + testdata1[i, 6] + 1
}

testdata1["Mother"] = NA

for(i in 1:nrow(testdata1)) {
  if(testdata1[i, 2] == "Mrs" & testdata1[i, 6] > 0) {
    testdata1[i, 9] = 1
  } else {
    testdata1[i, 9] = 2
  }
}
names(traindata1)
train.glm <- glm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass+Family+Mother, family = binomial("logit"), data = traindata1)
