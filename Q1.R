
setwd("/Users/ciel/Downloads/ECON613 A1")

datast<-read.csv(file='datstu.csv',header = TRUE)
dimnames(datast)
#1.Number of students
length(datast$X)

#1.Number of schools
school<-datast[,5:10]

length(unique(as.vector(as.matrix(school))))


#1.Number of programs
program<-datast[,11:16]
length(unique(as.vector(as.matrix(program))))

#1.Number of choices
datast$choice1=paste0(datast$schoolcode1,datast$choicepgm1)
datast$choice2=paste0(datast$schoolcode2,datast$choicepgm2)
datast$choice3=paste0(datast$schoolcode3,datast$choicepgm3)
datast$choice4=paste0(datast$schoolcode4,datast$choicepgm4)
datast$choice5=paste0(datast$schoolcode5,datast$choicepgm5)
datast$choice6=paste0(datast$schoolcode6,datast$choicepgm6)
choice<-datast[,19:24]
length(unique(as.vector(as.matrix(choice))))


#1.Missing test score
tt<-datast[datast$score=="NA",]
length(tt$score)

#1.Apply to the same school
mm<-datast[datast$schoolcode1==datast$schoolcode2,]
mmm<-mm[mm$schoolcode2==mm$schoolcode3,]
mmmm<-mmm[mmm$schoolcode3==mmm$schoolcode4,]
mmmmm<-mmmm[mmmm$schoolcode4==mmmm$schoolcode5,]
mmmmmm<-mmmmm[mmmm$schoolcode5==mmmm$schoolcode6,]
d<-mmmmm[mmmmm$choicepgm1!=mmmmm$choicepgm2,]

dd<-na.omit(d)
length(dd$X)

#1.Apply to less than 6 choices

sum(is.na(datast$schoolcode6))

