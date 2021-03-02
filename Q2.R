ds<-read.csv(file='datsss.csv',header = TRUE)
sn<-read.csv(file='datjss.csv',header = TRUE)
datast<-read.csv(file='datstu.csv',header = TRUE)

#merge two data sets to get schools' location, latitude and longitude
m<-merge(ds,sn,by.x="sssdistrict",by.y="jssdistrict")
var_school <- c("schoolcode","sssdistrict","ssslong","ssslat")
newss <- unique(m[,var_school])

#link a student with his admitted school and program
datast$rankplace <- as.numeric(datast$rankplace)

stu_temp <- datast[which(datast$rankplace <= 6 & datast$rankplace >=1), ]
for (i in 1: nrow(stu_temp)) {
  stu_temp$schoolplaced[i] <- unlist(stu_temp[i,as.numeric(stu_temp$rankplace[i])+4])
  stu_temp$programplaced[i] <- unlist(stu_temp[i,as.numeric(stu_temp$rankplace[i])+10])
}

for (i in 1: nrow(newss)) {
  school_temp = unlist(newss[i, "schoolcode"])
  stu_sch <- stu_temp[which(stu_temp$schoolplaced == school_temp),]
  newss$cutoff[i] = min(stu_sch[,"score"])
  newss$quality[i] = mean(stu_sch[,"score"])
  newss$size[i] = nrow(stu_sch)
}

#question 3

#merge stu_temp and sn to link student's j
jhs<-merge(stu_temp,sn,by.x=c("jssdistrict"),by.y=c("jssdistrict"),fill=-99)
shs<-merge(jhs,newss,by.x="schoolplaced",by.y="schoolcode",all.x=TRUE)



shs$dist_s<-(69.172*(shs$ssslong-shs$point_x)*cos(shs$point_y/57.3))^2+(69.172*(shs$ssslat-shs$point_y))^2
shs$dist<-sqrt(shs$dist_s)
distance<-shs[,c(1,2,31)]


#question 4
shs<-shs[which(shs$dist<1000),]
for (i in 1:6) {
rank_i<-shs[which(datast$rankplace==i),]
rank_i<-shs[which(shs$rankplace==i),]
cutoff<-min(rank_i[,"score"])
quality<-mean(rank_i[,"score"])
distance<-mean(rank_i[,"dist"])
sd_quality<-sd(rank_i[,"score"])
sd_distance<-sd(rank_i[,"dist"])
cat(i,cutoff,quality,distance,"\n")
cat(i,sd_quality,sd_distance,"\n")

}

#student test score quantiles


na.omit(shs$score)
tapply(shs$dist,shs$score,quantile)


new_quantile<-quantile(shs$score,probs = c(0.25,0.5,0.75,1))

q1<-shs[which(shs$score<256),]
q2<-shs[which(shs$score>256& shs$score<289),]
q3<-shs[which(shs$score>289 & shs$score<330 ),]
q4<-shs[which(shs$score>330),]

min(q1$score)
mean(q1$score)
mean(q1$dist)
sd(q1$score)
sd(q1$dist)

min(q2$score)
mean(q2$score)
mean(q2$dist)
sd(q2$score)
sd(q2$dist)

min(q3$score)
mean(q3$score)
mean(q3$dist)
sd(q3$score)
sd(q3$dist)

min(q4$score)
mean(q4$score)
mean(q4$dist)
sd(q4$score)
sd(q4$dist)