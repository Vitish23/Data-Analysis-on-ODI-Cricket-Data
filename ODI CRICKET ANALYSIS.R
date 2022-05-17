mydata = read.csv("C:/Users/vitis/OneDrive/Desktop/4 TH SEM MBA/ML/DATA SET/ODI_data.csv",na.strings = ",")
summary(mydata)# basic summary of the data 


#finding the correlation between Innings runs scored with Innings minutes batted
cor(mydata[c("Innings.Runs.Scored.Num","Innings.Minutes.Batted")])
summary(mydata[c("Innings.Runs.Scored.Num","Innings.Minutes.Batted")])


#sub-setting of the data 
mydata1 = subset(mydata,select = c("Innings.Runs.Scored.Num","Innings.Minutes.Batted","Innings.Balls.Faced"))
summary(mydata1)
cor(mydata1)

#Simple regression for Innings runs scored with innings mintues batted 
mydata1.lm <- lm( Innings.Runs.Scored.Num~Innings.Minutes.Batted, data = mydata1)
summary(mydata1.lm)

#simple regression for innings runs scored with innings faced balls 
mydata1.lm <- lm( Innings.Runs.Scored.Num~Innings.Balls.Faced, data = mydata1)
summary(mydata1.lm)

#plotting for innings runs scored with innings minutes batted 
plot(Innings.Runs.Scored.Num~Innings.Minutes.Batted, data = mydata1)

#plotting for innings runs scored with innings minutes batted 
plot(Innings.Runs.Scored.Num~Innings.Balls.Faced, data = mydata1)

#using subset of dataset1, for taking out the players how scored more than 25t runs
mydata2 = subset(mydata,Innings.Runs.Scored.Num > 25 )
print(mydata2)

##categorical variable for taking out the players with more than 25 runs and 10 mins of batting time

library(mosaic)
tally(~Innings.Runs.Scored.Num > 25 , data = mydata2 , margin = TRUE)
tally(~Innings.Minutes.Batted > 10 , data = mydata2 , margin = TRUE)
tally(~Innings.Runs.Scored.Num > 25 , data = mydata2 , margin = TRUE,format = "perc")
tally(~Innings.Minutes.Batted > 10 , data = mydata2 , margin = TRUE, format = "perc")

tally(~Innings.Runs.Scored.Num > 25 , data = mydata2 , margin = TRUE,format = "prop")
tally(~Innings.Minutes.Batted > 10 , data = mydata2 , margin = TRUE, format = "prop")



## contingency tables
tally(~Innings.Minutes.Batted|Innings.Runs.Scored.Num > 25 , data = mydata, margin = TRUE)


favstats(~Innings.Minutes.Batted,data = mydata)
favstats(~Innings.Runs.Scored.Num,data = mydata)

library(rpart)
library(rpart.plot)
mydata3= read.csv("C:/Users/vitis/OneDrive/Desktop/4 TH SEM MBA/ML/DATA SET/ODI_data.csv")

tree = rpart(Innings.Player ~Innings.Runs.Scored.Num+Innings.Runs.Scored.Num,mydata3)
print(tree)
rpart.plot(tree)



mydata4<-ggplot(mydata1, aes(x=Innings.Runs.Scored.Num, y=Innings.Minutes.Batted))+
  geom_point()
mydata4

unique(mydata$Innings.Runs.Scored.Num)
unique(mydata$Innings.Minutes.Batted)


data=read.csv("C:/Users/vitis/OneDrive/Desktop/4 TH SEM MBA/ML/DATA SET/ODI_data.csv")
runscr=Kohli$Innings.Runs.Scored.Num
ing_m=kohli$Innings.Minutes.Batted
ing_b=kohli$Innings.Balls.Faced


