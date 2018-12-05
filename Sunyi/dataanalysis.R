# Top ----
setwd("/Users/sunyichi/Documents/GitHub/GoT-Bayesian/")
data=readRDS("data1204.rds")
#d <- read.csv("character-deaths.csv")
require(randomForest)
require(ggplot2)
require(gridExtra)
require(caret)
library(MASS)
d=data

# try classification ----
set.seed(2017)
train = sample(1:nrow(d),2*nrow(d)/(3))
tr <- d[train,]
ts <- d[-train,]

fit <- randomForest(as.factor(DwD)~GoT+CoK+SoS+FfC,data=tr, importance=TRUE, ntree=1000)
varImpPlot(fit)
pdrf <- predict(fit, ts)
confusionMatrix(pdrf,as.factor(ts$DwD))


# add "survival expansion" BOOK 1-5----
data = d
# Some of the data is wrong, fixes:
# Cressen is intro'd and dies in book 2
#data$`Book of Death`[147] = 2


# add on 1 to the death chapter number and intro chapter number, for prologues.
data$`Book Intro Chapter` = data$`Book Intro Chapter` + 1
data$`Death Chapter` = data$`Death Chapter` + 1


# Make a book intro variable
data$Book.of.Intro = 0
#data$Book.of.Intro = data$GoT
X1 = which(data$GoT == 1)
X2 = which(data$CoK == 1 & data$GoT == 0)
X3 = which(data$SoS== 1 & data$CoK == 0 & data$GoT == 0)
X4 = which(data$FfC ==1 & data$SoS == 0 & data$CoK == 0 & data$GoT == 0)
X5 = which(data$DwD == 1 & data$FfC == 0 & data$SoS == 0 & data$CoK == 0 & data$GoT == 0)
data$Book.of.Intro[X1] = 1
data$Book.of.Intro[X2] = 2
data$Book.of.Intro[X3] = 3
data$Book.of.Intro[X4] = 4
data$Book.of.Intro[X5] = 5

# these are the characters that die in the same book they're introduced.
Same_book_death = which(data$Book.of.Intro == data$`Book of Death`)
Dies_same_book_1 = sum(data$Book.of.Intro[Same_book_death] == 1)
Dies_same_book_2 = sum(data$Book.of.Intro[Same_book_death] == 2)
Dies_same_book_3 = sum(data$Book.of.Intro[Same_book_death] == 3)
Dies_same_book_4 = sum(data$Book.of.Intro[Same_book_death] == 4)
Dies_same_book_5 = sum(data$Book.of.Intro[Same_book_death] == 5)

# Average number of deaths of characters that are introduced and die in the same book.
mean(c(Dies_same_book_1,Dies_same_book_2,Dies_same_book_3,Dies_same_book_4,Dies_same_book_5))

# Book 1 has 73 chapters
# book 2 has 70 chapters
# Book 3 has 82 chapters
# book 4 has 46 chapters
# book 5 has 73 chapters

# Make a character lives variable

Total_possible_Life = 73 + 70 + 82 + 46 + 73
book1chap = 73
book2chap = 70
book3chap = 82
book4chap = 46
book5chap = 73
data$Chapters_featured = data$GoT*book1chap + data$CoK*book2chap + data$SoS*book3chap + data$FfC*book4chap + data$DwD*book5chap
# subtract the chapters before a character appears in GoT
data$Chapters_featured[X1] = data$Chapters_featured[X1] - data$`Book Intro Chapter`[X1] + 1
data$Chapters_featured[X2] = data$Chapters_featured[X2] - data$`Book Intro Chapter`[X2] + 1 
data$Chapters_featured[X3] = data$Chapters_featured[X3] - data$`Book Intro Chapter`[X3] + 1
data$Chapters_featured[X4] = data$Chapters_featured[X4] - data$`Book Intro Chapter`[X4] + 1
data$Chapters_featured[X5] = data$Chapters_featured[X5] - data$`Book Intro Chapter`[X5] + 1

data$Death.Chapter2 = data$`Death Chapter`
data$Death.Chapter2[is.na(data$`Death Chapter`)] = 0
Not_Dead = is.na(data$`Death Chapter`)
Isnt_Dead = sum(Not_Dead)
#618 not dead yet

# subtract the difference in chapters before death and total # of chapters in book.
#data$In_Book  How many chapters character lives

data$In_Book = data$Chapters_featured

D1 = which(data$`Book of Death` == 1 & !is.na(data$`Death Chapter`))
D2 = which(data$`Book of Death` == 2 & !is.na(data$`Death Chapter`))
D3 = which(data$`Book of Death` == 3 & !is.na(data$`Death Chapter`))
D4 = which(data$`Book of Death` == 4 & !is.na(data$`Death Chapter`))
D5 = which(data$`Book of Death` == 5 & !is.na(data$`Death Chapter`))

data$In_Book[D1] = data$Chapters_featured[D1] - (73 - data$Death.Chapter2[D1]) 
data$In_Book[D2] = data$Chapters_featured[D2] - (70 - data$Death.Chapter2[D2])
data$In_Book[D3] = data$Chapters_featured[D3] - (82 - data$Death.Chapter2[D3])
data$In_Book[D4] = data$Chapters_featured[D4] - (46 - data$Death.Chapter2[D4]) 
data$In_Book[D5] = data$Chapters_featured[D5] - (73 - data$Death.Chapter2[D5])

negative<-c(which(data$In_Book < 0),which(is.na(data$In_Book)))
data$Name[which(data$In_Book < 0)]
hist(data$In_Book[-negative], breaks = 50, main = "Chapters Surviving in Book", xlab= "# of chapters")


###chapters introduced 
colnames(data)[6] <- "Book.Intro.Chapter"
data$FirstAppearanceChap <- 0
for(i in 1:nrow(data)){
  if(data$Book.of.Intro[i]==1){
    data$FirstAppearanceChap[i]=data$Book.Intro.Chapter[i]
  }
  if(data$Book.of.Intro[i]==2){
    # 72 chapters in book 1
    data$FirstAppearanceChap[i]=72 + data$Book.Intro.Chapter[i]
  }
  if(data$Book.of.Intro[i]==3){
    # 69 chapters in book 2
    data$FirstAppearanceChap[i]=72 + 69 + data$Book.Intro.Chapter[i]
  }
  if(data$Book.of.Intro[i]==3){
    data$FirstAppearanceChap[i]=72 + 69 + 81 + data$Book.Intro.Chapter[i]
  }
  if(data$Book.of.Intro[i]==4){
    data$FirstAppearanceChap[i]= 72 + 69 + 81 + 45 + data$Book.Intro.Chapter[i]
  }
}


# simulate to show data follows weibull
library(survival)
survreg(Surv(data$In_Book[-negative])~1)

ll.weibull<-function(dat,par){
  a=exp(par[1])
  b=exp(par[2])
  
  ll=-sum(dweibull(dat,a,scale=b,log=T))
  
}
weibull.optim <- optim(par=c(log(1/2.2),12),fn=ll.weibull,dat=data$In_Book[-negative])


g = function(x){
  return(dweibull(x,shape = 1.8,scale = 1.5))
}
ggplot(data.frame(x = data$In_Book),aes(x=x)) + 
  geom_histogram(aes(y=..density..),binwidth=1) +
  stat_function(fun = g, colour = "red",n=10000)+
  ggtitle("Density of Weibull with shape 1.75 and scale 1.5")+xlim(c(0,10))

p1=ggplot(data.frame(x = data$In_Book),aes(x=x)) + 
  geom_histogram(aes(y=..density..),binwidth=4) +
  ggtitle("Density of surviving chapters")



grid.arrange(p1,p2,nrow=2)

save.image("work_1201.RData")
