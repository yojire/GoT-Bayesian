data = read.csv2("~/sunyichi/Documents/GitHub/GoT-Bayesian/character-deaths.csv", header = T, sep = ",")
#In character-deaths.csv, Cressen dead in chapter 2 instead of chapter 1
data$Book.of.Death[157] = 2

#There is 0 in Book.Intro.Chapter and death chapter, which means prologues
data$Book.Intro.Chapter = data$Book.Intro.Chapter + 1
data$Death.Chapter = data$Death.Chapter + 1

#introduction chapter of characterise: Book.of.Intro
data$Book.of.Intro = 0
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

