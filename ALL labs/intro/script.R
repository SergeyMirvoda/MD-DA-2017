#1
35 + 777
13 * 47
675 * 15
849 * 629
#2
11111111 * 11111111
11111111 * 11111111 * 1111111 ? .
#3
lnVect <- c(3, 7, 12, 15, 20)
wdVect <- c(2, 5, 8, 11, 15)
lnVect * wdVect
#4
plot(wdVect, lnVect, type = "p", xlab = "width", ylab = "length")
#5
timeBar <- c("Math" = 40, "English" = 40, "PE" = 10, "Programming" = 150)
barplot(timeBar, main = "Vasya's timetable", xlab = "hours")
#6
drinks <- rnorm(10000, mean = 450, sd = 4)
mean(drinks > 455)*100