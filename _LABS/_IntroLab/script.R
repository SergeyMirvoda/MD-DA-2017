#1
    35 + 777
    13 * 47
    675 * 15
    849 * 629
#2
    a <- 11111111
    a * a * 1111111?.
#3
    recArea <- function(a, b) {
        S <- a * b
        return(S)
    }

    lnVect <- c(3, 7, 12, 15, 20)
    wdVect <- c(2, 5, 8, 11, 15)
    recArea(lnVect, wdVect)
#4
    plot(wdVect, lnVect, type = "p", xlab = "width", ylab = "length", main = "Rectangle dimensions", bg = "black", pch = 21)
#5
    timeBar <- c("Math" = 40, "English" = 40, "PE" = 10, "Programming" = 150)
    dotchart(timeBar, main = "Vasya's timetable", xlab = "hours", pch = "x")
#6
    overPourChance <- function(x, rule) {
        cmp <- x > rule
        if (is.na(cmp[cmp == "TRUE"][1])) return(0)
        ch <- length(cmp[cmp == "TRUE"]) / length(x) * 100
        return(ch)
    }
    n <- 10^4
    rule <- 455
    drinks <- rnorm(n, mean = 450, sd = 4)
    overPourPercent <- overPourChance(drinks, rule)
    print(paste("Chance of overfill: ", overPourPercent, "%"))