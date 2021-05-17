#' ---
#' title: A walk through modeling
#' author: Nicolas Delhomme
#' date: "`r Sys.Date()`"
#' output:
#'  html_document:
#'    toc: true
#'    number_sections: true
#' ---
#' From [Julian's Faraway book: Linear Models with R](http://www.utstat.toronto.edu/~brunner/books/LinearModelsWithR.pdf), chapter 11.
#' 
#' ```{r install faraway,eval=FALSE}
#' install.packages("faraway")
#' ```
#' 
#' # Intro
#' 
#' Verbatim from the book
#' 
#' "In this chapter, we present a relatively complete data analysis. The example 
#' is interesting because it illustrates several of the ambiguities and 
#' difficulties encountered in statistical practice.
#' 
#' Insurance redlining refers to the practice of refusing to issue insurance to 
#' certain types of people or within some geographic area. The name comes from 
#' the act of drawing a red line around an area on a map. Now few would quibble 
#' with an insurance company refusing to sell auto insurance to a frequent drunk 
#' driver, but other forms of discrimination would be unacceptable.
#'
#'In the late 1970s, the U.S. Commission on Civil Rights examined charges by 
#'several Chicago community organizations that insurance companies were 
#'redlining their neighborhoods. Because comprehensive information about 
#'individuals being refused homeowners insurance was not available, the number 
#'of FAIR plan policies written and renewed in Chicago by zip code for the 
#'months of December 1977 through May 1978 was recorded. The FAIR plan was 
#'offered by the city of Chicago as a default policy to homeowners who had been 
#'rejected by the voluntary market. Information on other variables that might 
#'affect insurance writing such as fire and theft rates were also collected at 
#'the zip code level. The variables are:
#'
#' * race racial composition in percentage of minority
#' * fire fires per 100 housing units
#' * theft theft per 1000 population
#' * age percentage of housing units built before 1939
#' * involact new FAIR plan policies and renewals per 100 housing units
#' * income median family income in thousands of dollars
#' * side North or South Side of Chicago
#' 
#' The data come from Andrews and Herzberg (1985) where more details of the 
#' variables and the background are provided."
#' 
#' # The data
library(faraway)
data (chredlin)
chredlin
summary(chredlin)
boxplot(chredlin[,1:6],notch = TRUE)
boxplot(chredlin[,5],notch = TRUE)
boxplot(chredlin[,6],notch = TRUE)

par(mfrow=c (2, 3))
for (i in 1:6) stripchart (chredlin [, i], main=names(chredlin) [i], vertical=TRUE,method="jitter")
par(mfrow=c (1, 1))
pairs (chredlin[,1:6])

#' # Testing
#' ## Simpler model
summary (lm (involact~race, chredlin))

plot (involact~race, chredlin)
abline (lm (involact~race, chredlin))

plot (fire~race, chredlin)
abline (lm (fire~ race, chredlin))

#' ## Complete model
g <- lm (involact ~ race+ fire + theft + age + log(income)  ,chredlin)
summary (g)
gs <- g

#' ### Assessment
plot (fitted (g), residuals (g), xlab="Fitted", ylab="Residuals")
abline (h=0)
qqnorm (residuals (g))
qqline (residuals (g))

gi <- influence (g)
qqnorml (gi$coef[,4])
halfnorm(cooks.distance(g))
range(rstudent(g))

#' ### Outliers
chredlin[c(6,24),]

#' ## Reduced model
g <- lm(involact~race + fire + theft + age + log(income), chredlin,subset = -c(6,24))
summary(g)

#' ### Assessment
#' Need for transformation?
prplot (g, 1)
prplot (g, 2)

chreduc <- matplot(chredlin, cbind(chredlin, gs$fit), type="pl", ylab="y", pch=18, lty=1)

#' ```{r install leaps,eval=FALSE}
#' install.packages("leaps")
#' ```
library(leaps)
(b <- regsubsets(involact ~ race + fire + theft + age + log(income), force.in=1, data=chredlin[-c(6,24),]))
(rs <- summary(b))
rs$adjr2

#' ## Best model (?)
g <- lm (involact~ race + fire + theft + age, chredlin,subset= -c(6,24))
summary(g)

#' ### Alternatives
galt <- lm(involact ~ race+fire+log(income), chredlin,subset= -c(6, 24))
summary (galt)

galt <- lm(involact~ race+fire, chredlin, subset=-c(6, 24))
summary(galt)

g <- lm(involact~race + fire + theft + age, chredlin)
summary (g)

#' # Session Info
#' ```{r session info, echo=FALSE}
#' sessionInfo()
#' ```

