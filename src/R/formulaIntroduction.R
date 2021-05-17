#' ---
#' title: Introduction to R formulas
#' author: Nicolas Delhomme
#' date: "`r Sys.Date()`"
#' output:
#'  html_document:
#'    toc: true
#'    number_sections: true
#' ---
#' # Formulas
#' ## What are formulas?
#' A lot of material (more than) inspired from a 
#' [DataCamp tutorial](https://www.datacamp.com/community/tutorials/r-formula-tutorial)
#' ```{r formula help,eval=FALSE}
#' ?formula
#' ```
#' It is a quoted, _i.e._ non evaluated expression:
x <- 1
x * 10
quote(x * 10) 
typeof(quote(x * 10))
class(quote(x * 10))
eval(quote(x * 10))

#' An example:
#' `Sepal.Width ~ Petal.Width + log(Petal.Length) + Species`
#' 
#' ## Response(s) and predictor(s)
#' In the example above, we model the sepal width as a function of the petal 
#' width and length and the species. Formulas can be one or two sided:
#'
#' One sided:
e <- ~ x + y + z

#' Two sided
f <- y ~ x + b 

#' In a comparison
length(e)
length(f)
e[[1]]
e[[2]]
f[[3]]

#' What is stored in a formula?
all.vars(f)
all.vars(e)
terms(f)
terms(e)

#' ## Writing formulas
#' ### In plain
y ~ x
~ x + y + z
g <- y ~ x + b

#' ### Programmatically
"y ~ x1 + x2"
h <- as.formula("y ~ x1 + x2")
h <- formula("y ~ x1 + x2")
as.formula(paste(c("~ x","y","z"),collapse="+"))

#' ## Formula operators
#' * `+` to add terms
#' * `-` to remove terms
#' * `*` for crossing
#' * `:` for interaction
#' * `%in%` for nesting
#' * `^` for limit crossing to the specified degree
#' * `|` for conditionals (sometimes, not base R)

#' ### + and -
#' Combine predictors. Note that `+` implies that the predictors are independent
#' _i.e._ in a linear model examples, they would be orthogonal
y ~ x1 + x2

#' Remove predictors
y ~ x1 - x2

#' ### *, :, %in% and ^
#' Important for regression, essential to describe interaction terms
#' 
#' Note that these are the same:
y ~ x1 * x2
y ~ x1 + x2 + x1:x2

#' Demonstration:
x = rnorm(5)
x2 = rnorm(5)
y = rnorm(5)

model.frame(y ~ x * x2, data = data.frame(x = x, y = y, x2=x2))

model.frame(y ~ x + x2 + x:x2 , data = data.frame(x = x, y = y, x2=x2))

terms(y ~ x1 * x2)
terms(y ~ x1 + x2 + x1:x2)

#' Nesting:
#' 
#' The terms on the left are nested within those on the right. A simple example to
#' revisit the example above
terms(y ~ x1 + x2 + x2 %in% x1)

terms(y~x2 %in% x1)

#' Crossing to a specified degree:
terms(y ~ (a+b+c)^2)
terms(y ~ (a+b+c) * (a+b+c))
terms(y ~ a*b*c - a:b:c)

#' ### conditionals:
#' For example
Sepal.Width ~ Petal.Width | Species

#' but be careful, it's not base R and has different meaning in different packages
terms(Sepal.Width ~ Petal.Width | Species)

#' ## Caveats
#' Formulas usually just involve variables and factor names, they can also use
#' arithmetic expressions. Imagine we want a model with x and x^2, writing the
#' following fails:
terms(y ~ x + x^2)

#' Why? Because of the above where ^2 is crossing x to the second degree 
#' (and we have only one variable)
terms(y ~ x + x * x)

#' Rather we need to encapsulate arithmetic operations in `I()`, the _as-is_
#' operator
terms(y ~ x + I(x^2))

terms(y ~ I(3 * x))

#' ## Inspecting formulas
#' Well, we have repeatedly used `terms`. There is also `all.vars`. And we toy'ed
#' with `model.frame`.
#' 
#' ## Modifying formulas
#' For example, we could `update` our `f` formula from above
f

#' Here, `~.` is a special variable that represent the formula in `f`
update(f,~. -b)

#' Arguably, one will most often simply rewrite the formula, using _e.g._ paste, 
#' but being able to dynamically update a model can come in handy
reformulate(termlabels=all.vars(e),intercept = FALSE,response="w")

#' ## Application
#' Formulas can be used for more than just modeling. For example, the 
#' `ggformula` and `lattice` graphic package use them. They also relies on the
#' *non-standard evaluation (NSE)*. The NSE means that part of the formula will
#' be evaluated only at run time. For example:
subset(iris, Sepal.Width > 3)
tryCatch(iris[Sepal.Width > 3,],error=function(e){e})
attach(iris)
iris[Sepal.Width > 3,]
detach(iris)

#' NSE is used in many packages (and so are formulas) for dynamic programming. 
#'
#' Examples:
#' 
#' * [Brodie Gaslam blog](https://www.brodieg.com/2020/05/05/on-nse/)
#' * [Hadley Wickham Advanced R](http://adv-r.had.co.nz/Computing-on-the-language.html)
#'
#' but modeling is, if not anymore the sole, the original application.
#' 
#' ### linear models
lm.m <- lm(Sepal.Width ~ Petal.Width + log(Petal.Length) + Species, 
           data = iris, 
           subset = Sepal.Length > 4.6)


print(lm.m)

summary(lm.m)

attach(iris)

plot(as.numeric(predict.lm(lm.m)),Sepal.Width[Sepal.Length>4.6])

abline(0,1,lty=2,col="red")

residuals.lm(lm.m)

chisq.test(abs(residuals.lm(lm.m)))

cor.test(as.numeric(predict.lm(lm.m)),Sepal.Width[Sepal.Length>4.6])

plot(as.numeric(predict.lm(lm.m,newdata = iris[Sepal.Length<=4.6,])),
     Sepal.Width[Sepal.Length<=4.6])

detach(iris)

#' ### non linear mixed effects
#' 
#' ```{r install nlme,eval=FALSE}
#' install.packages("nlme")
#' ```
library(nlme)
library(MASS)
data(oats)

names(oats) = c('block', 'variety', 'nitrogen', 'yield')
oats$mainplot = oats$variety
oats$subplot = oats$nitrogen

nlme.m = lme(yield ~ variety*nitrogen,
             random = ~ 1|block/mainplot,
             data = oats)

summary(nlme.m)

#' ### generalized linear models
#' 
#' ```{r install MPDiR,eval=FALSE}
#' install.packages("MPDiR")
#' ```

library(MPDiR)

data(Chromatic)

glm.m <- glm(Thresh ~ Axis:(I(Age^-1) + Age),
             family = Gamma(link = "identity"), 
             data = Chromatic)

summary(glm.m)

#' ### and more
#' see for example the `Formula` or `formula.tools` packages
#' 
#' ```{r install Formula, eval=FALSE}
#' install.packages("Formula")
#' ```
library(Formula)

#' multi-response
Formula(y1+y2~x1+x2)

#' multi-part
Formula(y~x1|x2)

#' both
Formula(y1+y2~x1+x2|I(x2^2))

#' # Further readings:
#' 
#' * Two opinion pieces by Max Kuhn (R Views blog): 
#' [Good](https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/) and 
#' [Bad](https://rviews.rstudio.com/2017/03/01/the-r-formula-method-the-bad-parts/) parts about formulas
#' 
#' # Session Info
#' ```{r session info, echo=FALSE}
#' sessionInfo()
#' ```
