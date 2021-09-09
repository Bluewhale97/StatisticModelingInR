#1. descriptive statistics
myvars <-c("mpg","hp","wt")
head(mtcars[myvars])

#2.a menagerie of methods
#summary() function to obtain descriptive statistics
summary(mtcars[myvars])

#summary function provides the minimum, maximum, quartiles and mean for numerical variables and frequencies for factors and logical vectors

#3. we can use apply() or sapply() for providing any descriptive statistics we choose
sapply(x, FUN, options) #x is the data frame and FUN is an arbitrary function
#typical functions that we can plug in are mean(), sd(), var(), min(), median(), length(), range() and quantile()
#fivenum() returns Tukey's five-number summary(minimum, lower-hinge, median, upper-hinge, and maximum)


mystats <-function(x, na.omit=F){
  if(na.omit) #if exist omit values, return all of values that not include missing values.
    x<-x[!is.na(x)]
  m <-mean(x)
  n <- length(x)
  s <-sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n-3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
  }

myvars <-c("mpg","hp","wt")
sapply(mtcars[myvars], mystats)

#4. other valued methods
#descriptive statistics tools in Hmisc, pastecs and psych are used helpfully

library(Hmisc)
myvars <-c("mpg","hp","wt")
describe(mtcars[myvars])

#describe() function in the Hmisc package returns the number of variables and observations and number of missing and unique values, the mean, quantiles and the hive hoghest and lowest values

stat.desc(x, basic=T, desc=T, norm=F, p=.95) #in pastecs package, providing a wide range of descriptvie statistics
#x is a data frame or time series. basic=T, the number of values, null values, missing values, minimum, maximum,range, and sum are provided
#if desc=T, the median, mean, sd of the mean, .95 confidence interval for the mean, variance, sd and coefficient of variation are provided
# if norm=T(not the default) normal distribution statistics are returned, including skewness and kurtosis(and their statistical significance) and theshapiro-Wilk test of normlity

install.packages("pastecs")
library(pastecs)
myvars <-c("mpg","hp","wt")
stat.desc(mtcars[myvars])
#psych and Hmisc both provide a function named describe(), R will use the package that is last loaded, which will take precedence
#if want to use Hmisc, just code Hmisc::describe(mt)

#5. descriptive statstics by group
#we can use the aggregate() function to obtain descriptive statistics by group
myvars <-c("mpg","hp","wt")
aggregate(mtcars[myvars], by=list(am=mtcars$am), mean)

aggregate(mtcars[myvars], by=list(am=mtcars$am),sd)
#if used list(mtcars$am), the am column would be labeled Group.1 rather than am.
# we can group more details, by=list(name1=groupvar1, name2=groupvar2, ..., nameN=groupvarN)

#aggregate() only allows to use single-value functions such as mean, sd and the like in each call.
by(data, INDICES, FUN) #data is a data frame or matrix, INDICES is a factor or list of factors that defines the groups, and the FUN is an arbitrary function that operates on data frame

dstats <-function(x) sapply(x, mystats)
myvars <-c("mpg","hp","wt")
by(mtcars[myvars], mtcars$am, dstats)

#6. additional methods by group
#doBy package and the psych package also provide functions for descriptive stastics by group
summaryBy(formula, data=dataframe, FUN=function) #the formula takes the form var1+ var2 +var3+...+varN ~groupvar1+ groupvar2+..+groupvarN

install.packages("doBy")
library(doBy)
summaryBy(mpg+hp+wt~am, data=mtcars, FUN=mystats) #left are the numeric variables to be analyzed and variables on the right are categorical grouping variables

#describeBy () function contained in the psych package provides the same descriptive statsitics as descirbe()
install.packages("psych")
library(psych)
myvars <-c("mpg","hp","wt")
describeBy(mtcars[myvars], list(am=mtcars$am))
#descibeBy function doesnt allow you to specify an arbitrary function, so its less generally applicable
#if there is more than one group variable, we can write them as list(name1=groupvar1, naame2=groupvar2,...,nameN=groupvarN)
#it will only work if there is no empty cell when grouping variables are crossed


#7. frequency and contingency tables
library(vcd)
head(Arthritis)

#creating frequency tables
table(var1, var2, ... , varN) #creates an N way contingency table from N categorical variables(factors)

xtab(formula, data) #creates an N way contingency table based on a formula and a matrix or data frame

prop.table(table, margins)# expresses table entries as fractions of the marginal table defined by the margins

margin.table(table, margins)#computes the sum of table entries for a marginal table defined by the margins

admargins(table, margins) #puts summary margns(sums by default) on a table

ftable(table) # creates a compact, "flat" contingency table

#a. one way tables
mytable <-with(Arthritis, table(Improved))
mytable #generate simple frequency counts using the table() function

#turn the frequencies into proportions iwth prop.table()
prop.table(mytable)

#or into percentages using prop.table()*100
prop.table(mytable)*100

#b. two-way tables
mytable <-table(A,B) #where A is the row variable and B is the column variable

mytable <- xtabs(~A+B, data=mydata) #xtabs() function allows to create a contingency table using formula-style input
#where mydata is a matrix or data frame. In general, the variables to be cross-classified apprear on the right of the formula, if the variable is included on the left side of the formula, it is assumed to be a vector of frequencies(useful if the data have already been tabulated)

mytable <-xtabs(~Treatment+Improved, data=Arthritis)
mytable

#we can generate marginal frequencies and proportions using the margin.table() and prop.table() functions
margin.table(mytable,1) #index (1) refers to the first variable in the table() statement
margin.table(mytable,2)

prop.table(mytable,1)
prop.table(mytable,2)


#use the addmargins() function to add marginal sums to these tables
addmargins(mytable)
addmargins(prop.table(mytable))
addmargins(prop.table(mytable,1))

#the default of addmargins() function is to create sum margins for all variables, the following code adds a Sum column alone
addmargins(prop.table(mytable,1),2) #index 2 for improved, 1 for adding the treatment
#table() function ignores missing values by default, option useNA="ifany" to include missing values


#CrossTable() function in the gmodels package to produce two way tables

install.packages("gmodels")
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)
#option to report percentages(row, column and cell); specify decimal palces; produce chi-square, Fisher and McNemar tests of independnce; report expected and residual values
help(CrossTable) #FOR DETAILS

#multidimensional tables
ftable() #print multidimensional tables in a compact and attractive manner

mytable <-xtabs(~Treatment+Sex+Improved, data=Arthritis) #cell frequencies
mytable
ftable(mytable)

margin.table(mytable,2)
margin.table(mytable,3)

margin.table(mytable,c(1,3)) #Treatment=1 and improved=3 means 1*3=treamtment * improved marginal frequencies

ftable(prop.table(mytable, c(1,2))) #imrpoved proportions for treatment * sex
ftable(addmargins(prop.table(mytable, c(1,2)),3)) 

#if multiply the resulting table by 100m 
ftable(addmargins(prop.table(mytable, c(1,2)),3))*100

#contigency tables tell us the freqyency or proportions of cases for each combination of the variables that make up the table

#8. test of independence
#Chi-square test of independence
chisq.test() #two way table independence test

library(vcd)
mytable <-xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)

mytable <-xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)

#warning message produced because one of the six cells in the table has an expected value less than five, which may invalidate the chi square approximation
#p<.01, reject the hypothesis that the treatment type and outcome are independent
#p>.01, it is not unresonable to assume that they are independent

#Firsher's exact test
fisher.test(mytable) #evaluate the null hypothesis of independece of rows and columns in a contingency table with fixed marginals
#mytable is a two way table

mytable <-xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)

#some other statistical packages includes the fisher test function that can be applied to any two way table with two or more rows and columns

#Cochran-mantal-haenszel test
mantelhaen.test() #provides Cochran-mantel-haenszel chi-square test of the h0 that two nominal variables are conditionally independent in each stratum of a third variable

mytable <-xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)
#assumes that there's no three way interaction

#9. measures of association

library(vcd)
mytable <-xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)
#generally larger magnitudes indicate stronger association
#vcs package also provides a kappa() function that can calculate Cohen's kappa and weighted kappa for a confusion matrix


#10. correlation
#correlation coefficients are used to describe relationships among quantitative variables

#type of correlations
#pearson: pearson product-moment assesses the degree of linear relationship between two quantitative variables
#spearman: spearman's rank-order correlation coefficient assesses the degree of relationship between two rank-ordered variables
#Kendall correlation: kendall's tau is a nonparametric meansure of rank correlation

cor(x, use=, method=) #x is the matrix or data frame
# use specifies the handling of missing data. options are all.obs(assumes no missing data- missing data will produce an error), everything(any correlation involving a case with missing values will be set to missing), complete.obs(listwise deletion), and pairwise.complete.obs(pairwise deletion).
#method specifies the type of correlation, the options are pearson, spearman and kenda11

#the default options are use="everything" and method="pearson"
states <- state.x77[,1:6]
cov(states) #first call produces the var and covar
cor(states)
cor(states, method="spearman")

#we can get square matrices by default(all variables crossed with all other variables)
#also we can produce nonsquare matrices
x<-states[,c("Population","Income","Illiteracy","HS Grad")]
y<-states[,c("Life Exp","Murder")]
cor(x,y

#Partial correlations: correlation between two quantitative variables controlling for one or more other quantiative variables
install.packages("ggm")
library(ggm)
pcor(u,s) #in the ggm package, u is a vector of numbers, with the first two numbers being the indices of the variables to be correlated, and the remaining numbers being the indices of the conditioning variables
#S is the covariance matrix among the variables
colnames(states)
pcor(c(1,5,2,3,6),cov(states))
#0.346 is the correlation between population and murder rate(1,5), controlling for the influence of income, illiteracy rate and high school graduation rate(variables 2,3, and 6 respectively).


#other types of correlation
hetcor() # it is in the polycor package and 
#can compute a heterogenerous correlation matrix containing Pearson product-moment correlations between numeric variables, 
#and polyserial correlations between numeric and ordinal vairbales, 
#and polychoric correlations between ordinal variables and tetrachoric correlations between two dichotomous variables.
#Polyserial, polychoric, and tetrachric correlations assume that the ordinal or dichotomous variables are derived from underlying normal distributions


#11. testing correlations for significance
cor.test(x,y, alternative=, method= )# test an individual pearson, spearman and kendall correlation coefficient
#x and y are the variables to be correlated
#alternative specifies a two-tailed or one-tailed test("two.side", "less" or "greater") and mehods specifies the type of correlation("pearson","kendall", or "spearman")
#alternative="less" when the research hypothesis is that the population correlation is less than 0, alternative="greater" for the population correlatin is greater than 0

cor.test(states[,3], states[,5])



#corr.test() for more correlation at a time
corr.test() #in the psych package

install.packages("psych")
library(psych)
corr.test(states, use="complete")
#use= options can be "pairwise" or "complete" for pairwise or listwise deletion of missing values, respectively
#the method= option is "pearson" by default

#other tests of siginificance
pcor.test() #test the conditional independence of two variables controlling for one or more additional variables, assuming multivariate normality
pcor.test(r, q, n)
#where r is the partial correlation produced by the pcor()
#q is the number of variables being controlled
#n is the sample size
#r.test() function in the psych package also provides a number of useful siginicance tests between two independent correlations, two dependent correlations sharing a single variable, two dependent correlations based on completely different variables.

#12. visualizing correlations
#bivariate relationships underlying correlations can be visualized through scatter plots and scatter plot matrices
#whereas correlograms provide a unique and powerful method for comparing a large number of correlation coefficients in a meaningful way

#13. T tests: comparison of two groups
#independent t tets
t.test(y~x, data) #where y is numeric and x is a dichotomous variable
t.test(y1, y2) 

#in contrast to most statistical packages, the default test assumes unequal variance and applies the Welsh degrees-of-freendom modification.
#option var.equal=T to specify equal variance and a pooled variance estimate
#two tailed alternative is assumed

library(MASS)
t.test(Prob ~ So, data = UScrime)

#if the outcome variable is a proportion, we might try to transform it to normality before carrying out the t-test

#dependent t test
t.test(y1, y2, paired=T)

library(MASS)
sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x), sd=sd(x))))


#14. when there are more than two groups
anova()

#15. Nonparametric tests of group difference

#comparing two groups

#if the two groups are independent, we can use the wilcoxon rank sum test
wilcox.test(y~x, data) #option data argument refers to a matrix or data frame, option "exact" to produce an exact test

with(UScrime, by(Prob, So, median))


#when the data of groups are paired and the assumption of normality is unwarranted, the format is identical but should add the paired=t
sapply(UScrime[c("U1","U2")],median)
with(UScrime, wilcox.test(U1, U2, paired=T))

#16. Comparing more than two groups

#when the groups are independent, Kruskal-Wallis test provides a useful approach
#if the groups are dependent, the Friedman test is more appropriate

kruskal.test(y~A, data)#where y is a numeric outcome variable and A is a grouping variable with two or more levels(if there are two levels, it is equivalent to the Mann-Whiney U test)

friedman.test(y~A|B, data)#A is a grouping variable, and B is a blocking variable that identifies matched observations

states <-data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~state.region, data=states)


#although we can make a decision on rejecting h0 but there is no result to tell that where makes it differed, we can compare groups at a time using the Wilcoxon test
#to apply a multiple-comparisons procedure that computes all pairwise comparisonsm while controlling the type I errir rate
#wmc() function can be used for it which compares groups two at a time using the Wilcoxon test and adjusts the probabiity values using the p.adj() function

#wmc() function from www.statmethods.net/RiA/wmc.txt

source("http://www.statmethods.net/RiA/wmc.txt")
states <-data.frame(state.region, state.x77)
wmc(Illiteracy~state.region, data=states,method="holm")

#adjustment method is Holm, which provides strong control of the family-wise error rate(the probability of making one or more Type I errors in a set of comparisons)
help(p.adjust)#for description of the other methods available

#17. visualizing group difference
#box plots(simple, notched and violin)
#overlapping kernel density plots and more
