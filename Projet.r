=#step1 : DATA CONCEPTION AND DEFINITION
"""
1-1 problematic
  The perception of students in the computer engineering sector at -ENSA Kenitra- on integration into the labor market.
1-2 population
  computer science engineering students of ENSA-K
1-3 research question
  Is concurrence among the factors that impact the integration of ENSAK computer engineering students into the labor market according to them?
1-4 Research hypothesis
  intense concurrence in the labor market impacts the perception of ENSAK computer engineering students on their integration into it
"""

#STEP 2 : DATA COLLECTION
#2-1  data importation

library(readxl)
data <- read_excel("dataprojet.xlsx")
View(data)

# STEP 3 : DATA PRETREATMENT 
# 3-1 Data conversion
# 3-1-1 variables codification

for (i in 7:15) {
  names(data)[i]<-paste("item",i-6)
}
names(data)[5]<- "field"
names(data)[6]<- "level"
names(data)[16]<-("Question")
#3-1-2 content codification
if(!is.numeric(data$Age)){
  data$Age = as.integer(data$Age)
}
if(is.character(data$Genre)){
  data$Genre=as.factor(data$Genre)
}
if(is.character(data$`situation familiaire`)){
  data$`situation familiaire`=as.factor(data$`situation familiaire`)
}
if(is.character(data$field)){
  data$field=as.factor(data$field)
}
if(is.character(data$level)){
  data$level=as.factor(data$level)
}
if(is.character(data$`item 1`)){
  data$`item 1`=as.factor(data$`item 1`)
}
if(is.character(data$`item 2`)){
  data$`item 2`=as.factor(data$`item 2`)
}
if(is.character(data$`item 3`)){
  data$`item 3`=as.factor(data$`item 3`)
}
if(is.character(data$`item 4`)){
  data$`item 4`=as.factor(data$`item 4`)
}
if(is.character(data$`item 5`)){
  data$`item 5`=as.factor(data$`item 5`)
}
if(is.character(data$`item 6`)){
  data$`item 6`=as.factor(data$`item 6`)
}
if(is.character(data$`item 7`)){
  data$`item 7`=as.factor(data$`item 7`)
}
if(is.character(data$`item 8`)){
  data$`item 8`=as.factor(data$`item 8`)
}

if(is.character(data$`item 9`)){
  data$`item 9`=as.factor(data$`item 9`)
}
if(is.character(data$Question)){
  data$Question=as.factor(data$Question)
}


# 3-2 Data cleaning
# 3-2-1 Outliers
boxplot(data$Age)
out=boxplot.stats(data$Age)$out
# There are no outliers 
# treatement of outliers :
for(i in out){
  for(j in 1:length(data$Age)){
    if(data$Age[j] == i && !is.na(data$Age[j])){
      data$Age[j] = NA
    }
  }
}


# 3-2-2 missing values
Len=sum(is.na(data$Age))
Per=Len/length(data$Age)

if(Per==0){
  print("there is no missing value in your sample")
}else if(Per<0.05)
{
  print("we need to delete the lignes contains Na")
} else
{
  print("We're going to estimate")
  for(i in 1:length(data$Age)){
    if(is.na(data$Age[i])){
      data$Age[i] = as.integer(mean(data$Age, na.rm = TRUE))
    }
  }
}
# 3-3 normality test
# 3-3-1 normality test in  shapiro's vision
shapiro.test(data$Age)
# p-value = 1.922e-05 < 5% 
# there is a significant difference between the normal distribution and the distribution of  age

# 3-3-2 quasi-normality
library(moments)
skewness(data$Age)
kurtosis(data$Age)

# skewness value is  between -3 and 3
# kurtosis value is   between -3 and 3
#we can conclude that  the Age follows   quasi-normality which is leaned to the right (Sk<0). 

#3-4: representativity test : 
#level :
chisq.test(table(data$level),p= c(0.441,0.418,0.141) )
#p-value = 0.00407 <5% there is a significant difference between the population and our sample 

#STEP 4: Treatment (data analysis)
#4-1 exploration
#I uni variate analysis
#4-1-1: numerical exploration:
summary(data$Age)
summary(data$Genre)
summary(data$`situation familiaire`)
summary(data$level)
summary(data$field)

#we explore item by item to see the perception on students on the integration to labor market

#item 1: the integration of labor market isn't easy
summary(data$`item 1`)
prop.table(table(data$`item 1`))
barplot(table(data$`item 1`))
# 97.33% of our sample see that the integration to labor market is difficult with 52.77% are totally agree

#item 2: The IT labor market is not very balanced when it comes to offer and demand
summary(data$`item 2`)
prop.table(table(data$`item 2`))
barplot(table(data$`item 2`))
#58,33% of studants see that the market is not balanced when it comes to offer and demand with 8% who are totally agree and 27% don't agree

#item 3: ENSAK don't help their students to integrate the labor market
summary(data$`item 3`)
prop.table(table(data$`item 3`))
barplot(table(data$`item 3`))
#in this point there is different perception but half of them sees that ENSAK don't help their studnat to integrat the labor market 


#item 4: concurrence in labor market is increasing rapidly in the computer science field
summary(data$`item 4`)
prop.table(table(data$`item 4`))
barplot(table(data$`item 4`))
#everyone say that the concurrence in labor market is increasing rapidly in the computer science field

#item 5: concurrence in labor incite market students to develop their selves
summary(data$`item 5`)
prop.table(table(data$`item 5`))
barplot(table(data$`item 5`))
#97% of students say that the concurrence in labor incite market students to develop their selves

#item 6: the student should have something to differentiate from others 
summary(data$`item 6`)
prop.table(table(data$`item 6`))
barplot(table(data$`item 6`))
#93% of students say that the student should have something to differentiate from others

#item 7: the need for IT services in different sectors increases competition in the labor market
summary(data$`item 7`)
prop.table(table(data$`item 7`))
barplot(table(data$`item 7`))
#83% of students say the need for IT services in different sectors increases competition in the labor market

#item 8 : the existence of many competing companies in the labor market makes integration into the latter easy
summary(data$`item 8`)
prop.table(table(data$`item 8`))
barplot(table(data$`item 8`))
#54% see that existence of companies in labor market makes integrating the latter easy but 38% of students don't see that

#item 9 : Do ENSA KENITRA students find it difficult to integrate into the labor market because of competition?
summary(data$`item 9`)
prop.table(table(data$`item 9`))
barplot(table(data$`item 9`))
#we see an almost equitable distribution on the two answers: not agree and agree 

#question : is the competition in labor market impacts the integration of ENSAK's students into it?
summary(data$Question)
prop.table(table(data$Question))
barplot(table(data$Question))
#the majority of students see that the competition in labor market impact the integration into it .

#4.1: graphical exploration
barplot(table(data$Age),main = "age's histogram")
barplot(table(data$Genre),main = "Gender's histogram")
barplot(table(data$level), main="study level histogram")

#our sample is gender and level balanced 

# 4-2 hypotheses test 
# 4-2-1 uni-variate test

# we have essentially two Variables that cut our population : LEVEL and gender 
#gender and levels are quantitative variables, so we'll use khi-2 test
# H0 : there's no significant diff erence between men and women
# H1 : there's a significant difference  between men and women
chisq.test(table(data$Genre))
# -> p-value = 1 > 5% so H0 is confirmed , there's no significant difference between men and women 


# H0 : there's no significant difference between levels
# H1 : there's a significant difference  between levels
chisq.test(table(data$level))
# -> p-value = 1 > 5% so H0 is confirmed ,and there's no significant difference between levels

# -> we have an exploration study

# 4-2-2 bi-variate test

chisq.test(table(data$`item 1`))#p-value = 0.0004307 < 5%  H1 accepted
chisq.test(table(data$`item 2`))#p-value = 0.001914 < 5% H1 accepted
chisq.test(table(data$`item 3`))#p-value = 0.01504  < 5%H1 accepted
chisq.test(table(data$`item 4`))#p-value = 0.7389   >5% H0 accepted
chisq.test(table(data$`item 5`))#p-value = 0.0003086 < 5% H1 accepted
chisq.test(table(data$`item 6`))#p-value = 0.000007697 < 5% H1 accepted
chisq.test(table(data$`item 7`))#p-value = 0.000004646< 5% H1 accepted
chisq.test(table(data$`item 8`))#p-value = 0.003095< 5% H1 accepted
chisq.test(table(data$`item 9`))#p-value = 0.0004184< 5% H1 accepted
chisq.test(table(data$Question))#p-value = 0.1824 > 5% H0 accepted

# we are going to see  if there's a difference between men and women in each level
chisq.test(table(data$Genre,data$level))
# -> so, there's no significant difference between men and women  in each level.

#we gonna test the difference in term of age between levels
#age is quasinormal => we gonna do both the parametric and non parametric test
summary(aov(data$Age~data$level))
#p-value = 1.84e-08<5% there is a difference of age between levels
kruskal.test(data$Age~data$level)
#p-value = 1.076e-05<5% there is a difference of age between levels

#so there is a difference of age between levels


#we gonna test the difference in term of age between genders
#age is quasinormal => we gonna do both the parametric and non parametric test
t.test(data$Age~data$Genre)
#p-value = 0.7049> 5% =>H0 accepted there is no difference of age between gender
wilcox.test(data$Age~data$Genre)
#p-value =  0.7313>5% =>H0 accepted there is no difference of age between gender

#=> so there is a difference of age between gender


# TESTS ON QUESTION
library(questionr)
freq(data$Question)
# 61% voted for Yes  |   38% voted for No 
# since 61% > 38% we can conclude that the increasing of  concurrence  in labor market 
#impacts the integration of students in the latter

#we'll see the difference between the  Age and Question
#Age variable is Quasi-normal and question has two categories
wilcox.test(data$Age~data$Question) #p-value = 0.9719 > 5% H0 accepted
t.test(data$Age~data$Question)     #p-value = 0.8997 > 5% H0 accepted
# -> there's a significant difference between the Question and Age

# we will see the relation between level and the Question
chisq.test(table(data$level,data$Question))
# p-value = 88% H0 accepted !,there's no  dependence between the  question  and levels
# so   the perception can't be resumed to one level.
# -> so level don't influence the perception of ENSAK students on the impact of concurrence  on the integration to labor market





chisq.test(table(data$level,data$`item 1`))
# p-value = 63% > 5% =>H0 accepted,there's no dependence between the item 1 and levels
# The perception of students on the difficulty of integration to labor market does not depend on level

chisq.test(table(data$level,data$`item 2`)) 
# p-value = 27 % > 5% =>H0 accepted
# The perception of students on the  balance of offer and demand  in labor market  does not depend on level variable

chisq.test(table(data$level,data$`item 3`)) 
# p-value = 0.85 % < 5% H1 accepted
# we have a correspondence between the two variables,
# the perception of students on ensak's help to integrate labor market depend on level
#we'll see the  degree of this association
library(questionr)
levelitem <- table(data$level,data$`item 3`)
cramer.v(levelitem)
# CRAMER'S VALUE : 53%  lower than 60% higher than 20% the fields are moderately associated
#--> there's dependence between item 3 and the level variable
# from the results ,we can see that 66,7% agreed that ENSAK don't help their students to integrate the labor market

chisq.test(table(data$level,data$`item 4`))
# p-value = 23% > 5% =>H0 accepted
# there is no correspondence between level and item 4
#the perception of students on the fast increasing of concurrence in the labor market does not depend on level

chisq.test(table(data$level,data$`item 5`))
# p-value = 64% > 5% =>H0 accepted
# there is no correspondence between level and item 5
#the perception of students on the help of concurrence in inciting student to develop their selves does not depend on level

chisq.test(table(data$level,data$`item 6`))
# p-value = 61% > 5% =>H0 accepted
# there is no correspondence between level and item 6
#the perception of students on their need of differentiating from others  does not depend on level variable

chisq.test(table(data$level,data$`item 7`))
# p-value = 49% > 5% =>H0 accepted
# there is no correspondence between level and item 7
#The perception of students on the need of the impact of IT services in different sectors in increasing competition in labor market does nor depend on level variable

chisq.test(table(data$level,data$`item 8`))
# p-value = 65%  > 5% =>H0 accepted
# there is no correspondence between level and item 8
# the perception of students of the impact of existence of companies in labor market in making integration to it easier does not depend on level variable

chisq.test(table(data$level,data$`item 9`))
# p-value = 49% > 5% =>H0 accepted
# there is no correspondence between level and item 9
#the perception of students of the competition as a factor of difficult integrating labor market does not depend on level variable

chisq.test(table(data$level,data$Question))
#p-value > 5% => H0 accepted
#there is no correspondence between level and question
#the perception of students on the impact of the competition in integrating the labor market does not depend on levels







chisq.test(table(data$`item 1`,data$`item 4`))
#p-value = 0.02362 <  5 % => H1 accepted
# there is a correspondence between item 1 and item 4

item1_item4 <- table(data$`item 1`,data$`item 4`)
cramer.v(item1_item4)
#cramer values = 0.4561602 => 45% lower than 60% greater than 20% the fields are moderately associated

chisq.test(table(data$`item 1`,data$`item 5`))
#p-value = 0.1079 >  5 % => H0 accepted
# there is no correspondence between item 1 and item 5

chisq.test(table(data$`item 1`,data$`item 6`))
#p-value = 0.8143 >  5 % => H0 accepted
# there is no correspondence between item 1 and item 6

chisq.test(table(data$`item 1`,data$`item 7`))
#p-value = 0.2421 >  5 % => H0 accepted
# there is no correspondence between item 1 and item 7

chisq.test(table(data$`item 1`,data$`item 8`))
#p-value = 1.014e-06 <  5 % => H1 accepted
# there is no correspondence between item 1 and item 8

item1_item8 <- table(data$`item 1`,data$`item 8`)
cramer.v(item1_item8)
#cramer values = 0.7698125 => 33% greater than 60%  the fields are strongly associated

chisq.test(table(data$`item 1`,data$`item 9`))
#p-value = 0.562>  5 % => H0 accepted
# there is no correspondence between item 1 and item 9




chisq.test(table(data$`item 2`,data$`item 4`))
#p-value = 0.4184 >  5 % => H0 accepted
# there is no correspondence between item 2 and item 4

chisq.test(table(data$`item 2`,data$`item 5`))
#p-value = 0.501 >  5 % => H0 accepted
# there is no correspondence between item 2 and item 5

chisq.test(table(data$`item 2`,data$`item 6`))
#p-value = 0.6479 >  5 % => H0 accepted
# there is no correspondence between item 2 and item 6

chisq.test(table(data$`item 2`,data$`item 7`))
#p-value = 0.8307 >  5 % => H0 accepted
# there is no correspondence between item 2 and item 7

chisq.test(table(data$`item 2`,data$`item 8`))
#p-value = 0.8114 >  5 % => H0 accepted
# there is no correspondence between item 2 and item 8
chisq.test(table(data$`item 2`,data$`item 9`))
#p-value = 0.1909 >  5 % => H0 accepted
# there is no correspondence between item 2 and item 9


chisq.test(table(data$`item 3`,data$`item 4`))
#p-value = 0.09056 > 5 % => H0 accepted
# there is no correspondence between item 3 and item 4
chisq.test(table(data$`item 3`,data$`item 5`))
#p-value = 0.8937 > 5 % => H0 accepted
# there is no correspondence between item 3 and item 5
chisq.test(table(data$`item 3`,data$`item 6`))
#p-value = 0.2806 > 5 % => H0 accepted
# there is no correspondence between item 3 and item 6
chisq.test(table(data$`item 3`,data$`item 7`))
#p-value = 0.3533 > 5 % => H0 accepted
# there is no correspondence between item 3 and item 7
chisq.test(table(data$`item 3`,data$`item 8`))
#p-value = 0.6881 > 5 % => H0 accepted
# there is no correspondence between item 3 and item 8
chisq.test(table(data$`item 3`,data$`item 9`))
#p-value = 0.1694 > 5 % => H0 accepted
# there is no correspondence between item 3 and item 9


# 4-3 reliability test
# 4-3-1 Quantification of items :


data$`item 1` = as.character(data$`item 1`)
data$`item 1`[data$`item 1`=="Tout à fait d'accord"]=5 
data$`item 1`[data$`item 1`=="D'accord"]=4
data$`item 1`[data$`item 1`=="Neutre"]=3
data$`item 1`[data$`item 1`=="Pas d'accord"]=2
data$`item 1`[data$`item 1`=="Pas du tout d'accord"]=1
if(is.character(data$`item 1`)){
  data$`item 1`=as.integer(data$`item 1`)
}

data$`item 2` = as.character(data$`item 2`)
data$`item 2`[data$`item 2`=="Tout à fait d'accord"]=5 
data$`item 2`[data$`item 2`=="D'accord"]=4
data$`item 2`[data$`item 2`=="Neutre"]=3
data$`item 2`[data$`item 2`=="Pas d'accord"]=2
data$`item 2`[data$`item 2`=="Pas du tout d'accord"]=1
if(!is.numeric(data$`item 2`)){
  data$`item 2`=as.integer(data$`item 2`)
}

data$`item 3` = as.character(data$`item 3`)
data$`item 3`[data$`item 3`=="Tout à fait d'accord"]=5 
data$`item 3`[data$`item 3`=="D'accord"]=4
data$`item 3`[data$`item 3`=="Neutre"]=3
data$`item 3`[data$`item 3`=="Pas d'accord"]=2
data$`item 3`[data$`item 3`=="Pas du tout d'accord"]=1
if(!is.numeric(data$`item 3`)){
  data$`item 3`=as.integer(data$`item 3`)
}

data$`item 4` = as.character(data$`item 4`)
data$`item 4`[data$`item 4`=="Tout à fait d'accord"]=5 
data$`item 4`[data$`item 4`=="D'accord"]=4
data$`item 4`[data$`item 4`=="Neutre"]=3
data$`item 4`[data$`item 4`=="Pas d'accord"]=2
data$`item 4`[data$`item 4`=="Pas du tout d'accord"]=1

if(!is.numeric(data$`item 4`)){
  data$`item 4`=as.integer(data$`item 4`)
}

data$`item 5` = as.character(data$`item 5`)
data$`item 5`[data$`item 5`=="Tout à fait d'accord"]=5 
data$`item 5`[data$`item 5`=="D'accord"]=4
data$`item 5`[data$`item 5`=="Neutre"]=3
data$`item 5`[data$`item 5`=="Pas d'accord"]=2 
data$`item 5`[data$`item 5`=="Pas du tout d'accord"]=1
if(!is.numeric(data$`item 5`)){
  data$`item 5`=as.integer(data$`item 5`)
}

data$`item 6` = as.character(data$`item 6`)
data$`item 6`[data$`item 6`=="Tout à fait d'accord"]=5 
data$`item 6`[data$`item 6`=="D'accord"]=4
data$`item 6`[data$`item 6`=="Neutre"]=3
data$`item 6`[data$`item 6`=="Pas d'accord"]=2
data$`item 6`[data$`item 6`=="Pas du tout d'accord"]=1
if(!is.numeric(data$`item 6`)){
  data$`item 6`=as.integer(data$`item 6`)
}

data$`item 7` = as.character(data$`item 7`)
data$`item 7`[data$`item 7`=="Tout à fait d'accord"]=5 
data$`item 7`[data$`item 7`=="D'accord"]=4
data$`item 7`[data$`item 7`=="Neutre"]=3
data$`item 7`[data$`item 7`=="Pas d'accord"]=2
data$`item 7`[data$`item 7`=="Pas du tout d'accord"]=1
if(!is.numeric(data$`item 7`)){
  data$`item 7`=as.integer(data$`item 7`)
}

data$`item 8` = as.character(data$`item 8`)
data$`item 8`[data$`item 8`=="Tout à fait d'accord"]=5 
data$`item 8`[data$`item 8`=="D'accord"]=4
data$`item 8`[data$`item 8`=="Neutre"]=3
data$`item 8`[data$`item 8`=="Pas d'accord"]=2
data$`item 8`[data$`item 8`=="Pas du tout d'accord"]=1
if(!is.numeric(data$`item 8`)){
  data$`item 8`=as.integer(data$`item 8`)
}

data$`item 9` = as.character(data$`item 9`)
data$`item 9`[data$`item 9`=="Tout à fait d'accord"]=5 
data$`item 9`[data$`item 9`=="D'accord"]=4
data$`item 9`[data$`item 9`=="Neutre"]=3
data$`item 9`[data$`item 9`=="Pas d'accord"]=2
data$`item 9`[data$`item 9`=="Pas du tout d'accord"]=1
if(!is.numeric(data$`item 9`)){
  data$`item 9`=as.integer(data$`item 9`)
}

data$Question=as.character(data$Question)
data$Question[data$Question=="Oui"]=1
data$Question[data$Question=="Non"]=2
if(!is.numeric(data$Question)){
  data$Question=as.integer(data$Question)
}

# 4-3-2 Association test (correlation)
items1=data.frame(data$`item 1`,data$`item 2`,data$`item 3`)
items2=data.frame(data$`item 4`,data$`item 5`,data$`item 6`,data$`item 7`,data$`item 8`,data$`item 9`)
bartlett.test(items1)
bartlett.test(items2)

plot(items1)
plot(items2)
# we have  p-value <5% for both themes,so there is correlation between items 
# 4-3-3 degree of correlation : 
library(psy)
cronbach(items1)
cronbach(items2)
# alpha <60% , we need to clean items 
library(Rcmdr)
#after we measure the items reliability after deleting each items .
reliability(cov(data[,c("item 1","item 2","item 3")], use="complete.obs"))
reliability(cov(data[,c("item 4","item 5","item 6","item 7","item 8","item 9")], use="complete.obs"))

# reliability is not verified so we're going to work with each item separately .

#4-4 : factorial analysis (ACP):

library("FactoMineR")
library("factoextra")

# PCA () automatically normalize data ,so we don't need to normalize our Data before starting  PCA .
#    xi??? mean(x)
#    ----------
#       sd(x)

#4-4-1    ACP on theme 1 

data.theme1 = data[,(7:9)]
View(data.theme1)
PCA(data.theme1, scale.unit = TRUE, ncp = 5, graph = TRUE)
res1.pca <- PCA(data.theme1, graph = FALSE)
res1.pca

#4-4-1-1 EXTRACTION of eigenvalues / Variance of principal components

eig.val <- get_eigenvalue(res1.pca)
eig.val
# We are satisfied with 74% we can limit the  number of axes to 2 which  represents 74% of the variance  

#4-4-1-2 Visualization of eigenvalues

fviz_eig(res1.pca)

#4-4-1-3 Extraction of results for variables and their visualization

var1<-get_pca_var(res1.pca)#This function returns a list of elements containing all the results for the active variables

# Interpretations :
var1$coord   
# used to see the   coordination  between a variable a principal component
fviz_pca_var(res1.pca, col.var = "black")
# -> From the graph ,we don't  have items regrouped in one zone which explains we don't have positive correlation
# -> Also we don't have negative correlation either,since representation of items isn't symmetric  according to a principal component
var1$cos2
# -> Quality of representation with PCA is good  ,item2 IS   better
#the variable is positioned near the circumference of the correlation circle.
library("corrplot")
corrplot(var1$cos2, is.corr=FALSE)
# we have  a hight contrast of brown color in the intersection between Dim2 and item 2  which means a hight quality of representation 

#visualization of  the  quality of representation graph :
fviz_pca_var(res1.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#FFCF00", "#FC1E07"),
             repel = TRUE 
)
var1$contrib 
# -> contribution of a items for each  principal component
# 100% Dim1 = 49% item1 + 0,01% item2 50% item3 
# 100% Dim2 = 6% item1  + 88% item2 + 5%  item3
# 100% Dim3 = 44% item1 + 11% + 44% item3

# We can visualize the contribution of each item to the dimension using these functions.
fviz_contrib(res1.pca, choice = "var", axes = 1, top = 10)
#Dim1 represented with item 3 and item 1
fviz_contrib(res1.pca, choice = "var", axes = 2, top = 10)
#Dim2 represented with item 2
fviz_contrib(res1.pca, choice = "var", axes = 3, top = 10)
#Dim3 represented with item 3 and item 1

#visualization of  the  correlation graph :
fviz_pca_var(res1.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# demensions description 1,2,3
res1.desc <- dimdesc(res1.pca, axes = 1:2 , proba = 0.05)
res1.desc$Dim.1
res1.desc$Dim.2
res1.desc$Dim.3
# from the results of "dimdesc" we can decide which variable are to be removed in order 
# to simplify our study and be able to visualize  dimensions on two dimensions

#PC1 = Dim 1 : the hardness of integration to labor market under the non help of ensak to their integration
#PC2 = Dim 2 : the market is not balanced when it comes to offer and demand 

# using Rcmdr we ganna calculate the score of PC1 and PC2

library(Rcmdr)
shapiro.test(data.theme1$PC1)
#p-value = 0.175 > 5% there no difference between the normal distribution and the distribution of PC1
#=> PC1 follows the normal distribution

#LEVEL 
summary(aov(data.theme1$PC1~data$level))
# p-value =0.00615 < 5 % there a difference of perception to this PC between levels

#Gender
t.test(table(data$Genre,data.theme1$PC1))
#p-value = 0.0000001646 < 5% there's a significant difference of perception between genders

#AGE
cor.test(data$Age,data.theme1$PC1 , method = "pearson")
#p-value = 0.007873 <5% => there is an association between age and the perception of students on this PC 
cor.test(data$Age,data.theme1$PC1 , method = "spearman")
# cor = 0.4358964 < 5% there is a significant correlation between the age and PC1
#the correlation is low 

#4-4-2    ACP on theme 2

data.theme2 = data[,(10:15)]
View(data.theme2)
PCA(data.theme2, scale.unit = TRUE, ncp = 5, graph = TRUE)
res2.pca <- PCA(data.theme2, graph = FALSE)
res2.pca

#4-4-2-1 EXTRACTION of eigenvalues / Variance of principal components

eig.val <- get_eigenvalue(res2.pca)
eig.val
# We are satisfied with 74% we can limit the  number of axes to 4 which  represents 79% of the variance  

#4-4-2-2 Visualization of eigenvalues
fviz_eig(res2.pca)

#4-4-2-3 Extraction of results for variables and their visualization

var2<-get_pca_var(res2.pca)
#This function returns a list of elements containing all the results for the active variables

# Interpretations :
var2$coord   
# used to see the   coordination  between a variable a principal component
fviz_pca_var(res2.pca, col.var = "black")
# -> From the graph ,we   have items regrouped in one zone (item 5,4) and (items 6 and 9) which explains we have positive correlation between these items
# -> Also we don't have negative correlation ,since representation of items isn't symmetric  according to a principal component
var2$cos2
# -> Quality of representation with PCA is good for item 6 and 8 
# these variables are positioned near the circumference of the correlation circle.
library("corrplot")
corrplot(var2$cos2, is.corr=FALSE)
#A hight contrast of brown color  means a hight quality of representation of items
#hight contrast : (item 8, Dim 2) (item 9 , Dim 3)
#medium contrast : (item 7, Dim 1) (item 4 , Dim 4)
#low contrast : (item 5, Dim 1) (item 6 , Dim 2) 
#Very low contrast : The rest of combinations

#visualization of  the  quality of representation graph :
fviz_pca_var(res2.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#FFCF00", "#FC1E07"),
             repel = TRUE 
)

# contribution of a items for each  principal component :
var2$contrib 

# We can visualize the contribution of each item to the dimension using these function
fviz_contrib(res2.pca, choice = "var", axes = 1, top = 10)
#Dim1 represented with item 4,5,6 and 7 
fviz_contrib(res2.pca, choice = "var", axes = 2, top = 10)
  #Dim2 represented with item 6 and 8
fviz_contrib(res2.pca, choice = "var", axes = 3, top = 10)
#Dim3 represented with item 9
fviz_contrib(res2.pca, choice = "var", axes = 4, top = 10)
#Dim4 represented with item 4 and 5

#visualization of  the  correlation graph :
fviz_pca_var(res2.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# dimensions description 1,2,3,4,5,6
res2.desc <- dimdesc(res2.pca, axes = 1:2 , proba = 0.05)
res2.desc$Dim.1 
res2.desc$Dim.2
res2.desc$Dim.3
res2.desc$Dim.4
res2.desc$Dim.5
res2.desc$Dim.6

# from the results of "dimdesc" we can decide which variable are to be removed in order 
# to simplify our study and be able to visualize  dimensions on two dimensions

#PC1 : Dim1 : the existence and increasing of concurrence in labor market
#PC2 : Dim2 : the fact that  students should  differentiate from others and the existence of many companies in labor market makes integration into it  easy

library(Rcmdr)
shapiro.test(data.theme2$PC1)
#p-value =  0.8079 > 5% there no difference between the normal distribution and the distribution of PC1
#=> PC1 follows the normal distribution
shapiro.test(data.theme2$PC2)
#p-value =  0.2359 > 5% there no difference between the normal distribution and the distribution of PC1
#=> PC1 follows the normal distribution

#LEVEL 
summary(aov(data.theme2$PC1~data$level))
# p-value =0.338 > 5 % there no difference of perception to this PC between levels
summary(aov(data.theme2$PC2~data$level))
# p-value =0.116 > 5 % there no difference of perception to this PC between levels

#Gender
t.test(table(data$Genre,data.theme2$PC1))
#p-value = 1.329e-11 < 5% there's a significant difference of perception between genders
t.test(table(data$Genre,data.theme2$PC2))
#p-value = 1.329e-11 < 5% there's a significant difference of perception between genders

#AGE
cor.test(data$Age,data.theme2$PC1 , method = "pearson")
#p-value = 0.5382 > 5% => there is no association between age and the perception of students on this PC 
cor.test(data$Age,data.theme2$PC1 , method = "spearman")
# cor =  0.5364 > 5% there is no significant correlation between the age and PC1

cor.test(data$Age,data.theme2$PC2 , method = "pearson")
#p-value = 0.008444 < 5% => there is no association between age and the perception of students on this PC 
cor.test(data$Age,data.theme2$PC2 , method = "spearman")
# cor =  0.009003 < 5% there is no significant correlation between the age and PC1
# the correlation is low 


#theme1 
# HCPC


res1.hcpc <- HCPC(res1.pca, graph = FALSE)

fviz_dend(res1.hcpc, 
          cex = 0.7,                     
          palette = "jco",               
          rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco",           
          labels_track_height = 0.8      
)

# -> The dendrogram suggests us 3 group solution

#Visualize individuals and color by groups

fviz_cluster(res1.hcpc,
             repel = TRUE,            
             show.clust.cent = TRUE, 
             palette = "jco",         
             ggtheme = theme_minimal(),
             main = "Factor map"
)
#3D graphic combining hierarchical classification and factor plan

# Principal components + tree
plot(res1.hcpc, choice = "3D.map")

#  HCPC can use the following functions :

# 1 data with an additional column called cluster containing the groups.
res1.hcpc$data.clust
# from  the result we can see which group ,each individual is associated to 

# 2 the variables describing the groups
res1.hcpc$desc.var$quanti

# from the results we can see that : 
 #items 1 , item 2 and item 3 are significantly associated to cluster 1 and 3
 #and item 2 is associaetd to cluster 2 

#3 les axes décrivant les groupes
res1.hcpc$desc.axes$quanti
#The results above indicate that individuals in Clusters 1 and 3 have high coordinates on the 1,3 axes.
#Cluster 2 individuals have high coordinates on axes 2 and 3.

#4 the most typical individuals of each group
res1.hcpc$desc.ind$para
#For each cluster, the top 5 individuals closest to the center of the cluster are displayed.
#These individuals are called paragons. The distance between each individual and the center of the group is provided.




# Theme 2
# HCPC


res2.hcpc <- HCPC(res2.pca, graph = FALSE)

fviz_dend(res2.hcpc, 
          cex = 0.7,                     
          palette = "jco",               
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",           
          labels_track_height = 0.8     
)

# -> The dendrogram suggests us 6 groups solution

#Visualize individuals and color by groups

fviz_cluster(res2.hcpc,
             repel = TRUE,            
             show.clust.cent = TRUE, 
             palette = "jco",         
             ggtheme = theme_minimal(),
             main = "Factor map"
)
#3D graphic combining hierarchical classification and factor plan

# Principal components + tree
plot(res2.hcpc, choice = "3D.map")

#  HCPC can use the following functions :

# 1 data with an additional column called cluster containing the groups.
res2.hcpc$data.clust
# from  the result we can see which group ,each individual is associated to 

# 2 the variables describing the groups
res2.hcpc$desc.var$quanti

#3 Axes describing Clusters
res2.hcpc$desc.axes$quanti

#4 the most typical individuals of each Cluster
res2.hcpc$desc.ind$para



#Step 5 CONCLUSION :

# From the uni-variate and bi-variate analysis that we did :
# students see that  the increasing of  concurrence  in labor market highly impacts their integration into it.
# the thing that confirm our statistical  study

# and we mention that in their perception there's no significant difference between men and women 
# also it doesn't depend on level either





