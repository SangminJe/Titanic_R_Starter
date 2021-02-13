#### package load
install.packages('VIM')
install.packages('gridExtra')
install.packages('corrplot')
install.packages('dplyr')
install.packages('ggplot2')

library(dplyr)
library(ggplot2)
library(VIM) # Visualzation and imputation of Missing Values
library(corrplot) # corrplot 


### Variable Definition
# survival	Survival	0 = No, 1 = Yes
# pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
# sex	Sex	
# Age	Age in years	
# sibsp	# of siblings / spouses aboard the Titanic	
# parch	# of parents / children aboard the Titanic	
# ticket	Ticket number	
# fare	Passenger fare	
# cabin	Cabin number	
# embarked	Port of Embarkation


#### 1. Data Load
train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')
all <- bind_rows(train,test) # 두개의 데이터를 합친다.
all_Origin <- all

#### 2. Count Survival
train %>% count(Survived)
surv <- train %>% count(Survived) %>% filter(Survived == 1) %>% .$n
nosurv <- train %>% count(Survived) %>% filter(Survived == 0) %>% .$n

#### 3. Data Check
str(all)
dim(all)
summary(all)
glimpse(all)

# a) Check Embarked
table(is.na(all$Embarked))

all[nchar(all$Embarked) <1, ] #62, 830 Data Missing
all[nchar(all$Embarked) <1,]$Embarked <- NA # NA 처리해주기
all[c(62,830),]$Embarked


all[-c(62,830),] %>%
  ggplot(mapping= aes(Embarked,Fare))+
  geom_boxplot()+


# Check Cabin Data Missing Processing
# ** Cabin 변수를 보면 분명 공란이 많은데 NA로 인식하지 않는다.
table(is.na(all$Cabin))

# 따라서 해당 변수에서 len이 0이하이면 NA로 바꿔주는 작업을하려고 한다.
# ** length는 데이터 Set의 길이를 반환한다.
# ** nchar는 데이터 각 Row의 길이를 반환한다.
all$Cabin <- ifelse(nchar(all$Cabin)>0,all$Cabin,NA)
head(all)


#### 4. Change int to Factor
# Survived, Pclass, Sex, Embarked는 Factor의 요소라고 볼 수 있음
# Factor로 바꾸기 전에 먼저 Missing Data를 체크해야함@!

all <- all %>% mutate(Survived = factor(Survived),
               Pclass = factor(Pclass),
               Sex = factor(Sex),
               Embarked = factor(Embarked))

# Survived Ratio on Sex
ggplot(data = all[1:891,], mapping = aes(Sex, fill=Survived))+
 geom_bar(position='fill')+ # 그래프를 채워서 비율로 표현한다는 뜻
 ylab('Survival Rate')+
 geom_hline(yintercept = sum(train$Survived)/nrow(train), col = 'white', lty = 2)




# aggr function을 사용해서 Missing Value 시각화
aggr(all, prop = F, numbers = T) # 확률 대신 정수로 표현하겠다는 뜻
# 이 패키지가 아니라면 일일이 is.na를 체크해야 할텐데.. 편리한듯

aggr(all, prop = F, numbers = T,
     col = c('ivory', 'orange'),
     ylab = c('Mssing Num','Missng Relation'),
     cex.axis = .5)  # 위 두 가지 그래프를 합쳐서 볼 수 있음

# Spinogram / SpinePlot
# 두 개 변수의 Missing 관계를 살펴보기 위한 차트
# x 변수에는 Categorical, y 변수에는 Continuous 자료형이 배치
spineMiss(all[,c('Survived','Age')])

# Mosaic Plot
mosaicMiss(all[,c('Survived','Sex','Cabin')], highlight = 3,
           plotvars = 1:2, miss.labels = F)

# 1) Fare에 대한  Imputation
# a. Fare Value Indexing
all[is.na(all$Fare),] # 1044
hist(all$Fare,breaks = 50)
d <- density(all$Fare, na.rm =T) # density function 그리기
plot(d)

# b. Pclass = 3 에 대한 Mean Imputation
all %>% group_by(Pclass) %>% 
  summarise(mean_Fare = mean(Fare, na.rm= T)) %>% 
  .[3,2] # 13.3
# 이것은 온전히 분석가의 주관에 의한 것이다. 하지만 이 두가지가 상관관계가 있음을 먼저 밝히면 좋지 않을까?


# b-1. Pclass와 Fare 간의 Boxplot 그리기
all %>% ggplot(mapping = aes(Pclass,Fare)) + 
  geom_boxplot(fill = 'skyblue')
# 분명히 Pclass와 Fare 간에는 상당한 관련성이 있음

# b-2. Embarked와 Fare 간의 관계
ggplot(data = all, mapping = aes(Embarked, Fare))+
  geom_boxplot()


# b-3. 

# d. 
library(corrplot) # corrplot 


cor(all)



# EDA
# 1) 