# 4. Check Cabin Data Missing Processing

aggr(all, prop = F, numbers = T,cex.axis = 0.4) # 확률 대신 정수로 표현하겠다는 뜻

aggr(all, prop = F, numbers = T,
     col = c('ivory', 'orange'),
     ylab = c('Mssing Num','Missng Relation'),
     combine = T,
     cex.axis = .5)  # 위 두 가지 그래프를 합쳐서 볼 수 있음
#Embarked,Fare,Cabin,Age,Survived에결측치가 존재함


# 1) Check Embarked
table(is.na(all$Embarked))

# Data NA 변환 확인
all[is.na(all$Embarked),]

# 아래 결과를 보면 
all[-c(62,830),] %>%
  filter(Pclass == 1) %>% 
  group_by(Embarked) %>% 
  summarise(mean_Fare = mean(Fare, na.rm =T),
            n = n())

# 시각화로 확인 
all[-c(62,830),] %>%
  filter(Pclass == 1) %>% 
  ggplot(mapping= aes(Embarked,Fare,fill = Embarked))+
  geom_boxplot(alpha = 0.7)+
  geom_hline(yintercept = 80)

# C로 치환하는게 로지컬 하다는 생각이 든다.
all[is.na(all$Embarked),'Embarked'] <- 'C'

all[c(62,830),] # 잘변환되었음

# 2) Fare에 대한  Imputation

# a. Fare Value Indexing
all[is.na(all$Fare),] # 1044

# Age와 Fare과 상관관계가 있을까?
ggplot(data=all,aes(Age,Fare))+
  geom_point()+
  geom_smooth(method='lm', formula = Fare ~ Age)+
  ylim(0,300)
# **추세는 없어보임

# 회귀분석 결과
fit <- lm(data = all, Fare ~ Age)
summary(fit)
# **유의미한 결과가 있다..?

# prediction 진행
new_test = data.frame(
  Age = c(60.5)
)

predict(fit, newdata = new_test) # 57.90377

# 성별에 따른 Fare의 차이
all %>% 
  group_by(Sex) %>% 
  summarise(mean_Fare = mean(Fare, na.rm =T),
            n = n()) %>% 
  ggplot(mapping=aes(Sex, mean_Fare, fill = Sex))+
  geom_col() +
  geom_text(aes(label = n), position = position_dodge(width = 0.8), vjust = -0.3) # n의 숫자
# ** 여자가 45$, 남자가 28$ 정도로 여자가 더 높은 금액을 지불하는 경향


# Pclass에 따른 금액
all %>% 
  group_by(Pclass) %>% 
  summarise(mean_Fare = round(mean(Fare, na.rm =T),1)) %>% 
  ggplot(aes(Pclass,mean_Fare))+
  geom_col()+
  geom_text(aes(label = mean_Fare), position = position_dodge(width = 0.8), vjust = -.3)
# 3등석이므로 13.3

# Embarked에 따른 금액
all %>% 
  group_by(Embarked) %>% 
  summarise(mean_Fare = round(mean(Fare, na.rm =T),1)) %>% 
  ggplot(aes(Embarked,mean_Fare))+
  geom_col()+
  geom_text(aes(label = mean_Fare), position = position_dodge(width = 0.8), vjust = -.3)
# S이므로 27.4

# Age, Plcass, Embarked 관점에서 보게 되는 Imputing 값이 다르다.

# b-1. Pclass와 Fare 간의 Boxplot 그리기
all %>% ggplot(mapping = aes(Pclass,Fare)) + 
  geom_boxplot(fill = 'skyblue')
# 분명히 Pclass와 Fare 간에는 상당한 관련성이 있음

# b-2. Embarked와 Fare 간의 관계
ggplot(data = all, mapping = aes(Embarked, Fare))+
  geom_boxplot()

# b-3. Embarked와 Pclass를 반영해서 mean과 median 값을 구해보자
all %>% 
  group_by(Embarked, Pclass) %>% 
  summarise(mean_Fare = mean(Fare, na.rm =T), # 14.4
            median_Fare = median(Fare, na.rm=T), # 8.05
            n =n())
## 1개의 값이고 Imputation에 큰 영향이 없으므로 간단하게 마무리한다.
## Embarked와 Pclass의 기준으로 최종 Inputation을 하는 게 로지컬해보인다.
## 그래서 중앙 값인 8.05로 대체하기로 하자
## 나중에 Imputation에 대한 방법론은 제대로 학습할 필요가 있을 것 같다.

all[is.na(all$Fare),'Fare'] <- 8.05
all[1044,]

# 3) Age에 대한 Imputation
# Age 안에는 총 250개 정도의 결측값이 있음

ggplot(data = all, mapping=aes(Age))+
  geom_density(fill='blue')+
  geom_vline(xintercept = median(all$Age,na.rm=T), lty = 2, col ='red')

head(all,10)

# a) 나이에 대한 Imputation을 위한 Name Column 작업
sum(is.na(all$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'title','Surname','Fsize','FsizeD')

all[factor_vars] <- lapply(all[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(all[, !names(all) %in% c('PassengerId','Name','Ticket','Cabin','Fsize','Surname','Survived')], method='rf') 

mice_output <- complete(mice_mod)

par(mfrow=c(1,2))
hist(all$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
all$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(all$Age))

aggr(all, numbers = T, combined= T,ces.axis =.8)

