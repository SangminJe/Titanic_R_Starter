#순서는 Data Check -> Feature Engineering -> Missingness Imputation -> Modeling 순으로 진행한다.

# 1. Age Feature Engineering
all$title <- gsub('(.+, )|(\\..*)','', all$Name)
table(all$Sex, all$title)

# 1-1. Title
rare_title <- c('Capt','Col','Dona','Don','Dr','Jonkheer',
                'Lady','Major','Rev','Sir','the Countess')
all$title[all$title=='Mlle'] <- 'Miss'
all$title[all$title=='Ms'] <- 'Miss'
all$title[all$title=='Mme'] <- 'Mrs'
all$title[all$title %in% rare_title] <- 'rare title'

# 1-2. Surname
head(all$Name)
head(strsplit(all$Name, split='[,.]')[[1]][1])
##이렇게 하면 하나씩 밖에 값을 얻지 못하므로, sapply 함수를 사용해서 전체를 끌고온다
##루프문과 어떻게 보면 비슷하다고 볼 수 있겠다.
##결과값은 Vector형식으로 뱉어준다.

all$Surname <- sapply(all$Name,  
       function(x) strsplit(x, split = '[,.]')[[1]][1])

#2. FamilySize
#2-1. Sibsp에 에 따른 생존여부
all$Fsize <- all$SibSp + all$Parch + 1

all[1:891,] %>% ggplot(mapping=aes(Fsize, fill=Survived))+
  geom_bar(position='dodge') # Stack이 안되도록 하는 명령어

