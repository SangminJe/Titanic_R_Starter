# 1. Split the data back into a train set and a test set
train <- all[1:891,]
test <- all[892:1309,]

set.seed(754)

# 2. Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + title + FsizeD,
                         data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


# 3. Variance Importance Check
# 3-1. Get Importance
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[,'MeanDecreaseGini'],2))

# 3-2. rank in Importance
rankImportance <- varImportance %>% 
  mutate(rank = paste0('#', dense_rank(desc(Importance))))

# 3-3. Plot Rank
rankImportance %>% 
  ggplot(mapping = aes(x = reorder(Variables,Importance), y = Importance, fill = Importance))+
  geom_bar(stat = 'identity')+
  geom_text(aes(x = Variables,y = 0.5, label = rank), hjust = 0, size = 3, color = 'red')+
  coord_flip()

# 4. Real Prediction
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerId = test$PassengerId, Survived= prediction)

# Write Solution
write.csv(solution, file = 'rf_model.csv', row.names = F)
