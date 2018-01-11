library(caret)
library(ROCR)
library(glmnet)
library(dplyr)


df = read.csv('/Users/jledoux/Documents/projects/Saber/hof/data/all_past.csv')
df_display = read.csv('/Users/jledoux/Documents/projects/Saber/hof/data/all_past_orig.csv')
currents_model = read.csv('/Users/jledoux/Documents/projects/Saber/hof/data/all_currents_model.csv')
currents_display = read.csv('/Users/jledoux/Documents/projects/Saber/hof/data/all_currents_display.csv')
master = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/master.csv')

df$X = NULL
currents_model$X = NULL
currents_display$X= NULL 

# train / validation split 
train_pct = 0.75
train_nrow = floor(train_pct*nrow(df))
train = df[1:train_nrow,]
test = df[(train_nrow+1):nrow(df),]
trainids = train$id 
testids = test$id 
test$id = NULL
train$id = NULL

# train the model
x_train = train[,!names(train) %in% c("inducted")]
x_test = test[,!names(test) %in% c("inducted")]
y_train = train$inducted
y_test = test$inducted

cv = cv.glmnet(x=as.matrix(x_train), y=as.matrix(y_train), family="binomial",alpha=1)
plot(cv, xvar="lambda")
best_lambda = cv$lambda.min

preds = predict(cv, newx=as.matrix(x_test), s="lambda.min",type="response")

y = as.data.frame(y_test)
y$pred = preds
names(y) = c("y","pred")

pred = prediction(y$pred, y$y)
perf = performance(pred,"tpr","fpr")
plot(perf,col="black",lty=3, lwd=3)

# calculating AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
print(auc)

# print all coefficients 
coef(cv,s='lambda.min')
# now just the non-zero ones
coef(cv, s = "lambda.min")[which(coef(cv, s = "lambda.min") != 0)]
colnames(x_test)[which(coef(cv, s = "lambda.min") != 0)-1] # -1 because of intercept column 

# those results look fine, so now predict current players hof odds using this same model
cur_ids = currents_model$id
currents_model$inducted = NULL
currents_model$id = NULL
cur_preds = predict(cv, newx=as.matrix(currents_model), s="lambda.min",type="response")

currents_display$playerID = currents_display$id
currents_display$id = NULL
currents_display = left_join(currents_display, master[,c('playerID','nameFirst','nameLast')],by='playerID')
currents_display$Name = paste0(currents_display$nameFirst,' ',currents_display$nameLast)
currents_display$prob_hof = cur_preds

df_display = left_join(df_display, master[,c('playerID','nameFirst','nameLast')],by=c('id'='playerID'))
df_display$Name = paste0(df_display$nameFirst,' ',df_display$nameLast)

tail(currents_display[order(currents_display$prob_hof),c('Name','playerID','prob_hof')], 30)

write.csv(currents_display, '/Users/jledoux/Documents/projects/Saber/hof/data/currents_with_preds_logitcv.csv')
write.csv(df_display, '/Users/jledoux/Documents/projects/Saber/hof/data/all_past_orig.csv')

