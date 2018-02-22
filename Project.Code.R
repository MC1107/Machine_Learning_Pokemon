#Question 1:
#load the dataset: 
pokemon <- read.csv("/Users/yanjunchen/Desktop/Pokemon_TypeAll.csv", header=TRUE)

#rename variables:
type <- pokemon$TypeCombine
leg <- pokemon$Legendary

#recode categorical variables into numbers. Attach them to the original dataset:
leg.num = rep(NA, length(leg))
leg.num[leg=="TRUE"] = 1
leg.num[leg=="FALSE"] = 0
leg.num

type.num = rep(NA, length(type))
type.num[type=="Grass"] = 1
type.num[type=="Fire"] = 2
type.num[type=="Water"] = 3
type.num[type=="Bug"] = 4
type.num[type=="Normal"] = 5
type.num[type=="Poison"] = 6
type.num[type=="Electric"] = 7
type.num[type=="Ground"] = 8
type.num[type=="Fairy"] = 9
type.num[type=="Fighting"] = 10
type.num[type=="Psychic"] = 11
type.num[type=="Rock"] = 12
type.num[type=="Ghost"] = 13
type.num[type=="Ice"] = 14
type.num[type=="Dragon"] = 15
type.num[type=="Dark"] = 16
type.num[type=="Steel"] = 17
type.num[type=="Flying"] = 18
type.num

pok.bind=cbind(pokemon, type.num, leg.num)

#LDA for the full model(without Generation):
bind.pok=cbind(pokemon, type.num, leg.num)
na.pok = na.omit(bind.pok)

set.seed(1)
train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
pok.train = na.pok[train, ]
pok.test= na.pok[-train, ]
test.type = pok.test$type.num

#for sed.seet 50
er_full=0
for(i in 1:50){
  set.seed(i)
  train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
  pok.train = na.pok[train, ]
  pok.test= na.pok[-train, ]
  test.type = pok.test$type.num
  lda.fit = lda(type.num~HP+Attack+Defense+Speed+Sp.Def+Sp.Atk+leg.num, data = pok.train)
  lda.pred = predict(lda.fit, pok.test)
  class = lda.pred$class
  er_full[i] = mean(class !=test.type)
  
}
mean(er_full)

#best subset selection:
fit.best= regsubsets(type.num~HP+Attack+Defense+Sp.Atk+Sp.Def+ Speed+Generation+leg.num, data= pok.bind)
best.sum = summary(fit.best)
best.sum
#plot cp, bic, adjr2
par(mfrow=c(2,2))
plot(best.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(best.sum$cp) # 6
points(6,best.sum$cp[6], col="red", cex =2, pch =20)
plot(best.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(best.sum$bic) # 3
points(4, best.sum$bic[4], col="red", cex =2, pch =20)
plot(best.sum$adjr2, xlab = "Number of Varibales", ylab = "Adjusted Rsq", type = "l")
which.max(best.sum$adjr2) # 6 
points(6, best.sum$adjr2[6], col="red", cex =2, pch =20)
#forward selection
fit.forward= regsubsets(type.num~HP+Attack+Defense+Sp.Atk+Sp.Def+Speed+Generation+leg.num, data= pok.bind, method = "forward")
forward.sum = summary(fit.forward)
forward.sum
which.min(forward.sum$cp) #6
which.min(forward.sum$bic) #4
which.max(forward.sum$adjr2) #6

#backward selection
fit.backward= regsubsets(type.num~HP+Attack+Defense+Sp.Atk+Sp.Def+ Speed+Generation+leg.num, data= pok.bind, method = "backward")
back.sum = summary(fit.backward)
back.sum
which.min(back.sum$cp) #6
which.min(back.sum$bic) #4
which.max(back.sum$adjr2) #6

#lasso
set.seed(1)
train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
pok.train = na.pok[train, ]
pok.test= na.pok[-train, ]
test.type = pok.test$type.num
train.type = pok.train$type.num
x = model.matrix(type.num~HP+Attack+Defense+Sp.Atk+Sp.Def+Speed+Generation+leg.num, data= pok.train)
fit.lasso = glmnet(x,train.type,alpha = 1 )
cv.lasso=cv.glmnet(x,train.type, alpha=1) 
#plot(cv.lasso)
bestlam.lasso=cv.lasso$lambda.min 
bestlam.lasso
#show coefficients
lasso.pred = predict(fit.lasso, type="coefficients", s=bestlam.lasso, newx =pok.test)
lasso.pred 

set.seed(1)
train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
pok.train = na.pok[train, ]
pok.test= na.pok[-train, ]
test.type = pok.test$type.num

#LDA cross validation for model 4 forward:
er4f=0
for(i in 1:50){
  set.seed(i)
  train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
  pok.train = na.pok[train, ]
  pok.test= na.pok[-train, ]
  test.type = pok.test$type.num
  lda.fit = lda(type.num~Attack+Defense+leg.num, data = pok.train)
  lda.pred = predict(lda.fit, pok.test)
  class = lda.pred$class
  er4f[i] = mean(class !=test.type)
}

#LDA cross validation for model 4 best subset:
er4b=0
for(i in 1:50){
  set.seed(i)
  train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
  pok.train = na.pok[train, ]
  pok.test= na.pok[-train, ]
  test.type = pok.test$type.num
  lda.fit = lda(type.num~Defense+Speed+leg.num, data = pok.train)
  lda.pred = predict(lda.fit, pok.test)
  class = lda.pred$class
  er4b[i] = mean(class !=test.type)
}

#LDA for best subset model 5:
er5=0
for(i in 1:50){
  set.seed(i)
  train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
  pok.train = na.pok[train, ]
  pok.test= na.pok[-train, ]
  test.type = pok.test$type.num
  lda.fit = lda(type.num~Attack+Defense+Speed+leg.num, data = pok.train)
  lda.pred = predict(lda.fit, pok.test)
  class = lda.pred$class
  er5[i] = mean(class !=test.type)
}

#Error rate for model 6:
er6=0
for(i in 1:50){
  set.seed(i)
  train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
  pok.train = na.pok[train, ]
  pok.test= na.pok[-train, ]
  test.type = pok.test$type.num
  lda.fit = lda(type.num~HP+Attack+Defense+Speed+leg.num, data = pok.train)
  lda.pred = predict(lda.fit, pok.test)
  class = lda.pred$class
  er6[i] = mean(class !=test.type)
}

#LDA cross validation for best subset model 7:
er7=0
for(i in 1:50){
  set.seed(i)
  train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
  pok.train = na.pok[train, ]
  pok.test= na.pok[-train, ]
  test.type = pok.test$type.num
  lda.fit = lda(type.num~HP+Attack+Defense+Speed+Sp.Atk+leg.num, data = pok.train)
  lda.pred = predict(lda.fit, pok.test)
  class = lda.pred$class
  er7[i] = mean(class !=test.type)
}

boxplot(er_full, er7, er6, er5, er4f, er4b,ylim = c(0.8, 0.9), las=2,names = c("full model", "model 7"," model6", "model5", "model4_for", "model4_best" ))

#Bagging for model 7: 
library(randomForest)
set.seed(1)
train = sample(dim(na.pok)[1], dim(na.pok)[1] / 2)
pok.train = na.pok[train, ]
pok.test= na.pok[-train, ]
bag = randomForest(type.num ~ HP + Attack + Defense + Sp.Atk + Speed + Generation + leg.num, data = pok.train, importance=TRUE)
bag
yhat.bag = predict(bag, newdata = pok.test) 
mean((yhat.bag-pok.test$type.num)^2)
importance(bag)

#Refit bagging using important variables:
bag2 = randomForest(type.num~HP+ Sp.Atk + Generation+ leg.num, data = pok.train, importance=TRUE)
bag2
yhat.bag2 = predict(bag2, newdata = pok.test) 
mean((yhat.bag2-pok.test$type.num)^2)
importance(bag2)


#Question2:
#10-fold Logistic Regression:
attach(pok.bind)
library(boot) 
set.seed(1) 
cv.error.10=rep(0,10) 
for (i in 1:10) { 
  glm.fit=glm(Legendary~.,family="binomial") 
  cv.error.10[i]=cv.glm(Pokemon,glm.fit,K=10)$delta[1] 
} 
cv.error.10
mean(cv.error.10)

#Lasso Model:
x <- model.matrix(Legendary~., data=pok.bind)[,-1]
y <- Pokemon$Legendary
lambda <- 10^seq(10, -2, length = 100)
library(glmnet)

train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1,lambda=lambda)
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = bestlam)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)
lasso.coef=coef(lasso.mod)[,1]
lasso.coef[lasso.coef!=0]

# Fitting Classification Trees
library(tree)
library(ISLR)
attach(Carseats)
#All-9 variables model - no need to put on report
tree.pokemonA=tree(Legendary~Type.1+Type.2+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation,pdata)
summary(tree.pokemonA)
plot(tree.pokemonA)
text(tree.pokemonA,pretty=0)
tree.pokemonA

#test error for tree method
set.seed(1)
train=sample(1:nrow(pdata), 400)
pdata.test=pdata[-train,]
Legendary.test=pdata$Legendary[-train]

tree.pokemonA=tree(Legendary~Type.1+Type.2+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation,pdata,subset=train)
#remove type2 and generation
tree.pokemon7=tree(Legendary~Type.1+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed,pdata,subset=train)
#then remove type1 and defense, special defense
tree.pokemon4=tree(Legendary~HP+Attack+Sp..Atk+Speed,pdata,subset=train)

#Predict for each fit
tree.predA=predict(tree.pokemonA,pdata.test,type="class") # type = class for classification
tree.pred7=predict(tree.pokemon7,pdata.test,type="class") # type = class for classification
tree.pred4=predict(tree.pokemon4,pdata.test,type="class") # type = class for classification

table(tree.predA,Legendary.test)
table(tree.pred7,Legendary.test)
table(tree.pred4,Legendary.test)
#notes only
#> table(tree.predA,Legendary.test) 
tree.predA False True
False   353   14
True     13   21
(353+21)/400 #93.5%
#> table(tree.pred7,Legendary.test)
tree.pred7 False True
False   355   14
True     10   21
(355+21)/400 #94%
#> table(tree.pred4,Legendary.test)
tree.predN4 False True
False   350    9
True     15   26
(350+26)/400 #94%
#above is without pruning


#tree pruning
set.seed(2)
#use cv to choose the pruning alpha(pruning parameter)
#change to correct model below
cv.pdata=cv.tree(tree.pokemon4,FUN=prune.misclass)
names(cv.pdata)
cv.pdata
par(mfrow=c(1,2))
plot(cv.pdata$size,cv.pdata$dev,type="b")
plot(cv.pdata$k,cv.pdata$dev,type="b")

#change best= to the best node from answers above
prune.pdatas=prune.misclass(tree.pokemonN4,best=7)
plot(prune.pdatas)
text(prune.pdatas,pretty=0)
tree.pred=predict(prune.pdatas,pdata.test,type="class")
table(tree.pred,Legendary.test)
#> table(tree.pred,Legendary.test)
tree.pred False True
False   358   15
True      7   20
(358+20)/400 #0.945 
#better tree for interpretation, with very slightly increased accuracy

# Bagging and Random Forests
library(randomForest)
set.seed(3)
#full model are commentted out - use correct model
#bag.pdata=randomForest(Legendary~Type.1+Type.2+Attack+Sp..Atk+Defence+Sp..Def+Speed+Generation,data=pdata,subset=trainp,mtry=9,importance=TRUE) #this equals to bagging since 9=9 variables
bag.pdata=randomForest(Legendary~HP+Attack+Sp..Atk+Speed,pdata,subset=train,mtry=4,importance=TRUE)
bag.pdata 
yhat.bag = predict(bag.pdata,newdata=pdata.test)#use bagging for prediction
table(yhat.bag, pdata.test$Legendary)
#notes here
#> table(yhat.bag, pdata.test$Legendary)
yhat.bag False True
False   356   13
True      9   22
(355+22)/400 #0.9425 

set.seed(4)
#mtry here is recommended to be p^(1/2), but depends on the best result
rf.pdata=randomForest(Legendary~HP+Attack+Sp..Atk+Speed,pdata,subset=train,mtry=2,importance=TRUE)
rf.pdata
yhat.bag = predict(rf.pdata,newdata=pdata.test)
table(yhat.bag, pdata.test$Legendary)

#> table(yhat.bag, pdata.test$Legendary)
yhat.bag False True
False   357   15
True      8   20
(355+22)/400 #0.945

#measure of importance of tree based method
importance(rf.pdata)
#Generation is the least important
varImpPlot(rf.pdata)
#special attack and attack are by far the most important

