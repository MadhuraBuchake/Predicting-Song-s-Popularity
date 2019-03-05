data= read.csv("C:/Users/kalya/Desktop/Masters/CS 4821 Data Mining/music.csv")
music=subset(data, select = -c(songtitle,artistname,songID,artistID,timesignature_confidence,tempo_confidence,key_confidence))
# scaling of data and later adding the response variable
music_scaled = as.data.frame(scale(music))
music_scaled$Top10 = music$Top10
music_scaled$Top10 =as.factor(music$Top10)
set.seed(5427)
nFolds= 10
folds= cvFolds(nrow(music_scaled),K=nFolds,R=1)
rf_error= rep(NA,nFolds)
rf_acc = rep(NA,nFolds)
for (i in 1:nFolds) {
train= music_scaled[folds$subsets[folds$which != i], ]
test=music_scaled[folds$subsets[folds$which == i], ]
rf_model = randomForest(Top10 ~ ., data=train, method = "class")
rf_test = predict(rf_model, test, type = "class")
rf_acc[i]= sum(rf_test==test[,32]) / nrow(test)
rf_error[i] = 1 - rf_acc[i]
rf_pred= prediction(as.numeric(rf_test), as.numeric(test[,32]))
rf_auc=performance(rf_pred, "auc")
}
cat("Accuracy of Rf model:", mean(rf_acc),"\n")
## Accuracy of Rf model: 0.865195
cat("Error of RF model :",mean(rf_error),"\n")
## Error of RF model : 0.134805
cat("AUC of RF model :", unlist(slot(rf_auc,"y.values")),"\n")
## AUC of RF model : 0.5745614
#(8g) SVM
set.seed(123)
nFolds = 10
folds= cvFolds(nrow(music_scaled),K=nFolds,R=1)
svm_error = rep(NA,nFolds)
svm_acc = rep(NA,nFolds)
for (c in c(0.01,0.1,1,10,100)){
for (i in 1:nFolds) {

svm_model= svm(Top10 ~., data = train, kernel='radial',cost = c)
svm_test = predict(svm_model, test[,-32])
svm_acc[i] = sum(svm_test==test[,32]) / nrow(test);
svm_error[i] = 1 - svm_acc[i];
}
svm_pred = prediction(as.numeric(svm_test), as.numeric(test[,32]));
svm_auc = performance(svm_pred, "auc")
cat("Accuracy for ",c, "is",mean(svm_acc),"\n")
cat("Error for",c, "is",mean(svm_error),"\n")
cat("AUC for",c, "is", unlist(slot(svm_auc,"y.values")),"\n")
}
