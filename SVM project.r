cat('\nStep1 : Library Inclusion')
library(kernlab)
library(hmeasure)
library(caret)

cat('\nStep2 : variable Declaration')
modelName <- "SVM"
modelName

InputDataFileName ='C:/Users/Satyam/Desktop/data.csv'
InputDataFileName

training = 65

cat('\nStep3 : Data Loading')

dataset <- read.csv(InputDataFileName)
dataset <- dataset[sample(nrow(dataset)),]

head(dataset)
nrow(dataset)
names(dataset)

cat('\nStep4 :Counting Dataset')
totalDataSet <- nrow(dataset)
totalDataset

cat('\nStep5 :Choose Target Variable')
target <- names(dataset)[15]
target

cat('\nStep6: Choose Input Variable')
inputs <- setdiff(names(dataset),target)
inputs
length(inputs)
anyNA(dataset)

cat('\nStep7 : Select traning dataset')
trainDataset <- dataset[1:(totalDataset*training/100),c(inputs,target)]
head(trainDataset)
nrow(trainDataset)

cat('\nStep8 : Select testing dataset')
testDataset <- dataset[(totalDataset*training/100):totalDataset,c(inputs,target)]
evaldata=read.csv('C:/Users/Satyam/Desktop/eval.csv')#evaluation example
head(testDataset)
nrow(testDataset)

cat('\nStep9 : ModelBuilding ->',modelName)

formula <- as.formula(paste(target,"~",paste(c(inputs),collapse="+")))
formula

model<-ksvm(formula,trainDataset,kernel="rbfdot",prob.model=TRUE)
model

cat('\nStep10 : Prediction Using ->',modelName)
Predicted <- round(as.numeric(predict(model,testDataset)))
Predicted1 <- round(as.numeric(predict(model,evaldata)))

head(Predicted)
PredictesProb <- predict(model,testDataset)
head(PredictedProb)

cat('\nStep11 : Extracting Actual')
Actual <- as.double(unlist(testDataset[target]))
head(Actual)

cat('\nStep12 : Model Evalution')
ConfusionMatrix <- misclassCounts(Predicted,Actual)$conf.matrix
ConfusionMatrix

accuracy <- round(mean(Actual == Predicted)*100,2)
accuracy

