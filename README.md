# CSC522-project-work
Code for project added - Bank telemarketing

Project consists of 3 files
1.Preprocess.r
2.Classification.r
3.tunesvm.r

We have used the following packages - caret, pROC, klaR, dplyr, randomForest, e1071, ROCR.

"bank-additional-full.csv" as a raw data file is the input for Preprocess.r, which gives an output file with the same name into the current working directory. The output file "Mydata.csv" now contains the preprocessed data.

We execute tunesvm.r after preprocessing to get best kernel and parameters.

Classification.r is executed which outputs the ROC curves, LIFT curves and then prints the AUC values.It also outputs the
confusion matrices.
