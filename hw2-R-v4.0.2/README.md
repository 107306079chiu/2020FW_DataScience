# hw2 :  evalute a mode for predicting loans that are about to default

```R
Rscript hw2.R --target bad --badthre 0.5 --input data/method1.csv data/method2.csv --output data/output1.csv
Rscript hw2.R --target bad --badthre 0.4 --input data/method1.csv data/method3.csv data/method5.csv --output data/output2.csv
Rscript hw2.R --target good --badthre 0.6 --input data/method2.csv data/method4.csv data/method6.csv --output data/output3.csv
```

* Read in multiple files
* Positive case defined by “--target” option
* Threshold defined by “--badthre” option

## Inputs

* data/method1.csv
* the last column, pred.score, is the predicted probability of "bad loan".


persons,reference,pred.score

person1,bad,0.807018548483029

person2,bad,0.740809247596189

person3,bad,0.0944965328089893

person4,good,0.148418645840138

## Output
* You should write your own function to calculate metrics.
* Find out which method contains the max
* pseudo R<sup>2</sup> = 1 - deviance(model)/deviance(null model) for *S*=0, where null model predicts the probability of inputs as a constant number = the proportion of the positive cases

method,sensitivity,specificity,F1,logLikelihood,pseudo R<sup>2</sup>

method1,0.91,0.96,0.85,-132,0.79

method2,0.99,0.98,0.86,-112,0.70

max,method2,method2,method2,method2,method1

## Null model
* as being “the obvious guess”
* please use the null model which always return the proportion of "bad" loans

**R version 4.0.2 is used in this file**
