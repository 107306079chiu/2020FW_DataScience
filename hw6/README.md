# hw6: kagge competition - GiveMeSomeCredit

[GiveMeSomeCredit](https://www.kaggle.com/c/GiveMeSomeCredit/overview)

Start here! Predict the probability that somebody will experience financial distress in the next two years (column=SeriousDlqin2yrs).

## Steps

1. perform *n*-fold cross-validation on the training data under three-way split to select the best prediction model
2. report the average accuracy of cross-validation (training, validation, testing in *n*-fold cross-validation), i.e., hw4
3. then apply the selected model on the test data
4. output prediction result

```R
Rscript hw6.R --fold 5 --train Data/train.csv --test Data/test.csv --report performance1.csv --predict predict.csv
...
Rscript hw6.R --fold 10 --train Data/train.csv --test Data/test.csv --report performance6.csv --predict predict.csv
```

![GiveMeSomeCreditLeaderBoard](submission.png)

Best fold: 5

* **Please use R version 3.6.3**
