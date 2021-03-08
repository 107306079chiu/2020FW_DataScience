### **IEEE CIS fraud detection**

IEEE-CIS works across a variety of AI and machine learning areas, including deep neural networks, fuzzy systems, evolutionary computation, and swarm intelligence. Today they’re partnering with the world’s leading payment service company, Vesta Corporation, seeking the best solutions for fraud prevention industry, and now you are invited to join the challenge.

This competition is a binary classification problem - i.e. our target variable is a binary attribute (Is the user making the click fraudlent or not?) and our goal is to classify users into "fraudlent" or "not fraudlent" as well as possible.

#### **Data**
In this competition you are predicting the probability that an online transaction is fraudulent, as denoted by the binary target `isFraud`.

The data is broken into two files **identity** and **transaction**, which are joined by `TransactionID`.

Note: Not all transactions have corresponding identity information.

#### **Categorical Features - Transaction**
- `ProductCD`
- `card1` - `card6`
- `addr1`, `addr2`
- `P_emaildomain`
- `R_emaildomain`
- `M1` - `M9`

#### **Categorical Features - Identity**
- `DeviceType`
- `DeviceInfo`
- `id_12` - `id_38`

#### **Files**
- train_{transaction, identity}.csv - the training set
- test_{transaction, identity}.csv - the test set (you must predict the `isFraud` value for these observations)
- sample_submission.csv - a sample submission file in the correct format

#### **Model Tried**
- Null model
- LightGBM Regressor
- KNN
- SVM
