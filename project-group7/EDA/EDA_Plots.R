##### Imports
install.packages('tidyverse')
install.packages('ggpubr')
##### Librarys
library('tidyverse')
library('ggplot2')
library(dplyr)
library(ggthemes)
library(ggmosaic)
library(gridExtra)
library(repr)
library(data.table)
library('ggpubr')

##### load data
# When loading large data, read_csv is faster than read.csv.
train_trans <- read_csv('input/train_transaction.csv')
train_id <- read_csv('input/train_identity.csv')
test_trans <- read_csv('input/test_transaction.csv')
test_id <- read_csv('input/test_identity.csv')

######EDA
##### Target variable
ggplot(train_trans, aes(factor(isFraud), fill = factor(isFraud))) + geom_bar(alpha = 0.8)  + theme_minimal() +
  ggtitle("Data imbalance: isFraud") + labs(x = "isFraud")

##### TransactionDT
a1  <- ggplot(train_trans, aes(TransactionDT, fill = factor(isFraud))) + geom_histogram(alpha = 0.7, bins = 30)  + theme_minimal() +
  ggtitle("Train TransactionDT variable") + labs(x = "TransactionDT") + theme(legend.position = "bottom")

b1 <- ggplot(test_trans, aes(TransactionDT)) + geom_histogram(alpha = 0.7, bins = 30)  + theme_minimal() +
  ggtitle("Test TransactionDT variable") + labs(x = "TransactionDT")

theme_set(theme_pubr())
options(repr.plot.width = 10, repr.plot.height = 3)
figure <- ggarrange(a1, b1,
                    common.legend = TRUE, legend = "bottom",
                    nrow = 1, ncol = 2)
#figure

##### TransactionAMT
options(repr.plot.width = 5, repr.plot.height = 3)
a1 <- ggplot(train_trans, aes(TransactionAmt, fill = factor(isFraud))) + geom_histogram()

train_trans$amt_log  <- log10(train_trans$TransactionAmt)
test_trans$amt_log  <- log10(test_trans$TransactionAmt)
a1 <- ggplot(train_trans, aes(amt_log, fill = factor(isFraud))) + geom_histogram() + ggtitle("train")
b1 <- ggplot(test_trans, aes(amt_log)) + geom_histogram() + ggtitle("test")

options(repr.plot.width = 10, repr.plot.height = 3)
figure2 <- ggarrange(a1, b1,
                    common.legend = TRUE, legend = "bottom",
                    nrow = 1, ncol = 2)

##### Summary of cards
# card1
a1 <- ggplot(train_trans, aes(card1, fill = factor(isFraud))) + geom_histogram(alpha = 0.7, bins = 50) + ggtitle("Train")
b1 <- ggplot(test_trans, aes(card1)) + geom_histogram(bins = 50) + ggtitle("Test")

options(repr.plot.width = 10, repr.plot.height = 3)
figure <- ggarrange(a1, b1,
                    common.legend = TRUE, legend = "bottom",
                    nrow = 1, ncol = 2)
figure

options(repr.plot.width = 5, repr.plot.height = 5)
ggplot(train_trans, aes(ProductCD, fill = factor(isFraud))) + geom_bar(alpha =0.7, position = 'fill') +
  geom_hline(yintercept = 0.035, colour="black", linetype="dashed")

#Card4
a1 <- ggplot(train_trans, aes(card4, fill = factor(isFraud))) + geom_bar(alpha = 0.7) + ggtitle("Train") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
b1 <- ggplot(test_trans, aes(card4)) + geom_bar() + ggtitle("Test") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

options(repr.plot.width = 10, repr.plot.height = 3)
figure <- ggarrange(a1, b1,
                    common.legend = TRUE, legend = "bottom",
                    nrow = 1, ncol = 2)
figure

options(repr.plot.width = 4, repr.plot.height = 3)
ggplot(train_trans, aes(card4, fill = factor(isFraud))) + geom_bar(alpha = 0.7, position = 'fill') +
  geom_hline(yintercept = 0.035, colour="black", linetype="dashed") + coord_cartesian( ylim = c(0, 0.2)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# card6
a1 <- ggplot(train_trans, aes(card6, fill = factor(isFraud))) + geom_bar(alpha = 0.7) + ggtitle("Train")
b1 <- ggplot(test_trans, aes(card6)) + geom_bar() + ggtitle("Test")

options(repr.plot.width = 10, repr.plot.height = 3)
figure <- ggarrange(a1, b1,
                    common.legend = TRUE, legend = "bottom",
                    nrow = 1, ncol = 2)
figure

options(repr.plot.width = 4, repr.plot.height = 3)
ggplot(train_trans, aes(card6, fill = factor(isFraud))) + geom_bar(alpha = 0.7, position = 'fill') +
  geom_hline(yintercept = 0.035, colour="black", linetype="dashed") + coord_cartesian( ylim = c(0, 0.2)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
