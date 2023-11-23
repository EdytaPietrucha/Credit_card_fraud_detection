# Credit_card_fraud_detection

Detailed analysis can be find in the pdf file: credit_card_fraud_detection.pdf

### Objective: 
The aim of the analysis is to predict whether a transaction is fraudulent or not.

### Database:
Simulated credit card transaction dataset containing legitimate and fraud transactions from the duration 1st Jan 2019 - 31st Dec 2020. This was generated using Sparkov Data Generation tool.

Database is available on kaggle:
<https://www.kaggle.com/kartik2112/fraud-detection/version/1?select=fraudTrain.csv>

### Metrics:
$$ recall = \frac{TP}{TP+FN}$$

$$ precision = \frac{TP}{TP+FP}$$

### Main findings:
Variables which impact on detecting fraudulent transactions:

* Transaction hour - especially night time.
* Amount of money spent on each transaction - huge disproportion between not-fraudulent transactions and fraudulent transactions.
* Weekday of a transaction - escpecially Thursday, Friday, Saturday, Sunday.
* Category of Purchase - important categories are: 'grocery_pos', 'misc_net', 'shopping_net', 'shopping_pos'.
* Age of each Customer in a day of a transaction - especially combined with variables such as amount of money spent on a transaction or transacion time.

### Model recomandation: 
Logistic regression with cut off level equals to 0.03.


Figure 1: Precision and recall metrics for logistic regression as a function of the cut off level (left side) and for random froest as a funtion of the class weight (right side).
<img src = "https://github.com/EdytaPietrucha/Credit_card_fraud_detection/blob/main/figures/model_calibration/precision_recall_logreg.jpeg" width="600" height="300"> | <img src = "https://github.com/EdytaPietrucha/Credit_card_fraud_detection/blob/main/figures/model_calibration/precision_recall_forest.jpeg" width="600" height="300">
:-------------------------:|:-------------------------:

Figure 2: Precision and recall density function for logistic regression.
<img src = "https://github.com/EdytaPietrucha/Credit_card_fraud_detection/blob/main/figures/model_calibration/precision_density_logreg.jpeg" width="600" height="300"> | <img src = "https://github.com/EdytaPietrucha/Credit_card_fraud_detection/blob/main/figures/model_calibration/recall_density_logreg.jpeg" width="600" height="300">
:-------------------------:|:-------------------------:

Figure 3: Precision and recall density function for random forest.
<img src = "https://github.com/EdytaPietrucha/Credit_card_fraud_detection/blob/main/figures/model_calibration/precision_density_forest.jpeg" width="600" height="300"> | <img src = "https://github.com/EdytaPietrucha/Credit_card_fraud_detection/blob/main/figures/model_calibration/recall_density_forest.jpeg" width="600" height="300">
:-------------------------:|:-------------------------:

Figure 4: Frequancy of fraudulent transactions grouped by the transaction hour.
<img src = "https://github.com/EdytaPietrucha/Credit_card_fraud_detection/blob/main/figures/data_manipulation/trans_hour_barplot.jpeg" width="900" height="500">

Figure 5: Boxplot for variable amt group by not-fraudulent transactions and fraudulent transactions.
<img src = "https://github.com/EdytaPietrucha/Credit_card_fraud_detection/blob/main/figures/data_manipulation/amt_boxplot.jpeg" width="900" height="500"> 
