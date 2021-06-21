from Capstone_Groupxx_Utility import modeling
from Capstone_Groupxx_Utility import read_csv
from Capstone_Groupxx_Utility import data_cleansing
from Capstone_Groupxx_Utility import split_set
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn import metrics
from sklearn.model_selection import train_test_split
wine_df = read_csv("/Users/zhenghaodeng/Desktop/Q3/ALY6140/W5/Capstone/winequality-red.csv")

data_cleansing(wine_df)
wine_df.head()
wine_df.describe()

split_set(wine_df)
X_train, X_test, Y_train, Y_test = split_set(wine_df)
wine_df['quality'].value_counts().plot.bar()

modeling(X_train, X_test, Y_train, Y_test)


Y = wine_df.quality
X = wine_df.drop('quality', axis=1)

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2)

clf_lr = LogisticRegression(multi_class='multinomial', solver='newton-cg')
clf_lr.fit(X_train, Y_train)
y_multi_log_pred = clf_lr.predict(X_test)

clf_tree = DecisionTreeClassifier()
clf_tree.fit(X_train, Y_train)
y_pred_tree = clf_tree.predict(X_test)
print('Accuracy of LogisticRegression Model: ' + str(metrics.accuracy_score(Y_test, y_multi_log_pred)))
print(metrics.accuracy_score(Y_test, y_pred_tree))


