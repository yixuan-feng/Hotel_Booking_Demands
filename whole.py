import codecs

file = codecs.open("/Users/zhenghaodeng/Desktop/Q3/ALY6140/W5/Capstone/winequality-red.csv", 'r', 'utf-8')
data = file.readlines()

for i in range(len(data)):
    if i == 0:
    	# strip 只能删除【首尾】的指定字符
        data[0] = [item.strip('"') for item in data[i].split(';')]
        # 注意到最后还有一个换行符，对最后一个串额外 strip
        data[0][len(data[0]) - 1] = data[0][len(data[0]) - 1].strip('"\n')
        continue
    data[i] = [float(item) for item in data[i].split(';')]

wine_df = pd.DataFrame(columns=data[0], data=data[1:])
wine_df.drop_duplicates(inplace=True)

print(wine_df.head())
print(wine_df.describe())

''' 统计各品质占比的代码可参考这个 '''
# 分离特征与标签
features = wine_df.drop('quality', 1)
labels = wine_df['quality']

l = len(labels)
print('Statistic info of wine quality is as follows:')
for i in range(7):
    print('Quality = %d: %.3f%%' % (i + 3, labels[labels == i + 3].count() / l * 100))

print('Quality between 5~7: %.3f%%' % (labels[(labels < 8) & (labels > 4)].count() / l * 100))

''' 统计特征取值个数的可参考这个 '''
col = wine_df.columns

unique_value = [len(wine_df[col[i]].unique()) for i in range(len(col))]
print(pd.DataFrame(data=unique_value, index=col, columns=['unique value']))

'''数据集划分'''
from sklearn.model_selection import train_test_split
# 测试集大小占 20%；random_state 设置为 0 表示完全随机，默认参数下不管运行几次划分结果都是一样的（伪随机）
features_train, features_test, labels_train, labels_test = train_test_split(
    features, labels, test_size=0.2, random_state=0)

print(features_train.shape)
print(features_test.shape)

from sklearn.feature_selection import SelectKBest, chi2

sp = SelectKBest(chi2, k=9)
features_train_selected = sp.fit_transform(features_train, labels_train)
# 你会发现如果你在此之前还没有划分数据集，那么下面这一行可以省略，只是结果可能会稍微不同
features_test_selected = sp.transform(features_test)

print(features_train_selected.shape)
print(features_test_selected.shape)
# 下面这三行纯粹是为了看到底剔除了哪些变量，如这里是倒数第三和倒数第四个被剔除了
print(sp.scores_)
print(sp.get_params())
print(sp.get_support())


# drop fliers via LOF
def dropFliers(features, labels, threshold):
    from sklearn.neighbors import LocalOutlierFactor as LOF

    lof = LOF(contamination=threshold).fit(features)
    r_features = features[lof.negative_outlier_factor_ > lof.threshold_]
    r_labels = labels[lof.negative_outlier_factor_ > lof.threshold_]
    return r_features, r_labels


r_features_train_selected, r_labels_train = dropFliers(features_train_selected, labels_train, 0.1)
r_features_test_selected, r_labels_test = dropFliers(features_test_selected, labels_test, 0.1)

print(r_features_train_selected.shape)
print(r_features_test_selected.shape)


'''默认参数训练结果'''
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn import metrics
import matplotlib.pyplot as plt

def clf(estimator, features_train, features_test, labels_train, labels_test):
    estimator.fit(features_train, labels_train)
    # 准确率
    print('Accuracy: %.3f%%' % (estimator.score(features_test, labels_test) * 100))
	# 混淆矩阵
    cm = metrics.confusion_matrix(labels_test, estimator.predict(features_test))
    print(cm)
    # 输出完整的分类结果
    print(metrics.classification_report(labels_test, estimator.predict(features_test)))
    plt.figure()
    plt.xlabel('Predicted labels')
    plt.ylabel('True labels')
    plt.imshow(cm)

rf = RandomForestClassifier()
print('\nTraining result for RandomForest:')
clf(rf, features_train_norm, features_test_norm, r_labels_train, r_labels_test)
ada = AdaBoostClassifier()
print('\nTraining result for AdaBoost:')
clf(ada, features_train_norm, features_test_norm, r_labels_train, r_labels_test)
