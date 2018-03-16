import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from scipy.stats import norm
from sklearn.preprocessing import StandardScaler
from scipy import stats
import warnings
warnings.filterwarnings("ignore")

df_train = pd.read_csv("./train.csv")
#print df_train.columns

#descriptive statistics summary
print df_train["SalePrice"].describe()

#sns.distplot(df_train["SalePrice"])
#plt.show()


print "------------------------------------------------"
#skewness and kutosis
print("Skewness: %f" % df_train["SalePrice"].skew())
print("Kurtosis: %f" % df_train["SalePrice"].kurt())


print "------------------------------------------------"
#scatter plot grlivarea/saleprice
var = "GrLivArea"
data = pd.concat([df_train["SalePrice"], df_train[var]], axis= 1)
#data.plot.scatter(x = var, y = "SalePrice", ylim = (0.800000))
#plt.show()

#scatter plot totalbsmtsf/saleprice
var = "TotalBsmtSF"
data = pd.concat([df_train["SalePrice"], df_train[var]], axis= 1)
#data.plot.scatter(x = var, y = "SalePrice", ylim = (0.800000))

#box plot overallqual/saleprice
var = "OverallQual"
data = pd.concat([df_train["SalePrice"], df_train[var]], axis = 1)
#f, ax = plt.subplots(figsize = (8, 6))
#fig = sns.boxplot(x = var, y = "SalePrice", data = data)
#fig.axis(ymin= 0, ymax= 800000)


var = "YearBuilt"
data = pd.concat([df_train["SalePrice"], df_train[var]], axis = 1)
f, ax = plt.subplots(figsize = (16, 8))
#fig = sns.boxplot(x = var, y = "SalePrice", data = data)
#fig.axis(ymin= 0, ymax= 800000)
plt.xticks(rotation = 90)

#correlation matrix
corrmat = df_train.corr()
#f, ax = plt.subplots(figsize = (12, 9))
#sns.heatmap(corrmat, vmax= .8, square= True)

#saleprice correlation matrix
k = 10 #number of variables for heatmap
cols = corrmat.nlargest(k, "SalePrice")["SalePrice"].index
cm = np.corrcoef(df_train[cols].values.T)
#sns.set(font_scale= 1.25)
#hm = sns.heatmap(cm, cbar= True, annot= True, square= True, fmt= ".2f", annot_kws= {"size": 10},
                 yticklabels= cols.values, xticklabels= cols.values)

#scatterplot
sns.set()
cols = ["SalePrice", "OverallQual", "GrLivArea", "GarageCars", "TotalBsmtSF", "FullBath", "YearBuilt"]
sns.pairplot(df_train[cols], size= 2.5)
plt.show()