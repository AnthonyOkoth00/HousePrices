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

sns.distplot(df_train["SalePrice"])
#plt.show()


print "------------------------------------------------"
#skewness and kutosis
print("Skewness: %f" % df_train["SalePrice"].skew())
print("Kurtosis: %f" % df_train["SalePrice"].kurt())


print "------------------------------------------------"
#scatter plot grlivarea/saleprice
var = "GrLivArea"
data = pd.concat([df_train["SalePrice"], df_train[var]], axis= 1)
data.plot.scatter(x = var, y = "SalePrice", ylim = (0.800000))
#plt.show()

#scatter plot totalbsmtsf/saleprice
var = "TotalBsmtSF"
data = pd.concat([df_train["SalePrice"], df_train[var]], axis= 1)
data.plot.scatter(x = var, y = "SalePrice", ylim = (0.800000))
plt.show()