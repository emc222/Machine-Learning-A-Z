
# coding: utf-8

# ### Importing Libaries

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


fileName='/home/xenon/extensomllearn/Machine Learning A-Z/Part 1 - Data Preprocessing/Data_Preprocessing/Data.csv'


# ### Reading the CSV File Using Pandas Library

dataset=pd.read_csv(fileName)


# ### For Machine Learning We need features and lables. 'x' is feature and 'y' is label  
x=dataset.iloc[:,:-1].values
#dataset.iloc[:,:-1] means that we want all the rows and columns except for the last column   
y=dataset.iloc[:,3:].values
# ### Handling the Missing Data
from sklearn.preprocessing import Imputer
imputer =Imputer(missing_values='NaN',strategy='mean', axis=0)
imputer=imputer.fit(x[:,1:3])
x[:,1:3]=imputer.transform(x[:,1:3])

# ### Encoding Categorical Data

from sklearn.preprocessing import LabelEncoder,OneHotEncoder
labelencoder_x=LabelEncoder()
x[:,0]=labelencoder_x.fit_transform(x[:,0])
 """Dummy encoding: We create three columns instead of one acontaining unique number for each category. 
# We need to do this since no relation exists between the number. 
Example France and Germany are encoded as 0 and 1 but the relation `0<1` does not apply 
since we cannot say `France < Germany` """ 
onehotencoder=OneHotEncoder(categorical_features=[0])
x=onehotencoder.fit_transform(x).toarray()
labelencoder_y=LabelEncoder()
y=labelencoder_y.fit_transform(y)
# ### Splitting the data into testing and training set.
from sklearn.model_selection import train_test_split
x_train,x_test,y_train,y_test=train_test_split(x,y,test_size=0.2,random_state=0)
# ### Feature Scaling: Since all the features are not in same scale we need to bring them to same scale.
from sklearn.preprocessing import StandardScaler
sc_x=StandardScaler()
x_test=sc_x.fit_transform(x_test)
x_train=sc_x.fit_transform(x_train)
