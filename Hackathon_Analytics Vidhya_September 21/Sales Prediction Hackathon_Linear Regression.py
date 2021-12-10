#!/usr/bin/env python
# coding: utf-8

# In[1]:


#pip install Pyforest 
#code for installing required libraries in environment


# In[2]:


#pip install -U klib
import klib #Using Klib fot EDA Purpose


# In[3]:


df=pd.read_csv("TRAIN.csv") #importing the data
df_test_final=pd.read_csv("TEST_FINAL.csv") #importing the data


# In[4]:


df.head() #Viewing the data


# In[5]:


df_test_final.head() #Viewing the data


# In[6]:


klib.missingval_plot(df) # returns a figure containing information about missing values


# In[7]:


klib.missingval_plot(df_test_final) # returns a figure containing information about missing values


# In[8]:


df.info() #info before cleaning the data


# In[9]:


df=klib.data_cleaning(df) # performs datacleaning (drop duplicates & empty rows/cols, adjust dtypes,...)
df_test_final=klib.data_cleaning(df_test_final) # performs datacleaning (drop duplicates & empty rows/cols, adjust dtypes,...)


# In[10]:


df.info() #info after cleaning the data


# In[11]:


df.drop('hash_order', axis=1, inplace=True)# dropping unwanted column order


# In[12]:


#df=df.set_index('date') # setting date column as index


# In[13]:


df['store_type'].describe() #viewing Statistics of categorical data and looking for unique values


# In[14]:


df['location_type'].describe() #viewing Statistics of categorical data and looking for unique values


# In[15]:


df['region_code'].describe() #viewing Statistics of categorical data and looking for unique values


# In[16]:


df['discount'].describe() #viewing Statistics of categorical data and looking for unique values


# In[17]:


le = LabelEncoder() #applying label encoder 
df['discount'] = le.fit_transform(df.discount) #Applying Label encoding to discount column

le = LabelEncoder() #applying label encoder 
df_test_final['discount'] = le.fit_transform(df_test_final.discount) #Applying Label encoding to discount column


# In[18]:


# Splitting Date column
df['date'] = pd.to_datetime(df['date'])
df['year'] = df['date'].dt.year
df['month'] = df['date'].dt.month
df['day'] = df['date'].dt.day

df_test_final['date'] = pd.to_datetime(df_test_final['date'])
df_test_final['year'] = df_test_final['date'].dt.year
df_test_final['month'] = df_test_final['date'].dt.month
df_test_final['day'] = df_test_final['date'].dt.day


# In[19]:


df.describe()  #viewing Statistics of numerical data and looking for negative values, outlaiers


# In[20]:


df.corr()


# In[21]:


# here store type and st
df.head() #here we see that it is a multi variate time series forecasting problem. 


# In[22]:


# here store type and st
df_test_final.head() #here we see that it is a multi variate time series forecasting problem. 


# In[23]:


df.plot( y='sales', figsize=(20,10))


# In[24]:


#Setting the value for X and Y
x = df[['holiday', 'discount', 'year','month','day','store_type','location_type','region_code']]
y = df['sales']


# In[25]:


#Applying One hot Encoding on both Test and train Dataset

df = pd.get_dummies(df,columns=["store_type","location_type","region_code"],drop_first=True)
x = pd.get_dummies(x,columns=["store_type","location_type","region_code"],drop_first=True)
df_test_final = pd.get_dummies(df_test_final,columns=["store_type","location_type","region_code"],drop_first=True)


# In[26]:


df.head()


# In[27]:


df_test_final.head()


# In[28]:


x.head()


# In[29]:


y.head()


# In[30]:


#create train and test split
from sklearn import model_selection
x_train,x_test,y_train,y_test = model_selection.train_test_split(x,y,test_size=0.2,random_state=42)


# In[31]:


#using train, test datasets
from sklearn import linear_model as lm
reg = lm.LinearRegression(normalize=True)
reg.fit(x_train, y_train)

# print intercept and coefficients
print(reg.intercept_)
print(reg.coef_)


# In[32]:



#predictions  on test dataset
predictions = reg.predict(x_test)
validate = pd.DataFrame({'Actual': y_test, 'Predicted': predictions})


# In[33]:


# Evaluating
from sklearn import metrics
from sklearn.metrics import mean_squared_log_error
print('Root Mean Squared Error:',np.sqrt(metrics.mean_squared_error(y_test, predictions)))
print('Mean Squared Error:', metrics.mean_squared_error(y_test, predictions))
print('Mean Squared Log Error:', mean_squared_log_error(y_test, predictions)*1000)


# In[34]:


import statsmodels.formula.api as smf
import statsmodels.api as sm
lm = smf.ols(formula='sales ~ holiday + discount + year + month + day + store_type_S2 + store_type_S3 + store_type_S4 + location_type_L2 + location_type_L3 + location_type_L4 + location_type_L5 + region_code_R2 + region_code_R3 + region_code_R4', data=df).fit()
lm.conf_int()
lm.summary()#viewing the metrics of linear regressions


# In[35]:


predictions = reg.predict(x_test) #predicting on the test dataset


# In[36]:


df.head()


# In[37]:


#Training on the whole model
reg.fit(x, y)


# In[38]:


df_id=df_test_final['id'] #copping Id on the final Dataset
df_test_final=df_test_final.drop(['store_id','date','id'], axis=1) #Dropping the columns


# In[39]:


df_test_final.info()


# In[40]:


predictions = reg.predict(df_test_final) #Applying the predictions on the unknown dataset


# In[41]:


predictions = pd.DataFrame(predictions)#converting into dataframe

predicted_sales= pd.concat([df_id, predictions],axis=1) #combining two datasets

predicted_sales.columns = ['ID', 'Sales'] #renaming Columns


# In[42]:


predicted_sales.describe() #checking The predictions


# In[43]:


y.describe()


# In[44]:


predicted_sales.to_csv('Leanear_Regression_prediction.csv',index=False) #getting Output


# In[ ]:




