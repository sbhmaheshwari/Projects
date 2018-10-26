# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import numpy as np
import pandas as pd

def downcast_dtypes(data):
    float_cols = [c for c in data if data[c].dtype == "float64"]
    int_cols =   [c for c in data if data[c].dtype == "int64"]
    data[float_cols] = data[float_cols].astype(np.float32)
    data[int_cols]   = data[int_cols].astype(np.int32)
    return(data)

def create_test(test, sales_test):
    test = test.loc[(test['shop_id'].isin(sales_test['shop_id']))&(test['item_id'].isin(sales_test['item_id'])),:].copy()
    test_pred = test['cnt_shop_item'].copy()
    test.drop('cnt_shop_item', axis = 1, inplace = True)
    return(test, test_pred)
    
def create_submission_file(pred, name, sales_test):
    ID = np.arange(0, sales_test.shape[0]) 
    new_df = pd.DataFrame({'ID': ID, 'item_cnt_month': pred})
    new_df.to_csv('Submission Time Series/'+name+'.csv',index = False)
    
def create_cv_sets(months, data, full_data):
    train = []
    test = []
    train_pred = []
    test_pred = []
    for i in months:
        x = data.loc[full_data['date_block_num']<i,:].drop('cnt_shop_item', axis = 1)
        y_pred = data.loc[full_data['date_block_num']<i,'cnt_shop_item']
        x_test = data.loc[full_data['date_block_num']==i,:]
        x_test, y_test_pred = create_test(x_test)
        train.append(x)
        test.append(x_test)
        train_pred.append(y_pred)
        test_pred.append(y_test_pred)
    return(train,test,train_pred,test_pred) 
    
def create_cv_sets2(months, data, sales_test):
    X_train = data.loc[~data['date_block_num'].isin(months+[34]),:].drop('cnt_shop_item', axis=1)
    X_val = data.loc[data['date_block_num'].isin(months),:]
    X_val, y_val = create_test(X_val, sales_test)
    X_test = data.loc[data['date_block_num'] == 34,:].drop('cnt_shop_item',axis=1)
    y_train = data.loc[~data['date_block_num'].isin(months+[34]),'cnt_shop_item']
    return(dict({'train': X_train, 'val': X_val, 'test': X_test, 'train_y': y_train, 'val_y': y_val}))

def create_test_data(prediction, dict_cat):
  trtv = pd.concat([dict_cat['train'], dict_cat['val']], axis = 0)
  test_ids = dict_cat['test']['item_id'].isin(trtv['item_id']) & dict_cat['test']['shop_id'].isin(trtv['shop_id'])
  prediction[~test_ids] = 0
  return(prediction)
