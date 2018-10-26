
This is a project inspired from the Kaggle competition __'Future Sales Prediction'__, and the data can be found on the link:
https://www.kaggle.com/c/competitive-data-science-final-project/data

Worked on Windows 10, i-5-3230M CPU @ 2.60GHz, 8 GB RAM, 2 cores. Used __Google Colabs__ for neural network training and __Bayesian optimiztion using hyperopt__ 

In the project, I have used the following models and stacked them:
1) _Neural network with categorical embeddings_ - Embedding_nn_pfs4.ipynb
2) _CatBoost_ - pfs4_catboost_optimize.ipynb
3) _XGBoost_ - XGBOOST_pfs4_tuning.ipynb
4) _Random forest_ - pfs_rf_tuning4.ipynb

The file for data preparation and feature processing is PFS_Modeling4.ipynb, where the following steps are taken:
1) Aggregate the data w.r.t [shop, item, date block num], [shop, date block num], [item, date block num] and [item category id, date block num]
2) Create lag features for 1-6, 12 months for monthly aggregated quantity, sales, price for different grouped features
3) Create mean encodings for categorical features - item, shop, month, year and item category id
4) Create text features (5 each) using _truncated pca_ for item names and item category names

Validation Strategy:
1) Use data for 9, 21 and 33 months for validation
2) While validating count error over only the shop and item ids that are present in the test data such that they have a similar distribution

Stacking: 
1) Used linear regression and random forest for stacking, but linear regression gave better results 

What could have been done better:
1) As in test set all shop id and item id pair occurs, it would have been a good idea to create all the combinations of shop and item ids in a date block num and use a bigger train set. But that would lead to 112M rows, and resources were limited at my end
2) On going through past projects, came across various examples where LightGBM produced good results, could work here
3) Using Deeper network and embedding Item ids as well 
