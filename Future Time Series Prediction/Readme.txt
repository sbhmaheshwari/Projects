This is a project inspired from the Kaggle competition 'Future Sales Prediction', and the data can be found on the link:
https://www.kaggle.com/c/competitive-data-science-final-project/data

In the project, I have used the following models and stacked them:
1) Neural network with categorical embeddings - Embedding_nn_pfs4.ipynb
2) CatBoost - pfs4_catboost_optimize.ipynb
3) XGBoost - XGBOOST_pfs4_tuning.ipynb
4) Random forest - pfs_rf_tuning4.ipynb

The file for data preparation and feature processing is PFS_Modeling4.ipynb, where the following steps are taken:
1) Aggregate the data w.r.t [shop, item, date block num], [shop, date block num], [item, date block num] and [item category id, date block num]
2) Create lag features for months 1, 2, 3, 4, 5, 6, 12 for monthly aggregated quantity, sales, price for different grouped features
3) Create mean encodings for categorical features - item, shop, month, year and item category id
4) Create text features (5 each) using truncated pca for item names and item category names

Validation Strategy:
1) Use data for 9, 21 and 33 months for validation
2) While validating count error over only the shop and item ids that are present in the test data such that they have a similar distribution
   