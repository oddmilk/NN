"""This is the physician segmentation module to be implemented on NN's MDP."""

"""Libraries"""
import pandas as pd
import numpy as np
from pandas import ExcelWriter
import datetime
from sklearn import cluster
from matplotlib import pyplot as plt

"""Functions"""
""""""

def bulkExport(x):
"""This function bulk output data frames to multiple sheets in one excel workbook"""
	for i in range(len(x)):
		x[i].to_excel(writer, 'sheet%s' % i)
	writer.save()


"""Data Loading"""
"""file directory: needs to be reset"""
keywords = pd.read_excel('keywords_v2.xlsx')
content = pd.read_excel('video_v6.xlsx')


"""Preprocessing"""
	# Time on video: format conversion
content['time'] = content.LENGTH.apply(lambda x: x.hour*3600 + x.minute*60 + x.second)
content['hour'] = content.LENGTH.apply(lambda x: x.hour)
	# Outlier removal: assume each video should not exceed 1 hour
outlier = content[content.hour > 0]  # 18 records of 15 unique physicians were found to have > 1-hour duration
normal = content[content.hour == 0]
	# for each video, obtain the max & min view duration
content_feature = normal.groupby('VIDEO_NAME')['time'].apply(lambda x: max(x)).reset_index()
content_feature.columns = ['VIDEO_NAME', 'max_view']
	# join view duration metrics back to the main record table
content_2 = normal.merge(content_feature, on = 'VIDEO_NAME', how = 'outer')

############################
### Information Retrieval
############################
# Corpus
title = normal.iloc[:,4].unique()
# Index
term = keywords.iloc[:,0] 	# Keywords selection
label = keywords.iloc[:,1] # mid-layer: n to 1
umbrella = keywords.iloc[:,2] # quadrants

# Inversed document frequency
title_df = pd.DataFrame(title)
doc_freq = np.zeros(len(term)) # create an empty array to store doc frequency for each term
for i in range(len(term_df)):
	doc_freq[i] = title_df.iloc[:,0].str.contains(term[i]).sum()

import math
term_df = pd.DataFrame(doc_freq)
term_df.columns = ['doc_freq']
term_df['term'] = term
N = len(title)
	# Remove terms with zero occurrence
nonzero_term_df = term_df[term_df.doc_freq > 0]
	# idf calculation
nonzero_term_df['idf'] = nonzero_term_df['doc_freq'].apply(lambda x: math.log(N/x))

# Term frequency
	# remove keywords with zero occurrence
term2 = term[nonzero_term_df.index].reset_index(drop = True)
content_term_matrix = np.zeros(shape = (len(content_feature), len(term2)))
for i in range(len(content_feature)):
	for j in range(len(term2)):
		content_term_matrix[i][j] = term2[j] in content_feature['VIDEO_NAME'][i]

	# DataFraming
content_term_df = pd.DataFrame(content_term_matrix)
LEN = len(content_term_df) + 1
WID = len(content_term_df.columns)
term_content_df = content_term_df.iloc[:,1:WID].T.reset_index()
term_content_idf = term_content_df.merge(nonzero_term_df[['term','idf']], how = 'inner')
term_content_idf.iloc[:,1:LEN] = term_content_idf.iloc[:,1:LEN].multiply(term_content_idf['idf'], axis = "index")
trans_back = term_content_idf.iloc[:,1:LEN].T # keeping user_id columns only
trans_back.columns = term_content_idf['term']
content_term_df_2 = pd.concat((content_term_df['VIDEO_NAME'], trans_back), axis = 1)

	# Data joining
content_3 = content_2.merge(content_term_df_2, how = "outer")
# Data output
ir_src = [title_df, term_df, nonzero_term_df, content_term_df_2, user_label_matrix, user_umbrella_matrix, content_watch_pattern, mm4]
writer = pd.ExcelWriter('ir_source.xlsx')
bulkExport(ir_src)

############################
### Weight Calculation
############################
	# user impression on content
content_3['impression_param'] = content_3['time']/content_3['max_view']
content_3.impression_param = content_3.time.apply(lambda x: content_3['impression_param']*0.01 if x < 180 else content_3['impression_param'])
content_3[term2] = content_3[term2].multiply(content_3['impression_param'], axis = "index")
	# group content-user-label data by phyician and sum across
content_4 = content_3.groupby(['NAME','HCO'])[term2].sum()
content_4 = content_4.reset_index()

	# transpose user-term matrix
user = content_4[['NAME','HCO']]
content_4_t = content_4.iloc[:,2:186].T.reset_index()
	# for each physician, map terms to label classes
content_4_t = content_4_t.rename(columns = {
'index': 'term'
})
	# data join
term_user_matrix = content_4_t.merge(keywords, how = 'left')
	# aggregate score per term to score per label class
label_user_matrix = term_user_matrix.groupby('label').sum()
	# data transposition: label_user to user_label
user_label_matrix = label_user_matrix.T.reset_index()
user_label_matrix = user_label_matrix.rename(columns = {
'index': 'user'
})
	# Add back user information
user_label_matrix['NAME'] = user['NAME']
user_label_matrix['HCO'] = user['HCO']
	# aggregate score per term to score per umbrella
umbrella_user_matrix = term_user_matrix.groupby('quadrant').sum()
user_umbrella_matrix = umbrella_user_matrix.T.reset_index()
user_umbrella_matrix['NAME'] = user['NAME']
user_umbrella_matrix['HCO'] = user['HCO']
user_umbrella_matrix = user_umbrella_matrix.merge(content[['NAME','HCO','SP','TITLE']].drop_duplicates(), how = 'outer')


# for each physician user, obtain: (1). average time spent on each video (2). # videos watched
content_watched = normal.groupby(['NAME','HCO']).apply(lambda x: len(x)) # total numbers of videos watched
unique_content_watched = normal.groupby(['NAME','HCO']).apply(lambda x: len(x['VIDEO_NAME'].unique()))
content_watched = content_watched.reset_index()
unique_content_watched = unique_content_watched.reset_index()
	# average time spent on video
		# outlier removal: contents that have a length > 1 hour will be removed
content_avg_time = normal.groupby(['NAME','HCO']).apply(lambda x: x['time'].mean())
content_avg_time = content_avg_time.reset_index()
content_avg_time.columns = ['NAME','HCO','avg_time']
	# data merge
content_watched.columns = ['NAME', 'HCO', 'watched']
unique_content_watched.columns = ['NAME', 'HCO', 'watched_unique']
content_watch_pattern = content_watched.merge(unique_content_watched)
content_watch_pattern = content_watch_pattern.merge(content_avg_time)
content_watch_pattern['unique_rate'] = content_watch_pattern['watched_unique']/content_watch_pattern['watched']
	# merge data
mm2 = content_watch_pattern.merge(user_umbrella_matrix, how = 'outer')
mm2['avg_mins'] = mm2['avg_time']/60 	# Question: should it get logged?

	# feature selection
mm3 = mm2[['NAME','HCO','SP','TITLE','unique_rate','avg_mins','并发症管理','治疗方案','诊断和分类','长期管理']]



########################
### Clustering
########################
	# outlier detection
mm3['overall'] = mm3.iloc[:,4:10].sum(axis = 1)
mm3['overall'].describe()
mm4 = mm3[mm3.overall <= 20]
	# Data export
list_matrix = [content_feature,content_term_df,content_3,content_4,label_user_matrix,user_label_matrix,user_umbrella_matrix, content_watch_pattern, mm2, mm3, mm4]
writer = pd.ExcelWriter('list_matrix.xlsx')
bulkExport(list_matrix)

	# specify the number of clusters (k) that will be formed in final solution
from sklearn.cluster import KMeans
param_cluster = 4
cluster = KMeans(n_clusters = param_cluster)
test = cluster.fit(mm4[mm4.columns[4:10]])
mm4['cluster'] = test.labels_
cluster_pct = mm4.cluster.value_counts()
cluster_pct = cluster_pct/cluster_pct.sum()
writer = pd.ExcelWriter('clustered_v4.xlsx')
mm4.to_excel(writer, 'sheet1')
writer.save()

	# summary stats: population vs. segments
pop_SP = mm3['SP_NUM'].value_counts()
pop_TITLE = mm3['TITLE_NUM'].value_counts()

	# by segments
grp_SP = mm3.groupby('cluster').apply(lambda x: x['TITLE_NUM'].value_counts()).reset_index()
