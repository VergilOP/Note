from psmpy import PsmPy  
from psmpy.functions import cohenD  
from psmpy.plotting import *
import numpy as np
import matplotlib.pyplot as plt

plt.rcParams['figure.figsize'] = [20, 10]

data_all = pd.read_csv('f:/学习资料/cs/note cs/考研 笔记/实习/Week3/case3/data_all.csv')
user_type = pd.read_csv('f:/学习资料/cs/note cs/考研 笔记/实习/Week3/case3/user_type.csv')

merged_data = pd.merge(data_all, user_type, on='user_id')

exposed_users = pd.read_csv('f:/学习资料/cs/note cs/考研 笔记/实习/Week3/case3/广告曝光用户.csv')
exposed_user_ids = exposed_users['user_id'].unique()

exposed_users = merged_data[merged_data['user_id'].isin(exposed_user_ids)]
non_exposed_users = merged_data[~merged_data['user_id'].isin(exposed_user_ids)]

exposed_users = exposed_users.assign(exposed=1)
non_exposed_users = non_exposed_users.assign(exposed=0)

combined_data = pd.concat([exposed_users, non_exposed_users], ignore_index=True)

# buying_users = combined_data[combined_data['behavior_type'] == 4]
# psm = PsmPy(buying_users, treatment='exposed', indx='user_id')

psm = PsmPy(combined_data, treatment='exposed', indx='user_id')

psm.logistic_ps(balance=True)

psm.knn_matched(matcher='propensity_logit', replacement=False, caliper=None)

# plt.figure()
# psm.plot_match(Title='Side by side matched users', Ylabel='Number of users', Xlabel='Propensity logit', names=['exposed', 'control'], save=True)
# plt.savefig("Side by side matched users.pdf", format="pdf")
# plt.show()
# 
# plt.figure()
# psm.effect_size_plot(save=False)
# plt.savefig("Standardized Mean differences accross covariates before an after matching.pdf", format="pdf")
# plt.show()

exposed_matched = psm.df_matched[psm.df_matched['exposed'] == 1]
non_exposed_matched = psm.df_matched[psm.df_matched['exposed'] == 0]

consumption_diff = exposed_matched.groupby('behavior_type')['revenue'].mean() - non_exposed_matched.groupby('behavior_type')['revenue'].mean()
print(consumption_diff)

behavior_types = [1, 2, 3]

for b_type in behavior_types:
    exposed_matched_filtered = exposed_matched[exposed_matched['behavior_type'] == b_type]
    non_exposed_matched_filtered = non_exposed_matched[non_exposed_matched['behavior_type'] == b_type]
    
    exposed_purchase = exposed_matched[exposed_matched['behavior_type'] == 4]
    non_exposed_purchase = non_exposed_matched[non_exposed_matched['behavior_type'] == 4]

    exposed_conversion_rate = len(exposed_purchase) / len(exposed_matched_filtered)
    print(f"exposed_conversion_rate for behavior type {b_type}: {exposed_conversion_rate}")
    non_exposed_conversion_rate = len(non_exposed_purchase) / len(non_exposed_matched_filtered)
    print(f"non_exposed_conversion_rate for behavior type {b_type}: {non_exposed_conversion_rate}")
    
    conversion_rate_diff = exposed_conversion_rate - non_exposed_conversion_rate
    print(f"Conversion rate difference for behavior type {b_type}: {conversion_rate_diff}")