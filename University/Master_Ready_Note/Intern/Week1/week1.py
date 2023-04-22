import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
plt.rcParams['figure.figsize'] = [20, 10]

# 读取数据
data_1 = pd.read_csv('user_data.csv')
data_2 = pd.read_csv('user_type.csv')
data_3 = pd.merge(data_1, data_2, on='user_id')
data_3['date'] = pd.to_datetime(data_3['date'])

# 筛选 behavior_type 为 1 的数据
views = data_3[data_3['behavior_type'] == 1]
views_counts = views.groupby('date').size().reset_index(name='views').sort_values('date')
views_woman = views[views['gender'] == 0]
views_woman_counts = views_woman.groupby('date').size().reset_index(name='views_woman').sort_values('date')
views_man = views[views['gender'] == 1]
views_man_counts = views_man.groupby('date').size().reset_index(name='views_man').sort_values('date')
views_member = views[views['is_member'] == 1]
views_member_counts = views_member.groupby('date').size().reset_index(name='views_member').sort_values('date')
views_not_member = views[views['is_member'] == 0]
views_not_member_counts = views_not_member.groupby('date').size().reset_index(name='views_not_member').sort_values('date')

# 筛选 behavior_type 为 2 的数据
favorite = data_3[data_3['behavior_type'] == 2]
favorite_counts = favorite.groupby('date').size().reset_index(name='favorite').sort_values('date')
favorite_woman = favorite[favorite['gender'] == 0]
favorite_woman_counts = favorite_woman.groupby('date').size().reset_index(name='favorite_woman').sort_values('date')
favorite_man = favorite[favorite['gender'] == 1]
favorite_man_counts = favorite_man.groupby('date').size().reset_index(name='favorite_man').sort_values('date')
favorite_member = favorite[favorite['is_member'] == 1]
favorite_member_counts = favorite_member.groupby('date').size().reset_index(name='favorite_member').sort_values('date')
favorite_not_member = favorite[favorite['is_member'] == 0]
favorite_not_member_counts = favorite_not_member.groupby('date').size().reset_index(name='favorite_not_member').sort_values('date')

# 筛选 behavior_type 为 3 的数据
add_to_cart = data_3[data_3['behavior_type'] == 3]
add_to_cart_counts = add_to_cart.groupby('date').size().reset_index(name='add_to_cart').sort_values('date')
add_to_cart_woman = add_to_cart[add_to_cart['gender'] == 0]
add_to_cart_woman_counts = add_to_cart_woman.groupby('date').size().reset_index(name='add_to_cart_woman').sort_values('date')
add_to_cart_man = add_to_cart[add_to_cart['gender'] == 1]
add_to_cart_man_counts = add_to_cart_man.groupby('date').size().reset_index(name='add_to_cart_man').sort_values('date')
add_to_cart_member = add_to_cart[add_to_cart['is_member'] == 1]
add_to_cart_member_counts = add_to_cart_member.groupby('date').size().reset_index(name='add_to_cart_member').sort_values('date')
add_to_cart_not_member = add_to_cart[add_to_cart['is_member'] == 0]
add_to_cart_not_member_counts = add_to_cart_not_member.groupby('date').size().reset_index(name='add_to_cart_not_member').sort_values('date')

# 筛选 behavior_type 为 4 的数据
purchase = data_3[data_3['behavior_type'] == 4]
purchase_counts = purchase.groupby('date').size().reset_index(name='purchase').sort_values('date')
purchase_woman = purchase[purchase['gender'] == 0]
purchase_woman_counts = purchase_woman.groupby('date').size().reset_index(name='purchase_woman').sort_values('date')
purchase_man = purchase[purchase['gender'] == 1]
purchase_man_counts = purchase_man.groupby('date').size().reset_index(name='purchase_man').sort_values('date')
purchase_member = purchase[purchase['is_member'] == 1]
purchase_member_counts = purchase_member.groupby('date').size().reset_index(name='purchase_member').sort_values('date')
purchase_not_member = purchase[purchase['is_member'] == 0]
purchase_not_member_counts = purchase_not_member.groupby('date').size().reset_index(name='purchase_not_member').sort_values('date')

# 筛选 去重的unique_view 的数据
unique_daily_views = views.drop_duplicates(subset=['user_id', 'date'])
unique_daily_views_counts = unique_daily_views.groupby('date').size().reset_index(name='unique_views').sort_values('date')
unique_daily_views_woman = views_woman.drop_duplicates(subset=['user_id', 'date'])
unique_daily_views_woman_counts = unique_daily_views_woman.groupby('date').size().reset_index(name='unique_views_woman').sort_values('date')
unique_daily_views_man = views_man.drop_duplicates(subset=['user_id', 'date'])
unique_daily_views_man_counts = unique_daily_views_man.groupby('date').size().reset_index(name='unique_views_man').sort_values('date')
unique_daily_views_member = views_member.drop_duplicates(subset=['user_id', 'date'])
unique_daily_views_member_counts = unique_daily_views_member.groupby('date').size().reset_index(name='unique_views_member').sort_values('date')
unique_daily_views_not_member = views_not_member.drop_duplicates(subset=['user_id', 'date'])
unique_daily_views_not_member_counts = unique_daily_views_not_member.groupby('date').size().reset_index(name='unique_views_not_member').sort_values('date')

# 计算每个用户的购买次数, 筛选出购买2次及以上的用户
user_purchase_counts = purchase.groupby(['user_id', 'date']).size().reset_index(name='purchase_count')
repurchases = user_purchase_counts[user_purchase_counts['purchase_count'] >= 2]
repurchase_count = repurchases.groupby('date').size().reset_index(name='repurchase').sort_values('date')
user_purchase_counts_woman = purchase_woman.groupby(['user_id', 'date']).size().reset_index(name='purchase_count_woman')
repurchases_woman = user_purchase_counts_woman[user_purchase_counts_woman['purchase_count_woman'] >= 2]
repurchase_woman_count = repurchases_woman.groupby('date').size().reset_index(name='repurchase_woman').sort_values('date')
user_purchase_counts_man = purchase_man.groupby(['user_id', 'date']).size().reset_index(name='purchase_count_man')
repurchases_man = user_purchase_counts_man[user_purchase_counts_man['purchase_count_man'] >= 2]
repurchase_man_count = repurchases_man.groupby('date').size().reset_index(name='repurchase_man').sort_values('date')
user_purchase_counts_member = purchase_member.groupby(['user_id', 'date']).size().reset_index(name='purchase_count_member')
repurchases_member = user_purchase_counts_member[user_purchase_counts_member['purchase_count_member'] >= 2]
repurchase_member_count = repurchases_member.groupby('date').size().reset_index(name='repurchase_member').sort_values('date')
user_purchase_counts_not_member = purchase_not_member.groupby(['user_id', 'date']).size().reset_index(name='purchase_count_not_member')
repurchases_not_member = user_purchase_counts_not_member[user_purchase_counts_not_member['purchase_count_not_member'] >= 2]
repurchase_not_member_count = repurchases_not_member.groupby('date').size().reset_index(name='repurchase_not_member').sort_values('date')

merged_data_total = pd.merge(views_counts, favorite_counts, on='date')
merged_data_total = pd.merge(merged_data_total,add_to_cart_counts, on='date')
merged_data_total = pd.merge(merged_data_total,purchase_counts, on='date')
merged_data_total = pd.merge(merged_data_total,unique_daily_views_counts, on='date')
merged_data_total = pd.merge(merged_data_total,repurchase_count, on='date')
merged_data_total['purchase_rate_pv'] = merged_data_total['purchase'] / merged_data_total['views']
merged_data_total['purchase_rate_uv'] = merged_data_total['purchase'] / merged_data_total['unique_views']
merged_data_total['repurchase_rate'] = merged_data_total['repurchase'] / merged_data_total['purchase']
merged_data_total['view_to_favorite'] = merged_data_total['favorite'] / merged_data_total['unique_views']
merged_data_total['favorite_to_purchase'] = merged_data_total['purchase'] / merged_data_total['favorite']
merged_data_total['add_to_cart_to_purchase'] = merged_data_total['purchase'] / merged_data_total['add_to_cart']
merged_data_total['favorite_to_add_to_cart'] = merged_data_total['add_to_cart'] / merged_data_total['favorite']

merged_data_woman = pd.merge(views_woman_counts, favorite_woman_counts, on='date')
merged_data_woman = pd.merge(merged_data_woman,add_to_cart_woman_counts, on='date')
merged_data_woman = pd.merge(merged_data_woman,purchase_woman_counts, on='date')
merged_data_woman = pd.merge(merged_data_woman,unique_daily_views_woman_counts, on='date')
merged_data_woman = pd.merge(merged_data_woman,repurchase_woman_count, on='date')
merged_data_woman['purchase_rate_pv_woman'] = merged_data_woman['purchase_woman'] / merged_data_woman['views_woman']
merged_data_woman['purchase_rate_uv_woman'] = merged_data_woman['purchase_woman'] / merged_data_woman['unique_views_woman']
merged_data_woman['repurchase_rate_woman'] = merged_data_woman['repurchase_woman'] / merged_data_woman['purchase_woman']
merged_data_woman['view_to_favorite_woman'] = merged_data_woman['favorite_woman'] / merged_data_woman['unique_views_woman']
merged_data_woman['favorite_to_purchase_woman'] = merged_data_woman['purchase_woman'] / merged_data_woman['favorite_woman']
merged_data_woman['add_to_cart_to_purchase_woman'] = merged_data_woman['purchase_woman'] / merged_data_woman['add_to_cart_woman']
merged_data_woman['favorite_to_add_to_cart_woman'] = merged_data_woman['add_to_cart_woman'] / merged_data_woman['favorite_woman']

merged_data_man = pd.merge(views_man_counts, favorite_man_counts, on='date')
merged_data_man = pd.merge(merged_data_man,add_to_cart_man_counts, on='date')
merged_data_man = pd.merge(merged_data_man,purchase_man_counts, on='date')
merged_data_man = pd.merge(merged_data_man,unique_daily_views_man_counts, on='date')
merged_data_man = pd.merge(merged_data_man,repurchase_man_count, on='date')
merged_data_man['purchase_rate_pv_man'] = merged_data_man['purchase_man'] / merged_data_man['views_man']
merged_data_man['purchase_rate_uv_man'] = merged_data_man['purchase_man'] / merged_data_man['unique_views_man']
merged_data_man['repurchase_rate_man'] = merged_data_man['repurchase_man'] / merged_data_man['purchase_man']
merged_data_man['view_to_favorite_man'] = merged_data_man['favorite_man'] / merged_data_man['unique_views_man']
merged_data_man['favorite_to_purchase_man'] = merged_data_man['purchase_man'] / merged_data_man['favorite_man']
merged_data_man['add_to_cart_to_purchase_man'] = merged_data_man['purchase_man'] / merged_data_man['add_to_cart_man']
merged_data_man['favorite_to_add_to_cart_man'] = merged_data_man['add_to_cart_man'] / merged_data_man['favorite_man']

merged_data_member = pd.merge(views_member_counts, favorite_member_counts, on='date')
merged_data_member = pd.merge(merged_data_member,add_to_cart_member_counts, on='date')
merged_data_member = pd.merge(merged_data_member,purchase_member_counts, on='date')
merged_data_member = pd.merge(merged_data_member,unique_daily_views_member_counts, on='date')
merged_data_member = pd.merge(merged_data_member,repurchase_member_count, on='date')
merged_data_member['purchase_rate_pv_member'] = merged_data_member['purchase_member'] / merged_data_member['views_member']
merged_data_member['purchase_rate_uv_member'] = merged_data_member['purchase_member'] / merged_data_member['unique_views_member']
merged_data_member['repurchase_rate_member'] = merged_data_member['repurchase_member'] / merged_data_member['purchase_member']
merged_data_member['view_to_favorite_member'] = merged_data_member['favorite_member'] / merged_data_member['unique_views_member']
merged_data_member['favorite_to_purchase_member'] = merged_data_member['purchase_member'] / merged_data_member['favorite_member']
merged_data_member['add_to_cart_to_purchase_member'] = merged_data_member['purchase_member'] / merged_data_member['add_to_cart_member']
merged_data_member['favorite_to_add_to_cart_member'] = merged_data_member['add_to_cart_member'] / merged_data_member['favorite_member']

merged_data_not_member = pd.merge(views_not_member_counts, favorite_not_member_counts, on='date')
merged_data_not_member = pd.merge(merged_data_not_member,add_to_cart_not_member_counts, on='date')
merged_data_not_member = pd.merge(merged_data_not_member,purchase_not_member_counts, on='date')
merged_data_not_member = pd.merge(merged_data_not_member,unique_daily_views_not_member_counts, on='date')
merged_data_not_member = pd.merge(merged_data_not_member,repurchase_not_member_count, on='date')
merged_data_not_member['purchase_rate_pv_not_member'] = merged_data_not_member['purchase_not_member'] / merged_data_not_member['views_not_member']
merged_data_not_member['purchase_rate_uv_not_member'] = merged_data_not_member['purchase_not_member'] / merged_data_not_member['unique_views_not_member']
merged_data_not_member['repurchase_rate_not_member'] = merged_data_not_member['repurchase_not_member'] / merged_data_not_member['purchase_not_member']
merged_data_not_member['view_to_favorite_not_member'] = merged_data_not_member['favorite_not_member'] / merged_data_not_member['unique_views_not_member']
merged_data_not_member['favorite_to_purchase_not_member'] = merged_data_not_member['purchase_not_member'] / merged_data_not_member['favorite_not_member']
merged_data_not_member['add_to_cart_to_purchase_not_member'] = merged_data_not_member['purchase_not_member'] / merged_data_not_member['add_to_cart_not_member']
merged_data_not_member['favorite_to_add_to_cart_not_member'] = merged_data_not_member['add_to_cart_not_member'] / merged_data_not_member['favorite_not_member']

merged_data = pd.merge(merged_data_total, merged_data_woman, on='date')
merged_data = pd.merge(merged_data, merged_data_man, on='date')
merged_data = pd.merge(merged_data, merged_data_member, on='date')
merged_data = pd.merge(merged_data, merged_data_not_member, on='date')

merged_data['date'] = pd.to_datetime(merged_data['date'])
merged_data['date_numeric'] = mdates.date2num(merged_data['date'])

# merged_data['purchase_rate_pv'] = round(merged_data['purchase_rate_pv'], 3)
# merged_data['purchase_rate_uv'] = round(merged_data['purchase_rate_uv'], 3)
# merged_data['repurchase_rate'] = round(merged_data['repurchase_rate'], 3)
# merged_data.plot(x='date', y=['purchase_rate_pv','purchase_rate_uv','repurchase_rate'], kind='line')
# plt.title('Daily Behavior Rates')
# plt.xlabel('Date')
# plt.ylabel('Rates')
# for i in range(len(merged_data)):
#     plt.annotate(str(merged_data.iloc[i]['purchase_rate_pv']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_rate_pv']), ha='center', va='bottom', fontsize=4)
#     plt.annotate(str(merged_data.iloc[i]['purchase_rate_uv']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_rate_uv']), ha='center', va='bottom', fontsize=4)
#     plt.annotate(str(merged_data.iloc[i]['repurchase_rate']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['repurchase_rate']), ha='center', va='bottom', fontsize=4)
# plt.savefig("purchase_rate.pdf", format="pdf")
# plt.show()

ax = merged_data.plot(x='date', y='views', kind='line')
plt.xlabel('Date')
plt.ylabel('times')
plt.title('Daily_pv')
for i in range(len(merged_data)):
    plt.annotate(str(merged_data.iloc[i]['views']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['views']), ha='center', va='bottom', fontsize=10)
ax.scatter(merged_data['date_numeric'], merged_data['views'], color='black', s=5)
plt.savefig("daily_pv.pdf", format="pdf")
plt.show()

ax = merged_data.plot(x='date', y='unique_views', kind='line')
plt.xlabel('Date')
plt.ylabel('times')
plt.title('Daily_uv')
for i in range(len(merged_data)):
    plt.annotate(str(merged_data.iloc[i]['unique_views']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['unique_views']), ha='center', va='bottom', fontsize=10)
ax.scatter(merged_data['date_numeric'], merged_data['unique_views'], color='black', s=5)
plt.savefig("daily_uv.pdf", format="pdf")
plt.show()

merged_data['purchase_rate_pv'] = round(merged_data['purchase_rate_pv'], 3)
merged_data['purchase_rate_uv'] = round(merged_data['purchase_rate_uv'], 3)
ax = merged_data.plot(x='date', y=['purchase_rate_pv','purchase_rate_uv'], kind='line')
plt.title('Daily_Behavior_Rates')
plt.xlabel('Date')
plt.ylabel('Perchase_Rates')
for i in range(len(merged_data)):
    plt.annotate(str(merged_data.iloc[i]['purchase_rate_pv']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_rate_pv']), ha='center', va='bottom', fontsize=10)
    plt.annotate(str(merged_data.iloc[i]['purchase_rate_uv']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_rate_uv']), ha='center', va='bottom', fontsize=10)
ax.scatter(merged_data['date_numeric'], merged_data['purchase_rate_pv'], color='black', s=5)
ax.scatter(merged_data['date_numeric'], merged_data['purchase_rate_uv'], color='black', s=5)
plt.savefig("purchase_rate.pdf", format="pdf")
plt.show()

merged_data['repurchase_rate'] = round(merged_data['repurchase_rate'], 3)
ax = merged_data.plot(x='date', y='repurchase_rate', kind='line')
plt.xlabel('Date')
plt.ylabel('Rates')
plt.title('Reperchase_Rate')
for i in range(len(merged_data)):
    plt.annotate(str(merged_data.iloc[i]['repurchase_rate']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['repurchase_rate']), ha='center', va='bottom', fontsize=10)
ax.scatter(merged_data['date_numeric'], merged_data['repurchase_rate'], color='black', s=5)
plt.savefig("reperchase_rate.pdf", format="pdf")
plt.show()

merged_data['view_to_favorite'] = round(merged_data['view_to_favorite'], 3)
merged_data['favorite_to_add_to_cart'] = round(merged_data['favorite_to_add_to_cart'], 3)
merged_data['add_to_cart_to_purchase'] = round(merged_data['add_to_cart_to_purchase'], 3)
ax = merged_data.plot(x='date', y=['view_to_favorite','favorite_to_add_to_cart','add_to_cart_to_purchase'], kind='line')
plt.title('Funnel')
plt.xlabel('Date')
plt.ylabel('Rates')
for i in range(len(merged_data)):
    plt.annotate(str(merged_data.iloc[i]['view_to_favorite']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['view_to_favorite']), ha='center', va='bottom', fontsize=10)
    plt.annotate(str(merged_data.iloc[i]['favorite_to_add_to_cart']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['favorite_to_add_to_cart']), ha='center', va='bottom', fontsize=10)
    plt.annotate(str(merged_data.iloc[i]['add_to_cart_to_purchase']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['add_to_cart_to_purchase']), ha='center', va='bottom', fontsize=10)
ax.scatter(merged_data['date_numeric'], merged_data['view_to_favorite'], color='black', s=5)
ax.scatter(merged_data['date_numeric'], merged_data['favorite_to_add_to_cart'], color='black', s=5)
ax.scatter(merged_data['date_numeric'], merged_data['add_to_cart_to_purchase'], color='black', s=5)
plt.savefig("funnel.pdf", format="pdf")
plt.show()

merged_data['purchase_rate_uv'] = round(merged_data['purchase_rate_uv'], 3)
ax = merged_data.plot(x='date', y='purchase_rate_uv', kind='line')
plt.title('Purchase_rate_uv')
plt.xlabel('Date')
plt.ylabel('Perchase_Rates')
for i in range(len(merged_data)):
    plt.annotate(str(merged_data.iloc[i]['purchase_rate_uv']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_rate_uv']), ha='center', va='bottom', fontsize=10)
ax.scatter(merged_data['date_numeric'], merged_data['purchase_rate_uv'], color='black', s=5)
plt.savefig("purchase_rate_uv.pdf", format="pdf")
plt.show()

merged_data['purchase'] = round(merged_data['purchase'], 3)
ax = merged_data.plot(x='date', y='purchase', kind='line')
plt.title('purchase')
plt.xlabel('Date')
plt.ylabel('Perchase_Rates')
for i in range(len(merged_data)):
    plt.annotate(str(merged_data.iloc[i]['purchase']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase']), ha='center', va='bottom', fontsize=10)
ax.scatter(merged_data['date_numeric'], merged_data['purchase'], color='black', s=5)
plt.savefig("purchase.pdf", format="pdf")
plt.show()

member_woman = purchase[purchase['is_member'] == 1]
member_woman = member_woman[member_woman['gender'] == 0]
purchase_member_woman = member_woman.groupby('date').size().reset_index(name='purchase_member_woman').sort_values('date')

not_member_woman = purchase[purchase['is_member'] == 0]
not_member_woman = not_member_woman[not_member_woman['gender'] == 0]
purchase_not_member_woman = not_member_woman.groupby('date').size().reset_index(name='purchase_not_member_woman').sort_values('date')

member_not_woman = purchase[purchase['is_member'] == 1]
member_not_woman = member_not_woman[member_not_woman['gender'] == 1]
purchase_member_not_woman = member_not_woman.groupby('date').size().reset_index(name='purchase_member_not_woman').sort_values('date')

not_member_not_woman = purchase[purchase['is_member'] == 0]
not_member_not_woman = not_member_not_woman[not_member_not_woman['gender'] == 1]
purchase_not_member_not_woman = not_member_not_woman.groupby('date').size().reset_index(name='purchase_not_member_not_woman').sort_values('date')

merged_member_woman = pd.merge(purchase_member_woman, purchase_not_member_woman, on='date')
merged_member_woman = pd.merge(merged_member_woman, purchase_member_not_woman, on='date')
merged_member_woman = pd.merge(merged_member_woman, purchase_not_member_not_woman, on='date')

merged_data = pd.merge(merged_data, merged_member_woman, on='date')

ax = merged_data.plot(x='date', y=['purchase_member_woman','purchase_not_member_woman', 'purchase_member_not_woman','purchase_not_member_not_woman'], kind='line')
plt.xlabel('Date')
plt.ylabel('times')
plt.title('Daily_member_woman')
for i in range(len(merged_data)):
    plt.annotate(str(merged_data.iloc[i]['purchase_member_woman']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_member_woman']), ha='center', va='bottom', fontsize=10)
    plt.annotate(str(merged_data.iloc[i]['purchase_not_member_woman']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_not_member_woman']), ha='center', va='bottom', fontsize=10)
    plt.annotate(str(merged_data.iloc[i]['purchase_member_not_woman']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_member_not_woman']), ha='center', va='bottom', fontsize=10)
    plt.annotate(str(merged_data.iloc[i]['purchase_not_member_not_woman']), xy=(merged_data.iloc[i]['date'], merged_data.iloc[i]['purchase_not_member_not_woman']), ha='center', va='bottom', fontsize=10)
ax.scatter(merged_data['date_numeric'], merged_data['purchase_member_woman'], color='black', s=5)
ax.scatter(merged_data['date_numeric'], merged_data['purchase_not_member_woman'], color='black', s=5)
ax.scatter(merged_data['date_numeric'], merged_data['purchase_member_not_woman'], color='black', s=5)
ax.scatter(merged_data['date_numeric'], merged_data['purchase_not_member_not_woman'], color='black', s=5)
plt.savefig("daily_member_woman.pdf", format="pdf")
plt.show()