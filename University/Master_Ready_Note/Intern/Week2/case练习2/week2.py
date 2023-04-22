import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
import statsmodels.stats.weightstats as sw
from scipy import stats

plt.rcParams['figure.figsize'] = [20, 10]

# 定义一个函数，用于在柱子上添加数据标签
def add_labels(ax):
    rects = ax.patches
    for rect in rects:
        height = rect.get_height()
        y_pos = rect.get_y() + height / 2
        ax.annotate(
            f"{height:.3f}",
            (rect.get_x() + rect.get_width() / 2, y_pos),
            ha="center",
            va="center",
            fontsize=8,
            rotation=0,
        )

# 读取数据
data_1 = pd.read_csv('ab_test.csv')
data_2 = pd.read_csv('user_type.csv')
data_3 = pd.merge(data_1, data_2, on='user_id')

# 把a组跟b组分类
a_group = data_3[data_3['treatment'] == 'A']
b_group = data_3[data_3['treatment'] == 'B']

# 计算每组中每种行为类型的比例
total_a_group = len(a_group)
total_b_group = len(b_group)

# 筛选view

a_views = a_group[a_group['behavior_type'] == 1]
a_views_counts = a_views.groupby('treatment').size().reset_index(name='a_views').sort_values('treatment')

b_views = b_group[b_group['behavior_type'] == 1]
b_views_counts = b_views.groupby('treatment').size().reset_index(name='b_views').sort_values('treatment')

## 处理性别

a_views_male = a_views[a_views['gender'] == 1]
a_views_female = a_views[a_views['gender'] == 0]
a_views_male_counts = len(a_views_male) / total_a_group
a_views_female_counts = len(a_views_female) / total_a_group

b_views_male = b_views[b_views['gender'] == 1]
b_views_female = b_views[b_views['gender'] == 0]
b_views_male_counts = len(b_views_male) / total_b_group
b_views_female_counts = len(b_views_female) / total_b_group

## 处理会员

a_views_is_not_member = a_views[a_views['is_member'] == 0]
a_views_is_member = a_views[a_views['is_member'] == 1]
a_views_is_not_member_counts = len(a_views_is_not_member) / total_a_group
a_views_is_member_counts = len(a_views_is_member) / total_a_group

b_views_is_not_member = b_views[b_views['is_member'] == 0]
b_views_is_member = b_views[b_views['is_member'] == 1]
b_views_is_not_member_counts = len(b_views_is_not_member) / total_b_group
b_views_is_member_counts = len(b_views_is_member) / total_b_group

# 筛选favorite

a_favorite = a_group[a_group['behavior_type'] == 2]
a_favorite_counts = a_favorite.groupby('treatment').size().reset_index(name='a_favorite').sort_values('treatment')

b_favorite = b_group[b_group['behavior_type'] == 2]
b_favorite_counts = b_favorite.groupby('treatment').size().reset_index(name='b_favorite').sort_values('treatment')

## 处理性别

a_favorite_male = a_favorite[a_favorite['gender'] == 1]
a_favorite_female = a_favorite[a_favorite['gender'] == 0]
a_favorite_male_counts = len(a_favorite_male) / total_a_group
a_favorite_female_counts = len(a_favorite_female) / total_a_group

b_favorite_male = b_favorite[b_favorite['gender'] == 1]
b_favorite_female = b_favorite[b_favorite['gender'] == 0]
b_favorite_male_counts = len(b_favorite_male) / total_a_group
b_favorite_female_counts = len(b_favorite_female) / total_a_group

## 处理会员

a_favorite_is_not_member = a_favorite[a_favorite['is_member'] == 0]
a_favorite_is_member = a_favorite[a_favorite['is_member'] == 1]
a_favorite_is_not_member_counts = len(a_favorite_is_not_member) / total_a_group
a_favorite_is_member_counts = len(a_favorite_is_member) / total_a_group

b_favorite_is_not_member = b_favorite[b_favorite['is_member'] == 0]
b_favorite_is_member = b_favorite[b_favorite['is_member'] == 1]
b_favorite_is_not_member_counts = len(b_favorite_is_not_member) / total_a_group
b_favorite_is_member_counts = len(b_favorite_is_member) / total_a_group

# 筛选add_to_cart

a_add_to_cart = a_group[a_group['behavior_type'] == 3]
a_add_to_cart_counts = a_add_to_cart.groupby('treatment').size().reset_index(name='a_add_to_cart').sort_values('treatment')

b_add_to_cart = b_group[b_group['behavior_type'] == 3]
b_add_to_cart_counts = b_add_to_cart.groupby('treatment').size().reset_index(name='b_add_to_cart').sort_values('treatment')

## 处理性别

a_add_to_cart_male = a_add_to_cart[a_add_to_cart['gender'] == 1]
a_add_to_cart_female = a_add_to_cart[a_add_to_cart['gender'] == 0]
a_add_to_cart_male_counts = len(a_add_to_cart_male) / total_a_group
a_add_to_cart_female_counts = len(a_add_to_cart_female) / total_a_group

b_add_to_cart_male = b_add_to_cart[b_add_to_cart['gender'] == 1]
b_add_to_cart_female = b_add_to_cart[b_add_to_cart['gender'] == 0]
b_add_to_cart_male_counts = len(b_add_to_cart_male) / total_a_group
b_add_to_cart_female_counts = len(b_add_to_cart_female) / total_a_group

## 处理会员

a_add_to_cart_is_not_member = a_add_to_cart[a_add_to_cart['is_member'] == 0]
a_add_to_cart_is_member = a_add_to_cart[a_add_to_cart['is_member'] == 1]
a_add_to_cart_is_not_member_counts = len(a_add_to_cart_is_not_member) / total_a_group
a_add_to_cart_is_member_counts = len(a_add_to_cart_is_member) / total_a_group

b_add_to_cart_is_not_member = b_add_to_cart[b_add_to_cart['is_member'] == 0]
b_add_to_cart_is_member = b_add_to_cart[b_add_to_cart['is_member'] == 1]
b_add_to_cart_is_not_member_counts = len(b_add_to_cart_is_not_member) / total_a_group
b_add_to_cart_is_member_counts = len(b_add_to_cart_is_member) / total_a_group

# 筛选purchase

a_purchase = a_group[a_group['behavior_type'] == 4]
a_purchase_counts = a_purchase.groupby('treatment').size().reset_index(name='a_purchase').sort_values('treatment')

b_purchase = b_group[b_group['behavior_type'] == 4]
b_purchase_counts = b_purchase.groupby('treatment').size().reset_index(name='b_purchase').sort_values('treatment')

## 处理性别

a_purchase_male = a_purchase[a_purchase['gender'] == 1]
a_purchase_female = a_purchase[a_purchase['gender'] == 0]
a_purchase_male_counts = len(a_purchase_male) / total_a_group
a_purchase_female_counts = len(a_purchase_female) / total_a_group

b_purchase_male = b_purchase[b_purchase['gender'] == 1]
b_purchase_female = b_purchase[b_purchase['gender'] == 0]
b_purchase_male_counts = len(b_purchase_male) / total_a_group
b_purchase_female_counts = len(b_purchase_female) / total_a_group

## 处理会员

a_purchase_is_not_member = a_purchase[a_purchase['is_member'] == 0]
a_purchase_is_member = a_purchase[a_purchase['is_member'] == 1]
a_purchase_is_not_member_counts = len(a_purchase_is_not_member) / total_a_group
a_purchase_is_member_counts = len(a_purchase_is_member) / total_a_group

b_purchase_is_not_member = b_purchase[b_purchase['is_member'] == 0]
b_purchase_is_member = b_purchase[b_purchase['is_member'] == 1]
b_purchase_is_not_member_counts = len(b_purchase_is_not_member) / total_a_group
b_purchase_is_member_counts = len(b_purchase_is_member) / total_a_group

#总人数(非比例)
# 创建一个合并的计数数据框
counts_data = pd.DataFrame({
    'behavior_type': ['view', 'favorite', 'add_to_cart', 'purchase'],
    'A': [a_views_counts['a_views'].values[0], a_favorite_counts['a_favorite'].values[0], a_add_to_cart_counts['a_add_to_cart'].values[0], a_purchase_counts['a_purchase'].values[0]],
    'B': [b_views_counts['b_views'].values[0], b_favorite_counts['b_favorite'].values[0], b_add_to_cart_counts['b_add_to_cart'].values[0], b_purchase_counts['b_purchase'].values[0]]
})

# 绘制柱状图
ax = counts_data.plot.bar(x='behavior_type', y=['A', 'B'], rot=0)
ax.set_ylabel("Counts")
ax.set_title("Behavior Type Counts for A and B Groups")
add_labels(ax)

# 设置纵轴的比例
ax.set_ylim([0, 100000])
ax.yaxis.set_ticks(np.arange(0, 1.1, 0.1))
ax.set_yscale('symlog', linthresh=1000)

plt.savefig("实验分流均匀对照_数量.pdf", format="pdf")
plt.show()

# 实验分流的均匀对照_性别

counts_data = pd.DataFrame({
    'behavior_type': ['view', 'favorite', 'add_to_cart', 'purchase'],
    'A_male': [a_views_male_counts, a_favorite_male_counts, a_add_to_cart_male_counts, a_purchase_male_counts],
    'A_female': [a_views_female_counts, a_favorite_female_counts, a_add_to_cart_female_counts, a_purchase_female_counts],
    'B_male': [b_views_male_counts, b_favorite_male_counts, b_add_to_cart_male_counts, b_purchase_male_counts],
    'B_female': [b_views_female_counts, b_favorite_female_counts, b_add_to_cart_female_counts, b_purchase_female_counts],
})

# 绘制柱状图
fig, ax = plt.subplots()

# 设置 bar 宽度
bar_width = 0.35

# 设置柱子位置
x = np.arange(len(counts_data['behavior_type']))

# 绘制 A_male 和 A_female 叠在一起的柱子
ax.bar(x - bar_width / 2, counts_data['A_male'], bar_width, label='A_male', color='blue', edgecolor='black')
ax.bar(x - bar_width / 2, counts_data['A_female'], bar_width, label='A_female', color='green', edgecolor='black', bottom=counts_data['A_male'])


ax.bar(x + bar_width / 2, counts_data['B_male'], bar_width, label='B_male', color='lightblue', edgecolor='black')
ax.bar(x + bar_width / 2, counts_data['B_female'], bar_width, label='B_female', color='lightgreen', edgecolor='black', bottom=counts_data['B_male'])

# 添加图例
ax.legend()

# 设置 x 轴标签
ax.set_xticks(x)
ax.set_xticklabels(counts_data['behavior_type'])

ax.set_ylabel("Proportion")
ax.set_title("Behavior Type Proportions for A and B Groups (A Group Split by Gender)")

# 设置纵轴的比例
ax.set_ylim([0, 1])
ax.yaxis.set_ticks(np.arange(0, 1.1, 0.1))
ax.set_yscale('symlog', linthresh=0.05)

add_labels(ax)
plt.savefig("实验分流均匀对照_性别.pdf", format="pdf")
plt.show()

# 实验分流的均匀对照_会员

counts_data = pd.DataFrame({
    'behavior_type': ['view', 'favorite', 'add_to_cart', 'purchase'],
    'A_is_not_member': [a_views_is_not_member_counts, a_favorite_is_not_member_counts, a_add_to_cart_is_not_member_counts, a_purchase_is_not_member_counts],
    'A_is_member': [a_views_is_member_counts, a_favorite_is_member_counts, a_add_to_cart_is_member_counts, a_purchase_is_member_counts],
    'B_is_not_member': [b_views_is_not_member_counts, b_favorite_is_not_member_counts, b_add_to_cart_is_not_member_counts, b_purchase_is_not_member_counts],
    'B_is_member': [b_views_is_member_counts, b_favorite_is_member_counts, b_add_to_cart_is_member_counts, b_purchase_is_member_counts],
})

# 绘制柱状图
fig, ax = plt.subplots()

# 设置 bar 宽度
bar_width = 0.35

# 设置柱子位置
x = np.arange(len(counts_data['behavior_type']))

# 绘制 A_is_not_member 和 A_is_member 叠在一起的柱子
ax.bar(x - bar_width / 2, counts_data['A_is_not_member'], bar_width, label='A_is_not_member', color='blue', edgecolor='black')
ax.bar(x - bar_width / 2, counts_data['A_is_member'], bar_width, label='A_is_member', color='green', edgecolor='black', bottom=counts_data['A_is_not_member'])


ax.bar(x + bar_width / 2, counts_data['B_is_not_member'], bar_width, label='B_is_not_member', color='lightblue', edgecolor='black')
ax.bar(x + bar_width / 2, counts_data['B_is_member'], bar_width, label='B_is_member', color='lightgreen', edgecolor='black', bottom=counts_data['B_is_not_member'])

# 添加图例
ax.legend()

# 设置 x 轴标签
ax.set_xticks(x)
ax.set_xticklabels(counts_data['behavior_type'])

ax.set_ylabel("Proportion")
ax.set_title("Behavior Type Proportions for A and B Groups (A Group Split by Member)")

# 设置纵轴的比例
ax.set_ylim([0, 1])
ax.yaxis.set_ticks(np.arange(0, 1.1, 0.1))
ax.set_yscale('symlog', linthresh=0.05)

add_labels(ax)
plt.savefig("实验分流均匀对照_会员.pdf", format="pdf")
plt.show()

# 实验分流的均匀对照_转化率

counts_data = pd.DataFrame({
    'behavior_type': ['view_to_favorite', 'favorite_to_purchase', 'add_to_cart_to_purchase', 'purchase'],
    'A_is_not_member': [a_views_is_not_member_counts, a_favorite_is_not_member_counts, a_add_to_cart_is_not_member_counts, a_purchase_is_not_member_counts],
    'A_is_member': [a_views_is_member_counts, a_favorite_is_member_counts, a_add_to_cart_is_member_counts, a_purchase_is_member_counts],
    'B_is_not_member': [b_views_is_not_member_counts, b_favorite_is_not_member_counts, b_add_to_cart_is_not_member_counts, b_purchase_is_not_member_counts],
    'B_is_member': [b_views_is_member_counts, b_favorite_is_member_counts, b_add_to_cart_is_member_counts, b_purchase_is_member_counts],
})

# 绘制柱状图
fig, ax = plt.subplots()

# 设置 bar 宽度
bar_width = 0.35

# 设置柱子位置
x = np.arange(len(counts_data['behavior_type']))

# 绘制 A_is_not_member 和 A_is_member 叠在一起的柱子
ax.bar(x - bar_width / 2, counts_data['A_is_not_member'], bar_width, label='A_is_not_member', color='blue', edgecolor='black')
ax.bar(x - bar_width / 2, counts_data['A_is_member'], bar_width, label='A_is_member', color='green', edgecolor='black', bottom=counts_data['A_is_not_member'])


ax.bar(x + bar_width / 2, counts_data['B_is_not_member'], bar_width, label='B_is_not_member', color='lightblue', edgecolor='black')
ax.bar(x + bar_width / 2, counts_data['B_is_member'], bar_width, label='B_is_member', color='lightgreen', edgecolor='black', bottom=counts_data['B_is_not_member'])

# 添加图例
ax.legend()

# 设置 x 轴标签
ax.set_xticks(x)
ax.set_xticklabels(counts_data['behavior_type'])

ax.set_ylabel("Proportion")
ax.set_title("Behavior Type Proportions for A and B Groups (A Group Split by Member)")

# 设置纵轴的比例
ax.set_ylim([0, 1])
ax.yaxis.set_ticks(np.arange(0, 1.1, 0.1))
ax.set_yscale('symlog', linthresh=0.05)

add_labels(ax)

plt.savefig("实验分流均匀对照_会员.pdf", format="pdf")
plt.show()

# 计算各子组的转化率和购买人均购买金额

# 计算转化率
a_conversion_rates = {
    'view': a_purchase_counts['a_purchase'].values[0] / a_views_counts['a_views'].values[0],
    'favorite': a_purchase_counts['a_purchase'].values[0] / a_favorite_counts['a_favorite'].values[0],
    'add_to_cart': a_purchase_counts['a_purchase'].values[0] / a_add_to_cart_counts['a_add_to_cart'].values[0]
}

b_conversion_rates = {
    'view': b_purchase_counts['b_purchase'].values[0] / b_views_counts['b_views'].values[0],
    'favorite': b_purchase_counts['b_purchase'].values[0] / b_favorite_counts['b_favorite'].values[0],
    'add_to_cart': b_purchase_counts['b_purchase'].values[0] / b_add_to_cart_counts['b_add_to_cart'].values[0]
}

# 计算 A 组男性、女性、会员和非会员的转化率
a_conversion_rates_male = {
    'view': a_purchase_male_counts/  ,
    'favorite': a_purchase_male_counts / a_favorite_male_counts,
    'add_to_cart': a_purchase_male_counts / a_add_to_cart_male_counts
}

a_conversion_rates_female = {
    'view': a_purchase_female_counts / a_views_female_counts,
    'favorite': a_purchase_female_counts / a_favorite_female_counts,
    'add_to_cart': a_purchase_female_counts / a_add_to_cart_female_counts
}

a_conversion_rates_is_member = {
    'view': a_purchase_is_member_counts / a_views_is_member_counts,
    'favorite': a_purchase_is_member_counts / a_favorite_is_member_counts,
    'add_to_cart': a_purchase_is_member_counts / a_add_to_cart_is_member_counts
}

a_conversion_rates_is_not_member = {
    'view': a_purchase_is_not_member_counts / a_views_is_not_member_counts,
    'favorite': a_purchase_is_not_member_counts / a_favorite_is_not_member_counts,
    'add_to_cart': a_purchase_is_not_member_counts / a_add_to_cart_is_not_member_counts
}

# 计算 B 组男性、女性、会员和非会员的转化率
b_conversion_rates_male = {
    'view': b_purchase_male_counts/ b_views_male_counts,
    'favorite': b_purchase_male_counts / b_favorite_male_counts,
    'add_to_cart': b_purchase_male_counts / b_add_to_cart_male_counts
}

b_conversion_rates_female = {
    'view': b_purchase_female_counts / b_views_female_counts,
    'favorite': b_purchase_female_counts / b_favorite_female_counts,
    'add_to_cart': b_purchase_female_counts / b_add_to_cart_female_counts
}

b_conversion_rates_is_member = {
    'view': b_purchase_is_member_counts / b_views_is_member_counts,
    'favorite': b_purchase_is_member_counts / b_favorite_is_member_counts,
    'add_to_cart': b_purchase_is_member_counts / b_add_to_cart_is_member_counts
}

b_conversion_rates_is_not_member = {
    'view': b_purchase_is_not_member_counts / b_views_is_not_member_counts,
    'favorite': b_purchase_is_not_member_counts / b_favorite_is_not_member_counts,
    'add_to_cart': b_purchase_is_not_member_counts / b_add_to_cart_is_not_member_counts
}

conversion_data = pd.DataFrame({
    'A_male': [a_conversion_rates_male['view'], a_conversion_rates_male['favorite'], a_conversion_rates_male['add_to_cart']],
    'A_female': [a_conversion_rates_female['view'], a_conversion_rates_female['favorite'], a_conversion_rates_female['add_to_cart']],
    'A_is_member': [a_conversion_rates_is_member['view'], a_conversion_rates_is_member['favorite'], a_conversion_rates_is_member['add_to_cart']],
    'A_is_not_member': [a_conversion_rates_is_not_member['view'], a_conversion_rates_is_not_member['favorite'], a_conversion_rates_is_not_member['add_to_cart']],
    'B_male': [b_conversion_rates_male['view'], b_conversion_rates_male['favorite'], b_conversion_rates_male['add_to_cart']],
    'B_female': [b_conversion_rates_female['view'], b_conversion_rates_female['favorite'], b_conversion_rates_female['add_to_cart']],
    'B_is_member': [b_conversion_rates_is_member['view'], b_conversion_rates_is_member['favorite'], b_conversion_rates_is_member['add_to_cart']],
    'B_is_not_member': [b_conversion_rates_is_not_member['view'], b_conversion_rates_is_not_member['favorite'], b_conversion_rates_is_not_member['add_to_cart']]
}, index=['view', 'favorite', 'add_to_cart'])

# 绘制堆叠柱状图
fig, axes = plt.subplots(nrows=1, ncols=2, figsize=(20, 10))

bar_width = 0.35
x = np.arange(len(conversion_data.index))

# 性别对比柱状图
axes[0].bar(x - bar_width / 2, conversion_data['A_male'], bar_width, label='A_male', color='blue', edgecolor='black')
axes[0].bar(x - bar_width / 2, conversion_data['A_female'], bar_width, label='A_female', color='green', edgecolor='black', bottom=conversion_data['A_male'])

axes[0].bar(x + bar_width / 2, conversion_data['B_male'], bar_width, label='B_male', color='lightblue', edgecolor='black')
axes[0].bar(x + bar_width / 2, conversion_data['B_female'], bar_width, label='B_female', color='lightgreen', edgecolor='black', bottom=conversion_data['B_male'])

axes[0].legend()
axes[0].set_xticks(x)
axes[0].set_xticklabels(conversion_data.index)
axes[0].set_ylabel("Conversion Rate")
axes[0].set_title("Conversion Rates by Gender for A and B Groups")

add_labels(axes[0])

# 会员对比柱状图
axes[1].bar(x - bar_width / 2, conversion_data['A_is_member'], bar_width, label='A_is_member', color='blue', edgecolor='black')
axes[1].bar(x - bar_width / 2, conversion_data['A_is_not_member'], bar_width, label='A_is_not_member', color='green', edgecolor='black', bottom=conversion_data['A_is_member'])

axes[1].bar(x + bar_width / 2, conversion_data['B_is_member'], bar_width, label='B_is_member', color='lightblue', edgecolor='black')
axes[1].bar(x + bar_width / 2, conversion_data['B_is_not_member'], bar_width, label='B_is_not_member', color='lightgreen', edgecolor='black', bottom=conversion_data['B_is_member'])

axes[1].legend()
axes[1].set_xticks(x)
axes[1].set_xticklabels(conversion_data.index)
axes[1].set_ylabel("Conversion Rate")
axes[1].set_title("Conversion Rates by Membership for A and B Groups")

add_labels(axes[1])

plt.subplots_adjust(wspace=0.3)
plt.savefig("实验分流转化率.pdf", format="pdf")
plt.show()

# 购买金额

def avg_revenue_by_group(df, gender=None, is_member=None):
    purchases = df[df['behavior_type'] == 4]
    if gender is not None:
        purchases = purchases[purchases['gender'] == gender]
    if is_member is not None:
        purchases = purchases[purchases['is_member'] == is_member]

    purchase_users = purchases['user_id'].nunique()
    avg_revenue = purchases['revenue'].sum() / purchase_users
    return avg_revenue

# 对每个子组计算转化率和平均收入
groups = ['male', 'female', 'is_member', 'is_not_member', 'all']
a_data = []
b_data = []

for group in groups:
    if group == 'male':
        a_values = avg_revenue_by_group(a_group, gender=1)
        b_values = avg_revenue_by_group(b_group, gender=1)
    elif group == 'female':
        a_values = avg_revenue_by_group(a_group, gender=0)
        b_values = avg_revenue_by_group(b_group, gender=0)
    elif group == 'is_member':
        a_values = avg_revenue_by_group(a_group, is_member=1)
        b_values = avg_revenue_by_group(b_group, is_member=1)
    elif group == 'is_not_member':
        a_values = avg_revenue_by_group(a_group, is_member=0)
        b_values = avg_revenue_by_group(b_group, is_member=0)
    else:
        a_values = avg_revenue_by_group(a_group)
        b_values = avg_revenue_by_group(b_group)

    a_data.append(a_values)
    b_data.append(b_values)

# 创建 DataFrame
avg_revenue_data = pd.DataFrame({
    'group': groups,
    'A_avg_revenue': a_data,
    'B_avg_revenue': b_data,
})

# 绘制柱状图
fig, ax = plt.subplots()
x = np.arange(len(avg_revenue_data['group']))
bar_width = 0.35

ax.bar(x - bar_width / 2, avg_revenue_data['A_avg_revenue'], bar_width, label='A', color='blue', edgecolor='black')
ax.bar(x + bar_width / 2, avg_revenue_data['B_avg_revenue'], bar_width, label='B', color='green', edgecolor='black')

ax.legend()
ax.set_xticks(x)
ax.set_xticklabels(avg_revenue_data['group'])
ax.set_ylabel("Average Revenue")
ax.set_title("Average Revenue by Group for A and B")

add_labels(ax)

plt.subplots_adjust(wspace=0.3)
plt.savefig("实验分流人均金额.pdf", format="pdf")
plt.show()



# 寻找显著人群

def revenue_by_group(df, gender=None, is_member=None):
    purchases = df[df['behavior_type'] == 4]
    if gender is not None:
        purchases = purchases[purchases['gender'] == gender]
    if is_member is not None:
        purchases = purchases[purchases['is_member'] == is_member]

    return purchases['revenue']

print(sw.ztest(revenue_by_group(a_group), revenue_by_group(b_group), value=0))
print(sw.ztest(revenue_by_group(a_group, gender=0, is_member=0),revenue_by_group(b_group, gender=0, is_member=0) , value=0))
print(sw.ztest(revenue_by_group(a_group, gender=1, is_member=0),revenue_by_group(b_group, gender=1, is_member=0) , value=0))
print(sw.ztest(revenue_by_group(a_group, gender=0, is_member=1),revenue_by_group(b_group, gender=0, is_member=1) , value=0))
print(sw.ztest(revenue_by_group(a_group, gender=1, is_member=1),revenue_by_group(b_group, gender=1, is_member=1) , value=0))

# (8.951065059372802, 3.520687879890345e-19)
# (6.293855486513557, 3.0967614748862214e-10)
# (3.8198188408996328, 0.00013354972484914368)
# (4.6475147384718865, 3.359580967886752e-06)
# (2.4626185981981155, 0.013792654878456814)
