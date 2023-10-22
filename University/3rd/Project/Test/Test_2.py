import requests

url = 'https://www.bbc.co.uk/news/business-67084533'
response = requests.get(url)

# 确保请求成功
if response.status_code == 200:
    with open('c:/_Study_Resource/Study_Note/University/3rd/Project/Test/output_2.html', 'w', encoding='utf-8') as file:
        file.write(response.text)
else:
    print("请求失败，状态码：", response.status_code)
