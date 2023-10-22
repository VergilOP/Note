from bs4 import BeautifulSoup
import requests

def fetch_html_from_url(url):
    response = requests.get(url)
    if response.status_code == 200:  # HTTP OK
        return response.text
    else:
        return None

url = "https://news.sky.com/story/israel-hamas-war-17-british-nationals-feared-dead-or-missing-including-children-12982229"
html_content = fetch_html_from_url(url)


# with open('c:/_Study_Resource/Study_Note/University/3rd/Project/Test/Luton Airport fire_ All flights suspended after massive blaze causes terminal car park to partially collapse _ UK News _ Sky News.html', 'r', encoding = "utf-8") as file:
#    html_doc = file.read()

soup = BeautifulSoup(html_content, 'html.parser')

classes_to_remove = [
    'sdc-site-layout-sticky-region__target',
    'sdc-site-component-top__media',
    'sdc-article-widget sdc-article-image',
    'sdc-article-widget sdc-article-factbox',
    'sdc-article-widget sdc-article-tweet',
    'sdc-article-widget sdc-site-video callfn',
    'sdc-site-layout-sticky-region sdc-site-layout-sticky-region__ghost',
    'ui-beyond-words',
    'site-share-wrapper site-component-vertical-margin site-wrap-padding sdc-article-body-width-limiter',
    'sdc-trust-project sdc-article-body-width-limiter site-wrap-padding',
    'sdc-site-outbrain sdc-site-outbrain--AR_3',
]

for class_name in classes_to_remove:
    for tag in soup.find_all(class_=class_name):
        tag.decompose()

with open('c:/_Study_Resource/Study_Note/University/3rd/Project/Test/output.html', 'w', encoding='utf-8') as file:
    file.write(str(soup))