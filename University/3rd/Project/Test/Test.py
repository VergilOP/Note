import feedparser

# Sky News RSS feed URL
SKY_NEWS_RSS_URL = "https://feeds.skynews.com/feeds/rss/home.xml"

# 获取RSS feed内容
def fetch_rss_content(url):
    feed = feedparser.parse(url)
    articles = []
    for entry in feed.entries:
        title = entry.title
        description = entry.description
        content = entry.content[0].value if hasattr(entry, 'content') else None
        articles.append((title, description, content))
    return articles

def main():
    articles = fetch_rss_content(SKY_NEWS_RSS_URL)
    
    for title, description, content in articles:
        print(f"Title: {title}\n")
        print(f"Description: {description}\n")
        if content:
            print(f"Content: {content}\n")
        print("-" * 100)

if __name__ == "__main__":
    main()
