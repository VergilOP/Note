# 应用

## 新闻摘要

### Concise Version: Daily Hot Search Summary Generation
1. **Project Objective**:
   - Automatically collect the top ten news/topics of daily hot searches.
   - Generate a summary for each news/topic.

2. **Motivation**:
   - Provide users with a platform for a quick overview of daily hot topics.
   - Leverage AI technology to enhance the efficiency of information digestion.

3. **Literature Review**:
   - Research the existing text summarization technologies.
   - Explore literature related to adaptive summarization, sentiment and opinion summary, and deep text interpretation.

4. **Methodology**:
   - Use web crawling techniques or APIs to collect hot search data.
   - Apply deep learning models to generate summaries, integrating features of adaptive adjustments, sentiment and opinion analysis, and deep text interpretation.

5. **Evaluation**:
   - Use standard evaluation metrics like ROUGE, BLEU, etc., to evaluate the summaries.
   - Consider collecting user feedback to understand the real-world effects and user satisfaction of the summaries.

增加项目的深度和挑战性：

1. **Project Objective**:
   - 除了自动生成每日热门新闻的摘要，还可以考虑提供每条新闻的情感倾向（正面、负面、中立）。

2. **Methodology**:
   - 尝试不仅使用现有的深度学习模型，还自己设计和调整模型结构来达到更好的摘要效果。
   - 考虑实现一个增量学习或在线学习的模式，使模型能够根据新的数据不断地更新和改进。
   - 对于情感分析，考虑使用深入的情感分析方法，如aspect-based sentiment analysis，这样可以为每个新闻的不同方面提供情感评分。

3. **Evaluation**:
   - 除了使用ROUGE和BLEU等传统指标外，还可以设计一些实验来评估模型在真实场景下的效果。例如，可以让真人评估摘要的质量和可读性，并与模型的评分进行对比。
   - 也可以设计一些用户测试，如A/B测试，来评估不同版本模型的实际效果。UAT

4. **User Interface**:
   - 考虑设计一个交互性更强的用户界面，例如，用户可以根据自己的需要调整摘要的长度，或者选择查看特定方面的情感分析。
   - 还可以提供一些可视化工具，如情感分析的雷达图，或者热门新闻的关键词云。

Q: 文献综述(literature review)/Project demonstrations, 40-credit(10,000 words)/60-credit(10,000 words)

1. Proposal Submission[mid-Oct]
   - Reviewed by the program directors
   - Convince us that you have a solid plan
2. Inspection Meeting[mid-Nov]
   - Between your inspector and yourself
   - Convince us that you are making good progress
3. Demonstration Meeting[late-March]
   - Between your inspector and yourself and marked by your inspector
   - Convince us that you have done what you proposed and produced good results
4. Final Report Submission[early-April]
   - Marked together by your supervisor and inspector
   - Put everything in writing

- 30% Report

- 20% Product
   - Application-based: A fully complete piece of software
   - Research-based: A combination of the outcome of the research and analysis and the prototype

- 20% Process
   - Application-based: Agile Methodology
   - Research-based: Pipeline

- 10% Demonstration/Management

- 20% Substantialness of Achievement

## Project Proposal

- General structure
  - Introduction
    - Significance
    - Motivation
    - Aims&Objectives
  - Literature review
    - What have been done already
    - Why previous solutions couldn't address the problem
  - Methodology(How you are going to address this problem)
  - Planning
    - Project Plan
    - Resource Plan
    - Contingency Plan

- Market research
- fake website of the news
- more functions/interviews
- get api from website(source)
- get api from openai(process)

**项目概述**：

- **名称**：Mirror News Summarizer（镜像新闻摘要器）

- **功能**：
  - 用户访问此“假”网站（例如 fake-bbc.com）后，看到与选定的真实新闻网站（如BBC）外观一致的界面。
  - 用户可以浏览新闻标题，与真实新闻网站的标题相同。
  - 当用户点击标题时，不显示原始新闻的全文，而是展示一个精炼的摘要。

- **目标**：为用户提供一个与真实新闻网站界面相似的体验，但内容是经过摘要处理的，以便用户在短时间内获得新闻的核心信息。

- **实现方式**：
  - 利用目标新闻网站的API（或其他方法）爬取其新闻内容。
  - 使用OpenAI API或其他摘要算法将新闻转换为简短摘要。
  - 在“假”网站上展示这些摘要。

**建议与可能的扩展**：

1. **真实性提示**：在网站的某个显眼位置，提醒用户这是一个为了提供新闻摘要而创建的模拟网站，与实际新闻网站无关。
2. **用户定制摘要长度**：允许用户选择摘要的长度，例如“超简短”、“中等”或“详细”。
3. **图形化摘要**：除了文字摘要，还可以为用户提供信息图或其他图形化内容，帮助他们更直观地了解新闻的核心信息。
4. **评论区模拟**：虽然是模拟网站，但可以允许用户在摘要下面留下评论，分享他们对新闻的看法。
5. **保存与分享**：允许用户保存他们感兴趣的摘要，并提供分享功能。
6. **用户反馈**：引入用户反馈机制，让用户评价摘要的质量和有用性。


**1. What is your project? – Aims and Objectives.**
   
   - **名称**: Mirror News Summarizer（镜像新闻摘要器）
   - **目标**：
     - 创建一个与目标新闻网站界面相似的摘要网站，例如BBC的模拟版本。
     - 自动提取新闻内容并生成简洁的摘要。
     - 为用户提供与真实新闻网站相似的体验，但内容是经过精炼的摘要。

**2. Why is it an important project? -- Motivation**

   - 随着新闻和信息的海量增长，人们很难迅速获得重要的信息。通过提供摘要，我们可以帮助用户更高效地消化内容。
   - 与传统的新闻摘要应用不同，这个项目还提供了一个独特的用户界面体验，模仿真实的新闻网站，从而提高用户的互动和参与度。

**3. Literature review – what people have done already.**

   - 过去，有许多文本摘要的研究，从简单的基于规则的方法到复杂的深度学习技术。
   - 现有的新闻摘要工具，如“SMMRY”和“AutoSummarizer”。
   - 研究表明，对于文本摘要的有效性，有一定的质疑，需要更为精确和适应性强的摘要方法。

**4. Methodology – how you are going to address the challenge(s).**

   - **数据收集**：使用目标新闻网站的API或其他web scraping技术收集新闻内容。
   - **摘要生成**：利用OpenAI API或其他先进的摘要算法将新闻转换为简短摘要。
   - **用户界面设计**：模仿目标新闻网站的界面设计，但将原始内容替换为摘要。
   - **用户交互**：添加功能，如用户自定义摘要长度、评论功能等。

**5. Evaluation – how you are going to evaluate your result(s).**

   - **自动评估**：使用ROUGE, BLEU等评估指标对生成的摘要进行自动评估。
   - **用户反馈**：通过用户调查和反馈机制收集用户对摘要质量的评价。
   - **性能评估**：测试网站的响应速度、摘要生成速度以及用户界面的流畅性。

---

这是一个基于你项目的初步框架。为了真正完成这个项目，你可能需要进行更深入的研究，尤其是在文献回顾部分，以及更详细的评估策略。

- Proposal 是否占比. 如果是,包含在process还是product
- 我的两个主题中哪个更好操作，另一个是否能行

- 网页的爬取
  - 用request直接爬取源网页
    - 需要自己设置好对应网页的风格与类型
    - 链接未经过处理，需要一种机制处理对应网站的链接(图片)
    - 工作量可能偏大，但更易处理
  - 用Selenium爬取javascript执行之后的网页
    - 不需要设置风格
    - 会下载所有内容，时间偏长，冗杂较多
    - 如果想添加风格不好添加

- report的占分比

- 是否必须ai

- 是否是创新点(Creative idea)

---

- Mirror News Summarizer
  - 主要功能，提供可选的几个镜像新闻网站，并优化外观，去除广告，并把内容替换为摘要
    - 相似的UI布局，网站主页只优化，不替换内容。
    - 每个单独的新闻页面，进行摘要替换
    - 两者都进行优化
  - 拥有一个单独的推荐页面，会通过统计用户点击的镜像新闻类别/热门与网站前几的热门新闻来进行推荐(某种推荐算法)
    - 在推荐界面能够进行页面风格的自定义，例如更喜欢表格类型或者小卡片形式
    - 在点击单独新闻时，新闻摘要会以弹窗的形式在屏幕偏右侧展示，并且左边会保持新闻的目录
    - 文本相似度分析，来对不用网站的相同内容进行分别，最终只会呈现一种
  - 对于每一个新闻单独页面，都能收藏，并且对摘要内容进行反馈
    - 如果是镜像的新闻，将会转换为推荐页面设置好的格式
    - 拥有一个单独的收藏夹用来查看收藏的内容
    - 同样可以自定义外观
    - 自适应内容摘要: 根据用户历史行为和反馈，动态调整摘要的长度和内容。
    - 语音功能集成: 为方便用户在忙碌时获取新闻，可以添加文本到语音（TTS）功能，使用户能够听取新闻摘要。
    - 暗模式与可视化主题:提供暗模式，减少在低光环境下阅读的眼睛疲劳。
    - 多语言支持: 通过添加多语言支持，拓展更广泛的用户群体，特别是非英语背景用户。

- 方法论
  - 新闻的内容获取运用request爬虫
  - 新闻的总结运用openai api
  - 推荐系统运用现有的推荐模型
  - 前端使用react，后端使用flask