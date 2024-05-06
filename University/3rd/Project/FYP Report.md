# Enhancing Online News Consumption with AI: The Development of Mirror News Summarizer

## Abstract

The digital era has ushered in an unprecedented volume of news content, presenting a challenge in managing the deluge of information efficiently. The "Mirror News Summarizer" project introduces an innovative solution, employing Artificial Intelligence (AI) to streamline the news consumption process. This initiative aims to condense extensive news articles into concise summaries and offer personalized recommendations, significantly enhancing user engagement and simplifying information access. By addressing the critical challenge of digital information overload, this platform sets a new standard for digital news consumption.

## 1. Introduction

The exponential growth of digital media has revolutionized the dissemination and consumption of news, offering unprecedented access to a wide array of information sources. However, this digital renaissance has also ushered in the era of information overload, where the sheer volume of available news can overwhelm users, complicating their ability to extract value and relevance from the content they consume.

### 1.1 Motivation & Aims

Amidst the vast expanse of the digital news ecosystem, users often find themselves inundated with content, struggling to sift through extensive articles to find information that is pertinent and engaging. This project introduces the "Mirror News Summarizer," a pioneering platform designed to alleviate this challenge by utilizing AI to deliver concise summaries of news articles. These summaries aim to preserve the core essence of the original content, making news consumption more manageable and efficient for users. Furthermore, the platform incorporates a personalized recommendation system, which curates content based on individual user preferences, thus aiming to enrich the news reading experience by tailoring it to the specific interests and needs of each user.

### 1.2 Significance of Problem

The challenge of efficiently navigating the deluge of digital news is a significant concern for many users today. Information overload not only hampers the ability to stay informed but also leads to decision fatigue, where the excessive amount of choices overwhelms users, potentially reducing their overall engagement with news content. The "Mirror News Summarizer" project seeks to address these issues by offering a solution that both condenses news into digestible summaries and personalizes the content delivery process. This dual approach is anticipated to significantly reduce the cognitive load on users, thereby improving their information consumption efficiency and making the platform a vital innovation in the domain of digital news consumption.

### 1.3 Prior Work on Problem

While several platforms currently offer news summarization and recommendation services, they often fall short in terms of integration and personalization. Existing solutions may provide summaries but lack the ability to tailor content to the unique preferences of each user, resulting in a one-size-fits-all approach that fails to engage users fully. The "Mirror News Summarizer" distinguishes itself by not only summarizing content but also mirroring the format of the original articles and eliminating extraneous distractions such as advertisements. This focus on maintaining the integrity of the original content, coupled with a robust personalization engine, offers a unique and immersive reading experience that is customized for each user.

## 2. Background and Related Work

The concept of summarizing news articles and providing personalized content recommendations is not new; however, the implementation of such features varies significantly across different platforms. This section reviews the landscape of current solutions, highlighting their limitations and the gap that the "Mirror News Summarizer" aims to fill.

## 2. Methodology

### 2.1 Intention

The project aims to develop a comprehensive system that automates the summarization of news articles using advanced Natural Language Processing (NLP) techniques and provides personalized news recommendations through sophisticated AI algorithms. The goal is to create a seamless and engaging platform for users to access condensed and relevant news content.

### 2.2 Approach

The development process involves aggregating news content from various sources, summarizing these articles through AI-driven NLP models, and implementing an AI-based recommendation system that adapts to user preferences over time. The project adopts an agile development methodology, emphasizing continuous testing, feedback integration, and iterative enhancements to ensure the platform meets user needs effectively.

### 2.3 Design

The platform's architecture integrates state-of-the-art NLP models for accurate summarization and leverages AI algorithms for dynamic content recommendation. It features a responsive user interface that prioritizes usability and personalization, ensuring a tailored and distraction-free reading experience for every user.

## 3. Results

> 

### 3.1 Outcome

The "Mirror News Summarizer" successfully demonstrates the practical application of AI in enhancing the news reading experience. It efficiently generates concise summaries without losing the critical information of the original articles and offers personalized recommendations, significantly improving user engagement and satisfaction.

### 3.2 Execution

User testing and feedback highlighted the platform's effectiveness in delivering quick and accurate summaries alongside relevant recommendations. The project's approach to personalized news consumption was particularly appreciated, underscoring the system's potential to revolutionize how users interact with digital news.

## 4. Evaluation

> UAT

### 4.1 Testing

The project underwent rigorous testing phases, including functionality tests of the summarization and recommendation systems and user acceptance tests to evaluate usability and satisfaction levels.

#### 4.1.1 Approach

Functional tests assessed the precision of news summaries and the appropriateness of recommendations. Adjustments were made based on iterative feedback to refine the platform's performance.

#### 4.1.2 Results

Testing results confirmed that the "Mirror News Summarizer" effectively meets its objectives, facilitating efficient news consumption and delivering a highly satisfactory user experience.

#### 4.1.3 Assessment

The project underscores the transformative potential of AI in the news industry, offering a novel solution to the challenge of digital information overload. While the platform significantly advances news consumption practices, future enhancements focusing on model accuracy and content diversity are essential for further improvement.

> Furture work
> 现有功能(优缺点分析) + 未来功能

## 5. Conclusion

The "Mirror News Summarizer" represents a significant advancement in digital news consumption, employing AI to provide a solution that is both efficient and user-centric. By delivering succinct article summaries and personalized content, the platform not only addresses the issue of information overload but also enhances the overall quality of the news reading experience. Future developments will aim to refine AI models and expand content offerings, solidifying the platform's position as a leader in innovative news consumption solutions.

## References

- Inshorts. (n.d.). Retrieved from https://www.inshorts.com/en/read
- Feedly: Discover. (n.d.). Retrieved from https://feedly.com/i/discover
- Flipboard. (n.d.). Retrieved from https://flipboard.com/

## Appendices

A. Algorithm for Content Summarization

B. User Interface Designs

C. User Feedback Surveys

D. System Diagrams

---

### A. Algorithm for Content Summarization

The core of the "Mirror News Summarizer" is its AI-driven content summarization algorithm. It utilizes a combination of Natural Language Processing (NLP) models, primarily based on the Generative Pre-trained Transformer (GPT) architecture, to extract and condense the key points from lengthy news articles. The algorithm follows these steps:

1. **Content Extraction**: Initially, the system scrapes the body of the news article, excluding any advertisements or unrelated content.
2. **Preprocessing**: The extracted content is then preprocessed to remove any formatting and to standardize the text for analysis.
3. **Summarization**: Utilizing a fine-tuned GPT model, the system generates a concise summary of the article. This involves identifying the main ideas and summarizing them in a way that retains the original message and tone.
4. **Postprocessing**: The summary undergoes postprocessing to ensure it meets length constraints and readability standards.
5. **Delivery**: Finally, the summarized content is presented to the user through the platform's interface.

### B. User Interface Designs

The user interface (UI) of the "Mirror News Summarizer" is designed with simplicity and user engagement in mind. It features a clean layout that highlights summarized news content and personalized recommendations. Key design elements include:

- **Homepage**: Showcases top news summaries and personalized recommendation feeds.
- **Article View**: Offers a detailed view of the news summary, with options to read the full article on the original website.
- **Favorites Section**: Allows users to save and easily access their preferred articles and topics.
- **Customization Options**: Users can tailor their news feed by selecting topics of interest and adjusting the summary length.

### C. User Feedback Surveys

To evaluate user satisfaction and identify areas for improvement, the project conducted surveys among a diverse group of users. The surveys focused on aspects such as the accuracy of summaries, relevance of recommendations, usability of the platform, and overall satisfaction. Feedback collected from these surveys informed iterative enhancements to both the summarization algorithm and the UI design.

### D. System Diagrams

The system architecture of the "Mirror News Summarizer" is illustrated through several diagrams, detailing the interaction between its various components:

- **Content Aggregator Module**: Diagrams illustrate the process of fetching and preprocessing news articles from multiple sources.
- **AI Summarization and Recommendation Engines**: These diagrams depict the workflow of summarizing content and generating personalized recommendations.
- **User Interface Flow**: Visual representations of the UI display the user journey from accessing the platform to engaging with summarized content and personalized feeds.


1. Git
```
sudo yum update -y
sudo yum install git -y
```

2. Clone
```
git clone git@git.cs.bham.ac.uk:projects-2023-24/zxl183.git
```

3. Python3 & pip
```
sudo yum install python3 -y

curl -O https://bootstrap.pypa.io/get-pip.py
python3 get-pip.py

```

4. requirements
```
python3 -m pip install -r requirements.txt
```

5. npm
```
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
# Restart
nvm install node
# Check
node -v
npm -v
# Run
npm install
npm run-script build
npm start
```

6. DB
```
flask db init
flask db migrate
flask db upgrade
```

7. deploy
```
npm run build
flask run
# or nohup flask run --host=0.0.0.0 --port=5000 &
```


# Installation Guide

This document outlines the steps necessary to install and run the application on a server running Amazon Linux.

## Step 1: Update and Install Git

First, update your package lists to ensure you can download the latest versions of the software:

```bash
sudo yum update -y
sudo yum install git -y
```

## Step 2: Clone the Repository

Clone the repository from the Git server using the following command:

```bash
git clone git@git.cs.bham.ac.uk:projects-2023-24/zxl183.git
```

Navigate to the cloned directory:

```bash
cd zxl183
```

## Step 3: Install Python3 and pip

Install Python3 and pip using the provided scripts:

```bash
sudo yum install python3 -y

curl -O https://bootstrap.pypa.io/get-pip.py
python3 get-pip.py
```

## Step 4: Install Python Requirements

Install the required Python packages by running:

```bash
python3 -m pip install -r requirements.txt
```

## Step 5: Install Node and npm

Install Node Version Manager (nvm), Node.js, and npm:

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
# You may need to restart your terminal session at this point.
nvm install node
# To check the installation, run:
node -v
npm -v
```

Then, install the npm dependencies and build the project assets:

```bash
npm install
npm run-script build
```

## Step 6: Set Up the Database

Set up the database with Flask-Migrate by running:

```bash
flask db init
flask db migrate
flask db upgrade
```

## Step 7: Deploy the Application

To deploy the application, you can build the static assets with npm and start the Flask server:

```bash
npm run build
flask run
# Optionally, to keep the server running in the background:
nohup flask run --host=0.0.0.0 --port=5000 &
```

If using `nohup`, you can press `Ctrl + C` after the server starts, and it will continue to run in the background.
