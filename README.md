# Learning-Speech-Melody
Influence of working memory, learning ability, and sound distortion upon speech recognition

Gabriele Fresia (s3121283)
Giorgio Spadaccini (s2793075)
MSc Statistics and Data Science, Leiden University
December 2022

(Brief) Description:
This environment analyzes the data collected during a speech pattern recognition experiment: a speech pattern is deviated by 1,3 or 5 Semitones either at the beginning or end of the pattern. The pattern can either belong to one of three archetypical patterns (flat, falling, M-shaped) or be a new, unheard pattern.
A participant, who has a score for working memory and learning ability, is presented with many distorted pattern and then tries to identify them as one of the archetypes or a new, different pattern ("other").
Two models logistic mixed-effects models were built to addres the questions:
• To what extent do working memory and learning ability affect one’s ability to recognize the correct speech pattern despite being distorted?
• What kind of distortions make the speech intonations harder to be recognized?
For details about the research experiment that this code is based on, please read this report.

The dataset:
The true data recorded from the experiment is unfortunately not open to public, therefore the datasets are not avaialable.

How to run this code:
First run the file data_preprocessing.R. This will produce a dataset called "preprocessed_data.csv"; 
To answer the first research question, run the code inference_firstRQ.R
To answer the second research question, run the code inference_firstRQ.R
