# ADS Project 5: 

Term: Spring 2017

+ Team #3
+ Projec title: Fragile Families Challenge Prediction
+ Team members
	+ Yue Gao
	+ Mengchen Li
	+ Jingwen Yin
	+ Chenyun Zhu
	+ Hongyi Zhu
+ Project summary: In this project, we built various predictive models on the Fragile Families Challenge data, to predict the following: GPA, Grit, Material hardship, Eviction, Layoff, and Job training.

+ Data: Fragile Families Challenge. It is a scientific mass collaboration combining predictive modeling,causal inference and qualitative interviews. Our project goal is to improve the lives of disadvantaged children in the US.

+ Data Cleaning:
![alt tag](https://github.com/TZstatsADS/Spr2017-proj5-grp3/blob/master/figs/140.pic.jpg)
![alt tag](https://github.com/TZstatsADS/Spr2017-proj5-grp3/blob/master/figs/160.pic.jpg)
+Assumption: Age 9 data has already included all the information from age 0-5
+Continuous outcomes: GPA, Grit, Material hardship
+Binary outcomes: Housing eviction, Layoff of a caregiver, Job training for a caregiver 
+Missin Data: -9 Not in wave, -6 Valid skip, -2 Dont know, -1 Refuse, NA also used occasionally 
+Categorical feature: Make NA a special level
+Continuous feature: Create a dummy variable indicating the missing situation of the feature,Impute the missing value with median (library: Hmisc)

+ Feature Selection:Boruta package
![alt tag](https://github.com/TZstatsADS/Spr2017-proj5-grp3/blob/master/figs/feature%20selection.png)

+ Firstly, it adds randomness to the given data set by creating shuffled copies of all features.
+ Then, it trains a random forest classifier on the extended data set and applies a feature importance measure to evaluate the importance of each feature where higher means more important.
+ At every iteration, it checks whether a real feature has a higher importance than the best of its shadow features and constantly removes features which are deemed highly unimportant.
+ Finally, the algorithm stops either when all features gets confirmed or rejected or it reaches a specified limit of random forest runs.

**Prediction Model**:
Our team attempts to use 14 models to predict results.
![alt tag](https://github.com/TZstatsADS/Spr2017-proj5-grp3/blob/master/figs/Model%20Summary.jpeg)

+ Final Prediction Model:









	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
