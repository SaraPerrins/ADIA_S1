# ADIA_S13[Regression.tree.analysis.implementation.pdf](https://github.com/ICF-Analytics/ADIA_S13/files/9456975/Regression.tree.analysis.implementation.pdf)

The folder SAS Data Cleaning contains the files we used to 1) reshape data and clean by age of respondent for exposure and outcome variables and 2) create final analytic variables. The raw imput files can also be found in a subfolder there.

The final file we used to run analysis in R can be found here:
 In the analyses folder you will find subfolders for each outcome. 

 The programs should be run in the numbered order. 1.rtree_outcomename will run the regression tree search, 2.inference_outcomename will run the inference testing to confirm results, and 3.additional tests_outcomename will run the group contrasts. 

The file that generated the training (rtree) and testing groups (inference groups) and did minor data cleaning can be found here ADIA_S13/analyses/preshlth/0.preprocess_ALL. However, if you run this program you will overwrite the finalvar file we used. Each time this program it runs it generates new testing and training samples that may not match the one we used for our final analysis. 

Hi Sara
