# ADIA_S13[Regression.tree.analysis.implementation.pdf](https://github.com/ICF-Analytics/ADIA_S13/files/9456975/Regression.tree.analysis.implementation.pdf)

The folder SAS Data Cleaning contains the files we used to 1) reshape data and clean by age of respondent for exposure and outcome variables and 2) create final analytic variables. The raw imput files can also be found in a subfolder there. The sas files generated 'data/finalvar_01.30.2023.csv'

We then ran analyses/preshlth/0.preprocess_ALL to generate the final file for regression tree analysis 'data/finalvar.rds' -- please note: to replicate analysis you must use our current finalvar.rds file. Each time 0.preprocess_ALL is run it randomly generates new training (regresstion tree) and testing (inference) samples. To independently test the preprocess code rename the final output datafile and proceed to run outcome-specific code as written. 

In the analyses folder you will find subfolders for each outcome. The programs should be run in the numbered order. 1.rtree_outcomename will run the regression tree search, 2.inference_outcomename will run the inference testing to confirm results, and 3.additional tests_outcomename will run the group contrasts. 
