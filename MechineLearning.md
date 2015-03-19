#MechineLearning

After downloadding the data and finding out that there are 19000+ rows and 160 variables.
The data are quite detailed and obviously we dont need to use all of the variables, which lead to our few round of feature selection
  
  1.To predict the "manner in which they did the exercise", I throw away varabless like "X":index,"user_name","raw_timestamp_part_1","raw_timestamp_part_2", "cvtd_timestamp" base on assumtions:
      
      1. The experiment is conducted over a short period of time and there should be minimum change in the participants' phycical ability, thus index is not a factor
      
      2. Name is really inrelavent to the study as we only care how the exercises are performed not who.
      
      3. Similar to names, the time_stamp variable is inrelavent too.
      
variables left: 153

  2.Remove NA columns
      1. some colums contains mostly NA values and need to be removed. The column is removed if it contains more than 90% of NAs
      
variables left: 86

  3.Remove zero covirate
      1. use function nearZeroVar to remove near zero covirate columns.
         removing the columns with almost zero varianve in data.
         
variables left: 52

  4.PCA for future dimention reductions
      1. Threshole applied: 95%, result shown below.
        
        Call:
        preProcess.default(x = dt.4.v, method = "pca")
        Created from 19622 samples and 52 variables
        Pre-processing: principal component signal extraction, scaled, centered 
        PCA needed 25 components to capture 95 percent of the variance
         
Variables left: 25


