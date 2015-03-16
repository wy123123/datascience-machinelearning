#MechineLearning

Downloaded the data and find out that there are 19000+ rows and 160 variables.
The data are quite detailed and obviously we dont need to use all of the variables. This leads to our first round of feature selection.
  1. To predict the "manner in which they did the exercise", I throw away varabless like "X","user_name","raw_timestamp_part_1","raw_timestamp_part_2", "cvtd_timestamp" base  on the assumptions
      1. The experiment is conducted over a short period of time and there should be minimum change in the participants' phycical ability.
      2. Name is really inrelavent to the study as we only care how the exercises are performed not who.
      3. Similar to names, the time_stamp variable is inrelavent too.
      variables left: 153
  2. Remove NA columns
      some colums contains mostly NA values and need to be removed
      variables left: 86
  3. Remove zero covirate
      use function nearZeroVar to remove near zero covirate columns.
      removing the columns with almost zero varianve in data.
      variables left: 52


