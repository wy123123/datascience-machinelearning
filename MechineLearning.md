#MechineLearning

Downloaded the data and find out that there are 19000+ rows and 160 variables.
The data are quite detailed and obviously we dont need to use all of the variables. This leads to our first round of feature selection.
  1. To predict the "manner in which they did the exercise", I throw away varabless like "X","user_name","raw_timestamp_part_1","raw_timestamp_part_2", "cvtd_timestamp" base  on the assumptions
      1. The experiment is conducted over a short period of time and there should be minimum change in the participants' phycical ability.
      2. Name is really inrelavent to the study as we only care how the exercises are performed not who.
      3. Similar to names, the time_stamp variable is inrelavent too.
  2. After reading the paper "Qualitative Activity Recognition of Weight Lifting Exercises",
    I found that they use Variables new_window and num_window indicate 
