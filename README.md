# Imbalanced learning for SDP

This is the data to share for software defect prediction including:

## 27 software defect data sets
These data sets comes from PROMISE repository and AEEEM defect data.
Collection of some software metrics enriches input attributes for each data set.
There are three kinds of input attributes (software metrics):

**i-** Code metrics (CK)

**ii-** Process metrics (PROC)

**iii-** Network metrics (NET)

Seven combinations from these three kinds of metrics are the seven types of data sets we studies.

## Imbalanced learning java program 

16 imbalanced learning methods were applied in our experment.
The jar file is a typical java program.

To run it in command line (with java environment):
java -jar ImbSDP_v2.jar 

The program first read "data_sets.txt" to find the list of data sets.
Then it loads data sets in Folder "input" and run the experiment of all learning methods.
Last the results are written in Folder "output".
Please note that it may run for hours which depend on the size of data sets.

## Results and Analysis Scripts

"rawdata.r" merges all raw data to one file.

Other R code can be found in "scripts_for_paper.r"

File "Rallfun-v33.txt" contains statistical tools for R code.

csv files are all data for analysis.

## Contact Infomation

If you have any question, please email: wispcat@stu.xjtu.edu.cn
