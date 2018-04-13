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

15 imbalanced learning methods were applied in our experment.
The jar file is a typical java program.
To run it in command line (with java environment):
java -jar reImbPUB0.jar 

The program first read "data_sets.txt" to find the list of data sets.
Then it loads data sets in Folder "input" and run the experiment of all learning methods.
Last the results are written in Folder "output".

## results and analysis scripts

R code

If you have any question, please email: heanyg@gmail.com
