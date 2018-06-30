library("VWPre")
setwd("D:/Dropbox/0001_University/RStudio/Gal_Josh_Matan")

# Load data

plot_avg(data = dat5, type = "proportion", xlim = c(0, 2000), 
         IAColumns = c(IA_1_P = "Target", IA_2_P = "Filler_1", IA_3_P = "Distractor", 
                       IA_4_P = "Filler_2"),
         Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA,
         ErrorBar = TRUE, VWPreTheme = TRUE) 

# plot critical correct trials only

plot_avg(data = keepcritcorrect(dat5), type = "proportion", xlim = c(0, 2000), 
         IAColumns = c(IA_1_P = "Target", IA_2_P = "Filler_1", IA_3_P = "Distractor", 
                       IA_4_P = "Filler_2"),
         Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA,
         ErrorBar = TRUE, VWPreTheme = TRUE) 

# split by competition type
plot_avg(data = keepcritcorrect(dat5), type = "proportion", xlim = c(0, 2000), 
         IAColumns = c(IA_1_P = "Target", IA_2_P = "Filler_1", IA_3_P = "Distractor", 
                       IA_4_P = "Filler_2"),
         Condition1 = "condition", Condition2 = NULL, Cond1Labels = c(c="Cohort (Onset)",r="Rhyme (Offset)"), Cond2Labels = NA,
         ErrorBar = TRUE, VWPreTheme = TRUE) 


# split by competition type vs. load
plot_avg(data = keepcritcorrect(dat5), type = "proportion", xlim = c(0, 2000), 
         IAColumns = c(IA_1_P = "Target", IA_2_P = "Filler_1", IA_3_P = "Distractor", 
                       IA_4_P = "Filler_2"),
         Condition1 = "condition",
         Condition2 = "load",
         Cond1Labels = c(c="Cohort (Onset)",r="Rhyme (Offset)"),
         Cond2Labels = c("1"="Low load", "4"="High load"),
         ErrorBar = TRUE, VWPreTheme = TRUE) 

plot_avg(data = keepcritcorrect(dat5a), type = "proportion", xlim = c(0, 3000), 
         IAColumns = c(IA_1_P = "Target", IA_2_P = "Filler_1", IA_3_P = "Distractor", 
                       IA_4_P = "Filler_2"),
         Condition1 = "condition",
         Condition2 = "load",
         Cond1Labels = c(c="Cohort (Onset)",r="Rhyme (Offset)"),
         Cond2Labels = c("1"="Low load", "4"="High load"),
         ErrorBar = TRUE, VWPreTheme = TRUE) 

plot_avg(data = keepcritcorrect(dat5), type = "proportion", xlim = c(0, 2000), 
         IAColumns = c(IA_1_P = "Target", IA_2_P = "Filler_1", IA_3_P = "Distractor", 
                       IA_4_P = "Filler_2"),
         Condition1 = "condition",
         Condition2 = "loadxwmcap",
         Cond1Labels = c(c="Cohort (Onset)",r="Rhyme (Offset)"),
         Cond2Labels = c("1 1"="Low load/Low cap","1 2"="Low load/High cap","4 1"="High load/Low cap","4 2"="High load/High cap"),
         ErrorBar = TRUE, VWPreTheme = TRUE) 

# range from britt study
plot_avg(data = keepcritcorrect(dat5), type = "proportion", xlim = c(200, 1500), 
         IAColumns = c(IA_1_P = "Target", IA_2_P = "Filler_1", IA_3_P = "Distractor", 
                       IA_4_P = "Filler_2"),
         Condition1 = "condition",
         Condition2 = "loadxwmcap",
         Cond1Labels = c(c="Cohort (Onset)",r="Rhyme (Offset)"),
         Cond2Labels = c("1 1"="Low load/Low cap","1 2"="Low load/High cap","4 1"="High load/Low cap","4 2"="High load/High cap"),
         ErrorBar = TRUE, VWPreTheme = TRUE) 