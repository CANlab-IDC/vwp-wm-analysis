library("VWPre")
my_wd <- "~/dev/CANlab-IDC/gal-matan-analysis/"
setwd(my_wd)

VWdat <- read.table("Sample_report_21_6_18_critl_period-002.txt", header = T, sep = "\t", na.strings = c(".", "NA"))


VWdat$LEFT_INTEREST_AREA_ID <- as.integer(as.character(VWdat$LEFT_INTEREST_AREA_ID))
VWdat$RIGHT_INTEREST_AREA_ID <- as.integer(as.character(VWdat$RIGHT_INTEREST_AREA_ID))
VWdat$AVERAGE_INTEREST_AREA_ID <- as.integer(as.character(VWdat$AVERAGE_INTEREST_AREA_ID))


dat0 <- prep_data(data = VWdat, Subject = "RECORDING_SESSION_LABEL", Item = "conditionxnoise")

dat0 <- rm_extra_DVcols(dat0)

check_ia(data = dat0)


newdat <- recode_ia(data=dat0, IDs=c("0"="0", "1"="1", "2"="2", "3"="3",
                                     "4"="4", "5"="0", "6"="0", "7"="0","8"="0", "9"="0"), 
                    Labels=c(Bottom="Outside",Center="Outside",Distractor="Distractor", Filler_1="Filler_1",Filler_2="Filler_2",Left="Outside",       
                             Right="Outside",Target="Target",Top="Outside"))

dat1 <- relabel_na(data = newdat, NoIA = 5)

dat1.pad <- dat1 %>%
  group_by(., Subject, TRIAL_INDEX) %>%
  pad(by=TIMESTAMP) %>%
  fill_with_function(last)

dat2 <- create_time_series(data = dat1, Adjust = 200)

check_time_series(data = dat2)

check_msg_time(data = dat2, Msg = "WITHOUT_HA")

check_eye_recording(data = dat2)

dat3 <- select_recorded_eye(data = dat2, Recording = "LandR", WhenLandR = "Right")

check_samplingrate(dat3)

ds_options(SamplingRate = 500)

dat4 <- bin_prop(dat3, NoIA = 5, BinSize = 20, SamplingRate = 500)

check_samplingrate(dat4)

check_samples_per_bin(dat4)


wmcaps <- read.csv("rawdata/WM_span.csv", header=T, col.names=c("Subject","wmcap"), colClasses = c("factor","factor"))
wmcaps$Subject <- as.factor(sub("111","111t",as.character(wmcaps$Subject)))
dat4 <- merge(x=dat4, y=wmcaps, by="Subject", all=T)

dat4 <- rename_columns(dat5, Labels = c(IA1="Target", IA2="Filler_1", IA3="Distractor", IA4="Filler_2")) 

dat4$loadxwmcap <- as.factor(paste(dat4$load, dat4$wmcap))

dat5 <- transform_to_elogit(dat4, NoIA = 5, ObsPerBin = 10)

dat5a <- create_binomial(data = dat4, NoIA = 5, ObsPerBin = 10)

dat5$load <- as.factor(dat5$load)
dat5a$load <- as.factor(dat5a$load)

colnames(dat5) 
colnames(dat5a)

dat6 <- rename_columns(dat5, Labels = c(IA1="Target", IA2="Filler_1", IA3="Distractor", IA4="Filler_2")) 

colnames(dat6)

dat4_correct <- keepcritcorrect(dat4, IncludeFiller = TRUE)
#dat4_critcorrect <- keepcritcorrect(dat4, IncludeFiller = FALSE)

TargetFixPForMcMurrayDat <- dat4_correct %>% 
  # Select just the columns you want
  select(., Subject, Item, starts_with("IA"), Event, TRIAL_INDEX, 
         condition, target, distractor, filler_1, filler_2, load, critical, Time, IA_1_P) %>%
  # Order the data by Subject, Trial, and Time
  arrange(., Subject, TRIAL_INDEX, Time);

CompetitorFixPForMcMurrayDat <- dat4_correct %>% 
  # Select just the columns you want
  select(., Subject, Item, starts_with("IA"), Event, TRIAL_INDEX, 
         condition, target, distractor, filler_1, filler_2, load, critical, Time, IA_3_P) %>%
  # Order the data by Subject, Trial, and Time
  arrange(., Subject, TRIAL_INDEX, Time);
save(FinalDat, file = "D:/tempfolder/FinalDat.rda", compress = "xz")
write.csv(TargetFixPForMcMurrayDat, file="./mcmurray-data/target-correct-data.csv")
write.csv(CompetitorFixPForMcMurrayDat, file="./mcmurray-data/competitor-correct-data.csv")

