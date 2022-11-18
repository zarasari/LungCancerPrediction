library(MASS)
library(Hmisc) #To run "Describe" Command
library(modeest) #To run "mlv" Command
res<-read.csv("Resol.csv") #Main sheet
attach(res)
health<-read.csv("HealthSheet.csv") #Health Sheet
attach(health)
resfh<-read.csv("Mainnhealth.csv") #Main sheet (Containing family history)
attach(resfh)
options(digits=4) #Number of digits to print on output
# Mean Age patients excluding missing values
a<-mean(AgeRegistered[!is.na(AgeRegistered)]);a
ncol(res) # Number of columns ReSoLuCENT main data sheet
nrow(res) # Number of rows of Health data sheet
ncol(health)
nrow(health)
# Mean Age Cases
b<-mean(AgeRegistered[CaseControl_c=="Case" ]);b
# Standard Error Age Cases
bvar<-var(AgeRegistered[CaseControl_c=="Case" ]);sqrt(bvar)
# Mean Age Controls
c<-mean(na.omit(AgeRegistered)[CaseControl_c=="Control" ]);c
# Standard Error Age Controls
cvar<-var(na.omit(AgeRegistered)[CaseControl_c=="Control" ]);sqrt(cvar)
hist(AgeRegistered[!is.na(AgeRegistered)],prob=TRUE,xlab="Age Registered",main="")
lines(density(AgeRegistered[!is.na(AgeRegistered)]))
describe(res)
# Barplot Case/Control freq.
plot(CaseControl_c,xlab="Subject",ylab="Frequency")
# Barplots Case - Control Gender Frequency
par(mfrow=c(1,2))
barplot(table(Sex,CaseControl_c),xlab="Subject",ylab="Frequency",
        legend.text=c("Male","Female"), ylim= c(0,550))
barplot(table(Sex,CaseControl_c),xlab="Subject",ylab="Frequency",
        63 64
        Figure 25: LLP multivariate risk model, with unadjusted and adjusted odds ratios and 95% confidence intervals corresponding to the model coefficients
        legend.text=c("Male","Female"),beside=TRUE, ylim= c(0,300))
# Number of Male and Female Cases #1(male) and #2(Female)
sum(res$CaseControl_c=="Case" & res$Sex=="1", na.rm = TRUE)
sum(res$CaseControl_c=="Case" & res$Sex=="2", na.rm = TRUE)
# Number of Male and Female Controls
sum(res$CaseControl_c=="Control" & res$Sex=="1", na.rm = TRUE)
sum(res$CaseControl_c=="Control" & res$Sex=="2", na.rm = TRUE)
# Controls Alive
sum((Status =="Alive")[CaseControl_c=="Control" ])
# Cases Alive
sum((Status =="Alive")[CaseControl_c=="Case" ])
# Controls Dead
879-sum((Status =="Alive")[CaseControl_c=="Control" ])
# Controls Dead
879-sum((Status =="Alive")[CaseControl_c=="Case" ])
summary(S28)
#Cases Current-smoker
sum((S28 =="Yes")[CaseControl_c=="Case" ]) ;150/516
# Cases Ex-smoker
sum((S28 =="Ex")[CaseControl_c=="Case" ]) ;245/516
#Controls Current-Smoker
sum((S28 =="Yes")[CaseControl_c=="Control" ]);99/363
# Controls Ex-smoker
sum((S28 =="Ex")[CaseControl_c=="Control" ]); 100/363
#Dusty Conditions
describe(S19)
#Cases
sum((S19 =="Yes")[CaseControl_c=="Case" ])
sum((S19 =="No")[CaseControl_c=="Case" ])
#Controls
sum((S19 =="Yes")[CaseControl_c=="Control" ])
sum((S19 =="No")[CaseControl_c=="Control" ])
barplot(table(S19,CaseControl_c),xlab="Subject",ylab="Frequency",
        legend.text=c("Unknown","No","Yes"), ylim= c(0,550))
#Quit Duration
quit<-AgeRegistered -S31;quit   # Age registered - Age quit = years quit
quit<-quit[!is.na(quit)] # Exclude NAs ie. Current and Non-smokers
quit[(quit<0)]<- (-quit[(quit<0)])
mean(quit)
quit[336];S31[336];AgeRegistered[336]  #-0.4,52,51.6  Quit after registering
quit[587];S31[587];AgeRegistered[587]  #-0.1,57,56.9
quit[432];S31[432];AgeRegistered[432]  #-0.3,56,55.7
quit[1];S31[1];AgeRegistered[1]
#1.4,44,45.4  Ex-smoker who quit before registering
quit[2];S31[2];AgeRegistered[2]  #NA
#Current smokers and Non-smokers
# Quitting duration for cases
casequit<-quit[CaseControl_c=="Case" ]
casequit <-casequit[!is.na(casequit)]
# Exclude NAs ie. Current and Non-smokers
mean(casequit)
# Quitting duration for controls
controlquit<-quit[CaseControl_c=="Control" ]
controlquit <-controlquit[!is.na(controlquit)]
# Exclude NAs ie. Current and Non-smokers
mean(controlquit)
par(mfrow=c(1,2))
hist(casequit,xlab="Case Quitting Duration",main="")
#Smoking Quitting frequency histogram
hist(controlquit,xlab="Control Quitting Duration",main="")
#Smoking Quitting frequenc histogram
#Smoking Duration
for(i in 1:length(S31))  #(Assigning 0 to NA values)
{if (is.na(S30[i])) {S30[i]=0}
  {if (is.na(S31[i])) {S31[i]=0}}}
dur<-S31-S30
#Smoking Duration = Age Stopped smoking regularly - Age started
for(i in 1:length(dur))
{if (dur[i]<0) {dur[i]= AgeRegistered[i]-S30[i]}}
#Current Smokers smoking duration
65
66

#Assuming negative value (subject 431 with missing age) is 0 (ie non-smoker)
dur[is.na(dur)]<-0
dur<-dur[dur>0] #Exclude 0 values ie. Non-smokers
# To calculate overall mean and variance
mean(dur) #32.34
durcase<-dur[CaseControl_c=="Case" ];durcase # Smoking Duration for Cases
durcase<-durcase[!is.na(durcase)]
length(durcase)
mean(durcase)
mlv(durcase, method = "mfv")
durcont<-dur[CaseControl_c=="Control" ];durcont # Smoking Duration for Controls
durcont <-durcont[!is.na(durcont)]
length(durcont)
mean(durcont)
mlv(durcont, method = "mfv")
par(mfrow=c(1,2))
hist(durcase,xlab="Case Smoking Duration",main="")
#Smoking durion frequency histogram
hist(durcont,xlab="Control Smoking Duration",main="")
#Smoking durion frequenc histogram
# Cases, Ex-Smokers
Exdurcase<-durcase[durcase>0];Exdurcase
mean(Exdurcase)
var<-var(Exdurcase);sqrt(var)
# Controls, Ex-Smokers
Exdurcont<-durcont[durcont>0];Exdurcont
mean(Exdurcont)
var<-var(Exdurcont);sqrt(var)
#Run "dur<-S31-S30" , "durcase" and "durcont" again
# Current Case Smokers
Curdurcase<-rep(NA,length(durcase))
for(i in 1:length(durcase))
{if (durcase[i]<0) {Curdurcase[i]=AgeRegistered[i]-S30[i]}}
mean(Curdurcase[!is.na(Curdurcase)])
var<-var(Curdurcase[!is.na(Curdurcase)]) ;sqrt(var)
# Current Control Smokers
Curdurcont<-rep(NA,length(durcont))
for(i in 1:length(durcont))
{if (durcont[i]<0) {Curdurcont[i]=AgeRegistered[i]-S30[i]}}
mean(Curdurcont[!is.na(Curdurcont)])
var<-var(Curdurcont[!is.na(Curdurcont)]) ;sqrt(var)
# Mean and Standard Deviation Smoking per day at Age 20 #Cases
exsmokers<-S28[S28=="Ex"] # Ex-Smokers
currentsmokers<-S28[S28=="Yes"] # Current Smokers
SS31_20<-S31_20[S31_20>0]
case20<-SS31_20[CaseControl_c=="Case" ];case20
case20<-case20[!is.na(case20)]
case20ex<-case20[S28=="Ex"]  #Ex-Smokers
mean(case20ex[!is.na(case20ex)])
var<-var(case20ex[!is.na(case20ex)]);sqrt(var)
case20cur<-case20[S28=="Yes"]  # Current Smokers
mean(case20cur[!is.na(case20cur)])
var<-var(case20cur[!is.na(case20cur)]);sqrt(var)
#Controls
control20<-SS31_20[CaseControl_c=="Control" ];control20
control20ex<-control20[S28=="Ex"]  #Non-Smokers
mean(control20ex[!is.na(control20ex)])
var<-var(control20ex[!is.na(control20ex)]);sqrt(var)
# Current Smokers
control20cur<-control20[S28=="Yes"]  # Current Smokers
mean(control20cur[!is.na(control20cur)])
var<-var(control20cur[!is.na(control20cur)]);sqrt(var)
# Mean and Standard Deviation Smoking per day at Age 30
#Cases
SS31_30<-S31_30[S31_30>0]
case30<-SS31_30[CaseControl_c=="Case" ];case30
case30ex<-case30[S28=="Ex"]  #Ex-Smokers
mean(case30ex[!is.na(case30ex)])
var<-var(case30ex[!is.na(case30ex)]);sqrt(var)
case30cur<-case30[S28=="Yes"]  # Current Smokers
67
68

mean(case30cur[!is.na(case30cur)])
var<-var(case30cur[!is.na(case30cur)]);sqrt(var)
#Controls
control30<-SS31_30[CaseControl_c=="Control" ];control30
control30ex<-control30[S28=="Ex"]  #Ex-Smokers
mean(control30ex[!is.na(control30ex)])
var<-var(control30ex[!is.na(control30ex)]);sqrt(var)
control30cur<-control30[S28=="Yes"]  # Current Smokers
mean(control30cur[!is.na(control30cur)])
var<-var(control30cur[!is.na(control30cur)]);sqrt(var)
# Mean and Standard Deviation Smoking per day at Age 40
#Cases
SS31_40<-S31_40[S31_40>0]
case40<-SS31_40[CaseControl_c=="Case" ];case40
case40ex<-case40[S28=="Ex"]  #Ex-Smokers
mean(case40ex[!is.na(case40ex)])
var<-var(case40ex[!is.na(case40ex)]);sqrt(var)
case40cur<-case40[S28=="Yes"]  # Current Smokers
mean(case40cur[!is.na(case40cur)])
var<-var(case40cur[!is.na(case40cur)]);sqrt(var)
#Controls
control40<-SS31_40[CaseControl_c=="Control" ];control40
control40ex<-control40[S28=="Ex"]  #Ex-Smokers
mean(control40ex[!is.na(control40ex)])
var<-var(control40ex[!is.na(control40ex)]);sqrt(var)
control40cur<-control40[S28=="Yes"]  # Current Smokers
mean(control40cur[!is.na(control40cur)])
var<-var(control40cur[!is.na(control40cur)]);sqrt(var)
# Mean and Standard Deviation Smoking per day at Age 50
#Cases
SS31_50<-S31_50[S31_50>0]
case50<-SS31_50[CaseControl_c=="Case" ];case50
case50ex<-case50[S28=="Ex"]  #Ex-Smokers
mean(case50ex[!is.na(case50ex)])
var<-var(case50ex[!is.na(case50ex)]);sqrt(var)
case50cur<-case50[S28=="Yes"]  # Current Smokers
mean(case50cur[!is.na(case50cur)])
var<-var(case50cur[!is.na(case50cur)]);sqrt(var)
# Controls
control50<-SS31_50[CaseControl_c=="Control" ];control50
control50ex<-control50[S28=="Ex"]  #Ex-Smokers
mean(control50ex[!is.na(control50ex)])
var<-var(control50ex[!is.na(control50ex)]);sqrt(var)
control50cur<-control50[S28=="Yes"]  # Current Smokers
mean(control50cur[!is.na(control50cur)])
var<-var(control50cur[!is.na(control50cur)]);sqrt(var)
summary(S28)
#Treat blank cells as Unknown variable
S28[(S28!="Ex") & (S28!="No") & (S28!="Yes") ] <- NA
S28<-factor(S28,labels=c("Ex","No","Yes"))
# Regular smoker (10 cigarettes per week for a year)
barplot(table(CaseControl_c, S28),beside=TRUE
        ,xlab="Regular Smoker Subject",ylab="Frequency",legend.text=TRUE)
S29[S29==0]<-NA
# Age at first full cigarette
barplot(table(CaseControl_c, S29),xlab="Age",ylab="Frequency"
        ,beside=TRUE,legend.text=TRUE)
mean(S29[!is.na(S29)])
S30[S30==0]<-NA
# Starting age regular smoking
barplot(table(CaseControl_c, S30),xlab="Age",ylab="Frequency"
        ,beside=TRUE,legend.text=TRUE)
mean(S30[!is.na(S30)])
#Treat blank cells as Unknown variable
S37[(S37!="Yes") & (S37!="No") ] <- NA
S37<-factor(S37,labels=c("No","Yes"))
# Passive smoking
barplot(table( CaseControl_c,S37),
        xlab="Passive Smoker Subjects",ylab="Frequency",
        beside=TRUE,legend.text=TRUE)
69
70

#Treat blank cells as Unknown variable
S20[(S20!="No") & (S20!="Yes") ] <- NA
S20<-factor(S20,labels=c("No","Yes"))
# Asbestos Exposure
barplot(table(CaseControl_c,S20),
        xlab="Asbestos Exposed Subjects",ylab="Frequency"
        ,beside=TRUE,legend.text=TRUE)
else
{if (PeriodAlive[i]>90) {PeriodAlive[i]=NA} #Excludes very large values
  else PeriodAlive[i]=PeriodAlive[i]}} PeriodAlive<-PeriodAlive[!is.na(PeriodAlive)] #Excludes N/A s length(PeriodAlive) #209 alive after diagnosis
  mean(PeriodAlive) # 12.85 months Calculate average months alive after diagnosis # Status
  barplot(table(CaseControl_c,Status[!is.na(Status)]),legend.text=TRUE)
  # Describe own health barplot(table(CaseControl_c,S05[!is.na(S05)]),beside=TRUE,legend.text=TRUE)
  # Occupation Pie Chart
  lbls <- paste(names(table(S16)), "\n", table(S16), sep="")
  pie(table(S16), labels = lbls,main="Occupation Pie Chart\n (with sample sizes)") #Assign NA to missing values
  Sitelung[(Sitelung!="Left lung") & (Sitelung!="Right lung") ] <- NA
  Sitelung <-factor(Sitelung,labels=c("Left lung","Right lung"))
  #Lung Site
  barplot(table(Sitelung),ylim=c(0,280),xlab="Lung Site",ylab="Frequency")
  # Sum of subjects living with partener/spouse
  for(i in 1:length(S04_1[CaseControl_c=="Case" ]))
  {if (is.na(S04_1[i])) {S04_1[i]=NA}
    else
    {if (S04_1[i]>=1) {S04_1[i]=1}
      else S04_1[i]=0}}
  sum(S04_1[!is.na(S04_1)])
  # Sum of subjects living with children/stepchildren
  for(i in 1:length(S04_2))
  {if (is.na(S04_2[i])) {S04_2[i]=NA}
    else
    {if (S04_2[i]>=1) {S04_2[i]=1}
      else S04_2[i]=0}}
  sum(S04_2[!is.na(S04_2)])
  # Sum of subjects living with Siblings
  for(i in 1:length(S04_4))
  {if (is.na(S04_4[i])) {S04_4[i]=NA}
    S31[S31==0]<-NA
    # If stopped smoking, Age last smoked?
    barplot(table(CaseControl_c,S31),xlab="Age",
            ylab="Frequency",beside=TRUE,legend.text=TRUE)
    # Mode smoking duraion age  # Overall
    library(modeest)
    mlv(S31[!is.na(S31)], method = "mfv")
    # Mean smoking duraion age  # Overall
    mean(S31[!is.na(S31)])
    # Controls
    controlce<-S31[CaseControl_c=="Control" ];controlce
    mean(controlce[!is.na(controlce)])
    # Cases
    casece<-S31[CaseControl_c=="Case" ];casece
    mean(casece[!is.na(casece)])
    # Cigarette type at ages 20,30,40 and 50
    par(mfrow=c(2,2))
    barplot(table(CaseControl_c, S31_20Type),xlab="Cigarette Type at Age
            beside=TRUE,legend.text=TRUE)
            barplot(table(CaseControl_c, S31_30Type),xlab="Cigarette Type at Age
            beside=TRUE,legend.text=TRUE)
    barplot(table(CaseControl_c, S31_40Type),xlab="Cigarette Type at Age
            beside=TRUE,legend.text=TRUE)
            barplot(table(CaseControl_c, S31_50Type),xlab="Cigarette Type at Age
            beside=TRUE,legend.text=TRUE)
    # Assign "N/A" to large negative and positive values of Period Alive
    for(i in 1:length(PeriodAlive))
    {if (PeriodAlive[i]<1) {PeriodAlive[i]=NA}  #Assigns N/A to negatives and
      71
      72
      20",
      30",
      40",
      50",
      Nils
      
      else
      { if (S04_4[i]>=1) {S04_4[i]=1}
        else S04_4[i]=0}}
    sum(S04_4[!is.na(S04_4)])
    # Health Spreadsheet Discriptive Satistics
    describe(health)
    describe(health$H1Alive) #Family Alive
    summary(health$H1Cause) # Cause of Death
    describe(health$H3) #Family Smoking Status
    summary(health$HCa) #Has had Cancer
    summary(health$H_2Ca) #More than 1 Cancer
    summary(health$HCaSite1) #Part of Body Affected
    summary(health$HCaSite2) #Cancer Site 2
    #Family Hisotry on Health Sheet (health data)
    FH
    fh<-H1Relationship[FH=="1"]
    onefh<-na.exclude(fh)
    describe(fh) #142
    describe(onefh)
    summary(fh)
    #(Mainnhealth) sheet
    describe(Family) #134
    caseFamily<-Family[CaseControl_c=="Case"]  #Cases
    describe(caseFamily)
    case1<-caseFamily[caseFamily=="1"];na.exclude(case1) #1 Family memebers
    describe(case1)
    case2<-caseFamily[caseFamily=="2"];na.exclude(case2) #2 Family memebers
    describe(case2)
    ControlFamily<-Family[CaseControl_c=="Control"]  #Controls
    describe(ControlFamily)
    control1<-ControlFamily[ControlFamily=="1"];na.exclude(control1)
    #1 Family member
    describe(control1)
    control2<-ControlFamily[ControlFamily=="2"];na.exclude(control2)
    #2 Family members
    describe(control2)
    caseover<-caseFamily[AgeRegistered>60] # Over 60 Family memeber Cases
    describe(caseover)
    controlover<-ControlFamily[AgeRegistered>60] # Over 60 Family memeber Control
    describe(controlover)
    casebelow<-caseFamily[AgeRegistered<60]
    # Below 60 Family memeber Cases
    describe(casebelow)
    controlbelow<-ControlFamily[AgeRegistered<60] # Below 60 Family memeber Control
    describe(controlbelow)
    ##### Pack Years#####
    # Assign "Nil" to unknown values
    for(i in 1:length(S31))
    {if (is.na(S30[i])) {S30[i]=0} #Non-Smokers
      {if (is.na(S31[i])) {S31[i]=0}           #Current Smokers
        {if (is.na(AgeRegistered[i])) {AgeRegistered[i]=0}   #Age joined the study
          73
          74
          {if (is.na(S31_20[i])) {S31_20[i]=0}
            {if (is.na(S31_30[i])) {S31_30[i]=0}
              {if (is.na(S31_40[i])) {S31_40[i]=0}
                {if (is.na(S31_50[i])) {S31_50[i]=0}
                }}}}}}}
    # Pack Years
    p<-rep(NA,length(S31)) #Create an array
    for(i in 1:length(S31))
    {if (S30[i]==0 && S31[i]==0) {p[i]=0}
      else
      {if ((S31[i]==0) && AgeRegistered[i]>=50)
        # Current smoker ,current age over 50
      {p[i]<-((AgeRegistered[i]-50)*S31_50[i]+10*S31_40[i]
              +10*S31_30[i]+10*S31_20[i]+((20-S30[i])*S31_20[i]))/20}
        else
        {if ((S31[i]==0) && AgeRegistered[i]>=40)
          # Current smoker ,current age over 40
        {p[i]<-((AgeRegistered[i]-40)*S31_40[i]+10*S31_30[i]
                +10*S31_20[i]+((20-S30[i])*S31_20[i]))/20}
          #Non smoker at age 20
          #Non smoker at age 30
          #Non smoker at age 40
          #Non smoker at age 50
          of NAs with length S31
          # Non-smokers
          
          else
          {if ((S31[i]==0) && AgeRegistered[i]>=30)    # Current smoker ,current age over 30
          {p[i]<-((AgeRegistered[i]-30)*S31_30[i]+10*S31_20[i]
                  +((20-S30[i])*S31_20[i]))/20}
            else
            {if ((S31[i]==0) && AgeRegistered[i]>=20)
            {p[i]<-((AgeRegistered[i]-20)*S31_20[i]+((20-S30[i])*S31_20[i]))/20} else
            {if ((S31[i]==0) && AgeRegistered[i]>=15)  # Current smoker ,current age over 15
            {p[i]<-((AgeRegistered[i]-S30[i])*S31_20[i])/20}
              else
              {if (S31[i]>=50)   # Stopped smoking after 50
              {p[i]<-(((S31[i]-50)*S31_50[i])+10*S31_40[i]+10*S31_30[i]
                      +10*S31_20[i]+((20-S30[i])*S31_20[i]))/20}
                else
                {if (S31[i]>=40)  #  Stopped smoking after 40
                {p[i]<-(((S31[i]-40)*S31_40[i])+10*S31_30[i]+10*S31_20[i]+((20-S30[i])*S31_20[i]))/20}
                  else
                  {if (S31[i]>=30)  #  Stopped smoking after 30
                  {p[i]<-(((S31[i]-30)*S31_30[i])+10*S31_20[i]+((20-S30[i])*S31_20[i]))/20}
                    else
                    {if (S31[i]>=20)  # Stopped smoking after 20
                    {p[i]<-((S31[i]-20)*S31_20[i]+((20-S30[i])*S31_20[i]))/20}
                      else
                      {if (S31[i]>=17)
                      {p[i]<-((S31[i]-S30[i])*S31_20[i])/20}  # Stopped smoking after 17
                      }}}}}}}}}}}
    # Include starting smoking ages in bands 20-30, 30-40
    and 40-50 for current and ex-smokers both
    for(i in 1:length(S31))
    {if (p[i]>0 && 30 >S30[i] && S30[i]>20) {p[i]<-p[i]+((30-S30[i])*S31_30[i])/20}
      else
      {if (p[i]>0 && 40 >S30[i] && S30[i]>30) {p[i]<-p[i]+((40-S30[i])*S31_40[i])/20}
        else
        {if (p[i]>0 && 50 >S30[i] && S30[i]>40) {p[i]<-p[i]+((50-S30[i])*S31_50[i])/20}}}}
    meanp<-mean(p[!is.na(p)]);meanp #mean p  21.18
    # Diagram
    #Pack Years Case Controls
    max(p[!is.na(p)])
    median(p[!is.na(p)])
    median(packcase)
    median(packcontrol)
    # Mean and Standard Deviation Pack Years
    #Cases
    packcase<-p[CaseControl_c=="Case" ];packcase
    packcase<-packcase[!is.na(packcase)]  #Exclude NA values
    mean(packcase)
    var<-var(packcase);sqrt(var)
    #Controls
    packcontrol<-p[CaseControl_c=="Control" ];packcontrol
    #Pack Years for relevant Controls
    packcontrol<-packcontrol[!is.na(packcontrol)] #Exclude NA values
    mean(packcontrol)
    var<-var(packcontrol);sqrt(var)
    # Pack Year Box-plot
    boxplot(p,packcontrol, packcase,notch=TRUE,
            names=c("All Subjects","Controls","Cases"),ylab="Pack Years")
    # Pack Years for Cases (Ex-Smokers and Current Smokers)
    exsmokers<-S28[S28=="Ex"] # Ex-Smokers
    currentsmokers<-S28[S28=="Yes"] # Current Smokers
    currentpckcase<-packcase[S28=="Yes"]
    currentpckcase <-currentpckcase[!is.na(currentpckcase)]
    # Current Smokers Cases Pack Years
    mean(currentpckcase)
    var<-var(currentpckcase);sqrt(var)
    Expackcase<-packcase[S28=="Ex"]
    Expackcase<-Expackcase[!is.na(Expackcase)] # Ex-smoker Cases Pack Years
    mean(Expackcase)
    var<-var(Expackcase);sqrt(var)
    # Pack Years for Controls (Ex-Smokers and Current Smokers)
    Currentpackcontrol<-packcontrol[S28=="Yes"]
    75
    76
    # Current smoker ,current age over 20
    #Pack Years for relevant Cases
    
    # Current Smokers controls pack years
    Currentpackcontrol <-Currentpackcontrol[!is.na(Currentpackcontrol)]
    mean(Currentpackcontrol)
    var<-var(Currentpackcontrol);sqrt(var)
    Expackcontrol<-packcontrol[S28=="Ex"] # Ex-smoker controls pack years
    Expackcontrol<-Expackcontrol[!is.na(Expackcontrol)]
    mean(Expackcontrol)
    var<-var(Expackcontrol);sqrt(var)
    #### Bach Risk Model ####
    ###Smoking Duration###
    for(i in 1:length(S31))  #(Assigning 0 to NA values)
    {if (is.na(S30[i])) {S30[i]=0}
      {if (is.na(S31[i])) {S31[i]=0}}}
    dur<-S31-S30  #Smoking Duration (Ex-smokers) = Age Stopped smoking regularly - Age started
    for(i in 1:length(dur))
    {if (dur[i]<0) {dur[i]= AgeRegistered[i]-S30[i]}}  #Current Smokers smoking duration
    #Assuming negative value (subject 431 with missing age) is 0 (ie non-smoker)
    dur[is.na(dur)]<-0
    #Quit Duration
    quit<-AgeRegistered -S31;quit
    for(i in 1:879)
    {if (S31[i]==0) {quit[i]=0} #Assing "NA" to nagtive values (current smokers)
      {if (quit[i]<0) {quit[i]=0}}} #Assing 0 to nagtive values (S31>Age)
    # As cigarettes per day are used in the calculation of pack years,
    I take account the
    # starting age and stopping age to calculate the weighted average
    #Only for purpose of calculating the risk model:
    To assing NAs (subject 835 and 431) "Nil" value
    for(i in 1:length(p))
    {if (is.na(p[i])) {p[i]=0}}  #Pack years
    CPD<-rep(NA,length(p)) #Create an array of NAs with length S31
    for(i in 1:length(p))
    {if (S30[i]==0 && S31[i]==0) {CPD[i]=0} # Non-smokers
      else
      {if ((S31[i]==0)) {CPD[i]<-20*p[i]/(AgeRegistered[i]-S30[i])}  # Current smoker
        else
        {CPD[i]<-20*p[i]/(S31[i]-S30[i])}}}  # Ex-smoker
    mean(CPD)  #12.42
    # oneyeardiagprob :  one-year probability of a diagnosis of lung cancer
    # oneyeardeathprob:one-year probability of death
    in the absence of a diagnosis of lung cancer
    ####durone, durtwo, durthree, durfour######
    #  CPD dur+1(if current smoker) quit AgeRegistered+1 S20 Sex
    durone<-rep(NA,length(dur))
    durtwo<-rep(NA,length(dur))
    durthree<-rep(NA,length(dur))
    durfour<-rep(NA,length(dur))
    for (i in 1:length(dur))    # If current smoker year add up
    {if  (S31[i]=="0") {durone[i]<-dur[i]+1}
      #Duration of smoking afre one year to calculate 2 years risk
      {if  (S31[i]=="0") {durtwo[i]<-dur[i]+2}
        {if  (S31[i]=="0") {durthree[i]<-dur[i]+3}
          {if  (S31[i]=="0") {durfour[i]<-dur[i]+4}
            # Non-smoker
            {if  (dur[i]=="0") {durone[i]<-0}
              {if  (dur[i]=="0") {durtwo[i]<-0}
                {if  (dur[i]=="0") {durthree[i]<-0}
                  {if  (dur[i]=="0") {durfour[i]<-0}
                    # Ex-smoker
                    {if  (S31[i]>"0") {durone[i]<-dur[i]}
                      {if  (S31[i]>"0") {durtwo[i]<-dur[i]}
                        {if  (S31[i]>"0") {durthree[i]<-dur[i]}
                          {if  (S31[i]>"0") {durfour[i]<-dur[i]}
                          }}}}}}}}}}}}
    ##One-year probability of death in the absence of diagnosis of lung cancer##
    #### CPD ####
    model1d<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(CPD))
    {if (CPD[i]>15) {model1d[i]<-(0.015490665*CPD[i])
    -7.2036219-(0.00001737645 * ((CPD[i]-15)^3))}
      # Age registered - Age quit = years quit
      77
      78
      
      {if (CPD[i]>20) {model1d[i]<-model1d[i]
      +(0.000021924149 *(CPD[i]-20.185718)^3)}
        {if (CPD[i]>40 && CPD[i]<60) {model1d[i]<-model1d[i]
        -(0.0000045476985 * (CPD[i]-40)^3)}
        }}} #### Duration: SMK (dur)####
    model2d<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(dur))
    {if (dur[i]>27) {model2d[i]<-(0.020041889 * dur[i])
    +0.0000065443781 * ((dur[i]-27.6577) ^3)}
      {if (dur[i]>40) {model2d[i]<- model2d[i]-(0.000013947696 * (dur[i] -40)^ 3) }
        {if (dur[i]>50  && dur[i]<50) {model2d[i]<- model2d[i]
        +(0.0000074033175 * (dur[i]-50.910335)^ 3) }
        }}}
    model2done<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(durone))
    {if (durone[i]>27) {model2done[i]<-(0.020041889 * durone[i])
    +0.0000065443781 * ((durone[i]-27.6577) ^3)}
      {if (durone[i]>40) {model2done[i]<- model2done[i]
      -(0.000013947696 * (durone[i] -40)^ 3) }
        {if (durone[i]>50  && durone[i]<50) {model2done[i]<-
          model2d[i]+(0.0000074033175 * (durone[i]-50.910335)^ 3) }
        }}}
    model2dtwo<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(durtwo))
    {if (durtwo[i]>27) {model2dtwo[i]<-(0.020041889 * durtwo[i])
    +0.0000065443781 * ((durtwo[i]-27.6577) ^3)}
      {if (durtwo[i]>40) {model2dtwo[i]<- model2dtwo[i]
      -(0.000013947696 * (durtwo[i] -40)^ 3) }
        {if (durtwo[i]>50  && durtwo[i]<50) {model2dtwo[i]<-
          model2dtwo[i]+(0.0000074033175 * (durtwo[i]-50.910335)^ 3) }
        }}}
    model2dthree<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(durthree))
    {if (durthree[i]>27) {model2dthree[i]<-(0.020041889 * durthree[i])
    +0.0000065443781 * ((durthree[i]-27.6577) ^3)}
      {if (durthree[i]>40) {model2dthree[i]<- model2dthree[i]
      -(0.000013947696 * (durthree[i] -40)^ 3) }
        {if (durthree[i]>50  && durthree[i]<50) {model2dthree[i]
          <- model2dthree[i]+(0.0000074033175 * (durthree[i]-50.910335)^ 3) }
        }}}
    model2dfour<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(durfour))
    {if (durfour[i]>27) {model2dfour[i]<-(0.020041889 * durfour[i])
    +0.0000065443781 * ((durfour[i]-27.6577) ^3)}
      {if (durfour[i]>40) {model2dfour[i]<- model2dfour[i]
      -(0.000013947696 * (durfour[i] -40)^ 3) }
        {if (durfour[i]>50  && durfour[i]<50) {model2dfour[i]<-
          model2dfour[i]+(0.0000074033175 * (durfour[i]-50.910335)^ 3) }
        }}} #### Quitting Duration (QUIT)####
    model3d<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(quit))
    {model3d[i]<-(0.0019208669*(quit[i]^3))-(0.023358962 * quit[i])}
    {if (quit[i]>0) {model3d[i]<-model3d[i]- (0.0020031611 * (quit[i]- 0.50513347) ^3)
    {if (quit[i]>12 && quit[i]<20) {model3d[i]<-model3d[i]
    +(0.000082294194 * (quit[i] -12.295688) ^3)}
    }} #### Age (AgeRegistered)####
      model4d<-rep(NA,879) #Create an array of NAs with length S31
      for(i in 1:length(AgeRegistered))
      {if (AgeRegistered[i]>53)
      {model4d[i]<-(0.099168033*AgeRegistered[i])+
        (0.0000062174577*((AgeRegistered[i]-53.459001)^3))}
        {if (AgeRegistered[i]>61)
        {model4d[i]<-model4d[i]-0.000012115774*((AgeRegistered[i]-61.954825)^3)}
          {if (AgeRegistered[i]>70 && AgeRegistered[i]<75)
          {model4d[i]<-model4d[i]+(0.0000058983164 *((AgeRegistered[i]-70.910335)^3) )}
          }}}
      model4done<-rep(NA,879) #Create an array of NAs with length S31
      for(i in 1:length(AgeRegistered))
      {if ((AgeRegistered[i]+1)>53)
      {model4done[i]<-(0.099168033*(AgeRegistered[i]+1))
      79
      80
      
      +(0.0000062174577*(((AgeRegistered[i]+1)-53.459001)^3))}
        {if ((AgeRegistered[i]+1)>61)
        {model4done[i]<-model4done[i]-0.000012115774
        *(((AgeRegistered[i]+1)-61.954825)^3)}
          {if ((AgeRegistered[i]+1)>70 && (AgeRegistered[i]+1)<75)
          {model4done[i]<-model4done[i]+(0.0000058983164
                                         *(((AgeRegistered[i]+1)-70.910335)^3) )}
          }}}
      model4dtwo<-rep(NA,879) #Create an array of NAs with length S31
      for(i in 1:length(AgeRegistered))
      {if ((AgeRegistered[i]+2)>53)
      {model4dtwo[i]<-(0.099168033*(AgeRegistered[i]+2))
      +(0.0000062174577*(((AgeRegistered[i]+2)-53.459001)^3))}
        {if ((AgeRegistered[i]+2)>61)
        {model4dtwo[i]<-model4dtwo[i]-0.000012115774
        *(((AgeRegistered[i]+2)-61.954825)^3)}
          {if ((AgeRegistered[i]+2)>70 && AgeRegistered[i]<75)
          {model4dtwo[i]<-model4dtwo[i]+(0.0000058983164
                                         *(((AgeRegistered[i]+2)-70.910335)^3) )}
          }}}
      model4dthree<-rep(NA,879) #Create an array of NAs with length S31
      for(i in 1:length(AgeRegistered))
      {if ((AgeRegistered[i]+3)>53)
      {model4dthree[i]<-(0.099168033*(AgeRegistered[i]+3))
      +(0.0000062174577*(((AgeRegistered[i]+3)-53.459001)^3))}
        {if ((AgeRegistered[i]+3)>61)
        {model4dthree[i]<-model4dthree[i]-0.000012115774
        *(((AgeRegistered[i]+3)-61.954825)^3)}
          {if ((AgeRegistered[i]+3)>70 && (AgeRegistered[i]+3)<75)
          {model4dthree[i]<-model4dthree[i]+(0.0000058983164
                                             *(((AgeRegistered[i]+3)-70.910335)^3) )}
          }}}
      model4dfour<-rep(NA,879) #Create an array of NAs with length S31
      for(i in 1:length(AgeRegistered))
      {if ((AgeRegistered[i]+4)>53)
      {model4dfour[i]<-(0.099168033*(AgeRegistered[i]+4))
      +(0.0000062174577*(((AgeRegistered[i]+4)-53.459001)^3))}
        {if ((AgeRegistered[i]+4)>61)
        {model4dfour[i]<-model4dfour[i]-0.000012115774
        *(((AgeRegistered[i]+4)-61.954825)^3)}
          {if ((AgeRegistered[i]+4)>70 && AgeRegistered[i]<75)
          {model4dfour[i]<-model4dfour[i]+(0.0000058983164
                                           *(((AgeRegistered[i]+4)-70.910335)^3) )}
          }}} #### ASB = asbestos exposure (S20)####
      model5d<-rep(NA,879) #Create an array of NAs
      for(i in 1:879)
      {if (S20[i]=="Yes") {model5d[i]<-0.06084611}
        else   {model5d[i]<-0} }
      #### Sex ####
      for(i in 1:length(Sex))
      {if (is.na(Sex[i])) {Sex[i]=1}}
      model6d<-rep(NA,879) #Create an array of NAs
      for (i in 1:879)
      {if (Sex[i]=="2") {model6d[i]=(-0.49042298)}
        {if (Sex[i]=="1") {model6d[i]=0}
        }} #### Model ####
      modeld<-rep(NA,879) #Create an array of NAs
      modeldone<-rep(NA,879) #Create an array of NAs
      modeldtwo<-rep(NA,879) #Create an array of NAs
      modeldthree<-rep(NA,879) #Create an array of NAs
      modeldfour<-rep(NA,879) #Create an array of NAs
      for (i in 1:879){
        {modeld[i]<-model1d[i] + model2d[i] + model3d[i] +
          model4d[i]+ model5d[i]+ model6d[i]}
        {modeldone[i]<-model1d[i] + model2done[i] + model3d[i] +
            model4done[i]+ model5d[i]+ model6d[i]}
        {modeldtwo[i]<-model1d[i] + model2dtwo[i] + model3d[i]
          +model4dtwo[i]+ model5d[i]+ model6d[i]}
        {modeldthree[i]<-model1d[i] + model2dthree[i] + model3d[i]
          +model4dthree[i]+ model5d[i]+ model6d[i]}
        81
        82
        
        {modeldfour[i]<-model1d[i] + model2dfour[i] + model3d[i]
          +model4dfour[i]+ model5d[i]+ model6d[i]}}
      # One-year probability of a death in the absence of a diagnosis of lung cancer
      # = 1 - (S_0)*(e^{model}) for S_0 =1 - (0.9917663)^(e^{model})
      oneyeardeathprob<-rep(NA,length(modeld)) #Create an array of NAs with length S3
      twoyeardeathprob<-rep(NA,length(modeld)) #Create an array of NAs with length S31
      threeyeardeathprob<-rep(NA,length(modeld)) #Create an array of NAs with length S31
      {if (durone[i]>40) {model2one[i]<-model2one[i]
      +(0.00017069483 * (durone[i] -40)^ 3) }
        {if (durone[i]>50 && durone[i]<50) {model2one[i]<-model2one[i]
        -(0.000090603358 * (durone[i]-50.910335)^ 3) }
        }}}
    model2two<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(dur))
    {if (durtwo[i]>27) {model2two[i]<-(0.11425297* durtwo[i])
    -(0.000080091477 * (durtwo[i]-27.6577) ^3)}
      {if (durtwo[i]>40) {model2two[i]<-model2two[i]
      +(0.00017069483 * (durtwo[i] -40)^ 3) }
        {if (durtwo[i]>50 && durtwo[i]<50) {model2two[i]<-model2two[i]
        -(0.000090603358 * (durtwo[i]-50.910335)^ 3) }
        }}}
    model2three<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(durthree))
    {if (durthree[i]>27) {model2three[i]<-(0.11425297* durthree[i])
    -(0.000080091477 * (durthree[i]-27.6577) ^3)}
      {if (durthree[i]>40) {model2three[i]<-model2three[i]
      +(0.00017069483 * (durthree[i] -40)^ 3) }
        {if (durthree[i]>50 && durthree[i]<50) {model2three[i]<-
          model2three[i]-(0.000090603358 * (durthree[i]-50.910335)^ 3) }
        }}}
    model2four<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(durfour))
    {if (durfour[i]>27) {model2four[i]<-(0.11425297* durfour[i])
    -(0.000080091477 * (durfour[i]-27.6577) ^3)}
      {if (durfour[i]>40) {model2four[i]<-model2four[i]+
        (0.00017069483 * (durfour[i] -40)^ 3) }
        {if (durfour[i]>50 && durfour[i]<50) {model2four[i]<-
          model2four[i]-(0.000090603358 * (durfour[i]-50.910335)^ 3) }
        }}} #### Quitting Duration (QUIT)####
    model3<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(quit))
    {model3[i]<-(0.0065499693*(quit[i] ^3))-(0.085684793*quit[i])}
    fouryeardeathprob<-rep(NA,length(modeld)) #Create an array of
    fiveyeardeathprob<-rep(NA,length(modeld)) #Create an array of
    for(i in 1:length(modeld)){
      {oneyeardeathprob[i]<- 1-(0.9917663^(exp(-modeld[i])))}
      {twoyeardeathprob[i]<- 1-(0.9917663^(exp(-modeldone[i])))}
      {threeyeardeathprob[i]<- 1-(0.9917663^(exp(-modeldtwo[i])))}
      {fouryeardeathprob[i]<- 1-(0.9917663^(exp(-modeldthree[i])))}
      {fiveyeardeathprob[i]<- 1-(0.9917663^(exp(-modeldfour[i])))}}
    #####One-year probability of a diagnosis of lung cancer######
    #### CPD ####
    NAs with length S31
    NAs with length S31
    model1<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(CPD))
    {if (CPD[i]>15) {model1[i]<-(0.060818386*CPD[i])-9.7960571-(0.00014652216 * ((CPD[i]-15)^3))}
      {if (CPD[i]>20) {model1[i]<-model1[i]+(0.00018486938*(CPD[i]-20.185718)^3)}
        {if (CPD[i]>40 && CPD[i]<60) {model1[i]<-model1[i]-(0.000038347226*(CPD[i]-40)^3)}
        }}}
    #### Duration: SMK (dur)####
    model2<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(dur))
    {if (dur[i]>27) {model2[i]<-(0.11425297*dur[i])-(0.000080091477 * (dur[i]-27.6577) ^3)}
      {if (dur[i]>40) {model2[i]<-model2[i]+(0.00017069483 * (dur[i] -40)^ 3) }
        {if (dur[i]>50 && dur[i]<50) {model2[i]<-model2[i]-
          (0.000090603358 * (dur[i]-50.910335)^ 3) }
        }}}
    model2one<-rep(NA,879) #Create an array of NAs
    for(i in 1:length(dur))
    {if (durone[i]>27) {model2one[i]<-(0.11425297*durone[i])
    -(0.000080091477 * (durone[i]-27.6577) ^3)}
      83
      84
      
      {if (quit[i]>0) {model3[i]<-model3[i]-(0.0068305845*(quit[i]-0.50513347) ^3)}
        {if (quit[i]>12 && quit[i]<20) {model3[i]<-model3[i]+(0.00028061519 * (quit[i]-12.295688) ^3)}
        }}
      #### Age (AgeRegistered)####
      model4<-rep(NA,879) #Create an array of NAs
      for(i in 1:length(AgeRegistered))
      {if (AgeRegistered[i]>53) {model4[i]<-(0.070322812*AgeRegistered[i])-
        (0.00009382122*(AgeRegistered[i]-53.459001)^3)}
        {if (AgeRegistered[i]>61) {model4[i]<-model4[i]
        +(0.00018282661*(AgeRegistered[i]-61.954825)^3)}
          {if (AgeRegistered[i]>70  && AgeRegistered[i]<75)
          {model4[i]<-model4[i]-(0.000089005389 *(AgeRegistered[i]-70.910335)^3)}
          }}}
      model4one<-rep(NA,879) #Create an array of NAs
      for(i in 1:length(AgeRegistered))
      {if ((AgeRegistered[i]+1)>53) {model4one[i]<-(0.070322812*(AgeRegistered[i]+1))-
        (0.00009382122*(AgeRegistered[i]-53.459001)^3)}
        {if ((AgeRegistered[i]+1)>61)
        {model4one[i]<-model4one[i]+(0.00018282661
                                     *((AgeRegistered[i]+1)-61.954825)^3)}
          {if ((AgeRegistered[i]+1)>70  && (AgeRegistered[i]+1)<75)
          {model4one[i]<-model4one[i]-(0.000089005389
                                       *((AgeRegistered[i]+1)-70.910335)^3)}}}}
      model4two<-rep(NA,879) #Create an array of NAs
      for(i in 1:length(AgeRegistered))
      {if ((AgeRegistered[i]+2)>53) {model4two[i]<-(0.070322812*(AgeRegistered[i]+2))
      -(0.00009382122*((AgeRegistered[i]+2)-53.459001)^3)}
        {if ((AgeRegistered[i]+2)>61) {model4two[i]<-model4two[i]+(0.00018282661
                                                                   *((AgeRegistered[i]+2)-61.954825)^3)}
          {if ((AgeRegistered[i]+2)>70  && (AgeRegistered[i]+2)<75)
          {model4two[i]<-model4two[i]-(0.000089005389 *((AgeRegistered[i]+2)-70.910335)^3)}
          }}}
      model4three<-rep(NA,879) #Create an array of NAs
      for(i in 1:length(AgeRegistered))
      {if ((AgeRegistered[i]+3)>53)
      {model4three[i]<-(0.070322812*(AgeRegistered[i]+3))
      -(0.00009382122*(AgeRegistered[i]-53.459001)^3)}
        {if ((AgeRegistered[i]+3)>61)
        {model4three[i]<-model4three[i]+(0.00018282661
                                         *((AgeRegistered[i]+3)-61.954825)^3)}
          {if ((AgeRegistered[i]+3)>70  && (AgeRegistered[i]+3)<75)
          {model4three[i]<-model4three[i]-(0.000089005389
                                           *((AgeRegistered[i]+3)-70.910335)^3)}}}}
      model4four<-rep(NA,879) #Create an array of NAs
      for(i in 1:length(AgeRegistered))
      {if ((AgeRegistered[i]+4)>53)
      {model4four[i]<-(0.070322812*(AgeRegistered[i]+4))
      -(0.00009382122*(AgeRegistered[i]-53.459001)^3)}
        {if ((AgeRegistered[i]+4)>61)
        {model4four[i]<-model4four[i]+(0.00018282661
                                       *((AgeRegistered[i]+4)-61.954825)^3)}
          {if ((AgeRegistered[i]+4)>70  && (AgeRegistered[i]+4)<75)
          {model4four[i]<-model4four[i]-(0.000089005389
                                         *((AgeRegistered[i]+4)-70.910335)^3)}}}} #### ASB = asbestos exposure (S20)####
      model5<-rep(NA,879) #Create an array of NAs
      for(i in 1:length(S20))
      {if (S20[i]=="Yes") {model5[i]<-0.2153936}
        else   {model5[i]<-0} #### Sex ####
        for(i in 1:length(Sex))
        {if (is.na(Sex[i])) {Sex[i]=1}}
        model6<-rep(NA,879) #Create an array of NAs
        for (i in 1:879)
        {if (Sex[i]=="2") {model6[i]=(-0.05827261)}
          {if (Sex[i]=="1") {model6[i]=0}
          }} #### Model ####
        diagmodel<-rep(NA,879) #Create an array of NAs
        diagmodelone<-rep(NA,879)
        diagmodeltwo<-rep(NA,879)
        diagmodelthree<-rep(NA,879)
        diagmodelfour<-rep(NA,879)
        85
        86
        
        for (i in 1:879){
          {diagmodel[i]<-model1[i] + model2[i] + model3[i] +model4[i]+ model5[i]+ model6[i]}
          {diagmodelone[i]<-model1[i] + model2one[i] + model3[i] + model4one[i]+ model5[i]+ model6[i]}
          {diagmodeltwo[i]<-model1[i] + model2two[i] + model3[i] + model4two[i]+ model5[i]+ model6[i]}
          {diagmodelthree[i]<-model1[i] + model2three[i] + model3[i] +
              #  +(probability of Survival and no diagnosis
              of lung cancer at the end of year 1,2,3 and 4 *
              probability of a diagnosis of lung cancer at the end of year 5)
Risk<-rep(NA,879)
for (i in 1:879){
  Risk[i]<-oneyeardiagprob[i]*(1-oneyeardeathprob[i])
  + (1-oneyeardeathprob[i])*(1-twoyeardeathprob[i])*(1-oneyeardiagprob[i])
  *twoyeardiagprob[i]+(1-oneyeardeathprob[i])*(1-twoyeardeathprob[i])
  *(1-threeyeardeathprob[i])*(1-oneyeardiagprob[i])*(1-twoyeardiagprob[i])
  *threeyeardiagprob[i]+(1-oneyeardeathprob[i])*(1-twoyeardeathprob[i])
  *(1-threeyeardeathprob[i])*(1-fouryeardeathprob[i])*(1-oneyeardiagprob[i])
  *(1-twoyeardiagprob[i])*(1-threeyeardiagprob[i]) *fouryeardiagprob[i]
  + (1-oneyeardeathprob[i])*(1-twoyeardeathprob[i])*(1-threeyeardeathprob[i])
  *(1-fouryeardeathprob[i])*(1-fiveyeardeathprob[i])*(1-oneyeardiagprob[i])
  *(1-twoyeardiagprob[i])*(1-threeyeardiagprob[i])
  *(1-fouryeardiagprob[i])*fiveyeardiagprob[i]}
summary(na.exclude(Risk))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 0.000000 0.008056 0.013650 0.015490 0.019550 0.073740
#Validation: http://jnci.oxfordjournals.org/content/95/6/470/suppl/DC1
######## Bach ROC Vurve  ###########
#Exclude unknown values of Case Control with respect to uknown values of Risk
for(i in 1:879)
{if (is.na(Risk[i])) {CaseControl_c[i]=NA}}
Risk<-(na.exclude(Risk))
CaseControl_c<-(na.exclude(CaseControl_c))
#for(i in 1:length(alpha))
#{if (is.na(alpha[i])) {alpha[i]=0}}
CaseControl_c <-ifelse(CaseControl_c=="Case", 1, 0)
#Recode to Case=1 and Control=0
length(Risk)#230
length(CaseControl_c) #230
bach<-cbind(Risk, CaseControl_c)
bach<-data.frame(bach)
attach(bach)
model4three[i]+ model5[i]+ model6[i]}
          {diagmodelfour[i]<-model1[i] + model2four[i] + model3[i]
            + model4four[i]+ model5[i]+ model6[i]}}
        # The one-year probability of a diagnosis of lung cancer
        #Create an array of NAs with length S31
        oneyeardiagprob<-rep(NA,length(diagmodel))  #Probability
        twoyeardiagprob<-rep(NA,length(diagmodel)) #Probability of diagnosis in 2 years
        threeyeardiagprob<-rep(NA,length(diagmodel)) #Probability of diagnosis in 3 years
        fouryeardiagprob<-rep(NA,length(diagmodel)) #Probability of diagnosis in 4 years
        fiveyeardiagprob<-rep(NA,length(diagmodel)) #Probability of diagnosis in 5 years
        for(i in 1:length(diagmodel)){
          {oneyeardiagprob[i]<-1-(0.99629^(exp(-diagmodel[i])))}
          {twoyeardiagprob[i]<-1-(0.99629^(exp(-diagmodelone[i])))}
          {threeyeardiagprob[i]<-1-(0.99629^(exp(-diagmodeltwo[i])))}
          {fouryeardiagprob[i]<-1-(0.99629^(exp(-diagmodelthree[i])))}
          {fiveyeardiagprob[i]<-1-(0.99629^(exp(-diagmodelfour[i])))}}
        #########
        # oneyeardiagprob :  one-year probability of a diagnosis of lung cancer
        # oneyeardeathprob:one-year probability of death in the absence of a diagnosis of lung cancer
        #  5 year risk=
        #  +(probability of a diagnosis of lung cancer at the end of year 1)
        #  +(probability of Survival and no diagnosis
        of lung cancer at the end of year 1 *
          probability of a diagnosis of lung cancer at the end of year 2)
#  +(probability of Survival and no diagnosis
of lung cancer at the end of year 1 and 2 *
  probability of a diagnosis of lung cancer at the end of year 3)
#  +(probability of Survival and no diagnosis
of lung cancer at the end of year 1,2 and 3 *
  probability of a diagnosis of lung cancer at the end of year 4)
87
88
= 1 - (0.99629)*(e^{model})
of diagnosis 1year

######
par(mfrow = c(1,2))
library(pROC)
library(ROCR)
library(gdata)
library(Daim)
library(verification)
#Plots a ROC for a given model
roc.plot(bach$CaseControl_c,bach$Risk, xlab = "False positive rate",
         ylab = "True positive rate", main = NULL, CI = T, n.boot = 100,
         plot = "both", binormal = TRUE)
M <- roc(bach$Risk,bach$CaseControl_c, "1")
summary(M)
plot(M,main = NULL)
#Sensitivity : 0.693 ; Specificity :  0.490 ; best cut-off :  0.010 ;  AUC :  0.606
ci.auc(bach$CaseControl_c,bach$Risk) # 95% CI: 0.517-0.695 (DeLong)
##### AUC #####
auc = colAUC(bach$Risk,bach$CaseControl_c, plotROC=TRUE, alg=c("Wilcoxon","ROC"))
auc  #0 vs. 1   0.6059
# Confidence Interval
t <- wilcox.test(bach$Risk,bach$CaseControl_c,paired=FALSE,conf.int=0.95)
######## Calcuating Sensitivity- Specificity ########
library(caret) #to calculate Sensitivity / Positivity
for (i in 1:230)
{if (bach$Risk[i]<0.025) {bach$Risk[i]<-0}
  {if (bach$Risk[i]>=0.025) {bach$Risk[i]<-1}}}  #Recode to Case=1 and Control=0
sensitivity(factor(bach$Risk), factor(bach$CaseControl_c)) #0.8824
specificity(factor(bach$Risk), factor(bach$CaseControl_c)) #0.1564
posPredValue(factor(bach$Risk), factor(bach$CaseControl_c)) # 0.2296
negPredValue(factor(bach$Risk), factor(bach$CaseControl_c)) #0.8235
#### LLP Model ####
# Assign "Nil" to unknown values
for(i in 1:length(AgeRegistered))
{if (is.na(AgeRegistered[i])) {AgeRegistered[i]=0}
  {if (is.na(Sex[i])) {Sex[i]="NA"} }}
#AgeRegistered = 5x+y
y<-rep(NA,length(AgeRegistered))
for(i in 1:length(AgeRegistered))
{y[i]<-AgeRegistered[i]%%5} # Age
x<-rep(NA,length(AgeRegistered))
for(i in 1:length(AgeRegistered))
{x[i]<-(AgeRegistered[i]-y[i])/5}
AgeRegistered[1] #46.4
x[1] #9
y[1] #1.4
### Calculating Alpha ###
alpha<-rep(NA,879)
for(i in 1:length(AgeRegistered))
{if (AgeRegistered[i]>=40 && AgeRegistered[i]<45 && Sex[i]=="2")
{if (AgeRegistered[i]>=45 && AgeRegistered[i]<50 && Sex[i]=="2")
{if (AgeRegistered[i]>=50 && AgeRegistered[i]<55 && Sex[i]=="2")
{if (AgeRegistered[i]>=55 && AgeRegistered[i]<60 && Sex[i]=="2")
{if (AgeRegistered[i]>=60 && AgeRegistered[i]<65 && Sex[i]=="2")
{if (AgeRegistered[i]>=65 && AgeRegistered[i]<70 && Sex[i]=="2")
{if (AgeRegistered[i]>=70 && AgeRegistered[i]<75 && Sex[i]=="2")
{if (AgeRegistered[i]>=75 && AgeRegistered[i]<80 && Sex[i]=="2")
{if (AgeRegistered[i]>=80 && AgeRegistered[i]<85 && Sex[i]=="2")
{if (AgeRegistered[i]>=40 && AgeRegistered[i]<45 && Sex[i]=="1")
{if (AgeRegistered[i]>=45 && AgeRegistered[i]<50 && Sex[i]=="1")
{if (AgeRegistered[i]>=50 && AgeRegistered[i]<55 && Sex[i]=="1")
{if (AgeRegistered[i]>=55 && AgeRegistered[i]<60 && Sex[i]=="1")
{if (AgeRegistered[i]>=60 && AgeRegistered[i]<65 && Sex[i]=="1")
{if (AgeRegistered[i]>=65 && AgeRegistered[i]<70 && Sex[i]=="1")
{if (AgeRegistered[i]>=70 && AgeRegistered[i]<75 && Sex[i]=="1")
{if (AgeRegistered[i]>=75 && AgeRegistered[i]<80 && Sex[i]=="1")
{if (AgeRegistered[i]>=80 && AgeRegistered[i]<85 && Sex[i]=="1")
}}}}}}}}}}}}}}}}}}
for(i in 1:length(AgeRegistered))
{alpha[i]<-( (5-y[i]-0.5)*alpha[i] + (y[i]+0.5)*alpha[i+5] )/5}
### Calculating Family history for Beta ###  ### Beta ###
89
90
Registered Modulus 5
# Age Registered multiple of 5
{alpha[i]<--9.9}
{alpha[i]<--8.06}
{alpha[i]<--7.46}
{alpha[i]<--6.5}
{alpha[i]<--6.22}
{alpha[i]<--5.99}
{alpha[i]<--5.49}
{alpha[i]<--5.23}
{alpha[i]<--5.42}
{alpha[i]<--9.06}
{alpha[i]<--8.16}
{alpha[i]<--7.31}
{alpha[i]<--6.63}
{alpha[i]<--5.97}
{alpha[i]<--5.56}
{alpha[i]<--5.31}
{alpha[i]<--4.83}
{alpha[i]<--4.68}

beta1<-rep(NA,879)
for(i in 1:879)
{if (dur[i]==0) {beta1[i]<-0}
  {if (dur[i]>=1 && dur[i]<20) {beta1[i]<-0.769}
    {if (dur[i]>=21 && dur[i]<40) {beta1[i]<-1.452}
      {if (dur[i]>=41 && dur[i]<60) {beta1[i]<-2.507}
        {if (dur[i]>=60) {beta1[i]<--2.724}}}}}}
beta2<-rep(NA,879)
for(i in 1:879)
{if (S20[i]=="Yes") {beta2[i]<-0.602}
  else {beta2[i]<-0} }
### Calculate Early and Late inset familty values
for(i in 1:879) #Assign "0" to NA values
{if (is.na(Family[i])) {Family[i]<-0}}
for(i in 1:879)
{if (AgeRegistered[i]>60 &&  Family[i]=="1") {Family[i]=1}  # Late onset then Family=1
  {if (AgeRegistered[i]>60 &&  Family[i]=="2") {Family[i]=1}  # Late onset then Family=1
    {if (AgeRegistered[i]<60 &&  Family[i]=="1") {Family[i]=2}  # Early onset then Family=2
      {if (AgeRegistered[i]<60 &&  Family[i]=="2") {Family[i]=2}  # Early onset then Family=2}}}}
        beta3<-rep(NA,879)
        for(i in 1:879)
        {if (is.na(Family[i])) {beta3[i]<-0}
          {if (Family[i]=="2") {beta3[i]<-0.703} #Early
            {if (Family[i]=="1") {beta3[i]<-0.168} #Late}}}
              beta<-rep(NA,879) #Create an array of NAs
              for (i in 1:879)
              {beta[i]<-beta1[i]+beta2[i]+beta3[i]}
              ### Calculating P ###
              p<-rep(NA,879) #Create an array of NAs
              for(i in 1:879)
              {p[i]= (1/ (1+exp(- ( alpha[i] +beta[i] ))))}
              summary(p) # mean =1.802 e-02
              #Exclude unknown values of p and respectively uknown values of Case Control
              for(i in 1:879)
              {if (is.na(p[i])) {CaseControl_c[i]=NA}}
              p<-(na.exclude(p))
              CaseControl_c<-(na.exclude(CaseControl_c))
              #for(i in 1:length(alpha))
              #{if (is.na(alpha[i])) {alpha[i]=0}}
              CaseControl_c <-ifelse(CaseControl_c=="Case", 1, 0)
              #Recode to Case=1 and Control=0
              length(p) #106
              length(CaseControl_c)  #106
              llp<-cbind(p,CaseControl_c)
              llp<-data.frame(llp)
              attach(llp)
              par(mfrow = c(1,2))
              library(ROCR)
              library(gdata)
              library(Daim)
              library(verification)
              #Plots a ROC for a given model
              roc.plot(llp$CaseControl_c, llp$p, xlab = "False positive rate",
                       ylab = "True positive rate", main = NULL, CI = T, n.boot = 100,
                       plot = "both", binormal = TRUE)
              M <- roc(llp$p, llp$CaseControl_c, "1")
              summary(M)
              plot(M) #Sensitivity : 0.619 ; Specificity : 0.864 ; best cut-off : 0.01043
              auc = colAUC(llp$p,llp$CaseControl_c, plotROC=TRUE, alg=c("Wilcoxon","ROC"))
              auc  #0 vs. 1  0.7192
              #http://rss.acs.unt.edu/Rdoc/library/verification/html/roc.plot.html
              #CIs are computed with Delong’s method (DeLong et al.) based
              on U-statistics theory and asymptotic normality. As this test does not
              # require bootstrapping
              ci.auc(llp$CaseControl_c,llp$p) #
              ######## Calcuating Sensitivity- Specificity ########
              library(caret) #to calculate Sensitivity / Positivity
              for (i in 1:106)
              {if (llp$p[i]<0.0104) {llp$p[i]<-0}
                {if (llp$p[i]>=0.0104) {llp$p[i]<-1}}}  #Recode to Case=1 and Control=0
              91
              92
              
              sensitivity(factor(llp$p), factor(llp$CaseControl_c) ,"1") # 0.619
              posPredValue(factor(llp$p), factor(llp$CaseControl_c)) #0.3725
              negPredValue(factor(llp$p), factor(llp$CaseControl_c)) #0.9455