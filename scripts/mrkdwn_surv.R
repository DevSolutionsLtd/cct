## Data Analysis for Survey of the SURE-P MCH CCT Pilot, June 2015

data <- read.csv('surveydata.csv')

## Characteristics of the sample
#Checking for randomness of the sample (numerical vector used)
library("lawstat") #if unavailable, first download with command 'install.packages("lawstat")
runs.test(data$child.no)

#No of beneficiaries interviewed per State
ben <- table(data$state)
ben
barplot(ben,
        ylim=c(0,60),
        col="pink",
        main="Beneficiaries Surveyed",
        ylab="No. of Beneficiaries",
        axes=F)
axis(2,las=2)
#X-square Goodness-Of-Fit test
p_state<-rep(1/9,9) #probabilities
chisq.test(ben,p=p_state) 
rm(p_state,ben)

#No of beneficiaries interviewed per Health facility
hf<-table(data$facility)
hf
#Chi-squared GOF Test
chisq.test(hf)
rm(hf)

#Beneficiaries' Age
ages<-factor(data$age[data$age!=""]) #removing the blanks within the vector
levels(ages) <- c("15-25","26-35","36-45")
ages<-table(ages)
ages
round(prop.table(ages),2)
barplot(ages,
        ylim=c(0,200),
        col="pink",
        main="Age of CCT Beneficiaries",
        ylab='No. of Beneficiaries',
        xlab="Age (years)",
        axes=F)
axis(2,las=2)
#Chi-square GOF Test
chisq.test(ages)
rm(ages)

#Beneficiaries' religion
religion<-table(data$religion)
religion
round(prop.table(religion),2)
barplot(religion,
        ylim=c(0,200),
        col="pink",
        main="Beneficiaries' Religion",
        ylab="No. of Beneficiaries",
        xlab="Religion",
        axes=F)
axis(2,las=2)
#Chi-square GOF Test
chisq.test(religion)
rm(religion)

#Beneficiaries' ethnicity
ethnic<-factor(data$nu_ethnicity[data$nu_ethnicity!=""]) #new variable merging all minor ethnic groups in the dataset
ethnic<-factor(ethnic,
               levels=c("Gbagyi","Hausa","Ibo","Ijaw","Nupe","Yoruba","Others"))
ethnic<-table(ethnic)
ethnic
round(prop.table(ethnic),2)
barplot(prop.table(ethnic),
        col="lightblue",
        axes=FALSE,
        ylim=c(0,.3),
        ylab="Proportion",
        xlab="Ethnicity",
        main="Proportions of Beneficiaries' Ethnicity")
axis(2,las=2)
#Chi-square GOF
ethnicp<-c(.05,.28,.2,.1,.02,.29,.06) #proportion of ethnic groups from literature
chisq.test(ethnic,p=ethnicp)
rm(ethnicp,ethnic)

#Beneficiaries' marital status
marital<-factor(data$marital.status[data$marital.status!=""])
marital <- factor(marital, levels = c("Married", "Single", "Separated", "Widowed"))
marital<-table(marital)
marital
round(prop.table(marital),3)
barplot (marital,
         ylim=c(0,300),
         axes=F,
         main='Marital Status of Beneficiaries',
         col="pink",
         ylab="No. of Beneficiaries",
         xlab="Marital Status")
axis(2,las=2)
rm(marital)

#Beneficiaries' literacy
#English
lit.eng <- data$literacy.eng
lit.eng <- table(lit.eng)
lit.eng
barplot(lit.eng,
        col="pink",
        axes=F,
        ylim=c(0,200),
        main="Literacy in English",
        ylab="No. of Beneficiaries")
axis(2,las=2)
#analysing by State
lit.eng.state <- table(data$literacy.eng,data$state)
lit.eng.state
round(prop.table(lit.eng.state,2),2)
barplot(prop.table(lit.eng.state,2),
        legend=T,
        main="Literacy in English according to State",
        axes=F,
        ylab="Proportion of Beneficiaries",
        ylim=c(0,1))
axis(2,las=2)
#Chi-squared Test of Independence
chisq.test(lit.eng.state)
rm(lit.eng,lit.eng.state)

#Hausa
lit.hau <- data$literacy.hau
lit.hau <- factor(data$literacy.hau[data$literacy.hau!=""])
lit.hau <- table(lit.hau)
lit.hau
round(prop.table(lit.hau),2)
barplot(prop.table(lit.hau),
        col="lightblue",
        axes=F,
        ylim=c(0,.8),
        main="Literacy in Hausa",
        ylab="Proportion of Beneficiaries")
axis(2,las=2)

#Ibo
lit.ibo <- data$literacy_ibo
lit.ibo <- factor(data$literacy_ibo[data$literacy_ibo!=""])
lit.ibo <- table(lit.ibo)
lit.ibo
round(prop.table(lit.ibo),2)
barplot(prop.table(lit.ibo),
        col="lightblue",
        axes=F,
        main="Literacy in Ibo",
        ylab="Proportion of Beneficiaries",
        ylim=c(0,.8))
axis(2,las=2)

#Yoruba
lit.yor <- data$literacy.yor
lit.yor <- factor(data$literacy.yor[data$literacy.yor!=""])
lit.yor <- table(lit.yor)
lit.yor
round(prop.table(lit.yor),2)
barplot(prop.table(lit.yor),
        col="lightblue",
        axes=F,
        main="Literacy in Yoruba",
        ylab="Proportion of Beneficiaries",
        ylim=c(0,1))
axis(2,las=2)

#Pidgin
lit.pid <- data$literacy.pid
lit.pid <- factor(data$literacy.pid[data$literacy.pid!=""])
lit.pid <- table(lit.pid)
lit.pid
round(prop.table(lit.pid),2)
barplot(prop.table(lit.pid),
        col="lightblue",
        axes=F,
        main="Literacy in Pidgin",
        ylab="Proportion of Beneficiaries",
        ylim=c(0,1))
axis(2,las=2)

#Beneficiaries' occupation
occ<-factor(data$occupation[data$occupation!=""]) #removing blanks
occ<-table(occ)
occ
round(prop.table(occ),2)
barplot(prop.table(occ),
        axes=F,
        ylim=c(0,.4),
        col="lightblue",
        main="Beneficiaries' Occupation",
        ylab="Proportion of Beneficiaries",
        xlab="Occupation")
axis(2,las=2)
barplot(prop.table(occ),
        axes=F,
        ylim=c(0,.4),
        col="lightblue",
        main="Beneficiaries' Occupation",
        ylab="Proportion of Beneficiaries",
        xlab="Occupation")
axis(2,las=2)
#Chi-squared Goodness-of-Fit Test
chisq.test(occ)

#Family income
faminc<-factor(data$family.income[data$family.income!=""])
levels(faminc)
levels(faminc)<-c("<.9","1-10","11-20","21-30","31-40","41-50",">51") #alter levels
faminc<-table(faminc)
faminc
barplot (faminc,
         col="pink",
         main= "Family Income of Beneficiaries (NGN '000)",
         axes=F)
axis(2,las=2)
round(prop.table(faminc),2)
barplot(round(prop.table(faminc),2),
        col="lightblue",
        main="Family Income of the Beneficiaries",
        ylab="Proportion",
        xlab="Income Group (NGN'000)",
        axes=F)
axis(2,las=2)

#Personal income
perinc<-factor(data$personal.income[data$personal.income!=""])
levels(perinc)
levels(perinc)<-c("<0.9","1-10","11-20","21-30","31-40","41-50",">51") #change levels
perinc<-table(perinc)
perinc
barplot(perinc,
        axes=FALSE,
        ylim=c(0,150),
        col="pink",
        main="Personal Income of the Beneficiaries",
        ylab="No. of Beneficiaries",
        xlab="Income Category (NGN '000)")
axis(2,las=2)
round(prop.table(perinc),3)
barplot(prop.table(perinc),
        axes=FALSE,
        ylim=c(0,.5),
        col="lightblue",
        main="Personal Income of the Beneficiaries",
        ylab="Proportion",
        xlab="Income Category")
axis(2,las=2)

#Descriptives of beneficiaries' no. of children
summary(data$child.no)
hist(data$child.no,
     breaks=7,
     axes=FALSE,
     main='No. of Children Born',
     ylab="No. of Beneficiaries",
     xlab='No. of children',
     col="magenta")
axis(2,las=1)
axis(1,las=0)

#No. of children had before CCT
summary(data$child.before.cct)
hist(data$child.before.cct,
     breaks=7,
     ylab="No. of Beneficiaries",
     main='Children Born before CCT',
     xlab="No. of Children",col="pink",
     ylim=c(0,140),
     axes=FALSE)
axis(2,las=2)
axis(1,las=0)

#No. of children that had ANC before CCT
summary(data$anc.before.cct)
hist(data$anc.before.cct,
     breaks=7,
     ylim=c(1,120),
     col="pink",
     ylab="No. of Beneficiaries",
     main= 'Has Attended ANC before CCT',
     xlab="No. of Children",
     axes=FALSE)
axis(1,las=0)
axis(2,las=2)

#Descriptives of beneficiaries' reasons for not going for ANC
no.anc <- (data$reason.no.anc)
levels(no.anc)<-c("","Didn't know imptce","Distance from PHC",
                  "Lack of money","Phobia")
no.anc<-factor(no.anc[no.anc!=""])
no.anc <- table(no.anc)
no.anc
barplot(no.anc,col="pink")

#No. of children delivered in a health facility
summary(data$delivered.hf)
hist(data$delivered.hf,
     breaks=7,
     col="purple",
     ylab="No. of Beneficiaries",
     main='No of Children Delivered in a Health Facility',
     xlab="No. of Children",
     axes=FALSE)
axis(1,las=0)
axis(2,las=2)

#Plotting the variables describing No. of Children and Utilization of health services
boxplot(data$child.no,
        data$child.before.cct,
        data$anc.before.cct,
        data$delivered.hf,
        main="Utilization of Services",
        ylab="No. of children",
        names=c("Born","Born Before CCT","ANC Before CCT","Delivered in HF"))

#Where beneficiaries had a child outside of a health facility
outdeliv<-factor(data$outside.delivery[data$outside.delivery!=""]) #removing all those Not Applicable
outdeliv<-table(outdeliv)
outdeliv
barplot(outdeliv,
        ylim=c(0,70),
        col="pink",
        ylab="No. of Beneficiaries",
        main="Location of Non-Health Facility Deliveries",
        axes=F)
axis(2,las=2)
round(prop.table(outdeliv),2)
barplot(prop.table(outdeliv),
        ylab="Proportion of Beneficiaries",
        ylim=c(0,.6),col="lightblue",
        axes=F)
axis(2,las=2)

#Type of health facilities beneficiaries used to deliver 
hf<-factor(data$hf.type[data$hf.type!=""])
hf <- table(hf)
round(prop.table(hf),3)
barplot(hf,
        col="pink",
        ylim=c(0,250),
        ylab="No. of Beneficiaries",
        xlab="Type of Health Facility",
        main="Type of Health Facility Beneficiaries Used for Deliveries",
        axes=F)
axis(2,las=2)

#Reasons given for not delivering in a health facility
notinhf<-factor(data$reason.notinhf[data$reason.notinhf!=""])
notinhf<-table(notinhf)
notinhf
round(prop.table(notinhf),2)

#Whether beneficiaries brought newborns for immunization
immun<-factor(data$newborn.immun[data$newborn.immun!=""])
immun<-table(immun)
immun
round(prop.table(immun),2)
barplot(prop.table(immun),
        main='Immunization of Newborns',
        col="lightblue",
        ylim=c(0,1),
        ylab="Proportion of beneficiaries",
        axes=F)
axis(2,las=2)

#Reasons why beneficiaries did not bring newborn for immunization
notimmun<-factor(data$reason.notimmun[data$reason.notimmun!=""])
levels(notimmun)<-c("Didn't know imptce","Distance of HF","Just delivered") #adjusting for space
levels(notimmun)
notimmun<-table(notimmun)
notimmun
round(prop.table(notimmun),2)
barplot(notimmun,
        ylim=c(0,10),
        col="pink",
        main='Reasons for Not Taking Newborn for Immunization',
        axes=F,
        ylab="No. of beneficiaries")
axis(2,las=2)

#Where beneficiaries first heard about CCT 
cctinfo <- data$info.cct[data$info.cct!=""]
cctinfo<-table(cctinfo)
cctinfo
round(prop.table(cctinfo),2)

cctinfo2<-table(data$nu_cctinfo) #created new variable ooutside of R to lump low freq responses
cctinfo2
barplot(cctinfo2, ylim=c(0,140),col="pink", ylab='No. of Beneficiaries')
round(prop.table(cctinfo2),2)
barplot(prop.table(cctinfo2),
        ylim=c(0,0.5),
        ylab="Proportion of beneficiaries",
        col="lightblue",
        axes=F)
axis(2,las=2)
#Chi-square GOF test
infop<-c(1/6,1/6,1/6,1/6,1/6,1/6)
chisq.test(cctinfo2,p=infop)
rm(infop)

#Beneficiaries' stage of pregnancy at time of CCT enrolment
ga<-factor(data$preg.stage[data$preg.stage!=""])
ga<-factor(ga,levels=c("First trimester","Second trimester","Third trimester","At delivery"))
ga<-table(ga)
ga
round(prop.table(ga),2)
barplot(prop.table(ga),
        ylim=c(0,.6),
        main='Pregnancy Stage at CCT Enrolment',
        col="lightblue",
        ylab="No. of beneficiaries",
        axes=F)
axis(2,las=2)
#Chi-squared GOF Test
gap<-c(.25,.25,.25,.25)
chisq.test(ga,p=gap)
chisq.test(ga,p=gap)$expected
rm(ga)

#Whether beneficiaries were given adequate information on their co-responsibilities
vec<-factor(data$adequate.info[data$adequate.info!=""])
levels(vec) <- c("Yes","Fair","No") #reordering responses
vec <- table(vec)
vec
barplot(vec,
        ylim=c(0,300),
        col="pink",
        ylab="No. of Beneficiaries",
        xlab="Response",
        axes=F,
        main="Where you informed of your co-responsibilities?")
axis(2,las=2)
round(prop.table(vec),2)

#At what point beneficiaries where given information on their co-responsibilities
whencrsp<-table(data$info.coresp)
whencrsp

#Wether the record in beneficiaries' card was the same as that in the register
cardsame<-factor(data$same.record[data$same.record!=""])
levels(cardsame)
cardsame<-factor(cardsame,levels=c("Yes","Some mistakes","No","Don't know")) #reorder responses
levels(cardsame)
cardsame<-table(cardsame)
cardsame
round(prop.table(cardsame),3)
barplot(prop.table(cardsame),
        ylim=c(0,1),
        col="lightblue",
        ylab="Porportion of Beneficiaries",
        xlab="Response",
        axes=F,
        main="Was record in card and register the same?")
axis(2,las=2)

#Complaints about CCT programme at enrolment
complain<-factor(data$complaints[data$complaints!=""])
complain<-table(complain)
complain
round(prop.table(complain),2)
barplot(prop.table(complain),
        ylim=c(0,1),
        col="lightblue",
        ylab="Proportion of Beneficiaries",
        xlab="Response",
        main="Complaints about CCT at enrolment?",
        axes=F)
axis(2,las=2)

#Whether beneficiaries where provided avenues to make complaints
avenue<-factor(data$complaint.avenue[data$complaint.avenue!=""])
avenue <-table(avenue)
avenue
round(prop.table(avenue),2)
barplot(prop.table(avenue),
        ylab="Proportion of Beneficiaries",
        col="lightblue",
        main="Where you provided avenue for laying complaints?",
        ylim=c(0,.6),
        axes=F)
axis(2,las=2)

#Was action taken to address beneficiaries' concerns?
measr<-factor(data$measures.taken[data$measures.taken!=""])
measr <- table(measr)
measr
round(prop.table(measr),2)
barplot(prop.table(measr),
        ylim=c(0,.8),
        main="Action Taken to Address Complaints",
        col="lightblue",
        axes=F,
        ylab="Proportion of beneficiaries")
axis(2,las=2)

#Perception of enrolment 
process<-factor(data$cct.process[data$cct.process!=""])
levels(process)
process<-factor(process,levels=c("Easy","Not so easy","Difficult","Very difficult"))
process<-table(process)
process
round(prop.table(process),2)
barplot(prop.table(process),
        ylim=c(0,1),
        col="lightblue",
        main='Perception on CCT Enrolment',
        ylab="Proportion of beneficiaries",
        axes=F)
axis(2,las=2)

#Perception of the entire CCT process
process2<-factor(data$experience[data$experience!=""])
process2<-factor(process2,levels=c("Easy","Not so easy","Difficult","Very difficult"))
process2<-table(process2)
process2
round(prop.table(process2),2)
barplot(prop.table(process2),
        ylim=c(0,1),
        col="lightblue", 
        ylab="Proporion of beneficiaries",
        xlab="Response",
        axes=F,
        main="Perception of entire CCT process")
axis(2,las=2)

#Experience from enrolment to payment of cash incentives
exp <- factor(data$experience[data$experience!=""])
levels(exp)
exp <- factor(exp,levels=c("Easy","Not so easy","Difficult","Very difficult"))
exp <-table(exp)
exp
round(prop.table(exp),2)
barplot(prop.table(exp),
        ylim=c(0,1),
        main='Perception on Entire CCT Process',
        col="lightblue",
        axes=F,
        ylab="Proportion of beneficiaries")
axis(2,las=2)

#How beneficiaries received information on pay-out events
payinfo<-factor(data$cct.info[data$cct.info!=""])
payinfo<-table(payinfo)
payinfo
round(prop.table(payinfo),2)

#Timeliness of information of pay-out events
timeinfo<-factor(data$timeliness.info[data$timeliness.info!=""])
timeinfo<-table(timeinfo)
timeinfo
round(prop.table(timeinfo),2)

#Whether pay-out events where on schedule
sched<-factor(data$payout.onschedule[data$payout.onschedule!=""])
levels(sched)
sched<-factor(sched,levels=c("Yes","No","Don't know"))
sched<-table(sched)
sched
round(prop.table(sched),2)
barplot(prop.table(sched),
        ylim=c(0,1),
        col="lightblue",
        ylab="Proportion of beneficiaries",
        main='Is Pay-Out Schedule reliable?',
        axes=F)
axis(2,las=2)

#Number of pay-outs per beneficiary
paynum<-factor(data$no.of.payments[data$no.of.payments!=""])
paynum<-table(paynum)
paynum
round(prop.table(paynum),3)
barplot(paynum,
        ylim=c(0,200),
        ylab="No. of Beneficiaries",
        col="pink",
        xlab="Payments per Beneficiary",
        main = "Number of Payments Received",
        axes=F)
axis(2,las=2)

#Did amount paid meet beneficiaries' expectation?
expamt<-factor(data$expected.amount.paid[data$expected.amount.paid!=""])
expamt<-table(expamt)
expamt
round(prop.table(expamt),2)
barplot(prop.table(expamt),
        main='Did you receive the amount you expected?',
        axes=F,
        ylim=c(0,.8),
        col="lightblue",
        ylab="Proportion of beneficiaries")
axis(2,las=2)

#Where they given reasons for any non-payment?
givreas<-factor(data$reason.given[data$reason.given!=""])
givreas<-table(givreas)
givreas
round(prop.table(givreas),2)

#Total amount received by beneficiaries
amount<-factor(data$amount.received[data$amount.received!=""])
amount<-factor(amount,
               levels=c("N1,000",   "N2,000" ,  "N3,000" ,  "N4,000" ,  "N5,000" ))
amount<-table(amount)
amount
round(prop.table(amount),2)
barplot(prop.table(amount),
        ylim=c(0,.5),
        col="lightblue",
        ylab="Proportion of Beneficiaries",
        xlab="Amount Paid",
        main="Total Amount Received",
        axes=F)
axis(2,las=2)




#Description of pay-out events
org<-factor(data$payout.organized[data$payout.organized!=""])
org
org<-factor(org,levels=c("Not organized","Fairly organized","Well organized"))
levels (org)
org <- table(org)
org
round(prop.table(org),2)
barplot(prop.table(org),
        ylim=c(0,.7),
        col="lightblue",
        main='Description of Pay-Out Events',
        ylab="Proportion of beneficiaries",
        axes=F)
axis(2,las=2)
#Chi-squared GOF Test
orgp<-c(1/3,1/3,1/3)
chisq.test(org,p=orgp)
rm(orgp)

#Payment for services at the health facility
payserv<-factor(data$pay.for.services[data$pay.for.services!=""])
payserv
payserv<-table(payserv)
payserv
round(prop.table(payserv),2)
barplot(prop.table(payserv),
        ylim=c(0,.7),
        col="lightblue",
        ylab="Proportion of Beneficiaries" ,
        xlab="Response",
        main='Payment for PHC Services',
        axes=F)
axis(2,las=2)
#Chi-squared GOF Test
payservp<-c(.5,.5)
chisq.test(payserv,p=payservp)
rm(payservp)

#What type of service was paid for
wtpd<-factor(data$service.paid.for[data$service.paid.for!=""])
head(wtpd)
wtpd <- table(wtpd)
wtpd
round(prop.table(wtpd),2)
barplot (prop.table(wtpd),
         ylim=c(0,.7),
         col="lightblue",
         ylab="Proportion of Beneficiaries",
         axes=F,
         main="Type of Services Paid for")
axis(2,las=2)
#Chi-squared GOF Test
pdp<-c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
chisq.test(wtpd,p=pdp)
rm(pdp)

#Husband/partner awareness of their enrolment with CCT
husbaw<-factor(data$husband.aware[data$husband.aware!=""])
head(husbaw)
husbaw<-table(husbaw)
husbaw
round(prop.table(husbaw),2)
barplot(prop.table(husbaw),
        ylim=c(0,1),
        main="Husband/Partner Awareness of CCT Enrolment",
        col="lightblue",
        ylab="Proportion of Beneficiaries",
        axes=F)
axis(2,las=2)

#Husbnand/partner awareness of the amount received
husmoney<-factor(data$husband.know.amount[data$husband.know.amount!=""])
head(husmoney)
husmoney<-table(husmoney)
husmoney
round(prop.table(husmoney),2)
barplot(prop.table(husmoney),
        ylim=c(0,1),
        main="Husband/Partner Awareness of Cash Payment",
        ylab="Proportion of beneficiaries",
        axes=F,
        col="lightblue")
axis(2,las=2)

#Husband/partners feeling about the CCT programme
feels<-factor(data$husband.feels[data$husband.feels!=""])
feels<-factor(feels,levels=c("Not satisfied","Fairly satisfied","Satisfied"))
feels<-table(feels)
feels
round(prop.table(feels),2)
barplot(prop.table(feels),
        col="lightblue" ,
        ylab="No. of Beneficiaries",
        main="Husband/Partner Feeling about the Programme",
        axes=F,
        ylim=c(0,1))
axis(2,las=2)

#Did cash incentive helped beneficiaries?
helped<-factor(data$incentive.help[data$incentive.help!=""])
head(helped)
helped<-table(helped)
helped
round(prop.table(helped),2)
barplot(prop.table(helped),
        ylim=c(0,1), 
        ylab="Proportion of Beneficiaries",
        axes=F,
        main="Did cash incentive help?",
        col="lightblue")
axis(2,las=2)

#How families where helped by incentive
how<-factor(data$help.how[data$help.how!=""])
how<-table(how)
how
round(prop.table(how),2)
barplot(prop.table(how),
        col="lightblue",
        ylab="Proportion of Beneficiaries",
        main="How incentive helped beneficiaries",
        axes=F,
        ylim=c(0,.6))
axis(2,las=2)

##SELECTED BIVARIATE ANALYSES

#Relationship between State and perception of CCT enrolment
datax <-data[data$cct.process!="",]   #create a dataframe that exludes blanks in this variable
rel1<-table(datax$cct.process,datax$state)
rel1
round(prop.table(rel1,2),2)
print(round(prop.table(rel1,2),2),zero.print="-")
barplot(rel1,
        ylim=c(0,60),
        beside=T,
        col=c("darkblue","red","yellow","black","green"),
        main="Perception of CCT Enrolment by State",
        ylab="No. of Beneficiaries",axes=F)
legend(legend=c("Difficult","Easy","Not so easy","Very difficult"),
       fill=c("darkblue","red","yellow","black","green"),
       "topright",
       horiz=F,
       cex=.7)
axis(2,las=2)
rm(rel1)

#No. of payments received vs. Amount Paid
datax <- data[data$amount.received!="",]
datax <- datax[datax$no.of.payments!="0",]
dim(datax)
newpay <- factor(datax$amount.received[datax$amount.received!=""])
newpay <-table(newpay,datax$no.of.payments)
newpay
round(prop.table(newpay,1),2)
print(round(prop.table(newpay,1),2),zero.print="-")
barplot(newpay,
        beside=TRUE,
        legend=TRUE,
        col=c("red","black","yellow","blue", "green"),
        ylab="No. of Beneficiaries", 
        xlab="No. of Instalments",
        ylim=c(0,70),
        main="No. of Instalments vs. Amount Paid",
        axes=F)
axis(2,las=2)
rm(newpay)

#Payment for PHC Services by State
datax <- data[data$pay.for.services!="",]
nu <- factor(datax$pay.for.services[datax$pay.for.services!=""])
tab <-  table(nu,datax$state)
tab
round(prop.table(tab,2),2)
barplot(tab,
        beside=TRUE,
        legend=F,
        col=c("pink","darkblue"),
        ylim=c(0,40),
        ylab="No. of Beneficiaries",
        main="Payment for services vs. State",
        axes=F)
legend(legend=c("No","Yes"),
       "topright",
       cex=.7,
       fill=c("pink","darkblue"))
axis(2,las=2)
rm(tab,nu)

#No. of Payments per state
tab <- table(data$no.of.payments,data$state)
tab
print(tab,zero.print="-")
barplot(tab,
        beside=TRUE,
        col=c("green","red","blue","grey","yellow","purple"),
        main="Pay-Out frequency across the Pilot States",
        ylab="No. of beneficiaries",
        axes=F)
legend(legend=c("0","1","2","3","4","5"),
       "topright",
       cex=.7,
       fill=c("green","red","blue","grey","yellow","purple"),
       title="Instalments")
axis(2,las=2)
rm(tab)

#Amount Paid to Beneficiaries by State
datax<-data[data$amount.received!="",]
dim(datax)
pay <- factor(datax$amount.received[datax$amount.received!=""])
tab <- table(pay,datax$state)
tab
round(prop.table(tab,1),2)
print(round(prop.table(tab,1),2),zero.print="-")
barplot(tab,
        beside=TRUE,
        ylim=c(0,30),
        col=c("red","black","yellow","blue","green"),
        ylab="No. of Beneficiaries",
        main="Amount Paid vs. State",
        axes=F)
legend("topleft",
       cex=.7,
       legend=c("N1,000","N2,000","N3,000","N4,000","N5,000"),
       fill=c("red","black","yellow","blue","green"))
axis(2,las=2)
#Chi-squared Test of Independence
chisq.test(tab)
chisq.test(tab)$expected
rm(tab,pay)

#Whether reasons were given for non-payment across the Pilot States
datax<-data[data$reason.given!="",]
pay <-factor(datax$reason.given[datax$reason.given!=""])
pay <- table(pay,datax$state)
round(prop.table(pay,2),2)
barplot(pay,
        beside=TRUE,
        col=c("pink","darkblue"),
        legend=TRUE,
        ylim=c(0,10),
        ylab="No. of Beneficiaries",
        main="Reasons Given for Non-Payment vs. State",
        axes=F)
axis(2,las=2)
rm(pay)

#Perception of Pay-Out Events vs. Pilot State
datax<-data[data$payout.organized!="",]
pay <-factor(datax$payout.organized[datax$payout.organized!=""])
pay <-factor(pay,levels=c("Not organized","Fairly organized","Well organized"))
pay <- table(pay,datax$state)
pay
round(prop.table(pay,2),2)
print(round(prop.table(pay,2),2),zero.print="-")
barplot(pay,
        beside=TRUE,
        legend=F,
        ylim=c(0,40),
        col=c("yellow","green","red"),
        ylab="No. of Beneficiaries",
        main="Perception of Pay-Out Events vs. Pilot State",
        axes=F)
axis(2,las=2)
legend(legend=c("Not Org","Fair","Well Org"),
       "topright",cex=.5,
       fill=c("yellow","green","red"))
#X-squared
chisq.test(pay,correct=T)
rm(pay)

#Where Beneficiaries First Heard about CCT compared by Pilot State
datax<-data[data$info.cct!="",]
inf<-factor(datax$info.cct[datax$info.cct!=""])
inf<-table(inf,datax$state)
print(inf,zero.print="-")
print(round(prop.table(inf,2),2),zero.print="-")
rm(inf)

#Where beneficiaries heard about Pay=Out Events by Pilot State
datax<-data[data$cct.info!="",]
infx<-factor(datax$cct.info[datax$cct.info!=""])
infx<-table(infx,datax$state)
infx
print(round(prop.table(infx,2),2),zero.print="-")
rm(infx)

#Comparing provision of avenues to lay complaints among the Pilot States
datax<-data[data$complaint.avenue!="",]
newave<-factor(datax$complaint.avenue[datax$complaint.avenue!=""])
newave<-table(newave,datax$state)
newave
round(prop.table(newave,2),2)
barplot(newave,
        beside=T,
        legend=T,
        ylim=c(0,10),
        main="Avenues for Laying Complaints by Pilot State",
        axes=F,
        ylab="No. of beneficiaries")
axis(2,las=2)
rm(newave)

#Comparing by State, whether received the amount they were expecting
datax<-data[data$expected.amount.paid!="",]
exptpay<-factor(datax$expected.amount.paid[datax$expected.amount.paid!=""])
exptpay<-table(exptpay,datax$state)
exptpay
round(prop.table(exptpay,2),2)
barplot(exptpay,
        beside=T,
        legend=F,
        col=c("black","lightgrey"),
        main="Receipt of Expected Amount in the States",
        ylab="No. of beneficiaries",
        axes=F)
axis(2,las=2)
legend(legend=c("No","Yes"),
       "topright",
       fill=c("black","lightgrey"),
       cex=.7)
#Chi-squared Test of Independence
chisq.test(exptpay,simulate.p.value=TRUE)
chisq.test(exptpay,simulate.p.value=TRUE)$expected
chisq.test(exptpay,correct=T)
rm(exptpay)

#Compare Beneficiaries religion by state
relig<-table(data$religion,data$state)
relig
rm(relig)

#Examining the Stage of Pregnancy During Enrolment and the Amount Paid 
datax<-data[data$amount.received!="",]
amt<-factor(datax$amount.received[datax$amount.received!=""])
stage<-factor(datax$preg.stage,levels=c("First trimester","Second trimester","Third trimester","At delivery"))
stageamt<-table(amt,stage)
stageamt
round(prop.table(stageamt,2),2)
barplot(stageamt,
        beside=TRUE,
        legend=FALSE,
        ylim=c(0,50),
        col=c("red","green","blue","yellow","darkgrey"),
        main="Amount Paid According to Pregnancy Stage at Enrolment",
        axes=F,
        ylab="No. of beneficiaries",
        xlab="Stage of Pregnancy")
axis(2,las=2)
legend(legend=c("N1,000","N2,000","N3,000","N4,000","N5,000"),
       "topright",
       fill=c("red","green","blue","yellow","darkgrey"),
       cex=.7,
       title="Amt Pd")
#Chi-squared Test of Independence
chisq.test(stageamt,correct=T)
chisq.test(stageamt)$expected
rm(amt,stage,stageamt)

#Husbands Aware of Amount Paid across Pilot States
datax<-data[data$husband.know.amount!="",]
husb<- factor(datax$husband.know.amount[datax$husband.know.amount!=""])
husb<-table(husb,datax$state)
husb
round(prop.table(husb,2),2)
print(round(prop.table(husb,2),2),zero.print="-")
barplot(husb,
        beside=TRUE,
        legend=TRUE,
        ylim=c(0,50),
        axes=F,
        main="Awareness amongs Partners in different Pilot States",
        ylab= "No. of beneficiaries")
axis(2,las=2)
#Chi-squared Test of Independence
chisq.test(husb)
chisq.test(husb)$expected
rm(husb)

#Non-Health Facility Delivery by States
datax<-data[data$outside.delivery!="",]
outer<-factor(datax$outside.delivery[datax$outside.delivery!=""])
outer<-table(outer,datax$state)
outer
print(round(prop.table(outer,2),2),zero.print="-")
barplot(outer,
        beside=T,
        ylim=c(0,20),
        ylab="No. of Beneficiaries",
        col=c("red","yellow","blue","magenta","green"),
        axes=F,
        main="Non-Health Facility Deliveries by State")
axis(2,las=2)
legend("topright",
       fill=c("red","yellow","blue","magenta","green"),
       legend=c("At Church","Home By Herself","Home with SBA" ,"Private facility",
                "TBA"),
       cex=.6)
rm(outer)

#Reliability of Pay-Out Schedule by State
datax<-data[data$payout.onschedule!="",]
schedx<-factor(datax$payout.onschedule[datax$payout.onschedule!=""])
schedx<-factor(schedx,levels=c("Yes","No","Don't know"))
schedx<-table(schedx,datax$state)
schedx
print(round(prop.table(schedx,2),2),zero.print="-")
barplot(schedx,
        beside=TRUE,
        ylim=c(0,50),col=c("green","red","yellow"),
        legend=FALSE,
        ylab="No. of Beneficiaries",
        main="Reliablity of Pay-Out Schedule by State",
        axes=F)
axis(2,las=2)
legend(legend=c("Yes","No","Don't know"),
       "topright",
       fill=c("green","red","yellow"),
       cex=.7)
rm(schedx)

#Age vs. Stage of Pregnancy at Enrolment
datax <- data[data$age!="",]
ages <- factor(datax$age[datax$age!=""])
stage<-factor(datax$preg.stage,levels=c("First trimester","Second trimester","Third trimester","At delivery","NAs"))
levels(stage)
tabx <- table(ages,stage)
tabx
barplot(tabx,
        ylim=c(0,100),
        beside=TRUE,
        col=c("black","red","blue"),
        legend=TRUE,
        axes=FALSE,
        main="Beneficiary Age vs. Pregnancy Stage at Enrolment",
        ylab="No. of beneficiaries",
        xlab="Pregnancy Stage")
axis(2,las=2)
rm(list=ls())

#END


