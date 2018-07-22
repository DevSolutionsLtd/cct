#Data Analysis for Survey of the SURE-P MCH CCT Pilot, June 2015

#START

#Loading the dataset ----------------------------------
library(RSQLite)
con <- dbConnect(SQLite(), "data/surveydata.db")
data <- dbReadTable(con, "main")
dbDisconnect(con)


#Characteristics of the sample
#Checking for randomness of the sample (numerical vector used)
library("lawstat") 
runs.test(data$child.no)



# Analyses------------------------------------
# No of beneficiaries interviewed per State
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
barplot(prop.table(ages)*100,
        ylim=c(0,.6)*100,
        col="lightblue",
        main="Age of CCT Beneficiaries",
        ylab='Percentage',
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
barplot(prop.table(ethnic)*100,
        col="lightblue",
        axes=FALSE,
        ylim=c(0,.3)*100,
        ylab="%",
        xlab="Ethnicity",
        main="Beneficiaries' Ethnicity")
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
round(prop.table(lit.eng),2)
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
barplot(prop.table(lit.eng.state,2)*100,
        legend=T,
        main="Literacy in English according to State",
        axes=F,
        ylab="Percentage of Beneficiaries",
        ylim=c(0,1)*100)
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
barplot(prop.table(lit.hau)*100,
        col="lightblue",
        axes=F,
        ylim=c(0,.8)*100,
        main="Literacy in Hausa",
        ylab="Percentage of Beneficiaries")
axis(2,las=2)
rm(lit.hau)

#Ibo
lit.ibo <- data$literacy_ibo
lit.ibo <- factor(data$literacy_ibo[data$literacy_ibo!=""])
lit.ibo <- table(lit.ibo)
lit.ibo
round(prop.table(lit.ibo),2)
barplot(prop.table(lit.ibo)*100,
        col="lightblue",
        axes=F,
        main="Literacy in Ibo",
        ylab="Proportion of Beneficiaries",
        ylim=c(0,.8)*100)
axis(2,las=2)
rm(lit.ibo)

#Yoruba
lit.yor <- data$literacy.yor
lit.yor <- factor(data$literacy.yor[data$literacy.yor!=""])
lit.yor <- table(lit.yor)
lit.yor
round(prop.table(lit.yor),2)
barplot(prop.table(lit.yor)*100,
        col="lightblue",
        axes=F,
        main="Literacy in Yoruba",
        ylab="Proportion of Beneficiaries",
        ylim=c(0,1)*100)
axis(2,las=2)
rm(lit.yor)

#Pidgin
lit.pid <- data$literacy.pid
lit.pid <- factor(data$literacy.pid[data$literacy.pid!=""])
lit.pid <- table(lit.pid)
lit.pid
round(prop.table(lit.pid),2)
barplot(prop.table(lit.pid)*100,
        col="lightblue",
        axes=F,
        main="Literacy in Pidgin",
        ylab="Proportion of Beneficiaries",
        ylim=c(0,1)*100)
axis(2,las=2)
rm(lit.pid)

#Beneficiaries' occupation
occ<-factor(data$occupation[data$occupation!=""]) #removing blanks
occ<-table(occ)
occ
round(prop.table(occ),2)
barplot(prop.table(occ)*100,
        axes=F,
        ylim=c(0,30),
        col="lightblue",
        main="Beneficiaries' Occupation",
        ylab="Percentage",
        xlab="Occupation")
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
#Chi-squared Goodness-of-Fit Test
chisq.test(occ)
rm(occ)

#Family income
faminc<-factor(data$family.income[data$family.income!=""])
levels(faminc)
levels(faminc)<-c("<.9","1-10","11-20","21-30","31-40","41-50",">51") #alter levels
faminc <- factor(faminc, ordered = TRUE)
faminc <- table(faminc)
faminc
barplot (faminc,
         col="pink",
         main= "Family Income of Beneficiaries (NGN '000)",
         ylab = "No. of beneficiaries",
         axes=FALSE)
axis(2,las=2)

round(prop.table(faminc),2)
barplot(prop.table(faminc)*100,
        col="lightblue",
        main="Family Income of the Beneficiaries",
        ylab="Percentage",
        xlab="Income Group (NGN'000)",
        ylim = c(0,25),
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lwd = 2)
rm(faminc)

#Personal income
perinc<-factor(data$personal.income[data$personal.income!=""])
levels(perinc) <- c("<0.9","1-10","11-20","21-30","31-40","41-50",">51")
perinc <- factor(perinc, 
                 levels = c("<0.9","1-10","11-20","21-30","31-40","41-50",">51"),
                 ordered = TRUE) #change levels
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
barplot(prop.table(perinc)*100,
        axes=FALSE,
        ylim=c(0, 50),
        col="lightblue",
        main="Personal Income of the Beneficiaries",
        ylab="Percentage",
        xlab="Income Category")
axis(2,las=2)
grid(NA, ny = NULL, lwd = 2)
rm(perinc)

#Descriptives of beneficiaries' no. of children
layout(matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))
hist(data$child.no,
     breaks=7,
     main='Children Born to Beneficiaries',
     col = "lightgrey",
     ylim = c(0, 140),
     xlab = NULL,
     ylab = "No. of Beneficiaries")
hist(data$child.before.cct,
     breaks=7,
     main='Children Born before CCT',
     col="lightgrey",
     ylim = c(0, 140),
     ylab = NULL,
     xlab = NULL)
hist(data$anc.before.cct,
     breaks=7,
     col="lightgrey",
     ylab = NULL,
     xlab = NULL,
     main= 'ANC before CCT',
     ylim = c(0, 140))
hist(data$delivered.hf,
     breaks=7,
     col="lightgrey",
     main='Children Delivered in HF',
     xlab= "No. of Children",
     ylim = c(0, 140),
     ylab = NULL)
layout(1)
layout.show()

#Plotting the variables describing No. of Children and Utilization of health services
boxplot(data$child.no,
        data$child.before.cct,
        data$anc.before.cct,
        data$delivered.hf,
        main="Utilization of Services",
        ylab="No. of children",
        names=c("Born","Born Before CCT","ANC Before CCT","Delivered in HF"))

#Where beneficiaries had a child outside of a health facility
outdeliv <- factor(data$outside.delivery[data$outside.delivery!=""]) #removing all those Not Applicable
outdeliv <- table(outdeliv)
outdeliv
barplot(outdeliv,
        ylim=c(0,70),
        col="pink",
        ylab="No. of Beneficiaries",
        main="Location of Non-Health Facility Deliveries",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
#in percentage
round(prop.table(outdeliv),2)
barplot(prop.table(outdeliv)*100,
        ylab="Proportion of Beneficiaries",
        main="Location of Non-Health Facility Deliveries",
        ylim=c(0,60),
        col="lightblue",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(outdeliv)

#Type of health facilities beneficiaries used to deliver 
hf<-factor(data$hf.type[data$hf.type!=""])
hf <- table(hf)
hf
round(prop.table(hf),3)
barplot(prop.table(hf)*100,
        col="lightblue",
        ylim=c(0,100),
        ylab="Percentage",
        xlab="Type of Health Facility",
        main="Type of Health Facility Used for Deliveries",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(hf)

#Reasons given for not delivering in a health facility
notinhf<-factor(data$reason.notinhf[data$reason.notinhf!=""])
notinhf<-table(notinhf)
notinhf
round(prop.table(notinhf),2)
rm(notinhf)

#Whether beneficiaries brought newborns for immunization
immun<-factor(data$newborn.immun[data$newborn.immun!=""])
immun<-table(immun)
immun
round(prop.table(immun),2)
barplot(prop.table(immun)*100,
        main='Immunization of Newborns',
        col="lightblue",
        ylim=c(0,100),
        ylab="Percentage",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(immun)

#Where beneficiaries first heard about CCT 
cctinfo<-table(data$nu_cctinfo) 
cctinfo
round(prop.table(cctinfo),2)
barplot(cctinfo, ylim=c(0,140),col="pink", ylab='No. of Beneficiaries')
barplot(prop.table(cctinfo)*100,
        ylim=c(0,50),
        ylab="Percentage",
        main = "Source of Information on CCT",
        col="lightblue",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")

#Chi-square GOF test
infop<-c(1/6,1/6,1/6,1/6,1/6,1/6)
chisq.test(cctinfo,p=infop)
rm(infop, cctinfo)

#Beneficiaries' stage of pregnancy at time of CCT enrolment
ga<-factor(data$preg.stage[data$preg.stage!=""])
ga<-factor(ga,
           levels=c("First trimester","Second trimester","Third trimester","At delivery"),
           ordered = TRUE)
ga<-table(ga)
ga
round(prop.table(ga),2)
barplot(prop.table(ga)*100,
        ylim=c(0,60),
        main='Pregnancy Stage at CCT Enrolment',
        col="lightblue",
        ylab="Percentage",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")

#Chi-squared GOF Test
gap<-c(.25,.25,.25,.25)
chisq.test(ga,p=gap)
rm(ga,gap)

#Whether beneficiaries were given adequate information on their co-responsibilities
vec<-factor(data$adequate.info[data$adequate.info!=""])
vec <- factor(vec, levels = c("Yes", "Fair", "No")) #reordering responses
vec <- table(vec)
vec
round(prop.table(vec),4)*100
barplot(prop.table(vec)*100,
        ylim=c(0,100),
        col="lightblue",
        ylab="Percentage",
        axes=F,
        main="Beneficiaries informed on co-responsibilities")
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(vec)

#Whether the record in beneficiaries' card was the same as that in the register
cardsame<-factor(data$same.record[data$same.record!=""])
cardsame<-factor(cardsame,levels=c("Yes","Some mistakes","No","Don't know")) #reorder responses
cardsame<-table(cardsame)
cardsame
round(prop.table(cardsame),4)*100
barplot(prop.table(cardsame)*100,
        ylim=c(0,100),
        col="lightblue",
        ylab="Percentage",
        xlab="Response",
        axes=F,
        main="Same record in card and register")
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(cardsame)

#Complaints about CCT programme at enrolment
complain<-factor(data$complaints[data$complaints!=""])
complain<-table(complain)
complain
round(prop.table(complain),2)
barplot(prop.table(complain)*100,
        ylim=c(0,100),
        col="lightblue",
        ylab="Percentage",
        xlab="Response",
        main="Complaints about CCT at enrolment",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(complain)

#Whether beneficiaries where provided avenues to make complaints
avenue <- factor(data$complaint.avenue[data$complaint.avenue!=""])
avenue <- table(avenue)
avenue
round(prop.table(avenue),2)
barplot(prop.table(avenue)*100,
        ylab="Pregnancy",
        col="lightblue",
        main="Avenue for Providing Complaints",
        ylim=c(0,60),
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(avenue)

#Perception of enrolment 
process<-factor(data$cct.process[data$cct.process!=""])
process<-factor(process,levels=c("Easy","Not so easy","Difficult","Very difficult"), ordered = TRUE)
process<-table(process)
process
round(prop.table(process),2)
barplot(prop.table(process)*100,
        ylim=c(0,100),
        col="lightblue",
        main='Perception of CCT Enrolment',
        ylab="Percentage",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(process)

#Perception of the entire CCT process
process2<-factor(data$experience[data$experience!=""])
process2<-factor(process2,levels=c("Easy","Not so easy","Difficult","Very difficult"),ordered = TRUE)
process2<-table(process2)
process2
round(prop.table(process2),2)
barplot(prop.table(process2)*100,
        ylim=c(0,100),
        col="lightblue", 
        ylab="Percentage",
        xlab="Response",
        axes=F,
        main="Perception of entire CCT process")
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(process2)

#How beneficiaries received information on pay-out events
payinfo<-factor(data$cct.info[data$cct.info!=""])
payinfo<-table(payinfo)
payinfo
round(prop.table(payinfo),2)
rm(payinfo)

#Timeliness of information of pay-out events
timeinfo<-factor(data$timeliness.info[data$timeliness.info!=""])
timeinfo<-table(timeinfo)
timeinfo
round(prop.table(timeinfo),2)
barplot(prop.table(timeinfo)*100,
        main = "Timely information on Pay-Out Events",
        ylab = "Percentage",
        ylim = c(0,100),
        axes = FALSE,
        col = "lightblue")
axis(2, las = 2)
grid(NA, ny = NULL, lty = "dashed")
rm(timeinfo)

#Whether pay-out events where on schedule
sched <-factor(data$payout.onschedule[data$payout.onschedule!=""])
sched <- factor(sched,levels=c("Yes","No","Don't know"))
sched <- table(sched)
sched
round(prop.table(sched),2)
barplot(prop.table(sched)*100,
        ylim=c(0,100),
        col="lightblue",
        ylab="Percentage",
        main='Reliability of Pay-Out Schedule',
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(sched)

#Number of pay-outs per beneficiary
paynum<-factor(data$no.of.payments[data$no.of.payments!=""], ordered = TRUE)
paynum<-table(paynum)
paynum
round(prop.table(paynum),3)
barplot(prop.table(paynum)*100,
        ylim=c(0,60),
        ylab="Percentage",
        col="lightblue",
        xlab="Payments per Beneficiary",
        main = "Number of Payments Received",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(paynum)

#Did amount paid meet beneficiaries' expectation?
expamt<-factor(data$expected.amount.paid[data$expected.amount.paid!=""])
expamt <- factor(expamt, levels = c("Yes", "No"))
expamt<-table(expamt)
expamt
round(prop.table(expamt),2)
barplot(prop.table(expamt)*100,
        main='Expected Amount Received',
        axes=F,
        ylim=c(0,80),
        col="lightblue",
        ylab="Percentage")
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(expamt)

#Where they given reasons for any non-payment?
givreas<-factor(data$reason.given[data$reason.given!=""])
reastab<-table(givreas)
reastab
round(prop.table(reastab),2)

#State-wise
datax <- data[data$reason.given!="",]
givreas <- factor(datax$reason.given[datax$reason.given!=""])
tab <- table(givreas, datax$state)
barplot(tab,
        beside = TRUE,
        main = "Reasons given for Non-Payment vs. State",
        ylab = "No. of beneficiaries",
        axes = FALSE,
        ylim = c(0,10))
axis(2, las = 2)
grid(NA, ny = NULL, lty = "dashed")

#Chi-squared Test of Independence
chisq.test(tab)
rm(givreas, tab, datax)

#Total amount received by beneficiaries
amount<-factor(data$amount.received[data$amount.received!=""])
amount<-factor(amount,
               levels=c("N1,000",   "N2,000" ,  "N3,000" ,  "N4,000" ,  "N5,000" ),
               ordered = TRUE)
amount<-table(amount)
amount
round(prop.table(amount),2)
barplot(prop.table(amount)*100,
        ylim=c(0,40),
        col="lightblue",
        ylab="Percentage",
        xlab="Amount Paid",
        main="Total Amount Received",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(amount)

#Description of pay-out events
org<-factor(data$payout.organized[data$payout.organized!=""])
org<-factor(org,levels=c("Not organized","Fairly organized","Well organized"))
org <- table(org)
org
round(prop.table(org),2)
barplot(prop.table(org)*100,
        ylim=c(0,70),
        col="lightblue",
        main='Description of Pay-Out Events',
        ylab="Percentages",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")

#Chi-squared GOF Test
orgp<-c(1/3,1/3,1/3)
chisq.test(org,p=orgp)
rm(orgp,org)

#Payment for services at the health facility
payserv <- factor(data$pay.for.services[data$pay.for.services!=""])
payserv <- table(payserv)
payserv
round(prop.table(payserv),2)
barplot(prop.table(payserv)*100,
        ylim=c(0,70),
        col="lightblue",
        ylab="Percentages" ,
        xlab="Response",
        main='Payment for PHC Services',
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")

#Chi-squared GOF Test
payservp<-c(.5,.5)
chisq.test(payserv,p=payservp)
rm(payservp,payserv)

#Payment for Service vs. State
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
grid(NA, ny = NULL, lty = "dashed")

#Chi-squared Test of Independence
chisq.test(tab)
chisq.test(tab)$expected
rm(tab,nu)

#What type of service was paid for
wtpd<-factor(data$service.paid.for[data$service.paid.for!=""])
wtpd <- table(wtpd)
wtpd
round(prop.table(wtpd),2)
barplot (prop.table(wtpd)*100,
         ylim=c(0,70),
         col="lightblue",
         ylab="Percentage",
         axes=F,
         main="Type of Services Paid for")
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")

#Chi-squared GOF Test
pdp<-rep(1/7,7)
chisq.test(wtpd,p=pdp)
rm(wtpd,pdp)

#Husband/partner awareness of their enrolment with CCT
husbaw<-factor(data$husband.aware[data$husband.aware!=""])
husbaw<-table(husbaw)
husbaw
round(prop.table(husbaw),2)
barplot(prop.table(husbaw),
        ylim=c(0,1),
        main="Husband/Partner Awareness of CCT Enrolment",
        col="lightblue",
        ylab="Percentages",
        axes=F)
axis(2,las=2)
grid(NA, ny = NULL, lty = "dashed")
rm(husbaw)

#Husband/partner awareness of the amount received
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
rm(husmoney)

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
rm(feels)

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
rm(helped)

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
rm(how)

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

#Comparing by State, whether received the amount they were expecting
datax<-data[data$expected.amount.paid!="",]
exptpay<-factor(datax$expected.amount.paid[datax$expected.amount.paid!=""])
exptpay<-table(exptpay,datax$state)
exptpay
round(prop.table(exptpay,2),2)
barplot(exptpay,
        beside=T,
        ylim = c(0,50),
        legend=F,
        col=c("black","lightgrey"),
        main="Receipt of Expected Amount in the States",
        ylab="No. of beneficiaries",
        axes=F)
axis(2,las=2)
legend(legend=c("No","Yes"),
       "topright",
       fill=c("black","lightgrey"),
       cex=.8)
#Chi-squared Test of Independence
chisq.test(exptpay,simulate.p.value=TRUE)
chisq.test(exptpay,simulate.p.value=TRUE)$expected
chisq.test(exptpay,correct=T)
rm(exptpay)

#Beneficiaries' religion in the FCT
fct <- data[data$state=="FCT",]
summary(fct$religion)
tab
round(prop.table(table(fct$religion)),2)
barplot(prop.table(table(fct$religion)),
        ylim = c(0,.6),
        col = "lightblue",
        main = "Beneficiary Religion in the FCT",
        ylab = "Proportion of beneficiaries",
        axes = F)
axis(2, las = 2)
rm(fct)

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

#TRIVIA
#No. of Interviews per consultant
table(data$interviewer)
plot(data$interviewer,
     main = "Consultants' Interviews",
     ylab = "No. of beneficiaries",
     ylim = c(0,60))

rm(data)
#END


