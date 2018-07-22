## Sandbox
getwd()

data$age
sum(data$age)
table(data$age)
realage<-data$age[data$age!='-']
realage
count(realage)
levels(realage)
table(realage)

is.na(data$age)
data<-read.csv('surveydata.csv')

colnames(data)

  ?hist
?table
table(data$reason.notinfh)
colnames (data)


vec = c("red","blue","red","green","green","yellow","orange")


vec<-data$adequate.info
factor(vec)
levels(vec)
fac2<-factor(vec,levels=c("Yes","Fair","No", ""))
fac2
levels(fac2)
newvar<-table(fac2)
barplot(newvar)

foo<-seq(1,100,by=2)
foo
foo.squared<-NULL
for(i in 1:50){
  foo.squared[i]<-foo[i]^2

}

?attach
??exclude
?prop.table
m<-matrix(1:7,2)
m
prop.table(m,2)

?chisq.test
devtools::install_github("gforge/Grmd")
install.packages("rmarkdown")

#match()
data$facility

state<-factor(data$state)
state
help(legend)
state=="FCT"
state=="Ogun"

s<-split(data$child.no,data$state)
s



data2<-data[data$amount.received!="",]
data3<-data2[data2$no.of.payments!="0",]
attach(data3)
newpay<-factor(amount.received[amount.received!=""])
tabpay<-table(newpay,no.of.payments)
tabpay
prop.table(tabpay)
barplot(tabpay, ylim=c(0,70),beside=TRUE,col=c("red","black","yellow","blue", "green"))
legend("topright",title="Amt Pd",fill=c("red","black","yellow","blue", "green"),c("N1,000","N2,000","N3,000","N4,000","N5,000"))
detach(data3)

par()
cpar<-par( )
 