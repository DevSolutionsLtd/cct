# cleaning.R

library(tidyverse)
source("scripts/clean-funs.R")

data <-
  read.csv('data/surveydata.csv',
           stringsAsFactors = FALSE,
           na.strings = c("", "-"))

## Some columns not useful to the analysis
data <- data %>%
  select(-c(ques.no, cct.no, contact.no, name)) 

data$interviewer <- data$interviewer %>% as_factor()
data$date <- as.Date(data$date, format = "%d/%m/%Y")
data$state <- data$state %>%
  as_factor()
data$lga <- data$lga %>% 
  as_factor()
data$ward <- data$ward %>% 
  str_replace_all("^iyi", "Iyi") %>% 
  str_replace("Sade East D|SadeD", "Sade East/ D") %>% 
  str_replace("Yna|Yana", "Yena") %>% 
  as_factor()
data$facility <- data$facility %>% 
  str_replace("fari", "Fari") %>%       # Is N/Mailayi a PHC?
  as_factor()
data$age <- data$age %>% 
  set_ord_fac(c("15 - 25 years", "26 - 35 years", "36 - 45 years"))
data$religion <- data$religion %>% 
  as_factor()
data$ethnicity <- data$ethnicity %>% 
  str_replace("\\(Gwari\\)", "") %>% 
  str_replace("east$", "East") %>% 
  str_trim() %>% 
  as_factor()
data$marital.status <- data$marital.status %>% 
  factor(c("Married", "Single", "Separated", "Widowed"))
data$literacy.eng <- set_yn(data$literacy.eng)
data$literacy.hau <- set_yn(data$literacy.hau)
data$literacy.ibo <- set_yn(data$literacy.ibo)
data$literacy.yor <- set_yn(data$literacy.yor)
data$literacy.pid <- set_yn(data$literacy.pid)
data$literacyoth <- data$literacyoth %>% 
  str_replace("Gbagi", "Gbagyi") %>% 
  str_replace("Others", NA_character_)
data$occupation <- data$occupation %>% 
  str_replace("private", "Private") %>% 
  str_replace("Serv", "serv") %>% 
  str_trim() %>% 
  as_factor() %>% 
  fct_infreq()

inc.brackets <- c(
      "Less than N900",
      "N1,000 - N10,000",
      "N11,000 - N20,000",
      "N21,000 - N30,000",
      "N31,000 - N40,000",
      "N41,000 - N50,000",
      "N51,000 and above",
      "Don’t know"
    )

data$family.income <- data$family.income %>%
  factor(inc.brackets)
data$personal.income <- data$personal.income %>% 
  factor(inc.brackets[-8])
data$reason.no.anc <- data$reason.no.anc %>% 
  str_replace("(PHC)(.+$)", "\\1") %>% 
  str_replace("(know)(\\sthe)", "\\1") %>% 
  str_trim() %>% 
  as_factor() %>% 
  fct_infreq()
data$outside.delivery <- data$outside.delivery %>% 
  str_replace("By Her", "by her") %>% 
  str_replace("the\\s", "") %>% 
  as_factor() %>% 
  fct_infreq()
data$hf.type <- data$hf.type %>%
  str_replace("hos", "Hos") %>%
  factor(c(
    "Primary Health Centre",
    "General Hospital",
    "Specialist Hospital"
  ))
data$reason.notinhf <- data$reason.notinhf %>% 
  str_trim() %>% 
  as_factor() %>% 
  fct_infreq()
data$newborn.immun <- set_yn(data$newborn.immun)
data$reason.notimmun <- data$reason.notimmun %>% 
  as_factor() %>% 
  fct_infreq()
data$reason.notimmun <- data$reason.notimmun %>% 
  str_replace("(PHC)(.+$)", "\\1") %>% 
  str_replace("(know)(\\sthe)", "\\1") %>% 
  as_factor() %>% 
  fct_infreq()
data$info.cct <- data$info.cct %>% 
  str_replace("^hea", "Hea") %>% 
  str_replace("Lead", "lead") %>% 
  str_replace("leaders", "leader") %>% 
  str_replace("comm", "Comm") %>% 
  str_replace("(^Family)(.+$)", "\\1/Friend") %>% 
  as_factor() %>% 
  fct_infreq()
data$preg.stage <- data$preg.stage %>%
  set_ord_fac(c(
    "First trimester",
    "Second trimester",
    "Third trimester",
    "At delivery"
  ))
data$adequate.info <- data$adequate.info %>% 
  set_ord_fac(c("Yes", "Fair", "No"))
data$info.coresp <- data$info.coresp %>% 
  str_replace("my\\s", "") %>% 
  set_ord_fac(c("At 2nd visit", "At 3rd visit", "At delivery", "None"))
data$same.record <- data$same.record %>% 
  factor(c("Yes", "No", "Some mistakes", "Don't know"))
data$complaints <- data$complaints %>%
  set_yn()
data$complaint.avenue <- data$complaint.avenue %>% 
  set_yn()
data$measures.taken <- data$measures.taken %>%
  factor(c("Yes", "No", "No response"))
data$cct.process <- set_ease(data$cct.process)
data$experience <- set_ease(data$experience)
data$cct.info <- data$cct.info %>% 
  str_replace("Call$", "call") %>% 
  str_replace("Leaders", "leaders") %>% 
  as_factor() %>% 
  fct_infreq()
data$no.of.payments <- data$no.of.payments %>% 
  str_replace("None", NA_character_) %>% 
  set_ord_fac(c("Once", "Twice", "Thrice", "Four times"))
data$timeliness.info <- set_yn(data$timeliness.info)
data$payout.onschedule <- 
  factor(data$payout.onschedule, c("Yes", "No", "Don't know"))

ynd <- c("Yes", "No", "No data")

data$expected.amount.paid <- 
  factor(data$expected.amount.paid, levels = ynd)
data$reason.given <- 
  factor(data$reason.given, levels = ynd)
data$amount.received <- data$amount.received %>% 
  str_trim() %>% 
  str_replace("(^N)([1-5])(,)(000$)", "\\2\\4") %>% 
  str_replace("Not paid", "Nil") %>% 
  set_ord_fac(c("5000", "4000", "3000", "2000", "1000", "Nil"))
data$payout.organized <- data$payout.organized %>% 
  str_replace("z", "s") %>% 
  factor(c("Well organised", "Fairly organised", "Not organised"))
data$pay.for.services <- set_yn(data$pay.for.services)
data$service.paid.for <- data$service.paid.for %>% 
  as_factor() %>% 
  fct_infreq()
data$husband.aware <- set_yn(data$husband.aware)
data$husband.know.amount <- set_yn(data$husband.know.amount)
data$husband.feels <- data$husband.feels %>% 
  set_ord_fac(c("Not satisfied", "Fairly satisfied", "Satisfied"))
data$incentive.help <- set_yn(data$incentive.help)
data$help.how <- data$help.how %>%
  as_factor() %>% 
  fct_infreq()

## Save to disk
saveRDS(data, "data/surveydata.rds")
