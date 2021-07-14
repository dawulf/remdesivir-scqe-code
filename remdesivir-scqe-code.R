library(tidyverse)
library(xtable)
library(sensemakr)
library(scqe)
library(KRLS)
library(xgboost)
library(caret)

### Our outcome is mortality within Y days, and our treatment must be given within D days. Set these values here.
Ydays <- 28
Ddays <- 7
Rdays <- 1e6

dat.full <- read.csv("EHR_data_COVID_remdes.csv")


### Data prep code

dat = dat.full %>% mutate(
  ICU_24hrs = ifelse(is.na(hrstoICU), 
                     0, as.numeric(hrstoICU < 24)),            
  vent_24hrs = ifelse(is.na(hrstovent), 
                      0, as.numeric(hrstovent < 24)),
  death_24hrs = ifelse(is.na(hrstodeath),
                       0, as.numeric(hrstodeath < 24)),
  o2_flowrate_24hrs = ifelse(as.numeric(hrstoOXYFLOW < 24),
                             FlowsheetValue_R_OXYGEN_FLOW_RATE, NA),
  o2_miss_24hrs = is.na(o2_flowrate_24hrs),
  spo2_24hrs = ifelse(as.numeric(hrstoPULSE < 24),
                      FlowsheetValue_PULSE_OXIMETRY, NA),
  spo2_miss_24hrs = is.na(spo2_24hrs),
  CRP_24hrs = ifelse(as.numeric(hrstoCRP < 24),
                     CRP, NA),
  CRP_miss_24hrs = is.na(CRP_24hrs),
  WBC_24hrs = ifelse(as.numeric(hrstoWBC < 24),
                     WBC, NA),
  WBC_miss_24hrs = is.na(WBC_24hrs),
  ANC_24hrs = ifelse(as.numeric(hrstoANC < 24),
                     ANC, NA),
  ANC_miss_24hrs = is.na(ANC_24hrs),     
  ALC_24hrs = ifelse(as.numeric(hrstoALC < 24),
                     ALC, NA),
  ALC_miss_24hrs = is.na(ALC_24hrs),    
  Creat_24hrs = ifelse(as.numeric(hrstoCreatinine < 24),
                       Creatinine, NA),
  Creat_miss_24hrs = is.na(Creat_24hrs),
  
  
  remdesivir_Rdays = ifelse(is.na(hrstoremdesivir), 
                            0, as.numeric(hrstoremdesivir < 24*Rdays)),
  cq_Ddays = ifelse(is.na(hrstochloroquine), 
                    0, as.numeric(hrstochloroquine < 24*Ddays)),
  heparin_Ddays = ifelse(is.na(hrstoheparin), 
                         0, as.numeric(hrstoheparin < 24*Ddays)),
  prone_Ddays = ifelse(is.na(hrstoprone), 
                       0, as.numeric(hrstoprone < 24*Ddays)),
  azc_Ddays = ifelse(is.na(hrstoazithromycin), 
                     0, as.numeric(hrstoazithromycin < 24*Ddays)),
  dexamethasone_Ddays = ifelse(is.na(hrstodexamethasone), 
                               0, as.numeric(hrstodexamethasone < 24*Ddays)),
  prednisone_Ddays = ifelse(is.na(hrstoprednisone), 
                            0, as.numeric(hrstoprednisone < 24*Ddays)),
  tocilizumab_Ddays = ifelse(is.na(hrstotocilizumab), 
                             0, as.numeric(hrstotocilizumab < 24*Ddays)),
  ceftriaxone_Ddays = ifelse(is.na(hrstoceftriaxone), 
                             0, as.numeric(hrstoceftriaxone < 24*Ddays)),
  enoxaprin_Ddays = ifelse(is.na(hrstotoenoxaprin), 
                           0, as.numeric(hrstotoenoxaprin < 24*Ddays)),
  hydrocortisone_Ddays = ifelse(is.na(hrstohydrocortisone), 
                                0, as.numeric(hrstohydrocortisone < 24*Ddays)),
  HCQ_Ddays = ifelse(is.na(hrstohydroxychloroquine), 
                     0, as.numeric(hrstohydroxychloroquine < 24*Ddays)),
  leronlimab_Ddays = ifelse(is.na(hrstoleronlimab), 
                            0, as.numeric(hrstoleronlimab < 24*Ddays)),
  lovenox_Ddays = ifelse(is.na(hrstotolovenox), 
                         0, as.numeric(hrstolovenox < 24*Ddays)),
  methylpred_Ddays = ifelse(is.na(hrstomethylprednisolone), 
                            0, as.numeric(hrstomethylprednisolone < 24*Ddays)),
  plasma_Ddays = ifelse(is.na(hrstoplasma), 
                        0, as.numeric(hrstoplasma < 24*Ddays)),
  sarilumab_Ddays = ifelse(is.na(hrstosarilumab), 
                           0, as.numeric(hrstosarilumab < 24*Ddays)),
  otherCS_Ddays = pmax(prednisone_Ddays, methylpred_Ddays, hydrocortisone_Ddays),
  
  highuse=as.numeric(daycount<=30 | daycount>=75),)

# 1 if deceased w/in Y days, 0 if not
dat$deceased_Ydays <- dat$death*(dat$hrsinhospital < 24*Ydays) 
dat$deceased_Ydays[is.na(dat$deceased_Ydays)] <- 0

dat <- dat[dat$daycount < 110,] 


### SCQE-specific code

deltas=seq(from=-0.30, to=0.05, length.out=15)
scqe.out = scqe(outcome=dat$deceased_Ydays, post=as.numeric(dat$highuse), 
                treatment=dat$remdesivir_Rdays, delta=deltas)
summary(scqe.out)
plot(scqe.out)



### Delta modeling code

dat.xgb = na.omit(dat[, #none were omitted with these chosen variables
                      c("deceased_Ydays","highuse","daycount","remdesivir_Rdays",
                        "Age","Male","white","latinx",
                        "diabetes","asthma","copd","chd","hypertension",
                        "prednisone_Ddays","methylpred_Ddays","hydrocortisone_Ddays",
                        "heparin_Ddays","enoxaprin_Ddays","HCQ_Ddays","azc_Ddays","prone_Ddays",
                        "ceftriaxone_Ddays","leronlimab_Ddays","sarilumab_Ddays","tocilizumab_Ddays",
                        "spo2_24hrs","WBC_24hrs", "o2_flowimpute_24hrs", "o2_flowimpute_square_24hrs",
                        "CRP_miss_24hrs", "Creat_miss_24hrs", "o2_miss_24hrs",
                        "ICU_24hrs","vent_24hrs","death_24hrs",
                        "cq_Ddays", "dexamethasone_Ddays", "IsEDVisit"
                      )])
dat.xgb.untr <- dat.xgb[dat.xgb$remdesivir_Rdays==0,c(-2,-3,-4)]

fitControl <- trainControl(method = "repeatedcv", number = 5, 
                           repeats = 3, search = "grid")
xgbGrid <- expand.grid(eta = c(0.1,0.2,0.3,0.4), 
                       max_depth = c(2,3,4,5),
                       gamma = c(0.2,0.3,0.4,0.5),
                       nrounds = c(200), 
                       subsample = 1, 
                       colsample_bytree = c(0.8,1),
                       min_child_weight = 1)
xgbGrid <- expand.grid(eta = c(0.3), max_depth = c(2), gamma = c(0.3), nrounds = c(200), 
                       subsample = 1, colsample_bytree = c(1), min_child_weight = 1)
untreated_xgb_cv <- train(deceased_Ydays ~ ., data = dat.xgb.untr, method = "xgbTree", 
                          trControl = fitControl, tuneGrid = xgbGrid)
pred_xgb_cv <- predict(untreated_xgb_cv, dat.xgb[,c(-1,-2,-3,-4)])
mean(pred_xgb_cv[dat.xgb$highuse==1])-mean(pred_xgb_cv[dat.xgb$highuse==0])



### covariate adjustment comparison

dat.post.full = na.omit(dat[dat$highuse==1, #again, none omitted
                            c("deceased_Ydays","remdesivir_Rdays",
                              "Age","diabetes","hypertension",
                              "ICU_24hrs","vent_24hrs"
                            )])
lm.post.selected = lm(deceased_Ydays ~ remdesivir_Rdays + 
                        Age + ICU_24hrs + vent_24hrs + diabetes + hypertension,
                      data=dat.post.full)
summary(lm.post.selected)