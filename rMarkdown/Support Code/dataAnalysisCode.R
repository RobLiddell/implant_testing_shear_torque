####Libraries####
library(MASS)
library(regtools)
library(tidymodels)
library(tidyverse)
library(glue)
library(lubridate)
library(kableExtra)

####Data Loading and Formatting####
dataPath<-"Data/formattedData/ST1_Data_Cleaned.txt"

data<-read_tsv(dataPath,col_types = list(col_character(),
                                         col_factor(levels=c("Right","Left")),
                                         col_factor(levels=c("Lateral","Medial")),
                                         col_double(),
                                         col_factor(levels=c("Tension","Shear")),
                                         col_factor(levels=c("BAE","BAE+DCD")),
                                         col_double()),
               lazy=FALSE)


#Data setup and creation of dummy variables
simpDat<-data %>% 
  pivot_wider(id_cols = c(animalID,testMethod,time,implantType),
              names_from = c(archSide),
              values_from = force) %>% 
  filter(!is.na(Medial)|!is.na(Lateral)) %>% 
  # drop_na() %>% 
  mutate(Medial= if_else(is.na(Medial),Lateral,Medial),
         Lateral= if_else(is.na(Lateral),Medial,Lateral),
         force=(Medial+Lateral)/2, .keep="unused") %>% 
  select(animalID,implantType,testMethod,time,force)



nlsDat<-simpDat %>% 
  mutate(tBin=if_else(testMethod=="Tension",1,0),
         nBin=if_else(implantType=="BAE+DCD",1,0))


####Functions####

##Automated asymptotic model fitting function
#Define model to fit
asymptoticNLS <- function(dat, cTermsNum=3, tauTermsNum=3, dTermsNum=1,
                          startingValues=list(Tau = 3.1,Tt=0,Tn=-0.5,Ttn=-0.5, C=25,Ct=10,Cn=0,Ctn=0,D=2)) {
  
  #Choose starting values for function
  startingValues<-startingValues[c(1:tauTermsNum,4+(1:cTermsNum),8+(1:dTermsNum))]
  
  #Define the terms to include in the function
  cTerms<-c("C","Ct*tBin","Cn*nBin","Ctn*tBin*nBin")[1:cTermsNum] %>% 
    paste0(collapse = "+")
  
  tauTerms<-c("Tau","Tt*tBin","Tn*nBin","Ttn*tBin*nBin")[1:tauTermsNum] %>% 
    paste0(collapse = "+")
  
  dTerms<-c("D")[1:dTermsNum] %>% 
    paste0(collapse = "+")
  
  #Generate the formula to use in the nls function
  nlsFormula<-formula(str_glue("force~({cTerms})*(1-exp(-(time-{dTerms})/exp({tauTerms})))"))
  
  #nls function is in a try statement in case the fit fails at the starting values provided
  fit<-try(nls(nlsFormula, dat,
               start = startingValues,
               weights = dat%>%group_by(time,nBin,tBin) %>% mutate(wts=1/(sd(force)^2)) %>% pull(wts), 
               control = nls.control(maxiter = 100, minFactor = 1/5000, warnOnly = TRUE),
               algorithm = 'port' ),
           silent = TRUE)
  if(inherits(fit,"try-error")){
    return(NA)
  }else{
    return(fit)
  }
  
}



####Data Summary####
groupCounts<-simpDat%>%
  group_by(testMethod,time,implantType) %>%
  summarise(Count=n(),.groups = "drop") %>% 
  pivot_wider(id_cols = c(testMethod,implantType),
              names_from = time,
              values_from = Count,
              names_glue = "Day {.name}")

####Example NLS analysis with true data####
if(FALSE){
  nlsDat%>%
    asymptoticNLS(cTermsNum=4, tauTermsNum=4) %>% 
    tidy() %>% 
    mutate(upperCI=estimate+std.error*1.96,
           lowerCI=estimate-std.error*1.96) 
}

####Bootstrapping NLS For Comparisons####
nSim=100
set.seed(27)

#Create Bootstraps
bootStraps<-tibble(trial=c(1:nSim)) %>%
  rowwise() %>%
  mutate(trialData=list(nlsDat %>%
                         group_by(testMethod,implantType,time) %>%
                         slice_sample(prop=1,replace=TRUE))) %>%
  ungroup() %>%
  mutate(model = map(trialData, asymptoticNLS,cTermsNum=4, tauTermsNum=4))%>% 
  rowwise() %>% 
  mutate(keep=is_logical(unlist(model))) %>% 
  filter(!keep) %>% 
  select(-keep) %>% 
  ungroup() %>% 
  mutate(augmented=map(model,augment)) %>% 
  mutate(coef_info=map(model,tidy)) %>% 
  select(-model,-trialData)

bootStrapResults <- bootStraps %>% 
  select(-augmented) %>% 
  unnest(coef_info) %>% 
  group_by(term) %>% 
  summarise(Estimate=mean(estimate),
            Std.Err=sd(estimate),
            CIlower=quantile(estimate,0.025),
            CIupper=quantile(estimate,0.975))

########
