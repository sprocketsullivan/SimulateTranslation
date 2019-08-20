#Simulation for a simplified preclinical research chain
# We assume that we are interested in one primary outcome and only have two groups (control treatment) to compare
#Stage 1: Exploratory Study
#in this stage a study is conducted with
#1. a true effect size (mean difference between two groups): ES_true
#2. a sample size per group: sample_size
#3. a systematic lab bias when estimating the effect size: l_bias
#4. a population standard deviation: pop_sd
#This will result in an initial sample and some derived meaures
library(tidyverse)

generate_study<- function(ES_true=1,sample_size=10,l_bias=0,pop_sd=1){
  ES_mod<-ES_true+l_bias
  #print(sample_size)
  sample_data<-data.frame(values=c(rnorm(sample_size,0,pop_sd),rnorm(sample_size,ES_mod,pop_sd)),treatment=rep(c("control","treat"),each=sample_size))
  return(sample_data)
}

#get some info on effect size and p_value from an exploratory study
get_summary_study<- function(study_data,confidence=.7){
  study_summary<-
    study_data %>% 
    group_by(treatment) %>% 
    summarise(mean_effect=mean(values),
              sd_effect=sd(values)) %>% 
    mutate(p_value=t.test(study_data$values~study_data$treatment)$p.value,
           CI_80=((t.test(study_data$values~study_data$treatment,conf.level=confidence)$conf.int)))
}
#based on such a study calculate the sample size needed for an additional study
calc_sample_size<-function(study_summary,study_data,max_sample_size=100,alpha=.05,power=.8){
  aa<-study_summary
  #if(aa$CI_80[1]*aa$CI_80[2]<0) {
   # return(0)
    #}  
  es_measured<-min(abs(aa$CI_80))
    #es_measured<-aa$mean_effect[2]-aa$mean_effect[1]
  sample_size<-nrow(study_data)/2
  sd_measured<-sqrt((aa$sd_effect[1]^2+aa$sd_effect[2]^2)/2)
  bb<-power.t.test(delta=es_measured,sd=sd_measured,sig.level=alpha,power=.8)
  if(es_measured>0){
  if(bb$n>max_sample_size) return(max_sample_size) else return(bb$n)
  }
}



