#small simulation for ppv over several generations of results
#first stage is a single experiment
library(tidyverse)
library(dplyr)

sd_per_group<-1
sample_size<-10
number_of_exp<-10
true_mean_diff<-rbeta(number_of_exp,1,5)
#hist(true_mean_diff)

results_vec<-list()
for (i in 1:number_of_exp)
  results_vec[[i]]<-t.test(rnorm(sample_size,0,sd_per_group),rnorm(sample_size,true_mean_diff[i],sd_per_group))

#now a second exact replication
#sample size based on estimated sample size
res_summary<-data.frame(p_value=as.numeric(map(results_vec,"p.value")),
                        x_value=as.numeric(map(map(results_vec,"estimate"),"mean of x")),
                        y_value=as.numeric(map(map(results_vec,"estimate"),"mean of y")))
                        #stderror=as.numeric(map(results_vec,"stderr")))
results_vec

res_summary<-res_summary %>% 
  mutate(mean_difference=y_value-x_value)

sample_size_estimate<-rep(0,number_of_exp)
for (i in 1:number_of_exp){
  sample_size_estimate[i]<-ceiling(power.t.test(delta=res_summary$mean_difference[i],sd=1,sig.level=0.05,power=.8)$n)
}

plot(sample_size_estimate[res_summary$p_value<0.05]~true_mean_diff[res_summary$p_value<0.05])

#conduct the experiment
exp_to_repeat<-which(res_summary$p_value<.05)
results_vec<-list()
j<-0
for (i in exp_to_repeat){
  j<-j+1
  results_vec[[j]]<-t.test(rnorm(sample_size_estimate[i],0,sd_per_group),rnorm(sample_size_estimate[i],true_mean_diff[i],sd_per_group))
}
res_summary<-data.frame(p_value=as.numeric(map(results_vec,"p.value")),
                        x_value=as.numeric(map(map(results_vec,"estimate"),"mean of x")),
                        y_value=as.numeric(map(map(results_vec,"estimate"),"mean of y")))
                        #stderror=as.numeric(map(results_vec,"stderr")))



res_summary<-res_summary %>% 
  mutate(mean_difference=y_value-x_value)

hist(res_summary$mean_difference)
hist(true_mean_diff[exp_to_repeat])
plot(res_summary$mean_difference~true_mean_diff[exp_to_repeat])

