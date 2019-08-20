#simulation for two stage design of preclinical studies 
#third stage multi center to be added later

#first stage
#conduct an experiment with low sample size
#generate 1000 effect sizes
number_of_exp<-1000
ES_true<-rbeta(number_of_exp,1,5)
hist(ES_true)
source("functions_for_sim.R")
exploratory_stage<-list()
for (i in 1:number_of_exp){
  exploratory_stage[[i]]<-generate_study(ES_true = ES_true[i])#sample size is 10 per group
}
exploratory_stage_summary<-list()
for(i in 1:number_of_exp){
  exploratory_stage_summary[[i]]<-get_summary_study(exploratory_stage[[i]],confidence=.8)
}
new_sample_size<-rep(0,number_of_exp)
for (i in 1:number_of_exp)
  new_sample_size[i]<-ceiling(calc_sample_size(exploratory_stage_summary[[i]],exploratory_stage[[i]],max_sample_size=100,power=.8))
  #new_sample_size[i]<-ceiling(power.t.test(delta=.5,sd=1,power=.8)$n)
#show the new sample sizes
hist(new_sample_size)
#what were the ES of the very large replications?
hist(ES_true[which(new_sample_size==0)])
length(which(new_sample_size==0&ES_true>.3))
length(which(new_sample_size>0&ES_true<.3))
sum(ES_true>.3)
#now generate a replication stage
replication_stage<-list()
for (i in 1:number_of_exp){
  replication_stage[[i]]<-generate_study(ES_true = ES_true[i],sample_size = new_sample_size[i])
}
replication_stage_summary<-list()
for(i in 1:number_of_exp){
  replication_stage_summary[[i]]<-get_summary_study(replication_stage[[i]])
}

res_summary<-data.frame(p_value=unlist(map(replication_stage_summary,"p_value"))[seq(1,2*number_of_exp,2)],
                        mean_control=unlist(map(replication_stage_summary,"mean_effect"))[seq(1,2*number_of_exp,2)],
                        mean_treatment=unlist(map(replication_stage_summary,"mean_effect"))[seq(2,2*number_of_exp,2)],
                        ES_true=ES_true,
                        sample_size=new_sample_size
                        )
res_summary<-res_summary %>% 
  mutate(mean_difference=mean_treatment-mean_control)
ggplot(aes(y=(mean_difference),x=ES_true,col=p_value<.05),data=res_summary)+geom_point()
m.1<-lm(mean_difference~ES_true,data=res_summary)
plot(residuals(m.1)~new_sample_size)


