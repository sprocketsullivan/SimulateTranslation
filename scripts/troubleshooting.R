
power.t.test(n = 7, delta = 1.14, sd = 1, sig.level = .05, power = NULL,
             type = "two.sample",
             alternative = "two.sided")

ceiling(power.t.test(n = NULL, delta = .3	, sd = 1, sig.level = .05, power = .5,
             type = "two.sample",
             alternative = "one.sided")$n)


round(power.t.test(n = 61, delta = NULL	, sd = 1, sig.level = .05, power = .8,
             type = "two.sample",
             alternative = "one.sided")$delta, 2)


# For example, if an original study used 20 participants per group, 
# the smallest effect size of interest would be d = 0.49
# (which is the effect size they had 33% power to detect with n = 20). 


abs(aa$mean_effect[2] - aa$mean_effect[1]) > 16

ceiling(power.t.test(n = NULL, delta = 1.4190	, sd = 1, sig.level = .05, power = .8,
                     type = "two.sample",
                     alternative = "one.sided")$n)


ES_true=1
sample_size=1
l_bias=0
pop_sd=1
ES_mod<-ES_true+l_bias


sample_data<-
  data.frame(values=c(rnorm(sample_size,0,pop_sd),
                      rnorm(sample_size,ES_mod,pop_sd)),
             treatment=rep(c("control","treat"),each=sample_size))

t1 <- t.test(values ~ treatment, data = sample_data, paired = FALSE, 
             alternative = "greater", conf.level =.8)

t1$conf.int[1]

exp_data_summary<-get_summary_study(study_data = sample_data, confidence = .8)

aa<-exp_data_summary

es_measured <- min(abs(aa$CI))
es_measured <-abs(aa$mean_effect[2]-aa$mean_effect[1])
es_measured <-.3

power.t.test(n = NULL, delta = 2, sd=1,sig.level=.05,power=.8,
             type = "two.sample", 
             alternative = "two.sided")


test <- safeguard.d(d = 0.89	 ,
            n.1 = 10, n.2 = 10,
            sig.level = .05, power = .8, conf = 0.8)

as.numeric(test[2])/2 > 200
