ES_true=1
sample_size=10
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

power.t.test(n = NULL, delta=0.8211451,sd=1,sig.level=.05,power=.8)


test <- safeguard.d(d = -0.358221828	 ,
            n.1 = 10, n.2 = 10,
            sig.level = .05, power = .8, conf = 0.8)
