

SESOI <- c(.3, .5, .7, 1)

for (i in SESOI) {
  
  n_per_group <-
    ceiling(power.t.test(n = NULL, delta = i	, sd = 1, sig.level = .05, power = .5,
                         type = "two.sample",
                         alternative = "one.sided")$n)
  
  
  ES_power_80 <-
    round(power.t.test(n = n_per_group, delta = NULL	, sd = 1, sig.level = .05, power = .8,
                       type = "two.sample",
                       alternative = "one.sided")$delta, 2)
  
  print(c(i, n_per_group, ES_power_80))
  
}


