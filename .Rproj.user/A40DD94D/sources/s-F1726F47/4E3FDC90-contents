safeguard.d <- function(d, n.1, n.2, sig.level = .05, power = .8, conf = .80)
{
  require(pwr)
  require(MBESS)
  if (d >= 0) alternative = "greater" else alternative = "less"
  if (conf < .5 | conf >= 1) stop ("The parameter conf must be in the interval .5 < conf < 1")
  conf.level <- 2*conf-1
  d.sg <- ci.smd(smd = d, n.1 = n.1, n.2 = n.2, conf.level = conf.level)$Lower.Conf.Limit.smd
  if(sign(d*d.sg) == -1)
  {
    warning("the safeguard d is negative: the safeguard sample size is therefore set to Inf")
    return (c("Lower_d" = d.sg, "N_required" = Inf, "SSR" = Inf))
  }
  pw <- tryCatch(pwr.t.test(n = NULL, d = d.sg, sig.level = sig.level, power = power, type = "two.sample", alternative = alternative),
                 error = {function(err)
                 {return(NA)}
                 })
  if(!all(is.na(pw))) N_required = ceiling(pw$n)*2
  # to prevent crashes
  if(all(is.na(pw)) & d.sg >= .5)
  {
    N_required <- 2
    warning("the value of d is so high that it was not possible to compute a precise value of sample size. The sample size was therefore rounded to 2")
  }
  if(all(is.na(pw)) & d.sg < .5)
  {
    warning("the safeguard d is too small: the safeguard sample size could not be computed exactly and is therefore set to Inf")
    return (c("Lower_d" = d.sg, "N_required" = Inf, "SSR" = Inf))
  }
  SSR = N_required/(n.1+n.2)
  c("Lower_d" = d.sg, "N_required" = N_required, "SSR" = SSR)
}
