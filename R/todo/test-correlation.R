# Figure out correlation heuristics.
# TODO: is it worth creating a vignette here...?
means = seq(0.1,10.1,0.5)
sds = seq(0.1,5.1,0.5)

out = tibble::tibble(
  mean = numeric(),
  sd = numeric(),
  correlation = numeric()
)

for(mean in means) {
  for(sd in sds) {

    shape = mean^2/sd^2
    rate = mean/sd^2

    samples = rgamma(100,shape,rate = rate)
    data = tibble::tibble(left = floor(samples), right=ceiling(samples))
    try({
      # browser()
      tmp = fitdistrplus::fitdistcens(as.data.frame(data),distr = "gamma")
      tmp2 = fitdistrplus::bootdistcens(tmp,niter = 100)
      params = tmp2$estim
      params = params %>% mutate(mean = shape/rate, sd = sqrt(shape/rate^2))
      out.row = tibble::tibble(
        mean = mean,
        sd = sd,
        mu = log(mean),
        sigma = log(sd),
        mean_of_mean = mean(params$mean),
        mean_of_sd = mean(params$sd),
        correlation = cor(log(params$mean), log(params$sd))
      )

      out = bind_rows(out, out.row)
    },silent = TRUE)

    message(".",appendLF = FALSE)

  }
}

ggplot(out,aes(x=mu,y=sigma,fill=correlation))+geom_tile()

# relationship between mu, sigma and correlation
ggplot(out,aes(x=mu,y=correlation))+geom_point()+geom_smooth(method = "lm")
ggplot(out,aes(x=sigma,y=correlation))+geom_point()+geom_smooth(method = "lm")

model = lm(correlation ~ mu*sigma, out)
equatiomatic::extract_eq(model,use_coefs = TRUE)

# quality of fit.
ggplot(out,aes(x=mean,y=mean_of_mean, colour=sd))+geom_point()+geom_abline()
ggplot(out,aes(x=sd,y=mean_of_sd, colour=mean))+geom_point()+geom_abline()

