#r packahe
args <- commandArgs(trailingOnly = TRUE) # take command argument
seed_index = as.numeric(args[1]) # passing first argument(seed index)
print(seed_index)

load(file = paste0("./seeds.rda"))
library(ebdm)
seed = seeds[seed_index]
set.seed(seed)
print(runif(1, min = 0, max = 1))

setting_index <- as.numeric(args[2])
print(setting_index)

#settings

settings <- list(
  list(ni_around = 100, ni_low = 100, ni_high = 200, p11_true = 0.25, p1_true = 0.6, p2_true = 0.35),
  list(ni_around = 100, ni_low = 100, ni_high = 200, p11_true = 0.75, p1_true = 0.77, p2_true = 0.8),
  list(ni_around = 1000, ni_low = 800, ni_high = 1000, p11_true = 0.25, p1_true = 0.6, p2_true = 0.35),
  list(ni_around = 1000, ni_low = 800, ni_high = 1000, p11_true = 0.75, p1_true = 0.77, p2_true = 0.8)
)

# select setting
current_setting <- settings[[setting_index]]
ni_around <- current_setting$ni_around
ni_low<- current_setting$ni_low
ni_high <- current_setting$ni_high
p11_true <- current_setting$p11_true
p1_true <- current_setting$p1_true
p2_true <- current_setting$p2_true

################################################################################
# simulation
################################################################################
sam <- function(p1,p2,p11,ni){
  
  a = p11
  b = p1
  c = p2 + p1 -p11
  
  sum_11 = 0
  sum_12 = 0
  sum_21 = 0
  sum_22 = 0
  
  for (n in 1:ni){
    rr = runif(1)
    if (rr < a){
      sum_11 = sum_11 + 1
    }else if (rr < b){
      sum_12 = sum_12 + 1
    }else if (rr < c){
      sum_21 = sum_21 + 1
    }else {
      sum_22 = sum_22 + 1
    }
  }
  sum_1 = sum_11 + sum_12
  sum_2 = sum_11 + sum_21
  return(c(ni,sum_1,sum_2))
}

simu <- function(kk, ni_min, ni_max, p1, p2, p11){
  ni_pool <- c(ni_min : ni_max)
  nn <- sample(ni_pool,kk,replace = TRUE)
  data = matrix(0, kk, 3)
  colnames(data) = c('ni','xi','yi')
  
  for (i in 1:kk){
    data[i,] = sam(p1, p2, p11, nn[i])
  }
  data = as.data.frame(data)
  return(data)
}

################################################################################
# different k
################################################################################

k_pool = c(10, 20, 30, 40, 50)

result = matrix(NA, length(k_pool), 20)
colnames(result) = c("rep","ni_around","k","p1_true","p2_true","p11_true",
                     "p1_hat","p2_hat","p11_hat", "var_hat","sd_hat",
                     "sd_error", "ratio","seed",
                     "CI_1_up","CI_1_down","CI_1_cover",
                     "CI_2_up","CI_2_down","CI_2_cover")
################################################################################




################################################################################

for (k in 1:length(k_pool)){
  
  kk = k_pool[k]
  
  data <- simu(kk = kk, ni_min = ni_low, ni_max = ni_high,
               p1 = p1_true, p2 = p2_true, p11 = p11_true)
  
  est1 <- ebdm_estimate(data$ni, data$xi, data$yi, ci_method = "normal")
  est2 <- ebdm_estimate(data$ni, data$xi, data$yi, ci_method = "lr")
  
  p1_hat <- est1$p1_hat
  p2_hat <- est1$p2_hat
  p11_hat <- est1$p11_hat
  se <- est1$sd_hat
  var <- est1$var_hat
  
  ci1 <- est1$ci
  ci2 <- est2$ci
  
  result[k, 1] = seed_index #rep
  result[k, 2] = ni_around #ni
  result[k, 3] = kk #k
  result[k, 4] = p1_true #p1_true
  result[k, 5] = p2_true #p2_true
  result[k, 6] = p11_true #p11_true
  result[k, 7] = p1_hat #p1_hat
  result[k, 8] = p2_hat #p2_hat
  result[k, 9] = p11_hat #p11_hat
  result[k, 10] = var #variance_hat
  result[k, 11] = se #se_hat
  result[k, 14] = seed
  if (!is.null(est1$ci) && length(est1$ci) == 2 && all(!is.na(est1$ci))) {
    ci1_upper <- est1$ci["upper"]
    ci1_lower <- est1$ci["lower"]
    result[k, 15] <- ci1_upper
    result[k, 16] <- ci1_lower
    result[k, 17] <- as.integer(p11_true > ci1_lower & p11_true < ci1_upper)
  } else {
    result[k, 15:17] <- c(NA, NA, 9)
  }
  if (!is.null(est2$ci) && length(est2$ci) == 2 && all(!is.na(est2$ci))) {
    ci2_upper <- est2$ci["upper"]
    ci2_lower <- est2$ci["lower"]
    result[k, 18] <- ci2_upper
    result[k, 19] <- ci2_lower
    result[k, 20] <- as.integer(p11_true > ci2_lower & p11_true < ci2_upper)
  } else {
    result[k, 18:20] <- c(NA, NA, 9)
  }
  print('k')
}

save(result, file = paste0("./results/","setting",setting_index,"_result_",seed_index,".rda"))

