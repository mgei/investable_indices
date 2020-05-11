weights <- FANG %>% 
  select(symbol) %>% 
  distinct() %>% 
  mutate(weight = 1/4)

rets <- FANG %>% 
  group_by(symbol) %>% 
  left_join(weights, by = "symbol") %>% 
  mutate(ret = (adjusted/lag(adjusted)-1), ret_contr = (adjusted/lag(adjusted)-1)*weight)


cumprod_na <- function(...) {
  out <- cumprod(replace_na(..., 1))
  
  return(out)
}

rets %>% 
  mutate(value = cumprod_na(1 + ret)) %>% 
  group_by(date) %>% 
  mutate(pf_weight = value/sum(value))

rets %>% 
  filter(date <= as.Date("2013-01-15")) %>% 
  group_by(date) %>% 
  mutate(weight_actual = (1+ret)/(sum(1+ret))) %>% 
  print(n = 100)


rets %>% 
  mutate()

library(dplyr)
library(tidyr)
set.seed(3)
n <- 6

rets <- tibble(period = rep(1:n, 3),
               stock = c(rep("A", n), rep("B", n), rep("C", n)),
               ret = c(rnorm(n, 0, 0.3), rnorm(n, 0, 0.2), rnorm(n, 0, 0.1)))

target_weights <- tibble(stock = c("A", "B", "C"), target_weight = 1/3)

rets_weights <- rets %>% 
  left_join(target_weights, by = "stock")

rets_weights

# # A tibble: 18 x 4
# period stock      ret target_weight
# <int> <chr>    <dbl>         <dbl>
#   1      1 A     -0.289           0.333
# 2      2 A     -0.0878          0.333
# 3      3 A      0.0776          0.333
# 4      4 A     -0.346           0.333
# 5      5 A      0.0587          0.333
# 6      6 A      0.00904         0.333
# 7      1 B      0.0171          0.333
# 8      2 B      0.223           0.333
# 9      3 B     -0.244           0.333
# 10      4 B      0.253           0.333
# 11      5 B     -0.149           0.333
# 12      6 B     -0.226           0.333
# 13      1 C     -0.0716          0.333
# 14      2 C      0.0253          0.333
# 15      3 C      0.0152          0.333
# 16      4 C     -0.0308          0.333
# 17      5 C     -0.0953          0.333
# 18      6 C     -0.0648          0.333

rets_weights_actual <- rets_weights %>% 
  group_by(stock) %>% 
  mutate(value = cumprod(1+ret)*target_weight[1]) %>% 
  group_by(period) %>% 
  mutate(actual_weight = value/sum(value))

rets_weights_actual

# # A tibble: 18 x 6
# # Groups:   period [6]
# period stock      ret target_weight value actual_weight
# <int> <chr>    <dbl>         <dbl> <dbl>         <dbl>
#   1      1 A     -0.289           0.333 0.237         0.268
# 2      2 A     -0.0878          0.333 0.216         0.228
# 3      3 A      0.0776          0.333 0.233         0.268
# 4      4 A     -0.346           0.333 0.153         0.178
# 5      5 A      0.0587          0.333 0.162         0.207
# 6      6 A      0.00904         0.333 0.163         0.238
# 7      1 B      0.0171          0.333 0.339         0.383
# 8      2 B      0.223           0.333 0.415         0.437
# 9      3 B     -0.244           0.333 0.314         0.361
# 10      4 B      0.253           0.333 0.393         0.458
# 11      5 B     -0.149           0.333 0.335         0.430
# 12      6 B     -0.226           0.333 0.259         0.377
# 13      1 C     -0.0716          0.333 0.309         0.349
# 14      2 C      0.0253          0.333 0.317         0.335
# 15      3 C      0.0152          0.333 0.322         0.371
# 16      4 C     -0.0308          0.333 0.312         0.364
# 17      5 C     -0.0953          0.333 0.282         0.363
# 18      6 C     -0.0648          0.333 0.264         0.385

rets_weights_actual %>% 
  summarise(ret_pf = sum(ret*actual_weight))

n <- 20

weights <- tibble(period = rep(1:n, 2),
                  stock = c(rep("A", n), rep("B", n))) %>% 
  mutate(weight = c(c(0.5, 0.55, 0.6, 0.55, 0.63, 0.6, 0.64, 0.5, 0.4, 0.37, 0.35, 0.5, 0.53, 0.56, 0.55, 0.6, 0.64, 0.5, 0.53, 0.48),
                    1-c(0.5, 0.55, 0.6, 0.55, 0.63, 0.6, 0.64, 0.5, 0.4, 0.37, 0.35, 0.5, 0.53, 0.56, 0.55, 0.6, 0.64, 0.5, 0.53, 0.48)))

weights %>% 
  ggplot(aes(x = period, y = weight, color = stock)) +
  geom_line() +
  geom_hline(yintercept = c(0.5, 0.5+0.15, 0.5-0.15)) +
  annotate("label", x = 4, y = 0.5, label = "target weight") +
  facet_wrap(~stock, ncol = 1)



r <- rets %>% 
  pivot_wider(names_from = "stock", values_from = ret)

for (i in 1:nrow(r)) {
  r[i] %>% mutate(weights)
}

i <- 1

r %>%
  mutate(w_A = 1/3, w_B = 1/3, w_C = 1/3) %>% 
  mutate(r_pf)
  mutate(w_A = lag(w_A)*1)
 
rets %>% 
  mutate(w = if_else(period == 1, 1/3, NA_real_)) %>% 
  mutate(return = lag(w)*ret)
  
  
  
  
  
   
r %>% 
  group_by(period) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% mutate(weights)))
  mutate(weight = list(c(1,2,3)))

tibble(period = 0, A = 0, B = 0, C = 0, w_A = 1/3, w_B = 1/3, w_C = 1/3) %>% 
  bind_rows(r) %>% 
  mutate(w_A = case_when(row_number() == 1 ~ w_A,
                         T ~ 1)) #lag(w_A)*lag(A))/((lag(A)+lag(B)+lag(C))/3))
  mutate(PF = (A+B+C)/3)

  



  


as_tibble(mtcars) %>% 
  mutate(x = c(1/2, rep(NA, 31))) %>% 
  mutate_interative(x = case_when(row_number() == 1 ~ x,
                          T ~ lag(x)))


for (i in 1:nrow(r)) {
  if (i == 1) {
    r[i,] %>% mutate(weights = )
  }
}
  
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

r %>% 
  mutate(w_A = 1/3)


r %>% 
  mutate(vA = cumprod(1+A),
         vB = cumprod(1+B),
         vC = cumprod(1+C)) %>% 
  mutate(PF = vA+vB+vC) %>% 
  mutate(wA = vA/PF,
         wB = vB/PF,
         wC = vC/PF)


compute_weights <- function(returns, weights = rep(1/ncol(returns), ncol(returns)), threshold = 0.05) {

  # returns <- as.matrix(returns)
  pf_weights <- matrix(weights, ncol = length(weights))
  pf_values <- weights
  pf_value <- rowSums(pf_values)
  
  for (t in 1:nrow(returns)) {
    pf_values <- pf_values %>% rbind((1 + returns[t,]) * pf_values[t,])
    pf_value <- pf_value %>% rbind(sum(pf_values[t+1,]))
    pf_weights <- pf_weights %>% rbind(pf_values[t+1,]/pf_value[t+1])
  }
  
  pf_values
  pf_weights
  pf_value
  
  # 
  # weights_out <- values_out / sum(values_out)
  # 
  # if (any(weights_out > weights+threshold) | any(weights_out < weights-threshold)){
  #   weights_out <- weights
  # }
  # 
  # weights_in <- weights_out
  
}

compute_weights(r %>% select(-period))


