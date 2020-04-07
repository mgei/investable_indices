costs$trade$migrosbank <- 40

migrosbank

cost_total <- function(pf_value, trade_num, trade_avg, broker, only_etfs = F) {
  
  if (broker == "Migrosbank") {
    pf_value_1 <- min(max(pf_value, pf_value - 750000), 750000)
    pf_value_3 <- max(pf_value - 1500000, 0)
    pf_value_2 <- pf_value - pf_value_1 - pf_value_3
    
    depot_1 <- pf_value_1 * 0.0023
    depot_2 <- pf_value_2 * 0.0022
    depot_3 <- pf_value_3 * 0.0019
    
    depot <- depot_1 + depot_2 + depot_3 
    
    trade_price <- 40 + max(trade_avg - 100000, 0) * 0.0002
    
    trading <- trade_num * trade_price
    
  } else if (broker == "Swissquote") {
    depot <- pf_value * 0.00025 * 4
    
    trade_price <- case_when(trade_avg < 500 ~ 9,
                             trade_avg < 2000 ~ 20,
                             trade_avg < 10000 ~ 30,
                             trade_avg < 15000 ~ 55,
                             trade_avg < 25000 ~ 80,
                             trade_avg < 50000 ~ 135,
                             T ~ 190)
    
    if (only_etfs == T) {
      trade_price <- 9
    }
    
    trading <- trade_num * trade_price
  }
  
}