# Retorno:

# libraries:

library(tidyverse)
library(stargazer)

# Em primeiro lugar iremos utilizar aquelas variáveis que apresentaram alguma 
#  significância no teste t para retorno em uma regressão linear simples


###### Cortes na base:

largefolio <- largefolio %>% 
  filter(retorno_total < 2630.113)

smallfolio <- smallfolio %>%
  filter(retorno_total < 1656.113)

# Regressões para largefolio:


reg_ret_1_l <- lm(data = largefolio, retorno_total ~ lucro_bruto)
reg_ret_2_l <- lm(data = largefolio, retorno_total ~ provisoes)
reg_ret_3_l <- lm(data = largefolio, retorno_total ~ lucro_bruto + provisoes)


# Regressões para smallfolio:


reg_ret_1_s <- lm(data = smallfolio, retorno_total ~ lucro_bruto)
reg_ret_2_s <- lm(data = smallfolio, retorno_total ~ provisoes)
reg_ret_3_s <- lm(data = smallfolio, retorno_total ~ lucro_bruto + provisoes)


stargazer(reg_ret_1_l, reg_ret_2_l, reg_ret_3_l,
          reg_ret_1_s, reg_ret_2_s, reg_ret_3_s,
          type = "latex", omit.stat = c("f", "ser") ,
          title = "Efeito dos fatores de qualidade sobre retorno total",
          dep.var.labels = c("Retorno entre 2009 e 2019"))


####################################################################################


# Sharpe:

# Em primeiro lugar iremos utilizar aquelas variáveis que apresentaram alguma 
#  significância no teste t para Sharpe em uma regressão linear simples


###### Cortes na base:

largefolio <- largefolio %>% 
  filter(sharpe_economatica > -0.47 & sharpe_economatica < 1.28)

smallfolio <- smallfolio %>%
  filter(sharpe_economatica < 1.7)

# Regressões para largefolio:
reg_sharpe_1_l <- lm(data = largefolio, retorno_total ~ ROA)
reg_sharpe_2_l <- lm(data = largefolio, retorno_total ~ ROIC)
reg_sharpe_3_l <- lm(data = largefolio, retorno_total ~ lucro_bruto)
reg_sharpe_4_l <- lm(data = largefolio, retorno_total ~ ativo_growth)
reg_sharpe_5_l <- lm(data = largefolio, retorno_total ~ provisoes)


# Reg multipla
reg_sharpe_6_l <- lm(data = largefolio, retorno_total ~ ROA + ROIC + lucro_bruto)
reg_sharpe_7_l <- lm(data = largefolio, retorno_total ~ ROIC + lucro_bruto + ativo_growth)
reg_sharpe_8_l <- lm(data = largefolio, retorno_total ~ lucro_bruto + ativo_growth + provisoes)
reg_sharpe_9_l <- lm(data = largefolio, retorno_total ~ ativo_growth + provisoes + ROA)
reg_sharpe_10_l <- lm(data = largefolio, retorno_total ~ provisoes + ROA + ROIC)



# Regressões para smallfolio:

reg_sharpe_1_s <- lm(data = largefolio, retorno_total ~ ROA)
reg_sharpe_2_s <- lm(data = largefolio, retorno_total ~ ROIC)
reg_sharpe_3_s <- lm(data = largefolio, retorno_total ~ lucro_bruto)
reg_sharpe_4_s <- lm(data = largefolio, retorno_total ~ ativo_growth)
reg_sharpe_5_s <- lm(data = largefolio, retorno_total ~ provisoes)



# Mutlipla

reg_sharpe_6_s <- lm(data = largefolio, retorno_total ~ ROA + ROIC + lucro_bruto)
reg_sharpe_7_s <- lm(data = largefolio, retorno_total ~ ROIC + lucro_bruto + ativo_growth)
reg_sharpe_8_s <- lm(data = largefolio, retorno_total ~ lucro_bruto + ativo_growth + provisoes)
reg_sharpe_9_s <- lm(data = largefolio, retorno_total ~ ativo_growth + provisoes + ROA)
reg_sharpe_10_s <- lm(data = largefolio, retorno_total ~ provisoes + ROA + ROIC)







# Saída das Simples com largefolio:
stargazer(reg_sharpe_1_l, reg_sharpe_2_l, reg_sharpe_3_l, reg_sharpe_4_l, reg_sharpe_5_l,
          type = "latex", omit.stat = c("f", "ser"),
          title = "Efeito dos fatores de qualidade sobre Sharpe - Regressões Simples em largefolio",
          dep.var.labels = c("Sharpe entre 2009 e 2019"))

# Saída das Simples com smallfolio:
stargazer(reg_sharpe_1_s, reg_sharpe_2_s, reg_sharpe_3_s, reg_sharpe_4_s, reg_sharpe_5_s,
          type = "latex", omit.stat = c("f", "ser"),
          title = "Efeito dos fatores de qualidade sobre Sharpe - Regressões Simples em smallfolio",
          dep.var.labels = c("Sharpe entre 2009 e 2019"))


# Saída das múltiplas com large:
stargazer(reg_sharpe_6_l, reg_sharpe_7_l, reg_sharpe_8_l, reg_sharpe_9_l,
          reg_sharpe_10_l, type = "latex", omit.stat = c("f", "ser"),
          title = "Efeito dos fatores de qualidade sobre Sharpe - Regressões Múltiplas em largefolio",
          dep.var.labels = c("Sharpe entre 2009 e 2019"))

# Saída das múltiplas com small:
stargazer(reg_sharpe_6_s, reg_sharpe_7_s, reg_sharpe_8_s, reg_sharpe_9_s,
          reg_sharpe_10_l, type = "latex", omit.stat = c("f", "ser"),
          title = "Efeito dos fatores de qualidade sobre Sharpe - Regressões Múltiplas em smallfolio",
          dep.var.labels = c("Sharpe entre 2009 e 2019"))
