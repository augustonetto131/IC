## Summary statistics

# Libraries:
library(tidyverse)
install.packages("table1")

theme_set(theme_bw())

# Working directory and databases:
setwd("C:/Users/gutao/OneDrive - Insper - Institudo de Ensino e Pesquisa/Documents/Insper/IC/Data/Dataset")

allfolio <- read.csv("allfolio.csv")
smallfolio <- read.csv("smallfolio.csv")
largefolio <- read.csv("largefolio.csv")

median(allfolio$mkt_cap)

# Summary statistics table for each database:

summary_portfolio <- table1::table1(~retorno_total + sharpe_calc + sharpe_economatica + mkt_cap + ROE + ROA + ROIC +  
               lucro_bruto + LPA_growth + alav_BS + EBIT_DivLiq + ativo_growth + 
              desvpad_ret + div_yld + provisoes , data = portfolio)


summary_smallfolio <- table1::table1(~retorno_total + sharpe_calc + sharpe_economatica + mkt_cap + ROE + ROA + ROIC +  
                                      lucro_bruto + LPA_growth + alav_BS + EBIT_DivLiq + ativo_growth + 
                                      desvpad_ret + div_yld + provisoes , data = smallfolio)

summary_largefolio <- table1::table1(~retorno_total + sharpe_calc + sharpe_economatica + mkt_cap + ROE + ROA + ROIC +  
                                      lucro_bruto + LPA_growth + alav_BS + EBIT_DivLiq + ativo_growth + 
                                      desvpad_ret + div_yld + provisoes , data = largefolio)

# Scatterplots com retorno:

(retorno_ROE <- portfolio %>%
    ggplot(aes(x = ROE, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F) +
    labs(colour = NULL))
             

(retorno_ROA <- portfolio %>%
    ggplot(aes(x = ROA, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))

(retorno_ROIC <- portfolio %>%
    ggplot(aes(x = ROIC, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))

(retorno_lucrobruto <- portfolio %>%
    ggplot(aes(x = lucro_bruto, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F) +
    xlim(0, 2500000))

(retorno_LPA_growth <- portfolio %>%
    ggplot(aes(x = LPA_growth, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(retorno_alav_BS <- portfolio %>%
    ggplot(aes(x = alav_BS, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(retorno_EBIT_DivLiq <- portfolio %>%
    ggplot(aes(x = EBIT_DivLiq, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(retorno_ativo_growth <- portfolio %>%
    ggplot(aes(x = ativo_growth, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))

(retorno_desvpad_ret <- portfolio %>%
    ggplot(aes(x = desvpad_ret, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(retorno_div_yld <- portfolio %>%
    ggplot(aes(x = div_yld, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(retorno_provisoes <- portfolio %>%
    ggplot(aes(x = provisoes, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))

# Scatterplots com sharpe:

(sharpe_economatica_ROE <- portfolio %>%
    ggplot(aes(x = ROE, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(sharpe_economatica_ROA <- portfolio %>%
    ggplot(aes(x = ROA, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))

(sharpe_economatica_ROIC <- portfolio %>%
    ggplot(aes(x = ROIC, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))

(sharpe_economatica_lucrobruto <- portfolio %>%
    ggplot(aes(x = lucro_bruto, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F) +
    xlim(0, 2500000))

(sharpe_economatica_LPA_growth <- portfolio %>%
    ggplot(aes(x = LPA_growth, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(sharpe_economatica_alav_BS <- portfolio %>%
    ggplot(aes(x = alav_BS, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(sharpe_economatica_EBIT_DivLiq <- portfolio %>%
    ggplot(aes(x = EBIT_DivLiq, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(retorno_ativo_growth <- portfolio %>%
    ggplot(aes(x = ativo_growth, y = retorno_total)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))

(sharpe_economatica_desvpad_ret <- portfolio %>%
    ggplot(aes(x = desvpad_ret, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(sharpe_economatica_div_yld <- portfolio %>%
    ggplot(aes(x = div_yld, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


(sharpe_economatica_provisoes <- portfolio %>%
    ggplot(aes(x = provisoes, y = sharpe_economatica)) +
    geom_point(aes(colour = size)) +
    geom_smooth(aes(colour = size), method = "lm", se = F))


# Boxplot para retorno nos dois portfolios:

ggplot() +
    geom_boxplot(data = allfolio, aes(y = retorno_total, x = portfolio)) +
    labs(y = NULL, x = NULL, title = "Boxplot dos retornos totais")

bla <- boxplot.stats(smallfolio$retorno_total)
bla$out


# Boxplot para sharpe nos dois portfolios:

ggplot() +
    geom_boxplot(data = allfolio, aes(y = sharpe_economatica, x = portfolio)) +
    labs(y = NULL, x = NULL, title = "Boxplot do índice de Sharpe")

bla <- boxplot(largefolio$sharpe_economatica)
bla$out
