# Retorno:




# Definidos os portfólios, podemos iniciar os testes t para médias de retorno, 
# sempre utilizando do trigésimo percentil acima como o "top30" e do septuagésimo 
# percentil para baixo como o "bottom30". 

# Primeiros vamos analisar através do ROE como indicador de qualidade:



###### Cortes na base:

largefolio <- largefolio %>% 
  filter(retorno_total < 2630.113)

smallfolio <- smallfolio %>%
  filter(retorno_total < 1656.113)

###### ROE no Bigfolio:
  
# Essas são as 30% com maiores ROE's
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno_total)) %>%
  filter(ROE > 0 & ROE <= 100) %>% 
  arrange(desc(ROE)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores ROE's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno_total)) %>%
  filter(ROE > 0 & ROE <= 100) %>% 
  arrange(ROE) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)




# Comparação:
p_return_ROE_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE,
                           var.equal = FALSE, conf.level = 0.95)$p.value



###### ROE no Smallfolio:
  
# Essas são as 30% com maiores ROE's
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno_total)) %>%
  filter(ROE > 0 & ROE <= 100) %>% 
  arrange(desc(ROE)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores ROE's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno_total)) %>%
  filter(ROE > 0 & ROE <= 100) %>% 
  arrange(ROE) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_ROE_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                             var.equal = FALSE, conf.level = 0.95)$p.value




###### ROA no Bigfolio: 
  
# Essas são as 30% com maiores ROA's
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno_total)) %>%
  filter(ROA > 0) %>% 
  arrange(desc(ROA)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores ROA's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno_total)) %>%
  filter(ROA > 0) %>%
  arrange(ROA) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_ROA_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                           var.equal = FALSE, conf.level = 0.95)$p.value



###### ROA no Smallfolio:
  
# Essas são as 30% com maiores ROA's
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno_total)) %>%
  filter(ROA > 0) %>%
  arrange(desc(ROA)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores ROA's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno_total)) %>%
  filter(ROA > 0) %>%
  arrange(ROA) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_ROA_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                             var.equal = FALSE, conf.level = 0.95)$p.value





###### ROIC no Bigfolio:
  
# Essas são as 30% com maiores ROIC's
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno_total)) %>%
  arrange(desc(ROIC)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno_total * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno_total * cap_weight) %>%
  select(retorno_total)


# Essas são as 30% com menores ROIC's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno_total)) %>%
  arrange(ROIC) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno_total * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno_total * cap_weight) %>%
  select(retorno_total)


# Comparação:
p_return_ROIC_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                            var.equal = FALSE, conf.level = 0.95)$p.value



###### ROIC no Smallfoilio:
  
  
# Essas são as 30% com maiores ROIC's
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno_total)) %>%
  arrange(desc(ROIC)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores ROIC's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno_total)) %>%
  arrange(ROIC) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_ROIC_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                              var.equal = FALSE, conf.level = 0.95)$p.value



###### Lucro bruto no bigfolio:
  
  
# Essas são as 30% com maiores Lucro Bruto
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno_total)) %>%
  arrange(desc(lucro_bruto)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores Lucro Bruto
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno_total)) %>%
  arrange(lucro_bruto) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_lucro_bruto_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                   var.equal = FALSE, conf.level = 0.95)$p.value





###### Lucro bruto no Smallfolio:
  
  
# Essas são as 30% com maiores Lucro Bruto
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno_total)) %>%
  arrange(desc(lucro_bruto)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores Lucro Bruto
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno_total)) %>%
  arrange(lucro_bruto) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_lucro_bruto_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                     var.equal = FALSE, conf.level = 0.95)$p.value



###### Crescimento de LPA no Bigfolio:
  
  
# Essas são as 30% com maiores LPA growth
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno_total)) %>%
  arrange(desc(LPA_growth)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores LPA growth
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno_total)) %>%
  arrange(LPA_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_LPA_growth_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                  var.equal = FALSE, conf.level = 0.95)$p.value




###### Crescimento de LPA no Smallfolio: 
  
  
# Essas são as 30% com maiores LPA growth
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno_total)) %>%
  arrange(desc(LPA_growth)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores LPA growth
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno_total)) %>%
  arrange(LPA_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_LPA_growth_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                    var.equal = FALSE, conf.level = 0.95)$p.value




###### Dívida/Cap. próprio no Bigfolio: 
  
  
# Essas são as 30% com maiores BS
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno_total)) %>%
  filter(alav_BS > 0) %>% 
  arrange(desc(alav_BS)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores BS
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno_total)) %>%
  filter(alav_BS > 0) %>%
  arrange(alav_BS) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_BS_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                          var.equal = FALSE, conf.level = 0.95)$p.value



###### Dívida/Cap. próprio no Smallfolio:
  
  
# Essas são as 30% com maiores BS
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno_total)) %>%
  filter(alav_BS > 0) %>%
  arrange(desc(alav_BS)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores BS
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno_total)) %>%
  filter(alav_BS > 0) %>%
  arrange(alav_BS) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_BS_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                            var.equal = FALSE, conf.level = 0.95)$p.value



###### EBIT/Div. Liq. no Bigfolio:
  
  
# Essas são as 30% com maiores EBIT_DivLiq
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno_total)) %>%
  filter(EBIT_DivLiq > 0) %>%
  arrange(desc(EBIT_DivLiq)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores EBIT_DivLiq
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno_total)) %>%
  filter(EBIT_DivLiq > 0) %>%
  arrange(EBIT_DivLiq) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_EBIT_DivLiq_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                   var.equal = FALSE, conf.level = 0.95)$p.value



###### EBIT/Div. Liq. no Smallfolio:
  
  
# Essas são as 30% com maiores EBIT_DivLiq
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno_total)) %>%
  filter(EBIT_DivLiq > 0) %>%
  arrange(desc(EBIT_DivLiq)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores EBIT_DivLiq
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno_total)) %>%
  filter(EBIT_DivLiq > 0) %>%
  arrange(EBIT_DivLiq) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>%
  select(retorno_total)


# Comparação:
p_return_EBIT_DivLiq_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                     var.equal = FALSE, conf.level = 0.95)$p.value




###### Crescimento de ativo no Bigfolio:
  
  
# Essas são as 30% com maiores ativo_growth
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno_total)) %>%
  arrange(desc(ativo_growth)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores ativo_growth
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno_total)) %>%
  arrange(ativo_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_ativo_growth_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                    var.equal = FALSE, conf.level = 0.95)$p.value



###### Crescimento de ativo no Smallfolio:
  
  
# Essas são as 30% com maiores ativo_growth
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno_total)) %>%
  arrange(desc(ativo_growth)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores ativo_growth
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno_total)) %>%
  arrange(ativo_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_ativo_growth_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                      var.equal = FALSE, conf.level = 0.95)$p.value



###### Provisões no Bigfolio:
  
  
# Essas são as 30% com maiores provisoes
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno_total)) %>%
  filter(provisoes > 0) %>% 
  arrange(desc(provisoes)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores provisoes
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno_total)) %>%
  filter(provisoes > 0) %>% 
  arrange(provisoes) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_provisoes_big <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                 var.equal = FALSE, conf.level = 0.95)$p.value




###### Provisões no Smallfolio:
  
  
# Essas são as 30% com maiores provisoes
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno_total, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno_total)) %>%
  filter(provisoes > 0) %>% 
  arrange(desc(provisoes)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno_total)


# Essas são as 30% com menores provisoes
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno_total, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno_total)) %>%
  filter(provisoes > 0) %>% 
  arrange(provisoes) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno_total)


# Comparação:
p_return_provisoes_small <- t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
                                   var.equal = FALSE, conf.level = 0.95)$p.value





###### Resultados dos testes:

cat("Comparativo de retorno para ROE nas empresas grandes,p-valor:   ", p_return_ROE_big, 
    "\nComparativo de retorno para ROE nas empresas pequenas, p-valor: ", p_return_ROE_small,
    "\n\nComparativo de retorno para ROA nas empresas grandes, p-valor:  ", p_return_ROA_big,
    "\nComparativo de retorno para ROA nas empresas pequenas, p-valor: ", p_return_ROA_small,
    "\n\nComparativo de retorno para ROIC nas empresas grandes, p-valor: ", p_return_ROIC_big,
    "\nComparativo de retorno para ROIC nas empresas pequenas, p-valor: ", p_return_ROIC_small,
    "\n\nComparativo de retorno para Lucro Bruto nas empresas grandes, p-valor: ", p_return_lucro_bruto_big,
    "\nComparativo de retorno para Lucro Bruto nas empresas pequenas, p-valor: ", p_return_lucro_bruto_small,
    "\n\nComparativo de retorno para crescimento de LPA nas empresas grandes, p-valor: ", p_return_LPA_growth_big,
    "\nComparativo de retorno para Crescimento de LPA nas empresas pequenas, p-valor: ", p_return_LPA_growth_small,
    "\n\nComparativo de retorno para Dívida/Cap. Próprio nas empresas grandes, p-valor: ", p_return_BS_big,
    "\nComparativo de retorno para Dívida/Cap. Próprio nas empresas pequenas, p-valor: ", p_return_BS_small,
    "\n\nComparativo de retorno para EBIT/Div. Líquida nas empresas grandes, p-valor: ", p_return_EBIT_DivLiq_big,
    "\nComparativo de retorno para EBIT/Div. Líquida nas empresas pequenas, p-valor: ", p_return_EBIT_DivLiq_small,
    "\n\nComparativo de retorno para Crescimento dos ativos nas empresas grandes, p-valor: ", p_return_ativo_growth_big,
    "\nComparativo de retorno para Crescimento dos ativos nas empresas pequenas, p-valor: ", p_return_ativo_growth_small,
    "\n\nComparativo de retorno para Provisões nas empresas pequenas, p-valor: ", p_return_provisoes_big,
    "\nComparativo de retorno para Provisões nas empresas pequenas, p-valor: ", p_return_provisoes_small)

