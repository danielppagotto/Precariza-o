library(readxl)
library(tidyverse)

prec_enf <- read_excel("dados/precarizacao_enf_artigo.xlsx")

prec_enf_tratado <- 
  prec_enf |> 
  gather(key = "categoria",
         value = "percentual",
         2:19) |> 
  separate(categoria, c("Regiao","Nível"), sep = "&") |> 
  mutate(Regiao = str_trim(Regiao),
         Nível = str_trim(Nível)) |> 
  mutate(Nível = case_when(Nível == "prec_prim" ~ "Primário",
                           Nível == "prec_sec" ~ "Secundário",
                           Nível == "prec_terc" ~ "Terciário"))


prec_enf_tratado |> 
  ggplot(aes(x = ano, y = 100*percentual, col = Nível)) + 
  geom_line(linewidth = 1) + 
  theme_minimal() + facet_wrap(~Regiao, ncol = 2) + 
  xlab("Ano") + 
  ylab("Percentual de vínculos precarizados (%)") + 
  theme(
    text = element_text(size = 12),     
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),   
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)   
  )
