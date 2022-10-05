pacman::p_load("knitr","magrittr","tidyverse","lubridate","janitor","kableExtra")



theme_grafic = function(
  legend.position = "top",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.text.x  = element_text(angle = 38),
  axis.ticks.x = element_blank(),
  axis.ticks = element_blank(),
  legend.title = element_blank(),
  axis.line = element_blank(),
  panel.border = element_blank(),
  axis.text = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)){
  
  ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.y = axis.title.y,
      axis.title.x = axis.title.x,
      axis.text = axis.text,
      panel.border = panel.border,
      axis.line = axis.line,
      axis.text.y = axis.text.y,
      axis.text.x = axis.text.x,
      axis.ticks.x = axis.ticks.x, 
      legend.title = legend.title,
      plot.title = plot.title,
      legend.position = legend.position
      
    )
  
  
}
# axis.text.y=element_text(size=15),
# axis.ticks=element_blank(),
# axis.text.x=element_blank() ,
# 
# panel.grid.major = element_blank(),
# panel.grid.minor = element_blank(),
# panel.background = element_rect(fill = "white")

ler_excel = function(path, sheet = 1, coluna_data = "DT_OCORRENCIA"){
  
  
  data = readxl::read_excel(path = path, sheet = sheet)
  
  col_types = sapply(data, typeof) 
  col_types[col_types  == "logical"] = "text"
  col_types[col_types  != "text"] = "guess"
  
  
  
  data = readxl::read_excel(path = path, sheet = sheet, col_types = col_types )%>%   
    rename("Data" = coluna_data) %>%
    mutate(ano = year(Data) ) %>%
    mutate(mes_ext = month(Data, label = T),
           mes_num = month(Data)) %>%
    mutate(ano = sprintf('%02d', ano %% 100)) %>%
    mutate(ano_mes = paste(mes_ext ,ano, sep = "/"))%>%
    arrange(Data) %>%
    mutate(ano = year(Data))

  
  
  
  
  return(data)
  
  
  
}




##___________________________________Funções Uteis_________________________####

primeiro_semestre =  c("jan","fev","mar","abr","mai","jun")
segundo_semestre = c("jul","ago","set","out","nov","dez")

meses = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")


meses_extenso = function(){
  meses = c("janeiro","fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro")  
  return(meses[Sys.Date() %>% month()-1]) 
} 


#CRIAR IMAGENS NULAS
imagem_null = function(path_image){
  
  img<-readPNG("Imagens/sem_observacoes.png")
  
  
  h <- dim(img)[1]
  w <- dim(img)[2]
  
  path_image = paste0("Imagens/",path_image)
  #open new file for output
  png(path_image, width=w, height=h)
  par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
  plot.new()
  plot.window(0:1, 0:1)
  
  
  #fill plot with image
  usr<-par("usr")
  rasterImage(img, usr[1], usr[3], usr[2], usr[4])
  
  
  
  #close image
  dev.off()
}



##EXCLUIR LINHAS QUE TENHAM 0
row_zeros = function(data){
  
  #no pacote kable, quandos os índices não estão sequenciados, ele acaba mostrando o número dos índices.
  #para resolver esse problema, eu reseto os índices
  
  
  #essa função exclui as linhas que tem todas as observações 0, por isso, na hora de aplica-la
  #eu n conto a primeira coluna
  row = sapply(1:nrow(data), function(row) ((data[row,1] == "Total") ||  !all(data[row,2:ncol(data)]==0)) )
  
  
  #filtra o dataframe sem as linhas com observações 0
  data = data[row,]


  #renomeia os índices com base na dificuldade encontrada acima
  rownames(data) = 1:nrow(data)
  
  return(data)
}





##ORGANIZAR OS MESES EM UMA ORDEM ESPECÍFICA
##ex: jun, jul, ago, ...
arrange_meses = function(indice){
  c(na.rm(meses[indice+1:12]), meses[1:indice])
}



##JUNTAR MES COM O ANO. 
##ex: jun/20
paste_mes_ano = function(mes_final, meses_reorg, ano_final, ano_inicial){
  cont = 0
  mes_ano = c()
  
  mudar_ano = 12 - (mes_final + 1)
  
  
  for(mes in meses_reorg){
    ano = ifelse(cont <= mudar_ano, ano_inicial, ano_final)
    mes_ano[cont+1] = paste0(mes,"/", ano)
    cont = cont+1
  }
  return(mes_ano)
  
}

## O VETOR COM OS MESES E ANOS JUNTOS E REORDENADOS 
meses_anos = function(mes_final, ano_final){
  
  ano_final = sprintf('%02d', ano_final %% 100) %>% as.numeric()
  
  ano_inicial = ifelse(mes_final == 12, ano_final, ano_final - 1)
  
  meses_reorg = arrange_meses(mes_final)
  
  mes_ano = paste_mes_ano(mes_final, meses_reorg, ano_final, ano_inicial)
  
  return(mes_ano)
}



# juntar_por_mes = function(data, mes, anos){
#   
#   
#   data_mes = data.frame(rep(0,12)) %>% t %>% data.frame
#   
#   colnames(data_mes) = meses_anos(mes, anos)
#   
#   if("nulo" %in% colnames(data)){
#     
#     data_final = data_mes
#     row.names(data_final) = "Total"
#     
#     
#   }else{
#     data_final = cbind(data, data_mes) %>%
#       select("var_indica", meses_anos(mes, anos)) %>%
#       adorn_totals(c("col"))
#       
#     
#     colnames(data_final)[1] = ""
#     
#     
#   }
#   
# 
#   
#   return(data_final)
#   
#   
# }






## GERAR ALGUM DATAFRAME COM OBSERVAÇÕES ALGUMA OBSERVACAO DEFAULT
gerar_data_nulo = function(vetor_linha, vetor_coluna, obs = 0){
  
  linhas = vetor_linha
  colunas = vetor_coluna
  
  valores_zeros = rep(obs,length(colunas)*length(linhas))
  
  
  matriz = matrix(valores_zeros, nrow = length(linhas)) 
  
  colnames(matriz) = colunas
  
  matriz[,1] = linhas
  
  matriz = matriz %>% data.frame
  
  colnames(matriz) = colunas
  
  matriz[,-1] = sapply(matriz[,-1], as.numeric)
  
  matriz
  
}





  


na.rm = function(vetor){
  
  vetor[!is.na(vetor)]
  
}


mes_convert = function(mes){
  if(mes == 12){
    mes = 11
  }else{
    mes = mes
  }
  
  return(mes)
}




periodo.final= function(date){

  
  periodo_final = paste0(anos,"-",mes,"-",dia) %>% as.Date + 31
  periodo_final = periodo_final - day(periodo_final)
  periodo_final

  
}





periodo.inicial = function(periodo_final, tipo_date, qtd_anos = 0){
  
  
  if(tipo_date == "semestral"){
    
    ano_inicial = year(periodo_final) - (qtd_anos+1)
    cond = T
    
    while(cond){
      periodo_final = periodo_final - 1 
      
      cond = year(periodo_final) != ano_inicial
      
    }
    
    periodo_final = periodo_final + 1 
    
    
    return(periodo_final)
  }
  
  if(tipo_date == "anual"){
    
    
    periodo_final = periodo_final - 360*(1+qtd_anos)
    periodo_final = periodo_final - day(periodo_final) + 1
    
    return(periodo_final)
    
    
  }
}


remover_imagens = function(){
  
  list_files = list.files("Imagens")
  
  list_files = list_files[!list_files %in% c("logo_header.jpg", "sem_observacoes.png")]
  
  list_files = paste0("Imagens/",list_files)
  
  
  sapply(list_files, file.remove)
}










# ##-----------------------------------------------GRÁFICOS-----------------------------------------------####
# grafico_linhas = function(data, variavel, path_image = "", title = "", metas = -1){
#   
#   out = tryCatch({
#      
#     data = data %>% 
#       rename("taxa" = variavel) %>% 
#       mutate(periodo = Data) %>% 
#       filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
#       mutate(taxa = round(taxa,1), taxa_texto = as.character(taxa)) %>% 
#       mutate(taxa_texto = str_replace(taxa_texto, "\\.",",")) %>% 
#       mutate(periodo = ano_mes)
#     
#     maximo = max(data$taxa %>% unlist) 
#     data$periodo = factor(data$periodo, levels = data$periodo)
#     
#     
#     ggplot(data, aes(x = `periodo`, y = taxa, label = taxa_texto, group = 1))+
#       geom_text(hjust = -.05, vjust = -.4)+
#       geom_line(size = 0.5, color = "#999494")+
#       scale_y_continuous(breaks = c(0,maximo+10), limits = c(0,maximo+10))+
#       geom_line(aes(x = periodo, y = metas, color = "blue"))+
#       scale_color_discrete(name = "", labels = c("Meta"))+
#       theme_grafic()+
#       ggsave(paste0("Imagens/",path_image))
#     
#     
#   },
#   error = function(cond){
#     return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
#   
#   
# }
# 
# #GRAFICO TAXAS
# 
# grafico_taxa<- function(data_num, incidente, periodo_num, data_den, var_den, periodo_den, var_base = "Data",
#                         more_filtros = F, filtro_extra = "", var_extra = "", path_image = "", divisor = 100, 
#                         vetor_metas = c(1,-1), y_max = 100){
#   
#   out = tryCatch({  
#     
#     convert_porcent = function(valor){
#       ifelse(divisor == 100, paste0(valor ,"%"),valor)
#     }
#     
#     vetor_data_levels = data_den$ano_mes %>% unique
#     
#     denominador = data_den %>% 
#       rename("periodo" = periodo_den, "var_den" = var_den) %>% 
#       filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
#       group_by(ano_mes) %>%
#       summarise(total = sum(var_den))
#     
#     if(more_filtros == T){
#       
#       numerador = data_num %>% 
#         rename("periodo" = periodo_num) %>% 
#         filter(Tipo_incidente == incidente) %>% 
#         filter(var_extra == filtro_extra) 
#       
#     }else{
#       
#       numerador = data_num %>% 
#         rename("periodo" = periodo_num) %>% 
#         filter(Tipo_incidente == incidente) 
#       
#     }
#     
#     numerador = numerador %>%
#       rename("var_base_rename" = var_base) %>% 
#       filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
#       filter(!is.na(var_base_rename)) %>% 
#       group_by(ano_mes) %>%
#       summarise(n = n())
#     
#     
#     
#     
#     data = left_join(denominador,numerador, by = 'ano_mes',) %>%
#       mutate(n = replace_na(n,0)) %>%
#       mutate(taxa = round((n/total)*divisor, 1)) %>%
#       mutate(taxa_texto = str_replace(taxa, "\\.",",")) %>%
#       mutate(taxa_texto = sapply(taxa_texto, convert_porcent))
# 
#     data$ano_mes = ordered(data$ano_mes, levels = vetor_data_levels)
# 
#    
#     
#     max_value_taxa = max(data$taxa)+mean(data$taxa)
#     lim_superior = ifelse(divisor == 1000, max_value_taxa, y_max)
# 
# 
#     if(nrow(denominador)==0){
#       img = imagem_null(path_image)
# 
#     }else{
#       grafico = ggplot(data, aes(x = ano_mes, y = taxa ,label = taxa_texto, group = 1))+
#         geom_text(hjust = -.05, vjust = -.4)+
#         geom_line(size = 0.5, color = "#999494")+
#         geom_line(data = data, aes(x = ano_mes, y = vetor_metas[1]),size = 0.5, color = "red")+
#         geom_line(data = data, aes(x = ano_mes, y = vetor_metas[2]),size = 0.5, color = "red")+
#         scale_color_discrete(name = "", labels = c("Meta"))+
#         theme_grafic()+
#         scale_y_continuous(limits = c(0,lim_superior))
# 
# 
# 
#         ggsave(paste0("Imagens/",path_image),plot = grafico)
#       }
# 
#     #return(grafico)
#   },
#   error = function(cond){
#     return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
#   
#   
# }
# 
# 
# limites = function(grafico, divisor){
#   
#   if(divisor == 100){
#     grafico+
#       scale_y_continuous(limits = c(0,1))
#     
#   }else{
#     grafico
#   }
# }
# #GRAFICO DAS METAS
# grafico_metas2 = function(data, var_indica, total, path_image = "", title = "", ala_existe = T, possui_meta = T, metas = -1){
#   
#   out = tryCatch({
#     
#     data = data %>% 
#       mutate(periodo = Data) %>% 
#       #filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
#       rename("var_indica" = var_indica, "total" = total) 
#     
#     vetor_data_levels = data$ano_mes %>% unique
#     
#     #alguns programas apresentam diferentes tipos de alas, por isso foi feito 
#     #essa estrutura de condição para pegar esses casos
#     
#     if(ala_existe == T){
#       data = data %>% 
#         group_by(ano_mes) %>%
#         arrange(Data)%>% 
#         summarise(taxa = sum(var_indica, na.rm = T)/sum(total, na.rm = T)) 
#       
#     }else {
#       data = data %>%
#         mutate(taxa = var_indica/total) 
#     }
#     
#     data$ano_mes = factor(data$ano_mes, levels = vetor_data_levels)
#     
#     data = data %>% 
#       arrange(ano_mes) %>% 
#       mutate(porcent = paste0(round(taxa,2)*100, "%"))
#     
#     
#    
#     
#     if(nrow(data) == 0){
#       img = imagem_null(path_image)
#       
#     }else{
#       grafico = ggplot(data, aes(x = ano_mes, y = taxa, label = porcent, group = 1))+
#         geom_text(hjust = -.05, vjust = -.4)+
#         geom_line(size = 0.5, colour = "#999494")+
#         scale_y_continuous(breaks = c(0,1), limits = c(0,1))
#       
#       if(possui_meta == T){
#         grafico = grafico+
#           geom_line(aes(x = ano_mes, y = metas, colour = "#000000"))+
#           scale_color_discrete(name = "", labels = c("Meta"))+
#           theme_grafic()+
#           ggsave(paste0("Imagens/",path_image))
#         
#         
#       }else {
#         grafico = grafico+
#           theme_grafic()+
#           ggsave(paste0("Imagens/",path_image))
#         
#       }
#       
#       return(grafico)
#       
#     }
#   },
#   error = function(cond){
#     return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
#   
# }
# 
# #GRAFICO DAS METAS
# grafico_metas = function(data, var_indica, total, path_image = "", title = "", ala_existe = T, possui_meta = T, metas = -1){
#   
#   out = tryCatch({
#     
#     data = data %>% 
#       mutate(periodo = Data) %>% 
#       filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
#       rename("var_indica" = var_indica, "total" = total) 
#     
#     vetor_data_levels = data$ano_mes %>% unique
#     
#     #alguns programas apresentam diferentes tipos de alas, por isso foi feito 
#     #essa estrutura de condição para pegar esses casos
#     
#     if(ala_existe == T){
#       data = data %>% 
#         group_by(ano_mes) %>%
#         arrange(Data)%>% 
#         summarise(taxa = sum(var_indica, na.rm = T)/sum(total, na.rm = T)) 
#       
#     }else {
#       data = data %>%
#         mutate(taxa = var_indica/total) 
#     }
#     
#     data$ano_mes = factor(data$ano_mes, levels = vetor_data_levels)
#     
#     data = data %>% 
#       arrange(ano_mes) %>% 
#       mutate(porcent = paste0(round(taxa,2)*100, "%"))
#     
#     
#     
#     
#     if(nrow(data) == 0){
#       img = imagem_null(path_image)
#       
#     }else{
#       grafico = ggplot(data, aes(x = ano_mes, y = taxa, label = porcent, group = 1))+
#         geom_text(hjust = -.05, vjust = -.4)+
#         geom_line(size = 0.5, colour = "#999494")+
#         scale_y_continuous(breaks = c(0,1), limits = c(0,1))
#       
#       if(possui_meta == T){
#         grafico = grafico+
#           geom_line(aes(x = ano_mes, y = metas, colour = "#000000"))+
#           scale_color_discrete(name = "", labels = c("Meta"))+
#           theme_grafic()+
#           ggsave(paste0("Imagens/",path_image))
#         
#         
#       }else {
#         grafico = grafico+
#           theme_grafic()+
#           ggsave(paste0("Imagens/",path_image))
#         
#       }
#       
#       return(grafico)
#       
#     }
#   },
#   error = function(cond){
#     return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
#   
# }
# #GRAFICO BARRAS
# 
# bar_plot = function(data, classifica, path_image = ""){
#   
#   out = tryCatch({
#     tabela = data %>% 
#       filter(CLASSIFICA == classifica) %>% 
#       tabyl(Tipo_incidente) %>% 
#       arrange(desc("n"))
#     
#     maximo = tabela$n %>% unlist %>% max
#     
#     
#     grafico = ggplot(tabela, aes(x = reorder(Tipo_incidente,n), y = n, label = n)) +
#       geom_col(width = 0.6,stat="identity", fill="#77a8ba") +
#       geom_text(vjust=0,hjust =-0.4, size=4.8)+
#       scale_y_continuous( limits = c(0,max(tabela$n)+mean(tabela$n)))+
#       labs(x="", y="") +
#       theme_grafic(axis.text.y=element_text(size=15))+
# 
#       coord_flip()+  
#       ggsave(paste0("Imagens/",path_image), width = 158, height = 93, units = "mm")
#     
#   },
#   error = function(cond){
#     return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
# }
# 
# 
# 
# 
# #GRAFICO DONUT
# 
# donutplot <- function(data, incidente, var_principal, var_secundaria = "", filtro = "", paleta, tamanho, path_image = "", more_filtros = F){
#   
#   
#   out = tryCatch({
#     
#     if(more_filtros == T){
#       donut = data %>% 
#         rename("var_princ" = var_principal, "var_sec" = var_secundaria) %>% 
#         filter(Tipo_incidente == incidente, var_princ == filtro) %>% 
#         tabyl(var_sec)
#       
#       
#     }else{
#       donut = data %>% 
#         rename("var_sec" = var_secundaria) %>% 
#         filter(Tipo_incidente == incidente) %>% 
#         tabyl(var_sec)
#     }
#     
#     
# 
#     if(nrow(donut) == 0){
#       imagem_null(path_image)
#       
#       
#     }else{
#       # Compute the cumulative percentages (top of each rectangle)
#       donut$ymax = cumsum(donut$percent)
#       # Compute the bottom of each rectangle
#       donut$ymin = c(0, head(donut$ymax, n=-1))
#       # Compute label position
#       donut$labelPosition <- (donut$ymax + donut$ymin) / 2
#       # Compute a good label
#       donut$label <- paste0(donut[[1]], "\n ",donut$n," (", round(donut$percent*100,1),"%",")")
#       
#       ordem = donut %>% arrange(n) 
#       ordem = unique(ordem$var_sec) 
#     
#       donut$var_sec = factor(donut$var_sec,levels = ordem)
# 
#       # Make the plot
#       grafico = ggplot(donut, aes(ymax=ymax, ymin=ymin, xmax=3, xmin=2, fill=donut[[1]])) +
#         geom_rect() +
#         geom_text( x=tamanho, aes(y=labelPosition, label=label), size=4.5) + # x here controls label position (inner / outer)
#         scale_fill_brewer(palette=paleta) +
#         coord_polar(theta="y") +
#         xlim(c(0, 4)) +
#         theme_void() +
#         theme(legend.position = "none")+
#         ggsave(paste0("Imagens/",path_image))
#       
#       return(grafico)
#     }
#     
#   },
#   error = function(cond){
#     return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
#     
#   })
#   
#   
#   
#   return(out)
# }
# 
# 
# 
# ##-----------------------------------------------TABELAS----------------------------------------------####
# 
# #TABELA DE ESTILIZAÇÃO
# table_generator = function(table, caption){
# 
#   kbl(table,format = "latex", booktabs = TRUE,caption = caption) %>% 
#     kable_styling(full_width = F, position = "center",latex_options = "HOLD_position") %>% 
#     row_spec(nrow(table)-1, hline_after = T)  %>% 
#     row_spec(nrow(table), bold = T)
# }
# 
# 
# ## TABELA SIMPLES
# 
# simple_table = function(data, incidente, coluna, nome_coluna = "", caption = ""){
#   
#   
#   out = tryCatch({
#     
#     tabela = data %>%
#       rename("coluna" = coluna) %>% 
#       filter(`Tipo_incidente` == incidente)%>% 
#       group_by(coluna) %>%
#       summarise(Total = n()) %>%
#       arrange(desc(Total) ) %>%
#       na.omit()
# 
#     tabela = tabela %>%
#       bind_rows(summarise(.,
#       across(where(is.numeric), sum),
#                           across(where(is.character), ~"Total")))
#     
#     
#     colnames(tabela)[1] = nome_coluna
#     
#     table_generator(tabela, caption)
#     
# 
#     
#   },
#   
#   error = function(cond){
#     return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
#   
# }
# 
# 
# 
# ## TABELA SIMPLES com mais filtros
# simple_tableDoubleFilter = function(data, incidente, var_principal, var_secundaria, filtro, nome_coluna = "", caption = ""){
#   
#   
#   out = tryCatch({
#     
#     tabela = data %>%
#       rename("var_princ" = var_principal, "var_sec" = var_secundaria) %>% 
#       filter(Tipo_incidente == incidente & var_princ == filtro) %>% 
#       group_by(var_sec) %>% 
#       summarise(Total = n()) %>% 
#       arrange(desc(Total) ) %>%
#       na.omit()
#     
#     tabela = tabela %>%
#       bind_rows(summarise(.,
#                           across(where(is.numeric), sum),
#                           across(where(is.character), ~"Total")))
#     
#     colnames(tabela)[1] = nome_coluna
#     
#     table_generator(tabela, caption)
#     
# 
#     
#   },
#   
#   error = function(cond){
#     return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
#   
# }
# 
# 
# #TABELA DUPLA
# double_table = function(data, incidente, var_indica, var_explica, nome_coluna = "aleatorio", caption = "", gerar_kable = T){
#   
#   
#   out = tryCatch({
#     
#     tabela = data %>% 
#       #filter(mes_num <= mes) %>% 
#       rename("var_indica" = var_indica, "var_explica" = var_explica) %>% 
#       filter(`Tipo_incidente` == incidente) %>%
#       tabyl(var_indica, var_explica, show_missing_levels = T) 
#     
#     #quando não tiver observações no filtro selecionado
#     if(nrow(tabela) == 0){
# 
# 
#       #essa estrutura de condição foi feita para pegar os valores únicos da variável explicativa
# 
#       #como o mês é uma coluna que necessita de uma ordem, foi criado esse if, que pega
#       #do arquivo utils, um vetor com os meses
#       if(var_explica == "mes_ext"){
# 
#         colunas = meses_anos(mes,anos)
# 
#       }else{
# 
#         colunas = unique(data[var_explica])  %>% unlist
#       }
#       
#       
#       
# 
# 
#       #já que não existem observações no banco, dado o filtro selecionado,
#       #cria-se um vetor de tamanho "n", com todas as observações sendo 0,
#       #e "n" sendo igual ao número de valores únicos que aquela coluna apresenta
#       valores = rep(0,length(colunas))
# 
# 
#       #cria-se o dataframe de acordo com os valores e colunas declarados anteriormente
#       tabela = data.frame(valores) %>% t %>% data.frame
#       colnames(tabela) = colunas
# 
# 
#       #faz-se a soma das linhas para criar a coluna "Total"
#       tabela = tabela %>%
#         adorn_totals(where = "col")
# 
#       #cria-se uma coluna com o nome da variável(apresentada como parâmetro),
#       #onde essa vai ser o índice
#       tabela[nome_coluna] = "Total"
# 
#       #muda a ordem das colunas, já que a coluna que apresenta o nome que vai ser o
#       #indice está no local errado
#       tabela = tabela %>%
#         select(nome_coluna, colunas, Total)
# 
# 
#       #define a coluna "nome_coluna" como o índice
#       rownames(tabela) <- tabela$nome_coluna
#       
#       colnames(tabela)[1] = nome_coluna
# 
#     }else{
# 
#       #caso tenha valores ele roda isso :)
#       tabela = tabela %>%
#         adorn_totals(c("row", "col")) 
#         
#         if(var_explica == "mes_ext"){
#           tabela = tabela %>% 
#             select(-na.rm(meses[(mes+1):12]))
#           
#           tabela = tabela[tabela$var_indica != "Total",colnames(tabela) != "Total"] %>%
#             adorn_totals(c("row", "col"))
#             
#         }
#         
#       tabela = tabela %>%
#           row_zeros
# 
#       
#     }
# 
# 
#     #gera a tabela formatada pelo pacote kable
#     #
#     
#     if(gerar_kable == T){
#       
#       colnames(tabela)[1] = ""
#       return(table_generator(tabela, caption)) 
#       
#     }else{
#       return(tabela) 
#     }
#     
#     
#   },
#   error = function(cond){
#     
#     return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
# }
# 
# 
# 
# 
# 
# #TABELAS MULTIPLAS
# multiple_tables = function(data, var_indica, var_explica){
#   
#   out = tryCatch({
#     
#     
#     data_geral = data %>% 
#       rename("var_indica" = var_indica, "var_explica" = var_explica) %>% 
#       filter(mes_num <= mes) %>% 
#       tabyl(var_indica, mes_ext, var_explica, show_missing_levels = T) %>% 
#       adorn_totals(c("row", "col"))
#     
#     data_list = list()
#     cont = 1
#     
#     for(dataframe in data_geral){
#       dataframe = dataframe %>%
#         #caso queira pegar só os meses do primeiro semestre
#         select(-na.rm(meses[(mes+1):12])) %>%
#         row_zeros
#       
#       colnames(dataframe)[1] = names(data_geral)[cont]
#       
#       data_list[[cont]] = dataframe
#       cont = cont + 1 
#       
#     }
#     
#     return(data_list)
#     
#   },
#   error = function(cond){
#     return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
# }  
# 
# 
# tabela_taxas = function(data, var1, var2, caption){
#   
#   out = tryCatch({
#     
#     tabela = data %>% 
#       rename(numerador = var1, denominador = var2, periodo = Data) %>% 
#       replace_na(list(numerador= 0, denominador= 0)) %>%
#       group_by(mes_ext) %>%
#       summarise(numerador = sum(as.numeric(numerador), na.rm = T),
#                 denominador = sum(as.numeric(denominador), na.rm = T)) %>%
#       mutate(taxa = as.numeric(numerador)/as.numeric(denominador)) %>%
#        #replace_na(list(`taxa`=0)) %>%
#        #arrange(periodo) %>%
#       mutate(porcent = paste0(round(taxa,2)*100, "%")) %>%
#       mutate(porcent = str_replace(porcent, "NA%|NaN%","-")) %>%
#       select(mes_ext, porcent) %>%
#       pivot_wider(names_from = mes_ext, values_from = porcent)
#        
#       
#     
#     #return(tabela)
#    
#     
#     # #colocando "-" nos valores que ainda não existem
#     mes_atual = Sys.Date() %>% month(label = T) %>% as.character
#     index_mes =  grep(mes_atual,colnames(tabela))
# 
#     for(i in index_mes:12){
#       tabela[i] = "-"
#     }
# 
# 
#     tabela = knitr::kable(tabela,format = "latex", booktabs = TRUE,caption = caption) %>%
#       kable_styling(full_width = T, position = "center",latex_options = "HOLD_position")
# 
# 
#     return(tabela)
#   },
#   error = function(cond){
#     return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
#     
#   })
#   
#   
#   return(out)
#   
# }
# 
# 
# 
# 
# tabela_taxas_multiplas = function(data, var1, var2, caption){
#   out = tryCatch({
#     
#     tabela = data %>% 
#       rename(numerador = var1, denominador = var2, periodo = Data) %>% 
#       replace_na(list(numerador= 0, denominador= 0)) %>%
#       group_by(Programa) %>% 
#       mutate(taxa = as.numeric(numerador)/as.numeric(denominador)) %>%
#       #replace_na(list(`taxa`=0)) %>%
#       arrange(periodo) %>%
#       mutate(porcent = paste0(round(taxa,2)*100, "%")) %>% 
#       mutate(porcent = str_replace(porcent, "NA%|NaN%","-")) %>% 
#       select(mes_ext, porcent,Programa) %>%
#       pivot_wider(names_from = mes_ext, values_from = porcent)
#     
#     
#     
#     #colocando "-" nos valores que ainda não existem
#     mes_atual =  month(mes,label = T) %>% as.character
#     index_mes =  grep(mes_atual,colnames(tabela))
#     
#     for(i in index_mes:13){
#       tabela[i] = rep("-", nrow(tabela))
#     }
#     
#     
#     tabela = kbl(tabela,format = "latex", booktabs = TRUE,caption = caption) %>%
#       kable_styling(full_width = F, position = "center",latex_options = c("scale_down","HOLD_position")) %>%
#       row_spec(c(1,3,5), color = "black",background = "lightgray") %>%
#       column_spec(1, width = "4cm")
#     
#     
#     return(tabela)
#     
#   },
#   error = function(cond){
#     return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
#     
#   })
#   
#   return(out)
#   
# }
# 
# list.files()
# 
# nome_arquivo = "1estagio" 
# 
# relatorios = c("notificacao", "seguranca")
# tipo_arquivo = c(".pdf", ".Rmd")
# 
# 
# remove_arquivo_futil = function(nome_arquivo, relatorios, tipo_arquivo){
#   
#   
#   nome_arquivo = paste0("_",nome_arquivo)
#   arquivos = c()
#   
#   for(tipo in tipo_arquivo){
#     for(relatorio in relatorios){
#       arquivo = paste0(relatorio, nome_arquivo, tipo)
#       arquivos = c(arquivos, arquivo)
#     }
#   }
#   
#   
#   arquivos = c("Estilo.sty", "Imagens", arquivos)
#   
#   return(arquivos)
#   
#   
# }
# 
# util = remove_arquivo_futil(nome_arquivo = "1estagio" ,
#                             relatorios = c("notificacao", "seguranca"),
#                             tipo_arquivo = c(".pdf", ".Rmd"))
# 
# arquivos = list.files()
# 
# 
# futil = arquivos[!(arquivos %in% util)]
# 
# sapply(futil, file.remove)
