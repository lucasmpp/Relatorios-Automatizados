
pacman::p_load("magrittr","tidyverse","lubridate","janitor","xcolor","png","formattable")




## Tema dos Gráficos ####
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


##-----------------------------------------------GRÁFICOS-----------------------------------------------####

grafico_linhas = function(data, variavel, path_image = "", title = "", metas = -1){
  
  out = tryCatch({
    
    data = data %>% 
      rename("taxa" = variavel) %>% 
      mutate(periodo = Data) %>% 
      filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
      mutate(taxa = formattable(taxa,digits = 1, format = 'f'), taxa_texto = as.character(taxa)) %>% 
      mutate(taxa_texto = str_replace(taxa_texto, "\\.",",")) %>% 
      mutate(periodo = ano_mes)
    
    maximo = max(data$taxa %>% unlist) 
    data$periodo = factor(data$periodo, levels = data$periodo)
    
    
    ggplot(data, aes(x = `periodo`, y = taxa, label = taxa_texto, group = 1))+
      geom_text(hjust = -.05, vjust = -.4)+
      geom_line(size = 0.5, color = "#999494")+
      scale_y_continuous(breaks = c(0,maximo+10), limits = c(0,maximo+10))+
      geom_line(aes(x = periodo, y = metas, color = "blue"))+
      scale_color_discrete(name = "", labels = c("Meta"))+
      theme_grafic()+
      ggsave(paste0("Imagens/",path_image))
    
    
  },
  error = function(cond){
    return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
  
}

#GRAFICO TAXAS

grafico_taxa<- function(data_num, incidente, periodo_num, data_den, var_den, periodo_den, var_base = "CD_INCIDENTE",
                        more_filtros = F, filtro_extra = "", var_extra = "", path_image = "", divisor = 100, 
                        vetor_metas = c(1,-1), y_max = 100){
  
  out = tryCatch({  
    
    convert_porcent = function(valor){
      ifelse(divisor == 100, paste0(valor ,"%"),valor)
    }
    
    vetor_data_levels = data_den$ano_mes %>% unique
    
    denominador = data_den %>% 
      rename("periodo" = periodo_den, "var_den" = var_den) %>% 
      filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
      group_by(ano_mes) %>%
      summarise(total = sum(var_den))
    
    if(more_filtros == T){
      
      numerador = data_num %>% 
        rename("periodo" = periodo_num, "var_extra" = var_extra) %>%
        filter(Tipo_incidente == incidente) %>%
        filter(var_extra == filtro_extra)
      
    }else{
      
      numerador = data_num %>% 
        rename("periodo" = periodo_num) %>% 
        filter(Tipo_incidente == incidente) 
      
    }
    
    
    
    numerador = numerador %>%
      rename("var_base_rename" = var_base) %>% 
      filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
      filter(!is.na(var_base_rename)) %>%
      group_by(ano_mes) %>%
      summarise(n = n())
    
    
    #return(numerador)
    
    data = left_join(denominador,numerador, by = 'ano_mes',) %>%
      mutate(n = replace_na(n,0)) %>%
      mutate(taxa = formattable((n/total)*divisor,digits =  1, format = 'f')) %>%
      mutate(taxa_texto = str_replace(taxa, "\\.",",")) %>%
      mutate(taxa_texto = sapply(taxa_texto, convert_porcent))
    
    data$ano_mes = ordered(data$ano_mes, levels = vetor_data_levels)
    
    
    max_value_taxa = 1.5*(max(data$taxa) + mean(data$taxa))
    lim_superior = ifelse(divisor == 1000, max_value_taxa, y_max)
    
    
    if(nrow(denominador)==0){
      img = imagem_null(path_image)
      
    }else{
      grafico = ggplot(data, aes(x = ano_mes, y = taxa ,label = taxa_texto, group = 1))+
        geom_text(hjust = -.05, vjust = -.4)+
        geom_line(size = 0.5, color = "#999494")+
        geom_line(data = data, aes(x = ano_mes, y = vetor_metas[1]),size = 0.5, color = "red")+
        geom_line(data = data, aes(x = ano_mes, y = vetor_metas[2]),size = 0.5, color = "red")+
        scale_color_discrete(name = "", labels = c("Meta"))+
        theme_grafic()+
        scale_y_continuous(limits = c(0,lim_superior))
      
      
      
      ggsave(paste0("Imagens/",path_image),plot = grafico)
    }
    
    return(grafico)
  },
  error = function(cond){
    return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
  
}


limites = function(grafico, divisor){
  
  if(divisor == 100){
    grafico+
      scale_y_continuous(limits = c(0,1))
    
  }else{
    grafico
  }
}
#GRAFICO DAS METAS
grafico_metas2 = function(data, var_indica, total, path_image = "", title = "", ala_existe = T, possui_meta = T, metas = -1){
  
  out = tryCatch({
    
    data = data %>% 
      mutate(periodo = Data) %>% 
      #filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
      rename("var_indica" = var_indica, "total" = total) 
    
    vetor_data_levels = data$ano_mes %>% unique
    
    #alguns programas apresentam diferentes tipos de alas, por isso foi feito 
    #essa estrutura de condição para pegar esses casos
    
    if(ala_existe == T){
      data = data %>% 
        group_by(ano_mes) %>%
        arrange(Data)%>% 
        summarise(taxa = sum(var_indica, na.rm = T)/sum(total, na.rm = T)) 
      
    }else {
      data = data %>%
        mutate(taxa = var_indica/total) 
    }
    
    data$ano_mes = factor(data$ano_mes, levels = vetor_data_levels)
    
    data = data %>% 
      arrange(ano_mes) %>% 
      mutate(porcent = paste0(round(taxa,2)*100, "%"))
    
    
    
    
    if(nrow(data) == 0){
      img = imagem_null(path_image)
      
    }else{
      grafico = ggplot(data, aes(x = ano_mes, y = taxa, label = porcent, group = 1))+
        geom_text(hjust = -.05, vjust = -.4)+
        geom_line(size = 0.5, colour = "#999494")+
        scale_y_continuous(breaks = c(0,1), limits = c(0,1))
      
      if(possui_meta == T){
        grafico = grafico+
          geom_line(aes(x = ano_mes, y = metas, colour = "#000000"))+
          scale_color_discrete(name = "", labels = c("Meta"))+
          theme_grafic()+
          ggsave(paste0("Imagens/",path_image))
        
        
      }else {
        grafico = grafico+
          theme_grafic()+
          ggsave(paste0("Imagens/",path_image))
        
      }
      
      return(grafico)
      
    }
  },
  error = function(cond){
    return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
}

#GRAFICO DAS METAS
grafico_metas = function(data, var_indica, total, path_image = "", title = "", ala_existe = T, possui_meta = T, metas = -1){
  
  out = tryCatch({
    
    data = data %>% 
      mutate(periodo = Data) %>% 
      filter((periodo >= periodo_inicial) & (periodo <= periodo_final)) %>%
      rename("var_indica" = var_indica, "total" = total) 
    
    vetor_data_levels = data$ano_mes %>% unique
    
    #alguns programas apresentam diferentes tipos de alas, por isso foi feito 
    #essa estrutura de condição para pegar esses casos
    
    if(ala_existe == T){
      data = data %>% 
        group_by(ano_mes) %>%
        arrange(Data)%>% 
        summarise(taxa = sum(var_indica, na.rm = T)/sum(total, na.rm = T)) 
      
    }else {
      data = data %>%
        mutate(taxa = var_indica/total) 
    }
    
    data$ano_mes = factor(data$ano_mes, levels = vetor_data_levels)
    
    data = data %>% 
      arrange(ano_mes) %>% 
      mutate(porcent = paste0(round(taxa,2)*100, "%"))
    
    
    
    
    if(nrow(data) == 0){
      img = imagem_null(path_image)
      
    }else{
      grafico = ggplot(data, aes(x = ano_mes, y = taxa, label = porcent, group = 1))+
        geom_text(hjust = -.05, vjust = -.4)+
        geom_line(size = 0.5, colour = "#999494")+
        scale_y_continuous(breaks = c(0,1), limits = c(0,1))
      
      if(possui_meta == T){
        grafico = grafico+
          geom_line(aes(x = ano_mes, y = metas, colour = "#000000"))+
          scale_color_discrete(name = "", labels = c("Meta"))+
          theme_grafic()+
          ggsave(paste0("Imagens/",path_image))
        
        
      }else {
        grafico = grafico+
          theme_grafic()+
          ggsave(paste0("Imagens/",path_image))
        
      }
      
      return(grafico)
      
    }
  },
  error = function(cond){
    return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
}
#GRAFICO BARRAS

bar_plot = function(data, classifica, path_image = ""){
  
  out = tryCatch({
    tabela = data %>% 
      filter(CLASSIFICA == classifica) %>% 
      tabyl(Tipo_incidente) %>% 
      arrange(desc("n"))
    
    maximo = tabela$n %>% unlist %>% max
    
    
    grafico = ggplot(tabela, aes(x = reorder(Tipo_incidente,n), y = n, label = n)) +
      geom_col(width = 0.8,stat="identity", fill="#77a8ba") +
      geom_text(vjust=0,hjust =-0.4, size=4.8)+
      scale_y_continuous( limits = c(0,max(tabela$n)+mean(tabela$n)))+
      labs(x="", y="") +
      theme_grafic(axis.text.y=element_text(size=15))+
      
      coord_flip()+  
      ggsave(paste0("Imagens/",path_image), width = 158, height = 93, units = "mm")
    
  },
  error = function(cond){
    return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
    
  })
  
  return(out)
}




#GRAFICO DONUT

donutplot <- function(data, incidente, var_principal, var_secundaria = "", filtro = "", paleta, tamanho, path_image = "", more_filtros = F){
  
  
  out = tryCatch({
    
    if(more_filtros == T){
      donut = data %>% 
        rename("var_princ" = var_principal, "var_sec" = var_secundaria) %>% 
        filter(Tipo_incidente == incidente, var_princ == filtro) %>% 
        tabyl(var_sec)
      
      
    }else{
      donut = data %>% 
        rename("var_sec" = var_secundaria) %>% 
        filter(Tipo_incidente == incidente) %>% 
        tabyl(var_sec)
    }
    
    
    
    if(nrow(donut) == 0){
      imagem_null(path_image)
      
      
    }else{
      # Compute the cumulative percentages (top of each rectangle)
      donut$ymax = cumsum(donut$percent)
      # Compute the bottom of each rectangle
      donut$ymin = c(0, head(donut$ymax, n=-1))
      # Compute label position
      donut$labelPosition <- (donut$ymax + donut$ymin) / 2
      # Compute a good label
      donut$label <- paste0(donut[[1]], "\n ",donut$n," (", round(donut$percent*100,1),"%",")")
      
      ordem = donut %>% arrange(n) 
      ordem = unique(ordem$var_sec) 
      
      donut$var_sec = factor(donut$var_sec,levels = ordem)
      
      # Make the plot
      grafico = ggplot(donut, aes(ymax=ymax, ymin=ymin, xmax=3, xmin=2, fill=donut[[1]])) +
        geom_rect() +
        geom_text( x=tamanho, aes(y=labelPosition, label=label), size=4.5) + # x here controls label position (inner / outer)
        scale_fill_brewer(palette=paleta) +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_void() +
        theme(legend.position = "none")+
        ggsave(paste0("Imagens/",path_image))
      
      return(grafico)
    }
    
  },
  error = function(cond){
    return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
    
  })
  
  
  
  return(out)
}



taxa_eventos_adversos = function(data_den, var_den, path_image = "" ){
  out = tryCatch({
    convert_porcent = function(valor){
      paste0(valor ,"%")
    }
    
    numerador = ler_excel("../Notificações.xlsx") %>% 
      filter(str_detect(DS_LOCAL,"LN|LAGO NORTE", negate = T)) %>% 
      filter((Data >= periodo_inicial) & (Data <= periodo_final)) %>%
      filter(PROGRAMA != "Externo") %>% 
      filter(CLASSIFICA == "Evento Adverso") %>% 
      filter(Tipo_incidente %in% c("Queda","Lesão por pressão","Medicamento","Procedimento cirúrgico")) %>% 
      filter(CRG_fase_pos_oper == "Deiscência" | CRG_fase_pos_oper == "Hemorragia" | is.na(CRG_fase_pos_oper)) %>% 
      filter(CRG_fase_intra_oper == "Lesão de órgão durante a cirurgia"|CRG_fase_intra_oper == "Retenção não intencional de corpo estranho"|is.na(CRG_fase_intra_oper)) %>% 
      filter(LPRES_Tipo == "Em proeminências ósseas"|is.na(LPRES_Tipo)) %>% 
      group_by(ano_mes) %>% 
      summarise(n = n())
    
    denominador = data_den
    
    vetor_data_levels = data_den$ano_mes %>% unique
    
    data_den = data_den %>% 
      rename("var" = var_den) %>% 
      group_by(ano_mes) %>% 
      summarise(soma = sum(var))
    
    
    data = merge(data_den,numerador, by = 'ano_mes',) %>%
      mutate(taxa = round((n/soma)*100, 1)) %>% 
      mutate(taxa_texto = str_replace(taxa, "\\.",",")) %>%
      mutate(taxa_texto = sapply(taxa_texto, convert_porcent))
    
    data$ano_mes = ordered(data$ano_mes, levels = vetor_data_levels)
    
    max_value_taxa = 1.5 * (max(data$taxa) + mean(data$taxa))
    lim_superior = 5
    
    grafico = ggplot(data, aes(x = ano_mes, y = taxa,label = taxa_texto , group = 1))+
      geom_text(hjust = -.05, vjust = -.4)+
      geom_line(size = 0.5, color = "#999494")+
      theme_grafic()+
      scale_y_continuous(limits = c(0,lim_superior))+
      ggsave(paste0("Imagens/",path_image))
    
    
    
  },
  error = function(cond){
    return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
  
}



taxa_incidentes_semdano = function(data_den, var_den, path_image = "" ){
  out = tryCatch({
    convert_porcent = function(valor){
      paste0(valor ,"%")
    }
    
    numerador = ler_excel("../Notificações.xlsx") %>% 
      filter(str_detect(DS_LOCAL,"LN|LAGO NORTE", negate = T)) %>% 
      filter((Data >= periodo_inicial) & (Data <= periodo_final)) %>%
      filter(PROGRAMA != "Externo") %>% 
      filter(CLASSIFICA == "Incidente sem dano") %>% 
      filter(Tipo_incidente %in% c("Queda","Medicamento","Procedimento cirúrgico")) %>% 
      filter(QUED_Dano == "Sem dano") %>% 
      group_by(ano_mes) %>% 
      summarise(n = n())
    
    denominador = data_den
    
    vetor_data_levels = data_den$ano_mes %>% unique
    
    data_den = data_den %>% 
      rename("var" = var_den) %>% 
      group_by(ano_mes) %>% 
      summarise(soma = sum(var))
    
    
    data = merge(data_den,numerador, by = 'ano_mes',) %>%
      mutate(taxa = round((n/soma)*100, 1)) %>% 
      mutate(taxa_texto = str_replace(taxa, "\\.",",")) %>%
      mutate(taxa_texto = sapply(taxa_texto, convert_porcent))
    
    data$ano_mes = ordered(data$ano_mes, levels = vetor_data_levels)
    
    max_value_taxa = 1.5 * (max(data$taxa) + mean(data$taxa))
    lim_superior = 5
    
    grafico = ggplot(data, aes(x = ano_mes, y = taxa,label = taxa_texto , group = 1))+
      geom_text(hjust = -.05, vjust = -.4)+
      geom_line(size = 0.5, color = "#999494")+
      theme_grafic()+
      scale_y_continuous(limits = c(0,lim_superior))+
      ggsave(paste0("Imagens/",path_image))
    
    
  },
  error = function(cond){
    return(paste("Esse gráfico não pode ser mostrado pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
  
}

