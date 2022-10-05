
pacman::p_load("knitr","magrittr","tidyverse","lubridate","janitor","kableExtra")



##-----------------------------------------------TABELAS----------------------------------------------####

#TABELA DE ESTILIZAÇÃO
table_generator = function(table, caption, latex_options = c("HOLD_position") ){
  
  kbl(table,format = "latex", booktabs = TRUE,caption = caption) %>% 
    kable_styling(full_width = F, position = "center",latex_options = latex_options) %>% 
    row_spec(nrow(table)-1, hline_after = T)  %>% 
    row_spec(nrow(table), bold = T)
}


## TABELA SIMPLES

simple_table = function(data, incidente, coluna, nome_coluna = "", caption = ""){
  
  
  out = tryCatch({
    
    tabela = data %>%
      rename("coluna" = coluna) %>% 
      filter(`Tipo_incidente` == incidente)%>% 
      group_by(coluna) %>%
      summarise(Total = n()) %>%
      arrange(desc(Total) ) %>%
      na.omit()
    
    tabela = tabela %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~"Total")))
    
    
    colnames(tabela)[1] = nome_coluna
    
    table_generator(tabela, caption)
    
    
    
  },
  
  error = function(cond){
    return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
}



## TABELA SIMPLES com mais filtros
simple_tableDoubleFilter = function(data, incidente, var_principal, var_secundaria, filtro, nome_coluna = "", caption = ""){
  
  
  out = tryCatch({
    
    tabela = data %>%
      rename("var_princ" = var_principal, "var_sec" = var_secundaria) %>% 
      filter(Tipo_incidente == incidente & var_princ == filtro) %>% 
      group_by(var_sec) %>% 
      summarise(Total = n()) %>% 
      arrange(desc(Total) ) %>%
      na.omit()
    
    tabela = tabela %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~"Total")))
    
    colnames(tabela)[1] = nome_coluna
    
    table_generator(tabela, caption)
    
    
    
  },
  
  error = function(cond){
    return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
}


#TABELA DUPLA
double_table = function(data, incidente, var_indica, var_explica, nome_coluna = "aleatorio", caption = "", gerar_kable = T){
  
  
  out = tryCatch({
    
    tabela = data %>% 
      rename("var_indica" = var_indica, "var_explica" = var_explica) %>% 
      filter(`Tipo_incidente` == incidente) %>%
      tabyl(var_indica, var_explica, show_missing_levels = T) 
    
      #quando não tiver observações no filtro selecionado
    
    if(nrow(tabela) == 0){
      
      
      #essa estrutura de condição foi feita para pegar os valores únicos da variável explicativa
      
      #como o mês é uma coluna que necessita de uma ordem, foi criado esse if, que pega
      #do arquivo utils, um vetor com os meses
      
      if(var_explica == "mes_ext"){
        
        colunas = meses_anos(mes,anos)
        
      }else{
        
        colunas = unique(data[var_explica])  %>% unlist
      }
      
      #já que não existem observações no banco, dado o filtro selecionado,
      #cria-se um vetor de tamanho "n", com todas as observações sendo 0,
      #e "n" sendo igual ao número de valores únicos que aquela coluna apresenta
      
      valores = rep(0,length(colunas))
      
      #cria-se o dataframe de acordo com os valores e colunas declarados anteriormente
      tabela = data.frame(valores) %>% t %>% data.frame
      colnames(tabela) = colunas
      
      
      #faz-se a soma das linhas para criar a coluna "Total"
      tabela = tabela %>%
        adorn_totals(where = "col")
      
      #cria-se uma coluna com o nome da variável(apresentada como parâmetro),
      #onde essa vai ser o índice
      tabela[nome_coluna] = "Total"
      
      #muda a ordem das colunas, já que a coluna que apresenta o nome que vai ser o
      #indice está no local errado
      tabela = tabela %>%
        select(nome_coluna, colunas, Total)
      
      
      #define a coluna "nome_coluna" como o índice
      rownames(tabela) <- tabela$nome_coluna
      
      colnames(tabela)[1] = nome_coluna
      
    }else{
      
      #caso tenha valores ele roda isso :)
      tabela = tabela %>%
        adorn_totals(c("row", "col")) 
      
      if(var_explica == "mes_ext"){
        tabela = tabela %>% 
          select(-na.rm(meses[mes+1:12]))
        
        tabela = tabela[tabela$var_indica != "Total",colnames(tabela) != "Total"] %>%
          adorn_totals(c("row", "col"))
        
      }
      
      tabela = tabela %>%
        row_zeros
      
      
    }
    
    
    #gera a tabela formatada pelo pacote kable
    #
    
    if(gerar_kable == T){
      
      colnames(tabela)[1] = ""
      return(table_generator(tabela, caption, latex_options = c("HOLD_position", "scale_down"))) 
      
    }else{
      return(tabela) 
    }
    
    
  },
  error = function(cond){
    
    return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
    
  })
  
  return(out)
}





#TABELAS MULTIPLAS
multiple_tables = function(data, var_indica, var_explica){
  
  out = tryCatch({
    
    
    data_geral = data %>% 
      filter(( Data>= periodo_inicial) & (Data<= periodo_final)) %>% 
      rename("var_indica" = var_indica, "var_explica" = var_explica) %>% 
      tabyl(var_indica, ano_mes, var_explica, show_missing_levels = T) 
    

    data_list = list()
    cont = 1
    
    # for(dataframe in data_geral){
    #   dataframe = dataframe %>%
    #     adorn_totals(c("col")) %>% 
    #     arrange(desc(Total)) %>% 
    #     adorn_totals(c("row")) %>% 
    #     select("var_indica", meses_anos(mes, anos),"Total") %>%
    #     mutate(taxa = round(Total/(sum(Total)/2),2)*100) %>%
    #     mutate(taxa = str_replace(taxa, "\\.",",")) %>%
    #     mutate(taxa = paste0(taxa,"%")) %>% 
    #     rename("%" = taxa) %>% 
    #     row_zeros
    #   
    #   colnames(dataframe)[1] = names(data_geral)[cont]
    #   
    #   data_list[[cont]] = dataframe
    #   cont = cont + 1 
    #   
    # }
    
    
    
    for(dataframe in data_geral){
      dataframe = dataframe %>%
        adorn_totals(c("col")) %>% 
        arrange(desc(Total)) %>% 
        adorn_totals(c("row"))
      
      cols_mes = meses_anos(mes, anos)
      cols_names = colnames(dataframe)
      
      
      meses_faltantes = cols_mes[!(cols_mes %in% cols_names)]
      meses_faltantes = c(meses_faltantes)
      
      for(col_name in meses_faltantes){
        dataframe[col_name] = rep(0, nrow(dataframe))
      }
      
      dataframe = dataframe %>%
        select("var_indica", meses_anos(mes, anos),"Total") %>%
        mutate(taxa = round(Total/(sum(Total)/2),2)*100) %>%
        mutate(taxa = str_replace(taxa, "\\.",",")) %>%
        mutate(taxa = paste0(taxa,"%")) %>%
        rename("%" = taxa) %>%
        row_zeros

      colnames(dataframe)[1] = names(data_geral)[cont]

      data_list[[cont]] = dataframe
      cont = cont + 1
      
    }
    
    return(data_list)
    
  },
  error = function(cond){
    return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
    
  })
  
  return(out)
}  


tabela_taxas = function(data, var1, var2, caption){
  
  out = tryCatch({
    
    tabela = data %>% 
      rename(numerador = var1, denominador = var2, periodo = Data) %>% 
      replace_na(list(numerador= 0, denominador= 0)) %>%
      group_by(mes_ext) %>%
      summarise(numerador = sum(as.numeric(numerador), na.rm = T),
                denominador = sum(as.numeric(denominador), na.rm = T)) %>%
      mutate(taxa = as.numeric(numerador)/as.numeric(denominador)) %>%
      mutate(porcent = paste0(round(taxa,2)*100, "%")) %>%
      mutate(porcent = str_replace(porcent, "NA%|NaN%","-")) %>%
      select(mes_ext, porcent) %>%
      pivot_wider(names_from = mes_ext, values_from = porcent)
    
    
    
    #return(tabela)
    
    
    # #colocando "-" nos valores que ainda não existem
    mes_atual = Sys.Date() %>% month(label = T) %>% as.character
    
    #mes_atual = mes
    index_mes =  grep(mes_atual,colnames(tabela))
    
    for(i in index_mes:12){
      tabela[i] = "-"
    }
    
    tabela = knitr::kable(tabela,format = "latex", booktabs = TRUE,caption = caption) %>%
      kable_styling(full_width = T, position = "center",latex_options = "HOLD_position")
    
    return(tabela)
  },
  error = function(cond){
    return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
}



tabela_taxas_multiplas = function(data, var1, var2, caption){
  out = tryCatch({
    
    tabela = data %>% 
      rename(numerador = var1, denominador = var2, periodo = Data) %>% 
      replace_na(list(numerador= 0, denominador= 0)) %>%
      group_by(Programa) %>% 
      mutate(taxa = as.numeric(numerador)/as.numeric(denominador)) %>%
      #replace_na(list(`taxa`=0)) %>%
      arrange(periodo) %>%
      mutate(porcent = paste0(round(taxa,2)*100, "%")) %>% 
      mutate(porcent = str_replace(porcent, "NA%|NaN%","-")) %>% 
      select(mes_ext, porcent,Programa) %>%
      pivot_wider(names_from = mes_ext, values_from = porcent)
    
   #return(tabela)
    
    
    #colocando "-" nos valores que ainda não existem
    mes_atual =  month(mes,label = T) %>% as.character
    index_mes =  grep(mes_atual,colnames(tabela))
    
    num_col = length(colnames(tabela))  
    
    
    
    while(index_mes < num_col){
    tabela[index_mes+1] = rep("-", nrow(tabela))
    
    index_mes = index_mes + 1
    
    }
  
    
    #return(index_mes)
    
    
    tabela = kbl(tabela,format = "latex", booktabs = TRUE,caption = caption) %>%
      kable_styling(full_width = F, position = "center",latex_options = c("scale_down","HOLD_position")) %>%
      row_spec(c(1,3,5), color = "black",background = "lightgray") %>%
      column_spec(1, width = "4cm")
    
    
    return(tabela)
    
  },
  error = function(cond){
    return(paste("Essa tabela não pode ser mostrada pelo seguinte erro:",cond))
    
  })
  
  return(out)
  
}



##CRIA UM DATA ESPECÍFICO PARA QUEDAS
data_quedas = function(quedas, mes, anos, caption){
  
  quedas_nulo = gerar_data_nulo(vetor_linha = c("Evento Adverso","Incidente sem dano"),
                                vetor_coluna = c("var_indica", meses_anos(mes,anos)))
  
  quedas = quedas[!quedas$var_indica == "Total",]
  
  
  if(nrow(quedas) == 0){
    
  df = quedas_nulo %>% 
    adorn_totals(c("col","row"))
    
  }else{
    
    #data_quedas = sapply(data_quedas[,-1], as.numeric)
    quedas = as.data.frame(quedas)
    # data_quedas[,-1] = sapply(data_quedas[,-1], as.numeric)
    
   df = bind_rows(quedas,quedas_nulo)%>%
       group_by(var_indica) %>%
       summarise_all(funs(sum(., na.rm = TRUE))) %>%
       select("var_indica", meses_anos(mes, anos)) %>%
       adorn_totals(c("col","row"))
    
    
  }
  
  
  
  colnames(df)[1] = ""
  # return(df)

 kbl( df ,format = "latex", booktabs = TRUE, caption = caption) %>%
    kable_styling(full_width = F, position = "center",latex_options = c("scale_down","HOLD_position")) %>%
    row_spec(nrow(df)-1, hline_after = T)  %>%
    row_spec(nrow(df), bold = T)
  

  
  
}


