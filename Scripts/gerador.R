#setwd("Z:/Nucleo de Segurança do Paciente/Relatórios assistencia/Programas/Pediatria")

diretorio = "Z:/Nucleo de Segurança do Paciente/Relatórios assistencia/Programas/"
pastas<- c("Pediatria" = "seguranca_pediatria")




for(pasta in names(pastas)){
  
  wd = paste0(diretorio,pasta)
  path = paste0( pastas[pasta],".Rmd")
  
  setwd(wd)
  
  rmarkdown::render(path, "pdf_document")
  
}













setwd("Z:/Nucleo de Segurança do Paciente/Relatórios assistencia/Programas/Pediatria")

rmarkdown::render("seguranca_pediatria.Rmd","pdf_document",output_format = "pdf_document", )
