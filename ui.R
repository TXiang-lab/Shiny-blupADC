# Load packages
library(shiny)
library(purrr)
library(bs4Dash)
library(echarts4r)
library(thematic)
library(waiter)
library(data.table)
library(markdown)
library(blupADC)

options(shiny.maxRequestSize = 300 * 1024^2)

thematic_shiny()


# echarts4r theme #3d444c
echarts_dark_theme <- list(
  options = '{
    "color":["#6610f2", "#ffc107", "#e83e8c", "#ff851b", "#17a2b8", "#3d9970"], 
    "backgroundColor": "#343a40", 
    "textStyle": {
        color: "#fff"
    }
  }',
  name = "dark_theme"
)

#' Format_tab ----
Format_tab<- tabItem(
 tabName ="format",
 
  fluidRow(
 
    box(
  title = "Input data format", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  radioButtons("input_data_format",NULL, c("Hapmap","Plink","Numeric","BLUPF90","VCF","Haplotype"),selected="Hapmap")
  ),
  column(width=4,
  uiOutput("input_data_dynamic")),
    box(
  title = "Output data format", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  checkboxGroupInput("output_data_format","Output_data_format", c("Hapmap","Plink","Numeric","BLUPF90","VCF","Haplotype"),selected="Plink")
  )), 
  fluidRow(
  column(6,actionButton(
    inputId = "format_button",
    label = "Start format conversion",
    icon = icon("users"),
    width = NULL,
    status = "primary",
    style = "margin: auto"
  ))), 
fluidRow(
   column(width=12,
  mainPanel(verbatimTextOutput("console"))
  ) 
 ), 

fluidRow(
  downloadButton("download_vcf", "Download VCF", class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("download_hapmap", "Download Hapmap format data",class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),
  
fluidRow(  
  downloadButton("download_numeric", "Download Numeric format data",class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),  
   
  downloadButton("download_blupf90", "Download BLUPF90 format data",class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),

 fluidRow( 
  downloadButton("download_plink_map", "Download Plink-map format data",class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  downloadButton("download_plink_ped", "Download Plink-ped format data",class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),
  
fluidRow(    
  downloadButton("download_hap_num", "Download Haplotype-hap format data",class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  downloadButton("download_hap_map", "Download Haplotype-map format data",class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  downloadButton("download_hap_sample", "Download Haplotype-sample format data",class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")))
  )

###################################################################
###                                                             ###
###                                                             ###
### QC and Imputation                                           ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

#' QC_imputation_tab ----
QC_imputation_tab<- tabItem(
 tabName ="qc_imputation",
  fluidRow(
  box( 
  title = "Default parameters1", height=300,
      closable = FALSE,  #是否可移除
      width = 4,
      status ="olive", 
	 background = "olive",
	 solidHeader = TRUE,
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
     textInput("qc_missing_value","Missing values in genotype",""),
     numericInput("qc_cpu_cores", "Number of threads",value = 1)
  ),
  box( 
  title = "Default parameters2", height=300,
      closable = FALSE, 
      width = 4,
      status ="olive", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
     numericInput("qc_SNP","Threshold of SNP call rate",0.1,step=0.01),
     numericInput("qc_ind", "Threshold of IND call rate",value = 0.1,step=0.01)

  ),
  box( 
  title = "Default parameters3", height=300,
      closable = FALSE, 
      width = 4,
      status ="olive", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
     numericInput("qc_maf", "Threshold of MAF",value =0.01,step=0.01),
	 numericInput("qc_hwe", "Threshold of HWE",value =1e-07,step=1e-08)	 
  )

), 
  fluidRow(
 
    box(
  title = "Input data format", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  radioButtons("qc_input_data_format",NULL, c("Hapmap","Plink","Numeric","BLUPF90","VCF","Haplotype"),selected="Hapmap")
  ),
  column(width=4,
  uiOutput("qc_input_data_dynamic")),  
    box(
  title = "Output data format", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  checkboxGroupInput("qc_output_data_format","Output_data_format", c("Hapmap","Plink","Numeric","BLUPF90","VCF","Haplotype"),selected="Plink")
  )), 
  fluidRow(
  column(width=12,
  selectInput("qc_method", "Please select the method in Quality Control and Imputation!",
              c("Only Quality Control","Only Imputation","Quality control and Imputatoin"),"Only Quality Control",width="500px") 
  )
  ),
  fluidRow(
  column(width=12,
  textInput("qc_path", "Please enter the output path!",getwd(),width="500px") 
  )
  ),  
  fluidRow(
  column(4,actionButton(
    inputId = "qc_button",
    label = "Start analysis!",
    icon = icon("users"),
    width = NULL,
    status = "info",
    style = "margin: auto"
  ))
  ), 
fluidRow(
   column(width=12,
  mainPanel(verbatimTextOutput("qc_console"))
  ) 
 )  
 # , 
# fluidRow(
  # downloadButton("qc_download_vcf", "Download VCF", class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  # , 
  # downloadButton("qc_download_hapmap", "Download Hapmap format data",class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),
  
# fluidRow(  
  # downloadButton("qc_download_numeric", "Download Numeric format data",class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),  
   
  # downloadButton("qc_download_blupf90", "Download BLUPF90 format data",class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),

 # fluidRow( 
  # downloadButton("qc_download_plink_map", "Download Plink-map format data",class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  # downloadButton("qc_download_plink_ped", "Download Plink-ped format data",class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),
  
# fluidRow(    
  # downloadButton("qc_download_hap_num", "Download Haplotype-hap format data",class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  # downloadButton("qc_download_hap_map", "Download Haplotype-map format data",class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  # downloadButton("qc_download_hap_sample", "Download Haplotype-sample format data",class="butt",style = "width:33%"),
  # tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")))
  )



###################################################################
###                                                             ###
###                                                             ###
### P Matrix construction                                       ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

P_tab<- tabItem(
 tabName ="P_matrix",
  fluidRow(
 
    box(
  title = "Upload pedigree data", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
    radioButtons("P_ped_format","Please select input pedigree data format", c("Standard colums","Multiple columns"),selected="Standard colums"),
	fileInput("P_ped", "Please upload pedigree data!")
  ),
 
    box(
  title = "Parameters 1", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
	 
     checkboxGroupInput("P_matrix_type","kinship type", c("P_A","P_Ainv","P_D","P_Dinv"),selected="P_A"),
	 checkboxGroupInput("P_output_type","Output matrix type", c("col_three","col_all"),selected="col_all")
  ), 
  
    box(
  title = "Parameters 2", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
	 selectInput("P_gene_drop", "Please select if apply gene dropping algorithm",c("Yes","No"),"No",width="500px"), 
	 numericInput("P_gene_drop_iter", "Iteration of gene dropping algorithm",value =1000),
	 numericInput("P_cpu_cores", "Number of threads",value =1)
  ) 
  ),  
  fluidRow(
  column(4,actionButton(
    inputId = "P_matrix_button",
    label = "Start analysis!",
    icon = icon("users"),
    width = NULL,
    status = "info",
    style = "margin: auto"
  ))
  ), 
fluidRow(
   column(width=12,
  mainPanel(verbatimTextOutput("P_matrix_console"))
  ) 
 )  
 , 
 
fluidRow(
  downloadButton("P_matrix_download_ind", "Download individual list", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color 
 ),
fluidRow(
  downloadButton("P_matrix_download_P_A", "Download P_A_col_all", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("P_matrix_download_P_A_three", "Download P_A_col_three", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("P_matrix_download_P_Ainv", "Download P_Ainv_col_all",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  
  downloadButton("P_matrix_download_P_Ainv_three", "Download P_Ainv_col_three",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))  
  ),
  
fluidRow(
  downloadButton("P_matrix_download_P_D", "Download P_D_col_all", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("P_matrix_download_P_D_three", "Download P_D_col_three", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("P_matrix_download_P_Dinv", "Download P_Dinv_col_all",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  
  downloadButton("P_matrix_download_P_Dinv_three", "Download P_Dinv_col_three",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))  
  )
   )
 

###################################################################
###                                                             ###
###                                                             ###
### G Matrix construction                                       ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

G_tab<- tabItem(
 tabName ="G_matrix",
  fluidRow(
  box( 
  title = "Default parameters1", height=300,
      closable = FALSE,  #是否可移除
      width = 4,
      status ="olive", 
	 background = "olive",
	 solidHeader = TRUE,
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
     textInput("G_missing_value","Missing values in genotype",""),
     numericInput("G_cpu_cores", "Number of threads",value = 1),
	radioButtons("G_scale","Please select scale method in G matrix", c("Current","Base","Trace"),selected="Current")
  ),
  box( 
  title = "Default parameters2", height=300,
      closable = FALSE, 
      width = 4,
      status ="olive", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
	numericInput("G_hap_nSNP", "Number of SNPs of each haplotype",value = 0),
	numericInput("G_hap_kb", "Window length of each haplotype(kb)",value = 0),
	fileInput("G_hap_block", "Please upload user-defined block file!")    	 
  ),
  box( 
  title = "Default parameters3", height=300,
      closable = FALSE, 
      width = 4,
      status ="olive", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
	   radioButtons("G_dominance_type","Please select dominance effect type", c("classical","genotypic"),selected="genotypic"),
     checkboxGroupInput("G_output_type","Output matrix type", c("col_three","col_all"),selected="col_all"),
	 numericInput("G_col_three_threshould", "Threshould of col_three",value = 0.000001)	 
	)
), 
  fluidRow(  

    box(
  title = "Upload genomic data", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  radioButtons("G_matrix_input_data_format","Please select input genomic data format", c("Hapmap","Plink","Numeric","BLUPF90","VCF","Haplotype"),selected="Hapmap")
  ),
   column(width=4,
  uiOutput("G_matrix__input_data_dynamic")),
     box(
  title = "Kinship type", height=390,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  checkboxGroupInput("G_matrix_type","Please select kinship type", c("G_A","G_Ainv","G_D","G_Dinv"),selected="G_A")) 
  ), 
  fluidRow(
  column(4,actionButton(
    inputId = "G_matrix_button",
    label = "Start analysis!",
    icon = icon("users"),
    width = NULL,
    status = "info",
    style = "margin: auto"
  ))
  ), 
fluidRow(
   column(width=12,
  mainPanel(verbatimTextOutput("G_matrix_console"))
  ) 
 ),  
 fluidRow(
  downloadButton("G_matrix_download_ind", "Download individual list", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color 
 ),
fluidRow(
  downloadButton("G_matrix_download_G_A", "Download G_A", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("G_matrix_download_G_A_three", "Download G_A_col_three", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("G_matrix_download_G_Ainv", "Download G_Ainv",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  
  downloadButton("G_matrix_download_G_Ainv_three", "Download G_Ainv_col_three",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))  
  ),
  
fluidRow(
  downloadButton("G_matrix_download_G_D", "Download G_D", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("G_matrix_download_G_D_three", "Download G_D_col_three", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("G_matrix_download_G_Dinv", "Download G_Dinv",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  
  downloadButton("G_matrix_download_G_Dinv_three", "Download G_Dinv_col_three",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))  
  )
  )
  
  
  
###################################################################
###                                                             ###
###                                                             ###
### H Matrix construction                                       ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

H_tab<- tabItem(
 tabName ="H_matrix",
  fluidRow(
  box( 
  title = "Default parameters1", height=300,
      closable = FALSE,  #是否可移除
      width = 3,
      status ="olive", 
	 background = "olive",
	 solidHeader = TRUE,
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
     textInput("H_missing_value","Missing values in genotype",""),
     numericInput("H_cpu_cores", "Number of threads",value = 1),
	radioButtons("H_trace","Please select scale method in H matrix", c("Current","Base","Trace"),selected="Current")
  ),
  box( 
  title = "Default parameters2", height=300,
      closable = FALSE, 
      width = 3,
      status ="olive", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
	numericInput("H_hap_nSNP", "Number of SNPs of each haplotype",value = 0),
	numericInput("H_hap_kb", "Window length of each haplotype(kb)",value = 0),
	fileInput("H_hap_block", "Please upload user-defined block file!")    	 
  ),
  box( 
  title = "Default parameters3", height=300,
      closable = FALSE, 
      width = 3,
      status ="olive", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
	numericInput("H_omega", "Omega of SSGBLUP",value = 0.05,step=0.05),	  
      checkboxGroupInput("H_output_type","Output matrix type", c("col 3","col all"),selected="col 3"),
	 numericInput("H_col_three_threshould", "Threshould of col_three",value = 0.000001)	 
	),
  box( 
  title = "Default parameters4", height=300,
      closable = FALSE, 
      width = 3,
      status ="olive", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
	 selectInput("H_algorithm", "Please select algorithm in SSGBLUP",c("Standard","Metafounder","APY"),"Standard"),
      checkboxGroupInput("H_output_type","Output matrix type", c("col 3","col all"),selected="col 3"),
	 numericInput("H_col_three_threshould", "Threshould of col_three",value = 0.000001)	 
	)	
), 
  fluidRow(  
    
     box(
  title = "Upload pedigree data", height=390,
      closable = TRUE, 
      width = 3,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
    radioButtons("H_ped_format","Please select input pedigree data format", c("Three colums","Multiple columns"),selected="Three colums"),
	fileInput("H_ped", "Please upload pedigree data!")
  ), 
  
    box(
  title = "Upload genomic data", height=390,
      closable = TRUE, 
      width = 3,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  radioButtons("H_matrix_input_data_format","Please select input genomic data format", c("Hapmap","Plink","Numeric","BLUPF90","VCF","Haplotype"),selected="Hapmap")
  ),
   column(width=3,
  uiOutput("H_matrix__input_data_dynamic")),
      box(
  title = "Kinship type", height=390,
      closable = TRUE, 
      width = 3,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  checkboxGroupInput("H_matrix_type","Please select kinship type", c("H_A","H_Ainv","H_D","H_Dinv"),selected="H_A")
  )
 ), 
  fluidRow(
  column(3,actionButton(
    inputId = "H_matrix_button",
    label = "Start analysis!",
    icon = icon("users"),
    width = NULL,
    status = "info",
    style = "margin: auto"
  ))
  ), 
fluidRow(
   column(width=12,
  mainPanel(verbatimTextOutput("H_matrix_console"))
  ) 
 ),  
fluidRow(
  downloadButton("H_matrix_download_H_A", "Download H_A", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("H_matrix_download_H_A_three", "Download H_A_col_three", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("H_matrix_download_H_Ainv", "Download H_Ainv",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  
  downloadButton("H_matrix_download_H_Ainv_three", "Download H_Ainv_col_three",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))  
  ),
  
fluidRow(
  downloadButton("H_matrix_download_H_D", "Download H_D", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("H_matrix_download_H_D_three", "Download H_D_col_three", class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  , 
  downloadButton("H_matrix_download_H_Dinv", "Download H_Dinv",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  
  downloadButton("H_matrix_download_H_Dinv_three", "Download H_Dinv_col_three",class="butt",style = "width:20%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))  
  )
  )


###################################################################
###                                                             ###
###                                                             ###
### Run DMU                                                     ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

Run_dmu_tab<-tabItem(
 tabName ="run_dmu",
   
  #默认参数  
  fluidRow(
  box( 
  title = "Default parameters", height=300,
      closable = FALSE,  #是否可移除
      width = 4,
      status ="warning", 
	 background = "gray",
	 solidHeader = TRUE,
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
     numericInput("dmu_missing_value","Missing value in phenotype",-9999),
     numericInput("dmu_iteration_criteria", "Threshold of convergence",value = 1.0e-7),
     numericInput("dmu_SSBLUP_omega", "SSBLUP omega",value =0.05)

  ),
  box( 
  title = "Default parameters", height=300,
      closable = FALSE, 
      width = 4,
      status ="warning", 
	 background = "gray",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
	 textInput("dmu_genetic_effect_name", "Genetic effect name","Id"),   
      textInput("dmu_prior_path", "File path of PRIOR information"), 
      textInput("dmu_prior_name", "File name of PRIOR information")   
  ),
  box( 
  title = "Default parameters", height=300,
      closable = FALSE, 
      width = 4,
      status ="warning", 
	 background = "gray",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
      numericInput("dmu_dmu_algorithm_code", "DMU algorithm code",value =1),
      selectInput("dmu_included_permanent_effect", "Include permanent effect?",c("No","Yes")),
      selectInput("dmu_included_dominance_effect", "Include dominance effect?",c("No","Yes"))
  )
), 
  #核心参数 
  fluidRow(
  box( 
  title = "Parameters set 1", height=300,
      closable = FALSE, 
      width = 4,
      status ="warning", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
     numericInput("dmu_trait_n","Number of trait",1),
     numericInput("dmu_integer_n", "Number of integer variable", value = 4),
	fileInput("dmu_phe","Upload phenotype!(with colnames)")  
  ),
 box(
  title = "Parameters set 2", height=300,
      closable = FALSE, 
      width = 4,
      status ="warning", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  selectInput("dmu_genotype_data_type", "Genotype data type",c("Plink","Hapmap","BLUPF90","VCF","Numeric")),	 
  fileInput("dmu_genotype_data","Upload genotype!"),
  fileInput("dmu_pedigree","Upload pedigree!")
  ), 
  box(
  title = "Parameters set 3", height=300,
      closable = FALSE, 
      width = 4,
      status ="warning", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
   textInput("dmu_relationship_name", "File name of relationship data"),  
   selectInput("dmu_dmu_module", "DMU module",c("dmuai","dmu4","dmu5")),
   selectInput("dmu_analysis_model", "DMU analysis model",c("PBLUP_A","GBLUP_A","SSBLUP_A","User_define"))
  )),  
  uiOutput("run_dmu_dynamic"),
        actionButton(
          inputId = "run_dmu_button",
          label = "Run DMU",
          icon = icon("users"),
          width = NULL,
          status = "primary",
          style = "margin: auto"
        ), 
fluidRow(
   column(width=12,
  mainPanel(verbatimTextOutput("run_dmu_console"))
  ) 
 ),		
 fluidRow(
  downloadButton("download_ebv_dmu", "Download EBV", class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  ),  		
  dataTableOutput("run_dmu_dir_result")
 
)  



###################################################################
###                                                             ###
###                                                             ###
### Run BLUPF90                                                 ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

RUN_blupf90_tab<- tabItem(
 tabName ="run_blupf90",
 
#默认参数  
  fluidRow(
  box( 
  title = "Default parameters", height=300,
      closable = FALSE,  #是否可移除
      width = 4,
      status ="warning", 
	 background = "gray",
	 solidHeader = TRUE,
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
     numericInput("blupf90_missing_value","Missing value in phenotype",-9999),
     numericInput("blupf90_iteration_criteria", "Threshold of convergence",value = 1.0e-7),
     numericInput("blupf90_SSBLUP_omega", "SSBLUP omega",value =0.05)

  ),
  box( 
  title = "Default parameters", height=300,
      closable = FALSE, 
      width = 4,
      status ="warning", 
	 background = "gray",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
	 textInput("blupf90_genetic_effect_name", "Genetic effect name","Id"),   
      textInput("blupf90_prior_path", "File path of PRIOR information"), 
      textInput("blupf90_prior_name", "File name of PRIOR information")   
  ),
  box( 
  title = "Default parameters", height=300,
      closable = FALSE, 
      width = 4,
      status ="warning", 
	 background = "gray",
      solidHeader = TRUE, 
      collapsed=TRUE,     #保持折叠
      collapsible = TRUE, #可以折叠或者显示
      selectInput("blupf90_included_permanent_effect", "Include permanent effect?",c("No","Yes")),
      selectInput("blupf90_included_dominance_effect", "Include dominance effect?",c("No","Yes"))
  )
),

  fluidRow(
  box(
  title = "Basic information", height=350,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "gray",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  numericInput("run_blupf90_trait_n","Number of trait",1),
	fileInput("run_blupf90_phenotype","Upload phenotype data(with colnames)!"),
	fileInput("run_blupf90_pedigree","Upload pedigree data!")	
  ),
 box(
  title = "File path and name", height=350,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "secondary",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  textInput("run_blupf90_relationship_path", "Kingship file path"),   
  textInput("run_blupf90_phe_path", "Phenotype file path"),  
  textInput("run_blupf90_phe_name", "Phenotype file name")
  ), 
  box(
  title = "Blupf90 parameter", height=350,
      closable = TRUE, 
      width = 4,
      status ="warning", 
	 background = "olive",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
  radioButtons("run_blupf90_algorithm", "BLUPF90 module", c("AI_REML","EM_REML","BLUPF90"),selected="AI_REML"),
  radioButtons("run_blupf90_method","Analysis method",c("PBLUP_A","SSBLUP_A"),selected="PBLUP_A"),
  radioButtons("run_blupf90_given_prior_logic", "Provide PRIOR?", c("YES","NO"),selected="NO")
  )),  
  uiOutput("run_blupf90_given_prior_dynamic"), 
  uiOutput("run_blupf90_dynamic"),
   actionButton(
  inputId = "run_blupf90_Button",
  label = "Run BLUPF90",
  icon = icon("users"),
  width = NULL,
  status = "primary",
  style = "margin: auto"
        ), 
fluidRow(
   column(width=12,
  mainPanel(verbatimTextOutput("run_blupf90_console"))
  ) 
 ),		
 fluidRow(
  downloadButton("download_ebv_blupf90", "Download EBV", class="butt",style = "width:33%"),
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")) #  font color
  ),   
  dataTableOutput("run_blupf90_result")

	)





 
#src="https://qsmei-markdown.oss-cn-shanghai.aliyuncs.com/markdown-img/20210318215728.png"
  ui = dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
    dark = FALSE,
    help = TRUE,
    fullscreen = TRUE,
    scrollToTop = TRUE,
    header = dashboardHeader(
      title = span(
         tags$img(src="https://qsmei-markdown.oss-cn-shanghai.aliyuncs.com/markdown-img/20210714231139.png", width = '100%')
      ),
      fixed = TRUE
    ),
    sidebar = dashboardSidebar(
      fixed = TRUE,
      skin = "light",
      status = "primary",
      id = "sidebar",
	  #width = 600,
      sidebarUserPanel(
        #image = "https://qsmei-markdown.oss-cn-shanghai.aliyuncs.com/markdown-img/20210318215728.png",
        name = "Focus on genetic analysis!"
      ),
      sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,				
        menuItem(
          "Format conversion",
          badgeLabel = "",
          badgeColor = "success",
          tabName = "format",
          icon = icon("laptop-code")
        ),
        menuItem(
          "QC and imputation",
          badgeLabel = "",
          badgeColor = "success",
          tabName = "qc_imputation",
          icon = icon("laptop-code")
        ),
        menuItem(
          text = "Matrix Construction",
          icon = icon("cubes"),
          startExpanded = FALSE,
          menuSubItem(
            text = HTML(
              paste(
                "P matrix",
                dashboardBadge(
                  "new",
                  position = "right",
                  color = "danger"
                )
              )
            ),
            tabName = "P_matrix",
            icon = icon("circle-thin")
          ),
          menuSubItem(
            text = HTML(
              paste(
                "G Matrix",
                dashboardBadge(
                  "!",
                  position = "right",
                  color = "success"
                )
              )
            ),
            tabName = "G_matrix"
          ),
          menuSubItem(
            text = HTML(
              paste(
                "H Matrix",
                dashboardBadge(
                  "!",
                  position = "right",
                  color = "success"
                )
              )
            ),
            tabName = "H_matrix"
          )		  
        ),
        menuItem(
          "Evaluation - DMU",
          badgeLabel = "",
          badgeColor = "success",
          tabName = "run_dmu",
          icon = icon("laptop-code")
        ),
        menuItem(
          "Evaluation - BLUPF90",
          badgeLabel = "",
          badgeColor = "success",
          tabName = "run_blupf90",
          icon = icon("laptop-code")
        )			
      )
    ),
    body = dashboardBody(
      e_theme_register(echarts_dark_theme$options, name = echarts_dark_theme$name),
      tabItems(
	  Format_tab,
	  QC_imputation_tab,
	   P_tab,
	   G_tab,
	   H_tab,
	   Run_dmu_tab,
	   RUN_blupf90_tab
      )
    ),
    controlbar = dashboardControlbar(
      id = "controlbar",
      skin = "light",
      pinned = TRUE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarMenu",
        type = "pills",
        controlbarItem(
          "Inputs",
          column(
            width = 12,
            align = "center",
            radioButtons(
              inputId = "dist",
              label = "Distribution type:",
              c(
                "Normal" = "norm",
                "Uniform" = "unif",
                "Log-normal" = "lnorm",
                "Exponential" = "exp"
              )
            )
          )
        ),
        controlbarItem(
          "Skin",
          skinSelector()
        )
      )
    ),
    footer = dashboardFooter(
      fixed = FALSE,
      left = a(
        #href = "https://twitter.com/divadnojnarg",
        target = "_blank", "@Quanshun Mei"
      ),
      right = "2022.3"
    ),
    title = "blupADC online"
  )