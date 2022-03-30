server = function(input, output, session) {

reset_sink <- function(){
    for(i in seq_len(sink.number())){
        sink(NULL)
    }
}

###################################################################
###                                                             ###
###                                                             ###
### Format  conversion                                          ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################
  
	#动态生成输入表格
output$input_data_dynamic <- renderUI({

      n_file=ifelse(input$input_data_format=="Plink",2,
		     ifelse(input$input_data_format=="Haplotype",3,1))	

	n_file=3

	if(input$input_data_format=="Plink"){


    box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width =12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
     fileInput("plink_map", "Please upload Plink-map format data!"),
	 fileInput("plink_ped", "Please upload Plink-ped format data!")
	)
	}else if(input$input_data_format=="Haplotype"){

    box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("haplotype_hap", "Please upload Haplotype-hap format data!"),
	 fileInput("haplotype_map", "Please upload Haplotype-map format data!"),
	 fileInput("haplotype_sample", "Please upload Haplotype-sample format data!")
	
	)	
	}else if(input$input_data_format=="Hapmap"){

  
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("hapmap", "Please upload Hapmap format data!")
	)
	
	}else if(input$input_data_format=="VCF"){

  fluidRow(
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("vcf", "Please upload VCF format data!")
	))
		
	}else if(input$input_data_format=="BLUPF90"){

  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("blupf90", "Please upload BLUPF90 format data!")
	
	)	
	}else if(input$input_data_format=="Numeric"){
  
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("numeric", "Please upload Numeric format data!")
	)	
	}
}) 

#click button and run application
observeEvent(input$format_button,{

 showModal(modalDialog("Start genomic data format conversion!", easyClose = TRUE))  
 
reset_sink()
sink("test.log.txt",split=TRUE)
						
 #input_type VCF						
	if(input$input_data_format=="VCF"){

	input_data_vcf =reactive({
	req(input$vcf)
	data.table::fread(input$vcf$datapath,data.table=F)
	})
		
conversion_data=blupADC::geno_format(             
						input_data_vcf=input_data_vcf(),
						return_result=T,
						output_data_type=input$output_data_format,  # "Plink" , "Hapmap" , "VCF"  "Blupf90"
						cpu_cores=1  #调用的cpu数目，用于加速计算
	                       	)
							
 #input_type Hapmap
	}else if(input$input_data_format=="Hapmap"){
	input_data_hmp =reactive({
	data.table::fread(input$hapmap$datapath,data.table=F,header=T)
	}) 
	
 conversion_data=blupADC::geno_format(             
						input_data_hmp=input_data_hmp(),
						return_result=T,
						output_data_type=input$output_data_format,  # "Plink" , "Hapmap" , "VCF"  "Blupf90"
						cpu_cores=1  #调用的cpu数目，用于加速计算
	                       	)



 #input_type Plink														
	}else if(input$input_data_format=="Plink"){

	input_data_ped =reactive({
	req(input$plink_ped)
	data.table::fread(input$plink_ped$datapath,data.table=F,header=T)
	})

	input_data_map =reactive({
	req(input$plink_map)
	data.table::fread(input$plink_map$datapath,data.table=F,header=T)
	})	

conversion_data=blupADC::geno_format(             
						input_data_plink_ped=input_data_ped(),
						input_data_plink_map=input_data_map(),
						return_result=T,
						output_data_type=input$output_data_format,  # "Plink" , "Hapmap" , "VCF"  "Blupf90"
						cpu_cores=1  #调用的cpu数目，用于加速计算
	                       	)
 #input_type Numeric														
	}else if(input$input_data_format=="Numeric"){

	input_data_numeric =reactive({
	req(input$numeric)
	data.table::fread(input$numeric$datapath,data.table=F,header=T)
	})
	
	
conversion_data=blupADC::geno_format(             
						input_data_numeric=input_data_numeric(),
						return_result=T,
						output_data_type=input$output_data_format,  # "Plink" , "Hapmap" , "VCF"  "Blupf90"
						cpu_cores=1  #调用的cpu数目，用于加速计算
	                       	)

 #input_type BLUPF90														
	}else if(input$input_data_format=="BLUPF90"){

	input_data_blupf90 =reactive({
	req(input$blupf90)
	data.table::fread(input$blupf90$datapath,data.table=F,header=T)
	})
	
	
conversion_data=blupADC::geno_format(             
						input_data_blupf90=input_data_blupf90(),
						return_result=T,
						output_data_type=input$output_data_format,  # "Plink" , "Hapmap" , "VCF"  "Blupf90"
						cpu_cores=1  #调用的cpu数目，用于加速计算
	                       	)							

 #input_type Haplotype														
	}else if(input$input_data_format=="Haplotype"){

	input_data_haplotype_hap =reactive({
	req(input$haplotype_hap)
	data.table::fread(input$haplotype_hap$datapath,data.table=F,header=T)
	})
	
	input_data_haplotype_map =reactive({
	req(input$haplotype_map)
	data.table::fread(input$haplotype_map$datapath,data.table=F,header=T)
	})
	
	input_data_haplotype_sample =reactive({
	req(input$haplotype_sample)
	data.table::fread(input$haplotype_sample$datapath,data.table=F,header=T)
	})	

conversion_data=blupADC::geno_format(             
						input_data_haplotype_hap=input_data_haplotype_hap(),
						input_data_haplotype_map=input_data_haplotype_map(),
						input_data_haplotype_sample=input_data_haplotype_sample(),
						haplotype_window_nSNP =5,
						return_result=T,
						output_data_type=input$output_data_format,  # "Plink" , "Hapmap" , "VCF"  "Blupf90"
						cpu_cores=1  #调用的cpu数目，用于加速计算
	                       	)
}
showModal(modalDialog("Complete genomic data format conversion!", easyClose = TRUE))

reset_sink()
message=system("cat test.log.txt",intern = TRUE)
   output$console <- renderPrint({
   
     return(print(message))
   })
   
file.remove("test.log.txt")
   
#VCF
  output$download_vcf <- downloadHandler(
  
    filename = function() {
        "data.vcf"
    },
    content = function(file) {
	# tmp=conversion_data$vcf
	# tmp=rbind(colnames(tmp),tmp)
	# header=matrix("",nrow=8,ncol=ncol(tmp))
	# header[,1]="#"
	# header[,2]=get_vcf_header()
	
     # data.table::fwrite(data.frame(rbind(header,tmp)),
	                      # file,quote=F,row.names=F,col.names=F,sep="\t")
     data.table::fwrite(data.frame(conversion_data$vcf,check.names=F),
	                      file,quote=F,row.names=F,col.names=T,sep="\t")
    }  
)					

#Hapmap
  output$download_hapmap <- downloadHandler(
    filename = function() {
        "data.hmp.txt"
    },
    content = function(file) {
     data.table::fwrite(data.table(conversion_data$hmp),file,quote=F,row.names=F,col.names=T,sep="\t")
    }  
)

#Numeric
  output$download_numeric <- downloadHandler(
    filename = function() {
        "data.numeric"
    },
    content = function(file) {
     data.table::fwrite(data.frame(conversion_data$numeric),file,quote=F,row.names=T,col.names=F,sep="\t")
    }  
)

#BLUPF90
  output$download_blupf90 <- downloadHandler(
    filename = function() {
        "data.blupf90"
    },
    content = function(file) {
     data.table::fwrite(data.frame(conversion_data$blupf90),file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)



#Plink
  output$download_plink_map <- downloadHandler(
    filename = function() {
        "data_plink.map"
    },
    content = function(file) {
     data.table::fwrite(data.frame(conversion_data$map),file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)
					
  output$download_plink_ped <- downloadHandler(
    filename = function() {
       "data_plink.ped"
    },
    content = function(file) {
     data.table::fwrite(data.frame(conversion_data$ped),file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

#Haplotype
  output$download_hap_num <- downloadHandler(
    filename = function() {
        "data_haplotype.hap"
    },
    content = function(file) {
     data.table::fwrite(data.frame(conversion_data$haplotype_hap),file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$download_hap_map <- downloadHandler(
    filename = function() {
        "data_haplotype.map"
    },
    content = function(file) {
     data.table::fwrite(data.frame(conversion_data$haplotype_map),file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$download_hap_sample <- downloadHandler(
    filename = function() {
        "data_haplotype.sample"
    },
    content = function(file) {
     data.table::fwrite(data.frame(conversion_data$haplotype_sample),file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)
})

 

###################################################################
###                                                             ###
###                                                             ###
### QC and Imputation                                           ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

	#动态生成输入表格
output$qc_input_data_dynamic <- renderUI({

      n_file=ifelse(input$qc_input_data_format=="Plink",2,
		     ifelse(input$qc_input_data_format=="Haplotype",3,1))	

	n_file=3

	if(input$qc_input_data_format=="Plink"){

 
    box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
     fileInput("qc_plink_map", "Please upload Plink-map format data!"),
	 fileInput("qc_plink_ped", "Please upload Plink-ped format data!")
	)	
	}else if(input$qc_input_data_format=="Haplotype"){

    box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("qc_haplotype_hap", "Please upload Haplotype-hap format data!"),
	 fileInput("qc_haplotype_map", "Please upload Haplotype-map format data!"),
	 fileInput("qc_haplotype_sample", "Please upload Haplotype-sample format data!")
	)
		
	}else if(input$qc_input_data_format=="Hapmap"){

  
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("qc_hapmap", "Please upload Hapmap format data!")
	)
	
	}else if(input$qc_input_data_format=="VCF"){

 
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("qc_vcf", "Please upload VCF format data!")
	)
		
	}else if(input$qc_input_data_format=="BLUPF90"){

  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("qc_blupf90", "Please upload BLUPF90 format data!")
	
	)	
	}else if(input$qc_input_data_format=="Numeric"){
 
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("qc_numeric", "Please upload Numeric format data!")
	)	
	}
}) 

#click button and run application
observeEvent(input$qc_button,{

showModal(modalDialog(paste0("Start analysis of ",input$qc_method,"!"), easyClose = TRUE)) 
reset_sink()
sink("test.log.txt",split=TRUE)
 
setwd(input$qc_path) 
method_qc=ifelse(input$qc_method=="Only Quality Control","QC",
             ifelse(input$qc_method=="Only Imputation","Imputation","QC_Imputation"))
						
 #input_type VCF						
	if(input$qc_input_data_format=="VCF"){

	input_data_vcf =reactive({
	req(input$qc_vcf)
	data.table::fread(input$qc_vcf$datapath,data.table=F)
	})

qc_missing_value="."	
if(input$qc_missing_value!=""){qc_missing_value=input$qc_missing_value}	

	
conversion_data=blupADC::geno_qc_impute(             
						input_data_vcf=input_data_vcf(),
						#return_result=T,
						output_data_name=method_qc,
						output_data_path=getwd(),
						miss_base=qc_missing_value, 
						qc_snp_rate=input$qc_SNP,
						qc_ind_rate=input$qc_ind,
						qc_maf=input$qc_maf,
						qc_hwe=input$qc_hwe,
						output_data_type=input$qc_output_data_format,
						cpu_cores=input$qc_cpu_cores  
	                       	)
							
 #input_type Hapmap
	}else if(input$qc_input_data_format=="Hapmap"){
	input_data_hmp =reactive({
	data.table::fread(input$qc_hapmap$datapath,data.table=F,header=T)
	}) 
qc_missing_value="N"	
if(input$qc_missing_value!=""){qc_missing_value=input$qc_missing_value}	


conversion_data=blupADC::geno_qc_impute(             
						input_data_hmp=input_data_hmp(),
						#return_result=T,
						output_data_name="data",
						data_analysis_method=method_qc,
						output_data_path=getwd(),
						miss_base=qc_missing_value, 
						qc_snp_rate=input$qc_SNP,
						qc_ind_rate=input$qc_ind,
						qc_maf=input$qc_maf,
						qc_hwe=input$qc_hwe,
						output_data_type=input$qc_output_data_format,
						cpu_cores=input$qc_cpu_cores  
	                       	)						

 #input_type Plink														
	}else if(input$qc_input_data_format=="Plink"){

	input_data_ped =reactive({
	req(input$qc_plink_ped)
	data.table::fread(input$qc_plink_ped$datapath,data.table=F,header=T)
	})

	input_data_map =reactive({
	req(input$qc_plink_map)
	data.table::fread(input$qc_plink_map$datapath,data.table=F,header=T)
	})	

qc_missing_value="0"	
if(input$qc_missing_value!=""){qc_missing_value=input$qc_missing_value}	
	
conversion_data=blupADC::geno_qc_impute(             
						input_data_plink_ped=input_data_ped(),
						input_data_plink_map=input_data_map(),
						#return_result=T,
						miss_base=qc_missing_value, 
						qc_snp_rate=input$qc_SNP,
						qc_ind_rate=input$qc_ind,
						qc_maf=input$qc_maf,
						qc_hwe=input$qc_hwe,
						output_data_type=input$qc_output_data_format,
						cpu_cores=input$qc_cpu_cores  
	                       	)
 #input_type Numeric														
	}else if(input$qc_input_data_format=="Numeric"){

	input_data_numeric =reactive({
	req(input$qc_numeric)
	data.table::fread(input$qc_numeric$datapath,data.table=F,header=T)
	})
	
qc_missing_value="5"	
if(input$qc_missing_value!=""){qc_missing_value=input$qc_missing_value}	
	
conversion_data=blupADC::geno_qc_impute(             
						input_data_numeric=input_data_numeric(),
						#return_result=T,
						miss_base=qc_missing_value, 
						qc_snp_rate=input$qc_SNP,
						qc_ind_rate=input$qc_ind,
						qc_maf=input$qc_maf,
						qc_hwe=input$qc_hwe,
						output_data_type=input$qc_output_data_format,
						cpu_cores=input$qc_cpu_cores  
	                       	)

 #input_type BLUPF90														
	}else if(input$qc_input_data_format=="BLUPF90"){

	input_data_blupf90 =reactive({
	req(input$qc_blupf90)
	data.table::fread(input$qc_blupf90$datapath,data.table=F,header=T)
	})	

qc_missing_value="5"	
if(input$qc_missing_value!=""){qc_missing_value=input$qc_missing_value}	
	
conversion_data=blupADC::geno_qc_impute(             
						input_data_blupf90=input_data_blupf90(),
						#return_result=T,
						miss_base=qc_missing_value, 
						qc_snp_rate=input$qc_SNP,
						qc_ind_rate=input$qc_ind,
						qc_maf=input$qc_maf,
						qc_hwe=input$qc_hwe,
						output_data_type=input$qc_output_data_format,
						cpu_cores=input$qc_cpu_cores  
	                       	)							

 #input_type Haplotype														
	}else if(input$qc_input_data_format=="Haplotype"){

	input_data_haplotype_hap =reactive({
	req(input$qc_haplotype_hap)
	data.table::fread(input$qc_haplotype_hap$datapath,data.table=F,header=T)
	})
	
	input_data_haplotype_map =reactive({
	req(input$qc_haplotype_map)
	data.table::fread(input$qc_haplotype_map$datapath,data.table=F,header=T)
	})
	
	input_data_haplotype_sample =reactive({
	req(input$qc_haplotype_sample)
	data.table::fread(input$qc_haplotype_sample$datapath,data.table=F,header=T)
	})	
qc_missing_value="5"	
if(input$qc_missing_value!=""){qc_missing_value=input$qc_missing_value}	
	
conversion_data=blupADC::geno_qc_impute(             
						input_data_haplotype_hap=input_data_haplotype_hap(),
						input_data_haplotype_map=input_data_haplotype_map(),
						input_data_haplotype_sample=input_data_haplotype_sample(),
						haplotype_window_nSNP =5,
						# return_result=T,
						miss_base=qc_missing_value, 
						qc_snp_rate=input$qc_SNP,
						qc_ind_rate=input$qc_ind,
						qc_maf=input$qc_maf,
						qc_hwe=input$qc_hwe,
						output_data_type=input$qc_output_data_format,
						cpu_cores=input$qc_cpu_cores  
	                       	)
}
  showModal(modalDialog(paste0("Complete analysis of ",input$qc_method,"!"), easyClose = TRUE))  

reset_sink()
qc_message=system("cat test.log.txt",intern = TRUE)
   output$qc_console <- renderPrint({
   
     return(print(message))
   })							
reset_sink()
message=system("cat test.log.txt",intern = TRUE)
   output$console <- renderPrint({
   
     return(print(message))
   })							
file.remove("test.log.txt")


  
# #VCF
  # output$qc_download_vcf <- downloadHandler(
  
    # filename = function() {
        # "data.vcf"
    # },
    # content = function(file) {
	# # tmp=conversion_data$qc_vcf
	# # tmp=rbind(colnames(tmp),tmp)
	# # header=matrix("",nrow=8,ncol=ncol(tmp))
	# # header[,1]="#"
	# # header[,2]=get_vcf_header()
	
     # # data.table::fwrite(data.frame(rbind(header,tmp)),
	                      # # file,quote=F,row.names=F,col.names=F,sep="\t")
     # data.table::fwrite(data.frame(conversion_data$vcf,check.names=F),
	                      # file,quote=F,row.names=F,col.names=T,sep="\t")
    # }  
# )					

# #Hapmap
  # output$qc_download_hapmap <- downloadHandler(
    # filename = function() {
        # "data.hmp.txt"
    # },
    # content = function(file) {
     # data.table::fwrite(data.table(conversion_data$hmp),file,quote=F,row.names=F,col.names=T,sep="\t")
    # }  
# )

# #Numeric
  # output$qc_download_numeric <- downloadHandler(
    # filename = function() {
        # "data.qc_numeric"
    # },
    # content = function(file) {
     # data.table::fwrite(data.frame(conversion_data$numeric),file,quote=F,row.names=T,col.names=F,sep="\t")
    # }  
# )

# #BLUPF90
  # output$qc_download_blupf90 <- downloadHandler(
    # filename = function() {
        # "data.qc_blupf90"
    # },
    # content = function(file) {
     # data.table::fwrite(data.frame(conversion_data$blupf90),file,quote=F,row.names=F,col.names=F,sep="\t")
    # }  
# )



# #Plink
  # output$qc_download_plink_map <- downloadHandler(
    # filename = function() {
        # "data_plink.map"
    # },
    # content = function(file) {
     # data.table::fwrite(data.frame(conversion_data$map),file,quote=F,row.names=F,col.names=F,sep="\t")
    # }  
# )
					
  # output$qc_download_plink_ped <- downloadHandler(
    # filename = function() {
       # "data_plink.ped"
    # },
    # content = function(file) {
     # data.table::fwrite(data.frame(conversion_data$ped),file,quote=F,row.names=F,col.names=F,sep="\t")
    # }  
# )

# #Haplotype
  # output$qc_download_hap_num <- downloadHandler(
    # filename = function() {
        # "data_haplotype.hap"
    # },
    # content = function(file) {
     # data.table::fwrite(data.frame(conversion_data$haplotype_hap),file,quote=F,row.names=F,col.names=F,sep="\t")
    # }  
# )

  # output$qc_download_hap_map <- downloadHandler(
    # filename = function() {
        # "data_haplotype.map"
    # },
    # content = function(file) {
     # data.table::fwrite(data.frame(conversion_data$haplotype_map),file,quote=F,row.names=F,col.names=F,sep="\t")
    # }  
# )

  # output$qc_download_hap_sample <- downloadHandler(
    # filename = function() {
        # "data_haplotype.sample"
    # },
    # content = function(file) {
     # data.table::fwrite(data.frame(conversion_data$haplotype_sample),file,quote=F,row.names=F,col.names=F,sep="\t")
    # }  
# )
 })


###################################################################
###                                                             ###
###                                                             ###
### P Matrix                                                    ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

#click button and run application
observeEvent(input$P_matrix_button,{

showModal(modalDialog(paste0("Start pedigree relationship matrix construction!"), easyClose = TRUE)) 
reset_sink()
sink("test.log.txt",split=TRUE)
 
	P_ped =reactive({
	data.table::fread(input$P_ped$datapath,data.table=F)
	}) 
	
kinship_result=blupADC::cal_kinship(
                input_pedigree=P_ped(),          #provided hapmap data object
                kinship_type=input$P_matrix_type,      #type of  kinship matrix
                output_matrix_type=input$P_output_type,
			 gene_dropping_algorithm=ifelse(input$P_gene_drop=="Yes",TRUE,FALSE),
			 gene_dropping_iteration=input$P_gene_drop_iter,
			 cpu_cores=input$P_cpu_cores,
                return_result=TRUE)
	
showModal(modalDialog(paste0("Complete pedigree relationship matrix construction!"), easyClose = TRUE))
 
							
reset_sink()
message=system("cat test.log.txt",intern = TRUE)
   output$P_matrix_console <- renderPrint({
   
     return(print(message))
   })							
file.remove("test.log.txt")

individual_id=NULL
if(!is.null(kinship_result$P_A$A)){individual_id=rownames(kinship_result$P_A$A)
}else if(!is.null(kinship_result$P_A$Ainv)){individual_id=rownames(kinship_result$P_A$Ainv)			   
}else if(!is.null(kinship_result$P_D$D)){individual_id=rownames(kinship_result$P_D$D)			   
}else if(!is.null(kinship_result$P_D$Dinv)){individual_id=rownames(kinship_result$P_D$Dinv)}			   
	
  				
  output$P_matrix_download_ind <- downloadHandler(
  
    filename = function() {
        "P_individual_list.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(Id=individual_id,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)
#P_A
  output$P_matrix_download_P_A <- downloadHandler(
  
    filename = function() {
        "P_A_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$P_A$A,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$P_matrix_download_P_A_three <- downloadHandler(
  
    filename = function() {
        "P_A_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$P_A$A_col_three,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)					

#P_Ainv
  output$P_matrix_download_P_Ainv <- downloadHandler(
  
    filename = function() {
        "P_Ainv_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$P_A$Ainv,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$P_matrix_download_P_Ainv_three <- downloadHandler(
  
    filename = function() {
        "P_Ainv_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$P_A$Ainv_col_three,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)	


#P_D
  output$P_matrix_download_P_D <- downloadHandler(
  
    filename = function() {
        "P_D_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$P_D$D,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$P_matrix_download_P_D_three <- downloadHandler(
  
    filename = function() {
        "P_D_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$P_D$D_col_three,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)					

#P_Dinv
  output$P_matrix_download_P_Dinv <- downloadHandler(
  
    filename = function() {
        "P_Dinv_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$P_D$Dinv,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$P_matrix_download_P_Dinv_three <- downloadHandler(
  
    filename = function() {
        "P_Dinv_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$P_D$Dinv_col_three,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)	

 }
 )
 
 
 
###################################################################
###                                                             ###
###                                                             ###
### G Matrix                                                    ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

	#动态生成输入表格
output$G_matrix__input_data_dynamic <- renderUI({

      n_file=ifelse(input$G_matrix_input_data_format=="Plink",2,
		     ifelse(input$G_matrix_input_data_format=="Haplotype",3,1))	

	n_file=3

	if(input$G_matrix_input_data_format=="Plink"){

 
    box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
     fileInput("G_matrix_plink_map", "Please upload Plink-map format data!"),
	 fileInput("G_matrix_plink_ped", "Please upload Plink-ped format data!")
	)	
	}else if(input$G_matrix_input_data_format=="Haplotype"){

    box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("G_matrix_haplotype_hap", "Please upload Haplotype-hap format data!"),
	 fileInput("G_matrix_haplotype_map", "Please upload Haplotype-map format data!"),
	 fileInput("G_matrix_haplotype_sample", "Please upload Haplotype-sample format data!")
	)
		
	}else if(input$G_matrix_input_data_format=="Hapmap"){

  
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("G_matrix_hapmap", "Please upload Hapmap format data!")
	)
	
	}else if(input$G_matrix_input_data_format=="VCF"){

 
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("G_matrix_vcf", "Please upload VCF format data!")
	)
		
	}else if(input$G_matrix_input_data_format=="BLUPF90"){

  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("G_matrix_blupf90", "Please upload BLUPF90 format data!")
	
	)	
	}else if(input$G_matrix_input_data_format=="Numeric"){
 
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("G_matrix_numeric", "Please upload Numeric format data!")
	)	
	}
}) 

#click button and run application
observeEvent(input$G_matrix_button,{

showModal(modalDialog(paste0("Start analysis of genomic relationship matrix construction!"), easyClose = TRUE)) 
reset_sink()
sink("test.log.txt",split=TRUE)
 
						
 #input_type VCF						
	if(input$G_matrix_input_data_format=="VCF"){

	input_data_vcf =reactive({
	req(input$G_matrix_vcf)
	data.table::fread(input$G_matrix_vcf$datapath,data.table=F)
	})
		
	G_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$G_hap_block$datapath,data.table=F)	
	})

G_hap_nSNP_value=NULL	
if(input$G_hap_nSNP!=0){G_hap_nSNP_value=input$G_hap_nSNP}	

G_hap_kb_value=NULL	
if(input$G_hap_kb!=0){G_hap_kb_value=input$G_hap_kb}	

G_missing_value=NULL	
if(input$G_missing_value!=""){G_missing_value=input$G_missing_value}	


kinship_result=blupADC::cal_kinship(
                input_data_vcf=input_data_vcf(),          #provided hapmap data object
                kinship_type=input$G_matrix_type,      #type of  kinship matrix
                  dominance_type=input$G_dominance_type,
			   output_matrix_type=input$G_output_type,
			     kinship_base=ifelse(input$G_scale=="Base",TRUE,FALSE),
			     kinship_trace=ifelse(input$G_scale=="Trace",TRUE,FALSE),
			    col3_threshold=input$G_col_three_threshould,			 
			     miss_base=G_missing_value,
			    cpu_cores=input$G_cpu_cores,
			    haplotype_window_nSNP=G_hap_nSNP_value,
			    haplotype_window_kb=G_hap_kb_value,
			   haplotype_window_block=G_block(),		 
                 return_result=TRUE)
							
 #input_type Hapmap
	}else if(input$G_matrix_input_data_format=="Hapmap"){
	input_data_hmp =reactive({
	data.table::fread(input$G_matrix_hapmap$datapath,data.table=F,header=T)
	}) 

	G_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$G_hap_block$datapath,data.table=F)	
	})

G_hap_nSNP_value=NULL	
if(input$G_hap_nSNP!=0){G_hap_nSNP_value=input$G_hap_nSNP}	

G_hap_kb_value=NULL	
if(input$G_hap_kb!=0){G_hap_kb_value=input$G_hap_kb}	

G_missing_value=NULL	
if(input$G_missing_value!=""){G_missing_value=input$G_missing_value}	



kinship_result=blupADC::cal_kinship(
                input_data_hmp=input_data_hmp(),          #provided hapmap data object
                kinship_type=input$G_matrix_type,      #type of  kinship matrix
                 dominance_type=input$G_dominance_type,
			  output_matrix_type=input$G_output_type,
			   kinship_base=ifelse(input$G_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$G_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$G_col_three_threshould,			 
			   miss_base=G_missing_value,
			   cpu_cores=input$G_cpu_cores,
			   haplotype_window_nSNP=G_hap_nSNP_value,
			   haplotype_window_kb=G_hap_kb_value,
			  haplotype_window_block=G_block(),		 
                 return_result=TRUE)
						

 #input_type Plink														
	}else if(input$G_matrix_input_data_format=="Plink"){

	input_data_ped =reactive({
	req(input$G_matrix_plink_ped)
	data.table::fread(input$G_matrix_plink_ped$datapath,data.table=F,header=F)
	})

	input_data_map =reactive({
	req(input$G_matrix_plink_map)
	data.table::fread(input$G_matrix_plink_map$datapath,data.table=F,header=F)
	})	

	G_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$G_hap_block$datapath,data.table=F)	
	})

G_hap_nSNP_value=NULL	
if(input$G_hap_nSNP!=0){G_hap_nSNP_value=input$G_hap_nSNP}	

G_hap_kb_value=NULL	
if(input$G_hap_kb!=0){G_hap_kb_value=input$G_hap_kb}	


G_missing_value=NULL	
if(input$G_missing_value!=""){G_missing_value=input$G_missing_value}	


kinship_result=blupADC::cal_kinship(
                input_data_plink_ped=input_data_ped(),          #provided hapmap data object
			input_data_plink_map=input_data_map(),
                kinship_type=input$G_matrix_type,      #type of  kinship matrix
                 dominance_type=input$G_dominance_type,
			  output_matrix_type=input$G_output_type,
			   kinship_base=ifelse(input$G_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$G_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$G_col_three_threshould,			 
			   miss_base=G_missing_value,
			   cpu_cores=input$G_cpu_cores,
			   haplotype_window_nSNP=G_hap_nSNP_value,
			   haplotype_window_kb=G_hap_kb_value,
			  haplotype_window_block=G_block(),		 
                 return_result=TRUE)	

 #input_type Numeric														
	}else if(input$G_matrix_input_data_format=="Numeric"){

	input_data_numeric =reactive({
	req(input$G_matrix_numeric)
	data.table::fread(input$G_matrix_numeric$datapath,data.table=F,header=T)
	})
	
	G_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$G_hap_block$datapath,data.table=F)	
	})

G_hap_nSNP_value=NULL	
if(input$G_hap_nSNP!=0){G_hap_nSNP_value=input$G_hap_nSNP}	

G_hap_kb_value=NULL	
if(input$G_hap_kb!=0){G_hap_kb_value=input$G_hap_kb}	

G_missing_value=NULL	
if(input$G_missing_value!=""){G_missing_value=input$G_missing_value}	



kinship_result=blupADC::cal_kinship(
                input_data_numeric=input_data_numeric(),          #provided hapmap data object
                kinship_type=input$G_matrix_type,      #type of  kinship matrix
                 dominance_type=input$G_dominance_type,
			  output_matrix_type=input$G_output_type,
			   kinship_base=ifelse(input$G_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$G_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$G_col_three_threshould,			 
			   miss_base=G_missing_value,
			   cpu_cores=input$G_cpu_cores,
			   haplotype_window_nSNP=G_hap_nSNP_value,
			   haplotype_window_kb=G_hap_kb_value,
			  haplotype_window_block=G_block(),		 
                 return_result=TRUE)		

 #input_type BLUPF90														
	}else if(input$G_matrix_input_data_format=="BLUPF90"){

	input_data_blupf90 =reactive({
	req(input$G_matrix_blupf90)
	data.table::fread(input$G_matrix_blupf90$datapath,data.table=F,header=F)
	})	
	
	G_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$G_hap_block$datapath,data.table=F)	
	})

G_hap_nSNP_value=NULL	
if(input$G_hap_nSNP!=0){G_hap_nSNP_value=input$G_hap_nSNP}	

G_hap_kb_value=NULL	
if(input$G_hap_kb!=0){G_hap_kb_value=input$G_hap_kb}	

G_missing_value=NULL	
if(input$G_missing_value!=""){G_missing_value=input$G_missing_value}	

kinship_result=blupADC::cal_kinship(
                input_data_blupf90=input_data_blupf90(),          #provided hapmap data object
                kinship_type=input$G_matrix_type,      #type of  kinship matrix
                 dominance_type=input$G_dominance_type,
			  output_matrix_type=input$G_output_type,
			   kinship_base=ifelse(input$G_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$G_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$G_col_three_threshould,			 
			   miss_base=G_missing_value,
			   cpu_cores=input$G_cpu_cores,
			   haplotype_window_nSNP=G_hap_nSNP_value,
			   haplotype_window_kb=G_hap_kb_value,
			  haplotype_window_block=G_block(),		 
                 return_result=TRUE)									

 #input_type Haplotype														
	}else if(input$G_matrix_input_data_format=="Haplotype"){

	input_data_haplotype_hap =reactive({
	req(input$G_matrix_haplotype_hap)
	data.table::fread(input$G_matrix_haplotype_hap$datapath,data.table=F,header=F)
	})
	
	input_data_haplotype_map =reactive({
	req(input$G_matrix_haplotype_map)
	data.table::fread(input$G_matrix_haplotype_map$datapath,data.table=F,header=F)
	})
	
	input_data_haplotype_sample =reactive({
	req(input$G_matrix_haplotype_sample)
	data.table::fread(input$G_matrix_haplotype_sample$datapath,data.table=F,header=F)
	})	


	G_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$G_hap_block$datapath,data.table=F)	
	})

G_hap_nSNP_value=NULL	
if(input$G_hap_nSNP!=0){G_hap_nSNP_value=input$G_hap_nSNP}	

G_hap_kb_value=NULL	
if(input$G_hap_kb!=0){G_hap_kb_value=input$G_hap_kb}	


G_missing_value=NULL	
if(input$G_missing_value!=""){G_missing_value=input$G_missing_value}	


kinship_result=blupADC::cal_kinship(
                input_data_haplotype_hap=input_data_haplotype_hap(),
			 input_data_haplotype_map=input_data_haplotype_map(),
			 input_data_haplotype_sample=input_data_haplotype_sample(),			
                kinship_type=input$G_matrix_type,      #type of  kinship matrix
                 dominance_type=input$G_dominance_type,
			  output_matrix_type=input$G_output_type,
			   kinship_base=ifelse(input$G_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$G_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$G_col_three_threshould,			 
			   miss_base=G_missing_value,
			   cpu_cores=input$G_cpu_cores,
			   haplotype_window_nSNP=G_hap_nSNP_value,
			   haplotype_window_kb=G_hap_kb_value,
			  haplotype_window_block=G_block(),		 
                 return_result=TRUE)		

}
showModal(modalDialog(paste0("Complete analysis of genomic relationship matrix construction!"), easyClose = TRUE))  

reset_sink()
G_matrix_message=system("cat test.log.txt",intern = TRUE)
   output$G_matrix_console <- renderPrint({
   
     return(print(message))
   })							
reset_sink()
message=system("cat test.log.txt",intern = TRUE)
   output$console <- renderPrint({
   
     return(print(message))
   })							
file.remove("test.log.txt")

individual_id=NULL
if(!is.null(kinship_result$G_A$A)){individual_id=rownames(kinship_result$G_A$A)
}else if(!is.null(kinship_result$G_A$Ainv)){individual_id=rownames(kinship_result$G_A$Ainv)			   
}else if(!is.null(kinship_result$G_D$D)){individual_id=rownames(kinship_result$G_D$D)			   
}else if(!is.null(kinship_result$G_D$Dinv)){individual_id=rownames(kinship_result$G_D$Dinv)}			   
	
  				
  output$G_matrix_download_ind <- downloadHandler(
  
    filename = function() {
        "G_individual_list.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(Id=individual_id,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)
#G_A
  output$G_matrix_download_G_A <- downloadHandler(
  
    filename = function() {
        "G_A_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$G_A$A,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$G_matrix_download_G_A_three <- downloadHandler(
  
    filename = function() {
        "G_A_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$G_A$A_col3,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)					

#G_Ainv
  output$G_matrix_download_G_Ainv <- downloadHandler(
  
    filename = function() {
        "G_Ainv_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$G_A$Ainv,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$G_matrix_download_G_Ainv_three <- downloadHandler(
  
    filename = function() {
        "G_Ainv_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$G_A$Ainv_col3,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)	


#G_D
  output$G_matrix_download_G_D <- downloadHandler(
  
    filename = function() {
        "G_D_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$G_D$D,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$G_matrix_download_G_D_three <- downloadHandler(
  
    filename = function() {
        "G_D_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$G_D$D_col3,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)					

#G_Dinv
  output$G_matrix_download_G_Dinv <- downloadHandler(
  
    filename = function() {
        "G_Dinv_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$G_D$Dinv,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$G_matrix_download_G_Dinv_three <- downloadHandler(
  
    filename = function() {
        "G_Dinv_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$G_D$Dinv_col3,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)
  
  
 })


###################################################################
###                                                             ###
###                                                             ###
### H Matrix                                                    ###
###                                                             ###
###                                                             ###
###                                                             ###
###################################################################

	#动态生成输入表格
output$H_matrix__input_data_dynamic <- renderUI({

      n_file=ifelse(input$H_matrix_input_data_format=="Plink",2,
		     ifelse(input$H_matrix_input_data_format=="Haplotype",3,1))	

	n_file=3

	if(input$H_matrix_input_data_format=="Plink"){

 
    box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示
     fileInput("H_matrix_plink_map", "Please upload Plink-map format data!"),
	 fileInput("H_matrix_plink_ped", "Please upload Plink-ped format data!")
	)	
	}else if(input$H_matrix_input_data_format=="Haplotype"){

    box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("H_matrix_haplotype_hap", "Please upload Haplotype-hap format data!"),
	 fileInput("H_matrix_haplotype_map", "Please upload Haplotype-map format data!"),
	 fileInput("H_matrix_haplotype_sample", "Please upload Haplotype-sample format data!")
	)
		
	}else if(input$H_matrix_input_data_format=="Hapmap"){

  
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("H_matrix_hapmap", "Please upload Hapmap format data!")
	)
	
	}else if(input$H_matrix_input_data_format=="VCF"){

 
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("H_matrix_vcf", "Please upload VCF format data!")
	)
		
	}else if(input$H_matrix_input_data_format=="BLUPF90"){

  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("H_matrix_blupf90", "Please upload BLUPF90 format data!")
	
	)	
	}else if(input$H_matrix_input_data_format=="Numeric"){
 
  box(
  title = "Upload Genomic Data", height=130*n_file,
      closable = TRUE, 
      width = 12,
      status ="warning", 
	 background = "info",
      solidHeader = TRUE, 
      collapsible = TRUE, #可以折叠或者显示	 
     fileInput("H_matrix_numeric", "Please upload Numeric format data!")
	)	
	}
}) 

#click button and run application
observeEvent(input$H_matrix_button,{

showModal(modalDialog(paste0("Start analysis of genomic relationship matrix construction!"), easyClose = TRUE)) 
reset_sink()
sink("test.log.txt",split=TRUE)
 
 
	H_ped =reactive({
	data.table::fread(input$H_ped$datapath,data.table=F)
	})
						
 #input_type VCF						
	if(input$H_matrix_input_data_format=="VCF"){

	input_data_vcf =reactive({
	req(input$H_matrix_vcf)
	data.table::fread(input$H_matrix_vcf$datapath,data.table=F)
	})
		
	H_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$H_hap_block$datapath,data.table=F)	
	})

H_hap_nSNP_value=NULL	
if(input$H_hap_nSNP!=0){H_hap_nSNP_value=input$H_hap_nSNP}	

H_hap_kb_value=NULL	
if(input$H_hap_kb!=0){H_hap_kb_value=input$H_hap_kb}	


H_missing_value=NULL	
if(input$H_missing_value!=""){H_missing_value=input$H_missing_value}	


kinship_result=blupADC::cal_kinship(
		      input_pedigree=P_ped(),
			  IND_rename=TRUE,
                input_data_vcf=input_data_vcf(),          #provided hapmap data object
                kinship_type=input$H_matrix_type,      #type of  kinship matrix               
			  output_matrix_type=input$H_output_type,
			   col3_threshold=input$H_col_three_threshould,			 
			   miss_base=input$H_missing_value,
			   cpu_cores=input$H_cpu_cores,
			   haplotype_window_nSNP=H_hap_nSNP_value,
			   haplotype_window_kb=H_hap_kb_value,
			  haplotype_window_block=H_block(),		 
                 return_result=TRUE)
							
 #input_type Hapmap
	}else if(input$H_matrix_input_data_format=="Hapmap"){
	input_data_hmp =reactive({
	data.table::fread(input$H_matrix_hapmap$datapath,data.table=F,header=T)
	}) 

	H_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$H_hap_block$datapath,data.table=F)	
	})

H_hap_nSNP_value=NULL	
if(input$H_hap_nSNP!=0){H_hap_nSNP_value=input$H_hap_nSNP}	

H_hap_kb_value=NULL	
if(input$H_hap_kb!=0){H_hap_kb_value=input$H_hap_kb}	

H_missing_value=NULL	
if(input$H_missing_value!=""){H_missing_value=input$H_missing_value}	

kinship_result=blupADC::cal_kinship(
		      input_pedigree=H_ped(),
			  IND_rename=TRUE,
                input_data_hmp=input_data_hmp(),          #provided hapmap data object
                kinship_type=input$H_matrix_type,      #type of  kinship matrix               
			  output_matrix_type=input$H_output_type,
			   col3_threshold=input$H_col_three_threshould,			 
			   miss_base=input$H_missing_value,
			   cpu_cores=input$H_cpu_cores,
			   haplotype_window_nSNP=H_hap_nSNP_value,
			   haplotype_window_kb=H_hap_kb_value,
			  haplotype_window_block=H_block(),		 
                 return_result=TRUE)			

 #input_type Plink														
	}else if(input$H_matrix_input_data_format=="Plink"){

	input_data_ped =reactive({
	req(input$H_matrix_plink_ped)
	data.table::fread(input$H_matrix_plink_ped$datapath,data.table=F,header=F)
	})

	input_data_map =reactive({
	req(input$H_matrix_plink_map)
	data.table::fread(input$H_matrix_plink_map$datapath,data.table=F,header=F)
	})	

	H_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$H_hap_block$datapath,data.table=F)	
	})

H_hap_nSNP_value=NULL	
if(input$H_hap_nSNP!=0){H_hap_nSNP_value=input$H_hap_nSNP}	

H_hap_kb_value=NULL	
if(input$H_hap_kb!=0){H_hap_kb_value=input$H_hap_kb}	


H_missing_value=NULL	
if(input$H_missing_value!=""){H_missing_value=input$H_missing_value}	


kinship_result=blupADC::cal_kinship(
                input_data_plink_ped=input_data_ped(),          #provided hapmap data object
			input_data_plink_map=input_data_map(),
                kinship_type=input$H_matrix_type,      #type of  kinship matrix
                 dominance_type=input$H_dominance_type,
			  output_matrix_type=input$H_output_type,
			   kinship_base=ifelse(input$H_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$H_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$H_col_three_threshould,			 
			   miss_base=input$H_missing_value,
			   cpu_cores=input$H_cpu_cores,
			   haplotype_window_nSNP=H_hap_nSNP_value,
			   haplotype_window_kb=H_hap_kb_value,
			  haplotype_window_block=H_block(),		 
                 return_result=TRUE)	

 #input_type Numeric														
	}else if(input$H_matrix_input_data_format=="Numeric"){

	input_data_numeric =reactive({
	req(input$H_matrix_numeric)
	data.table::fread(input$H_matrix_numeric$datapath,data.table=F,header=T)
	})
	
	H_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$H_hap_block$datapath,data.table=F)	
	})

H_hap_nSNP_value=NULL	
if(input$H_hap_nSNP!=0){H_hap_nSNP_value=input$H_hap_nSNP}	

H_hap_kb_value=NULL	
if(input$H_hap_kb!=0){H_hap_kb_value=input$H_hap_kb}	

H_missing_value=NULL	
if(input$H_missing_value!=""){H_missing_value=input$H_missing_value}	



kinship_result=blupADC::cal_kinship(
                input_data_numeric=input_data_numeric(),          #provided hapmap data object
                kinship_type=input$H_matrix_type,      #type of  kinship matrix
                 dominance_type=input$H_dominance_type,
			  output_matrix_type=input$H_output_type,
			   kinship_base=ifelse(input$H_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$H_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$H_col_three_threshould,			 
			   miss_base=input$H_missing_value,
			   cpu_cores=input$H_cpu_cores,
			   haplotype_window_nSNP=H_hap_nSNP_value,
			   haplotype_window_kb=H_hap_kb_value,
			  haplotype_window_block=H_block(),		 
                 return_result=TRUE)		

 #input_type BLUPF90														
	}else if(input$H_matrix_input_data_format=="BLUPF90"){

	input_data_blupf90 =reactive({
	req(input$H_matrix_blupf90)
	data.table::fread(input$H_matrix_blupf90$datapath,data.table=F,header=F)
	})	
	
	H_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$H_hap_block$datapath,data.table=F)	
	})

H_hap_nSNP_value=NULL	
if(input$H_hap_nSNP!=0){H_hap_nSNP_value=input$H_hap_nSNP}	

H_hap_kb_value=NULL	
if(input$H_hap_kb!=0){H_hap_kb_value=input$H_hap_kb}	

H_missing_value=NULL	
if(input$H_missing_value!=""){H_missing_value=input$H_missing_value}	

kinship_result=blupADC::cal_kinship(
                input_data_blupf90=input_data_blupf90(),          #provided hapmap data object
                kinship_type=input$H_matrix_type,      #type of  kinship matrix
                 dominance_type=input$H_dominance_type,
			  output_matrix_type=input$H_output_type,
			   kinship_base=ifelse(input$H_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$H_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$H_col_three_threshould,			 
			   miss_base=input$H_missing_value,
			   cpu_cores=input$H_cpu_cores,
			   haplotype_window_nSNP=H_hap_nSNP_value,
			   haplotype_window_kb=H_hap_kb_value,
			  haplotype_window_block=H_block(),		 
                 return_result=TRUE)									

 #input_type Haplotype														
	}else if(input$H_matrix_input_data_format=="Haplotype"){

	input_data_haplotype_hap =reactive({
	req(input$H_matrix_haplotype_hap)
	data.table::fread(input$H_matrix_haplotype_hap$datapath,data.table=F,header=F)
	})
	
	input_data_haplotype_map =reactive({
	req(input$H_matrix_haplotype_map)
	data.table::fread(input$H_matrix_haplotype_map$datapath,data.table=F,header=F)
	})
	
	input_data_haplotype_sample =reactive({
	req(input$H_matrix_haplotype_sample)
	data.table::fread(input$H_matrix_haplotype_sample$datapath,data.table=F,header=F)
	})	


	H_block=reactive({
     if (is.null(input$run_dmu_input_data)) return(NULL)	
	data.table::fread(input$H_hap_block$datapath,data.table=F)	
	})

H_hap_nSNP_value=NULL	
if(input$H_hap_nSNP!=0){H_hap_nSNP_value=input$H_hap_nSNP}	

H_hap_kb_value=NULL	
if(input$H_hap_kb!=0){H_hap_kb_value=input$H_hap_kb}	


H_missing_value=NULL	
if(input$H_missing_value!=""){H_missing_value=input$H_missing_value}	


kinship_result=blupADC::cal_kinship(
                input_data_haplotype_hap=input_data_haplotype_hap(),
			 input_data_haplotype_map=input_data_haplotype_map(),
			 input_data_haplotype_sample=input_data_haplotype_sample(),			
                kinship_type=input$H_matrix_type,      #type of  kinship matrix
                 dominance_type=input$H_dominance_type,
			  output_matrix_type=input$H_output_type,
			   kinship_base=ifelse(input$H_scale=="Base",TRUE,FALSE),
			   kinship_trace=ifelse(input$H_scale=="Trace",TRUE,FALSE),
			   col3_threshold=input$H_col_three_threshould,			 
			   miss_base=input$H_missing_value,
			   cpu_cores=input$H_cpu_cores,
			   haplotype_window_nSNP=H_hap_nSNP_value,
			   haplotype_window_kb=H_hap_kb_value,
			  haplotype_window_block=H_block(),		 
                 return_result=TRUE)		

}
showModal(modalDialog(paste0("Complete analysis of genomic relationship matrix construction!"), easyClose = TRUE))  

reset_sink()
H_matrix_message=system("cat test.log.txt",intern = TRUE)
   output$H_matrix_console <- renderPrint({
   
     return(print(message))
   })							
reset_sink()
message=system("cat test.log.txt",intern = TRUE)
   output$console <- renderPrint({
   
     return(print(message))
   })							
file.remove("test.log.txt")

individual_id=NULL
if(!is.null(kinship_result$H_A$A)){individual_id=rownames(kinship_result$H_A$A)
}else if(!is.null(kinship_result$H_A$Ainv)){individual_id=rownames(kinship_result$H_A$Ainv)			   
}else if(!is.null(kinship_result$H_D$D)){individual_id=rownames(kinship_result$H_D$D)			   
}else if(!is.null(kinship_result$H_D$Dinv)){individual_id=rownames(kinship_result$H_D$Dinv)}			   
	
  				
  output$H_matrix_download_ind <- downloadHandler(
  
    filename = function() {
        "H_individual_list.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(Id=individual_id,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)
#H_A
  output$H_matrix_download_H_A <- downloadHandler(
  
    filename = function() {
        "H_A_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$H_A$A,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$H_matrix_download_H_A_three <- downloadHandler(
  
    filename = function() {
        "H_A_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$H_A$A_col3,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)					

#H_Ainv
  output$H_matrix_download_H_Ainv <- downloadHandler(
  
    filename = function() {
        "H_Ainv_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$H_A$Ainv,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$H_matrix_download_H_Ainv_three <- downloadHandler(
  
    filename = function() {
        "H_Ainv_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$H_A$Ainv_col3,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)	


#H_D
  output$H_matrix_download_H_D <- downloadHandler(
  
    filename = function() {
        "H_D_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$H_D$D,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$H_matrix_download_H_D_three <- downloadHandler(
  
    filename = function() {
        "H_D_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$H_D$D_col3,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)					

#H_Dinv
  output$H_matrix_download_H_Dinv <- downloadHandler(
  
    filename = function() {
        "H_Dinv_col_all.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$H_D$Dinv,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)

  output$H_matrix_download_H_Dinv_three <- downloadHandler(
  
    filename = function() {
        "H_Dinv_col_three.txt"
    },
    content = function(file) {	
     data.table::fwrite(data.frame(kinship_result$H_D$Dinv_col3,check.names=F),
	                      file,quote=F,row.names=F,col.names=F,sep="\t")
    }  
)
  
  
 })

 
 
    useAutoColor()



    # alerts ------------------------------------------------------------------

    observeEvent(input$show_alert, {
      print("created")
      createAlert(
        id = "alert_anchor",
        options = list(
          title = "Be Careful!",
          status = "danger",
          closable = TRUE,
          width = 12,
          content = "Danger alert preview. This alert is dismissable. 
          A wonderful serenity has taken possession of my entire soul, 
          like these sweet mornings of spring which 
          I enjoy with my whole heart."
        )
      )
    })

    observeEvent(input$hide_alert, {
      print("deleted")
      closeAlert(id = "alert_anchor")
    })

    # alert callback event
    observeEvent(input$alert_anchor, {
      alertStatus <- if (input$alert_anchor) "opened" else "closed"
      toastColor <- if (input$alert_anchor) "bg-lime" else "bg-fuchsia"
      toast(
        title = sprintf("Alert succesfully %s!", alertStatus),
        options = list(
          class = toastColor,
          autohide = TRUE,
          position = "bottomRight"
        )
      )
    })

    # tooltips, popovers ------------------------------------------------------

    # observe({
    #  addTooltip(
    #    id = "controlbarToggle",
    #    options = list(
    #      title = "This toggles the right sidebar",
    #      placement = "bottom"
    #    )
    #  )
    # })



    # current theme info ---------------------------------------------------------

    observeEvent(input$dark_mode, {
      toast(
        title = if (input$dark_mode) "Dark theme on!" else "Light theme on",
        options = list(position = "topRight", class = "bg-warning", autohide = TRUE)
      )
    })
  }
  
# get_vcf_header<-function(date=Sys.time()){
# H1="##fileformat=VCFv4.2"
# H2=paste0("##filedate=",date)
# H3="##source=R-package:blupADC"
# H4="##INFO=<ID=AF,Number=A,Type=Float"
# H5="##INFO=<ID=DR2,Number=A,Type=Float"
# H6="##INFO=<ID=IMP,Number=0,Type=Flag"
# H7="##FORMAT=<ID=GT,Number=1,Type=String,Description=Genotype"
# H8="##FORMAT=<ID=DS,Number=A,Type=Float"
# return(rbind(H1,H2,H3,H4,H5,H6,H7,H8))
# }  
