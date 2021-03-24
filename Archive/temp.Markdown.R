
output$downloadReport <-
  downloadHandler(
    "TPpre_Report.pdf",
    content =
      function(file)
      {
        rmarkdown::render(
          input = "Rmarkdown/TPpre_Report.Rmd",
          output_file = "TPpre_Report.pdf",
          params = list(table = table(),
                        plot = plot())
        )
        readBin(con = "TPpre_Report.pdf",
                what = "raw",
                n = file.info("TPpre_Report.pdf")[, "size"]) %>%
          writeBin(con = file)
      }
  )

observe({
  shinyjs::hide(selector = "#navbar li a[data-value=report-tab]")
  #shinyjs::show(selector = "#navbar li a[data-value=model-panel]")
})



output$downloadReport <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  # filename = function() {
  #   paste('my-report', sep = '.', switch(
  #     input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  #   ))
  # },
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    if(input$ID_MeanMedianMode!="Percentiles"){
      tempReport <- file.path("Rmarkdown/temp/TPpre_Report.Rmd")
      file.copy("Rmarkdown/TPpre_Report.Rmd", tempReport, overwrite = TRUE)
    }
    if(input$ID_MeanMedianMode=="Percentiles"){
      tempReport <- file.path("Rmarkdown/temp/TPpre_Report_Perc.Rmd")
      file.copy("Rmarkdown/TPpre_Report_Perc.Rmd", tempReport, overwrite = TRUE)
    }
    if(input$ID_SingleMultiple=="Multiple populations" & input$ID_ZeroPrevalence=="Yes"){
      tempReport <- file.path("Rmarkdown/temp/TPpre_Report_MultZero.Rmd")
      file.copy("Rmarkdown/TPpre_Report_Mult.Rmd", tempReport, overwrite = TRUE)
    }
    if(input$ID_SingleMultiple=="Multiple populations" & input$ID_ZeroPrevalence=="No"){
      tempReport <- file.path("Rmarkdown/temp/TPpre_Report_Mult.Rmd")
      file.copy("Rmarkdown/TPpre_Report_Mult.Rmd", tempReport, overwrite = TRUE)
    }
    # Set up parameters to pass to Rmd document
    params <- list(fb=fb,input=input,table2=table2)
    
    # n = input$n,
    # y = input$y,
    # fb = fb,
    # pop=input$ID_SingleMultiple,
    # zero=input$ID_ZeroPrevalence,
    # pre=input$ID_TrueApp,
    # mea=input$ID_MeanMedianMode
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(input = tempReport,output_format = ,
                      switch(
                        input$format,
                        PDF = pdf_document(),
                        HTML = html_document(),
                        Word = word_document()
                      ),output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)