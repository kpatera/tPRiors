fluidPage(width = 2,
             tabsetPanel(
               tabPanel("Download Model (MCMC samples)",
                        downloadButton("downloadModel")
               )
             )
)