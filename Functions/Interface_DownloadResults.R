fluidPage(width = 2,
             tabsetPanel(
               tabPanel("Input",
                        downloadButton("downloadData",label = "Download data")
               ),
               tabPanel("Model",
                        downloadButton("downloadJags",label = "Download JAGS code")
               ),
               tabPanel("Output",
                        downloadButton("downloadModel",label = "Download MCMC samples")
               )
             )
)


