library(shiny)
library(ggplot2)
library(plotly)

shinyApp(
    ##### ui #######
    ui = fluidPage(
        fluidRow(
            sliderInput("n", 
                        "Number of plots", 
                        value = 1, min = 1, max = 5)),
        fluidRow(
            uiOutput("plots"))
    ), 
    ##### server ######
    server = function(input, output) {
        data("cars")
        # define max number of plots
        max_plots <- 5
        # generate the plots
        output$plots <- renderUI({
            plot_output_list <- lapply(1:input$n, function(i) {
                plotname <- paste0("plot", i)
                plotlyOutput(plotname)
            })
            # convert the list to a tagList - this is necessary for the list of 
            # items to display properly
            do.call(tagList, plot_output_list)
        })
        
        # call renderPlotly for each plot. Plots are only generated when they are 
        # visible on the web page
        for(i in 1:max_plots) {
            # Need local so that each item gets its own number. Without it, the value
            # of i in the renderPlotly() will be the same across all instances, because
            # of when the expression is evaluated
            local({
                my_i <- i
                plotname <- paste0("plot", my_i)
                
                output[[plotname]] <- renderPlotly({
                    g <- ggplot(cars, aes(x = speed, y = dist)) +
                        geom_point() +
                        labs(title = paste0("Plot ", my_i))
                    g <- ggplotly(g)
                    dev.off()
                    g
                })
            })
        }
    }
)
shinyApp(ui = ui, server = server)