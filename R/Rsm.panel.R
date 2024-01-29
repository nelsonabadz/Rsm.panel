#' Interactive panel to visualize and develop a response surface analysis
#'
#' @return A shiny panel with the response surface analysis, based on
#' the specification of the dependent and independent variables of
#' the data set provided, also provides various first- and second-order models,
#' both with and without interaction terms. Additionally, furnishes insights into
#' the optimal region of the surface and its corresponding location.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_freqpoly labs xlab geom_violin geom_jitter geom_boxplot aes_string geom_point
#' @importFrom shiny icon h2 fluidPage navbarPage tabPanel sidebarPanel fileInput selectInput mainPanel tableOutput plotOutput dataTableOutput verbatimTextOutput h3 renderDataTable reactiveValues observeEvent renderPrint shinyApp renderTable renderPlot updateSelectInput
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem menuSubItem dashboardBody tabItems tabItem
#' @importFrom plotly plot_ly plotlyOutput renderPlotly layout
#' @importFrom purrr is_null
#' @importFrom readxl read_xlsx
#' @importFrom rsm rsm
#' @importFrom stats coefficients as.formula median var sd coef
#' @importFrom graphics persp par contour
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#'
#' @examples
#' # Execute the function Rsm.panel()
#' # and provide the dataset. That's all!
#'
#' @export

Rsm.panel = function(){

# Funcion para codificar
codificar <- function(datos, factor1, factor2){

  if (length(unique(datos[[factor1]]))>3){
    ordx1 = sort(unique(datos[[factor1]]))
    x1max = ordx1[4]
    x1min = ordx1[2]

    ordx2 = sort(unique(datos[[factor2]]))
    x2max = ordx2[4]
    x2min = ordx2[2]

  }else {
    x1max = max(datos[[factor1]])
    x1min = min(datos[[factor1]])

    x2max = max(datos[[factor2]])
    x2min = min(datos[[factor2]])

  }



  x1 = (datos[[factor1]] - ((x1max + x1min)/2)) / ((x1max - x1min)/2)
  x2= (datos[[factor2]] - ((x2max + x2min)/2)) / ((x2max - x2min)/2)

  return(list(x1=x1,x2=x2,x1min = x1min, x1max=x1max,x2min = x2min, x2max=x2max))
}

# Funcion Pablo

SRmodels <- function(x1, x2, y, cent.point, ax.point){

  n <- length(y)
  data <- data.frame(x1 = x1, x2 = x2, y = y)

  if ((ax.point == T)){
    mod1ord <- rsm(y ~ FO(x1, x2), data)
    mod2ord <- rsm(y ~ FO(x1, x2)+TWI(x1,x2), data)
    modnonint <- rsm(y ~ FO(x1, x2) + PQ(x1,x2), data)
    modelos <- list(First_Order = mod1ord, Second_Order =mod2ord, Second_Order_no_int = modnonint)
  }
  if ((ax.point == F) & (cent.point == T)){
    mod1ord <- rsm(y ~ FO(x1, x2), data)
    modnonint <- rsm(y ~ FO(x1, x2) + PQ(x1,x2), data)
    modelos <- list(First_Order = mod1ord, Second_Order_no_int = modnonint)
  }

  if ((cent.point == ax.point) == F){
    mod1ord <- rsm(y ~ FO(x1, x2), data)
    modelos <- list(First_Order = mod1ord)
  }

  AIC <- lapply(modelos, AIC)
  BIC <- lapply(modelos, AIC, k = log(n))
  R_2 <- lapply(modelos, function(i){sum <- summary(i);return(sum$r.squared)})
  R_2_ajust <- lapply(modelos, function(i){sum <- summary(i);return(sum$adj.r.squared)})
  MCE <- lapply(modelos, function(i){sum <- summary(i);return((sum$sigma)^2)})

  table <- cbind(AIC, BIC, R_2, R_2_ajust, MCE)

  return(list(modelos = modelos, Metricas = table))
}





# Funcion Gerleen
graficar_rsm <- function(modelo) {
  x1=as.name(matrix(modelo$newlabs,length(coefficients(modelo)),1)[2,])
  x2=as.name(matrix(modelo$newlabs,length(coefficients(modelo)),1)[3,])
  par(mfrow=c(1,2))
  persp(modelo, as.formula(paste(x1,"~",x2)),
        zlab = "sf",
        contours = list(z = "bottom", col = "colors"),
        at = c(summary(modelo$canonical$xs)),
        theta = 50,
        phi = 20)

  contour(modelo, as.formula(paste("~",x1,"+",x2)), image = TRUE)
}


ui <- dashboardPage(skin="blue",
                    #NOMBRE DEL PANEL
                    dashboardHeader(title = "PROJECT"),
                    ## Contenido Sidebar, PANEL IZQUIERDO
                    dashboardSidebar(
                      sidebarMenu(

                        menuItem("HOME", tabName = "ea",icon =icon("house"),
                                 menuSubItem("DATA ENTRY", tabName = "table"),
                                 menuSubItem("DESCRIPTIVE STATISTICS", tabName = "andes")),
                        menuItem("MODELING", tabName = "model",icon=icon("chart-line")),
                        menuItem("RESULTS", tabName = "resultados", icon = icon("diagram-project"))
                      )),

                    #CUERPO


                    dashboardBody(tabItems(




                      # INGRESO DE DATOS------------------------------------------------
                      tabItem(tabName = "table", #"table"
                              h2("RESPONSE SURFACE MODELS", align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("DATA ENTRY",
                                                    sidebarPanel(
                                                      fileInput("myfileinput","Upload excel file"),
                                                      selectInput("myselectinput", "Factor 1",
                                                                  choices = NULL),
                                                      selectInput("myselectinput2", "Factor 2",
                                                                  choices = NULL),
                                                      selectInput("varRespuesta","Response",
                                                                  choices = NULL)
                                                    ),
                                                    mainPanel(tableOutput("mytable"),
                                                              plotOutput("primerDisenio")
                                                    )),
                                           tabPanel("MORE GRAPHICS",
                                                    plotOutput("scatter"),
                                                    plotlyOutput("graph3D"))
                                )
                              )),
                      # ANÁLISIS DESCRIPTIVO DE LOS VALORES Y DE LA VARIABLE RESPUESTA--------------------------------
                      tabItem(tabName = "andes",
                              h2("DESCRIPTIVE ANALYSIS OF THE VALUES AND THE RESPONSE VARIABLE", align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("RESPONSE VARIABLE ANALYSIS",

                                                    plotOutput("histograma"),
                                                    plotOutput("violin1"),
                                                    plotOutput("violin2"),
                                                    dataTableOutput("metricas")
                                           )
                                ))

                      ),
                      # MODELADO ----------------------------------------------------------
                      tabItem(tabName = "model",
                              h2("MODELING", align = "center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("MODELING",
                                                    verbatimTextOutput("mod1"),
                                                    h3("Some model metrics", align = "center"),
                                                    dataTableOutput("resumen"))
                                ))
                      ),
                      # GRAFICACIÓN--------------------------------------------------------------
                      tabItem(tabName = "resultados", #"table"
                              h2("MODEL PLOT", align="center"),
                              fluidPage(
                                navbarPage(tabPanel("EN BLANCO"),
                                           tabPanel("PLOT",
                                                    selectInput("modelo", "Select the model",
                                                                choices = c("First Order",
                                                                            "Second Order",
                                                                            "Second Order without interaction")),
                                                    plotOutput("yerlin"),
                                                    h3("The optimal region is located in: ", align = "center"),
                                                    dataTableOutput("regionMax")
                                           ),
                                           tabPanel("SUMMARY",
                                                    verbatimTextOutput("allModels"))
                                )
                              ))


                    )))





# Define server logic
server <- function(input, output, session) {

  output$tabla1 <- renderDataTable({
    datos

  })




  #Reactive to store loaded data
  reactives <- reactiveValues(

    mydata = NULL,

    # # aqui agregue
    factor1 = NULL,
    factor2 = NULL,
    y = NULL

  )

  # Para acceder a los modelos en cualquier parte
  all_modelos <- reactiveValues()

  X <- reactiveValues()


  # Observar los inputs -----------------------------------------------------


  observeEvent(input$myfileinput, {

    #Store loaded data in reactive
    reactives$mydata <- read_xlsx(input$myfileinput$datapath)

    #Update select input
    updateSelectInput(session, inputId = 'myselectinput',
                      choices  = c("",colnames(reactives$mydata)))


    updateSelectInput(session, inputId = "myselectinput2",
                      choices = c("",colnames(reactives$mydata)))


    updateSelectInput(session, inputId = "myselectinput3",
                      choices = c("",colnames(reactives$mydata)),
                      selected = NULL)

    updateSelectInput(session, inputId = "varRespuesta",
                      choices = c("",colnames(reactives$mydata)))



  })

  #Data table
  output$mytable <- renderTable({

    reactives$mydata


  })




  # SECTION 1: DATA ENTRY ---------------------------------------------------


  observeEvent(c(input$myselectinput,
                 input$myselectinput2,
                 input$varRespuesta
  ), {

    if (!is.null(reactives$mydata)) {


      datos <- reactives$mydata # guardado mis datos en datos

      factor1 <- input$myselectinput #guardada la variable de mis datos en selected_var
      factor2 <- input$myselectinput2
      respuesta <- input$varRespuesta




      output$primerDisenio <- renderPlot({
        codificados <- codificar(datos, factor1, factor2)
        X$x1 <- codificados$x1
        X$x2 <- codificados$x2

        X$x1min <- codificados$x1min
        X$x1max <- codificados$x1max
        X$x2min <- codificados$x2min
        X$x2max <- codificados$x2max

        plot(X$x1,X$x2, pch = 16, main = "Design Graph",
             xlab = factor1,
             ylab = factor2)

      })

      output$scatter <- renderPlot({
        graph1 = ggplot(datos, aes_string(x = factor1, y = respuesta)) +
          geom_point(colour = 3, size = 3)

        graph2 = ggplot(datos, aes_string(x = factor2, y = respuesta)) +
          geom_point(colour = 4, size = 3)

        grid.arrange(graph1, graph2, ncol=2,
                     top="Scatterplots of Factor 1 and Factor 2")

      })

      output$graph3D <- renderPlotly({

        scatter_3d <- plot_ly(datos, x = ~datos[[factor1]], y = ~datos[[factor2]], z = ~datos[[respuesta]], mode = "markers",
                              marker = list(size = 10, opacity = 0.8))

        scatter_3d %>% layout(title = "3D Scatter Chart",
                              scene = list(xaxis = list(title = factor1),
                                           yaxis = list(title = factor2),
                                           zaxis = list(title = respuesta)))


      })

      # SECTION 2 ---------------------------------------------------------------



      output$histograma <- renderPlot({
        ggplot(data = datos,aes(x= datos[[respuesta]]))+
          geom_histogram(bins = 10, fill = "lightgreen", color = "black")+
          geom_freqpoly(bins = 10, color = "black")+
          labs(title = "Histogram of the response variable") +
          xlab(respuesta)
      })

      output$violin1 <- renderPlot({

        ggplot(datos, aes(x = factor1, y = respuesta)) +
          geom_violin(trim = FALSE,adjust = 1L, scale = "area", fill = "#F3990E") +
          geom_jitter()+
          geom_boxplot(width = 1)+ #0.07
          labs(title='Factor 1 Violin Chart')
      })

      output$violin2 <- renderPlot({
        ggplot(datos, aes(x = factor2, y = respuesta)) +
          geom_violin(trim = FALSE,adjust = 1L, scale = "area", fill = "#0EF3F3") +
          geom_jitter()+
          geom_boxplot(width = 1)+ #0.07
          labs(title='Factor 2 Violin Chart')
      })


      output$metricas <- renderDataTable({

        data.frame(Mean = mean(datos[[respuesta]]),
                   Median = median(datos[[respuesta]]),
                   Var = var(datos[[respuesta]]),
                   Sd = sd(datos[[respuesta]]))
      }, options = list(dom = 't'))


      # SECTION 3: MODELING -----------------------------------------------------

      output$mod1 <- renderPrint({
        # X <- codificar(datos, factor1, factor2)
        x1 <- X$x1
        x2 <- X$x2


        # Validar si hay centrales o axiales
        if((0 %in% x1) & (0 %in% x2) ){cent.point=T}else{cent.point=FALSE}
        ax.point <- any(x1 > 1 | x1 < -1 | x2 > 1 | x2 < -1)

        # Guardar en reactives
        X$ax.point <- ax.point
        X$cent.point <- cent.point


        a = SRmodels(X$x1,X$x2,datos[[respuesta]], X$cent.point, X$ax.point)$modelos

        all_modelos$modelos <- a

        all_modelos$modelos

      })

      output$resumen <- renderDataTable({

        a <- as.data.frame(SRmodels(X$x1,X$x2,datos[[respuesta]], X$cent.point, X$ax.point)$Metricas)

        if((!is_null(all_modelos$modelos$Second_Order)) & (!is_null(all_modelos$modelos$Second_Order_no_int))){

          cbind(Model = c("First Order", "Second order", "Second order no interaction"), a)

        } else if(!is_null(all_modelos$modelos$Second_Order) & is_null(all_modelos$modelos$Second_Order_no_int)){
          cbind(Model = c("First Order", "Second order"), a)
        } else{cbind(Model = c("First Order"), a)}


        # cbind(Model = c("First Order", "Second order", "Second order no interaction"), a)
      }, options = list(dom = "t"))



      # SECTION 4: RESULTS ------------------------------------------------------


      output$allModels <- renderPrint({

        aux <- datos
        colnames(aux) <- c("x1", "x2","y")

        if (input$modelo == "First Order") {
          summary(rsm(y ~ FO(x1, x2),aux))

        } else if ((input$modelo == "Second Order") &
                   !(is_null(all_modelos$modelos$Second_Order_no_int))) {

          summary(rsm(y ~ FO(x1, x2)+TWI(x1,x2), aux))

        } else if ((input$modelo == "Second Order without interaction")
                   & !(is_null(all_modelos$modelos$Second_Order_no_int))){

          summary(rsm(y ~ FO(x1, x2)+TWI(x1,x2), aux))

        } else {
          print("It is not possible to make this model")
        }


      })


      output$yerlin <- renderPlot({
        if (input$modelo == "First Order") {
          graficar_rsm(all_modelos$modelos$First_Order)
        } else if ((input$modelo == "Second Order") &
                   !(is_null(all_modelos$modelos$Second_Order_no_int))) {
          graficar_rsm(all_modelos$modelos$Second_Order)
        } else if ((input$modelo == "Second Order without interaction")
                   & !(is_null(all_modelos$modelos$Second_Order_no_int))){
          graficar_rsm(all_modelos$modelos$Second_Order_no_int)
        }else {
          print("No es posible realizar el modelo")
        }


      })

      # output$regionMax <- renderDataTable({
      #
      #   if (input$modelo == "First Order") {
      #     mod = all_modelos$modelos$First_Order
      #     max_coords <- which.max(mod$fitted.values)
      #     max_x <- mod$data[max_coords,][,c(1,2,3)]
      #     colnames(max_x) <- c(factor1, factor2, respuesta)
      #     max_x
      #
      #   } else if ((input$modelo == "Second Order") &
      #              !(is_null(all_modelos$modelos$Second_Order_no_int))) {
      #     mod = all_modelos$modelos$Second_Order
      #     max_coords <- which.max(mod$fitted.values)
      #     max_x <- mod$data[max_coords,][,c(1,2,3)]
      #     colnames(max_x) <- c(factor1, factor2, respuesta)
      #     max_x
      #
      #   } else if ((input$modelo == "Second Order without interaction")
      #              & !(is_null(all_modelos$modelos$Second_Order_no_int))){
      #     mod = all_modelos$modelos$Second_Order_no_int
      #     max_coords <- which.max(mod$fitted.values)
      #     max_x <- mod$data[max_coords,][,c(1,2,3)]
      #     colnames(max_x) <- c(factor1, factor2, respuesta)
      #     max_x
      #   }else {
      #     print("No es posible realizar el modelo")
      #   }
      #
      #
      # }, options = list(dom = "t"))




      output$regionMax <- renderDataTable({

        # Tomar valores de reactive
        x1min <- X$x1min
        x1max <- X$x1max
        x2min <- X$x2min
        x2max <- X$x2max


        if (input$modelo == "First Order") {
          mod = all_modelos$modelos$First_Order
          f <- function(x, y) coef(mod)[1] + coef(mod)[2]*x + coef(mod)[3]*y
          # Crear una secuencia de valores para x y y
          x_vals = seq(-1, 1, length = 100)
          y_vals = seq(-1, 1, length = 100)

          # Crear la matriz z
          z = outer(x_vals, y_vals, f)

          # Inicializar vectores para almacenar los valores óptimos
          optimos_x = numeric(0)
          optimos_y = numeric(0)

          # Iterar sobre los valores de x y y
          for (x_val in x_vals) {
            for (y_val in y_vals) {
              # Calcular el valor de la función del modelo para cada combinación de x e y
              z_val = f(x_val, y_val)

              # Verificar si el valor de la función es cercano al óptimo
              if (z_val > 0.9*max(z) && z_val < 1.1*max(z)) {
                # Almacenar los valores de x e y en la región óptima
                optimos_x = c(optimos_x, x_val)
                optimos_y = c(optimos_y, y_val)
              }
            }
          }

          # Desnormalizar los valores óptimos
          LI_t= (((x1max - x1min)/2) * min(optimos_x)) + ((x1max + x1min)/2)
          LS_t = (((x1max - x1min)/2) * max(optimos_x)) + ((x1max + x1min)/2)

          LI_Temp = (((x2max - x2min)/2) * min(optimos_y)) + ((x2max + x2min)/2)
          LS_Temp = (((x2max - x2min)/2) * max(optimos_y)) + ((x2max + x2min)/2)

          data.frame(LL_Factor1=LI_t, UL_Factor1=LS_t,LL_Factor2 = LI_Temp, UL_Factor2 = LS_Temp)

        } else if ((input$modelo == "Second Order") &
                   !(is_null(all_modelos$modelos$Second_Order_no_int))) {
          mod = all_modelos$modelos$Second_Order
          f <- function(x, y) coef(mod)[1] + coef(mod)[2]*x + coef(mod)[3]*y+coef(mod)[4]*x*y
          # Crear una secuencia de valores para x y y
          x_vals = seq(-1, 1, length = 100)
          y_vals = seq(-1, 1, length = 100)

          # Crear la matriz z
          z = outer(x_vals, y_vals, f)

          # Inicializar vectores para almacenar los valores óptimos
          optimos_x = numeric(0)
          optimos_y = numeric(0)

          # Iterar sobre los valores de x y y
          for (x_val in x_vals) {
            for (y_val in y_vals) {
              # Calcular el valor de la función del modelo para cada combinación de x e y
              z_val = f(x_val, y_val)

              # Verificar si el valor de la función es cercano al óptimo
              if (z_val > 0.9*max(z) && z_val < 1.1*max(z)) {
                # Almacenar los valores de x e y en la región óptima
                optimos_x = c(optimos_x, x_val)
                optimos_y = c(optimos_y, y_val)
              }
            }
          }

          # Desnormalizar los valores óptimos
          LI_t= (((x1max - x1min)/2) * min(optimos_x)) + ((x1max + x1min)/2)
          LS_t = (((x1max - x1min)/2) * max(optimos_x)) + ((x1max + x1min)/2)

          LI_Temp = (((x2max - x2min)/2) * min(optimos_y)) + ((x2max + x2min)/2)
          LS_Temp = (((x2max - x2min)/2) * max(optimos_y)) + ((x2max + x2min)/2)

          data.frame(LL_Factor1=LI_t, UL_Factor1=LS_t,LL_Factor2 = LI_Temp, UL_Factor2 = LS_Temp)

        } else if ((input$modelo == "Second Order without interaction")
                   & !(is_null(all_modelos$modelos$Second_Order_no_int))){
          mod = all_modelos$modelos$Second_Order_no_int
          f <- function(x, y) coef(mod)[1] + coef(mod)[2]*x + coef(mod)[3]*y+coef(mod)[4]*x*x+coef(mod)[5]*y*y
          # Crear una secuencia de valores para x y y
          x_vals = seq(-1, 1, length = 100)
          y_vals = seq(-1, 1, length = 100)

          # Crear la matriz z
          z = outer(x_vals, y_vals, f)

          # Inicializar vectores para almacenar los valores óptimos
          optimos_x = numeric(0)
          optimos_y = numeric(0)

          # Iterar sobre los valores de x y y
          for (x_val in x_vals) {
            for (y_val in y_vals) {
              # Calcular el valor de la función del modelo para cada combinación de x e y
              z_val = f(x_val, y_val)

              # Verificar si el valor de la función es cercano al óptimo
              if (z_val > 0.9*max(z) && z_val < 1.1*max(z)) {
                # Almacenar los valores de x e y en la región óptima
                optimos_x = c(optimos_x, x_val)
                optimos_y = c(optimos_y, y_val)
              }
            }
          }

          # Desnormalizar los valores óptimos
          LI_t= (((x1max - x1min)/2) * min(optimos_x)) + ((x1max + x1min)/2)
          LS_t = (((x1max - x1min)/2) * max(optimos_x)) + ((x1max + x1min)/2)

          LI_Temp = (((x2max - x2min)/2) * min(optimos_y)) + ((x2max + x2min)/2)
          LS_Temp = (((x2max - x2min)/2) * max(optimos_y)) + ((x2max + x2min)/2)

          data.frame(LL_Factor1=LI_t, UL_Factor1=LS_t,LL_Factor2 = LI_Temp, UL_Factor2 = LS_Temp)
        }else {
          print("It's not possible to make the model")
        }


      }, options = list(dom = "t"))



    }
  }

  )
}

# Run the application
shinyApp(ui = ui, server = server)
}
