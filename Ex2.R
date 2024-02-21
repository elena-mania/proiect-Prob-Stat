library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)
library(MASS)

#Pentru interfata utilizatorului am impartit in 2 sectiuni : sectiunea de input si cea de output 

ui <- fluidPage(
  titlePanel("Integrarea Functiilor Bidimensionale folosind Teorema lui Fubini"),
  sidebarLayout(
    sidebarPanel(
      textInput("func", "Introduceti functia f(x, y):", "x^2 + y^2"),
      numericInput("xmin", "Limita inferioara x:", -1),
      numericInput("xmax", "Limita superioara x:", 1),
      numericInput("ymin", "Limita inferioara y:", -1),
      numericInput("ymax", "Limita superioara y:", 1),
      actionButton("calc", "Calculeaza"),
      actionButton("check_density", "Verifica Densitatea de Probabilitate"),
      selectInput("var_type", "Selectati tipul variabilei aleatoare:",
                  choices = c("Unidimensionala", "Bidimensionala")),
      textInput("density", "Introduceti densitatea de probabilitate:", "1/(sqrt(2*pi)) * exp(-x^2/2)"),
      actionButton("create_var", "Creeaza Variabila Aleatoare"),
      actionButton("calc_marginal", "Calculeaza Densitatile Marginale"),
      actionButton("calc_conditional", "Calculeaza Densitatile Conditionate"),
      
      numericInput("x_val_conditional", "Valoarea X pentru densitatea conditionata:", 0),
      numericInput("y_val_conditional", "Valoarea Y pentru densitatea conditionata:", 0),
      
      numericInput("param1", "Parametrul 1 (medie pentru unidimensional):", 0),
      numericInput("param2", "Parametrul 2 (deviatie standard pentru unidimensional):", 1),
      selectInput("graphic_type", "Alegeti tipul graficului:",
                  choices = c("Densitate", "Functie de Repartitie")),
      actionButton("plot_graphic", "Reprezinta Graficul")
    ),
    mainPanel(
      plotlyOutput("plot"),  
      plotlyOutput("plot3D"), 
      textOutput("result"),
      plotOutput("graphic_plot") 
    )
  )
)

server <- function(input, output, session) {
  f <- function(x, y) {
    sapply(X = x, FUN = function(x) eval(parse(text = paste("(", input$func, ")")), list(x = x, y = y)))
    # calculez valorile functiei introduse de user pentru setul de valori “x” avand in vedere valorile “y” 
  }
  
  #punctul a si b
  observeEvent(input$calc, {
    tryCatch({ # tryCatch pentru erori 
      test_val <- f(0, 0) # fac o valoare de test 
      if (length(test_val) != 1) { 
        stop("Functia trebuie sa returneze o singura valoare.")
      }
    }, error = function(e) {
      output$result <- renderText(paste("Eroare: Functia introdusa nu este valida. Eroare:", e$message))
      return()
    })
    
    # calculez integrala 
    result <- tryCatch({
      integral <- integrate(Vectorize(function(x) { # integrarea functiei in raport cu x 
        integrate(Vectorize(function(y) f(x, y)), input$ymin, input$ymax)$value # integrarea #functiei in raport cu y 
      }), input$xmin, input$xmax)$value # limitele de integrare 
      paste("Valoarea integralei este: ", integral)
    }, error = function(e) {
      paste("Eroare la calculul integralei: ", e$message)
    })
    
    output$result <- renderText(result)
    
    #generarea valorilor pentru x si y 
    xvals <- seq(input$xmin, input$xmax, length.out = 30)
    yvals <- seq(input$ymin, input$ymax, length.out = 30)
    grid <- expand.grid(x = xvals, y = yvals) #si creez grila 
    grid$z <- mapply(f, grid$x, grid$y)
    
    #afisarea grafului interactiv 
    output$plot <- renderPlotly({
      p <- ggplot(grid, aes(x = x, y = y, z = z)) + 
        geom_tile(aes(fill = z)) + 
        stat_contour() +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Grafic 3D
    output$plot3D <- renderPlotly({
      #fac o matrice z cu rezultatele functiei (x,y) pentru fiecare punct 
      zvals <- outer(xvals, yvals, Vectorize(f))
      plot_ly(x = ~xvals, y = ~yvals, z = ~zvals, type = "surface")
    })
  })
  
  #punctul c 
  observeEvent(input$check_density, {
    #verific daca valorile functiei sunt pozitive pe grid
    is_positive <- tryCatch({
      test_grid <- expand.grid(x = seq(-10, 10, length.out = 100), y = seq(-10, 10, length.out = 100))
      all(mapply(f, test_grid$x, test_grid$y) >= 0)
    }, error = function(e) {
      FALSE
    })
    
    #calculez integrala totala a functiei 
    integral_total <- tryCatch({
      integrate(Vectorize(function(x) {
        integrate(Vectorize(function(y) f(x, y)), -Inf, Inf)$value
      }), -Inf, Inf)$value
    }, error = function(e) {
      NA
    })
    
    #verific daca integrala totala este aproape de 1 si valorile sunt pozitive
    is_density <- is_positive && !is.na(integral_total) && abs(integral_total - 1) < 0.01
    
    #afisez rezultatul 
    density_result <- if (is_density) {
      "Functia introdusa este o densitate de probabilitate."
    } else {
      "Functia introdusa NU este o densitate de probabilitate."
    }
    
    output$result <- renderText(density_result)
  })
  
  #punctul d 
  
  observeEvent(input$create_var, {
    if (input$var_type == "Unidimensionala") {
      tryCatch({
        #evalueaz functia de densitate de probabilitate 
        dens_func <- eval(parse(text = paste("function(x) {", input$density, "}")))
        
        # verific daca integrala densitatii de probabilitate este 1
        
        if (abs(integrate(dens_func, -100, 100)$value - 1) > 0.01) {
          stop("Densitatea de probabilitate nu este valida.")
        }
        
        #creez o variabila aleatoare unidimensionala 
        var_aleatoare <- stats::rnorm(1000) 
        #mesajul de confirmare 
        output$result <- renderText("Variabila aleatoare unidimensionala creata.")
      }, error = function(e) {
        output$result <- renderText(paste("Eroare:", e$message))
      })
    } else if (input$var_type == "Bidimensionala") {
      tryCatch({
        #creez o functie de densitate de probabilitate bidimensionala 
        dens_func_bi <- eval(parse(text = paste("function(x, y) {", input$density, "}")))
        #verific daca integrala totala a densitatii de probabilitate este aprox 1 
        integral_valid <- abs(integrate(function(x) {
          integrate(function(y) dens_func_bi(x, y), -100, 100)$value
        }, -100, 100)$value - 1) < 0.01
        if (!integral_valid) {
          stop("Densitatea de probabilitate bidimensionala nu este valida.")
        }
        #presupun o distributie normala bidimensionala 
        mu <- c(input$param1, input$param1)
        sigma <- matrix(c(input$param2^2, 0, 0, input$param2^2), nrow = 2)
        #generez o variabila aleatoare bidimensionala 
        var_aleatoare_bi <- mvrnorm(1000, mu = mu, Sigma = sigma)
        output$result <- renderText("Variabila aleatoare bidimensională creata.")
      }, error = function(e) {
        output$result <- renderText(paste("Eroare:", e$message))
      })
    }
  })
  
  #punctul e 
  
  observeEvent(input$calc_marginal, {
    tryCatch({
      #limita pentru intervalul de integrare
      interval_limit <- 10 
      
      #calculez densitatea marginala X prin integrarea functiei f(0,y) pe intervalul [-10,10]
      marginal_X <- integrate(function(y) f(0, y), -interval_limit, interval_limit)$value
      #calculez densitatea marginala Y prin integrarea functiei f(x,0) pe intervalul [-10,10]
      marginal_Y <- integrate(function(x) f(x, 0), -interval_limit, interval_limit)$value
      #rezultatul
      output$result <- renderText(paste("Densitatea marginala X: ", marginal_X, 
                                        "\nDensitatea marginala Y: ", marginal_Y))
      #daca apar erori
    }, error = function(e) {
      output$result <- renderText(paste("Eroare la calculul densitatilor marginale: ", e$message, 
                                        "\nVerificati functia si intervalul de integrare."))
    })
  })
  
  observeEvent(input$calc_conditional, {
    tryCatch({
      #intervalul limita pentru integrare
      interval_limit <- 10
      
      #calculul densitatii marginale X
      marginal_X_func <- function(x) {
        integrate(function(y) f(x, y), -interval_limit, interval_limit)$value
      }
      #calculul densitatii marginale Y
      marginal_Y_func <- function(y) {
        integrate(function(x) f(x, y), -interval_limit, interval_limit)$value
      }
      
      #calculul densitatii conditionate X de Y si invers 
      conditional_X_given_Y <- function(x, y) {
        f(x, y) / marginal_Y_func(y)
      }
      conditional_Y_given_X <- function(x, y) {
        f(x, y) / marginal_X_func(x)
      }
      
      #preiau valorile introduse de utilizator
      x_val <- input$x_val_conditional
      y_val <- input$y_val_conditional
      
      #densitatile conditionate
      conditional_X_at_given_Y <- conditional_X_given_Y(x_val, y_val)
      conditional_Y_at_given_X <- conditional_Y_given_X(x_val, y_val)
      
      output$result <- renderText(paste("f(X|Y=", y_val, ") la X=", x_val, ": ", conditional_X_at_given_Y,
                                        "\nf(Y|X=", x_val, ") la Y=", y_val, ": ", conditional_Y_at_given_X))
    }, error = function(e) {
      output$result <- renderText(paste("Eroare la calculul densităților condiționate: ", e$message))
    })
  })
  
  # punctul f 
  observeEvent(input$plot_graphic, {
    #pentru graficul densitate si variabila unidimensionala 
    if (input$graphic_type == "Densitate") {
      if (input$var_type == "Unidimensionala") {
        
        #fac un set de date pentru a reprezenta grafic densitatea unidimensionala 
        data <- data.frame(x = seq(input$xmin, input$xmax, length.out = 100))
        
        #calculez densitatea pentru fiecare punct x ( folosesc ditributia normala )
        data$y <- dnorm(data$x, mean = input$param1, sd = input$param2)
        #creez si afisez graficul cu pplot 
        p <- ggplot(data, aes(x, y)) + geom_line() + ggtitle("Densitate Unidimensionala")
        output$graphic_plot <- renderPlot(p)
      } else {
        if (input$var_type == "Bidimensionala" && input$graphic_type == "Densitate") {
          
          #fac un grid pentru a reprezenta grafic densitatea bidimensionala 
          xvals <- seq(input$xmin, input$xmax, length.out = 30)
          yvals <- seq(input$ymin, input$ymax, length.out = 30)
          grid <- expand.grid(x = xvals, y = yvals)
          
          #setez parametrii pentru ditributia normala bidimensionala 
          mu <- c(input$param1, input$param1) 
          sigma <- matrix(c(input$param2^2, 0, 0, input$param2^2), nrow = 2) 
          #calculez densitatea pentru fiecare pereche (x,y) din grid
          grid$z <- apply(grid, 1, function(v) dmvnorm(v[1:2], mean = mu, sigma = sigma))
          
          #creez si afisez graficul 
          output$graphic_plot <- renderPlot({
            ggplot(grid, aes(x, y, z = z)) + 
              geom_tile(aes(fill = z)) + 
              stat_contour() +
              ggtitle("Densitate Bidimensionala")
          })
        }
      }
    } else {
      if (input$var_type == "Unidimensionala") {
        
        #fac un set de date pentru reprezentarea grafica a functiei de repartitie unidimensionala
        data <- data.frame(x = seq(input$xmin, input$xmax, length.out = 100))
        
        #calculez functia de repartitie pentru fiecare punct x folosind distributia normala
        data$y <- pnorm(data$x, mean = input$param1, sd = input$param2)
        #creez si afisez graficul
        p <- ggplot(data, aes(x, y)) + geom_line() + ggtitle("Functie de Repartitie Unidimensionala")
        output$graphic_plot <- renderPlot(p)
      } else {
        
        if (input$var_type == "Bidimensionala" && input$graphic_type == "Functie de Repartitie") {
          
          #fac un grid pentru reprezentarea grafica a functiei de repartitie bidimensionala
          xvals <- seq(input$xmin, input$xmax, length.out = 30)
          yvals <- seq(input$ymin, input$ymax, length.out = 30)
          grid <- expand.grid(x = xvals, y = yvals)
          #parametrii pentru distributia normala bidimensionala 
          mu <- c(input$param1, input$param1) 
          sigma <- matrix(c(input$param2^2, 0, 0, input$param2^2), nrow = 2) 
          #calculez probabilitatea cumulativa pentru fiecare pereche (x,y) din grid 
          grid$z <- apply(grid, 1, function(v) pmvnorm(lower = -Inf, upper = v[1:2], mean = mu, sigma = sigma))
          #fac graficul si il afisez 
          output$graphic_plot <- renderPlot({
            ggplot(grid, aes(x, y, z = z)) + 
              geom_tile(aes(fill = z)) + 
              stat_contour() +
              ggtitle("Functie de Repartitie Bidimensionala")
          })
        }
      }
    }
  })
  
  
}

shinyApp(ui = ui, server = server)