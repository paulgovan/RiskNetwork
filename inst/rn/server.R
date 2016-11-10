# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 10 * 1024 ^ 2)

# Load demo data from 'bnlearn'
data(learning.test, package = "bnlearn")
data(gaussian.test, package = "bnlearn")
data(insurance, package = "bnlearn")

#' @import bnlearn
#' @import shiny
#' @import shinydashboard
# Define required server logic
shiny::shinyServer(function(input, output, session) {

  # Get the data selection from the user
  dat <- shiny::reactive({
    if (input$net == 1) {
      dat <- learning.test
    } else if (input$net == 2) {
      dat <- gaussian.test
    } else if (input$net == 3) {
      dat <- insurance
    } else if (input$net == 4) {

      # Create an example risk network
      dat <- matrix(NA, ncol = 14)
      colnames(dat) <-
        c(
          "Cause-Delays on other project",
          "Cause-Install scheduled during hurricane season",
          "Risk-vessel delayed on other project",
          "Risk-Inclement weather",
          "Resource-Transport vessel rate",
          "Resource-2 transport vessels",
          "Resource-1 install vessel",
          "Resource-Install vessel rate",
          "Resource-25 HUC personnel",
          "Resource-HUC personnel rate",
          "Task-Jacket, topsides, pile tow",
          "Task-Offshore install",
          "Task-HUC",
          "Project-Platform install"
        )
      dat
    } else  {

      # Get the uploaded file from the user
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      dat <- read.csv(inFile$datapath,
                       header = input$header,
                       sep = input$sep)
    }
  })

  # Learn the structure of the network
  dag <- shiny::reactive({
    if (is.null(dat()))
      return(NULL)

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Learning network structure", value = 0)

    # Create an example risk network
    if (input$net == 4) {
      dag <-
        bnlearn::model2network(
          "[Cause-Delays on other project][Cause-Install scheduled during hurricane season][Risk-vessel delayed on other project|Cause-Delays on other project][Risk-Inclement weather|Cause-Install scheduled during hurricane season][Resource-Transport vessel rate][Resource-2 transport vessels|Risk-Inclement weather][Resource-1 install vessel|Risk-Inclement weather][Resource-Install vessel rate|Risk-vessel delayed on other project][Resource-25 HUC personnel][Resource-HUC personnel rate][Task-Jacket, topsides, pile tow|Resource-Transport vessel rate:Resource-2 transport vessels][Task-Offshore install|Resource-Transport vessel rate:Resource-2 transport vessels:Resource-Install vessel rate:Resource-1 install vessel][Task-HUC|Resource-HUC personnel rate:Resource-25 HUC personnel][Project-Platform install|Task-Jacket, topsides, pile tow:Task-Offshore install:Task-HUC]"
        )

      # Get the selected learning algorithm from the user and learn the network
    } else if (input$alg == "gs") {
      dag <- bnlearn::cextend(bnlearn::gs(dat()), strict = FALSE)
    } else if (input$alg == "iamb") {
      dag <- bnlearn::cextend(bnlearn::iamb(dat()), strict = FALSE)
    } else if (input$alg == "fast.iamb") {
      dag <- bnlearn::cextend(bnlearn::fast.iamb(dat()), strict = FALSE)
    } else if (input$alg == "inter.iamb") {
      dag <- bnlearn::cextend(bnlearn::inter.iamb(dat()), strict = FALSE)
    } else if (input$alg == "hc") {
      dag <- bnlearn::cextend(bnlearn::hc(dat()), strict = FALSE)
    } else if (input$alg == "tabu") {
      dag <- bnlearn::cextend(bnlearn::tabu(dat()), strict = FALSE)
    } else if (input$alg == "mmhc") {
      dag <- bnlearn::cextend(bnlearn::mmhc(dat()), strict = FALSE)
    } else if (input$alg == "rsmax2") {
      dag <- bnlearn::cextend(bnlearn::rsmax2(dat()), strict = FALSE)
    } else if (input$alg == "mmpc") {
      dag <- bnlearn::cextend(bnlearn::mmpc(dat()), strict = FALSE)
    } else if (input$alg == "si.hiton.pc") {
      dag <- bnlearn::cextend(bnlearn::si.hiton.pc(dat()), strict = FALSE)
    } else if (input$alg == "aracne") {
      dag <- bnlearn::cextend(bnlearn::aracne(dat()), strict = FALSE)
    } else
      dag <- bnlearn::cextend(bnlearn::chow.liu(dat()), strict = FALSE)
  })

  # Create the nodes value box
  output$nodesBox <- shiny::renderUI({
    if (is.null(dat()))
      return(NULL)

    # Get the number of nodes in the network
    nodes <- bnlearn::nnodes(dag())
    shinydashboard::valueBox(nodes,
                             "Nodes",
                             icon = shiny::icon("circle"),
                             color = "blue")
  })

  # Create the arcs value box
  output$arcsBox <- shiny::renderUI({
    if (is.null(dat()))
      return(NULL)

    # Get the number of arcs in the network
    arcs <- bnlearn::narcs(dag())
    shinydashboard::valueBox(arcs,
                             "Arcs",
                             icon = shiny::icon("arrow-right"),
                             color = "green")
  })

  # Plot the d3 force directed network
  output$netPlot <- networkD3::renderSimpleNetwork({
    if (is.null(dat()))
      return(NULL)

    # Get the arc directions
    networkData <- data.frame(bnlearn::arcs(dag()))

    networkD3::simpleNetwork(
      networkData,
      Source = "from",
      Target = "to",
      zoom = TRUE
    )
  })

  # Print the network score
  output$score <- shiny::renderText({
    if (is.null(dat()) | input$net == 4)
      return(NULL)
    if (bnlearn::directed(dag())) {

      # If the data is continuous,...
      if (is.numeric(dat()[, 1])) {

        # Get the selected score function from the user and calculate the score
        if (input$type == "loglik") {
          bnlearn::score(dag(), dat(), type = "loglik-g")
        } else if (input$type == "aic") {
          bnlearn::score(dag(), dat(), type = "aic-g")
        } else if (input$type == "bic") {
          bnlearn::score(dag(), dat(), type = "bic-g")
        } else {
          bnlearn::score(dag(), dat(), type = "bge")
        }
      }

      # If the data is discrete,...
      else {
        if (input$type == "loglik") {
          bnlearn::score(dag(), dat(), type = "loglik")
        } else if (input$type == "aic") {
          bnlearn::score(dag(), dat(), type = "aic")
        } else if (input$type == "bic") {
          bnlearn::score(dag(), dat(), type = "bic")
        } else {
          bnlearn::score(dag(), dat(), type = "bde")
        }
      }
    } else
      shiny::validate(
        shiny::need(
          try(score != "")
          , "Make sure your network is completely directed in order to view your network's score..."
        )
      )
  })

  # Fit the model parameters
  fit <- shiny::reactive({
    if (is.null(dat()))
      return(NULL)

    # Create an example risk network
    if (input$net == 4) {
      tf <- c("True", "False")
      cptC1 <- matrix(c(0.75, 0.25),
                      ncol = 2,
                      dimnames = list(NULL, tf))
      cptC2 <- matrix(c(0.5, 0.5),
                      ncol = 2,
                      dimnames = list(NULL, tf))
      cptR1 <- matrix(c(0.95, 0.05, 0.25, 0.75),
                      ncol = 2,
                      dimnames = list(
                        "Risk-vessel delayed on other project" = tf,
                        "Cause-Delays on other project" = tf
                      )
      )
      cptR2 <- matrix(c(0.75, 0.25, 0.4, 0.6),
                      ncol = 2,
                      dimnames = list(
                        "Risk-Inclement weather" = tf,
                        "Cause-Install scheduled during hurricane season" = tf
                      )
      )
      lh <- c("Low", "High")
      cptQ1 <- matrix(
        c(0.1, 0.9, 0.5, 0.5),
        ncol = 2,
        dimnames = list(
          "Resource-2 transport vessels" = lh,
          "Risk-Inclement weather" = tf
        )
      )
      cptU1 <- matrix(c(0.5, 0.5),
                      ncol = 2,
                      dimnames = list(NULL, lh))
      cptQ2 <- matrix(c(0.1, 0.9, 0.5, 0.5),
                      ncol = 2,
                      dimnames = list(
                        "Resource-1 install vessel" = lh,
                        "Risk-Inclement weather" = tf
                      )
      )
      cptU2 <- matrix(c(0.1, 0.9, 0.8, 0.2),
                      ncol = 2,
                      dimnames = list(
                        "Resource-Install vessel rate" = lh,
                        "Risk-vessel delayed on other project" = tf
                      )
      )
      cptQ3 <- matrix(c(0.7, 0.3),
                      ncol = 2,
                      dimnames = list(NULL, lh))
      cptU3 <- matrix(c(0.3, 0.7),
                      ncol = 2,
                      dimnames = list(NULL, lh))
      cptT1 <- c(1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, 1.0)
      dim(cptT1) <- c(2, 2, 2)
      dimnames(cptT1) <-
        list(
          "Task-Jacket, topsides, pile tow" = lh,
          "Resource-2 transport vessels" = lh,
          "Resource-Transport vessel rate" = lh
        )
      cptT2 <-c(1.0,
                0.0,
                0.75,
                0.25,
                0.75,
                0.25,
                0.5,
                0.5,
                0.75,
                0.25,
                0.5,
                0.5,
                0.5,
                0.5,
                0.25,
                0.75,
                0.75,
                0.25,
                0.5,
                0.5,
                0.5,
                0.5,
                0.25,
                0.75,
                0.5,
                0.5,
                0.25,
                0.75,
                0.25,
                0.75,
                0.0,
                1.0
      )
      dim(cptT2) <- c(2, 2, 2, 2, 2)
      dimnames(cptT2) <-
        list(
          "Task-Offshore install" = lh,
          "Resource-2 transport vessels" = lh,
          "Resource-Transport vessel rate" = lh,
          "Resource-1 install vessel" = lh,
          "Resource-Install vessel rate" = lh
        )
      cptT3 <- c(1.0, 0.0, 1.0, 0.0, 0.5, 0.5, 0.0, 1.0)
      dim(cptT3) <- c(2, 2, 2)
      dimnames(cptT3) <-
        list(
          "Task-HUC" = lh,
          "Resource-25 HUC personnel" = lh,
          "Resource-HUC personnel rate" = lh
        )
      cptP <- c(1.0,
                0.0,
                0.67,
                0.33,
                0.67,
                0.33,
                0.33,
                0.67,
                0.67,
                0.33,
                0.33,
                0.67,
                0.33,
                0.67,
                0.0,
                1.0
      )
      dim(cptP) <- c(2, 2, 2, 2)
      dimnames(cptP) <-
        list(
          "Project-Platform install" = lh,
          "Task-Jacket, topsides, pile tow" = lh,
          "Task-Offshore install" = lh,
          "Task-HUC" = lh
        )
      fit <- bnlearn::custom.fit(
        dag(),
        dist = list(
          "Cause-Delays on other project" = cptC1,
          "Cause-Install scheduled during hurricane season" = cptC2,
          "Risk-vessel delayed on other project" = cptR1,
          "Risk-Inclement weather" = cptR2,
          "Resource-2 transport vessels" = cptQ1,
          "Resource-Transport vessel rate" = cptU1,
          "Resource-1 install vessel" = cptQ2,
          "Resource-Install vessel rate" = cptU2,
          "Resource-25 HUC personnel" = cptQ3,
          "Resource-HUC personnel rate" = cptU3,
          "Task-Jacket, topsides, pile tow" = cptT1,
          "Task-Offshore install" = cptT2,
          "Task-HUC" = cptT3,
          "Project-Platform install" = cptP
        )
      )
    } else if (bnlearn::directed(dag())) {
      fit <- bnlearn::bn.fit(dag(), dat(), method = input$met)
    }
  })

  # Create data frame for selected paramater
  param <- shiny::reactive({
    param <- data.frame(coef(fit()[[input$Node]]))
    if (is.numeric(dat()[, 1])) {
      colnames(param) <- "Param"
      param <- cbind(param = rownames(param), param)
      param[, "Param"] <- round(param[, "Param"], digits = 3)
      param <- transform(param, Param = as.numeric(Param))
    } else {
      param[, "Freq"] <- round(param[, "Freq"], digits = 3)
      param <- transform(param, Freq = as.numeric(Freq))
    }
  })

  # Plot Handsontable for selected parameter
  values = shiny::reactiveValues()
  setHot = function(x)
    values[["hot"]] <<- x
  output$hot = rhandsontable::renderRHandsontable({
    if (!is.null(input$hot)) {
      DF = rhandsontable::hot_to_r(input$hot)
    } else {
      DF = param()
    }
    if (is.numeric(dat()[, 1])) {
      col <- "Param"
    } else {
      col <- "Freq"
    }
    setHot(DF)
    rhandsontable::rhandsontable(DF, readOnly = TRUE, rowHeaders = NULL) %>%
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      rhandsontable::hot_col(col, readOnly = FALSE)
  })

  # Add expert knowledge to the model
  expertFit <- shiny::reactive({
    if (!is.null(values[["hot"]])) {
      expertFit <- fit()
      temp <- data.frame(values[["hot"]])
      if (is.numeric(dat()[, 1])) {
        stdev <- as.numeric(fit()[[input$Node]]["sd"])
        expertFit[[input$Node]] <- list(coef = as.numeric(temp[, "Param"]), sd = stdev)
      } else {
        cpt <- coef(expertFit()[[input$Node]])
        cpt[1:length(param()[, "Freq"])] <- as.numeric(temp[, "Freq"])
        expertFit[[input$Node]] <- cpt
      }
    } else {
      expertFit <- fit()
    }
  })

  # Set the paramater graphic options
  graphic <- shiny::reactive({

    # If the data is continuous,...
    if (is.numeric(dat()[, 1])) {
      graphic <- c("Histogram" = "histogram",
                   "XY Plot" = "xyplot",
                   "QQ Plot" = "qqplot")
      # If the data is discrete,...
    } else {
      graphic <- c("Bar Chart" = "barchart",
                   "Dot Plot" = "dotplot")
    }
  })

  # Send the paramater choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "param", choices = graphic())
  })

  # Send the node choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "Node", choices = colnames(dat()))
  })

  # Plot the model parameters
  output$condPlot <- shiny::renderPlot({
    if (is.null(dat()))
      return(NULL)
    if (bnlearn::directed(dag())) {

      # Get the selected graphic from the user and plot the paramaters
      if (input$param == "histogram") {
        bnlearn::bn.fit.histogram(fit())
      } else if (input$param == "xyplot") {
        bnlearn::bn.fit.xyplot(fit())
      } else if (input$param == "qqplot") {
        bnlearn::bn.fit.qqplot(fit())
      } else if (input$param == "barchart") {
        bnlearn::bn.fit.barchart(fit()[[input$Node]])
      } else if (input$param == "dotplot") {
        bnlearn::bn.fit.dotplot(fit()[[input$Node]])
      }
    } else
      shiny::validate(
        shiny::need(
          try(condPlot != ""), "Make sure your network is completely directed in order to view the paramater infographics..."
        )
      )
  })

  # Send the event node choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "event", choices = names(dat()))
  })

  # Send the event node choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "evidenceNode", choices = names(dat()))
  })

  # Send the evidence choices to the user
  shiny::observe({
    whichNode <- which(colnames(dat()) == input$evidenceNode)
    evidenceLevels <- as.vector(unique(dat()[,whichNode]))
    shiny::updateSelectInput(session, "evidence", choices = evidenceLevels)
  })

  # Send the event node choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "event", choices = names(dat()))
  })

  # Perform Bayesian Inference based on evidence and plot results
  output$distPlot <- shiny::renderPlot({
    if (is.null(dat()))
      return(NULL)
    if (is.numeric(dat()[,1]))
      shiny::validate(
        shiny::need(
          try(distPlot != ""),
          "Inference is currently not supported for continuous variables..."
        )
      )

    # Create a string of the selected evidence
    str1 <<- paste0("(", input$evidenceNode, "=='", input$evidence, "')")

    # Estimate the conditional PD and tabularize the results
    nodeProbs <- prop.table(table(bnlearn::cpdist(fit(), input$event, eval(parse(text = str1)))))

    # Create a bar plot of the conditional PD
    barplot(
      nodeProbs,
      col = "lightblue",
      main = "Conditional Probabilities",
      border = NA,
      xlab = "Levels",
      ylab = "Probabilities",
      ylim = c(0, 1)
    )
  })

  # Send the node names to the user
  shiny::observe({
    shiny::updateSelectInput(session, "nodeNames", choices = colnames(dat()))
  })

  # Get the selected node measure from the user and print the results
  output$nodeText <- shiny::renderText({
    if (is.null(dat()))
      return(NULL)
    if (input$nodeMeasure == "mb") {
      bnlearn::mb(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "nbr") {
      bnlearn::nbr(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "parents") {
      bnlearn::parents(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "children") {
      bnlearn::children(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "in.degree") {
      bnlearn::in.degree(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "out.degree") {
      bnlearn::out.degree(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "incident.arcs") {
      bnlearn::incident.arcs(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "incoming.arcs") {
      bnlearn::incoming.arcs(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "outgoing.arcs") {
      bnlearn::outgoing.arcs(dag(), input$nodeNames)
    } else
      bnlearn::incident.arcs(dag(), input$nodeNames)
  })

  # Get the selected network measure from the user and plot the results
  output$netTable <- d3heatmap::renderD3heatmap({
    if (is.null(dat()))
      return(NULL)

    # Plot a d3 heatmap of the adjacency matrix
    d3heatmap::d3heatmap(
      bnlearn::amat(dag()),
      dendrogram = input$dendrogram,
      symm = TRUE,
      cexRow = 0.7,
      cexCol = 0.7,
      colors = "Blues"
    )
  })

  # Get the sample size from the user and simulate data from the network
  simData <- shiny::reactive({
    simData <- bnlearn::rbn(fit(), input$n)
  })

  # Create a handler for downloading the simulated data
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste('bn', '.csv', sep = '')
    },
    content = function(file) {
      write.csv(simData(), file)
    }
  )
})
