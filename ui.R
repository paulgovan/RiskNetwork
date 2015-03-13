# Copyright 2015 Paul Govan

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(shiny)
library(shinyapps)
library(shinydashboard)
library(networkD3)

dashboardPage(skin="black",
              dashboardHeader(title = "RiskNetwork",
                              dropdownMenu(type = "messages",
                                           messageItem(
                                             from = "Support",
                                             message = "Welcome to RiskNetwork!"
                                           )
                              )),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Structure", icon = icon("globe"), tabName = "structure",
                           badgeLabel = "New", badgeColor = "green"),
                  menuItem("Parameters", tabName = "paramaters", icon = icon("bar-chart")),
                  menuItem("Inference", icon = icon("arrows"), tabName = "inference",
                           badgeLabel = "Coming Soon", badgeColor = "yellow"),
                  menuItem("Measures", tabName = "measures", icon = icon("table")),
                  menuItem("Simulation", tabName = "simulation", icon = icon("random"))
                  
                )),
              dashboardBody(
                tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                          tags$title("RiskNetwork")),
                tabItems(
                  tabItem(tabName = "dashboard",
                          fluidRow(
                            box(
                              title = "RiskNetwork", status = "primary", solidHeader = TRUE, width = 8,
                              img(src = "favicon.png", height = 50, width = 50),
                              h3("Welcome to RiskNetwork!"),
                              h4("RiskNetwork is a web app for risk network modeling and analysis. Click", em("Structure"), "in the sidepanel to get started."),
                              h4('Powered by',
                                 a(href = 'http://christophergandrud.github.io/networkD3/', 'networkD3'),
                                 ',',
                                 a(href = 'http://www.bnlearn.com', 'bnlearn'),
                                 'and',
                                 a(href = 'http://shiny.rstudio.com/', 'Shiny.')),
                              h4('Copyright 2015 By Paul Govan. ',
                                 a(href = 'http://www.apache.org/licenses/LICENSE-2.0', 'Terms of Use.'))
                            ),
                            uiOutput("nodesBox"),
                            uiOutput("arcsBox")
                          )
                  ),
                  tabItem(tabName = "structure",
                          fluidRow(
                            column(width = 4,
                                   box(
                                     title = "Network Input", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                     helpText("Select a sample network, upload your risk network data, or create your own risk network:"),
                                     selectInput("net", h5("Risk Network:"), 
                                                 c("Sample Discrete Network"=1,
                                                   "Sample Gaussian Network"= 2,
                                                   "Sample Insurance Network"=3,
                                                   "Sample Project Risk Network"=4,
                                                   "Upload your risk network data"=5,
                                                   "Create your risk network (coming soon)"=6
                                                 )),
                                     conditionalPanel(condition = "input.net == 5",
                                                      p('Note: your data should be structured as a ',
                                                        a(href = 'http://en.wikipedia.org/wiki/Comma-separated_values', 'csv file')),
                                                      fileInput('file', strong('File Input:'),
                                                                accept = c(
                                                                  'text/csv',
                                                                  'text/comma-separated-values',
                                                                  'text/tab-separated-values',
                                                                  'text/plain',
                                                                  '.csv',
                                                                  '.tsv'
                                                                )
                                                      ),
                                                      checkboxInput('header', 'Header', TRUE),
                                                      selectInput('sep', strong('Separator:'),
                                                                  c(Comma=',',
                                                                    Semicolon=';',
                                                                    Tab='\t'),
                                                                  ','))
                                   ),
                                   conditionalPanel(condition = "input.net != 4",
                                                    box(
                                                      title = "Structural Learning", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                                      helpText("Select a structural learning algorithm:"),
                                                      selectizeInput("alg", h5("Learning Algorithm:"), 
                                                                     choices = list("Constraint-based Learning"=
                                                                                      c("Grow-Shrink"="gs",
                                                                                        "Incremental Association"="iamb",
                                                                                        "Fast IAMB"="fast.iamb",
                                                                                        "Inter IAMB"="inter.iamb"),
                                                                                    "Score-based Learning"=
                                                                                      c("Hill Climbing"="hc",
                                                                                        "Tabu"="tabu"),
                                                                                    "Hybrid Learning"=
                                                                                      c("Max-Min Hill Climbing"="mmhc",
                                                                                        "2-phase Restricted Maximization"='rsmax2'),
                                                                                    "Local Discovery Learning"=
                                                                                      c("Max-Min Parents and Children"='mmpc',
                                                                                        "Semi-Interleaved HITON-PC"="si.hiton.pc",
                                                                                        "ARACNE"="aracne", 
                                                                                        "Chow-Liu"="chow.liu"))
                                                      )
                                                    )
                                   ),
                                   conditionalPanel(condition = "input.net != 4",
                                                    box(
                                                      title = "Network Score", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                                      selectInput("type", h5("Network Score:"),
                                                                  c("Log-Likelihood"="loglik",
                                                                    "Akaike Information Criterion"="aic",
                                                                    "Bayesian Information Criterion"="bic",
                                                                    "Bayesian Equivalent"="be"),
                                                                  'loglik-g'),
                                                      verbatimTextOutput("score")
                                                    )
                                   )
                            ),
                            column(width = 8,
                                   box(
                                     title = "Risk Network", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                     simpleNetworkOutput("netPlot")
                                   )
                            )
                          )
                  ),
                  tabItem(tabName = "paramaters",
                          fluidRow(
                            column(width = 4,
                                   box(
                                     title = "Paramater Learning", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                     helpText("Select a parameter learning method:"),
                                     selectInput("met", h5("Learning Method:"), 
                                                 c("Maximum Likelihood Estimation"="mle",
                                                   "Bayesian Estimation"="bayes"
                                                 ))
                                   ),
                                   box(
                                     title = "Paramater Controls", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                     helpText("Select a paramater infographic:"),
                                     selectInput("param", label = h5("Paramater Infographic:"),
                                                 ""),
                                     conditionalPanel(condition = "input.param=='barchart'|input.param=='dotplot'",
                                                      selectInput("Node", label = h5("Node:"),
                                                                  ""))
                                   )
                            ),
                            column(width = 8,
                                   box(
                                     title = "Network Paramaters", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                     plotOutput("condPlot")
                                   )
                            )
                          )
                  ),
                  tabItem(tabName = "measures",
                          fluidRow(
                            box(
                              title = "Node Control", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4,
                              helpText("Select a node measure:"),
                              selectInput("nodeMeasure", h5("Node Measure:"), 
                                          c("Markov Blanket"="mb",
                                            "Neighborhood"="nbr",
                                            "Parents"="parents",
                                            "Children"="children", 
                                            "In Degree"="in.degree",
                                            "Out Degree"="out.degree",
                                            "Incident Arcs"="incident.arcs",
                                            "Incoming Arcs"="incoming.arcs",
                                            "Outgoing Arcs"="outgoing.arcs"
                                          )),
                              selectInput("nodeNames", label = h5("Node:"),
                                          "")
                            ),
                            box(
                              title = "Node Measure", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                              verbatimTextOutput("nodeText")
                            )
                          ),
                          fluidRow(
                            box(
                              title = "Network Control", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4,
                              helpText("Select a network measure:"),
                              selectInput("netMeasure", h5("Network Measure:"), 
                                          c("Adjacency Matrix"="amat",
                                            "Arcs"="arcs",
                                            "Directed Arcs"="directed.arcs",
                                            "Undirected Arcs"="undirected.arcs",
                                            "Root Nodes"="root.nodes",
                                            "Leaf Nodes"="leaf.nodes",
                                            "Compelled Arcs"="compelled.arcs"
                                          ))
                            ),
                            box(
                              title = "Network Measure", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                              dataTableOutput("netTable")
                            )
                          )
                  ),
                  tabItem(tabName = "simulation",
                          fluidRow(
                            column(width = 4,
                                   box(
                                     title = "Network Simulation", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                     helpText("Simulate random data from your network and download for future use:"),
                                     numericInput("n", label = h5("N (Sample Size):"), value = 100, min = 0),
                                     downloadButton('downloadData', 'Download')
                                   )
                            )
                          )
                  )
                )
              )
)
