library(shiny)
library(dplyr)
library(ggplot2)
library(wordcloud2)
library(shinyWidgets)
library(plotly)
library(stopwords)
library(udpipe)
library(googlesheets4)

options(gargle_oauth_cache = "secrets",
        gargle_oauth_email = "youremail@email.com")

gs4_auth()


#cargar modelo NLP
udmodelAncora <- udpipe_load_model("data/spanish-ancora-ud-2.5-191206.udpipe")

#function to resize text input area
textAreaInput2 <- function (inputId, label, value = "", width = NULL, height = NULL, 
                            cols = NULL, rows = NULL, placeholder = NULL, resize = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", 
                                  "horizontal"))
  }
  style <- paste("max-width: 100%;", if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), if (!is.null(height)) 
      paste0("height: ", validateCssUnit(height), ";"), if (!is.null(resize)) 
        paste0("resize: ", resize, ";"))
  if (length(style) == 0) 
    style <- NULL
  div(class = "form-group", 
      tags$label(label, `for` = inputId), tags$textarea(id = inputId, 
                                                        class = "form-control", placeholder = placeholder, style = style, 
                                                        rows = rows, cols = cols, value))
}

#add NRC data
NRC <- readRDS("data/NRC.rds")

#add stopwords
stopwords <- readRDS("data/stopwords.rds")

# Define UI for application
ui <- fluidPage(
  setBackgroundImage(src = "seaWaves.gif"),
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    .well {
    min-height: 0px;
    padding: 19px;
    margin-bottom: 20px;
    background-color: transparent;
      border: 1px solid transparent;
    border-radius: 4px;
    -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
    box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
    }
    
    .h2, h2 {
    font-size: 20px;
    color: yellow;
    }
    
    .form-control {
    display: block;
    width: 100%;
    height: 34px;
    padding: 6px 12px;
    font-size: 14px;
    line-height: 1.42857143;
    color: #fff;
    background-color: #000;
    background-image: none;
    border: 1px solid #ccc;
    border-radius: 4px;
    -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 8%);
    box-shadow: inset 0 1px 1px rgb(0 0 0 / 8%);
    -webkit-transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s;
    -o-transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s;
    -webkit-transition: border-color ease-in-out .15s,-webkit-box-shadow ease-in-out .15s;
    transition: border-color ease-in-out .15s,-webkit-box-shadow ease-in-out .15s;
    transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s;
    transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s,-webkit-box-shadow ease-in-out .15s;
                    }
    .bttn-unite.bttn-default {
    border-color: #AAAAAA;
    color: #AAAAAA;
    background-color: transparent;
                    }"))
  ),

  # Application title
  titlePanel(HTML("La máquina de escribir<br>que detecta<br>emociones")),

  # Show a plot
  wellPanel(
    column(8, align="center",
           #div(style = "height:200px;"),
           wordcloud2Output("distPlot")),
    column(4, align="center",
           plotlyOutput("indices")
      )
      ),
  
  #create a row for the text input
  fluidRow(
    column(8, align="center", offset = 2,
           textAreaInput2("miTexto", label = "", width = "100%", height = 200,
                          placeholder = "Comienza a escribir tu historia aquí..."),
           #verbatimTextOutput("prueba"),
           actionBttn("enviar", label = "Envía tu historia al muro!", 
                      icon = icon("share"),
                      style = "unite",
                      size = "sm"),
           HTML("<br><br><br>")
           )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #con udpipe
  xTexto <- reactive(udpipe_annotate(udmodelAncora, x = input$miTexto) %>% 
                        as_tibble() %>% 
                        #(!xpos=="PUNCT") %>% 
                        select(lemma) %>% 
                        #filter(!lemma%in%stopwords$word) %>% 
                        filter(lemma%in%NRC$palabras) %>% 
                        count(lemma, sort = TRUE) #%>% 
                        #mutate(n=ifelse(lemma%in%NRC$palabras, n*1.8, n))
                      )

  #nube de palabras
    output$distPlot <- renderWordcloud2({
      if (req(nchar(input$miTexto))>140) {
      xTexto()  %>% 
        select(lemma, n) %>% 
        wordcloud2(
          size = 0.35,
          color = "random-light",
          backgroundColor = "transparent")
      }
    })
    
    forPlot <- reactive(xTexto() %>%
                          right_join(NRC, by = c("lemma"="palabras")) %>%
                          filter(!is.na(n)) %>%
                          group_by(emocion) %>%
                          summarise(n=sum(n)) %>%
                          ungroup() %>%
                          #mutate(n=n/sum(n)) %>% 
                          mutate(text=paste0(emocion, ": ", n))
    )
    
    forScore <- reactive(forPlot() %>% 
                           filter(emocion=="MIEDO") %>% 
                           pull(n)
                         )
    
    #ratings de emociones
    
  output$indices <- renderPlotly({
    #if (length(miTexto()$lemma)>10){
      plot <- forPlot() %>% 
        ggplot(aes(emocion, n, fill=emocion, text=text))+
        geom_col()+
        coord_flip()+
        xlab(NULL)+
        ylab(NULL)+
        theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent'),
          axis.text.x = element_text(colour = "white"),
          axis.text.y = element_text(colour = "white"),
          legend.position = "none"
        )
      
      ggplotly(plot, tooltip = c("text")) %>% config(displayModeBar = FALSE)
    #} else {NULL}
  })

    
    #eventos basados en el botón
    observeEvent(input$enviar, {
      tibble(texto=input$miTexto,
             Puntuación=forScore()) %>% 
        sheet_append(., ss="1uKYZ01TG0B3qfBp2doCxw-2e6WnkLxqXdWZJJ4UalCU",
                     sheet = "historias")
      
      updateTextInput(session, "miTexto", value = "")     
    })
    
    # output$prueba <- renderPrint(print(tibble(texto=input$miTexto,
    #                                           score=forScore())
    #                                    ))
}

# Run the application 
shinyApp(ui = ui, server = server)
