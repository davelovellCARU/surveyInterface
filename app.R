library(shiny)
library(here)


ui = fluidPage(splitLayout(
  ### Show 'results' as verbatim text output ::::::::::
  verticalLayout(
    textOutput("keyNum"),
    # textOutput("testText"),
    p(verbatimTextOutput("textBlock"))),
  
  ### Run a scrip that gets 'whichKey' from keyListener
  tags$script(
    ### Math.Random() is included in this vector for it's side effect - 
    ### Changes to the value are a hacky way of registering repeat keystrokes
    '
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("whichKey", [e.which,Math.random()]);
    });'
  ),htmlOutput("pdfViewer")))
  
  server = function(input, output, session) {
    ### Libraries ----------------------------------------------------
    library(pdftools)
    library(tidyverse)
    
    ### Functions ----------------------------------------------------
    
    ### inputToNum function turn keystroke data into a number between 
    ### 1 and 9 for presses on the homekeys (and "'")
    inputToNum <-
      function(y){
        (y %>%  
          (safely(function(x)
          {
            as.character(x) %>%
              switch("65" = 1, #a
                     "83" = 2, #s
                     "68" = 3, #d
                     "70" = 4, #f
                     "74" = 5, #j
                     "75" = 6, #k
                     "76" = 7, #l
                     "186" = 8, #;
                     "192" = 9, #'
                     "72" = NA, #h (always yields NA)
                     "8" = 901) # `backspace`
          }
          )))$result
      }

    ### Sever Scripts -------------------------------------------------------------------------------------
    
    ### Split pdf into pages ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    pdfDir = choose.dir(here("www/pdfs"),"Choose folder to process PDFs from")
    pdfFile = dir(pdfDir, full.names =  TRUE)[dir(pdfDir, full.names = TRUE) %>% str_detect("\\.pdf$")][1]
    
    suppressWarnings(dir.create(paste0(pdfDir,"/pdfPages")))
    pdfLength = pdf_length(pdfFile)
    
    pdf <- tibble(pageNo = 1:pdfLength)
    pdf <- pdf %>% 
      mutate(outputFile = map_chr(pageNo,
                                  ~{paste0(pdfDir,"/pdfPages/", as.character(.), ".pdf")})) %>%
      mutate(page = map2(pageNo, outputFile,
                         ~ pdf_subset(pdfFile, .x, output = .y))) %>%
      mutate(http = paste0("http:\\\\127.0.0.1:6056\\",
                           str_remove(page,"^.+\\\\www\\\\")) %>% 
               str_replace_all("\\\\","/"))
    
    ## Initialise reactiveValues :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    values <- reactiveValues()
    values$whichPage <- 1
    values$questions <- "Gender: \r\n"
    
    ### Reactives --------------------------------------------------------------------------------------------
    ### Only want element 1 of 'whichKey' - element 2 only exists to differentiate values
    numPress = reactive({inputToNum(input$whichKey[1])})
    
    ### Outputs ----------------------------------------------------------------------------------------------
    ### PDF iframe html text - updates when input$whichPage changes
    ###  Wrapping my reactiveValues in a reactive wrapper
    ###  Somehow that doesn't sound like the right thing to do 
    pageReactor = reactive({values$whichPage}) 
    
    output$pdfViewer = 
      renderText({
        return(paste('<iframe style="height:800px; width:100%" src="', pdf$http[pageReactor()], '"</iframe>', sep = ""))
      })
    
    output$keyNum = renderText({numPress()})
  
    ### When a new key is pressed:
    observeEvent(input$whichKey,
                 {
                   ### Question and Response Data -----------------------------------------------------------------
                   ## Adult questions ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                   adultQuestions <- 
                     c("Gender:",
                       "Age Group:",
                       "Regularity:",
                       "University:",
                       "Ethnicity:",
                       "Church History:",
                       "Join Reason:",
                       "0-10:",
                       "11-17:",
                       "18-24:",
                       "25+:",
                       "Christian:",
                       "Become here?") %>% 
                     paste0("\r\n")
                   
                   adultResponses <- c("Female", "Male", "Other", rep(NA,7),
                                       "11-15", "16-17", "18-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65-74", "75+",
                                       "Almost always", "About half the time", "A few times a year", rep(NA,7),
                                       "Yes", "No", "I am currently a student", "I have done vocational training/apprenticeship", rep(NA,6),
                                       "White", "Black", "Asian", "Other", "Prefer not to say", rep(NA,5),
                                       "First church been part of", "Returning after a break", "Moved straight from another", "Part of another church as well", rep(NA,6),
                                       "Because I moved here", "Planting team", "Changed church", "Since Child", "Other", rep(NA,5),
                                       rep(c("Not part of a church", "Was part of something", "Stopped during this period", rep(NA,7)),3),
                                       "Not part of a church", "All my life", "Left, came back", "Copmlicated", rep(NA,6),
                                       "Yes", "No", "Exploring", "Complicated", rep(NA,6),
                                       "Yes", "No", "Rediscover", rep(NA,7)) %>%
                     matrix(ncol = 10, byrow = TRUE) %>%
                     t
                   
                   colnames(adultResponses) <- adultQuestions
                   
                   ## Child question :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                   childQuestions <- 
                     c("Gender",
                       "Regularity",
                       "Church History",
                       "Join Reason") %>% 
                     paste0("\r\n")
                   
                   childResponses <- c("BOY", "GIRL", rep(NA,3),
                                       "Almost always", "About half", "Few times", "First or second", "Just visiting",
                                       "Non-churched", "De-churched", "Churched", "Blending", NA, 
                                       "Moved here", "team", "better for us", "before five", "Other") %>% 
                     matrix(ncol = 5, byrow = TRUE) %>% 
                     t
                   
                   colnames(childResponese) <- childQuestions
                   
                   ### `pressHistory`` handilng ----------------------------------------------------------------------
                   ## Flip the page if all questions are answered, then set pH to length 0 :::::::::::::::::::::::::
                   if(length(values$pressHistory) == length(adultQuestions)) {
                     values$whichPage <- values$whichPage + 1
                     values$pressHistory <- numeric(0)
                     
                   } else {
                   ## If pH not 'full', concatenate latest keypress num to pressHistory ::::::::::::::::::::::::::::
                   values$pressHistory <-
                     numPress() %>% 
                     (function(newNum){
                       # A non-assigned key is pressed, or one of the 'standard' keys:
                       if(length(newNum) == 0 || is.na(newNum) || newNum < 900) thisOut <- c(values$pressHistory,newNum) else
                         # Over 900 is `backspace` - so take last val off of pressHistory
                         thisOut <- values$pressHistory[0:(max(0,(length(values$pressHistory)-1)))]
                       return(thisOut)
                     })
                   }
                   
                   ### Use a different set of questions depending on the answer to question 1
                   if(vect[1] == 1) {
                     values$questions <- adultQuestions
                     values$responses <- adultResponses
                   } else if(vect[1] == 2) {
                     values$questions <- childQuestions
                     values$responses <- childResponses
                   }
                   
                   ### Push pressHistory through this function, printing the rendered result ------------------------
                   output$textBlock <-
                     renderText({
                       values$pressHistory %>% 
                         (function(vect) {
                           if (length(vect) > 0) {
                             
                             if(length(vect) <= length(adultQuestions)) {
                            
                             outputString = ""
                             for (i in (1:length(vect))) {
                               
                               outputString <- paste0(outputString,
                                                      adultQuestions[i],
                                                      adultResponses[vect[i], i],
                                                      "\r\n")
                             } #------------------------------------------- for (i in (1:length(vect))) 
                             
                             if(length(vect) == length(adultQuestions)){
                               
                               outputString <- paste0(outputString, "\r\nPress any key to load next survey")
                             }
                             
                            } else outputString <- "Gender:\r\n THIS SHOULDN'T BE HAPPENING" 
                            
                           } else outputString <- "Survey Type:\r\n" #---------- if (length(vect) > 0)
                                              
                           return(outputString)
                                              
                         })
                     })
                 })
    
    }

  shinyApp(server = server, ui = ui)
