## This is a oneLine script that runs the shiny app on the right port
require("here")
require("shiny")

runApp("app.R",port = 6056,
       display.mode = "normal")
