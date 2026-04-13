# Define the About Module in Shiny App
# This script defines the user interface (UI) and server for the about module
# Author: Timothy Hackmann
# Date: 26 February 25

# === Define user interface (UI) ===
aboutUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Title
    create_title_div("About"),
    
    bslib::layout_sidebar(
      
      # Sidebar
      sidebar = bslib::navset_pill(
        bslib::nav_panel(
          title = "Acknowledgements",
          value = "Acknowledgements",
          id = "Acknowledgements"
        ),
        id = ns("subtabs"),
        bslib::nav_panel(
          title = "License",
          value = "License",
          id = "License"
        ),
        bslib::nav_panel(
          title = "Privacy Policy",
          value = "Privacy Policy",
          id = "Privacy Policy"
        ),
        bslib::nav_panel(
          title = "Legal Notice",
          value = "Legal Notice",
          id = "Legal Notice"
        ),
      ),
      # Main content area
      div(
        shiny::uiOutput(ns("about_content"))
      )
    )
  )
}

# === Define server ===
aboutServer <- function(input, output, session) {
  ns <- session$ns
  
  output$about_content <- renderUI({
    switch(input$subtabs,
           "Acknowledgements" = div(
             h3("Acknowledgements"),
             p("This resource was developed from another tool (", url_FermentationExplorer, ").  A manuscript describing the current resource is in preparation."),
             p(shiny::tagList(
               "We acknowledge ", 
               url_BacDive, ", ", 
               url_FAPROTAX, ", ",
               url_GOLD, ", ",
               url_GTDB, ", ",
               url_IMG, ", ", 
               url_LPSN, ", and ", 
               url_NCBI, 
               " databases for use of their data. Data from ", 
               url_BacDive, ", ", 
               url_GTDB, ", and ", 
               url_LPSN, 
               " appear under the terms of a ", url_CC, ".  Data from ", 
               url_NCBI, 
               " appear under the terms of a ", url_MIT, ". Data from ",
               url_FAPROTAX, 
               "appear under the terms of ", 
               url_FAPROTAX_license, ". Data from ", 
               url_GOLD, " and ", url_IMG, 
               " appear under the terms of ", url_JGI, "."
             )),
             p(shiny::tagList(
               "We also acknowledge ", 
               url_AnaerobeManual, ", ", 
               url_Bergey, ", and authors of the primary literature for use of their data. Data from these sources appear under the doctrine of ", 
               url_fairuse, "."
             )),
             p("This work was supported by an Agriculture and Food Research Initiative Competitive Grant [grant no. 2018-67015-27495] and Hatch Project [accession no. 1019985] from the United States Department of Agriculture National Institute of Food and Agriculture."),
           ),
           
           "License" = div(
             h3("License"),
             p(shiny::tagList(
               "This work is licensed under a ",
               shiny::a(
                 "Creative Commons Attribution 4.0 International License",
                 href = "https://creativecommons.org/licenses/by/4.0/",
                 target = "_blank"
               ),
               "."
             )),
             shiny::a(
               href = "https://creativecommons.org/licenses/by/4.0/",
               target = "_blank",
               shiny::img(
                 src = "https://i.creativecommons.org/l/by/4.0/88x31.png",
                 alt = "Creative Commons Attribution 4.0 International License",
                 style = "border-width: 0;"
               )
             )
           ),
           
           "Privacy Policy" = div(
             h3("Privacy Policy"),
             tags$i("User registration"),
             p("Microbe Decoder does not require registration."),
             tags$i("Data collected"),
             p("Microbe Decoder collects data files for uploaded analysis and IP address."),
             tags$i("Purpose"),
             p("Data are used solely to perform analyses requested by the user and to maintain server functionality."),
             tags$i("Storage"),
             p("Uploaded data and results are stored temporarily and automatically deleted after 30 days."),
             tags$i("Sharing"),
             p("No user data are shared with third parties."),
             tags$i("Security"),
             p("We take reasonable measures to protect data, but users should not upload sensitive or confidential information."),
             tags$i("Contact"),
             p(shiny::tagList(
               "For questions or data removal requests, contact ",
               shiny::a("tjhackmann@ucdavis.edu", href = "mailto:tjhackmann@ucdavis.edu"), 
               ".")
             )
           ),
           
           "Legal Notice" = div(
             h3("Legal Notice"),
             p("Dr. Timothy J. Hackmann is responsible for the content of this website."),
             p(shiny::tagList(
               "Dr. Timothy J. Hackmann", shiny::br(),
               "Associate Professor", shiny::br(),
               "Department of Animal Science", shiny::br(),
               "University of California, Davis", shiny::br(),
               "One Shields Avenue", shiny::br(),
               "Davis, CA 95616, USA", shiny::br(),
               shiny::a("tjhackmann@ucdavis.edu", href = "mailto:tjhackmann@ucdavis.edu"), shiny::br(),
               "+1 530 754 1672"
             )),
             p("This website is for research purposes and comes with no warranty.")
           )
    )
  })
}