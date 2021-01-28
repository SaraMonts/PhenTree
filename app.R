
# Per a instal·lar l'última versió del paquet kinship2modified
#devtools::install_github("SaraMonts/kinship2modified")

# Es carreguen els paquets necessaris
library(kinship2modified)
library(shiny)
library(shinyjs)
library(colourpicker)
library(shinyalert)
library(RMySQL)
library(pool)
library(DT)
library(gtools)
library(fresh)
library(markdown)

# Es carreguen les taules que contenen els termes HPO (hpo), els gens i els identificadors de les
# malalties relacionades amb els termes HPO (ptg) i els noms de les malalties (ann)
hpo <- readRDS("www/hpo_list_subset.RData")
ptg <- readRDS("www/ptg_subset.RData")
ann <- readRDS("www/annotation_subset.RData")

# Es carrega la informació per a accedir a la base de dades
info_DB <- readRDS("www/info_DB.RData")

# Es defineix la informació de la base de dades i l'usuari que es vol utilitzar per a accedir-hi
host = info_DB[1]
port = as.numeric(info_DB[2])
initial_user = info_DB[3]
initial_password = info_DB[4]
dbname = info_DB[5]

# Es crea un pool per a accedir a la base de dades. Els pools s'utilitzen molt en Shiny per a facilitar
# la connexió i la desconnexió a una base de dades
initial_pool <- dbPool(
  RMySQL::MySQL(), 
  dbname = dbname, 
  host = host, 
  port = port, 
  username = initial_user, 
  password = initial_password)

# S'indica que el pool s'ha de tancar un cop es tanca l'aplicació
onStop(function() {
  poolClose(initial_pool)
})



## Es defineixen varies funcions que s'utilitzaran posteriorment dins del "server" ##

# Per a obtenir un llistat dels noms d'usuari
usersList <- function() {
  db <- initial_pool
  
  query <- sprintf("SELECT User FROM mysql.user")
  
  users_list <- dbGetQuery(db, query)
  users_list$User
}

# Per a crear un nou usuari
createUser <- function(username, password) {
  db <- initial_pool
  
  query1 <- sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s'",
                   username,
                   password)
  
  dbGetQuery(db, query1)
  
  query2 <- sprintf("GRANT SELECT, INSERT, DELETE, UPDATE ON PhenTree.* TO '%s'@'%%'",
                    username)
  
  dbGetQuery(db, query2)
}

# Per a eliminar un usuari
deleteUser <- function(username) {
  db <- initial_pool
  
  query <- sprintf("DROP USER '%s'@'%%'",
                    username)
  
  dbGetQuery(db, query)
}

# Per a obtenir un llistat dels IDs de tots els pedigrees de la base de dades o només els assignats a un usuari
pedIDs <- function(pool, username) {
  db <- pool
  
  if (username == 0) {
    query <- sprintf("SELECT ped_id FROM pedigrees")
  }
  else {
    query <- sprintf("SELECT ped_id FROM pedigrees WHERE username = '%s'",
                     username)
  }
  
  ped_ids <- dbGetQuery(db, query)
  ped_ids
}

# Per a afegir una nova fila a la taula "pedigrees" que conté tota la informació relacionada amb cada pedigree
addConfig <- function(pool, config) {
  db <- pool
  
  query <- sprintf(
    "INSERT INTO pedigrees (%s) VALUES ('%s')",
    paste(names(config), collapse = ", "),
    paste(config, collapse = "', '")
  )
  
  dbGetQuery(db, query)
}

# Per a obtenir una llista que contingui els noms i els IDs dels pedigrees d'un usuari
pedNames <- function(pool, username) {
  db <- pool
  
  query <- sprintf("SELECT ped_id, ped_name FROM pedigrees WHERE username = '%s' ORDER BY creation_date DESC",
                   username)
  
  tab <- dbGetQuery(db, query)
  
  if (nrow(tab) == 0) {
    choices <- c("-" = NA)
  }
  else {
    list <- tab$ped_id
    names(list) <- tab$ped_name
    choices <- list
  }
  
  return(choices)
}

# Per a eliminar un pedigree
deletePed <- function(pool, ped_id) {
  db <- pool
  
  # Primer s'han d'eliminar totes les files de la taula "members" amb aquest ID de pedigree
  query1 <- sprintf(
    "DELETE FROM members WHERE ped = '%s'",
    ped_id
  ) 
  
  dbGetQuery(db, query1)
  
  # Després ja es pot borrar el pedigree de la taula "pedigrees"
  query2 <- sprintf(
    "DELETE FROM pedigrees WHERE ped_id = '%s'",
    ped_id
  )
  
  dbGetQuery(db, query2)
}

# Per a actualitzar un valor de la taula "pedigrees"
updateConfig <- function(pool, ped_id, field, value) {
  db <- pool
  
  query <- sprintf(
    "UPDATE pedigrees SET %s = '%s' WHERE ped_id = '%s'",
    field,
    value,
    ped_id
  )
  
  dbGetQuery(db, query)
}

# Per a carregar la informació de la taula "pedigrees" per a un pedigree en concret
loadConfig <- function(pool, ped_id = 0) {
  db <- pool
  
  if (is.null(ped_id)) ped_id <- 0
  
  query <- sprintf("SELECT * FROM pedigrees WHERE ped_id = '%s'",
                   ped_id)
  
  data <- dbGetQuery(db, query)
  data
}

# Per a obtenir el nom d'un pedigree amb un ID concret
pedName <- function(pool, ped_id = 0) {
  db <- pool
  
  if (is.null(ped_id)) ped_id <- 0
  
  query <- sprintf("SELECT ped_name FROM pedigrees WHERE ped_id = '%s'",
                   ped_id)
  
  name <- dbGetQuery(db, query)
  name
}

# Per a guardar les dades introduïdes d'un membre d'una família a la taula "members"
addMember <- function(pool, member) {
  db <- pool
  
  query <- sprintf(
    "INSERT INTO members (%s) VALUES ('%s')",
    paste(names(member), collapse = ", "),
    paste(member, collapse = "', '")
  )
  
  dbGetQuery(db, query)
}

# Per a mostrar les dades contingudes a la taula "members" per a un pedigree concret
loadData <- function(pool, ped_id = 0) {
  db <- pool
  
  if (is.null(ped_id)) ped_id <- 0
  
  query <- sprintf("SELECT * FROM members WHERE ped = '%s' ORDER BY tab_order",
                   ped_id)
  
  data <- dbGetQuery(db, query)
  data
}

# Per a eliminar una fila de la taula "members"
deleteRow <- function(pool, id_num) {
  db <- pool
  
  query <- sprintf(
    "DELETE FROM members WHERE id = '%s'",
    id_num
    )
  
  dbGetQuery(db, query)
}

# Per a canviar l'ordre en el que es mostren els membres en la taula resum
changeOrder <- function(pool, data, id_num, direction) {
  
  # Es mira en quin número de fila es troba el ID d'interès (id1) i amb quin altre ID (id2) s'ha d'intercanviar la
  # posició segons la direcció "up" o "down"
  id1 <- id_num
  index_id1 <- which(data$id == id1)
  if (direction == "up") id2 <- data$id[index_id1 - 1]
  else if (direction == "down") id2 <- data$id[index_id1 + 1]
  
  # Si id2 és un id correcte
  if (length(id2) != 0) {
   if (!is.na(id2)) {
  
    # S'intercanvien les posicions entre els IDs 
    new_order_id1 <- data$tab_order[data$id == id2]
    new_order_id2 <- data$tab_order[data$id == id1]
  
    # Es canvia la variable tab_order de la base de dades
    db <- pool
  
    query1 <- sprintf(
      "UPDATE members SET tab_order = '%s' WHERE id = '%s'",
      new_order_id1,
      id1
    )
  
    dbGetQuery(db, query1)
  
    query2 <- sprintf(
      "UPDATE members SET tab_order = '%s' WHERE id = '%s'",
      new_order_id2,
      id2
    )
  
    dbGetQuery(db, query2)
   }
  }
}

# Per a seleccionar el "id" i el "name" dels homes ("male"), les dones ("female") o tots dos ("all")
selectSex <- function(data, sex) {
  
  # Es selecciona el subset necessari del data frame
  if (sex == "all") dt <- data
  else if (sex == "male") dt <- subset(data, data$sex == 1)
  else if (sex == "female") dt <- subset(data, data$sex == 2)
  
  # Si el data frame està buit només es retorna l'opció "-"
  if (nrow(dt) == 0) {
    choices <- c("-" = NA)
  } 
  # Si el data frame conté informació es retorna l'opció "-" seguida dels "name" i "id" pertinents
  else {
    ids <- substr(dt$id, nchar(dt$id) - 2, nchar(dt$id))
    list <- dt$id
    names(list) <- paste0(dt$name, " (", ids, ")")
    choices <- c("-" = NA, list)
  }
  
  return(choices)
}

# Per a crear un link a partir d'un identificador OMIM o Orphanet
createLink <- function(disID) {
  
  if (substring(disID, 1, 5) == "ORPHA") {
    link <- sprintf('<a href="https://www.orpha.net/consor/cgi-bin/OC_Exp.php?lng=en&Expert=%s">%s</a>', 
                    substring(disID, 7),
                    disID)
  }
  else if (substring(disID, 1, 4) == "OMIM") {
    link <- sprintf('<a href="https://www.omim.org/entry/%s">%s</a>', 
                    substring(disID, 6),
                    disID)
  }
  
  return(link)
}

# Per a crear la taula que relaciona cada fenotip amb un conjunt de gens i malalties
createGenesTable <- function(phenotype, ptg, ann) {
  
  # Es seleccionen de la taula "ptg", carregada a l'inici del script, aquelles files que 
  # contenen l'identificador HPO d'interès
  tab <- subset(ptg, ptg$hpoID == phenotype)
  
  # Si no hi ha cap fila que contingui l'identificador, es genera una taula buida 
  if (nrow(tab) == 0) {
    tab <- data.frame(Gene = c(NA), Disease_ID = c(NA), Disease_Name = c(NA))
  }
  # Si sí que n'hi ha alguna, es genera una taula amb els noms dels gens, els identificadors
  # de les malalties (que contenen enllaços a OMIM o Orphanet) i els noms de les malalties,
  # obtinguts de la taula "ann" carregada a l'inici del script.
  else {
    disName <- c()
    disLink <- c()
    for (i in 1:nrow(tab)) {
      disID <- as.character(tab$DiseaseID[i])
      if (disID %in% as.character(ann$DB_ID)) {
        disName[i] <- as.character(ann$DB_Name[ann$DB_ID == disID])
      }
      else disName[i] <- NA
      
      disLink[i] <- createLink(disID)
    }
    tab <- cbind(tab, disLink, disName)
    tab <- tab[, c(2, 4, 5)]
  }
  
  # Es defineixen els noms de les columnes i es retrona la taula
  colnames(tab) <- c("Gene", "Disease ID", "Disease Name")
  rownames(tab) <- NULL
  return(tab)
}


# Es defineix una funció en JavaScript que permet fer scroll automàtic fins a la part superior de la
# UI al clicar algun botó
jsCode <- "shinyjs.toTop = function(){scroll(0,0);}"






### Es defineix la interfície d'usuari ###

ui <- tagList(
  
       # S'indica que es volen utilitzar els paquets shinyjs i shinyalert i al funció en JavaScript definida anteriorment
       useShinyjs(),
       useShinyalert(),
       extendShinyjs(text = jsCode, functions = c("toTop")),
       
       # Es defineixen diferents pestanyes a la part superior de la pàgina 
       navbarPage(id = "superior_menu", title = span("PhenTree", style = "font-family: 'Damion'; color: #5c9d12; font-size: 35px"),
                  windowTitle = "PhenTree - Pedigree Creator",
         
         # PRIMERA PESTANYA SUPERIOR: User account
         tabPanel(tags$b("User account"), 
            
            # S'afegeix el títol de l'aplicació al centre de la UI
            fluidRow(style = "text-align: center; font-family: 'Damion'; font-size: 80px; color: #5c9d12; height: 130px",
              use_googlefont("Damion"),
              tags$p("PhenTree")
            ),
            
            # S'afegeixen els inputs per a crear un usuari o entrar amb un usuari ja existent
            fluidRow(
              column(6, style='border-right: 1px solid black',
                fluidRow(
                  column(3),
                  
                  # Registre d'un nou usuari
                  column(6,
                    fluidRow(style = "text-align: center; height: 120px",
                      tags$h3(HTML(paste(paste("First time in ", tags$span("PhenTree", style = "font-family: 'Damion'"), "?", sep = ""),
                                   paste("Register here"), sep = "<br/>")))
                    ),
                    fluidRow(style = "word-spacing: 10px; height: 30px",
                      icon("user"),
                      tags$b("Username")
                    ),
                    fluidRow(style = "height: 60px",
                      textInput("username_register", label = NULL, width = "100%", placeholder = "Enter user name")
                    ),
                    fluidRow(style = "word-spacing: 10px; height: 30px",
                             icon("unlock-alt"),
                             tags$b("Password")
                    ),
                    fluidRow(style = "height: 60px",
                      passwordInput("password_register", label = NULL, width = "100%", placeholder = "Enter password")
                    ),
                    fluidRow(style = "height: 30px",
                             icon("unlock-alt"),
                             tags$b(HTML('&nbsp;'), "Confirm password")
                    ),
                    fluidRow(style = "height: 50px",
                             passwordInput("password_confirm", label = NULL, width = "100%", placeholder = "Enter password again")
                    ),
                    fluidRow(style = "height: 100px",
                      helpText("Both username and password should have between 6 and 32 character and can only contain lowercase or upercase letters or numbers.")
                    ),
                    fluidRow(style = "text-align: center",
                      actionButton("sign_up", "Sign up", class = "btn-primary")
                    ),
                    fluidRow(style = "height: 30px")
                  ),
                  column(3)
                )
              ),
              
              # Log in amb un usuari existent
              column(6,
                fluidRow(
                  column(3),
                  column(6,
                    fluidRow(style = "text-align: center; height: 120px",
                      tags$h3(HTML(paste("Already have a user account?", "Log in", sep = "<br/>")))
                    ),
                    fluidRow(style = "word-spacing: 10px; height: 30px",
                      icon("user"),
                      tags$b("Username")
                    ),
                    fluidRow(style = "height: 60px",
                      textInput("username_login", label = NULL, width = "100%", placeholder = "Enter user name")
                    ),
                    fluidRow(style = "word-spacing: 10px; height: 30px",
                      icon("unlock-alt"),
                      tags$b("Password")
                    ),
                    fluidRow(style = "height: 70px",
                      passwordInput("password_login", label = NULL, width = "100%", placeholder = "Enter password")
                    ),
                    fluidRow(style = "text-align: center; height: 100px",
                      actionButton("log_in", "Log in", class = "btn-primary")
                    ),
                    
                    # Informació de l'usuari actual i botons "log out" i "delete user"
                    hidden(
                      div(id = "logged_user_show",
                          fluidRow(style = "height: 120px; background-color: #f7f7f7", 
                            fluidRow(style = "height: 70px",
                              column(6, style = "font-size: 16px; padding-top: 20px; text-align: right",
                                     tags$p("Logged in with user:")
                              ),
                              column(6, style = "font-size: 16px; padding-top: 20px; text-align: left",
                                     tags$b(textOutput("logged_user"))
                              )
                            ),
                            fluidRow(
                              column(6, style = "text-align: right",
                                     actionButton("delete_user", "Delete user", width = "80%")
                              ),
                              column(6, style = "text-align: left",
                                     actionButton("log_out", "Log out", class = "btn-warning", width = "80%")
                              )
                              
                            )
                          )
                      )
                    )
                  ),
                  column(3)
                )
              )
            )
        ),
        
        
        # SEGONA PESTANYA SUPERIOR: Pedigree creator
        tabPanel(tags$b("Pedigree creator"), value = "ped_creator_tab",
            
            # Es defineix l'estructura de la pàgina amb un panell a l'esquerra per a introduir la informació (inputs panel)
            # i un espai a la dreta per mostrar els resultats (outputs panel)
            sidebarLayout(
                sidebarPanel(
                    
                    # Dins del panell de l'esquerra es defineixen varies pestanyes
                    tabsetPanel(id = "inputs_panel",
                        
                        # PRIMERA PESTANYA: Choose a pedigree
                        tabPanel(HTML(paste("Choose a", "pedigree", sep = "<br/>")),
                           tags$br(),
                           tags$p("Choose the pedigree you want to edit or create a new one:"),
                           
                           # Es mostren els pedigrees de l'usuari (si és que n'hi ha) i els diferents botons
                           hidden(
                             tags$div(id = "no_pedigrees",
                                tags$br(),
                                helpText("No pedigrees yet"),
                                tags$br(),
                                actionButton("new_pedigree1", "Create a new pedigree", class = "btn-primary",
                                             icon = icon("plus"))),
                             tags$div(id = "pedigree_choose",
                                tags$br(),
                                uiOutput("ped_choose"),
                                tags$br(),
                                actionButton("working_ped", "Work with the selected pedigree", 
                                             class = "btn-primary", width = "100%", icon = icon("edit")),
                                tags$table(style = "width: 100%",
                                   tags$tr(
                                     tags$td(style = "width: 47.5%; height: 10px", " ")
                                   ),
                                   tags$tr(
                                     tags$td(style = "width: 47.5%",
                                             actionButton("new_pedigree2", "Create a new pedigree", width = "100%",
                                                          icon = icon("plus"))),
                                     tags$td(style = "width: 5%"),
                                     tags$td(style = "width: 47.5%",
                                             actionButton("delete_ped", "Delete selected pedigree", width = "100%",
                                                          icon = icon("trash-alt")))
                             )))
                           ),
                           tags$br(),
                           tags$br(),
                           
                           # S'indica amb quin pedigree s'està treballant
                           tags$b(textOutput("work_ped_now"))
                        ),
                        
                        
                        # SEGONA PESTANYA: Define the phenotypes
                        tabPanel(HTML(paste("Define the", "phenotypes", sep = "<br/>")), value = "define_phenotypes",
                            tags$br(),
                            helpText("You can select up to four phenotypes to represent in the pedigree.
                                     The terms and IDs shown are the standards defined by the Human Phenotype Ontology."),
                            tags$br(),
                            
                            # Petit input per a seleccionar el nombre de fenotips a mostrar (de 1 a 4)
                            tags$table(style = "width: 40%",
                                tags$tr(
                                    tags$td(style = "width: 33%",
                                            helpText("Show:")),
                                    tags$td(style = "width: 35%",
                                            numericInput("show_num1", label = NULL, value = 1, min = 1, max = 4)),
                                    tags$td(style = "width: 32%; text-align: right",
                                            helpText("4 max."))
                                )
                            ),
                            
                            # Inputs per a seleccionar els fenotips
                            tags$h4("Phenotype 1:"),
                            selectInput("phenotype1", label = NULL, choices = hpo),
                            hidden(
                              tags$div(id = "phenotype2_show",
                                  tags$h4("Phenotype 2:"),
                                  selectInput("phenotype2", label = NULL, choices = hpo)),
                              tags$div(id = "phenotype3_show",
                                  tags$h4("Phenotype 3:"),
                                  selectInput("phenotype3", label = NULL, choices = hpo)),
                              tags$div(id = "phenotype4_show",
                                  tags$h4("Phenotype 4:"),
                                  selectInput("phenotype4", label = NULL, choices = hpo))
                            ),
                            tags$br(),
                            tags$br(),
                            
                            # Comentari
                            helpText("The Human Phenotype Ontology contains over 15,000 terms of human phenotypes. 
                                     To prevent the app from being too slow, only 5,000 of the terms were included 
                                     in the app as a proof of concept. The included terms were randomly selected.")
                        ),
                                
                                
                        # TERCERA PESTANYA: Add family members
                        tabPanel(HTML(paste("Add family", "members", sep = "<br/>")), value = "add_family_members",
                            tags$br(),
                            helpText("Enter the information of the family members one at a time. It is recommended
                                     that you introduce them from oldest to youngest."),
                            tags$br(),
                            
                            # Si s'està editant un membre ja introduït, es mostra un missatge a la part superior
                            # que ho indica
                            hidden(
                              tags$div(id = "editing_show",
                                  tags$h4("EDITING FAMILY MEMBER", align = "center", style = "color: #00cc00"))
                            ),
                            
                            # Nom de la persona
                            textInput("name", label = "Name", 
                                      placeholder = "Enter the name of the family member"),
                            tags$br(),
                            
                            ## Informació per a definir el símbol:
                            tags$h3("Define the symbol"),
                            tags$table(style = "width: 100%",
                                tags$tr(
                                  
                                  # Gènere
                                  tags$td(style = "width: 45%; vertical-align: top",
                                          radioButtons("sex", label = "Gender", 
                                                       choices = c("Female" = 2,
                                                                   "Male" = 1,
                                                                   "Unknown/Non-binary/Disorders of sex development" = 3))),
                                  tags$td(style = "width: 5%"),
                                  
                                  # Status
                                  tags$td(style = "width: 50%",
                                          radioButtons("status", label = "Status",
                                                       choices = c("Alive" = 0,
                                                                   "Deceased" = 1,
                                                                   "Pregnancy" = 2,
                                                                   "Spontaneous abortion (miscarriage)" = 3,
                                                                   "Induced abortion (termination of pregnancy)" = 4)))
                                )),
                            tags$br(),
                            
                            ## Informació per a definir la posició en el pedigree
                            tags$h3("Define the position in the pedigree"),
                            helpText("In the following selectors you can choose one of the individuals from the 
                                     summary table."),
                            tags$table(style = "width: 100%",
                                tags$tr(
                                  
                                  # Mare (input canviant)
                                  tags$td(style = "width: 47.5%",
                                          uiOutput("mother_select")),
                                  tags$td(style = "width: 5%"),
                                  
                                  # Pare (input canviant)
                                  tags$td(style = "width: 47.5%",
                                          uiOutput("father_select"))
                                )),
                            
                            # Adopcions: es mostren amagades d'entrada
                            tags$b("Is he/she adopted? "),
                            tags$a(id = "show_hide1", "Show/hide"),
                            hidden(
                              tags$div(id = "adopted_show",
                                tags$table(style = "width: 100%",
                                    tags$tr(
                                      tags$td(style = "width: 100%; height: 10px", " ")),
                                    tags$tr(
                                      tags$td(style = "width: 100%",
                                              radioButtons("adopted", label = NULL, choices = c("Not adopted" = NA, 
                                                           "Adopted in" = "in", "Adopted out" = "out"), 
                                                           selected = NULL, inline = TRUE)))
                            ))),
                            tags$br(),
                            
                            # Altres relacions (inputs canviants): es mostren amagades d'entrada
                            tags$b("Add other relationships: "),
                            tags$a(id = "show_hide2", "Show/hide"),
                            hidden(
                             tags$div(id = "other_rel_show",
                              tags$table(style = "width: 100%",
                                         
                                # Bessons monozigòtics
                                tags$tr(
                                    tags$td(style = "width: 55%",
                                            tags$p("Monozygotic (identical) twins")),
                                    tags$td(style = "width: 45%",
                                            uiOutput("motwins_select"))),
                                
                                # Bessons dizigòtics
                                tags$tr(
                                    tags$td(style = "width: 55%",
                                            tags$p("Dizygotic (fraternal) twins")),
                                    tags$td(style = "width: 45%",
                                            uiOutput("ditwins_select"))),
                                
                                # Bessons no definits
                                tags$tr(
                                    tags$td(style = "width: 55%",
                                            tags$p("Twins of unknown zygosity")),
                                    tags$td(style = "width: 45%",
                                            uiOutput("untwins_select"))),
                                
                                # Matrimoni/parella sense fills
                                tags$tr(
                                    tags$td(style = "width: 55%",
                                            tags$p("Marriage/couple without kids")),
                                    tags$td(style = "width: 45%",
                                            uiOutput("marriage_select")))
                            ))),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            
                            ## Informació sobre els fenotips a destacar en el pedigree. Se'n mostren tants com 
                            ## s'hagi indicat en la pestanya "Define the phenotypes"
                            tags$h3("Add phenotypes"),
                            helpText('Note: To represent the carrier status for one phenotype, all the other phenotypes
                                     must be "Not affected". Else, you will get an error.'),
                            
                            # Fenotip 1
                            selectInput("phen1", label = NULL,
                                        choices = c("Not affected" = 0,
                                                    "Affected" = 1,
                                                    "Carrier (not likely to manifest phenotype)" = 2,
                                                    "Presymptomatic carrier (could manifest phenotype)" = 3)),
                            hidden(
                              
                              # Fenotip 2
                              div(id = "phen2_show",
                                  selectInput("phen2", label = NULL,
                                              choices = c("Not affected" = 0,
                                                          "Affected" = 1,
                                                          "Carrier (not likely to manifest phenotype)" = 2,
                                                          "Presymptomatic carrier (could manifest phenotype)" = 3))),
                              
                              # Fenotip 3
                              div(id = "phen3_show",
                                  selectInput("phen3", label = NULL,
                                              choices = c("Not affected" = 0,
                                                          "Affected" = 1,
                                                          "Carrier (not likely to manifest phenotype)" = 2,
                                                          "Presymptomatic carrier (could manifest phenotype)" = 3))),
                              
                              # Fenotip 4
                              div(id = "phen4_show",
                                  selectInput("phen4", label = NULL,
                                              choices = c("Not affected" = 0,
                                                          "Affected" = 1,
                                                          "Carrier (not likely to manifest phenotype)" = 2,
                                                          "Presymptomatic carrier (could manifest phenotype)" = 3)))
                            ),
                            tags$br(),
                            
                            ## Més informació extra sobre la persona
                            tags$h3("Add more information"),
                            
                            # Edat
                            textInput("age", label = "Age",
                                      placeholder = "Enter the age of the family member"),
                            helpText("Note: You can enter the age, year of birth or age at death 
                                     in any format you prefer (Examples: 39 y, 4 mo, b. 1983, d. 78 y...)"),
                            
                            # Si és la persona que consulta
                            tags$b("Is he/she the consultand?"),
                            checkboxInput("consultand", label = 'Check if "Yes"'),
                            
                            # Nombre de persones
                            selectInput("number", label = "If you want to represent more than one person with the same symbol, choose how many:",
                                        choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, "Unknown number (n)" = "n")),
                            
                            # Altre informació: s'ha d'indicar el nombre de camps que es volen mostrar (max 4)
                            tags$b("Other information"),
                            helpText("You can add all the information you find relevant. For example: age of onset 
                                     of a disease, karyotype, gene mutation, cause of death, year of diagnosis of a disease..."),
                            tags$table(style = "width: 40%",
                                tags$tr(
                                    tags$td(style = "width: 33%",
                                            helpText("Show:")),
                                    tags$td(style = "width: 35%",
                                            numericInput("show_num2", label = NULL, value = 1, min = 1, max = 4)),
                                    tags$td(style = "width: 32%; text-align: right",
                                            helpText("4 max."))
                                )
                            ),
                            textInput("info1", label = NULL,
                                      placeholder = "Enter information"),
                            hidden(
                              div(id = "info2_space",
                                textInput("info2", label = NULL,
                                          placeholder = "Enter information")),
                              div(id = "info3_space",
                                textInput("info3", label = NULL,
                                          placeholder = "Enter information")),
                              div(id = "info4_space",
                                textInput("info4", label = NULL,
                                          placeholder = "Enter information"))),
                            tags$br(),
                            tags$br(),
                            
                            # S'afegeix el botó "Submit"
                            tags$div(id = "submit_show",
                                actionButton("submit", label = "Submit", width = "100%", class = "btn-primary")),
                            
                            # Si s'està editant un membre ja introduit, en comptes del botó "Submit" es mostren
                            # els botons "Save changes" i "Cancel"
                            hidden(
                              tags$div(id = "changes_show",
                                tags$table(style = "width: 100%",
                                    tags$tr(
                                      tags$td(style = "width: 47.5%",
                                              actionButton("save_changes", "Save changes", 
                                                           width = "100%", class = "btn-success")),
                                      tags$td(style = "width: 5%"),
                                      tags$td(style = "width: 47.5%",
                                              actionButton("cancel", "Cancel", width = "100%")))))
                            )
                        ),
                        
                        
                        # QUARTA PESTANYA: Customize pedigree
                        tabPanel(HTML(paste("Customize", "pedigree", sep = "<br/>")), value = "customize_pedigree",
                            tags$br(),
                            helpText("You can customize the plot using the following options."),
                            tags$br(),
                            
                            # S'indica quines són les característiques que es volen mostrar en el gràfic i quines no
                            tags$b("Mark the features to include in the pedigree:"),
                            tags$table(style = "width: 100%",
                                tags$tr(
                                  tags$td(style = "width: 50%; height: 10px", " ")
                                ),
                                tags$tr(
                                    tags$td(style = "width: 50%; vertical-align : top",
                                            checkboxGroupInput("include1", label = NULL, 
                                                               choices = c("Name" = "name", "Phenotype 1" = "phen1"),
                                                               selected = c("name", "phen1"))),
                                    tags$td(style = "width: 50%; vertical-align: top",
                                            checkboxGroupInput("include2", label = NULL, 
                                                               choices = c("Age" = "age", "Consultand" = "consultand", 
                                                                           "Information 1" = "info1"),
                                                               selected = c("age", "consultand", "info1"))))
                            ),
                            tags$br(),
                            
                            # Inputs en forma de slider per a canviar l'amplada, l'altura, la mida del símbol,
                            # la mida del text i la distància entre el símbol i el text, entre d'altres
                            sliderInput("width", "Plot width", min = 5, max = 15, value = 10, step = 0.5),
                            sliderInput("height", "Plot height", min = 1, max = 9, value = 5, step = 0.5),
                            sliderInput("symbol_size", "Symbol size", min = 0.5, max = 4, value = 1.5,
                                        step = 0.1),
                            sliderInput("text_size", "Text size", min = 0.5, max = 3, value = 1, step = 0.1),
                            sliderInput("text_dist", "Distance between symbol and text", min = 0.1, max = 7,
                                        value = 2, step = 0.1),
                            tags$b("More options: "),
                            tags$a(id = "show_hide3", "Show/hide"),
                            hidden(
                              tags$div(id = "more_options_show",
                                sliderInput("pconnect", "Connection point between parents and children", min = 0,
                                            max = 3, value = 0.5, step = 0.1),
                                sliderInput("branch", "Slope of the connection line between parents and children",
                                            min = 0, max = 1, value = 0.6, step = 0.1)
                            )),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            
                            # Inputs per a seleccionar el color, la densitat i l'angle dels fenotips d'interès
                            tags$div(id = "color1_show",
                                tags$h4("Phenotype 1"),
                                colourInput("color1", "Color", value = "#73e1ff", 
                                            showColour = "background"),
                                tags$table(style = "width: 100%",
                                    tags$tr(
                                      tags$td(style = "width: 47.5%",
                                              selectInput("density1", label = "Density", 
                                                          choices = c("Solid" = 1,
                                                                      "Striped (density 10)" = 10,
                                                                      "Striped (density 20)" = 20,
                                                                      "Striped (density 30)" = 30,
                                                                      "Striped (density 40)" = 40,
                                                                      "Striped (density 50)" = 50))),
                                      tags$td(style = "width: 5%"),
                                      tags$td(style = "width: 47.5%",
                                              selectInput("angle1", label = "Stripes' angle", 
                                                          choices = c("0º" = 0, "25º" = 25, "45º" = 45,
                                                                      "65º" = 65, "90º" = 90, "115º" = 115,
                                                                      "135º" = 135, "155º" = 155),
                                                          selected = 45))
                                ))),
                            hidden(
                              tags$div(id = "color2_show",
                                  tags$br(),
                                  tags$h4("Phenotype 2"),
                                  colourInput("color2", "Color", value = "#82ff73", 
                                              showColour = "background"),
                                  tags$table(style = "width: 100%",
                                      tags$tr(
                                        tags$td(style = "width: 47.5%",
                                                selectInput("density2", label = "Density", 
                                                            choices = c("Solid" = 1,
                                                                        "Striped (density 10)" = 10,
                                                                        "Striped (density 20)" = 20,
                                                                        "Striped (density 30)" = 30,
                                                                        "Striped (density 40)" = 40,
                                                                        "Striped (density 50)" = 50))),
                                        tags$td(style = "width: 5%"),
                                        tags$td(style = "width: 47.5%",
                                                selectInput("angle2", label = "Stripes' angle", 
                                                            choices = c("0º" = 0, "25º" = 25, "45º" = 45,
                                                                        "65º" = 65, "90º" = 90, "115º" = 115,
                                                                        "135º" = 135, "155º" = 155),
                                                            selected = 45))
                                  ))),
                              tags$div(id = "color3_show",
                                  tags$br(),
                                  tags$h4("Phenotype 3"),
                                  colourInput("color3", "Color", value = "#ffc173", 
                                              showColour = "background"),
                                  tags$table(style = "width: 100%",
                                      tags$tr(
                                        tags$td(style = "width: 47.5%",
                                                selectInput("density3", label = "Density", 
                                                            choices = c("Solid" = 1,
                                                                        "Striped (density 10)" = 10,
                                                                        "Striped (density 20)" = 20,
                                                                        "Striped (density 30)" = 30,
                                                                        "Striped (density 40)" = 40,
                                                                        "Striped (density 50)" = 50))),
                                        tags$td(style = "width: 5%"),
                                        tags$td(style = "width: 47.5%",
                                                selectInput("angle3", label = "Stripes' angle", 
                                                            choices = c("0º" = 0, "25º" = 25, "45º" = 45,
                                                                        "65º" = 65, "90º" = 90, "115º" = 115,
                                                                        "135º" = 135, "155º" = 155),
                                                            selected = 45))
                                  ))),
                              tags$div(id = "color4_show",
                                  tags$br(),
                                  tags$h4("Phenotype 4"),
                                  colourInput("color4", "Color", value = "#bf73ff", 
                                              showColour = "background"),
                                  tags$table(style = "width: 100%",
                                      tags$tr(
                                        tags$td(style = "width: 47.5%",
                                                selectInput("density4", label = "Density", 
                                                            choices = c("Solid" = 1,
                                                                        "Striped (density 10)" = 10,
                                                                        "Striped (density 20)" = 20,
                                                                        "Striped (density 30)" = 30,
                                                                        "Striped (density 40)" = 40,
                                                                        "Striped (density 50)" = 50))),
                                        tags$td(style = "width: 5%"),
                                        tags$td(style = "width: 47.5%",
                                                selectInput("angle4", label = "Stripes' angle", 
                                                            choices = c("0º" = 0, "25º" = 25, "45º" = 45,
                                                                        "65º" = 65, "90º" = 90, "115º" = 115,
                                                                        "135º" = 135, "155º" = 155),
                                                            selected = 45))
                                  )))
                            ),
                            tags$br(),
                            tags$br(),
                            
                            # Inputs per a modificar la llegenda
                            tags$h3("Legend"),
                            checkboxInput("show_legend", "Show legend", value = TRUE),
                            sliderInput("legend_width", "Legend width", min = 5, max = 15, value = 10, step = 0.5),
                            sliderInput("legend_height", "Legend height", min = 1, max = 9, value = 4, step = 0.5),
                            sliderInput("legend_symbolsize", "Legend symbol size", min = 0.5, max = 4, value = 1.3,
                                        step = 0.1),
                            sliderInput("legend_textsize", "Legend text size", min = 0.5, max = 3, value = 1, step = 0.1)
                        )
                    )
                ),
                
                
                # L'espai de la dreta conté tres pestanyes
                hidden(
                 div(id = "output_panel_show",
                  mainPanel(
                    tabsetPanel(id = "outputs_panel",
                      
                        # PRIMERA PESTANYA: Summary table
                        # Conté la taula que resumeix la informació dels membres de la família
                        # i els botons que es poden utilitzar per a modificar-la
                        tabPanel("Summary table", value = "summary_table",
                            tags$br(),
                            tags$table(style = "width: 100%",
                                tags$tr(helpText("The following table summarizes the information of the introduced family members.")),
                                tags$tr(helpText("To edit the table, select one row and use the following buttons.")),
                                tags$tr(
                                  tags$td(style = "width: 14%",
                                          actionButton("edit_row", "Edit member", icon = icon("edit"))),
                                  tags$td(style = "width: 15.5%",
                                          actionButton("delete_row", "Delete member", icon = icon("trash-alt"))),
                                  tags$td(style = "width: 15.8%",
                                          actionButton("up", "Reorder row up", icon = icon("arrow-up"))),
                                  tags$td(style = "width: 20%",
                                          actionButton("down", "Reorder row down", icon = icon("arrow-down"))),
                                  tags$td(style = "width: 34.7%")
                            )),
                            tags$br(),
                            dataTableOutput("sum_table"),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            
                            # S'afegeix informació sobre les sigles que aparèixen a la taula
                            includeMarkdown("www/table_abbreviations.md")
                            
                        ),
                        
                        # SEGONA PESTANYA: Pedigree
                        tabPanel("Pedigree",
                            tags$br(),
                            helpText("The pedigree is automatically generated and updated with the family members
                                     information added in the summary table."),
                            tags$br(),
                            
                            # Pedigree
                            plotOutput("pedigree", width = "100%", height = "600px"),
                            
                            # LLegenda
                            tags$div(id = "legend_show",
                                plotOutput("legend", width = "100%", height = "400px"),
                            ),
                            tags$br(),
                            
                            # Opcions de descàrrega
                            tags$h3("Download options:"),
                            fluidRow(
                              column(6,
                                     fluidRow(style = "height: 170px",
                                       column(6,
                                              radioButtons("os", "Operating system", choices = c("Windows" = "windows",
                                                                                                 "Others" = "others"))),
                                       column(6,
                                              radioButtons("image_format", "Image file format", choices = c("PNG" = "png",
                                                                                                            "JPEG" = "jpeg",
                                                                                                            "TIFF" = "tiff",
                                                                                                            "PDF" = "pdf")))
                                     ),
                                     fluidRow(style = "width: 95%; padding-left: 16px",
                                       helpText("Note: if you increase resolution, you should also increase image 
                                                width and height. Otherwise, the plot is going to be cut.")
                                     ),
                                     fluidRow(style = "width: 95%; padding-left: 16px",
                                       helpText("* In the case of PDF, consider the units as inches per 100. For
                                                example, a value of 4000 equals 40 inches.")
                                     )
                              ),
                              column(6,
                                     sliderInput("resolution", "Image resolution in ppi", min = 100, max = 1000,
                                                 value = 300, step = 50, width = "100%"),
                                     sliderInput("width_px", "Image width in pixels *", min = 500, max = 10000, 
                                                 value = 4000, step = 100, width = "100%"),
                                     sliderInput("height_px", "Image height in pixels *", min = 500, max = 10000, 
                                                 value = 3000, step = 100, width = "100%")
                              )
                            ),
                            
                            # Botons de descàrrega
                            fluidRow(
                              column(3, 
                                     downloadButton("download_pedigree", "Download pedigree", class = "btn-primary")
                              ),
                              column(3, 
                                     downloadButton("download_legend", "Download legend", class = "btn-primary")
                              ),
                              column(6,
                                     downloadButton("download_both", "Download pedigree + legend", class = "btn-primary")
                              )
                            ),
                            tags$br(),
                            tags$br(),
                            tags$br()
                        ),
                        
                        # TERCERA PESTANYA: Genes and diseases
                        # Mostra una taula per cada fenotip d'interès amb els gens i les 
                        # malalties amb les que ha estat relacionat segons HPO
                        tabPanel("Genes and diseases",
                            tags$style(HTML(".dataTables_wrapper .dt-buttons { float: right; text-align: right; }")),
                            tags$br(),
                            helpText("The following tables show the genes and the respective diseases than have been
                                     associated to the phenotypes of interest, according to the database of the Human
                                     Phenotype Ontology. The tables contain links to the OMIM and Orphanet webpages."),
                            helpText('The tables can be downloaded as CSV or Excel files. Make sure you select "Show
                                     All entries" before downloading, otherwise the downloaded table will only contain
                                     the rows that are currently shown.'),
                            tags$br(),
                            tags$h3(textOutput("phenotype1_title")),
                            dataTableOutput("phenotype1_genes"),
                            hidden(
                              tags$div(id = "phenotype2_genes_show",
                                  tags$br(),
                                  tags$h3(textOutput("phenotype2_title")),
                                  dataTableOutput("phenotype2_genes")),
                              tags$div(id = "phenotype3_genes_show",
                                  tags$br(),
                                  tags$h3(textOutput("phenotype3_title")),
                                  dataTableOutput("phenotype3_genes")),
                              tags$div(id = "phenotype4_genes_show",
                                  tags$br(),
                                  tags$h3(textOutput("phenotype4_title")),
                                  dataTableOutput("phenotype4_genes"))
                            )
                            
                        )
                    )
                )
               )
              )
            )
        ),
        
        
        
        # TERCERA PESTANYA SUPERIOR: App information
        # S'afegeix informació sobre la app a partir de markdowns guardats dins la carpeta www
        tabPanel(tags$b("App information"),
          fluidRow(
            column(1),
            column(10,
              tags$head(
                tags$style(HTML("img.info_images { border: 1px solid #555; } 
                                 .symbolstable table, .symbolstable th { border: 1px solid #555; border-collapse: collapse; }
                                 .symbolstable th { text-align: center; height: 40px; background-color: #f2f2f2; }
                                 .symbolstable td { border-left: 1px solid #555; border-bottom: 1px solid #555; height: 100px; padding-left: 15px; padding-right: 15px; }
                                 .refstable td { vertical-align: top; padding-right: 10px; padding-left: 10px; height: 60px; }"))
              ),
              navlistPanel(widths = c(3, 9),
                tabPanel("General Information", 
                         includeMarkdown("www/general_information.md")
                ),
                tabPanel("Pedigree Symbols",
                         HTML('<div class="symbolstable" markdown="1">'),
                         includeMarkdown("www/pedigree_symbols.md"),
                         HTML('</div>')
                ),
                tabPanel("Frequent Asked Questions (FAQs)",
                         includeMarkdown("www/frequent_asked_questions.md")
                ),
                tabPanel("References",
                         HTML('<div class="refstable" markdown="1">'),
                         includeMarkdown("www/references.md"),
                         HTML('</div>')
                )
              )
            ),
            column(1)
          )
        )
    )
)






### Es defineix la funció "server" que conté el codi que fa que l'aplicació funcioni ###

server <- function(input, output, session) {
            
            # S'indica que l'aplicació es pari al tancar la pestanya del navegador
            session$onSessionEnded(stopApp)
  
            # S'indica que s'amaguin determinades pestanyes a l'inici de l'aplicació
            hideTab("superior_menu", "ped_creator_tab")
            hideTab("inputs_panel", "define_phenotypes")
            hideTab("inputs_panel", "add_family_members")
            hideTab("inputs_panel", "customize_pedigree")
            
            # Es defineix la variable reactiva "values" i es guarda un primer valor dins d'aquesta
            # que conté el nom de l'usuari que s'està utilitzant (l'usuari "initial" a l'iniciar l'aplicació)
            values <- reactiveValues()
            values$user <- initial_user
            
            
            # S'indica què ha de passar al clicar el botó "Sign up"
            observeEvent(input$sign_up, {
              
              # Si l'usuari utilitzat és "initial"
              if (values$user == "initial") {
                
                # S'obté un llistat dels noms d'usuari que ja estan presents a la base de dades
                users_list <- usersList()
                
                # Si el nom d'usuari introduït ja està agafat, es mostra un missatge d'avís
                if (input$username_register %in% users_list) {
                  shinyalert(title = "Unavailable username", text = "This username is already taken. Choose another one.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), initial_user, ":: trying to create user: unavailable username", "\n")
                }
                
                # Si el nom d'usuari introduït té menys de 6 o més de 32 caràcters, es mostra un missatge d'avís
                else if (nchar(input$username_register) < 6 | nchar(input$username_register) > 32) {
                  shinyalert(title = "Wrong username length", text = "Username should have a length between 6 and 32 characters.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), initial_user, ":: trying to create user: wrong username length", "\n")
                }
                
                # Si el nom d'usuari introduït conté caràcters que no s'accepten, es mostra un missatge d'avís
                else if (!all(strsplit(input$username_register, "")[[1]] %in% chr(c(97:122, 65:90, 48:57)))) {
                  shinyalert(title = "Wrong characters for username", text = "Username should only contain lowercase or upercase letters or numbers.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), initial_user, ":: trying to create user: wrong username characters", "\n")
                }
                
                # Si la contrasenya introduïda té menys de 6 o més de 32 caràcters, es mostra un missatge d'avís
                else if (nchar(input$password_register) < 6 | nchar(input$password_register) > 32) {
                  shinyalert(title = "Wrong password length", text = "Password should have a length between 6 and 32 characters.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), initial_user, ":: trying to create user: wrong password length", "\n")
                }
                
                # Si la contrasenya introduïda conté caràcters que no s'accepten, es mostra un missatge d'avís
                else if (!all(strsplit(input$password_register, "")[[1]] %in% chr(c(97:122, 65:90, 48:57)))) {
                  shinyalert(title = "Wrong characters for password", text = "Password should only contain lowercase or upercase letters or numbers.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), initial_user, ":: trying to create user: wrong password characters", "\n")
                }
                
                # Si la contrasenya introduïda no coincideix amb la confirmació, es mostra un missatge d'avís
                else if (input$password_register != input$password_confirm) {
                  shinyalert(title = '"Password" and "Confirm password" do not match', text = '"Confirm password" should be exactly the same as "Password".',
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), initial_user, ":: trying to create user: not matching password and confirm password", "\n")
                }
                
                # Si no es dóna cap de les condicions anteriors, es crea el nou usuari i es mostra un missatge d'èxit
                else {
                  createUser(input$username_register, input$password_register)
                  shinyalert(title = "User created successfully!", text = "You can now log in with your username and password.\nRemember them well because for now we do not offer any option to recover it.",
                             size = "m", type = "success", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), initial_user, ":: successful creation of new user", input$username_register, "\n")
                  
                  # Es reinicien els inputs
                  reset("username_register")
                  reset("password_register")
                  reset("password_confirm")
                }
              }
              
              # Si l'usuari actual no és "initial" es mostra un missatge d'error
              else {
                shinyalert(title = "Can't create a new user", text = "To create a new user you have to log out first.",
                           size = "m", type = "error", showConfirmButton = TRUE, showCancelButton = FALSE,
                           confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                cat(file = stderr(), values$user, ":: unable to create new user without logging out first", "\n")
              }
            })
  
  
            # Es crea un altre valor reactiu que conté la informació del pool
            values$pool <- initial_pool
            
            # Es crea un altre valor reactiu per a indicar l'accés d'un nou usuari
            values$new_user_access <- FALSE
            
            
            # Es defineix què passa al clicar "Log in"
            observeEvent(input$log_in, {
              
              # Si l'usuari actual és "initial"
              if (values$user == "initial") {
                # Si el nom d'usuari introduit es troba en la base de dades
                if (input$username_login %in% usersList()) {
                  # S'intenta connectar a la base de dades amb el nom d'usuari i la contrasenya indicades
                  try_pool <- try({dbPool(
                                   RMySQL::MySQL(), 
                                   dbname = dbname, 
                                   host = host, 
                                   port = port, 
                                   username = input$username_login, 
                                   password = input$password_login)}, silent = TRUE)
                  
                  # Si no es pot connectar a la base de dades, es mostra un missatge d'error
                  if (class(try_pool)[1] == "try-error") {
                    shinyalert(title = "Wrong username or password", text = "The username or password introduced are not correct. Try again.",
                               size = "m", type = "error", showConfirmButton = TRUE, showCancelButton = FALSE,
                               confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                    cat(file = stderr(), initial_user, ":: couldn't connect to database with the specified password", "\n")
                  }
                  
                  # Si es pot connectar a la base de dades
                  else {
                    
                    # S'indica que es tanqui el nou pool al tancar la sessió
                    onSessionEnded(function() {
                      poolClose(try_pool)
                    })
                    
                    # Es guarda el nou pool en la variable reactiva
                    values$pool <- try_pool
                    
                    # Es guarda el nou usuari en la variable reactiva
                    values$user <- input$username_login
                    
                    # Es mostra un missatge d'èxit
                    shinyalert(title = "You have successfully logged in!", text = "Now you can create, edit or delete your pedigrees.",
                               size = "m", type = "success", showConfirmButton = TRUE, showCancelButton = FALSE,
                               confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                    
                    # Es mostra la pestanya "Pedigree creator" del menú superior
                    showTab("superior_menu", "ped_creator_tab", select = TRUE)
                    
                    # Es mostra la informació de l'usuari que ha accedit i els botons "Log out" i "Delete user"
                    shinyjs::show("logged_user_show")
                    
                    # Es reinicien els inputs
                    reset("username_login")
                    reset("password_login")
                    
                    # Es converteix a TRUE la variable reactiva que indica un nou accés
                    values$new_user_access <- TRUE
                    
                    cat(file = stderr(), values$user, ":: accessed database", "\n")
                  }
                }
                
                # Si el nom d'usuari introduit no està present a la base de dades, es mostra un missatge d'error
                else {
                  shinyalert(title = "Wrong username or password", text = "The username or password introduced are not correct. Try again.",
                             size = "m", type = "error", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), initial_user, ":: couldn't connect to database with the specified user", "\n")
                }
              }
              
              # Si l'usuari actual no és "initial", es mostra un missatge d'error
              else {
                shinyalert(title = "Can't log in with another user", text = "To log in with another user you have to log out first.",
                           size = "m", type = "error", showConfirmButton = TRUE, showCancelButton = FALSE,
                           confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                cat(file = stderr(), values$user, ":: unable to log in with another user without logging out first", "\n")
              }
            })
  
  
            # Es mostra el nom de l'usuari que ha accedit
            output$logged_user <- renderText(values$user)
  
  
            # S'indica que s'ha de reiniciar l'aplicació al clicar "Log out"
            observeEvent(input$log_out, {
                cat(file = stderr(), values$user, ":: logged out. Reseting session", "\n")
                session$reload()
            })
            
            
            # Es defineix el que ha de passar al clicar "Delete user"
            observeEvent(input$delete_user, {
              
              # Si no hi ha pedigrees associats a l'usuari actual, es mostra un missatge de confirmació
              # i s'elimina l'usuari en cas de confirmar-ho
              if (nrow(pedIDs(values$pool, values$user)) == 0) {
                shinyalert(title = "Delete user account", size = "m", animation = TRUE,
                           text = "Are you sure you want to delete your user account?", 
                           type = "warning", showConfirmButton = TRUE, showCancelButton = TRUE, 
                           confirmButtonText = "Yes", confirmButtonCol = "#ED5151", cancelButtonText = "No",
                           callbackR = function(x) {
                             if(x) {
                               deleteUser(values$user)
                               cat(file = stderr(), initial_user, ":: deleting user", values$user, "and reseting session", "\n")
                               session$reload()
                             }
                           })
              }
              
              # Si hi ha pedigrees associats a l'usuari, es mostra un missatge indicant que s'han d'eliminar primer
              else {
                shinyalert(title = "Can't delete user account", text = "You have to delete all your pedigrees before deleting your user account.",
                           size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                           confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                cat(file = stderr(), values$user, ":: unable to delete user account until all pedigrees are deleted", "\n")
              }
            })
  
  
            # Es crea un valor reactiu per a indicar la creació d'un nou pedigree
            values$new_ped <- 0
            
            # Es crea un valor reactiu per a indicar l'eliminació d'un pedigree
            values$del_ped <- FALSE
            
            # Es crea un valor reactiu que guarda el ID del pedigree amb el que s'està treballant (NULL a l'inici)
            values$ped_id <- NULL
            
            
            # S'indica que es mostrin o s'amaguin determinades pestanyes i el panell de la dreta en funció
            # de si values$ped_id és NULL o no
            observe({
                if (is.null(values$ped_id)) {
                  hide("output_panel_show")
                  hideTab("inputs_panel", "define_phenotypes")
                  hideTab("inputs_panel", "add_family_members")
                  hideTab("inputs_panel", "customize_pedigree")
                }
                else {
                  shinyjs::show("output_panel_show")
                  showTab("inputs_panel", "define_phenotypes")
                  showTab("inputs_panel", "add_family_members")
                  showTab("inputs_panel", "customize_pedigree")
                }
            })
            
            
            # Es defineix el que ha de passar al clicar el botó "Create a new pedigree"
            events_createped <- reactive({
                list(input$new_pedigree1, input$new_pedigree2)
            })
            
            observeEvent(events_createped(), {
                
                # Es mostra un quadre de diàleg per a introduir el nom del nou pedigree
                shinyalert(inputId = "ped_name", title = "New pedigree", text = "Enter a name for the pedigree",
                           size = "s", type = "input", inputType = "text", inputValue = "",
                           inputPlaceholder = "Pedigree name", showConfirmButton = TRUE, showCancelButton = TRUE,
                           confirmButtonText = "Create pedigree", confirmButtonCol = "#2C8AF5",
                           cancelButtonText = "Cancel", animation = TRUE,
                           callbackR = function(x) {
                             if(x != FALSE) {
                               values$new_ped <- 1
                             }
                           }
                           )
                cat(file = stderr(), values$user, ":: opening dialog to create a new pedigree", "\n")
            }, ignoreInit = TRUE)
            
            
            # Es defineix el que ha de passar al clicar "Create pedigree" dins del quadre de diàleg
            events_newped <- reactive({
                if (values$new_ped == 1) return(TRUE)
            })
            
            observeEvent(events_newped(), {
              
              # Si el nom del pedigree té més de 50 caràcters, es mostra un missatge d'avís
              if (nchar(input$ped_name) > 50) {
                shinyalert(title = "The name of the pedigree is too long", text = "Pedigree name should have less than 50 characters.",
                           size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                           confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                cat(file = stderr(), values$user, ":: trying to create pedigree: the name of the pedigree is too long", "\n")
              }
              
              # Si el nom del pedigree conté caràcters que no s'accepten, es mostra un missatge d'avís
              else if (!all(strsplit(input$ped_name, "")[[1]] %in% chr(c(97:122, 65:90, 48:57, 32, 44:46, 95)))) {
                shinyalert(title = "Wrong characters for pedigree name", text = "Pedigree name should only contain lowercase or upercase letters, numbers, spaces, periods, commas, hyphens or underscores.",
                           size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                           confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                cat(file = stderr(), initial_user, ":: trying to create pedigree: wrong pedigree name characters", "\n")
              }
              
              # Si el nom del pedigree té 0 caràcters, es mostra un missatge d'avís
              else if (nchar(input$ped_name) == 0) {
                shinyalert(title = "You must enter a pedigree name", text = "Pedigree name should have at least one character.",
                           size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                           confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                cat(file = stderr(), values$user, ":: trying to create pedigree: any name entered", "\n")
              }
              
              # Si no es dóna cap de les anterior condicions
              else {
                
                # Es genera un ID de sis xifres aleatòries i es comprova que no estigui ja present a la base de dades
                tries <- 1
                while(tries < 100) {
                  x <- sample(100000:999999, 1)
                  if (x %in% pedIDs(values$pool, 0)) tries <- tries + 1
                  else {
                    tries <- 101
                    ped_id <- x
                  }
                }
                
                # Es guarden en un vector el ID generat, el nom introduit pel pedigree, el nom d'usuari, la data de creació
                # i tots els valors per defecte de les característiques del pedigree que es podran modificar posteriorment
                config <- c(ped_id = ped_id, ped_name = input$ped_name, username = values$user, 
                            creation_date = as.character(Sys.Date()),
                            n_phen = 1, n_info = 1, phen1_hpo = "HP:0000001", phen2_hpo = "HP:0000001", 
                            phen3_hpo = "HP:0000001", phen4_hpo = "HP:0000001", width = 10,
                            height = 5, symbolsize = 1.5, textsize = 1, 
                            distance = 2, pconnect = 0.5, branch = 0.6, 
                            phen1_color = "#73e1ff", phen1_density = 1, phen1_angle = 45, 
                            phen2_color = "#82ff73", phen2_density = 1, phen2_angle = 45, 
                            phen3_color = "#ffc173", phen3_density = 1, phen3_angle = 45, 
                            phen4_color = "#bf73ff", phen4_density = 1, phen4_angle = 45, 
                            include1 = "name-phen1-phen2-phen3-phen4",
                            include2 = "age-consultand-info1-info2-info3-info4",
                            show_legend = 1, legend_width = 10, legend_height = 4,
                            legend_symbolsize = 1.3, legend_textsize = 1)
                
                # S'afegeix aquest vector com una nova fila de la taula "pedigrees" amb la funció definida a l'inici del script       
                addConfig(values$pool, config[config != "NA"])
                
                # S'assigna el ID del pedigree al valor reactiu values$ped_id
                values$ped_id <- ped_id
                
                # Es mostra la pestanya "Summary table" en el panell de la dreta
                updateTabsetPanel(session, "outputs_panel", selected = "summary_table")
                
                cat(file = stderr(), values$user, ":: creating new pedigree with ID", ped_id, "and name", input$ped_name, "\n")
                cat(file = stderr(), values$user, ":: setting", input$ped_name, "(", ped_id, ") as working pedigree", "\n")
              } 
            }, ignoreInit = TRUE, priority = 2)
            
            
            # S'indica que es mostri una cosa o una altra a la pestanya "Choose a pedigree" en funció de si
            # hi ha pedigrees associats a un nom d'usuari o no
            events_changepeds <- reactive({
                list(events_newped(), values$del_ped, values$new_user_access)
            })
  
            observeEvent(events_changepeds(), {
              if (nrow(pedIDs(values$pool, values$user)) > 0) {
                shinyjs::show("pedigree_choose")
                hide("no_pedigrees")
              }  
              else {
                hide("pedigree_choose")
                shinyjs::show("no_pedigrees")
              }
              
              values$new_ped <- 0
              values$del_ped <- FALSE
              values$new_user_access <- FALSE
            })
            
            
            # Es mostra l'input que permet seleccionar el pedigree amb el que es vol treballar
            output$ped_choose <- renderUI({
                {events_changepeds()}
                radioButtons("ped_sel", label = NULL, choices = pedNames(values$pool, values$user), selected = values$ped_id)
            })
            
            
            # S'indica el que ha de passar al clicar "Delete selected pedigree"
            observeEvent(input$delete_ped, {
              
                # Es guarda el nom del pedigree seleccionat
                name <- as.character(pedName(values$pool, input$ped_sel))
              
                # Es mostra un missatge per a confirmar l'eliminació i s'elimina el pedigree si es confirma
                shinyalert(title = "Delete pedigree", size = "m", animation = TRUE,
                           text = "Are you sure you want to delete the selected pedigree?", 
                           type = "warning", showConfirmButton = TRUE, showCancelButton = TRUE, 
                           confirmButtonText = "Yes", confirmButtonCol = "#ED5151", cancelButtonText = "No",
                           callbackR = function(x) {
                             if(x) {
                               deletePed(values$pool, input$ped_sel)
                               cat(file = stderr(), values$user, ":: deleting pedigree", name, "(", input$ped_sel, ")", "\n")
                               values$del_ped <- TRUE
                               values$ped_id <- NULL
                             }
                           })
            }, priority = 2)
            
            
            # S'indica el que ha de passar al clicar "Work with the selected pedigree"
            observeEvent(input$working_ped, {
              
                # Es guarda el ID del pedigree seleccionat a la variable reactiva values$ped_id
                values$ped_id <- as.numeric(input$ped_sel)
                
                # Es mostra la pestanya "Summary table" al panell de la dreta
                updateTabsetPanel(session, "outputs_panel", selected = "summary_table")
                
                # Es guarda el nom del pedigree seleccionat
                name <- as.character(pedName(values$pool, input$ped_sel))
                cat(file = stderr(), values$user, ":: setting", name, "(", input$ped_sel, ") as working pedigree", "\n")
            })
            
            
            # Es mostra un missatge indicant amb quin pedigree s'està treballant
            output$work_ped_now <- renderText({ 
                if (is.null(values$ped_id)) return("No selected pedigree to work with")
                else return(paste("Working with pedigree", pedName(values$pool, values$ped_id)))
            })
            
  
            
            # Es guarda en forma de valor reactiu el data frame que conté tota la informació de la taula "members"
            # pel pedigree seleccionat
            observe({
                values$data <- as.data.frame(loadData(values$pool, values$ped_id))
            })
            
  
            # Es crea un valor reactiu que contindrà el ID de l'individu que es vulgui editar. Inicialment és NULL
            # i només se li otorga un valor quan es clica el botó "Edit member"
            values$id_edit <- NULL
            
            # Es crea un valor reactiu que s'utilitzarà per a confirmar quan es vol eliminar un membre de la taula
            values$delete_yes <- FALSE
            
            # S'indica que el data frame reactiu s'ha d'actualitzar cada vegada que es clica "Submit", "Save changes",
            # "Reorder row up" o "Reorder row down" o que es confirma l'acció de "Delete member" o "Delete pedigree"
            events_tabupdate <- reactive({
                list(input$submit, input$save_changes, input$up, input$down, values$delete_yes, values$del_ped)
            })
  
            observeEvent(events_tabupdate(), {
                values$data <- as.data.frame(loadData(values$pool, values$ped_id))
                values$delete_yes <- FALSE
                cat(file = stderr(), values$user, ":: loading members table", "\n")
            }, priority = 1)
            
            
            # Es defineixen els inputs canviants indicats anteriorment amb la funció "uiOutput". 
            # Aquests mostren opcions diferents segons els membres de la família ja introduïts. 
            events_membersupdate <- reactive({
                list(input$submit, values$delete_yes, input$save_changes, input$cancel)
            })
            
            # Selecció mare (o 1r progenitor/tutor)
            output$mother_select <- renderUI({
                {events_membersupdate()}
                selectInput("mother", label = "Mother (or 1st parent)", choices = selectSex(values$data, "all"))
            })
            
            # Selecció pare (o 2n progenitor/tutor)
            output$father_select <- renderUI({
                {events_membersupdate()}
                selectInput("father", label = "Father (or 2nd parent)", choices = selectSex(values$data, "all"))
            })
            
            # Selecció bessons monozigòtics (s'han de mostrar homes, dones o tots dos segons el sexe seleccionat)
            output$motwins_select <- renderUI({
                {events_membersupdate()}
                if (input$sex == 1) selectInput("motwins", label = NULL, choices = selectSex(values$data, "male"))
                
                else if (input$sex == 2) selectInput("motwins", label = NULL, choices = selectSex(values$data, "female"))
              
                else selectInput("motwins", label = NULL, choices = selectSex(values$data, "all"))
            })
            
            # Selecció bessons dizigòtics
            output$ditwins_select <- renderUI({
                {events_membersupdate()}
                selectInput("ditwins", label = NULL, choices = selectSex(values$data, "all"))
            })
            
            # Selecció bessons de zigositat desconeguda 
            output$untwins_select <- renderUI({
                {events_membersupdate()}
                selectInput("untwins", label = NULL, choices = selectSex(values$data, "all"))
            })
            
            # Selecció matrimoni/parella sense fills
            output$marriage_select <- renderUI({
                {events_membersupdate()}
                selectInput("marriage", label = NULL, choices = selectSex(values$data, "all"))
            })
            
            observeEvent(events_membersupdate(), {
                values$delete_yes <- FALSE
            })
            
            
            
            # S'indica que l'apartat d'adopcions es mostri o s'amagui al clicar a l'enllàs que ho indica
            observe({
                onclick(id = "show_hide1", toggle("adopted_show", anim = TRUE))
            })
            
            
            # S'indica que l'apartat "Add other relationships" es mostri o s'amagui al clicar l'enllaç
            # que ho indica
            observe({
                onclick(id = "show_hide2", toggle("other_rel_show", anim = TRUE))
            })
            

            # S'indica el nombre que s'ha de mostrar tant per als inputs per a seleccionar els fenotips d'interès 
            # (pestanya "Define the phenotypes"), com per als inputs per a indicar si un individu està afectat
            # o no (pestanya "Add family members") com per a les taules de gens i malalties (pestanya 
            # "Genes and diseases") 
            observe({
                if (input$show_num1 == 1) {
                  hide("phenotype2_show")
                  hide("phenotype3_show")
                  hide("phenotype4_show")
                  hide("phen2_show")
                  hide("phen3_show")
                  hide("phen4_show")
                  hide("phenotype2_genes_show")
                  hide("phenotype3_genes_show")
                  hide("phenotype4_genes_show")
                }
                else if (input$show_num1 == 2) {
                  shinyjs::show("phenotype2_show")
                  hide("phenotype3_show")
                  hide("phenotype4_show")
                  shinyjs::show("phen2_show")
                  hide("phen3_show")
                  hide("phen4_show")
                  shinyjs::show("phenotype2_genes_show")
                  hide("phenotype3_genes_show")
                  hide("phenotype4_genes_show")
                }
                else if (input$show_num1 == 3) {
                  shinyjs::show("phenotype2_show")
                  shinyjs::show("phenotype3_show")
                  hide("phenotype4_show")
                  shinyjs::show("phen2_show")
                  shinyjs::show("phen3_show")
                  hide("phen4_show")
                  shinyjs::show("phenotype2_genes_show")
                  shinyjs::show("phenotype3_genes_show")
                  hide("phenotype4_genes_show")
                }
                else if (input$show_num1 == 4) {
                  shinyjs::show("phenotype2_show")
                  shinyjs::show("phenotype3_show")
                  shinyjs::show("phenotype4_show")
                  shinyjs::show("phen2_show")
                  shinyjs::show("phen3_show")
                  shinyjs::show("phen4_show")
                  shinyjs::show("phenotype2_genes_show")
                  shinyjs::show("phenotype3_genes_show")
                  shinyjs::show("phenotype4_genes_show")
                }
            })
            
            
            # S'indica que es canviï el nom dels fenotips dins la pestanya "Add family members"
            observe({
                updateSelectInput(session, "phen1", label = names(which(hpo == input$phenotype1)))
                updateSelectInput(session, "phen2", label = names(which(hpo == input$phenotype2)))
                updateSelectInput(session, "phen3", label = names(which(hpo == input$phenotype3)))
                updateSelectInput(session, "phen4", label = names(which(hpo == input$phenotype4)))
            })
            
            
            # S'indica que es mostrin més o menys camps en l'apartat "Other information" segons el número indicat
            observe({
                if (input$show_num2 == 1) {
                  hide("info2_space")
                  hide("info3_space")
                  hide("info4_space")
                }
                else if (input$show_num2 == 2) {
                  shinyjs::show("info2_space")
                  hide("info3_space")
                  hide("info4_space")
                }
                else if (input$show_num2 == 3) {
                  shinyjs::show("info2_space")
                  shinyjs::show("info3_space")
                  hide("info4_space")
                }
                else if (input$show_num2 == 4) {
                  shinyjs::show("info2_space")
                  shinyjs::show("info3_space")
                  shinyjs::show("info4_space")
                }
            })
            
            
            # S'indica que es reiniciïn els valors per defecte de cada input de la pestanya "Add family members"
            # cada vegada que es prem "Submit", "Save changes" o "Cancel" o que es crea un nou pedigree
            events_reset <- reactive({
                list(values$submit_reset, input$cancel, events_newped())
            })
            
            observeEvent(events_reset(), {
                reset("name")
                reset("sex")
                reset("status")
                reset("adopted")
                reset("phen1")
                reset("phen2")
                reset("phen3")
                reset("phen4")
                reset("age")
                reset("consultand")
                reset("number")
                reset("info1")
                reset("info2")
                reset("info3")
                reset("info4")
                
                values$submit_reset <- FALSE
                
                #cat(file = stderr(), values$user, ":: reseting 'Add family members' form", "\n")
            }, priority = 1, ignoreInit = TRUE)
            
            
            
            # S'indica que es mostrin més o menys opcions a l'input en que s'indiquen quines característiques
            # es volen incloure en el gràfic (apartat "Customize pedigree")
            observe({
                if (input$show_num1 == 1) {
                  updateCheckboxGroupInput(session, "include1", choices = c("Name" = "name", "Phenotype 1" = "phen1"),
                                           selected = c("name", "phen1"))
                }
                else if (input$show_num1 == 2) {
                  updateCheckboxGroupInput(session, "include1", choices = c("Name" = "name", "Phenotype 1" = "phen1",
                                                                            "Phenotype 2" = "phen2"),
                                           selected = c("name", "phen1", "phen2"))
                }
                else if (input$show_num1 == 3) {
                  updateCheckboxGroupInput(session, "include1", choices = c("Name" = "name", "Phenotype 1" = "phen1",
                                                                            "Phenotype 2" = "phen2", 
                                                                            "Phenotype 3" = "phen3"),
                                           selected = c("name", "phen1", "phen2", "phen3"))
                }
                else if (input$show_num1 == 4) {
                  updateCheckboxGroupInput(session, "include1", choices = c("Name" = "name", "Phenotype 1" = "phen1",
                                                                            "Phenotype 2" = "phen2", 
                                                                            "Phenotype 3" = "phen3",
                                                                            "Phenotype 4" = "phen4"),
                                           selected = c("name", "phen1", "phen2", "phen3", "phen4"))
                }
                #cat(file = stderr(), values$user, ":: changing number of phenotypes to show in 'include':", input$show_num1, "\n")
            })
            
            observe({
                if (input$show_num2 == 1) {
                  updateCheckboxGroupInput(session, "include2", choices = c("Age" = "age", "Consultand" = "consultand", 
                                                                            "Information 1" = "info1"),
                                           selected = c("age", "consultand", "info1"))
                }
                else if (input$show_num2 == 2) {
                  updateCheckboxGroupInput(session, "include2", choices = c("Age" = "age", "Consultand" = "consultand", 
                                                                            "Information 1" = "info1",
                                                                            "Information 2" = "info2"),
                                           selected = c("age", "consultand", "info1", "info2"))
                }
                else if (input$show_num2 == 3) {
                  updateCheckboxGroupInput(session, "include2", choices = c("Age" = "age", "Consultand" = "consultand", 
                                                                            "Information 1" = "info1",
                                                                            "Information 2" = "info2",
                                                                            "Information 3" = "info3"),
                                           selected = c("age", "consultand", "info1", "info2", "info3"))
                }
                else if (input$show_num2 == 4) {
                  updateCheckboxGroupInput(session, "include2", choices = c("Age" = "age", "Consultand" = "consultand", 
                                                                            "Information 1" = "info1",
                                                                            "Information 2" = "info2",
                                                                            "Information 3" = "info3",
                                                                            "Information 4" = "info4"),
                                           selected = c("age", "consultand", "info1", "info2", "info3", "info4"))
                }
                #cat(file = stderr(), values$user, ":: changing number of information fields to show in 'include':", input$show_num2, "\n")
            })
            
            
            
            # S'indica que es reiniciïn tots els inputs que defineixen les característiques dels pedigrees
            # al crear un nou pedigree
            observeEvent(events_newped(), {
                reset("phenotype1")
                reset("phenotype2")
                reset("phenotype3")
                reset("phenotype4")
                reset("show_num1")
                reset("show_num2")
                reset("include1")
                reset("include2")
                reset("width")
                reset("height")
                reset("symbol_size")
                reset("text_size")
                reset("text_dist")
                reset("pconnect")
                reset("branch")
                reset("color1")
                reset("density1")
                reset("angle1")
                reset("color2")
                reset("density2")
                reset("angle2")
                reset("color3")
                reset("density3")
                reset("angle3")
                reset("color4")
                reset("density4")
                reset("angle4")
                reset("show_legend")
                reset("legend_width")
                reset("legend_height")
                reset("legend_symbolsize")
                reset("legend_textsize")
                
                cat(file = stderr(), values$user, ":: reseting pedigree parameters", "\n")
            })
            
            
            
            # S'indica que l'apartat "More options" es mostri o s'amagui al clicar l'enllaç que ho indica
            observe({
                onclick(id = "show_hide3", toggle("more_options_show", anim = TRUE))
            })
            
            
            
            # Es mostra el data frame que conté tota la informació de la taula "members" pel pedigree seleccionat
            # en la pestanya "Summary table".
            # Es modifica la taula per a que sigui més entenedora per als usuaris. 
            # Es permet que es puguin seleccionar files de la taula i es guarda el "id" de la fila que està seleccionada
            # per tal de que pugui ser utilitzat al clicar algun dels botons superiors. 
            output$sum_table <- renderDataTable({
                
                # Es guarda el data frame reactiu amb el nom "df"
                df <- values$data
                
                # Si el data frame no conté informació es mostra una taula buida
                if (nrow(df) == 0) {
                  tab <- data.frame(ID = c(NA), Name = c(NA), Sex = c(NA), Status = c(NA), Mother = c(NA),
                                    Father = c(NA), Adopted = c(NA), MZ.twin = c(NA), DZ.twin = c(NA), UZ.twin = c(NA), 
                                    Marriage = c(NA), Phen1 = c(NA), Phen2 = c(NA), Phen3 = c(NA), Phen4 = c(NA), 
                                    Age = c(NA), Cons = c(NA), Num = c(NA), Info1 = c(NA), Info2 = c(NA), Info3 = c(NA), 
                                    Info4 = c(NA))
                }
                
                # Si el data frame conté informació, es modifiquen els valors d'algunes variables del data frame
                # per a fer-los més entenedors per a l'usuari
                else {
                  Name <- df$name
                  Adopted <- df$adopted
                  Age <- df$age
                  Num <- df$number
                  I1 <- df$info1
                  I2 <- df$info2
                  I3 <- df$info3
                  I4 <- df$info4
                  
                  ID <- c()
                  Sex <- c()
                  Status <- c()
                  Mother <- c()
                  Father <- c()
                  MZtwin <- c()
                  DZtwin <- c()
                  UZtwin <- c()
                  Marriage <- c()
                  P1 <- c()
                  P2 <- c()
                  P3 <- c()
                  P4 <- c()
                  Cons <- c()
                  
                  for (i in 1:nrow(df)) {
                    
                    ID[i] <- substr(df$id[i], nchar(df$id[i]) - 2, nchar(df$id[i]))
                    
                    if (df$sex[i] == 1) Sex[i] <- "M"
                    else if (df$sex[i] == 2) Sex[i] <- "F"
                    else if (df$sex[i] == 3) Sex[i] <- "U"
                    else if (df$sex[i] == 4) Sex[i] <- "A"

                    if (df$status[i] == 0 & df$sex[i] != 4) Status[i] <- "Al"
                    else if (df$status[i] == 1 & df$sex[i] != 4) Status[i] <- "De"
                    else if (df$status[i] == 2) Status[i] <- "Pr"
                    else if (df$status[i] == 0 & df$sex[i] == 4) Status[i] <- "SpAb"
                    else if (df$status[i] == 1 & df$sex[i] == 4) Status[i] <- "InAb"
                    
                    if (is.na(df$mother[i])) Mother[i] <- NA
                    else Mother[i] <- substr(df$mother[i], nchar(df$mother[i]) - 2, nchar(df$mother[i]))

                    if (is.na(df$father[i])) Father[i] <- NA
                    else Father[i] <- substr(df$father[i], nchar(df$father[i]) - 2, nchar(df$father[i]))
                    
                    if (is.na(df$mono_twins[i])) MZtwin[i] <- NA
                    else MZtwin[i] <- substr(df$mono_twins[i], nchar(df$mono_twins[i]) - 2, nchar(df$mono_twins[i]))

                    if (is.na(df$di_twins[i])) DZtwin[i] <- NA
                    else DZtwin[i] <- substr(df$di_twins[i], nchar(df$di_twins[i]) - 2, nchar(df$di_twins[i]))

                    if (is.na(df$unk_twins[i])) UZtwin[i] <- NA
                    else UZtwin[i] <- substr(df$unk_twins[i], nchar(df$unk_twins[i]) - 2, nchar(df$unk_twins[i]))

                    if (is.na(df$marriage[i])) Marriage[i] <- NA
                    else Marriage[i] <- substr(df$marriage[i], nchar(df$marriage[i]) - 2, nchar(df$marriage[i]))

                    if (df$phen1[i] == 0) P1[i] <- "NA"
                    else if (df$phen1[i] == 1) P1[i] <- "A"
                    else if (df$phen1[i] == 2) P1[i] <- "C"
                    else if (df$phen1[i] == 3) P1[i] <- "PC"

                    if (df$phen2[i] == 0) P2[i] <- "NA"
                    else if (df$phen2[i] == 1) P2[i] <- "A"
                    else if (df$phen2[i] == 2) P2[i] <- "C"
                    else if (df$phen2[i] == 3) P2[i] <- "PC"

                    if (df$phen3[i] == 0) P3[i] <- "NA"
                    else if (df$phen3[i] == 1) P3[i] <- "A"
                    else if (df$phen3[i] == 2) P3[i] <- "C"
                    else if (df$phen3[i] == 3) P3[i] <- "PC"

                    if (df$phen4[i] == 0) P4[i] <- "NA"
                    else if (df$phen4[i] == 1) P4[i] <- "A"
                    else if (df$phen4[i] == 2) P4[i] <- "C"
                    else if (df$phen4[i] == 3) P4[i] <- "PC"

                    if (df$consultand[i] == 1) Cons[i] <- "Yes"
                    else Cons[i] <- NA
                  }
                  
                  # Es guarda en un nou data frame la taula resultant
                  tab <- data.frame(ID = ID, Name = Name, Sex = Sex, Status = Status, Mother = Mother,
                                    Father = Father, Adopted = Adopted, MZ.twin = MZtwin, DZ.twin = DZtwin, 
                                    UZ.twin = UZtwin, Marriage = Marriage, Phen1 = P1, Phen2 = P2, Phen3 = P3, 
                                    Phen4 = P4, Age = Age, Cons = Cons, Num = Num, Info1 = I1, Info2 = I2, 
                                    Info3 = I3, Info4 = I4)
                }
                
                # Es determina quines de les columnes es volen mostrar. Per a reduïr la taula, algunes variables
                # no es mostraran si tots els seus valors són NA (per exemple, en el cas dels bessons, matrimonis
                # sense fills, edats...). En el cas dels fenotips i els camps d'informació, només es mostrarà
                # el nombre de columnes que toca segons el nombre de fenotips i camps d'informació indicat
                n <- 1:6
                
                if(!all(is.na(tab$Adopted) | tab$Adopted == "")) n <- c(n, 7)
                if(!all(is.na(tab$MZ.twin))) n <- c(n, 8)
                if(!all(is.na(tab$DZ.twin))) n <- c(n, 9)
                if(!all(is.na(tab$UZ.twin))) n <- c(n, 10)
                if(!all(is.na(tab$Marriage))) n <- c(n, 11)
                
                if (input$show_num1 == 1) n <- c(n, 12)
                else if (input$show_num1 == 2) n <- c(n, 12:13)
                else if (input$show_num1 == 3) n <- c(n, 12:14)
                else if (input$show_num1 == 4) n <- c(n, 12:15)
                
                if(!all(is.na(tab$Age) | tab$Age == "")) n <- c(n, 16)
                if(!all(is.na(tab$Cons))) n <- c(n, 17)
                if(!all(is.na(tab$Num) | tab$Num == 1)) n <- c(n, 18)
                
                if (input$show_num2 == 1) n <- c(n, 19)
                else if (input$show_num2 == 2) n <- c(n, 19:20)
                else if (input$show_num2 == 3) n <- c(n, 19:21)
                else if (input$show_num2 == 4) n <- c(n, 19:22)
                  
                # Finalment, es mostra la taula amb les columnes seleccionades. En aquesta taula es pot seleccionar
                # una fila i guardar el "id" de la fila seleccionada en la variable input$id_num
                datatable(tab[,n], class = "row-border hover", rownames = FALSE,
                          selection = "single", extensions = "Scroller", 
                          options = list(dom = "ti", ordering = FALSE, scrollX = 1000, scrollY = 450, 
                                         scroller = TRUE, 
                                         columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                          callback = JS("table.on('click.dt', 'tr',
                                         function() {
                                         var data=table.row(this).data();
                                         Shiny.onInputChange('id_num', data[0]);
                                         });"))
            })
            
            
            
            
            # Es defineix com guardar en la taula "members" de la base de dades la informació introduïda en 
            # el formulari de la pestanya "Add family members" per a un nou membre de la família un cop es 
            # clica "Submit" o "Save changes" (en el cas d'estar editant un membre ja introduit)
            values$submit_reset <- FALSE
            
            events_newmember <- reactive({
                list(input$submit, input$save_changes)
            })
            
            observeEvent(events_newmember(), {
              
                # Primer s'han de realitzar varies comprovacions
              
                # Si no s'ha seleccionat cap pedigree, es mostra un missatge d'avís
                if (is.null(values$ped_id)) {
                  shinyalert(title = "No selected pedigree", text = "You have to select a pedigree to work with before adding members",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member: no selected pedigree", "\n")
                }
              
                # Si el nom introduit té més de 50 caràcters, es mostra un missatge d'avís
                else if (nchar(input$name) > 50) {
                  shinyalert(title = "The name is too long", text = "Names should have less than 50 characters.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": name is too long", "\n")
                }
              
                # Si el nom introduit conté caràcters que no s'accepten, es mostra un missatge d'avís
                else if (!all(strsplit(input$name, "")[[1]] %in% chr(c(97:122, 65:90, 48:57, 32, 44:46, 95)))) {
                  shinyalert(title = "Wrong characters for name", text = "Name should only contain lowercase or upercase letters, numbers, spaces, periods, commas, hyphens or underscores.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": wrong characters for name", "\n")
                }
              
                # Si l'edat introduida té més de 30 caràcters, es mostra un missatge d'avís
                else if (nchar(input$age) > 30) {
                  shinyalert(title = "The age is too long", text = "Age should have less than 30 characters.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": age is too long", "\n")
                }
              
                # Si l'edat introduida conté caràcters que no s'accepten, es mostra un missatge d'avís
                else if (!all(strsplit(input$age, "")[[1]] %in% chr(c(97:122, 65:90, 48:57, 32, 44:46, 95)))) {
                  shinyalert(title = "Wrong characters for age", text = "Age should only contain lowercase or upercase letters, numbers, spaces, periods, commas, hyphens or underscores.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": wrong characters for age", "\n")
                }
              
                # Si el primer camp d'informació té més de 100 caràcters, es mostra un missatge d'avís
                else if (nchar(input$info1) > 100) {
                  shinyalert(title = "The information filed 1 is too long", text = "Information fields should have less than 100 characters.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": info1 is too long", "\n")
                }
              
                # Si el primer camp d'informació conté caràcters que no s'accepten, es mostra un missatge d'avís
                else if (!all(strsplit(input$info1, "")[[1]] %in% chr(c(97:122, 65:90, 48:59, 32, 40:46, 95, 63)))) {
                  shinyalert(title = "Wrong characters for information field 1", text = "Information fields should only contain lowercase or upercase letters, numbers, spaces, periods, commas, hyphens, underscores, parenthesis, asterisks, pluses, colons, semicolons and question marks.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": wrong characters for info1", "\n")
                }
              
                # Si el segon camp d'informació té més de 100 caràcters, es mostra un missatge d'avís
                else if (nchar(input$info2) > 100) {
                  shinyalert(title = "The information filed 2 is too long", text = "Information fields should have less than 100 characters.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": info2 is too long", "\n")
                }
              
                # Si el segon camp d'informació conté caràcters que no s'accepten, es mostra un missatge d'avís
                else if (!all(strsplit(input$info2, "")[[1]] %in% chr(c(97:122, 65:90, 48:59, 32, 40:46, 95, 63)))) {
                  shinyalert(title = "Wrong characters for information field 2", text = "Information fields should only contain lowercase or upercase letters, numbers, spaces, periods, commas, hyphens, underscores, parenthesis, asterisks, pluses, colons, semicolons and question marks.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": wrong characters for info2", "\n")
                }
              
                # Si el tercer camp d'informació té més de 100 caràcters, es mostra un missatge d'avís
                else if (nchar(input$info3) > 100) {
                  shinyalert(title = "The information filed 3 is too long", text = "Information fields should have less than 100 characters.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": info3 is too long", "\n")
                }
              
                # Si el tercer camp d'informació conté caràcters que no s'accepten, es mostra un missatge d'avís
                else if (!all(strsplit(input$info3, "")[[1]] %in% chr(c(97:122, 65:90, 48:59, 32, 40:46, 95, 63)))) {
                  shinyalert(title = "Wrong characters for information field 3", text = "Information fields should only contain lowercase or upercase letters, numbers, spaces, periods, commas, hyphens, underscores, parenthesis, asterisks, pluses, colons, semicolons and question marks.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": wrong characters for info3", "\n")
                }
              
                # Si el quart camp d'informació té més de 100 caràcters, es mostra un missatge d'avís
                else if (nchar(input$info4) > 100) {
                  shinyalert(title = "The information filed 4 is too long", text = "Information fields should have less than 100 characters.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": info4 is too long", "\n")
                }
              
                # Si el quart camp d'informació conté caràcters que no s'accepten, es mostra un missatge d'avís
                else if (!all(strsplit(input$info4, "")[[1]] %in% chr(c(97:122, 65:90, 48:59, 32, 40:46, 95, 63)))) {
                  shinyalert(title = "Wrong characters for information field 4", text = "Information fields should only contain lowercase or upercase letters, numbers, spaces, periods, commas, hyphens, underscores, parenthesis, asterisks, pluses, colons, semicolons and question marks.",
                             size = "m", type = "warning", showConfirmButton = TRUE, showCancelButton = FALSE,
                             confirmButtonText = "OK", confirmButtonCol = "#AEDEF4", animation = TRUE)
                  cat(file = stderr(), values$user, ":: can't add new member to", values$ped_id, ": wrong characters for info4", "\n")
                }
              
                # Si no es dóna cap d'aquestes condicions
                else {
                  
                  # Primer de tot es comprova si hi ha algún ID guardat dins la variable "values$id_edit".
                  # Si és així, vol dir que s'està editant un membre ja existent i, per tant, la variable "id" ha de 
                  # ser el ID que ja té assignat aquell membre. També s'ha de mantenir la variable "tab_order"
                  # que defineix l'ordre d'aquell individu en la taula. 
                  if (!is.null(values$id_edit)) {
                    id <- values$id_edit
                    tab_order <- values$data$tab_order[values$data$id == id]
                  }
                  # Si la variable "values$id_edit" és nul·la, es defineix un nou "id". En cas de que encara no 
                  # s'hagi afegit cap membre a la família, el "id" ha de ser el ID del pedigree seguit de 001. 
                  # Sinó, el "id" ha de ser el "id" més elevat més 1. 
                  # Una idea semblant s'aplica per la variable "tab_order".
                  else if (nrow(values$data) == 0) {
                    id <- formatC(1, width = 3, flag = 0)
                    id <- paste0(values$ped_id, id)
                    tab_order <- 1
                  }
                  else {
                    id <- as.numeric(max(values$data$id)) + 1
                    tab_order <- as.numeric(max(values$data$tab_order)) + 1
                  }
                  
                  # Es modifiquen els números corresponents a "sex" i "status" per als casos d'avortament per tal
                  # de que es corresponguin amb els números que s'utilitzen en el paquet kinship2modified per 
                  # a seguir la simbologia estàndard dels pedigrees:
                  # Avortament espontani: "sex = 4" i "status = 0" 
                  # Avortament induit: "sex = 4" i "status = 1"
                  if (input$status == 3) {
                    sex <- 4
                    status <- 0
                  }
                  else if (input$status == 4) {
                    sex <- 4
                    status <- 1
                  }
                  else {
                    sex <- input$sex
                    status <- input$status
                  }
  
                  # Es modifica el valor de la variable "consultand" per a que sigui acceptat per MySQL
                  if (input$consultand == TRUE) cons <- 1
                  else if (input$consultand == FALSE) cons <- 0
                  
                  # Es guarda tota la informació de l'individu en un vector amb els noms corresponents
                  member <- c(ped = values$ped_id, id = as.numeric(id), name = input$name, sex = sex, status = status, 
                              mother = input$mother, father = input$father, adopted = input$adopted,
                              mono_twins = input$motwins, di_twins = input$ditwins, unk_twins = input$untwins, 
                              marriage = input$marriage, phen1 = input$phen1, phen2 = input$phen2, 
                              phen3 = input$phen3, phen4 = input$phen4, age = input$age, 
                              consultand = cons, number = input$number, info1 = input$info1, info2 = input$info2, 
                              info3 = input$info3, info4 = input$info4, tab_order = tab_order)
                  
                  # S'afegeix aquesta informació a la taula "members" de la base de dades mitjançant la 
                  # funció definida a l'inici del script
                  addMember(values$pool, member[member != "NA"])
                  
                  # S'aplica la funció en JavaScript que fa que es faci scroll a la part superior de la UI
                  js$toTop()
                  
                  # S'indica que s'han de reiniciar els inputs del formulari
                  values$submit_reset <- TRUE
                  
                  cat(file = stderr(), values$user, ":: adding member", input$name, "(", id, ") to pedigree", values$ped_id, "\n")
                }
            }, ignoreInit = TRUE, priority = 2)
            
            
            
            
            # S'indica que s'ha d'eliminar la fila seleccionada en la taula quan es clica "Delete member".
            # Es mostra primer un missatge d'alerta per a confirmar que es vol eliminar l'individu
            observeEvent(input$delete_row, {
                if (!is.null(values$ped_id)) {
                  id <- paste0(values$ped_id, input$id_num)
                  name <- values$data$name[values$data$id == id]
                  shinyalert(title = "Delete member", size = "m", animation = TRUE,
                             text = "Are you sure you want to delete the selected family member?", 
                             type = "warning", showConfirmButton = TRUE, showCancelButton = TRUE, 
                             confirmButtonText = "Yes", confirmButtonCol = "#ED5151", cancelButtonText = "No",
                             callbackR = function(x) {
                               if(x) {
                                 deleteRow(values$pool, id)
                                 cat(file = stderr(), values$user, ":: deleting member", name, "(", id, ") from pedigree", values$ped_id, "\n")
                                 values$delete_yes <- TRUE
                               }
                             })
                }
            }, priority = 2)
            
            
            
            
            # S'indica tot el que ha de passar quan es clica "Edit member"
            observeEvent(input$edit_row, {
                
                # Si s'ha seleccionat alguna fila de la taula i, per tant, input$id_num no és NULL
                if (!is.null(input$id_num)) {
                
                  # S'indica que es mostri la pestanya "Add family members" en el panell de l'esquerra
                  updateTabsetPanel(session, "inputs_panel", selected = "add_family_members")
                
                  # S'indica que es mostri el missatge a la part superior d'aquesta pestanya que avisa de que s'està
                  # editant un individu ja introduit
                  shinyjs::show("editing_show")
                
                  # Es guarda el data frame que conté la informació de la taula "members" amb el nom "df" i el ID 
                  # de l'individu seleccionat amb el nom "id_num". També es guarda el ID en la variable reactiva
                  # "values$id_edit" per a saber que s'està editant un membre. 
                  df <- values$data
                  id_num <- paste0(values$ped_id, input$id_num)
                  values$id_edit <- id_num
                  
                  # S'actualitzen tots els valors del formulari amb els valors corresponents a l'individu seleccionat
                  updateTextInput(session, "name", value = df$name[df$id == id_num])
                  
                  if (df$sex[df$id == id_num] != 4) {
                    updateRadioButtons(session, "sex", selected = df$sex[df$id == id_num])
                    updateRadioButtons(session, "status", selected = df$status[df$id == id_num])
                  }
                  else if (df$sex[df$id == id_num] == 4) {
                    updateRadioButtons(session, "sex", selected = 3)
                    if (df$status[df$id == id_num] == 0) {
                      updateRadioButtons(session, "status", selected = 3)
                    } 
                    else if (df$status[df$id == id_num] == 1) {
                      updateRadioButtons(session, "status", selected = 4)
                    }
                  }
                  
                  updateSelectInput(session, "mother", selected = df$mother[df$id == id_num])
                  updateSelectInput(session, "father", selected = df$father[df$id == id_num])
                  
                  updateRadioButtons(session, "adopted", selected = df$adopted[df$id == id_num])
                  
                  updateSelectInput(session, "motwins", selected = df$mono_twins[df$id == id_num])
                  updateSelectInput(session, "ditwins", selected = df$di_twins[df$id == id_num])
                  updateSelectInput(session, "untwins", selected = df$unk_twins[df$id == id_num])
                  updateSelectInput(session, "marriage", selected = df$marriage[df$id == id_num])
                  
                  updateSelectInput(session, "phen1", selected = df$phen1[df$id == id_num])
                  updateSelectInput(session, "phen2", selected = df$phen2[df$id == id_num])
                  updateSelectInput(session, "phen3", selected = df$phen3[df$id == id_num])
                  updateSelectInput(session, "phen4", selected = df$phen4[df$id == id_num])
                  
                  updateTextInput(session, "age", value = df$age[df$id == id_num])
                  
                  if (df$consultand[df$id == id_num] == 0) {
                    updateCheckboxInput(session, "consultand", value = FALSE)
                  }
                  else if (df$consultand[df$id == id_num] == 1) {
                    updateCheckboxInput(session, "consultand", value = TRUE)
                  }
                  
                  updateSelectInput(session, "number", selected = df$number[df$id == id_num])
                  updateTextInput(session, "info1", value = df$info1[df$id == id_num])
                  updateTextInput(session, "info2", value = df$info2[df$id == id_num])
                  updateTextInput(session, "info3", value = df$info3[df$id == id_num])
                  updateTextInput(session, "info4", value = df$info4[df$id == id_num])
                  
                  # S'amaga el botó "Submit" i es mostren els botons "Save changes" i "Cancel"
                  hide("submit_show")
                  shinyjs::show("changes_show")
                  
                  cat(file = stderr(), values$user, ":: preparing to edit member", df$name[df$id == id_num], "(", id_num, ") from pedigree", values$ped_id, "\n")
                }
            })
            
            
            
            # S'indica que quan es cliqui "Save changes" s'amaguin els botons "Save changes" i "Cancel" i
            # torni a aparèixer el botó "Submit". També que s'elimini la fila corresponent a l'individu que
            # s'ha editat (valors antics de l'individu)
            observeEvent(input$save_changes, {
                hide("editing_show")
                hide("changes_show")
                shinyjs::show("submit_show")
                deleteRow(values$pool, values$id_edit)
                ped_id <- values$ped_id
                name <- values$data$name[values$data$id == values$id_edit]
                cat(file = stderr(), values$user, ":: deleting member", name, "(", values$id_edit, ") from pedigree", ped_id, "\n")
            }, priority = 3, ignoreInit = TRUE)
            
            # També s'indica que quan es cliqui "Save changes" la variable "values$id_edit" torni a ser NULL.
            # S'indica en un "observeEvent" diferent ja que té una altra prioritat (és l'últim que s'ha de fer)
            observeEvent(input$save_changes, {
                values$id_edit <- NULL
                cat(file = stderr(), values$user, ":: reseting ID to edit", "\n")
            }, priority = 1, ignoreInit = TRUE)
            
            
            
            # S'indica que quan es cliqui "Cancel" s'amaguin els botons "Save changes" i "Cancel" i
            # torni a aparèixer el botó "Submit". També s'indica que la variable "values$id_edit" torni a ser NULL
            observeEvent(input$cancel, {
                hide("editing_show")
                hide("changes_show")
                shinyjs::show("submit_show")
                values$id_edit <- NULL
                js$toTop()
                cat(file = stderr(), values$user, ":: reseting ID to edit", "\n")
            })
            
            
            
            # S'indica que s'executi la funció "changeOrder" definida a l'inici del script quan es clica "Reorder
            # row up" o "Reorder row down". 
            observeEvent(input$up, {
                if (!is.null(values$ped_id) & !is.null(input$id_num)) {
                  id <- paste0(values$ped_id, input$id_num)
                  name <- values$data$name[values$data$id == id]
                  changeOrder(values$pool, values$data, id, "up")
                  cat(file = stderr(), values$user, ":: reordering up member", name, "(", id, ")", "\n")
                }
            }, priority = 2)
            
            observeEvent(input$down, {
                if (!is.null(values$ped_id) & !is.null(input$id_num)) {
                  id <- paste0(values$ped_id, input$id_num)
                  name <- values$data$name[values$data$id == id]
                  changeOrder(values$pool, values$data, id, "down")
                  cat(file = stderr(), values$user, ":: reordering down member", name, "(", id, ")", "\n")
                }
            }, priority = 2)
            
            
            
            
            # S'indica que es mostri el pedigree, que s'anirà actualitzant cada vegada que s'actualitzi el 
            # data frame reactiu
            output$pedigree <- renderPlot({
                
                # Es guarda el data frame que conté la informació de la taula "members" amb el nom "df"
                df <- values$data
                
                # Si aquest data frame conté informació
                if (nrow(df) != 0) {
                
                  # Per tal de poder aplicar la funció "pedigree" del paquet kinship2modified, s'ha de definir
                  # primer la matriu que s'introduirà en l'argument "affected" d'aquesta funció. Es comença
                  # definint els següents vectors i llistes amb la informació dels fenotips i els seus colors, 
                  # densitats i angles.
                  phens <- c("phen1", "phen2", "phen3", "phen4")
                  phens_list <- list(df$phen1, df$phen2, df$phen3, df$phen4)
                  colors_list <- c(input$color1, input$color2, input$color3, input$color4)
                  density_list <- c(input$density1, input$density2, input$density3, input$density4)
                  angle_list <- c(input$angle1, input$angle2, input$angle3, input$angle4)
                  colors_show_list <- c("color1_show", "color2_show", "color3_show", "color4_show")
                  
                  # Es defineix el valor de n segons el nombre de fenotips introduits
                  if (input$show_num1 == 1) n <- 1
                  else if (input$show_num1 == 2) n <- 1:2
                  else if (input$show_num1 == 3) n <- 1:3
                  else if (input$show_num1 == 4) n <- 1:4
                  
                  # Per als fenotips que estiguin marcats en l'apartat "Mark the features to include in the pedigree"
                  # es genera una matriu amb els valors dels diferents fenotips per cada individu. També es guarden
                  # en un vector els colors, les densitats i els angles a utilitzar i a mostrar en la pestanya 
                  # "Customize pedigree".
                  t <- c(which(phens[n] %in% input$include1))
                  affected <- c()
                  colors <- c()
                  densities <- c()
                  angles <- c()
                  colors_show <- c()
                  for (i in t) {
                    affected <- cbind(affected, phens_list[[i]])
                    colors <- c(colors, colors_list[i])
                    angles <- c(angles, angle_list[i])
                    colors_show <- c(colors_show, colors_show_list[i])
                    if (density_list[i] == 1) densities <- c(densities, -1)
                    else densities <- c(densities, density_list[i])
                  }
                  
                  # Si "affected" està buit, es defineix com a un vector amb zeros.
                  if (length(affected) == 0) affected <- cbind(rep(0, nrow(df)))
                  
                  # Si "colors" està buit, s'iguala a 1
                  if (length(colors) == 0) colors <- 1
                  
                  # Si "densities" està buit, s'iguala a -1
                  if (length(densities) == 0) densities <- -1
                  
                  # Si "angles" està buit, s'iguala a 90
                  if (length(angles) == 0) angles <- 90
                  
                  # S'indica que es mostrin o s'amaguin els inputs per a seleccionar els colors, les densitats i 
                  # els angles en la pestanya "Customize pedigree"
                  for (i in 1:4){
                    if (colors_show_list[i] %in% colors_show) shinyjs::show(colors_show_list[i])
                    else hide(colors_show_list[i])
                  }
                  
                  # També s'ha de generar la matriu que conté les altres relacions entre els membres de la família.
                  # Es comença definint les següent variables
                  relations <- list(df$mono_twins, df$di_twins, df$unk_twins, df$marriage)
                  relations_num <- c(1, 2, 3, 4)
                  relations_matrix <- matrix(NA, ncol = 4, nrow = 0)
                  # Per a cadascuna de les possibles relacions, es comprova si hi ha algun valor diferent de NA i, si
                  # és així, s'introduiexen aquests valors en la matriu. 
                  for (i in 1:4) {
                    if (!all(is.na(relations[[i]]))) {
                      rel <- relations[[i]]
                      ids <- df$id[!is.na(rel)]
                      ids2 <- rel[!is.na(rel)]
                      rel_num <- rep(relations_num[i], length(ids))
                      ped_id <- rep(values$ped_id, length(ids))
                      mat <- matrix(c(ids, ids2, rel_num, ped_id), ncol = 4, byrow = FALSE)
                      relations_matrix <- rbind(relations_matrix, mat)
                    }
                  }
                  
                  # S'aplica la funció "pedigree" del paquet kinship2modified amb els valors corresponents
                  ped <- pedigree(famid = df$ped, id = df$id, momid = df$mother, dadid = df$father,
                                  sex = df$sex, status = df$status, affected = affected, relation = relations_matrix)
                  
                  # Per a posteriorment aplicar la funció "plot.pedigree" del paquet kinship2modified, s'han de definir
                  # més variables. Per començar es comprova si l'edat es vol afegir en el gràfic.
                  if ("age" %in% input$include2) age <- df$age
                  else age <- NULL
                  
                  # Es comprova també si es vol afegir el consultor i es defineix el valor que ha de prendre
                  if (sum(df$consultand) != 0 & "consultand" %in% input$include2) consultand <- df$id[df$consultand == 1]
                  else consultand <- NULL
                  
                  # També es defineix la matriu que contindrà la informació que es vol mostrar per a cada individu
                  info <- c()
                  info_list <- list(df$name, df$info1, df$info2, df$info3, df$info4)
                  info_names <- c("name", "info1", "info2", "info3", "info4")
                  for (i in 1:5) {
                    if (info_names[i] %in% input$include1 | info_names[i] %in% input$include2) {
                      info <- cbind(info, info_list[[i]])
                    }
                  }
                  
                  # Es comprova si hi ha algun individu adoptat
                  if (!all(df$adopted == "")) {
                    adopted <- df$adopted
                    a <- which(adopted == "")
                    adopted[a] <- NA
                  }
                  else adopted <- NULL
                  
                  # Es guarden tots els valors que s'inclouran en la funció "plot.pedigree" com a valors reactius
                  # per a poder-los reutilitzar després en la descàrrega de la imatge
                  values$ped <- ped
                  values$age <- age
                  values$number <- df$number
                  values$consultand <- consultand
                  values$info <- info
                  values$adopted <- adopted
                  values$colors <- colors
                  values$densities <- densities
                  values$angles <- angles
                  
                  # Es defineix el valor mínim que ha de tenir l'altura del gràfic en funció del nombre de
                  # generacions que té la família. Per a calcular-ho, s'utilitza la funció "kindepth" de kinship2
                  min_height <- max(kindepth(df$id, df$father, df$mother)) + 1
                  updateSliderInput(session, "height", min = min_height)
                  
                  # S'aplica la funció "plot.pedigree" del paquet kinship2modified amb les variables definides
                  # i els inputs introduits per l'usuari
                  plot.pedigree(ped[as.character(df$ped[1])], age = age, number = df$number, consultand = consultand, 
                                info = info, adopted = adopted,
                                cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                width = input$width, height = input$height, col = colors, density = as.numeric(densities),
                                angle = as.numeric(angles), pconnect = input$pconnect, branch = input$branch)
                }
            })
            
            
            
            # S'indica que es mostri o no la llegenda sota el gràfic segons indiqui l'usuari
            observe({
                if (input$show_legend == TRUE) shinyjs::show("legend_show")
                else hide("legend_show")
            })
            
            # Es mostra la llegenda del pedigree
            output$legend <- renderPlot({
                
                # Si hi ha mínim un membre en la família
                if (nrow(values$data) != 0) {
                  
                  # Si s'indica que es mostri la llegenda
                  if (input$show_legend == TRUE) {
                    
                    # Es guarden els noms dels fenotips d'interès en una variable reactiva
                    phen1 <- names(hpo)[hpo == input$phenotype1]
                    phen2 <- names(hpo)[hpo == input$phenotype2]
                    phen3 <- names(hpo)[hpo == input$phenotype3]
                    phen4 <- names(hpo)[hpo == input$phenotype4]
                    phens <- c("phen1", "phen2", "phen3", "phen4")
                    t <- c(which(phens %in% input$include1))
                    if (length(t) == 0) phen_labels <- NA
                    else phen_labels <- c(phen1, phen2, phen3, phen4)[t]
                    values$phen_labels <- phen_labels
                    
                    # S'aplica la funció ped.legend de kinshi2modified
                    ped.legend(values$ped, values$ped_id, values$adopted, phen.labels = values$phen_labels,
                               cex = input$legend_textsize, col = values$colors,
                               symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                               density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  }
                }
            })
            
            
            
            
            # S'indica què ha de passar al clicar el botó "Download pedigree"
            output$download_pedigree <- downloadHandler(
              
                # Es determina el nom que se li donarà a l'arxiu. Aquest començarà per PhenTree
                # seguit del nom del pedigree i la paraula "pedigree". L'extensió de l'arxiu dependrà 
                # del tipus d'imatge que s'ha indicat que es vol obtenir i del sistema operatiu 
                # (en Windows hi ha algunes extensions que són diferents)
                filename = function() {
                  name <- pedName(values$pool, values$ped_id)
                    
                  if (input$image_format == "png") file_ext <- ".png"
                  else if (input$image_format == "pdf") file_ext <- ".pdf"
                  else if (input$image_format == "jpeg" & input$os == "windows") file_ext <- ".jpg"
                  else if (input$image_format == "jpeg" & input$os == "others") file_ext <- ".jpeg"
                  else if (input$image_format == "tiff" & input$os == "windows") file_ext <- ".tif"
                  else if (input$image_format == "tiff" & input$os == "others") file_ext <- ".tiff"
                  
                  return(paste0("PhenTree_", name, "_pedigree", file_ext))
                },
                
                # Es defineix el contingut de la imatge tornant a aplicar la funció "plot.pedigree" amb 
                # els arguments que toqui (molts d'ells són els que s'han guardat anteriorment com a variables
                # reactives). Es guarda la imatge en el format indicat per l'usuari
                content = function(file) {
                  name <- as.character(pedName(values$pool, values$ped_id))
                  
                  if (input$image_format == "png") {
                    png(file, width = input$width_px, height = input$height_px, res = input$resolution)
                    plot.pedigree(values$ped[as.character(values$ped_id)], age = values$age, number = values$number, 
                                  consultand = values$consultand, info = values$info, adopted = values$adopted,
                                  cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                  width = input$width, height = input$height, col = values$colors, 
                                  density = as.numeric(values$densities), angle = as.numeric(values$angles), 
                                  pconnect = input$pconnect, branch = input$branch, mar = c(1, 1, 4.1, 1))
                    title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                      paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                          adj = 1, line = 1, cex.sub = input$text_size)
                    dev.off()
                    cat(file = stderr(), values$user, ":: downloading pedigree", name, "as PNG image", "\n")
                  }
                  
                  else if (input$image_format == "jpeg") {
                    jpeg(file, width = input$width_px, height = input$height_px, res = input$resolution)
                    plot.pedigree(values$ped[as.character(values$ped_id)], age = values$age, number = values$number, 
                                  consultand = values$consultand, info = values$info, adopted = values$adopted,
                                  cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                  width = input$width, height = input$height, col = values$colors, 
                                  density = as.numeric(values$densities), angle = as.numeric(values$angles), 
                                  pconnect = input$pconnect, branch = input$branch, mar = c(1, 1, 4.1, 1))
                    title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                      paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                          adj = 1, line = 1, cex.sub = input$text_size)
                    dev.off()
                    cat(file = stderr(), values$user, ":: downloading pedigree", name, "as JPEG image", "\n")
                  }
                  
                  else if (input$image_format == "tiff") {
                    tiff(file, width = input$width_px, height = input$height_px, res = input$resolution)
                    plot.pedigree(values$ped[as.character(values$ped_id)], age = values$age, number = values$number, 
                                  consultand = values$consultand, info = values$info, adopted = values$adopted,
                                  cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                  width = input$width, height = input$height, col = values$colors, 
                                  density = as.numeric(values$densities), angle = as.numeric(values$angles), 
                                  pconnect = input$pconnect, branch = input$branch, mar = c(1, 1, 4.1, 1))
                    title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                      paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                          adj = 1, line = 1, cex.sub = input$text_size)
                    dev.off()
                    cat(file = stderr(), values$user, ":: downloading pedigree", name, "as TIFF image", "\n")
                  }
                  
                  else if (input$image_format == "pdf") {
                    pdf(file, width = input$width_px/100, height = input$height_px/100)
                    plot.pedigree(values$ped[as.character(values$ped_id)], age = values$age, number = values$number, 
                                  consultand = values$consultand, info = values$info, adopted = values$adopted,
                                  cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                  width = input$width, height = input$height, col = values$colors, 
                                  density = as.numeric(values$densities), angle = as.numeric(values$angles), 
                                  pconnect = input$pconnect, branch = input$branch, mar = c(1, 1, 4.1, 1))
                    title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                      paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                          adj = 1, line = 1, cex.sub = input$text_size)
                    dev.off()
                    cat(file = stderr(), values$user, ":: downloading pedigree", name, "as PDF image", "\n")
                  }
                }
            )
            
            
            
            # S'indica què ha de passar al clicar el botó "Download legend"
            output$download_legend <- downloadHandler(
              
              # Es determina el nom que se li donarà a l'arxiu. Aquest començarà per PhenTree
              # seguit del nom del pedigree i la paraula "legend". L'extensió de l'arxiu dependrà 
              # del tipus d'imatge que s'ha indicat que es vol obtenir i del sistema operatiu 
              # (en Windows hi ha algunes extensions que són diferents)
              filename = function() {
                name <- pedName(values$pool, values$ped_id)
                
                if (input$image_format == "png") file_ext <- ".png"
                else if (input$image_format == "pdf") file_ext <- ".pdf"
                else if (input$image_format == "jpeg" & input$os == "windows") file_ext <- ".jpg"
                else if (input$image_format == "jpeg" & input$os == "others") file_ext <- ".jpeg"
                else if (input$image_format == "tiff" & input$os == "windows") file_ext <- ".tif"
                else if (input$image_format == "tiff" & input$os == "others") file_ext <- ".tiff"
                
                return(paste0("PhenTree_", name, "_legend", file_ext))
              },
              
              # Es defineix el contingut de la imatge tornant a aplicar la funció "ped.legend" amb 
              # els arguments que toqui (molts d'ells són els que s'han guardat anteriorment com a variables
              # reactives). Es guarda la imatge en el format indicat per l'usuari
              content = function(file) {
                name <- as.character(pedName(values$pool, values$ped_id))
                
                if (input$image_format == "png") {
                  png(file, width = input$width_px, height = input$height_px, res = input$resolution)
                  ped.legend(values$ped, values$ped_id, values$adopted, 
                             phen.labels = values$phen_labels,
                             cex = input$legend_textsize, col = values$colors,
                             symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                             density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                    paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                        adj = 1, line = 6, cex.sub = input$legend_textsize)
                  dev.off()
                  cat(file = stderr(), values$user, ":: downloading legend for", name, "as PNG image", "\n")
                }
                
                else if (input$image_format == "jpeg") {
                  jpeg(file, width = input$width_px, height = input$height_px, res = input$resolution)
                  ped.legend(values$ped, values$ped_id, values$adopted, 
                             phen.labels = values$phen_labels,
                             cex = input$legend_textsize, col = values$colors,
                             symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                             density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                    paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                        adj = 1, line = 6, cex.sub = input$legend_textsize)
                  dev.off()
                  cat(file = stderr(), values$user, ":: downloading legend for", name, "as JPEG image", "\n")
                }
                
                else if (input$image_format == "tiff") {
                  tiff(file, width = input$width_px, height = input$height_px, res = input$resolution)
                  ped.legend(values$ped, values$ped_id, values$adopted, 
                             phen.labels = values$phen_labels,
                             cex = input$legend_textsize, col = values$colors,
                             symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                             density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                    paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                        adj = 1, line = 6, cex.sub = input$legend_textsize)
                  dev.off()
                  cat(file = stderr(), values$user, ":: downloading legend for", name, "as TIFF image", "\n")
                }
                
                else if (input$image_format == "pdf") {
                  pdf(file, width = input$width_px/100, height = input$height_px/100)
                  ped.legend(values$ped, values$ped_id, values$adopted, 
                             phen.labels = values$phen_labels,
                             cex = input$legend_textsize, col = values$colors,
                             symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                             density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                    paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                        adj = 1, line = 6, cex.sub = input$legend_textsize)
                  dev.off()
                  cat(file = stderr(), values$user, ":: downloading legend for", name, "as PDF image", "\n")
                }
              }
            )
            
            
            
            # S'indica què ha de passar al clicar el botó "Download pedigree + legend"
            output$download_both <- downloadHandler(
              
              # Es determina el nom que se li donarà a l'arxiu. Aquest començarà per PhenTree
              # seguit del nom del pedigree. L'extensió de l'arxiu dependrà del tipus d'imatge que s'ha
              # indicat que es vol obtenir i del sistema operatiu (en Windows hi ha algunes extensions que són diferents)
              filename = function() {
                name <- pedName(values$pool, values$ped_id)
                
                if (input$image_format == "png") file_ext <- ".png"
                else if (input$image_format == "pdf") file_ext <- ".pdf"
                else if (input$image_format == "jpeg" & input$os == "windows") file_ext <- ".jpg"
                else if (input$image_format == "jpeg" & input$os == "others") file_ext <- ".jpeg"
                else if (input$image_format == "tiff" & input$os == "windows") file_ext <- ".tif"
                else if (input$image_format == "tiff" & input$os == "others") file_ext <- ".tiff"
                
                return(paste0("PhenTree_", name, file_ext))
              },
              
              # Es defineix el contingut de la imatge tornant a aplicar les funcions "plot.pedigree" i "ped.legend" amb 
              # els arguments que toqui (molts d'ells són els que s'han guardat anteriorment com a variables
              # reactives). Es guarda la imatge en el format indicat per l'usuari
              content = function(file) {
                name <- as.character(pedName(values$pool, values$ped_id))
                
                if (input$image_format == "png") {
                  png(file, width = input$width_px, height = input$height_px, res = input$resolution)
                  par(mfrow = c(2, 1))
                  plot.pedigree(values$ped[as.character(values$ped_id)], age = values$age, number = values$number, 
                                consultand = values$consultand, info = values$info, adopted = values$adopted,
                                cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                width = input$width, height = input$height, col = values$colors, 
                                density = as.numeric(values$densities), angle = as.numeric(values$angles), 
                                pconnect = input$pconnect, branch = input$branch, mar = c(1, 1, 4.1, 1))
                  ped.legend(values$ped, values$ped_id, values$adopted, 
                             phen.labels = values$phen_labels,
                             cex = input$legend_textsize, col = values$colors,
                             symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                             density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                    paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                        adj = 1, line = 6, cex.sub = input$text_size)
                  dev.off()
                  cat(file = stderr(), values$user, ":: downloading", name, "as PNG image", "\n")
                }
                
                else if (input$image_format == "jpeg") {
                  jpeg(file, width = input$width_px, height = input$height_px, res = input$resolution)
                  par(mfrow = c(2, 1))
                  plot.pedigree(values$ped[as.character(values$ped_id)], age = values$age, number = values$number, 
                                consultand = values$consultand, info = values$info, adopted = values$adopted,
                                cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                width = input$width, height = input$height, col = values$colors, 
                                density = as.numeric(values$densities), angle = as.numeric(values$angles), 
                                pconnect = input$pconnect, branch = input$branch, mar = c(1, 1, 4.1, 1))
                  ped.legend(values$ped, values$ped_id, values$adopted, 
                             phen.labels = values$phen_labels,
                             cex = input$legend_textsize, col = values$colors,
                             symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                             density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                    paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                        adj = 1, line = 6, cex.sub = input$text_size)
                  dev.off()
                  cat(file = stderr(), values$user, ":: downloading", name, "as JPEG image", "\n")
                }
                
                else if (input$image_format == "tiff") {
                  tiff(file, width = input$width_px, height = input$height_px, res = input$resolution)
                  par(mfrow = c(2, 1))
                  plot.pedigree(values$ped[as.character(values$ped_id)], age = values$age, number = values$number, 
                                consultand = values$consultand, info = values$info, adopted = values$adopted,
                                cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                width = input$width, height = input$height, col = values$colors, 
                                density = as.numeric(values$densities), angle = as.numeric(values$angles), 
                                pconnect = input$pconnect, branch = input$branch, mar = c(1, 1, 4.1, 1))
                  ped.legend(values$ped, values$ped_id, values$adopted, 
                             phen.labels = values$phen_labels,
                             cex = input$legend_textsize, col = values$colors,
                             symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                             density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                    paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                        adj = 1, line = 6, cex.sub = input$text_size)
                  dev.off()
                  cat(file = stderr(), values$user, ":: downloading", name, "as TIFF image", "\n")
                }
                
                else if (input$image_format == "pdf") {
                  pdf(file, width = input$width_px/100, height = input$height_px/100)
                  par(mfrow = c(2, 1))
                  plot.pedigree(values$ped[as.character(values$ped_id)], age = values$age, number = values$number, 
                                consultand = values$consultand, info = values$info, adopted = values$adopted,
                                cex = input$text_size, symbolsize = input$symbol_size, dist_text = input$text_dist, 
                                width = input$width, height = input$height, col = values$colors, 
                                density = as.numeric(values$densities), angle = as.numeric(values$angles), 
                                pconnect = input$pconnect, branch = input$branch, mar = c(1, 1, 4.1, 1))
                  ped.legend(values$ped, values$ped_id, values$adopted, 
                             phen.labels = values$phen_labels,
                             cex = input$legend_textsize, col = values$colors,
                             symbolsize = input$legend_symbolsize, width = input$legend_width, height = input$legend_height,
                             density = as.numeric(values$densities), angle = as.numeric(values$angles))
                  title(sub = paste(paste("Pedigree created by", values$user, sep = " "), 
                                    paste("on", Sys.time(), sep = " "), "using PhenTree", sep = "\n"), 
                        adj = 1, line = 6, cex.sub = input$text_size)
                  dev.off()
                  cat(file = stderr(), values$user, ":: downloading", name, "as PDF image", "\n")
                }
              }
            )
            
            
            
            
            # Es defineixen els títols que han de tenir les taules que es mostren en la pestanya "Genes and diseases"
            output$phenotype1_title <- renderText({ names(hpo[hpo == input$phenotype1]) })
            output$phenotype2_title <- renderText({ names(hpo[hpo == input$phenotype2]) })
            output$phenotype3_title <- renderText({ names(hpo[hpo == input$phenotype3]) })
            output$phenotype4_title <- renderText({ names(hpo[hpo == input$phenotype4]) })
            
            
            # Es defineix el contingut de la cada taula de la pestanya "Genes and diseases" utilitzant la 
            # funció definida a l'inici del script
            output$phenotype1_genes <- renderDataTable({
                tab <- createGenesTable(input$phenotype1, ptg, ann)
                datatable(tab, escape = FALSE, selection = "none", extensions = 'Buttons', 
                          options = list(buttons = c("csv", "excel"), dom = 'lBrtip',
                                         lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))))
            })
            
            output$phenotype2_genes <- renderDataTable({
                tab <- createGenesTable(input$phenotype2, ptg, ann)
                datatable(tab, escape = FALSE, selection = "none", extensions = 'Buttons', 
                          options = list(buttons = c("csv", "excel"), dom = 'lBrtip',
                                         lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))))
            })
            
            output$phenotype3_genes <- renderDataTable({
                tab <- createGenesTable(input$phenotype3, ptg, ann)
                datatable(tab, escape = FALSE, selection = "none", extensions = 'Buttons', 
                          options = list(buttons = c("csv", "excel"), dom = 'lBrtip',
                                         lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))))
            })
            
            output$phenotype4_genes <- renderDataTable({
                tab <- createGenesTable(input$phenotype4, ptg, ann)
                datatable(tab, escape = FALSE, selection = "none", extensions = 'Buttons', 
                          options = list(buttons = c("csv", "excel"), dom = 'lBrtip',
                                         lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))))
            })

            
            
            
            # S'indica que s'actualitzin cada 30 segons els paràmetres de la taula "pedigrees" de la base de dades
            # pel pedigree amb el que s'està treballant 
            execute <- reactiveTimer(30000)
            
            observe({
              execute()
              
              delay(20000, {
                if (!is.null(values$ped_id)) {
                  updateConfig(values$pool, values$ped_id, "n_phen", input$show_num1)
                  updateConfig(values$pool, values$ped_id, "n_info", input$show_num2)
                  updateConfig(values$pool, values$ped_id, "phen1_hpo", input$phenotype1)
                  updateConfig(values$pool, values$ped_id, "phen2_hpo", input$phenotype2)
                  updateConfig(values$pool, values$ped_id, "phen3_hpo", input$phenotype3)
                  updateConfig(values$pool, values$ped_id, "phen4_hpo", input$phenotype4)
                  updateConfig(values$pool, values$ped_id, "width", input$width)
                  updateConfig(values$pool, values$ped_id, "height", input$height)
                  updateConfig(values$pool, values$ped_id, "symbolsize", input$symbol_size)
                  updateConfig(values$pool, values$ped_id, "textsize", input$text_size)
                  updateConfig(values$pool, values$ped_id, "distance", input$text_dist)
                  updateConfig(values$pool, values$ped_id, "pconnect", input$pconnect)
                  updateConfig(values$pool, values$ped_id, "branch", input$branch)
                  updateConfig(values$pool, values$ped_id, "phen1_color", input$color1)
                  updateConfig(values$pool, values$ped_id, "phen1_density", input$density1)
                  updateConfig(values$pool, values$ped_id, "phen1_angle", input$angle1)
                  updateConfig(values$pool, values$ped_id, "phen2_color", input$color2)
                  updateConfig(values$pool, values$ped_id, "phen2_density", input$density2)
                  updateConfig(values$pool, values$ped_id, "phen2_angle", input$angle2)
                  updateConfig(values$pool, values$ped_id, "phen3_color", input$color3)
                  updateConfig(values$pool, values$ped_id, "phen3_density", input$density3)
                  updateConfig(values$pool, values$ped_id, "phen3_angle", input$angle3)
                  updateConfig(values$pool, values$ped_id, "phen4_color", input$color4)
                  updateConfig(values$pool, values$ped_id, "phen4_density", input$density4)
                  updateConfig(values$pool, values$ped_id, "phen4_angle", input$angle4)
                  updateConfig(values$pool, values$ped_id, "legend_width", input$legend_width)
                  updateConfig(values$pool, values$ped_id, "legend_height", input$legend_height)
                  updateConfig(values$pool, values$ped_id, "legend_symbolsize", input$legend_symbolsize)
                  updateConfig(values$pool, values$ped_id, "legend_textsize", input$legend_textsize)
                  
                  if (input$show_legend == TRUE) updateConfig(values$pool, values$ped_id, "show_legend", 1)
                  else updateConfig(values$pool, values$ped_id, "show_legend", 0)
                  
                  incl1 <- paste0(input$include1, collapse = "-")
                  updateConfig(values$pool, values$ped_id, "include1", incl1)
                  
                  incl2 <- paste0(input$include2, collapse = "-")
                  updateConfig(values$pool, values$ped_id, "include2", incl2)
                  
                  #cat(file = stderr(), values$user, ":: updating parameters and 'include' values for pedigree", values$ped_id, "\n")
                }
              })
            })
            
            

            # S'indica que es carreguin els valors de la taula "pedigrees" pel pedigree seleccionat 
            # i s'actualitzin els inputs cada vegada que es seleccioni un pedigree diferent
            observe({
                if (!is.null(values$ped_id)) {
                  df <- loadConfig(values$pool, values$ped_id)
                
                  updateNumericInput(session, "show_num1", value = df$n_phen)
                  updateNumericInput(session, "show_num2", value = df$n_info)
                  updateSelectInput(session, "phenotype1", selected = df$phen1_hpo)
                  updateSelectInput(session, "phenotype2", selected = df$phen2_hpo)
                  updateSelectInput(session, "phenotype3", selected = df$phen3_hpo)
                  updateSelectInput(session, "phenotype4", selected = df$phen4_hpo)
                  updateSliderInput(session, "width", value = df$width)
                  updateSliderInput(session, "height", value = df$height)
                  updateSliderInput(session, "symbol_size", value = df$symbolsize)
                  updateSliderInput(session, "text_size", value = df$textsize)
                  updateSliderInput(session, "text_dist", value = df$distance)
                  updateSliderInput(session, "pconnect", value = df$pconnect)
                  updateSliderInput(session, "branch", value = df$branch)
                  updateColourInput(session, "color1", value = df$phen1_color)
                  updateSelectInput(session, "density1", selected = df$phen1_density)
                  updateSelectInput(session, "angle1", selected = df$phen1_angle)
                  updateColourInput(session, "color2", value = df$phen2_color)
                  updateSelectInput(session, "density2", selected = df$phen2_density)
                  updateSelectInput(session, "angle2", selected = df$phen2_angle)
                  updateColourInput(session, "color3", value = df$phen3_color)
                  updateSelectInput(session, "density3", selected = df$phen3_density)
                  updateSelectInput(session, "angle3", selected = df$phen3_angle)
                  updateColourInput(session, "color4", value = df$phen4_color)
                  updateSelectInput(session, "density4", selected = df$phen4_density)
                  updateSelectInput(session, "angle4", selected = df$phen4_angle)
                  updateSliderInput(session, "legend_width", value = df$legend_width)
                  updateSliderInput(session, "legend_height", value = df$legend_height)
                  updateSliderInput(session, "legend_symbolsize", value = df$legend_symbolsize)
                  updateSliderInput(session, "legend_textsize", value = df$legend_textsize)
                  
                  if (df$show_legend == 1) updateCheckboxInput(session, "show_legend", value = TRUE)
                  else updateCheckboxInput(session, "show_legend", value = FALSE)
                  
                  cat(file = stderr(), values$user, ":: loading parameters for pedigree", values$ped_id, "\n")
                }
            }, priority = 3)
            
            
            # S'indica que es carreguin també els valors de l'apartat "Mark the features to include in the pedigree"
            # que s'han de carregar de manera diferent i amb un cert retràs
            observeEvent(input$working_ped, {
              delay(7000, {
                if (!is.null(values$ped_id)) {
                  df <- loadConfig(values$pool, values$ped_id)
                  
                  incl1 <- strsplit(df$include1, split = "-")[[1]]
                  updateCheckboxGroupInput(session, "include1", selected = incl1)
                  
                  incl2 <- strsplit(df$include2, split = "-")[[1]]
                  updateCheckboxGroupInput(session, "include2", selected = incl2)
                  
                  cat(file = stderr(), values$user, ":: loading 'include' values for pedigree", values$ped_id, "\n")
                }
              })
            }, priority = 2)
            

}





### S'executa l'aplicació ###

shinyApp(ui = ui, server = server)














