ui <- dashboardPage(skin='green',
    dashboardHeader(title='Create Metadata CSV File'),
    dashboardSidebar(includeHTML("analytics.html"),
        sidebarMenu(
            menuItem("User Inputs", tabName="userInputs", icon=icon("pen")),
            menuItem("Configure Plate Layout", tabName='plateConfig', icon=icon('th')),
            menuItem("Preview Metadata Table", tabName="outputs", icon=icon("table")),
            menuItem("OMIQ setup", tabName="omiq_setup", icon=icon("cog", lib= "glyphicon"))
        )
    ),
    dashboardBody(
        tabItems(
            ####### User Inputs Tab #######
            tabItem(tabName='userInputs',

                fluidRow(
                    box(shinyDirButton("filePath", "FCS File Directory", ""),
                        textInput("plateID", "Plate ID*", value="20191218-10299178-01"),
                        textInput("stainID", "Stain ID*", value="20191218-10299178"),
                        selectizeInput("sampleType", "Sample Type*",
                                    choices = NULL,
                                    options=list(create=TRUE)),
                        textInput("donorID", "Donor ID", value = 'DN22'),
                        selectizeInput("sampleSpecies", "Sample Species*",
                                    choices = NULL,
                                    options=list(create=TRUE)),
                        textInput("sampleStrain", "Sample Strain", value = "NA"),
                        selectizeInput("cytometer", "Cytometer*",
                                    choices = NULL,
                                    options=list(create=TRUE)),
                        p(em(h6('* indicates required field')))
                ))),


            ####### Configure Plate Layout Tab #######
            tabItem(tabName='plateConfig',
                    p(em(h6('* indicates required field'))),
                    fluidRow(
                        box(p(strong(h4("Titrations* (left to right on plate)"))),
                            column(1, numericInput('titrate_1', '', value=2, min=0, max=3, step=0.05)),
                            column(1, numericInput('titrate_2', '', value=1, min=0, max=3, step=0.05)),
                            column(1, numericInput('titrate_3', '', value=0.5, min=0, max=3, step=0.05)),
                            column(1, numericInput('titrate_4', '', value=0.25, min=0, max=3, step=0.05)),
                            column(1, numericInput('titrate_5', '', value=0.125, min=0, max=3, step=0.05)),
                            column(1, numericInput('titrate_6', '', value=0.06, min=0, max=3, step=0.05)),
                            column(1, numericInput('titrate_7', '', value=0.03, min=0, max=3, step=0.05)),
                            column(1, radioButtons('units', 'Units',choices=c('ug/test', 'ng/test'))),
                            width=12)

                    ),
                    fluidRow(
                        box(width=12,
                               fluidRow(
                                   column(COLUMN_ID, offset=OFFSET_ID+1, h3('Row A')),
                                   column(COLUMN_ID, h3('Row B')),
                                   column(COLUMN_ID, h3('Row C')),
                                   column(COLUMN_ID, h3('Row D')),
                                   column(COLUMN_ID, h3('Row E')),
                                   column(COLUMN_ID, h3('Row F')),
                                   column(COLUMN_ID, h3('Row G')),
                                   column(COLUMN_ID, h3('Row H'))

                           ),
                    fluidRow(
                       column(COLUMN_ID+1, h3('Control Row*')),
                       column(COLUMN_ID, selectizeInput('controlRow_A', '', choices=NULL, selected='NA')),
                       column(COLUMN_ID, selectizeInput('controlRow_B', '', choices=NULL, selected='NA')),
                       column(COLUMN_ID, selectizeInput('controlRow_C', '', choices=NULL, selected='NA')),
                       column(COLUMN_ID, selectizeInput('controlRow_D', '', choices=NULL, selected='NA')),
                       column(COLUMN_ID, selectizeInput('controlRow_E', '', choices=NULL, selected='NA')),
                       column(COLUMN_ID, selectizeInput('controlRow_F', '', choices=NULL, selected='NA')),
                       column(COLUMN_ID, selectizeInput('controlRow_G', '', choices=NULL, selected='NA')),
                       column(COLUMN_ID, selectizeInput('controlRow_H', '', choices=NULL, selected='NA'))
                    ),

                    fluidRow(
                           column(COLUMN_ID+1, h3('Target Species*')),
                           column(COLUMN_ID, selectizeInput('targetSpecies_A', '', choices=NULL)),
                           column(COLUMN_ID, selectizeInput('targetSpecies_B', '', choices=NULL)),
                           column(COLUMN_ID, selectizeInput('targetSpecies_C', '', choices=NULL)),
                           column(COLUMN_ID, selectizeInput('targetSpecies_D', '', choices=NULL)),
                           column(COLUMN_ID, selectizeInput('targetSpecies_E', '', choices=NULL)),
                           column(COLUMN_ID, selectizeInput('targetSpecies_F', '', choices=NULL)),
                           column(COLUMN_ID, selectizeInput('targetSpecies_G', '', choices=NULL)),
                           column(COLUMN_ID, selectizeInput('targetSpecies_H', '', choices=NULL))
                       ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Specificity*')),
                       column(COLUMN_ID, selectizeInput('specificity_A', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('specificity_B', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('specificity_C', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('specificity_D', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('specificity_E', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('specificity_F', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('specificity_G', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('specificity_H', '', choices=NULL, options=list(create=TRUE)))

                           ),

                    fluidRow(
                        column(COLUMN_ID+1, h3('Host Species*')),
                        column(COLUMN_ID, selectizeInput('hostSpecies_A', '', choices=NULL)),
                        column(COLUMN_ID, selectizeInput('hostSpecies_B', '', choices=NULL)),
                        column(COLUMN_ID, selectizeInput('hostSpecies_C', '', choices=NULL)),
                        column(COLUMN_ID, selectizeInput('hostSpecies_D', '', choices=NULL)),
                        column(COLUMN_ID, selectizeInput('hostSpecies_E', '', choices=NULL)),
                        column(COLUMN_ID, selectizeInput('hostSpecies_F', '', choices=NULL)),
                        column(COLUMN_ID, selectizeInput('hostSpecies_G', '', choices=NULL)),
                        column(COLUMN_ID, selectizeInput('hostSpecies_H', '', choices=NULL))

                            ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Isotype*')),
                       column(COLUMN_ID, selectizeInput('isotype_A', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('isotype_B', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('isotype_C', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('isotype_D', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('isotype_E', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('isotype_F', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('isotype_G', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('isotype_H', '', choices=NULL))

                           ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Clone*')),
                       column(COLUMN_ID, selectizeInput('clone_A', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('clone_B', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('clone_C', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('clone_D', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('clone_E', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('clone_F', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('clone_G', '', choices=NULL, options=list(create=TRUE))),
                       column(COLUMN_ID, selectizeInput('clone_H', '', choices=NULL, options=list(create=TRUE)))

                           ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Fluorochrome*')),
                       column(COLUMN_ID, selectizeInput('fluorochrome_A', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('fluorochrome_B', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('fluorochrome_C', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('fluorochrome_D', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('fluorochrome_E', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('fluorochrome_F', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('fluorochrome_G', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('fluorochrome_H', '', choices=NULL))

                           ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Parameter*')),
                       column(COLUMN_ID, selectizeInput('parameter_A', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('parameter_B', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('parameter_C', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('parameter_D', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('parameter_E', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('parameter_F', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('parameter_G', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('parameter_H', '', choices=NULL))

                           ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Batch Number')),
                       column(COLUMN_ID, textInput('batchNo_A', '', value=batch_nbr)),
                       column(COLUMN_ID, textInput('batchNo_B', '', value=batch_nbr)),
                       column(COLUMN_ID, textInput('batchNo_C', '', value=batch_nbr)),
                       column(COLUMN_ID, textInput('batchNo_D', '', value=batch_nbr)),
                       column(COLUMN_ID, textInput('batchNo_E', '', value=batch_nbr)),
                       column(COLUMN_ID, textInput('batchNo_F', '', value=batch_nbr)),
                       column(COLUMN_ID, textInput('batchNo_G', '', value=batch_nbr)),
                       column(COLUMN_ID, textInput('batchNo_H', '', value=batch_nbr))
                   ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Spec 1 Range*')),
                       column(COLUMN_ID, textInput('spec1range_A', '', value='20-30')),
                       column(COLUMN_ID, textInput('spec1range_B', '', value='20-30')),
                       column(COLUMN_ID, textInput('spec1range_C', '', value='20-30')),
                       column(COLUMN_ID, textInput('spec1range_D', '', value='20-30')),
                       column(COLUMN_ID, textInput('spec1range_E', '', value='20-30')),
                       column(COLUMN_ID, textInput('spec1range_F', '', value='20-30')),
                       column(COLUMN_ID, textInput('spec1range_G', '', value='20-30')),
                       column(COLUMN_ID, textInput('spec1range_H', '', value='20-30'))
                       # column(COLUMN_ID, textInput('spec1range', '', placeholder='e.g. 50-100')),

                           ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Spec 2 Range*')),
                       column(COLUMN_ID, textInput('spec2range_A', '', value='30-40')),
                       column(COLUMN_ID, textInput('spec2range_B', '', value='30-40')),
                       column(COLUMN_ID, textInput('spec2range_C', '', value='30-40')),
                       column(COLUMN_ID, textInput('spec2range_D', '', value='30-40')),
                       column(COLUMN_ID, textInput('spec2range_E', '', value='30-40')),
                       column(COLUMN_ID, textInput('spec2range_F', '', value='30-40')),
                       column(COLUMN_ID, textInput('spec2range_G', '', value='30-40')),
                       column(COLUMN_ID, textInput('spec2range_H', '', value='30-40'))
                       # column(COLUMN_ID, textInput('spec2range', '', placeholder='e.g. 50-100')),

                           ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Spec 3 Range*')),
                       column(COLUMN_ID, textInput('spec3range_A', '', value='40-50')),
                       column(COLUMN_ID, textInput('spec3range_B', '', value='40-50')),
                       column(COLUMN_ID, textInput('spec3range_C', '', value='40-50')),
                       column(COLUMN_ID, textInput('spec3range_D', '', value='40-50')),
                       column(COLUMN_ID, textInput('spec3range_E', '', value='40-50')),
                       column(COLUMN_ID, textInput('spec3range_F', '', value='40-50')),
                       column(COLUMN_ID, textInput('spec3range_G', '', value='40-50')),
                       column(COLUMN_ID, textInput('spec3range_H', '', value='40-50'))
                       # column(COLUMN_ID, textInput('spec3range', '', placeholder='e.g. 50-100')),

                           ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Gating Method*')),
                       column(COLUMN_ID, selectizeInput('gatingMethod_A', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('gatingMethod_B', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('gatingMethod_C', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('gatingMethod_D', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('gatingMethod_E', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('gatingMethod_F', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('gatingMethod_G', '', choices=NULL)),
                       column(COLUMN_ID, selectizeInput('gatingMethod_H', '', choices=NULL))

                           ),

                   fluidRow(
                       column(COLUMN_ID+1, h3('Gating Argument*')),
                       column(COLUMN_ID, numericInput('gatingArg_A', '', value= 3, min=0, max=5)),
                       column(COLUMN_ID, numericInput('gatingArg_B', '', value= 3, min=0, max=5)),
                       column(COLUMN_ID, numericInput('gatingArg_C', '', value= 3, min=0, max=5)),
                       column(COLUMN_ID, numericInput('gatingArg_D', '', value= 3, min=0, max=5)),
                       column(COLUMN_ID, numericInput('gatingArg_E', '', value= 3, min=0, max=5)),
                       column(COLUMN_ID, numericInput('gatingArg_F', '', value= 3, min=0, max=5)),
                       column(COLUMN_ID, numericInput('gatingArg_G', '', value= 3, min=0, max=5)),
                       column(COLUMN_ID, numericInput('gatingArg_H', '', value= 3, min=0, max=5))

                           )

                    )
                )


        ),

        ####### Metadata Table Output Tab #######
        tabItem(tabName='outputs',

                # Button
                actionButton('createTable', 'Create Metadata Table'),
                downloadButton("downloadData", "Download Metadata CSV File"),
                # Metadata Table Preview Table
                DT::dataTableOutput('finalTable')
            ),

        ###### OMIQ setup ######
        tabItem(tabName='omiq_setup',

                # button to push to R-OMIQ
                actionButton('pushData', 'Push Metadata to OMIQ'),
                actionButton('rerun', 'Re-run'),
                uiOutput('uiStatus'),

                fluidRow(
                        column(OMIQ_COLUMN, textInput('Pop1', 'Pop1', value='Lymph')),
                        column(OMIQ_COLUMN, textInput('Pop2', 'Pop2', value='Mono')),
                        column(OMIQ_COLUMN, textInput('Pop3', 'Pop3', value='Gran')),
                        width=12),
                fluidRow(
                        column(OMIQ_COLUMN, numericInput('PopCount1', 'PopCount1', value=2000, min=100, max=5000, step=100)),
                        column(OMIQ_COLUMN, numericInput('PopCount2', 'PopCount2', value=300, min=100, max=3000, step=100)),
                        column(OMIQ_COLUMN, numericInput('PopCount3', 'PopCount3', value=1500, min=100, max=10000, step=100)),
                        width=12),
                fluidRow(
                        column(OMIQ_COLUMN, colourInput('col1', 'Pop1 colour', "#fc1828ff")),
                        column(OMIQ_COLUMN, colourInput('col2', 'Pop2 colour', "#fd8628ff")),
                        column(OMIQ_COLUMN, colourInput('col3', 'Pop3 colour', "#2983ffff")),
                        width=12),
                hr(),
                sliderInput("biexSlider", label = h3("biex value"), min = -1000, max = -1, value = -300)
                )
    )


))

