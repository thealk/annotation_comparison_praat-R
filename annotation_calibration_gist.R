#annotation_comparison gist
#
# This gist contains code for 1) a shiny app that allows you to click points in a plot that are associated with .textgrid/.wav files to be opened in Praat. It was designed to assist with visual inspection of Praat annotations.
#
# Assumptions:
#   - You have a set of .textgrids and accompanying .wav files with the same name, and can provide the directories where they live 
#   - You have Praat installed.
#   - Your textgrids contain intervals you would like to compare to a baseline interval.
#       - In my example, I have manually annotated VOT on one tier that is the "gold-standard". I have trained several RAs to annotate VOT on a subset of audio. I have merged their textgrids with mine to create interval boundaries that can be directly compared to the standard.
#   - You have a plot generated from your textgrid data. This is what will be fed to the shiny app.
#
# General steps:
#   - Read in your .textgrids in R as a data frame & make any modifications needed.
#   - Generate a plot from your textgrid data frame. This is what the shiny app will allow you to interact with.
#   - Run the shiny app




# Load libraries
library(shiny) # for interactive plotting
library(tidyverse)
#remotes::install_github("tjmahr/readtextgrid")
library(readtextgrid) # to read in textgrids

# Read in textgrids ----
# Use full path information
wav_dir <- "/Users/thea/Documents/GitHub/annotation_comparison_praat-R/wavs/"

tg_dir <- "/Users/thea/Documents/GitHub/annotation_comparison_praat-R/textgrids/"

# Generate a list of the textgrids
paths <- list.files(
  path = tg_dir, 
  pattern = "TextGrid$",
  full.names = TRUE, 
  recursive = FALSE
)

# Recursively read in your textgrids
tgs <- purrr::map_dfr(paths, readtextgrid::read_textgrid)
#View(tgs)

# ..Tidy your textgrids as needed ----

# Set up baseline and comparison data frames
# Baseline (Thea's original annotations; tier is labelled baseline)
baseline <- tgs %>% filter(tier_name=="baseline") %>% 
  mutate(id = paste(file,text,"_")) %>%
  select(id,xmin,xmax) %>%
  rename(xmin_baseline = xmin, xmax_baseline = xmax)

# Comparison (annotators)
comparison <- tgs %>%
  # Remove baseline
  filter(tier_name != "baseline") %>%
  # Remove additional unwanted tiers
  filter(!(tier_name %in% c("words","phones","XX","tk","EM","kh"))) %>%
  mutate(id = paste(file,text,"_")) %>%
  select(id,file,tier_name,xmin,xmax,text)

# Merge baseline, comparison & generate variable with boundary differences
# Name must be df for shinyApp to recognize
df <- comparison %>%
  merge(baseline,by="id") %>%
  # dif: if negative: annotator marked boundary later
  # dif: if positive: annotator marked boundary earlier
  mutate(dif_xmin = 1000*(xmin_baseline - xmin),
         dif_xmax = 1000*(xmax_baseline - xmax)
         ) %>%
  # We are only interested in VOT intervals
  filter(text %in% c(
    "P_vot","T_vot","K_vot",
    "B_vot","D_vot","G_vot"
  )) %>%
  # Grab the phoneme (first character in my text labels)
  mutate(phoneme = str_sub(text,1,1),
         voicing = case_when(
           phoneme %in% c("P","T","K") ~ "vcls",
           phoneme %in% c("B","D","G") ~ "vcd"
         ),
         place = case_when(
           phoneme %in% c("P","B") ~ "bilabial",
           phoneme %in% c("T","D") ~ "alveolar",
           phoneme %in% c("K","G") ~ "velar"
         ),
         annotator = tier_name
  ) %>%
  # Remove the extension .TextGrid from the file column
  mutate(filename = str_sub(file,1,-10)) %>%
  select(filename,annotator,phoneme,dif_xmin,voicing,place)
  
# View(df)

# Generate plot ----
# Name must be my_plot for shinyApp to recognize

my_plot <- df %>%
  # For the purposes of demonstration we are going to remove gross errors
  filter(abs(dif_xmin) < 100) %>%
  ggplot() +
  aes(x = place,
      y = dif_xmin,
      fill = voicing) +
  geom_violin(alpha = 0.5) +
  geom_jitter(aes(shape = annotator,
                  color = voicing),
              size=3) +
  theme_bw(base_size = 20) +
  geom_hline(yintercept=0) +
  # Create visual boundaries around +/- 5ms differences
  geom_hline(yintercept=5,linetype="dashed",color="red")+
  geom_hline(yintercept=-5,linetype="dashed",color="red")+
  facet_grid(voicing~place,
             scales = "free_x")+
  labs(y="Difference from baseline (miliseconds)")+
  ggtitle("VOT onset")
#my_plot

# Run shiny app ----
shinyApp(

  ui <- fluidPage(
    fluidRow(
      column(width = 3,
             h4("Click & drag to see info about points."),
             h4("Double click to open in Praat.")),
      column(width = 6,
             plotOutput("plot1", 
                        height = "500px",
                        width = "500px",
                        # Equivalent to: click = clickOpts(id = "plot_click")
                        click = "plot1_click",
                        dblclick = "plot1_dblclick",
                        brush = brushOpts(
                          id = "plot1_brush"
                        )
             )
      )
    ),
    fluidRow(
      column(width = 10,
             h4("Points in selection"),
             verbatimTextOutput("brush_info")
             ),
      column(width = 10,
             #h4("Points near click"),
             verbatimTextOutput("click_info")
      )
    )
    ),
  
  server <- function(input, output) {
    
    output$plot1 <- renderPlot({
      my_plot
    })
    
    output$click_info <- renderPrint({
      
      # Adjust threshold to allow more/less sensitivity relative to plot size
      click_threshold = 50
      
      # Display info for single clicks
      nearPoints(df, input$plot1_click,
                 addDist = TRUE,
                 threshold=click_threshold)
      
      # Open in Praat for double clicks
      tg <- paste0(tg_dir,
                   nearPoints(df, 
                              input$plot1_dblclick,
                              threshold=click_threshold,
                              addDist = TRUE)[1,'filename'],
                   ".TextGrid")
      wav <- paste0(wav_dir,
                    nearPoints(df,
                               input$plot1_dblclick,
                               threshold=click_threshold,
                               addDist = TRUE)[1,'filename'],
                    ".wav")
      
      system2("open", wav)
      system2("open", tg)
    
    })
    
    output$brush_info <- renderPrint({
      brushedPoints(df, input$plot1_brush)
    })
  }  
  
)
