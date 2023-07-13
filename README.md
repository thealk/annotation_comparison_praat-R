# annotation_comparison_praat-R

The code here contains a shiny app that allows you to click points in a plot that are associated with .textgrid/.wav. It also allows you to open the files in Praat by double clicking on the points in the plot. It was designed to assist with visual inspection of Praat annotations.

## Assumptions:

  - You have a set of .textgrids and accompanying .wav files with the same name.
  - You have Praat installed and set as the default application for reading .TextGrid and .wav files (otherwise it will open these files in whatever your default system app is).
  - Your textgrids contain intervals you would like to compare to a baseline interval.
      - In this example, I have manually annotated VOT on one tier that is the "gold-standard". I have trained several RAs to annotate VOT on a subset of audio. I have merged their textgrids with mine to create interval boundaries that can be directly compared to the standard.
  - You have a plot generated from your textgrid data. This is what will be fed to the shiny app.
    - This example additionally contains code to read in textgrids and generate a plot to feed to Shiny

## General steps:
  - Read in your .textgrids in R as a data frame & make any modifications needed.
  - Generate a plot from your textgrid data frame. This is what the shiny app will allow you to interact with.
  - Run the shiny app.

  ![](annotation_calibration_example.mov)
