# Preprocess-fEMG

This code is for pre-processing facial EMG data captured with a Biopac system running AcqKnowledge software. It does the following things:
 * read raw data exported from AcqKnowledge (details below)
 * clean up a stimulus marker channel 
 * perform basic validation of stimulus code labelling of the data
 * apply useful labels to the data including trial number, pre-stimulus and stimulus phases
 * perform automatic artifact rejection based on https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0084053

## Examples
Download example data
 * raw fEMG data files: https://osf.io/tc32g/download
 * stimulus presentation log files: https://osf.io/np4c6/download

Put these files in the folders 
 * "example_experiment/1 raw data" and 
 * "example_experiment/0 stim sequences"
 
To see how it works, look at example_single_session.R for a single session, or example_experiment for a whole experiment, and read the comments

## Data format

To use with your own data, export the data from AcqKnowledge with "File" > "Save As..." 

In the "Save as type" drop-down, choose "Text (*.txt *.csv)"

![Save as type](documentation/Readme1_TextType.png?raw=true)

In the export options, tick "Include header" and choose "Delimiter: comma" (tab delimiter is also possible, you specify when calling "read_acq_text()").

![Export options](documentation/Readme2_ExportOptions.png?raw=true)

Wait for it to export, and save it somewhere handy.
