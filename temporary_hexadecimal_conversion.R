
#Trying to figure out the whole hexadecimals thing

#first I will try to figure out how to transform the hexadecimal values to decimals.
# Sample hexadecimal stimulus codes
hex_stimulus_codes <- c("1F", "20", "2A", "3B")

# Convert hexadecimal to decimal
decimal_stimulus_codes <- as.integer(hex_stimulus_codes, base = 16)

# Print the results
print(decimal_stimulus_codes)

# Sample hexadecimal stimulus codes
hex_stimulus_codes <- c("1F", "20", "2A", "3B")

# Convert hexadecimal to decimal using a loop
decimal_stimulus_codes <- numeric(length(hex_stimulus_codes))
for (i in seq_along(hex_stimulus_codes)) {
  decimal_stimulus_codes[i] <- as.integer(hex_stimulus_codes[i], base = 16)
}

# Print the results
print(decimal_stimulus_codes)


# Sample hexadecimal stimulus codes
hex_stimulus_codes <- c("1F", "41", "42", "3B")

# Convert hexadecimal to decimal
decimal_stimulus_codes <- as.integer(hex_stimulus_codes, base = 16)

# Print the results
print(decimal_stimulus_codes)


print(hex_stimulus_codes)


#Let's just make a variable filled with all the hexadecimals we have from the presentation log file.
hexadecimal_codes <- c("4B", "41", "4C", "47", "55", "33", "22", "40", "49", "24", "37", "20",  "1F", "42", "34", "4A", "3F", "51", "38", "54", "2E", "3E", "53", "35", "29", "2D", "52", "36", "23", "21", "3D", "48")

hexadecimal_codes %>%
  4B==75
print hexadecimal_codes




# Sample decimal stimulus codes
decimal_stimulus_codes <- c(31, 32, 42, 59)

# Convert decimal to hexadecimal
hex_stimulus_codes <- sprintf("%02X", decimal_stimulus_codes)

# Print the results
print(hex_stimulus_codes)



# Sample hexadecimal stimulus codes
hex_stimulus_codes <- c("1F", "20", "2A", "3B")

# Convert hexadecimal to decimal
decimal_stimulus_codes <- as.integer(hex_stimulus_codes, base = 16)

# Print the results
print(decimal_stimulus_codes)



#The following actually works!
# Sample hexadecimal stimulus codes
hex_stimulus_codes <- c("1F", "20", "2A", "3B")

# Convert hexadecimal to decimal using strtoi
decimal_stimulus_codes <- strtoi(hex_stimulus_codes, base = 16)

# Print the results
print(decimal_stimulus_codes)



#Let's do it with our real values!
#create a variable with all the hexadecimal values from the stim file. 
hexa_stim_codes <- c("4B", "41", "4C", "47", "55", "33", "22", "40", "49", "24", "37", "20",  "1F", "42", "34", "4A", "3F", "51", "38", "54", "2E", "3E", "53", "35", "29", "2D", "52", "36", "23", "21", "3D", "48")

# Convert hexadecimal to decimal using strtoi
decimal_stim_codes <-  strtoi(hexa_stim_codes, base = 16)

# Print the results
print(decimal_stim_codes)



# read in stim file
stimData <- suppressWarnings(
  read_tsv(stim.File, skip = 3, col_types = '__c_n__d__d_____')) %>%
  rename('StimCode' = strtoi(`Pic(num)`, base=16) %>% 
           mutate(Start.sec = Time/10000,
                  Duration.sec = Duration/10000,
                  End.sec = Start.sec + Duration.sec ) %>% 
           filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode) 

         
         # read in stim file
         stimData <- suppressWarnings(
           read_tsv(stim.File, skip = 2, col_types = '__c_n__d__d_____')) %>%
           rename('StimCode' = strtoi(`Pic(num)`, base=16)) %>% 
           mutate(Start.sec = Time/10000,
                  Duration.sec = Duration/10000,
                  End.sec = Start.sec + Duration.sec ) %>% 
           filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode) 
         
         
         
         # read in stim file
         stimData <- suppressWarnings(
           read_tsv(stim.File, skip = 2, col_types = '__c_n_c_n____d__c_d_c_d__c__')) %>%
           rename('StimCode' = strtoi('Pic(num)', base=16)) %>% 
           mutate(Start.sec = Time/10000,
                  Duration.sec = Duration/10000,
                  End.sec = Start.sec + Duration.sec ) %>% 
           filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode)          
         
         
         # read in stim file
         stimData <- suppressWarnings(
           read_tsv(stim.File, skip = 2, col_types = '__c_n_c_n____d__c_d_c_d__c__')) %>%
           rename('HexaStimCode' = 'Pic(num)') %>% 
           'DeciStimCode' = strtoi(HexaStimCode) %>%
           mutate(Start.sec = Time/10000,
                  Duration.sec = Duration/10000,
                  End.sec = Start.sec + Duration.sec ) %>% 
           filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode)  

         # read in stim file
         stimData <- suppressWarnings(
           read_tsv(stim.File, skip = 2, col_types = '__c_n_c_n____d__c_d_c_d__c__')) %>%
           mutate('StimCode' = strtoi(`Pic(num)`, base=16)%>% 
                    filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode) 
         
         print(head(stimData))         
         
         
         
         , 
         Start.sec = Time/1000,
         Duration.sec = Duration/1000,
         End.sec = Start.sec + Duration.sec ) 
         
         
         
         # read in stim file
         stimData <- suppressWarnings(
           read_tsv(stim.File, skip = 2, col_types = '__c_n_c_n____d__c_d_c_d__c__')) %>%
           mutate('StimCode' = strtoi(`Pic(num)`, base=16)),
                  Start.sec = Time/1000,
                  Duration.sec = Duration/1000,
                  End.sec = Start.sec + Duration.sec) %>% 
           filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode) 
         
         print(head(stimData))
         
         
         stimData <- suppressWarnings(
           read_tsv(stim.File, skip = 2, col_types = '__c_n_c_n____d__c_d_c_d__c__')) %>%
           rename('StimCode' = `Pic(num)`)
         
         print(head(stimData))
         
         
         %>% 
           mutate(Start.sec = Time/10000,
                  Duration.sec = Duration/10000,
                  End.sec = Start.sec + Duration.sec ) %>% 
           filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode)
         
         
         #Let's try just the first part and printing it. 
         stimDataNew <- suppressWarnings(
           read_tsv(stim.File, skip = 2, col_types = '__c_n_c_n_c_n__d__c_d_c_d__c__')) %>%
           mutate('StimCode' = strtoi(`Pic(num)`, base=16))
         
         print(head(stimDataNew))
         
         
         stimData2 <- read_tsv(stim.File, skip = 2)
         
         #This works!!! :D
         stimDataNew2 <- suppressWarnings(
           read_tsv(stim.File, skip = 2)) %>%
           mutate('StimCode' = strtoi(`Pic(num)`, base=16))
         
         stimDataNew3 <- suppressWarnings(
           read_tsv(stim.File, skip = 2)) %>%
           mutate('StimCode' = strtoi(`Pic(num)`, base=16),
                  Start.sec = Time/10000,
                  Duration.sec = Duration/10000,
                  End.sec = Start.sec + Duration.sec ) %>% 
           filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != 0)
         
         stimData <- suppressWarnings(
           read_tsv(stim.File, skip = 2)) %>% #in real one, stim.File should be changed to stimFile.
           mutate('StimCode' = strtoi(`Pic(num)`, base=16),
                  Start.sec = Time/10000, #not sure why we have to divide by 10 000 rather than 1000 but that seems to be the right way to go. 
                  Duration.sec = Duration/10000,
                  End.sec = Start.sec + Duration.sec ) %>% 
           filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != 0) #in real one 0 should be changed to offCode
         
         
         View(stimDataNew4)
         