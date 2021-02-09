# ImageMusicConversion
The source code (written in R) of my project investigating a way to make music from pixel data.

In the repository are the files necessary to run the MIDICSV / CSVMIDI programs (Fourmilab). Some are my property, some are not, but all are necessary to run my project files. The following files were created by me:

My Files:
 - Image.R
 - Music.R
 - Batch_FILE.bat
 - color_image_1.jpg (sample image)
 - countermeasures.csv (sample CSV file to convert to MIDI). Countermeasures is a 3-part piece (flute, clarinet, cello) I wrote in 2014.

Quick Instructions:
 1. Download all files
 2. Open Image.R
 3. Add an image of your choosing to the directory and run Image.R (via shell or directly in R Studio). The program converts the image to a CSV file.
 4. Run "Batch_FILE.bat". This will convert the CSV file created in Image.R to a MIDI file, using CSVMIDI.
 5. Listen to the MIDI file; optionally, open Music.R.
 6. Ensure that the program is reading the newly created CSV file (default: music_MIDI.csv). Run Music.R.
 7. View image and/or video output. It should have similar characteristics to the original image.
 8. Repeat steps 5-7 with any MIDI file of your choosing (3 MIDI channels maximum. If there are more than 3, the program uses the first 3. Add this MIDI file to the directory, and run MIDICSV.c to convert the MIDI file to a CSV file. Substitute this file name for the file in step 6.
 
 Questions? Contact me at eric-m-anderson@hotmail.com
 
