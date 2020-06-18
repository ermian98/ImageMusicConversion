# Example command in the shell: Rscript.exe "Image.R" "color_image_1.jpg" "columns" 145 24 1 1 1 1
# Convert image to music!
# If you get an error, please try running the code again.

start.time <- Sys.time()
options(scipen=999999999)
library(magrittr) # these libraries are required; please download.
library(imager) # imager is really interesting. Check out the documentation: https://dahtah.github.io/imager/imager.html
args = commandArgs(trailingOnly=TRUE)
########### Arguments: ###########

im <- load.image("color_image_1.jpg") # input image

statement <- "rows" # "rows", "columns", "circles in", "circles out"
# If "columns", each row is a "movement". Notes traverse the row . If "rows", each column is a "movement". Notes traverse the column.
# "circles out" means pixels play from center outward in a square radial pattern; "circles in" goes opposite direction

row_len <- 60 # how many COLUMNS are we sampling (for both, will grab equidistant; e.g. 1st, 31st, etc., AND first+last ones)

col_len <- 47 # how many ROWS are we sampling? meaning length of column vector.

tonal <- 0.5 # assign all notes to a key signature (1), assign some based on RGB similarity (0.5), or keep "atonal" (0)

t_ON <- 1 # Keep tempo constant throughout the MIDI file (0) or change it per column (1)

grayVolume_ON <- 1 # random volumes (0) instead of based on grayscale value (1) 
                                     # (1) means all instruments are the same volume, since grayscale represents all color channels

click_ON <- 1 # add metronome to midi file (1) or not (0)

################################### Read image data, prepare data frames

if (length(args)==0) {
  stop("At least one argument must be supplied", call.=FALSE)
}
if(statement == "rows") { # Rotate the image around its center to swap interpretation of columns vs rows
  im <- imrotate(im,90) 
} 
red_im <- R(im) ; blue_im <- B(im) ; green_im <- G(im) # display red/blue/green bands of image
gray_im <- grayscale(im) # display grayscale
orig_row <- length(imrow(red_im,1)) # dimensions of image # display red/blue/green bands of image
orig_col <- length(imcol(red_im,1)) #

if (col_len > orig_col-1 | row_len > orig_row-1) {
  if (statement != "rows") {
    stop(paste("Image has only:", orig_row-1, "columns and", orig_col-1, "rows."), call.= FALSE)
  } else {stop(paste("Image has only:", orig_col-1, "columns and", orig_row-1, "rows."), call.= FALSE)}
}
row_breaks <- orig_row/(row_len+1) # Number of columns between the equidistant columns we are sampling
col_breaks <- orig_col/(col_len+1) # Same for rows

red_colors <- matrix(nrow = col_len+1, ncol = row_len+1) # initialize matrix that'll store MIDI note data by color band
blue_colors <- matrix(nrow = col_len+1, ncol = row_len+1) #
green_colors <- matrix(nrow = col_len+1, ncol = row_len+1) #
all_color_sum <- matrix(nrow = col_len+1, ncol = row_len+1) # grayness proportion

# (28,92), (32,89), (36-87), (40-82), (44-80), (45-77), (47-75) # randomly pick MIDI note value range. FEEL FREE TO MODIFY.
scaling <- list(c(112,4), c(144,4.5), c(180,5), c(238,6), c(307,7), c(360,8), c(423,9)) 
scaling_sample <- sample(scaling,1)
samp1 <- scaling_sample[[1]][1] #
samp2 <- scaling_sample[[1]][2] # This assumes only Piano MIDI voices, can be altered in "instr_list" below.

################################### create MIDI note values based on pixel color.
# 1.) Extract R/G/B color data. 
# 2.) Divide its R/G/B value by 255
# 3.) Scale these values (0 - 255) into reasonable MIDI range (see lines 54-58).
#     Calcuation: Add samp1, then divide by samp2. This yields a reasonable integer range. Sample multiple ranges to have different ranges per voice.
# 4.) Assign them to the red/blue/green_colors matrices, which stores the notes that correspond to the pixels we extracted.
# 5.) The "all_color_sum" matrix holds % grayscale values - close to 1.00 means cell is more white (bright).

if (row_len > orig_row - 9 || col_len > orig_col - 9) {
  for (l in 1:(col_len+1)) {
    red_colors[l,] <- round((samp1 + round(255*(imrow(red_im,(col_breaks*l))[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
    green_colors[l,] <- round((samp1 + round(255*(imrow(green_im,(col_breaks*l))[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
    blue_colors[l,] <- round((samp1 + round(255*(imrow(blue_im,(col_breaks*l))[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
    all_color_sum[l,] <- round(imrow(gray_im,(col_breaks*l))[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)], digits = 2)
  }
} else {
   ave_red <- matrix(nrow = 9, ncol = row_len+1) # averages color of all adjacent cells (3x3 neighbors). Only does this if breaks between
   ave_green <- matrix(nrow = 9, ncol = row_len+1) # extracted rows are greater than 3.
   ave_blue <- matrix(nrow = 9, ncol = row_len+1)
   
   red_colors[1,] <- round((samp1 + round(255*(imrow(red_im,1)[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
   green_colors[1,] <- round((samp1 + round(255*(imrow(green_im,1)[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
   blue_colors[1,] <- round((samp1 + round(255*(imrow(blue_im,1)[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
   all_color_sum[1,] <- round(imrow(gray_im,1)[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)], digits = 2)
   
   for (l in 2:col_len) {
     for (m in c(0,1,2)) {
       for (n in c(0,1,2)) {
         ave_red[3*m + n+1,] <- round((samp1 + round(255*(imrow(red_im,(col_breaks*(l-1)+m))[c(seq(n, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
         ave_green[3*m + n+1,] <- round((samp1 + round(255*(imrow(green_im,(col_breaks*(l-1)+m))[c(seq(n, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
         ave_blue[3*m + n+1,] <- round((samp1 + round(255*(imrow(blue_im,(col_breaks*(l-1)+m))[c(seq(n, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
       }
     }
     red_colors[l,] <- round(colMeans(ave_red))
     green_colors[l,] <- round(colMeans(ave_green))
     blue_colors[l,] <- round(colMeans(ave_blue))
     all_color_sum[l,] <- round(imrow(gray_im,(col_breaks*(l-1)+1))[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)], digits = 2)
   }
   red_colors[col_len+1,] <- round((samp1 + round(255*(imrow(red_im,orig_col)[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
   green_colors[col_len+1,] <- round((samp1 + round(255*(imrow(green_im,orig_col)[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
   blue_colors[col_len+1,] <- round((samp1 + round(255*(imrow(blue_im,orig_col)[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)])))/samp2)
   all_color_sum[col_len+1,] <- round(imrow(gray_im,orig_col)[c(seq(1, by = row_breaks, row_len*row_breaks),orig_row)], digits = 2)
}

################################### create grid that shows what pixels were extracted and converted into notes.
if (orig_col*orig_row > 1250000) { 
  color.at(im,c(1,2,seq(row_breaks-1, by = row_breaks, row_len*row_breaks),
                seq(row_breaks, by = row_breaks, row_len*row_breaks),
                seq(row_breaks+1, by = row_breaks, row_len*row_breaks),orig_row-1, orig_row),
           c(1,2,seq(col_breaks-1, by = col_breaks, col_len*col_breaks),
             seq(col_breaks, by = col_breaks, col_len*col_breaks),
             seq(col_breaks+1, by = col_breaks, col_len*col_breaks),orig_col-1, orig_col)) <- c(3,3,3) # if large picture, represent pixels with
} else {                                                                                               # a 3x3 square for visiblility.
  color.at(im,c(seq(1, by = row_breaks, row_len*row_breaks),orig_row),  #
              c(seq(1, by = col_breaks, col_len*col_breaks),orig_col)) <- c(3,3,3) }

if (statement == "rows") { 
  save.image(imrotate(im,-90),"Rplots.png")
} else {
  save.image(im,"Rplots.png") # save this grid to a PNG file for viewing
}

if (statement != "circles in" & statement != "circles out") {
  red_colors <- t(red_colors) # transpose matrices to accomodate the eventual MIDI file. "Circles .." doesn't require transpose.
  green_colors <- t(green_colors)
  blue_colors <- t(blue_colors)
  all_color_sum <- t(all_color_sum)
}
if (statement == "rows") { # reversing the matrices; if we don't do this, rows are traversed bottom to top instead of top to bottom
  red_colors <- red_colors[,ncol(red_colors):1]
  green_colors <- green_colors[,ncol(green_colors):1]
  blue_colors <- blue_colors[,ncol(blue_colors):1]
  all_color_sum <- all_color_sum[,ncol(all_color_sum):1]
}
# write.table(all_color_sum, file = "all_color_sum.csv", sep = ",", # creates .csv file for the grayscale values (optional)
#            row.names = FALSE, col.names = FALSE)

################################### Prepare .csv file that can be converted to a .mid file via csvmidi.c.
d = 3 # number of tracks (14 max)
instr_list1 <- c(rep("Piano", d),"Woodblock") # needs to be at least length (d - instruments used)
instr_list <- sapply(strsplit(instr_list1, '[, ]+'), function(x) toString(dQuote(x))) # for printing later
instr_num <- c(1,1,1,116) # corresponding MIDI instrument
chan_list <- c(1,2,3,4,5,6,7,8,10,11,12,13,14,15) # skip channel 9 (dedicated to percussion)

num_row <- col_len+1 # same as col_len, adding 1 to get border
num_col <- row_len+1 # same as row_len, adding 1 to get border

colors_to_make_music <- matrix(nrow=num_col*num_row, ncol=d+grayVolume_ON)  # variable can be any color or row combo !!!
all_reds <- c() ; all_blues <- c() ; all_greens <- c() ; all_grays <- c() # lists that will ultimately queue the pixels in playing order

################################### IF a circular path was chosen, this algorithm orders the pixels accordingly (sets up the circle path for pixels to follow)
tracker <- c()
if (statement == "circles in" || statement == "circles out") { 
  max_dim <- max(num_col, num_row)
  if (max_dim%%2 == 0) {
    max_dim <- max_dim + 1
  }
  aa <- ceiling((max_dim - num_row)/2)
  add_above <- matrix(nrow = aa, ncol = num_col)
  ab <- floor((max_dim - num_row)/2)
  add_below <- matrix(nrow = ab, ncol = num_col)
  red_colors <- rbind(add_above, red_colors, add_below)
  green_colors <- rbind(add_above, green_colors, add_below)
  blue_colors <- rbind(add_above, blue_colors, add_below)
  all_color_sum <- rbind(add_above, all_color_sum, add_below)
  
  ar <- ceiling((max_dim - num_col)/2)
  add_right <- matrix(nrow = length(red_colors[,1]), ncol = ar)
  al <- floor((max_dim - num_col)/2)
  add_left <- matrix(nrow = length(red_colors[,1]), ncol = al)
  red_colors <- cbind(add_left, red_colors, add_right)
  green_colors <- cbind(add_left, green_colors, add_right)
  blue_colors <- cbind(add_left, blue_colors, add_right)
  all_color_sum <- cbind(add_left, all_color_sum, add_right) # all of this extends red/blue/green_colors to square shape for circular traversal
                                                             # NA values are skipped (lines 231-234).
  new_num_row <- length(red_colors[1,])
  new_num_col <- length(red_colors[,1])
  center <- ceiling((new_num_row/2)) + floor((new_num_col/2))*new_num_row

  all_reds[1] <- red_colors[center]
  all_greens[1] <- green_colors[center]
  all_blues[1] <- blue_colors[center]
  all_grays[1] <- all_color_sum[center]
  tracker <- c(tracker,center)
  for (x in 1:floor((0.5*new_num_row))) { # down direction (start from center)
    r2 <- x
    for (y in x:1) {
      if (((center + (x+1-y) - (x-1)*new_num_row) != 0) & !((center + (x+1-y) - (x-1)*new_num_row) %in% tracker)) {
        all_reds <- c(all_reds, red_colors[center + (x+1-y) - (x-1)*new_num_row])
        all_greens <- c(all_greens, green_colors[center + (x+1-y) - (x-1)*new_num_row])
        all_blues <- c(all_blues, blue_colors[center + (x+1-y) - (x-1)*new_num_row])
        all_grays <- c(all_grays, all_color_sum[center + (x+1-y) - (x-1)*new_num_row])
        tracker <- c(tracker,(center + (x+1-y) - (x-1)*new_num_row))
      }
    }
    for (y in 1:(2*x-1)) { # right direction
      if (((center + x + (y-(x-1))*new_num_row) != 0) & !((center + x + (y-(x-1))*new_num_row) %in% tracker)) {
        all_reds <- c(all_reds, red_colors[center + x + (y-(x-1))*new_num_row])
        all_greens <- c(all_greens, green_colors[center + x + (y-(x-1))*new_num_row])
        all_blues <- c(all_blues, blue_colors[center + x + (y-(x-1))*new_num_row])
        all_grays <- c(all_grays, all_color_sum[center + x + (y-(x-1))*new_num_row])
        tracker <- c(tracker,(center + x + (y-(x-1))*new_num_row))
      } }
    for (y in 1:(2*x)) { # up direction
      if (((center + (x-y) + x*new_num_row) != 0) & !((center + (x-y) + x*new_num_row) %in% tracker)) {
        all_reds <- c(all_reds, red_colors[center + (x-y) + x*new_num_row])
        all_greens <- c(all_greens, green_colors[center + (x-y) + x*new_num_row])
        all_blues <- c(all_blues, blue_colors[center + (x-y) + x*new_num_row])
        all_grays <- c(all_grays, all_color_sum[center + (x-y) + x*new_num_row])
        tracker <- c(tracker,(center + (x-y) + x*new_num_row))
      } }
    for (y in 1:(2*x)) { # left direction
      if (((center - x - (y-x)*new_num_row) != 0) & !((center - x - (y-x)*new_num_row) %in% tracker)) {
        all_reds <- c(all_reds, red_colors[center - x - (y-x)*new_num_row])
        all_greens <- c(all_greens, green_colors[center - x - (y-x)*new_num_row])
        all_blues <- c(all_blues, blue_colors[center - x- (y-x)*new_num_row])
        all_grays <- c(all_grays, all_color_sum[center - x - (y-x)*new_num_row])
        tracker <- c(tracker,(center - x - (y-x)*new_num_row))
      }}
    if (center != round((new_num_row*new_num_col)/2)) { # remaining down direction
      r2 <- 2*x - 1
    }
    for (y in 1:r2) {
      if (((center - (x-y) - x*new_num_row) != 0) & !((center - (x-y) - x*new_num_row) %in% tracker)) {
        all_reds <- c(all_reds, red_colors[center - (x-y) - x*new_num_row])
        all_greens <- c(all_greens, green_colors[center - (x-y) - x*new_num_row])
        all_blues <- c(all_blues, blue_colors[center - (x-y) - x*new_num_row])
        all_grays <- c(all_grays, all_color_sum[center - (x-y) - x*new_num_row])
        tracker <- c(tracker,(center - (x-y) - x*new_num_row))
      } }
  }
  all_reds <- as.vector(na.omit(c(all_reds,red_colors[new_num_row])))
  all_greens <- as.vector(na.omit(c(all_greens,green_colors[new_num_row])))
  all_blues <- as.vector(na.omit(c(all_blues,blue_colors[new_num_row])))
  all_grays <- as.vector(na.omit(c(all_grays,all_color_sum[new_num_row])))
  
  if (statement == "circles in") {
    all_reds <- rev(all_reds)
    all_greens <- rev(all_greens)
    all_blues <- rev(all_blues)
    all_grays <- rev(all_grays)
  }
  
} else { # IF columns or rows was selected as the statement... no need to reorganize the pixels, simply populate row-wise or column-wise
  for(i in 1:num_row) {
    for (j in 1:num_col) {
      all_reds <- c(all_reds, red_colors[j,i])
      all_blues <- c(all_blues, blue_colors[j,i])
      all_greens <- c(all_greens, green_colors[j,i])
      all_grays <- c(all_grays, all_color_sum[j,i])
    }
  }}

################################### Section which determines how tonal the final music output will sound, via a tonality rating (see argument 5).
# We want to adjust MIDI values before they enter .csv file.
key_sign <- floor(runif(1,1,13))
key_sign_letter <- c("Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B", "C")
minor <- c(0,2,3,5,7,8,10)
major <- c(0,2,4,5,7,9,11) # other scales could absolutely be added to this list

mode <- floor(runif(1,1,3)) # Major or Minor design
if (mode == 1) {
  mode_use <- minor
  mode_name <- "Minor"
} else if (mode == 2) {
  mode_use <- major
  mode_name <- "Major"
}

if (tonal == 0.5) {
  red_blue <- abs(cor(all_reds,all_blues)) # "tonality rating" is the mean correlation coefficient of the red/blue/green combos
  red_green <- abs(cor(all_reds,all_greens))
  blue_green <- abs(cor(all_blues,all_greens))
  tonality_rating  <- mean(c(red_blue,red_green,blue_green)) # value between 0 and 1, 1 being fully tonal
  
  adjustments <- unique(floor(runif(ceiling(length(all_reds)*(tonality_rating)),1,length(all_reds)))) # randomly pick which notes to adjust to tonality
  adjust_these_notes <- c(seq(1,length(all_reds),1))[! c(seq(1,length(all_reds),1)) %in% adjustments]
} else if (tonal == 1) {
  adjust_these_notes <- 1:length(all_reds) # adjust all notes to a tonal center
}

if (tonal != 0) { # ignore if desiring to keep atonal
  for (i in adjust_these_notes) {
    if (all_reds[i]%%12 != mode_use[which.min(abs(mode_use - all_reds[i]%%12))]) {
      all_reds[i] <- all_reds[i] - (all_reds[i]%%12 - mode_use[which.min(abs(mode_use - all_reds[i]%%12))])
    }
    if (key_sign > 5) {
      all_reds[i] <- all_reds[i] - (12-key_sign)
    } else {
      all_reds[i] <- all_reds[i] + key_sign
    }}
  for (i in adjust_these_notes) {
    if (all_blues[i]%%12 != mode_use[which.min(abs(mode_use - all_blues[i]%%12))]) {
      all_blues[i] <- all_blues[i] - (all_blues[i]%%12 - mode_use[which.min(abs(mode_use - all_blues[i]%%12))])
    }
    if (key_sign > 5) {
      all_blues[i] <- all_blues[i] - (12-key_sign)
    } else {
      all_blues[i] <- all_blues[i] + key_sign
    }}
  for (i in adjust_these_notes) {
    if (all_greens[i]%%12 != mode_use[which.min(abs(mode_use - all_greens[i]%%12))]) {
      all_greens[i] <- all_greens[i] - (all_greens[i]%%12 - mode_use[which.min(abs(mode_use - all_greens[i]%%12))])
    }
    if (key_sign > 5) {
      all_greens[i] <- all_greens[i] - (12-key_sign)
    } else {
      all_greens[i] <- all_greens[i] + key_sign
    }}
}

################################### Still some preparation
colors_to_make_music[,seq(1,by=3,d)] <- all_reds # number of notes per track (num_col)
colors_to_make_music[,seq(2,by=3,d)] <- all_blues #
colors_to_make_music[,seq(3,by=3,d)] <- all_greens #
if (grayVolume_ON == 1) {colors_to_make_music[,d+1] <- all_grays} # grayscale volume if argument 7 is 1, else random

note_lengths <- c(2.6666667,5.33333334,2,2,2,2,4,4,4,4,8,8,16) # sixteenths to half notes. 16ths/8ths are four times as likely. 4ths twice as likely.
must_do_3 <- 4 # If a triplet is selected, two more MUST occur, otherwise the rhythm gets too syncopated.
volume <- 80 # I chose a 50 - 127 volume range, begins at volume = 80

if (statement == "rows" || statement == "circles out") { # if circle path, randomly change tempo if argument 6 is 1.
  rnum <- round(runif(1,1,min(num_row, num_col)))              # this isn't necessary for rows/columns, since the end of the row/column marks the end
  rtvec <- unique(sort(round(runif(rnum,1,num_row*num_col-2))))
  rnum <- length(rtvec)
} else {
  rnum <- num_row
  rtvec <- seq(num_col, by = num_col, num_col*num_row) # we now have checkpoints for adjusting tempo
}

noclue <- 0 ; one <- 0
if (num_col > 13) { # The whole header is 12 lines; I had to do this to prevent the header from overwriting the first note data for small # columns
  noclue <- num_col - 13
}
if (col_len%%2 == 1 & (statement == "circles in" | statement == "circles out")) {
  one <- 1
}

################################### Begin to build the .csv file (music_MIDI, line 332)
music_MIDI <- matrix(nrow = num_col*num_row*(2*d) + t_ON*(d*rnum-6 - (2*noclue-1)*3) + 4*d + 11 + one, ncol = 7) # 
music_len <- length(music_MIDI[,1])
music_MIDI[1,1] <- 0 # these various rows create the mandatory header of the MIDI file
header <- 960 # my default clock speed, defines resolution of audio.
for(i in 2:9) {
  music_MIDI[i,1] <- 1
}
for(i in 10:12) {
  music_MIDI[i,1] <- 2
}
for(i in 1:12) {
  music_MIDI[i,2] <- 0
}
music_MIDI[1,3] <- "Header"
music_MIDI[2,3] <- "Start_track"
music_MIDI[2,4] <- as.numeric(samp1) # optionally for recreating image (see "bread_crumbs" in Music.R)
music_MIDI[2,5] <- as.numeric(samp2) # #
music_MIDI[2,6] <- num_row # #
music_MIDI[2,7] <- num_col # #
music_MIDI[3,4] <- statement # #
music_MIDI[3,3] <- "Title_t"
music_MIDI[4,3] <- "Text_t"
music_MIDI[5,3] <- "Copyright_t"
music_MIDI[6,3] <- "Time_signature"
music_MIDI[7,3] <- "Key_signature"
music_MIDI[8,3] <- "Tempo"
music_MIDI[9,3] <- "End_track"
music_MIDI[10,3] <- "Start_track"
music_MIDI[11,3] <- "Title_t"
music_MIDI[12,3] <- "Program_c"
music_MIDI[1,4] <- 1    # Header
music_MIDI[1,5] <- d+1+click_ON  #
music_MIDI[1,6] <- header  #
music_MIDI[6,4] <- sample(c(2,3,4,5,6),1) # numerator - beats per measure
music_MIDI[6,5] <- sample(c(2,3),1) # 2 means quarter note gets beat; 3 means eighth
if (music_MIDI[6,5]==2){
  denom <- 4
  denom1 <- 4
} else {
  denom <- 8
  denom1 <- 6
}
music_MIDI[6,6] <- 24
music_MIDI[6,7] <- 8
music_MIDI[7,4] <- 0
music_MIDI[7,5] <- "major"
if (t_ON == 1) {
  tempo <- runif(length(rtvec)+1, min=333333.333333*(denom1/4), max=750000*(denom1/4)) # random tempo (microseconds per beat)
} else {
  rand_var <- runif(1,min=333333.333333*(denom1/4), max=750000*(denom1/4))
  tempo <- rep(rand_var,length(rtvec)+1)
}
music_MIDI[8,4] <- tempo[1]
music_MIDI[11,4] <- instr_list[1]
music_MIDI[12,4] <- 1
music_MIDI[12,5] <- instr_num[1]

mid <- 0 # for "Program_c's" in middle
simplify <- header*as.numeric(music_MIDI[6,4]) # this is the value that the MIDI data considers as ONE MEASURE. Comes up frequently.
new_zero <- t_ON
clicks <- 0 # ultimately the BPM
clicks_list <- c() # tells file in MIDI time when to click....
interval <- 0 # ... at these intervals based on current tempo
interval_list <- c(2) # for restarting metronome (louder click noise, see the line 590 region) 
interval_list_sum <- c(2) # for unique click at beginning of each "movement"
base_vec <- c(0) # monitors microseconds between note occurances
base_check <- 0 # boolean variable for very first note of an instrument track
d_count <- 3 # which track is being iterated through.
t_count <- 1 # which tempo[] is currently running (note, if t_ON = 0, tempo[] has the same value duplicated instead of random ones).
t_count0 <- 1 #

tempo_add <- c() # build a sequence that sets aside a .csv line to specify new tempo. This is iterated through when add note/volume/duration data.
add1 <- 0  ; add2 <- 0
if (t_ON == 1) {
  for (s in 1:(13 + num_col*num_row - num_col)) {
    if (length(tempo_add) < length(colors_to_make_music[,1])) {
      if ((s-1) %in% rtvec & s != 1) {
        add1 <- 1
        tempo_add <- c(tempo_add, 11 + 2*s + add1 + add2)
        add2 <- add2 + 1
      } else {tempo_add <- c(tempo_add, 11 + 2*s + add1 + add2)}
      add1 <- 0
    } }
} else {tempo_add <- seq(13,11+num_col*num_row*2,2)} # we don't need new lines if no tempo changes!

################################### Begin iteration

for (c in 1:d) { 3 # for each track...
  count <- 0 #
  base <- 0 # reset per track
  base_ref <- 0
  if (t_ON == 0 & c > 1) { mid = (4-d)*(c-1) }
 for(i in tempo_add) { # inserting time data into the music_MIDI file. MAIN FOR LOOP. Metadata for the end of each "NOTE"
   count <- count + 1
   music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+mid,1] <- c+1
   music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,1] <- c+1
   if (base_check == 0) {
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+mid,2] <- base
     if (must_do_3 == 4) {
       if (d_count >= 3) {
         duration <- sample(note_lengths, 1)/(as.numeric(music_MIDI[6,5])-1) # random note length for next note
         if (duration == 2.6666667 | duration == 5.33333334) {
           must_do_3 <- 1
         }
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- base + (tempo[t_count]/tempo[1])*(header*duration)/4 # add the random note length to current time
         if (duration%%1!=0) {
           d_count <- d_count - 1
         }}
       else if (d_count == 2) {
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- base + (tempo[t_count]/tempo[1])*(header*duration)/4 #
         d_count <- d_count - 1
       }
       else if (d_count == 1) {
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- base + (tempo[t_count]/tempo[1])*(header*duration)/4 #
         d_count <- 3
       }}        
   }
    else { # This runs when the end of a movement occurs. The program figures out exactly when to stop, stores that info for later.
        new_duration <- sample(c(0,note_lengths[3:13]), 1)/(as.numeric(music_MIDI[6,5])-1)
        music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+2+mid,2] <- base_vec[count] + (tempo[t_count]/tempo[1])*(header*new_duration)/4 #
        if (i != max(tempo_add)) {
          # checking if newly added duration exceeds time point of next note, so that events aren't out of order!
          if ((base_vec[count] + (tempo[t_count]/tempo[1])*(header*new_duration/4)) > (base_vec[count+1])) { 
            music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+2+mid,2] <- base_vec[count]
          }
        }
        music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+3+mid,2] <- base_vec[count + 1]
     }
     if (must_do_3 < 4) {
       music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- base + (tempo[t_count]/tempo[1])*(header*duration)/4 #
       must_do_3 <- must_do_3 + 1
     }
     # everything above was time data (rhythms). Now for the note data.
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,6] <- 0
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+mid,3] <- "Note_on_c"
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,3] <- "Note_off_c"
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+mid,4] <- chan_list[c]  # channel number
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,4] <- chan_list[c]   # channel number
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+mid,5] <- colors_to_make_music[count,c]
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,5] <- colors_to_make_music[count,c]
     
     if (count %in% rtvec & count != 0) { # if final note of row/column is reached, create a pause ("end of movement")
       t_count <- t_count + 1                   # this allows the metronome to line up despite tempo changes!
       t_count0 <- t_count0 + 1
       if (t_ON == 1) {
         simplify <- header*(tempo[t_count-1]/tempo[1])*as.numeric(music_MIDI[6,4])
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(2000000/tempo[t_count-1]) # this was complicated...
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+2+mid,1] <- c+1
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+2+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(2000000/tempo[t_count-1])
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+2+mid,3] <- "Tempo"
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+2+mid,4] <- tempo[t_count]
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+3+mid,1] <- c+1
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+3+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(2000000/tempo[t_count-1])
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+3+mid,3] <- "Note_on_c"
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+3+mid,4] <- chan_list[c]
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+4+mid,1] <- c+1
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+4+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(2000000/tempo[t_count-1])
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+4+mid,3] <- "Note_off_c"
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+4+mid,4] <- chan_list[c]
       } else {
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(2000000/tempo[1])
         music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+2+mid,1] <- c+1
       }
       if (new_zero == 1) {
         new_zero <- (base - (base - new_zero)%%simplify) + simplify*floor(2000000/tempo[t_count-1])
       }
     }
     # And now the volume data.
     if (grayVolume_ON == 1) { # volume control, either based on graycale or random
       volume <- 50 + colors_to_make_music[count,d+1]*77
     } else {
       volume <- min(max(50,round(runif(1, min = .8*volume, max = 1.25*volume))), 127)
     }
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+mid,6] <- volume
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,6] <- 0
     base <- as.numeric(music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2])
     if (c == 1) {
       base_vec[count] <- base
     }
     # Now, some information for populating time data for the click_track (appended to MIDI file later, see line 590 region)
     # Most of this doesn't happen if t_ON = 0 
     if (count %in% rtvec & count != 0 & c==d) { 
       if (t_ON == 0) {
         clicks <- (60/(tempo[1]/1000000))*(as.numeric(music_MIDI[6,5])-1) # current BPM - eighth note denominator means twice as many "clicks"      
       } else {
         clicks <- (60/(tempo[t_count-1]/1000000))*(as.numeric(music_MIDI[6,5])-1)
       }
       interval <- header/(clicks/(60/(tempo[1]/1000000))) # how much space between quarter notes, in BPM language
       if (length(clicks_list) == 0) {
         clicks_list <- c(clicks_list, seq(0, by = interval, base)) # add for later click track
         interval_list <- c(interval_list, length(seq(0, by = interval, base)))
         base_ref <- base
       } else {
         clicks_list <- c(clicks_list, seq(base_ref, by = interval, base)) # add for later click track
         interval_list <- c(interval_list, length(seq(base_ref, by = interval, base)))
         base_ref <- base
       }
       if (t_ON == 0) { # helps out with establishing new "movement" time intervals
         interval_list_sum <- c(interval_list_sum, sum(interval_list[1:t_count0-1]) + interval_list[t_count0])
       } else {
         interval_list_sum <- c(interval_list_sum, sum(interval_list[1:t_count-1]) + interval_list[t_count])
       }} 
 }
   base_check = 1
   # Metadata for the end of each TRACK
   music_MIDI[1+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,1] <- c+1
   if (c==1) {
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(3000000/tempo[t_count-1])
     music_MIDI[1+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(3000000/tempo[t_count-1])
   } else {
     music_MIDI[d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(3000000/tempo[t_count-1])
     music_MIDI[1+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- (base - (base - new_zero)%%simplify) + simplify*floor(3000000/tempo[t_count-1])
   }
   music_MIDI[1+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,3] <- "End_track"
   music_MIDI[2+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,1] <- c+2
   music_MIDI[2+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- 0
   music_MIDI[2+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,3] <- "Start_track"
   if (c != d) {
     music_MIDI[3+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,1] <- c+2
     music_MIDI[3+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- 0
     music_MIDI[3+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,3] <- "Title_t"
     music_MIDI[3+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,4] <- instr_list[c+1]
     music_MIDI[4+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,1] <- c+2
     music_MIDI[4+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- 0
     music_MIDI[4+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,3] <- "Program_c"
     music_MIDI[4+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,4] <- chan_list[c+1]
     music_MIDI[4+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,5] <- instr_num[c+1]
     music_MIDI[5+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,1] <- c+2
     music_MIDI[5+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+1+mid,2] <- 0
     music_MIDI[5+d*(c-1)+i+t_ON*(c-1)*(rnum-(2*noclue-1)-1)+(c-1)*num_col*num_row*2+2+mid,2] <- base_vec[1]
   }
   mid <- 0
   t_count <- 1
   t_count0 <- 1
}
if (t_ON == 0) { # create clicks for final movement - needed outside of the for-loop. Again, ignore if t_ON is zero.
  clicks <- (60/(tempo[1]/1000000))*(as.numeric(music_MIDI[6,5])-1) # current BPM       
} else {
  clicks <- (60/(tempo[rnum]/1000000))*(as.numeric(music_MIDI[6,5])-1) # current BPM
}
clicks_list <- c(clicks_list, seq(base_ref, by = header/(clicks/(60/(tempo[1]/1000000))), base)) # add for later click track (below)
interval_list <- c(interval_list, length(seq(base_ref, by = interval, base)))
interval_list_sum <- c(interval_list_sum, sum(interval_list))
interval_list_sum <- c(interval_list_sum, interval_list_sum[length(interval_list_sum)])
music_MIDI[length(music_MIDI[,1]),1] <- c+2
music_MIDI[length(music_MIDI[,1]),2] <- 0
music_MIDI[length(music_MIDI[,1]),3] <- "Title_t"
music_MIDI[length(music_MIDI[,1]),4] <- instr_list[length(instr_list)]

#######################################################################
# metadata for the click track, which is appended to the instrument tracks already created above. Argument 7 optionally omits the click track.
if (click_ON == 1) { # Note: omitting click track dramatically improves runtime. This is like adding another instrument with LOTS OF NOTES.
  click_len <- length(as.vector(clicks_list))
  MIDI_click <- matrix(nrow = 2*click_len + 4, ncol = 7) # click track portion
  MIDI_click[,1] <- d + 2
  MIDI_click[,2][1:2] <- 0
  i_count <- 1
  for (i in seq(3, click_len*2 + 1, 2)) {
    MIDI_click[,2][i] <- clicks_list[i_count+1]
    MIDI_click[,2][i+1] <- clicks_list[i_count+1]
    MIDI_click[,3][i] <- "Note_off_c"
    MIDI_click[,3][i+1] <- "Note_on_c"
    MIDI_click[,6][i] <- 0
    MIDI_click[,6][i+1] <- 77 # volume constant
    i_count <- i_count + 1
  }
#  if (t_ON == 1) {
#    MIDI_click[,3][1] <- "Title_t"
 #   MIDI_click[,4][1] <- instr_list[length(instr_list)]
 # }
  MIDI_click[,3][1] <- "Program_c"
  MIDI_click[,3][2] <- "Note_on_c"
  MIDI_click[,4][1:(click_len*2 + 2)] <- c + 1
  MIDI_click[,5][1] <- instr_num[c+1]
  MIDI_click[,5][2:(click_len*2 + 2)] <- 80 # non-beat 1 click
  MIDI_click[,6][2] <- 77 # volume
  for (i in 1:length(tempo)) { # timing is everything!
    for (j in seq(interval_list_sum[i]*2 - 2, by = as.numeric(music_MIDI[6,4])*2, interval_list_sum[i+1]*2 - 2)) {
      if (j == interval_list_sum[i]*2 - 2 & j < length(MIDI_click[,5])) {
        MIDI_click[,5][j] <- 120 # NEW MOVEMENT click
        MIDI_click[,6][j] <- 127 # volume of NEW MOVEMENT click
      } else if (j < length(MIDI_click[,5])) {
        MIDI_click[,5][j] <- 110 # beat 1 click
      }
    }
  }
  music_MIDI <- rbind(music_MIDI,MIDI_click) # slap the click track as the last "track" of the MIDI file
  music_len <- length(music_MIDI[,1])
}

####################################################################### Metadata for the end of THE ENTIRE FILE
music_MIDI[music_len-3,2] <- base
music_MIDI[music_len-3,3] <- "Note_off_c"
music_MIDI[music_len-3,4] <- d + 1
music_MIDI[music_len-3,5] <- 80
music_MIDI[music_len-3,6] <- 0
music_MIDI[music_len-2,2] <- base
music_MIDI[music_len-2,3] <- "End_track"
music_MIDI[music_len-1,1] <- 0
music_MIDI[music_len-1,2] <- 0
music_MIDI[music_len-1,3] <- "End_of_file"
music_MIDI[music_len,] <- "" # final line!

write.table(music_MIDI[1:(music_len-1),], file = "music_MIDI.csv", sep = ",", # create Excel .csv file that is input for csvmidi.c
          row.names = FALSE, col.names = FALSE, na = "")

# Use csvmidi.c to create the MIDI file, and enjoy! I recommend opening it in a notation software, too.

####################################################################### Printed outputs for the user
total_dur <- base/((60*header)/(mean(tempo)/1000000)) # approximate length of entire piece in minutes!

if (statement == "rows") {
  print(paste("# of columns ('movements') performed:", num_row, ". Approx. every" , round(col_breaks,0), "columns"))
  print(paste("# of rows ('notes+rests per movement') performed:", num_col, ". Approx. every", round(row_breaks,0), "rows"))
} else if (statement == "columns") {
  print(paste("# of rows ('movements') performed:", num_row, ". Approx. every", round(col_breaks,0), "rows"))
  print(paste("# of columns ('notes+rests per movement') performed:", num_col, ". Approx. every", round(row_breaks,0), "columns"))
} else if (statement == "circles in") {
  print(paste("Radial pattern from outside to center. # of notes:", num_row*num_col, ". # of movements:", rnum, "."))
} else if (statement == "circles out") {
  print(paste("Radial pattern from center to outside. # of notes:", num_row*num_col, ". # of movements:", rnum, "."))
}
print(paste("MIDI note range:", round(scaling_sample[[1]][1]/scaling_sample[[1]][2]), "-",
            round((scaling_sample[[1]][1]+255)/scaling_sample[[1]][2])))

print(paste("Quarter BPM:", round(60/(tempo[1]/1000000), 0), "; Time Signature:", music_MIDI[6,4], "/", denom))
print(paste("Key Signature:", key_sign_letter[key_sign], mode_name))
print(paste("Instrument", 1:d, "=", instr_list1[1:d]))

if (t_ON == 0) {
  print("Tempo changes: OFF")
} else{print("Tempo changes: ON")}

if (grayVolume_ON == 0) {
  print("Grayscale volume: OFF")
} else {print("Grayscale volume: ON")}

if (total_dur > 60) {
  print(paste("Estimated duration:", round(total_dur/60, 2), "hours"))
} else { print(paste("Est. duration of piece:", round(total_dur, 1), "minutes")) }

end.time <- Sys.time()
print(round(end.time - start.time,1)) # estimated runtime of this R file

### END