# Example command in the shell: Rscript.exe "Music.R" "music_MIDI.csv" 0 1 1
# Convert music to image, and get a video of approximately what pixel is playing!
# If you get an error, please try running the code again.

start.time <- Sys.time()
library(magrittr) # these libraries are required; please download.
library(imager)
library(magick) # requires ImageMagick path be specified
library(av) #
library(gifski) #

args = commandArgs(trailingOnly=TRUE)
########### Arguments: ###########
csv <- as.matrix(read.csv("music_MIDI.csv", header=FALSE)) # import the csv file that was created by converting the MIDI file.
# To obtain this csv file, use midicsv.c that is included with this file.

bread_crumbs_num <- 0 # if reconverting to an image from Image.R, should we cheat and use data in the MIDI file to recreate perfectly (1)?

beauty <- 0.5 # convert to 6-bit, more homogeneous color (1), assign some (based on RGB similarity) to 6-bit (0.5), or keep 24-bit (0)

create_video <- 1 # create a video showing where the pixel is (1), or not (0). No video reduces runtime.

################################### Sort out the bread crumbs
if (length(args)==0) {
  stop("At least one argument must be supplied", call.=FALSE)
}
if (is.na(as.numeric(csv[2,6])) || as.numeric(csv[2,6] == "")) {
  bread_crumbs_num <- 0
}
if (bread_crumbs_num == 0) {
  bread_crumbs <- FALSE
} else {bread_crumbs <- TRUE}

if (bread_crumbs & (grepl("rows",csv[3,4]) || grepl("columns",csv[3,4]) || grepl("circles in",csv[3,4]) || grepl("circles out",csv[3,4]))) {
  statement <- csv[3,4][[1]]
} else {
  choice <- sample(c("columns","rows","circles in","circles out"), size = 1)
  statement <- choice
} # when I say rows, each column is a the movement. Each note is from a different row.

################################### Pull the note/volume/rhythm data from the .csv file and put into a queue (list)
chan_check <- -1
notes_1 <- c() ; notes_2 <- c() ; notes_3 <- c()
volume_1 <- c() ; volume_2 <- c() ; volume_3 <- c()
rhythm_1 <- c() ; rhythm_2 <- c() ; rhythm_3 <- c()
inst_list <- c()
tempo_list <- c() ; when <- c() # for doing a weighted average of tempi, helps construct an appropriately long video file at the end.

if (bread_crumbs) {
  old_samp1 <- as.numeric(csv[2,4])
  old_samp2 <- as.numeric(csv[2,5])
} else {
  old_samp1 <- NA
  old_samp2 <- NA
}
clock <- as.numeric(csv[1,6])
start_tempo <- as.numeric(csv[8,4])

for(i in 1:length(csv[,1])) {
  if (grepl("Note_on_c", csv[i,3]) && as.numeric(csv[i,6]) > 0) {
    if (chan_check == 1) { # these equivalencies can be changed for desired channels (i.e. if there are more than three instruments)
      notes_1 <- c(notes_1,as.numeric(csv[i,5]))
      volume_1 <- c(volume_1,as.numeric(csv[i,6])/127)
      rhythm_1 <- c(rhythm_1,as.numeric(csv[i,2]))
    }
    else if (chan_check == 2) { #
      notes_2 <- c(notes_2,as.numeric(csv[i,5]))
      volume_2 <- c(volume_2,as.numeric(csv[i,6])/127)
      rhythm_2 <- c(rhythm_2,as.numeric(csv[i,2]))
    }
    else if (chan_check == 3) { #
      notes_3 <- c(notes_3,as.numeric(csv[i,5]))
      volume_3 <- c(volume_3,as.numeric(csv[i,6])/127)
      rhythm_3 <- c(rhythm_3,as.numeric(csv[i,2]))
    } }
  else if (grepl("Title_t", csv[i,3])) {
    inst_list <- c(inst_list,csv[i,4])
  }
  else if (grepl("Start_track", csv[i,3])) {
    chan_check <- chan_check + 1
  }
  else if (grepl("Tempo", csv[i,3])) {
    tempo_list <- c(tempo_list,as.numeric(csv[i,4]))
    when <- c(when,as.numeric(csv[i,2]))
  }}

################################### A few things: 
# 1.) # We scale some colors' notes/volumes/rhythms because we NEED equal # of notes for each R/G/B channel. This duplicates some data.
      # We figure out this scaling coefficient by figuring how many pixels there are.
# 2.) # Figure out the factors of the # of pixels, to define dimensions of picture. If bread_crumbs = 1, this is irrelevant.

advance <- FALSE # a WHILE loop JUST IN CASE the number of pixels is a prime number (meaning no factors!). Sometimes the number of scaled notes is prime.
w <- 0
while (!advance) {
  notes_1 <- notes_1[1:(length(notes_1) - w)] # 1.)
  notes_2 <- notes_2[1:(length(notes_2) - w)] #
  notes_3 <- notes_3[1:(length(notes_3) - w)] #
  
  pixels <- max(length(notes_1),length(notes_2),length(notes_3))
  scale_factor1 <- length(notes_1)/pixels
  scale_factor2 <- length(notes_2)/pixels
  scale_factor3 <- length(notes_3)/pixels
  
  scaled_notes_1 <- c() ; scaled_notes_2 <- c() ; scaled_notes_3 <- c() 
  scaled_volume_1 <- c() ; scaled_volume_2 <- c() ; scaled_volume_3 <- c() 
  scaled_rhythm_1 <- c() ; scaled_rhythm_2 <- c() ; scaled_rhythm_3 <- c() 
  
  for (i in 1:pixels) { # get pseudo-original color!
    scaled_notes_1 <- c(scaled_notes_1,min((notes_1[ceiling(i*scale_factor1)]*runif(1,0.96,1.04)),255))
    scaled_notes_2 <- c(scaled_notes_2,min((notes_2[ceiling(i*scale_factor2)]*runif(1,0.96,1.04)),255))
    scaled_notes_3 <- c(scaled_notes_3,min((notes_3[ceiling(i*scale_factor3)]*runif(1,0.96,1.04)),255))
    scaled_volume_1 <- c(scaled_volume_1,volume_1[ceiling(i*scale_factor1)])
    scaled_volume_2 <- c(scaled_volume_2,volume_2[ceiling(i*scale_factor2)])
    scaled_volume_3 <- c(scaled_volume_3,volume_3[ceiling(i*scale_factor3)])
    scaled_rhythm_1 <- c(scaled_rhythm_1,rhythm_1[ceiling(i*scale_factor1)])
    scaled_rhythm_2 <- c(scaled_rhythm_2,rhythm_2[ceiling(i*scale_factor2)])
    scaled_rhythm_3 <- c(scaled_rhythm_3,rhythm_3[ceiling(i*scale_factor3)])
  }
  frames <- max(scaled_rhythm_1,scaled_rhythm_2,scaled_rhythm_3) # for later animated video

  fact_1 <- c() ; fact_2 <- c() # 2.)
  for(i in rev(2:sqrt(pixels))) { #
    if((pixels %% i) == 0) { #
      if (((pixels/i)/i) < 16) { # this if statement prevents super skinny images (e.g. 2 x 256)
        fact_1 <- c(fact_1,i) #
        fact_2 <- c(fact_2,pixels/i) #
      }}
  }
  if ((length(fact_1) != 0 & length(fact_2) != 0) | bread_crumbs) {
    advance <- TRUE
  }
  w <- w+1
}
rand_dim <- floor(max(1,round(runif(1,1,ceiling(length(fact_1)/2))))) # choose a random height and width
if (!bread_crumbs) {
  if (statement == "rows" || statement == "columns") {
    num_cols <- fact_1[rand_dim]
    num_rows <- fact_2[rand_dim]
  } else {
    num_rows <- fact_1[rand_dim]
    num_cols <- fact_2[rand_dim]
  }} else {
    num_cols <- as.numeric(csv[2,6])
    num_rows <- as.numeric(csv[2,7])
  }

# This is where color is estimated if bread_crumbs = 0
if (is.na(old_samp1)) {  # scale to values between 0 - 255 for color placement (with variation). Get pseudo-original color!
  scaled_notes_1 <- 255 - (max(scaled_notes_1) - scaled_notes_1)*floor(255/(max(scaled_notes_1)-min(scaled_notes_1)))
  scaled_notes_2 <- 255 - (max(scaled_notes_2) - scaled_notes_2)*floor(255/(max(scaled_notes_2)-min(scaled_notes_2)))
  scaled_notes_3 <- 255 - (max(scaled_notes_3) - scaled_notes_3)*floor(255/(max(scaled_notes_3)-min(scaled_notes_3)))
} else {
  scaled_notes_1 <- ((scaled_notes_1*old_samp2) - old_samp1) # get original color!
  scaled_notes_2 <- ((scaled_notes_2*old_samp2) - old_samp1) #
  scaled_notes_3 <- ((scaled_notes_3*old_samp2) - old_samp1) #
}

################################### 
# Section which determines how colorful the final image output will look, via a "beauty" rating (see argument 3).
# We want to adjust pixels values before they enter the matrix.
if (beauty == 0.5) {
  red_blue <- abs(cor(scaled_notes_1,scaled_notes_2)) # "beauty rating" is the mean correlation coefficient of the red/blue/green combos
  red_green <- abs(cor(scaled_notes_1,scaled_notes_3)) #
  blue_green <- abs(cor(scaled_notes_2,scaled_notes_3)) #
  beauty_rating  <- mean(c(red_blue,red_green,blue_green)) # value between 0 and 1, 1 being full 6-bit
  
  adjustments <- unique(floor(runif(ceiling(length(scaled_notes_1)*(beauty_rating)),1,length(scaled_notes_1)))) # randomly pick which pixels to adjust to 6-bit
  adjust_these_pixels <- c(seq(1,length(scaled_notes_1),1))[! c(seq(1,length(scaled_notes_1),1)) %in% adjustments]
} else if (beauty == 1) {
  adjust_these_pixels <- 1:length(scaled_notes_1) # adjust all pixels to a 6-bit format
}
if (beauty != 0) { # ignore if desiring to keep 24-bit
  for (i in adjust_these_pixels) {
    if (scaled_notes_1[i] < 0.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_1[i] = 0
    } else if (scaled_notes_1[i] < 85.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_1[i] = 85
    } else if (scaled_notes_1[i] < 170.01 && (i %in% adjust_these_pixels))  {
      scaled_notes_1[i] = 170
    } else if (scaled_notes_1[i] < 255.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_1[i] = 255
    } }
  for (i in adjust_these_pixels) {
    if (scaled_notes_2[i] < 0.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_2[i] = 0
    } else if (scaled_notes_2[i] < 85.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_2[i] = 85
    } else if (scaled_notes_2[i] < 170.01 && (i %in% adjust_these_pixels))  {
      scaled_notes_2[i] = 170
    } else if (scaled_notes_2[i] < 255.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_2[i] = 255
    }}
  for (i in adjust_these_pixels) {
    if (scaled_notes_3[i] < 0.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_3[i] = 0
    } else if (scaled_notes_3[i] < 85.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_3[i] = 85
    } else if (scaled_notes_3[i] < 170.01 && (i %in% adjust_these_pixels))  {
      scaled_notes_3[i] = 170
    } else if (scaled_notes_3[i] < 255.01 && (i %in% adjust_these_pixels)) {
      scaled_notes_3[i] = 255
    } }
}

################################### Get the approximate length of the video compiled later (see line 360 region)
when_diff <- c()
if (is.na(as.numeric(csv[2,6])) || as.numeric(csv[2,6] == "")) {
  for (i in 1:(length(when)-1)) {
    when_diff <- c(when_diff,when[i+1] - when[i])
  }
  when_diff <- c(when_diff,frames-as.numeric(when[length(when)]))
  tempo_avg <- weighted.mean(tempo_list,when_diff)
} else {
  if (length(when) == 1) {
    tempo_avg <- tempo_list[1]
  } else {
    for (i in 1:(length(unique(when))-1)) {
      when_diff <- c(when_diff,unique(when)[i+1] - unique(when[i]))
    }
    when_diff <- c(when_diff,frames-as.numeric(unique(when)[length(unique(when))]))
    tempo_avg <- weighted.mean(unique(tempo_list),when_diff)
  }}

################################### Like in Image.R, we set up a matrix that imitates the image's ultimate pixel value. Order depends on "statement".
if (statement != "circles in" & statement != "circles out") {
  music_to_make_red <- matrix(nrow = num_rows, ncol = num_cols)
  music_to_make_blue <- matrix(nrow = num_rows, ncol = num_cols)
  music_to_make_green <- matrix(nrow = num_rows, ncol = num_cols)
  for(i in 1:num_cols) {
    music_to_make_red[,i] <- rev(scaled_notes_1[(i-1)*num_rows + 1:num_rows])*rev(scaled_volume_1[(i-1)*num_rows + 1:num_rows])
    music_to_make_blue[,i] <- rev(scaled_notes_2[(i-1)*num_rows + 1:num_rows])*rev(scaled_volume_2[(i-1)*num_rows + 1:num_rows])
    music_to_make_green[,i] <- rev(scaled_notes_3[(i-1)*num_rows + 1:num_rows])*rev(scaled_volume_3[(i-1)*num_rows + 1:num_rows])
  }
} else {
  music_to_make_red <- matrix(nrow = num_cols, ncol = num_rows)
  music_to_make_blue <- matrix(nrow = num_cols, ncol = num_rows)
  music_to_make_green <- matrix(nrow = num_cols, ncol = num_rows)
  for (i in 1:length(scaled_notes_1)) {
    music_to_make_red[i] <- scaled_notes_1[i]*scaled_volume_1[i]
    music_to_make_blue[i] <- scaled_notes_2[i]*scaled_volume_2[i]
    music_to_make_green[i] <- scaled_notes_3[i]*scaled_volume_3[i]
  }
}

if (statement == "rows") {
 music_to_make_red <- music_to_make_red[,ncol(music_to_make_red):1]
 music_to_make_blue <- music_to_make_blue[,ncol(music_to_make_blue):1]
 music_to_make_green <- music_to_make_green[,ncol(music_to_make_green):1]
} else if (statement == "columns" && is.na(csv[2,6])) {
  music_to_make_red <- music_to_make_red[nrow(music_to_make_red):1,]
  music_to_make_blue <- music_to_make_blue[nrow(music_to_make_blue):1,]
  music_to_make_green <- music_to_make_green[nrow(music_to_make_green):1,]
} else if (statement == "circles in" || statement == "circles out") { # We now have image dimensions - if radial pattern, here's the algorithm to put the pixels in their places
  xy_tracker <- c() ; x_tracker <- c() ; y_tracker <- c() # The algorithm is essentially backwards from the one in Image.R
  max_dim <- max(num_rows, num_cols)
  if (max_dim%%2 == 0) {
    max_dim <- max_dim + 1
  }
  aa <- ceiling((max_dim - num_cols)/2)
  ab <- floor((max_dim - num_cols)/2)
  al <- ceiling((max_dim - num_rows)/2)
  ar <- floor((max_dim - num_rows)/2)
  
  new_num_cols <- max_dim
  new_num_rows <- max_dim
  center <- ceiling((new_num_rows/2)) + floor((new_num_cols/2))*new_num_rows #### #
  
  xy_tracker <- c(xy_tracker, center)
  for (x in 1:floor((0.5*new_num_cols))) { # down direction (start from center)
    r2 <- x
    for (y in x:1) {
      if (!((center + (x+1-y) - (x-1)*new_num_cols) %in% xy_tracker)) {
        xy_tracker <- c(xy_tracker,(center + (x+1-y) - (x-1)*new_num_cols))
      }
    }
    for (y in 1:(2*x-1)) { # right direction
      if (!((center + x + (y-(x-1))*new_num_cols) %in% xy_tracker)) {
        xy_tracker <- c(xy_tracker,(center + x + (y-(x-1))*new_num_cols))
      }
    }
    for (y in 1:(2*x)) { # up direction
      if (!((center + (x-y) + x*new_num_cols) %in% xy_tracker)) {
        xy_tracker <- c(xy_tracker,(center + (x-y) + x*new_num_cols))
      }
    }
    for (y in 1:(2*x)) { # left direction
      if (!((center - x - (y-x)*new_num_cols) %in% xy_tracker)) {
        xy_tracker <- c(xy_tracker,(center - x - (y-x)*new_num_cols))
      }
    }
    if (center != round((new_num_cols*new_num_rows)/2)) { # remaining down direction
      r2 <- 2*x - 1
    }
    for (y in 1:r2) {
      if (!((center - (x-y) - x*new_num_cols) %in% xy_tracker)) {
        xy_tracker <- c(xy_tracker,(center - (x-y) - x*new_num_cols))
      }}
  }
  xy_tracker <- c(xy_tracker,new_num_cols) # to accomodate the color.at() function, we have to figure out the XY coordinates of the image.
  if (num_cols > num_rows) {
    xy_tracker <- xy_tracker[xy_tracker > ar*new_num_rows & xy_tracker < ((new_num_cols-al)*new_num_rows)] # if portrait orientation
    x_tracker <- floor(xy_tracker/new_num_rows) - max(ar,al) + 1
    y_tracker <- xy_tracker%%(new_num_rows)
  } else {
    xy_tracker <- xy_tracker[(xy_tracker%%new_num_cols > aa) & (xy_tracker%%new_num_cols <= (new_num_rows-ab))] # if landscape orientation
    x_tracker <- floor(xy_tracker/new_num_rows)
    y_tracker <- xy_tracker%%new_num_rows - max(aa,ab)
  }
  if (statement == "circles in") {
    music_to_make_red <- rev(music_to_make_red) # apply colors in opposite order as "circles out"
    music_to_make_green <- rev(music_to_make_green) #
    music_to_make_blue <- rev(music_to_make_blue) #
  }
}
music_to_make_red[is.na(music_to_make_red)] <- 0 # remove NA values with 0; this could happen with negative numbers and/or outside of actual image
music_to_make_green[is.na(music_to_make_green)] <- 0 #
music_to_make_blue[is.na(music_to_make_blue)] <- 0 #

################################### Create an empty image and fill in the proper color, pixel by pixel.
if (statement != "circles in" & statement != "circles out") {
  im <- cimg(array(1,c(num_cols,num_rows,1,3)))
  for (i in 1:num_cols) {
    for (j in 1:num_rows) {
      color.at(im,i,j) <- c(ceiling(music_to_make_red[j,i])
                            ,ceiling(music_to_make_green[j,i])
                            ,ceiling(music_to_make_blue[j,i]))
    }}
} else {
  im <- cimg(array(1,c(num_rows,num_cols,1,3)))
  for (i in 1:min(length(xy_tracker),(num_cols*num_rows))) {
    color.at(im,x_tracker[i],y_tracker[i]) <- c(ceiling(music_to_make_red[i])
                                         ,ceiling(music_to_make_green[i])
                                         ,ceiling(music_to_make_blue[i]))
  }}    
if (statement == "columns") { # just have to do this, apparently.
  im <- imrotate(im,90) 
} else if (statement == "rows" & is.na(csv[2,6])) { # just have to do this, apparently.
  im <- imrotate(im,180)
}

################################### Plot the image, and save as a JPEG to see the pixelated image more crisply. 
# Dimensions of this were specified by the factors in lines 120-130 
if (statement != "rows") {
  converted_plot <- plot(im,axes=FALSE, main = paste(statement,"(",num_rows,"x",num_cols,")"))
  text <- paste("Converted to Image, ",statement," (",num_rows,"x",num_cols,").jpg",sep = "")
} else {
  converted_plot <- plot(im,axes=FALSE, main = paste(statement,"(",num_cols,"x",num_rows,")"))
  text <- paste("Converted to Image, ",statement," (",num_cols,"x",num_rows,").jpg",sep = "")
}
save.image(im,text)

################################### END of creating the image. Below optionally makes a video showing the pixel path (if argument 4 is 1)
################################### 
### Create the animation showing approximately what pixel is being performed of the Converted Image from line 350 ###
if (create_video == 1) {
  im2 <- load.image(text)
  im2 <- pad(mirror(im2,"x"),1,pos=1,"xy") # pad() creates a frame of sorts.
  im_for_video <- c() # list of images that the ImageMagick writer will iterate through.
  
  cnt <- 1
  if (statement == "rows") { # the pixels move top to bottom
    for (time in 1:length(scaled_notes_1)) {
      dup_im2 <- im2
      color.at(dup_im2,num_cols-floor(cnt/num_rows),cnt%%(num_rows+1)) <- c(255,255,255)
      if (!(num_cols-floor(cnt/num_rows) < 1) & !(num_cols-floor(cnt/num_rows) > (num_cols-1)) # cannot surround in black if moving along an edge!
          & !(cnt%%(num_rows+1) < 1) & !(cnt%%(num_rows+1) > (num_rows-1))) {
        color.at(dup_im2,num_cols-floor(cnt/num_rows)-1,cnt%%(num_rows+1)-1) <- c(0,0,0) # all these surround around the currently played pixel n black
        color.at(dup_im2,num_cols-floor(cnt/num_rows)-1,cnt%%(num_rows+1)) <- c(0,0,0) # helps with seeing where the darn thing is!
        color.at(dup_im2,num_cols-floor(cnt/num_rows)-1,cnt%%(num_rows+1)+1) <- c(0,0,0)
        color.at(dup_im2,num_cols-floor(cnt/num_rows),cnt%%(num_rows+1)-1) <- c(0,0,0)
        color.at(dup_im2,num_cols-floor(cnt/num_rows),cnt%%(num_rows+1)+1) <- c(0,0,0)
        color.at(dup_im2,num_cols-floor(cnt/num_rows)+1,cnt%%(num_rows+1)-1) <- c(0,0,0)
        color.at(dup_im2,num_cols-floor(cnt/num_rows)+1,cnt%%(num_rows+1)) <- c(0,0,0)
        color.at(dup_im2,num_cols-floor(cnt/num_rows)+1,cnt%%(num_rows+1)+1) <- c(0,0,0)
      }
      im_for_video <- append(im_for_video,cimg2magick(dup_im2)) # must use APPEND because c() puts lists within lists.
      cnt <- cnt + 1
    }
  } else if (statement == "columns") { # the pixels move left to right
    for (time in 1:length(scaled_notes_1)) {
      dup_im2 <- im2
      color.at(dup_im2,num_rows-cnt%%(num_rows+1),ceiling(cnt/num_rows)) <- c(255,255,255)
      if (!(num_rows-cnt%%(num_rows+1) < 1) & !(num_rows-cnt%%(num_rows+1) > (num_rows-1))
          & !(ceiling(cnt/num_rows) < 1) & !(ceiling(cnt/num_rows) > (num_cols-1))) {
        color.at(dup_im2,num_rows-cnt%%(num_rows+1)-1,ceiling(cnt/num_rows)-1) <- c(0,0,0) #
        color.at(dup_im2,num_rows-cnt%%(num_rows+1)-1,ceiling(cnt/num_rows)) <- c(0,0,0)
        color.at(dup_im2,num_rows-cnt%%(num_rows+1)-1,ceiling(cnt/num_rows)+1) <- c(0,0,0)
        color.at(dup_im2,num_rows-cnt%%(num_rows+1),ceiling(cnt/num_rows)-1) <- c(0,0,0)
        color.at(dup_im2,num_rows-cnt%%(num_rows+1),ceiling(cnt/num_rows)+1) <- c(0,0,0)
        color.at(dup_im2,num_rows-cnt%%(num_rows+1)+1,ceiling(cnt/num_rows)-1) <- c(0,0,0)
        color.at(dup_im2,num_rows-cnt%%(num_rows+1)+1,ceiling(cnt/num_rows)) <- c(0,0,0)
        color.at(dup_im2,num_rows-cnt%%(num_rows+1)+1,ceiling(cnt/num_rows)+1) <- c(0,0,0)
      }
      im_for_video <- append(im_for_video,cimg2magick(dup_im2))
      cnt <- cnt + 1
    }
  } else { # the pixels move in a circle, in or out. Simply the values of x_tracker, y_tracker.
    if (statement == "circles in") {
      cnt <- length(xy_tracker)
    }
    for (time in 1:length(xy_tracker)) {
      dup_im2 <- im2
      color.at(dup_im2,x_tracker[cnt],y_tracker[cnt]) <- c(255,255,255)
      color.at(dup_im2,x_tracker[cnt]-1,y_tracker[cnt]-1) <- c(0,0,0) #
      color.at(dup_im2,x_tracker[cnt]-1,y_tracker[cnt]) <- c(0,0,0)
      color.at(dup_im2,x_tracker[cnt]-1,y_tracker[cnt]+1) <- c(0,0,0)
      color.at(dup_im2,x_tracker[cnt],y_tracker[cnt]-1) <- c(0,0,0)
      color.at(dup_im2,x_tracker[cnt],y_tracker[cnt]+1) <- c(0,0,0)
      color.at(dup_im2,x_tracker[cnt]+1,y_tracker[cnt]-1) <- c(0,0,0)
      color.at(dup_im2,x_tracker[cnt]+1,y_tracker[cnt]) <- c(0,0,0)
      color.at(dup_im2,x_tracker[cnt]+1,y_tracker[cnt]+1) <- c(0,0,0)
      im_for_video <- append(im_for_video,cimg2magick(dup_im2))
      if (statement == "circles in") {
        cnt <- cnt - 1
      } else {cnt <- cnt + 1}
    }
    cnt <- length(xy_tracker)
  }
  framerate <- cnt/(frames/((clock)/(tempo_avg/1000000))) # one frame is about one quarter note, which is the average length of all notes combined.
  image_write_video(im_for_video, "Pixel Video.wmv", framerate = framerate) # luckily, framerate can be a decimal.
  # Line 420 creates the video, saves it to your working directory. I find .wmv files are quickest and easiest to manipulate.
}

################################### Printed outputs for the user
if (statement != "rows") {
  print(paste("# of rows:",num_rows,". # of columns:",num_cols,"."))
} else {
  print(paste("# of rows:",num_cols,". # of columns:",num_rows,"."))
}

end.time <- Sys.time()
print(round(end.time - start.time,1)) # estimated runtime of this R file

### END