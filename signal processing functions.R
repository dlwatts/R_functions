
library(tuneR)


# --------------------------------------------------------------------------
# GENERALIZED PLOT/INFO
# --------------------------------------------------------------------------

import.wav <- function(file.loc, file.name, ifverbose=F){
  # function to import .wav files
  soundobj <- readWave(paste0(file.loc, file.name), from=5000, units='samples')
  if(ifverbose==T){
    # inspect the properties
    str(soundobj)
    summary(soundobj)
    # how many seconds it is by
    cat("\nclip is", length(soundobj@left)/soundobj@samp.rate, "seconds\n")
  }
  return(soundobj)
}

sound.to.amplitude <- function(soundobj){
  # convert a wav sound file to dB & Hz
  # seperating out the channels
  s1 <- soundobj@left
  # readWave reads wav files as integer types. 
  # For some reason we need to convert the sound array to floating point values ranging from -1 to 1
  s1 <- s1/2^(soundobj@bit - 1)
  
#  if(soundobj@stereo==TRUE){
#    s2 <- soundobj@right
#    s2 <- s2/2^(soundobj@bit - 1)
#  }
  
  # time representation of the sound-- pressure values agains the time axis
  # make the time
  timeArray <- (0:(length(soundobj@left) - 1))/soundobj@samp.rate
  # scale to milliseconds
  timeArray <- timeArray * 1000
#  if(soundobj@stereo==TRUE){  
#  return(data.frame(
#    "ms" = timeArray,
 #   "s1" = s1,
#    "s2" = s2)
#    )
#  }else{
    return(data.frame(
      "ms" = timeArray,
      "s1" = s1)
    )
#      }
}

db.Hz <- function(soundamp, samp.rate=soundobj@samp.rate, ifStereo = soundobj@stereo) {
  # use the output from sound.to.amplitude 
  # samp.rate comes from the original soundobj # soundobj@samp.rate
  n <- length(soundamp$s1)
  # fast fourier transform
  p1 <- fft(soundamp$s1)
  nUniquePts <- ceiling((n + 1)/2)
  # select just the first 1/2 since the second 1/2 is a mirror image of the first
  p1 <- p1[1:nUniquePts]
  # fft of the tone contains both magnitude and phase information and is given a complex representation
  # by taking abs value of the fft we get the information about the magnitude of the frequency components
  # scale by the n of pts so that the magnitude does not depend on the length of the signal or on its sampling freq
  p1 <- p1/n
  # square it to get the power
  p1 <- p1^2
  
#  if(ifStereo==TRUE){
#    p2 <- fft(soundamp$s2)
#    p2 <- p2[1:nUniquePts]
#    p2 <- p2/n
#    p2 <- p2^2
#  }
  
  # multiply by two (I have no idea why, but it's in some technical documentation somewhere)
  # odd nfft excludes Nyquist point
  if (n%%2 > 0) {
    # we've got odd number of points fft
    p1[2:length(p1)] <- p1[2:length(p1)] * 2
#    if(ifStereo==TRUE){p2[2:length(p2)] <- p2[2:length(p2)] * 2}
  } else {
    # we've got even number of points fft
    p1[2:(length(p1) - 1)] <- p1[2:(length(p1) - 1)] * 2
#    if(ifStereo==TRUE){p2[2:(length(p2) - 1)] <- p2[2:(length(p2) - 1)] * 2}
    }
  
  #  create the frequency array
  freqArray <- (0:(nUniquePts - 1)) * (samp.rate/n)
#  if(ifStereo==TRUE){
#  return(data.frame(
#    "freq" = freqArray,
#    "dB1" = 10 * log10(p1),
#    "dB2" = 10 * log10(p2)
#  ))
#  } else {
    return(data.frame(
      "freq" = freqArray,
      "dB1" = 10 * log10(p1)))
#  }
}

snd.prcs <- function(soundobj) {
  # gives a list of 2 dataframes
  # return[[1]] is the amplitude x time (ms)
  # return[[2]] is the power (dB) x frequency (Hz)
  
  # convert the wav sound object to a time series x amplitude for each channel
  ts.comp <-sound.to.amplitude(soundobj)
  # convert the object from that to the fft Hz x dB
  fft.conv <- db.Hz(ts.comp, samp.rate=soundobj@samp.rate)

 return(list(ts.comp, fft.conv))
}


sound.plot <- function(soundlist, plotName){
  ifStereo = ifelse(ncol(soundlist[[1]])<3, FALSE, TRUE)
    # plot the tones
#  if(ifStereo==TRUE){
#    par(mfrow = c(2, 2))
#  }else{
    par(mfrow=c(2,1))
#  }
  par(oma=c(0,0,0,0))
  par(mar=c(4,4,3, 0.5) + 0.1)
    plot(soundlist[[1]]$ms, soundlist[[1]]$s1, 
         type = "l", col = "black", xlab = "Time (ms)", ylab = "Amplitude", 
         main = paste("left channel\n", plotName), cex.main=0.8)
    legend("topright", legend = paste("entropy = ", shannon.entropy(soundlist[[1]]$s1)), bty = "n")
#    if(ifStereo==TRUE){
#    plot(soundlist[[1]]$ms, soundlist[[1]]$s2, 
#         type = "l", col = "black", xlab = "Time (ms)", ylab = "Amplitude", 
#         main = paste("right channel\n", plotName), cex.main=0.8)
#    legend("topright", legend = paste("entropy = ", shannon.entropy(soundlist[[1]]$s2)), bty = "n")
#    }
    # plotting root mean square of the signal, which can be seen as a measure of the amplitude of the waveform
    # plotting the power in decibals by taking the log(p) and clipping to the Hz between 0 and 1000
    plot(soundlist[[2]]$freq, soundlist[[2]]$dB1, type = "l", col = "black", xlab = "Frequency (Hz)", ylab = "Power (dB)", 
         main = paste("left channel"), cex.main=0.8, xlim=c(0, 1000))
    legend("topright", legend = paste("rms = ", rms(soundlist[[1]]$s1)), bty = "n")
#    if(ifStereo==TRUE){
#    plot(soundlist[[2]]$freq, soundlist[[2]]$dB2, type = "l", col = "black", xlab = "Frequency (Hz)", ylab = "Power (dB)", 
#         main = paste("right channel"), cex.main=0.8, xlim=c(0, 1000))
#    legend("topright", legend = paste("rms = ", rms(soundlist[[1]]$s2)), bty = "n")
#    }
}

pft <- function(audio){
  # the frequency of your audio file
  freq <- audio@samp.rate
  # the length and duration of your audio file
  totlen <- length(audio)
  totsec <- totlen/freq
  
  # the duration that you want to chop the file into
  seglen <- 2
  
  # defining the break points
  breaks <- unique(c(seq(0, totsec, seglen), totsec))
  index <- 1:(length(breaks)-1)
  # a list of all the segments
  pieces <- lapply(index, function(i) audio[(breaks[i]*freq):(breaks[i+1]*freq)])
  pieces.1 <- lapply(pieces, function(x) sound.to.amplitude(x))
  Hzpieces <- lapply(pieces.1, function(x) db.Hz(x, freq))
  peaks <- lapply(Hzpieces, function(x) peakHz(x))
  dplyr::bind_rows(peaks)

  # apply shannon entropy function
  #  se <- lapply(pieces, function(x) shannon.entropy(x@left))
  #  return(unlist(se))
}



# --------------------------------------------------------------------------
# CALCULATIONS
# --------------------------------------------------------------------------

shannon.entropy <- function(x){
  freqs <- table(x)/length(x)
  freqs <- as.data.frame(freqs)[,2]
  round(-sum(freqs * log2(freqs)), 2)
}

rms <- function(s){
  # since the rms is equal to the square root of the overall power of the signal, 
  # summing the power values (p1 & p2) calculated with the fft over all frequencies and 
  # taking the square root of this sum would give a similar value
  # usually done on the amplitude/time signal
 return(
   round(
     sqrt(mean(s^2)), 
     3)
   )
}

peakHz <- function(dBHz){
  # find the Hz for the peak dB
  do.i <- tryCatch(round(dBHz$freq[which.max(dBHz$dB1)], 1), error=function(e) NA)
  peakHz_L <- ifelse(length(do.i)>0, do.i, NA)
#  do1.i <- tryCatch(round(dBHz$freq[which.max(dBHz$dB2)], 1), error=function(e) NA)
 # peakHz_R <- ifelse(length(do1.i)>0, do1.i, NA)
#  return(data.frame(peakHz_L = peakHz_L, peakHz_R = peakHz_R))
  return(data.frame(peakHz_L = peakHz_L))
}
