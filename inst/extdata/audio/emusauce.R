emusauce <- function(snd, f0min=50, f0max=300, timestep=5,
                     windowLength=25) {

  bw <- (1 / (windowLength / 1000)) / 2
  derived_sr <- 1 / (timestep / 1000)

  f0 <- wrassp::ksvF0(snd, toFile=F, verbose=F,
                      minF=f0min, maxF=f0max)
  sr <- attr(f0, 'origFreq')
  startTime <- attr(f0, 'startTime')
  f0 <- f0$F0[,1]
  f0[which(f0 == 0)] <- NA
  nFrames <- length(f0)
  endTime <- startTime + (nFrames * timestep / 1000)

  t <- seq(startTime, endTime, length.out=nFrames)

  fm <- wrassp::forest(snd, toFile=F, verbose=F, numFormants=5)
  f1 <- fm$fm[,1]; f1[which(f1 == 0)] <- NA
  f2 <- fm$fm[,2]; f2[which(f2 == 0)] <- NA
  f3 <- fm$fm[,3]; f3[which(f3 == 0)] <- NA
  b1 <- b2 <- b3 <- rep(NA, nFrames)
  b1[which(f1 < 500)] <-
    getbw_HawksMiller_lowFmt(f0[which(f1 < 500)], f1[which(f1 < 500)])
  b1[which(f1 > 500)] <-
    getbw_HawksMiller_highFmt(f0[which(f1 > 500)], f1[which(f1 > 500)])
  b2[which(f2 < 500)] <-
    getbw_HawksMiller_lowFmt(f0[which(f2 < 500)], f2[which(f2 < 500)])
  b2[which(f2 > 500)] <-
    getbw_HawksMiller_highFmt(f0[which(f2 > 500)], f2[which(f2 > 500)])
  b3[which(f3 > 500)] <-
    getbw_HawksMiller_highFmt(f0[which(f3 > 500)], f3[which(f3 > 500)])

  h1db <- h2db <- h4db <- a1db <- a2db <- a3db <-
    h1hz <- h2hz <- h4hz <- a1hz <- a2hz <- a3hz <-
    h2kdb <- h5kdb <- cpp <- rep(NA, nFrames)

  spec <- wrassp::dftSpectrum(snd, toFile=F, verbose=F)$dft
  freqSteps <- dim(spec)[2]
  freqRange <- seq(0, sr/2, length.out=freqSteps)

  cep <- wrassp::cepstrum(snd, toFile=F, verbose=F)$cep
  cep <- 20*log10(abs(cep))

  quefRange <- (0:(dim(cep)[2] - 1)) * (1 / bw / dim(cep)[2])
  quef2freq <- 2/quefRange
  cppAna <- data.frame(id = 1:dim(cep)[2],
                       qr = quefRange,
                       fr = quef2freq)

  for (i in 1:nFrames) {
    if (!is.na(f0[i])) {
      if ((f0[i] * 0.1) * 2 < bw + 5) {
        mult <- 0.2
      } else {
        mult <- 0.1
      }

      h1db[i] <- max(spec[i,which(freqRange > f0[i] - (f0[i]*mult) &
                                    freqRange < f0[i] + (f0[i]*mult))])
      h2db[i] <- max(spec[i,which(freqRange > (f0[i]*2) - (f0[i]*mult) &
                                    freqRange < (f0[i]*2) + (f0[i]*mult))])
      h4db[i] <- max(spec[i,which(freqRange > (f0[i]*4) - (f0[i]*mult) &
                                    freqRange < (f0[i]*4) + (f0[i]*mult))])
      h2kdb[i] <- max(spec[i,which(freqRange > 2000 - f0[i],
                                   freqRange < 2000 + f0[i])])
      h5kdb[i] <- max(spec[i,which(freqRange > 5000 - f0[i],
                                   freqRange < 5000 + f0[i])])
      if (!is.na(f1[i])) {
        a1db[i] <- max(spec[i,which(freqRange > f1[i] - (f1[i]*0.2) &
                                      freqRange < f1[i] + (f1[i]*0.2))])
      }
      if (!is.na(f2[i])) {
        a2db[i] <- max(spec[i,which(freqRange > f2[i] - (f2[i]*0.1) &
                                      freqRange < f2[i] + (f2[i]*0.1))])
      }
      if (!is.na(f3[i])) {
        a3db[i] <- max(spec[i,which(freqRange > f3[i] - (f3[i]*0.1) &
                                      freqRange < f3[i] + (f3[i]*0.1))])
      }
      cppAna_tmp <- cppAna
      cppAna_tmp$cep <- cep[i,]
      cppAna_tmp <- cppAna_tmp[-which(cppAna_tmp$qr < 0.001),]
      cp <- max(cppAna_tmp[which(cppAna_tmp$fr > f0[i] - (f0[i]*mult) &
                                   cppAna_tmp$fr < f0[i] + f0[i]*mult),'cep'])
      cpId <- which(cppAna_tmp$cep == cp)
      cppAna_tmp$fit <- lm(cep ~ qr, data=cppAna_tmp)$fitted.values
      cpp[i] <- abs(cppAna_tmp$cep[cpId] - cppAna_tmp$fit[cpId])
    }
  }

  h1c <- h1db - correct_iseli(f0, f1, b1, sr) - correct_iseli(f0, f2, b2, sr)
  h2c <- h2db - correct_iseli(f0*2, f1, b1, sr) - correct_iseli(f0*2, f2, b2, sr)
  h4c <- h4db - correct_iseli(f0*4, f1, b1, sr) - correct_iseli(f0*4, f2, b2, sr)
  a1c <- a1db - correct_iseli(f1, f1, b1, sr) - correct_iseli(f1, f2, b2, sr)
  a2c <- a2db - correct_iseli(f2, f1, b1, sr) - correct_iseli(f2, f2, b2, sr)
  a3c <- a3db - correct_iseli(f3, f1, b1, sr) - correct_iseli(f3, f2, b2, sr) -
    correct_iseli(f3, f3, b3, sr)

  h1h2u <- h1db - h2db; h1h2c <- h1c - h2c
  h2h4u <- h2db - h4db; h2h4c <- h2c - h4c
  h1a1u <- h1db - a1db; h1a1c <- h1c - a1c
  h1a2u <- h1db - a2db; h1a2c <- h1c - a2c
  h1a3u <- h1db - a3db; h1a3c <- h1c - a3c
  h2kh5k <- h2kdb - h5kdb

  res <- data.frame(file = rep(snd, nFrames), t = t,
                    f0 = f0, F1 = f1, F2 = f2, F3 = f3,
                    B1 = b1, B2 = b2, B3 = b3,
                    H1u = h1db, H2u = h2db, H4u = h4db,
                    H1c = h1c, H2c = h2c, H4c = h4c,
                    A1u = a1db, A2u = a2db, A3u = a3db,
                    A1c = a1c, A2c = a2c, A3c = a3c,
                    H1H2u = h1h2u, H2H4u = h2h4u,
                    H1A1u = h1a1u, H1A2u = h1a2u, H1A3u = h1a3u,
                    H1H2c = h1h2c, H2H4c = h2h4c,
                    H1A1c = h1a1c, H1A2c = h1a2c, H1A3c = h1a3c,
                    H2KH5Ku = h2kh5k, CPP = cpp
  )
  return(res)

}

getbw_HawksMiller_highFmt <- function(f0, fmt) {
  s <- 1 + 0.25*(f0-132)/88
  k <- 15.8146139
  coef <- c(8.10159009e-2, -9.79728215e-5, 5.28725064e-8,
            -1.07099364e-11, 7.91528509e-16)
  fbw <- s * (k + (coef[1] * fmt) + (coef[2] * fmt^2) + (coef[3] * fmt^3) +
                (coef[4] * fmt^4) + (coef[5] * fmt^5) )
  return(fbw)
}

getbw_HawksMiller_lowFmt <- function(f0, fmt) {
  s <- 1 + 0.25*(f0-132)/88
  k <- 165.327516
  coef <- c(-6.73636734e-1, 1.80874446e-3, -4.52201682e-6,
            7.49514000e-9, -4.70219241e-12)
  fbw <- s * (k + (coef[1] * fmt) + (coef[2] * fmt^2) + (coef[3] * fmt^3) +
                (coef[4] * fmt^4) + (coef[5] * fmt^5) )
  return(fbw)
}

correct_iseli <- function(f, fx, bx, fs) {
  r <- exp(-pi*bx/fs)
  omega_x <- 2*pi*fx/fs
  omega <- 2*pi*f/fs
  a <- r ^ 2 + 1 - 2*r*cos(omega_x + omega)
  b <- r ^ 2 + 1 - 2*r*cos(omega_x - omega)
  corr <- -10*(log10(a)+log10(b))
  numerator <- r ^ 2 + 1 - 2 * r * cos(omega_x)
  corr <- -10*(log10(a)+log10(b)) + 20*log10(numerator)
  return(corr)
}

es2 <- emusauce('inst/extdata/audio/1.wav')

compare_emu_praat <- function(file, es_obj, signal) {
  fn <- paste0(file, '.', signal)
  ps <- wrassp::read.AsspDataObj(fn)
  ps <- unlist(ps[1])
  ps[which(ps == 0)] <- NA
  es <- unlist(es_obj[signal])
  t <- es_obj$t

  lowerLim <- min(es, ps, na.rm=T)
  upperLim <- max(es, ps, na.rm=T)

  plot(x=t, y=es, type='l', col='red', ylim=c(lowerLim, upperLim),
       ylab=signal)
  lines(x=seq(0, max(t), length.out=length(ps)), y=ps, col='blue')
  mtext('EMU', col='red', line=2, adj=0)
  mtext('Praat', col='blue', line=1, adj=0)
}

compare_emu_praat('F3-0004-car-rep1-buh-23', es, 'CPP')

### bulk-process

emusauce_bulk <- function() {
  fls <- list.files(getwd(), pattern = '*.wav')

  res <- data.frame(file = NA, t = NA,
                    f0 = NA, F1 = NA, F2 = NA, F3 = NA,
                    B1 = NA, B2 = NA, B3 = NA,
                    H1u = NA, H2u = NA, H4u = NA,
                    H1c = NA, H2c = NA, H4c = NA,
                    A1u = NA, A2u = NA, A3u = NA,
                    A1c = NA, A2c = NA, A3c = NA,
                    H1H2u = NA, H2H4u = NA,
                    H1A1u = NA, H1A2u = NA, H1A3u = NA,
                    H1H2c = NA, H2H4c = NA,
                    H1A1c = NA, H1A2c = NA, H1A3c = NA,
                    H2KH5Ku = NA, CPP = NA)

  for (f in fls) {
    tmp <- emusauce(f, f0max=600)
    res <- rbind(res, tmp)
    print(paste0(f, ' done!'))
  }

  return(res[-1,])

}

maxF0 <- 600
minF0 <- 75
sr <- 44100

acfAna <- wrassp::acfana(fl, toFile=F, verbose=F, analysisOrder=(sr*0.06),
                         windowSize=60, window='HANN')$acf
f0Ana <- wrassp::ksvF0(fl, toFile=F, verbose=F)$F0

len <- length(f0Ana)

maxPeakSamp <- 1/(maxF0/sr)
minPeakSamp <- 1/(minF0/sr)

hnr <- f0res <- peakEnergy <- rep(NA, len)

for (i in 1:len) {
  # acfAnaCentered <- acfAna[i,] - mean(acfAna[i,])
  acfAnaScaled <- scales::rescale(acfAna[i,], c(0,1))
  peak[i] <- which.max(acfAnaScaled[maxPeakSamp:minPeakSamp]) + maxPeakSamp
  peakEnergy[i] <- acfAnaScaled[peak[i]]
  # f0res[i] <- 1/(peak/sr)
  # hnr[i] <- 10*log10(peakEnergy[i]/(1-peakEnergy[i]))
}

f0res <- 1/(peak/sr)
hnr <- 10*log10(peakEnergy/(1-peakEnergy))

plot(hnr, type='l', ylab='HNR')

### This one includes correction with a normalized autocorrelation of Hann
### window. This tends to lead to HNR (prop) values above 1, also mentioned by
### Boersma 1993. He suggests 1 / HNR before converting to dB in these cases.
### This doesn't work very well, as it leads to valleys -- 1.02 will give
### a smaller value than 1.01 which is not desired. Instead I'm resampling all
### values above 0.999 to between 0.999--0.9999 (30--40 dB). It's a hack, but
### it seems effective enough.

fl <- list.files(pattern='*.wav')
maxF0 <- 600
minF0 <- 50
sr <- 44100

acfAna <- wrassp::acfana(fl, toFile=F, verbose=F, analysisOrder=(sr*0.06),
                         windowSize=60, window='HANN')$acf
f0Ana <- wrassp::ksvF0(fl, toFile=F, verbose=F)$F0
# t <- seq(attr(f0Ana, 'startTime'),
#          attr(f0Ana, 'startTime') + (0.005*306),
#          length.out=306)

nSamp <- dim(acfAna)[2]
x <- 1:nSamp
nAutocorHann <- (1 - (x / nSamp)) * ((2/3) + (1/3)*cos((2*pi*x)/nSamp)) +
  (1/(2*pi))*sin((2*pi*x)/nSamp)

len <- length(f0Ana)

maxPeakSamp <- 1/(maxF0/sr)
minPeakSamp <- 1/(minF0/sr)

hnr <- f0res <- peakEnergy <- rep(NA, len)

for (i in 1:len) {
  acfAnaCentered <- acfAna[i,] - mean(acfAna[i,])
  acfAnaScaled <- scales::rescale(acfAnaCentered, c(-1, 1),
                                  from=c(-max(acfAnaCentered),
                                         max(acfAnaCentered)))
  reWindowed <- acfAnaScaled / nAutocorHann
  peak <- which.max(acfAnaScaled[maxPeakSamp:minPeakSamp]) + maxPeakSamp
  peakEnergy[i] <- reWindowed[peak]
  # if (peakEnergy[i] > 1) peakEnergy[i] <- 1 / peakEnergy[i]
  f0res[i] <- 1/(peak/sr)
  # hnr[i] <- 10*log10(peakEnergy[i]/(1-peakEnergy[i]))
}

# peakEnergy[which(peakEnergy > 0.999)] <-
  scales::rescale(peakEnergy[which(peakEnergy > 0.999)], c(0.999, 0.9999))
# peakEnergy <- scales::rescale(peakEnergy, c(0.999, 0.9999))
peakEnergy[which(peakEnergy > 1)] <- 1 / peakEnergy[which(peakEnergy > 1)]
hnr <- 10*log10(peakEnergy/(1-peakEnergy))

# praatRes <- read.csv('test.csv')
# praat_t <- seq(0.06022, 1.47522, length.out=284)

plot(hnr, type='l', ylab='HNR', ylim=c(-10, 40))

hnr35fl <- list.files(pattern='*.HNR35')
hnr35 <- wrassp::read.AsspDataObj(hnr35fl)$HNR35
lines(hnr35, col='red')
# smoothed <- emuR::dct(hnr, m=306/2, fit=T)
# plot(smoothed, type='l')

###

i <- 150


# acfAnaScaled <- scales::rescale(acfAna[i,], c(0,1))
acfAnaScaled <- acfAna[i,] - mean(acfAna[,i])
acfAnaScaled <- scales::rescale(acfAnaScaled, c(-1, 1),
                                from=c(-max(acfAnaScaled), max(acfAnaScaled)))
peak <- which.max(acfAnaScaled[maxPeakSamp:minPeakSamp]) + maxPeakSamp
peakEnergy[i] <- acfAnaScaled[peak]
f0res[i] <- 1/(peak/sr)
# hnr[i] <- 10*log10(peakEnergy[i]/(1-peakEnergy[i]))
plot(acfAnaScaled, type='l')
abline(v=minPeakSamp, col='blue', lty='dotted')
abline(v=maxPeakSamp, col='blue', lty='dotted')
abline(v=peak, col='red', lty='dotted')
print(f0res[i]); print(f0Ana[i])

nAutocorHann <- (1 - (x / nSamp)) * ((2/3) + (1/3)*cos((2*pi*x)/nSamp)) +
  (1/(2*pi))*sin((2*pi*x)/nSamp)
reWindowed <- acfAnaScaled / nAutocorHann
hnr <- 10*log10(reWindowed[peak] / (1-reWindowed[peak]))


###

nSamp <- length(acfAnaScaled)
x <- 1:nSamp
nAutocorHann <- (1 - (x / nSamp)) * ((2/3) + (1/3)*cos((2*pi*x)/nSamp)) +
  (1/(2*pi))*sin((2*pi*x)/nSamp)
plot(nAutocorHann, type='l')
plot(acfAnaScaled, type='l')
test <- acfAnaScaled / nAutocorHann
plot(test, type='l')
test <- test[1:(nSamp/2)]
plot(test, type='l')

plot(test, type='l', xlim=c(0,600), ylim=c(-1, 1))
plot(test, type='l')

###

bp05 <- wrassp::affilter(fl, highPass=0, lowPass=500)
newFile <- paste0(unlist(strsplit(fl, '.wav')), '.lpf')
acfAna <- wrassp::acfana(newFile, toFile=F, verbose=F, analysisOrder=(sr*0.02),
                         windowSize=60, window='HANN')$acf

len <- length(f0Ana)

maxPeakSamp <- 1/(maxF0/sr)
minPeakSamp <- 1/(minF0/sr)

hnr <- f0res <- peakEnergy <- rep(NA, len)

for (i in 1:len) {
  acfAnaScaled <- scales::rescale(acfAna[i,], c(0,1))
  peak <- which.max(acfAnaScaled[maxPeakSamp:minPeakSamp]) + maxPeakSamp
  peakEnergy[i] <- acfAnaScaled[peak]
  f0res[i] <- 1/(peak/sr)
  hnr[i] <- 10*log10(peakEnergy[i]/(1-peakEnergy[i]))
}
lines(hnr, col='red')

unlink(newFile)
text(0, 20, 'HNR05', col='red', adj=0)
text(0, 19, 'no filter', adj=0)

x <- 1:100
plot((1 - (x / 100)) * ((2/3) + (1/3)*cos((2*pi*x)/100)) + (1/(2*pi))*sin((2*pi*x)/100),
     type='l')
