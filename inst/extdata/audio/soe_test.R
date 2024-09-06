fn <- 'inst/extdata/audio/1.wav'

snd <- tuneR::readWave(fn)
sr <- snd@samp.rate
snd <- snd@left
#plot(snd, type='l')
f0 <- wrassp::ksvF0(fn, toFile=F)
f0 <- f0$F0[,1]
f0[which(f0==0)] <- NA
#plot(f0, type='l')
mean_f0 <- mean(sr/f0, na.rm=T)
# mean_f0 <- 400

### differenced signal

sig <- stats::filter(snd,
                     filter = c(1, -1),
                     method='recursive')
# sig <- snd

# sig <- signal::filtfilt(c(1, -1), 1, snd)
#plot(sig, type='l')
#lines(snd, col='red')

### 1st ZFR

zfr1_filt <- signal::filter(1, c(1, -2*0.999, 0.999^2), sig)
#plot(zfr1_filt, type='l')

n0 <- round(mean_f0/1.5)
wid <- 2*n0+1
len <- length(sig)

a <- signal::filter(rep(1, wid)/wid, 1, zfr1_filt)
#plot(a, type='l')
# abegin <- cumsum(sig[1:(wid-2)])
# abegin <- abegin[seq(1, length(abegin), by=2)] / seq(1, wid-2, by=2)
# aend <- cumsum(sig[len:(len-wid+3)])
# aend <- aend[seq(length(aend), 1, by=-2)] / seq(wid-2, 1, by=-2)
# a <- c(abegin, a[wid:length(a)], aend)



# zfr1_trendRem <- zfr1_filt - a
#plot(zfr1_trendRem, type='l')


### 2nd ZFR

zfr2_filt <- signal::filter(1, c(1, -2*0.999, 0.999^2), zfr1_filt)
# zfr2_filt <- signal::filter(1, c(1, -2*0.999, 0.999^2), zfr1_trendRem)
#plot(zfr2_filt, type='l')

a <- signal::filter(rep(1, wid)/wid, 1, zfr2_filt)
#plot(a, type='l')
abegin <- cumsum(zfr1_filt[1:(wid-2)])
abegin <- abegin[seq(1, length(abegin), by=2)] / seq(1, wid-2, by=2)
aend <- cumsum(zfr1_filt[len:(len-wid+3)])
aend <- aend[seq(length(aend), 1, by=-2)] / seq(wid-2, 1, by=-2)
# aend <- rep(0)
a <- c(abegin, a[wid:length(a)], aend)

zfr_out <- zfr2_filt - a
#plot(zfr_out, type='l')

z <- 0.95*zfr_out[1:(length(zfr_out)-wid)]/max(abs(zfr_out[1:(length(zfr_out)-wid)]))
#plot(z, type='l')

z1 <- c(NA, z[1:length(z)-1])
# plot(z1[30000:40000], type='l')

# tf <- z1 > 0.00005 & z<=-0.00005
tf <- z1 < 0 & z>=0
# abline(v=which(tf)-30000, col='red')

# plot(snd[30000:40000], type='l')
# abline(v=which(tf)-30000, col='red')

par(mfrow = c(4, 1))

plot(NULL, xlim=c(0, len), ylim=c(-2, 2), ylab='')
abline(v=which(tf), col='lightgrey')
lines((0.95*snd/max(abs(snd)))-1, col='red')
lines(z1+1, col='blue')

pulses <- which(tf)
pitch <- data.frame(
  pulses = pulses / sr,
  diff = c(NA, pulses[-1] / sr - pulses[-length(pulses)] / sr)
)
pitch$f0 <- 1 / pitch$diff
pitch$t <- pitch$pulses - (pitch$diff / 2)
pitch <- pitch[-1,]
plot(pitch$t, pitch$f0, xlim=c(0, len / sr), ylim=c(50,250), pch=20)

ts <- data.frame(f0 = signal::interp1(pitch$t, pitch$f0,
                                      seq(0, len / sr, by = 0.005)),
                 t = seq(0, len / sr, by = 0.005))
ts$orig <- pitch$t[findInterval(ts$t, pitch$t, all.inside=T)]
# kil <- pitch[which(pitch$f0 < 50)-1,'t']
# kil <- which(pitch$f0 < 50)
# kil <- pitch[c(kil, kil-1),'t']
# ts[which(ts$orig %in% kil),'f0'] <- NA

# plot(ts$f0, ylim=c(50,250), pch=20)

dx <- c()
for (i in 1:length(pulses)) {
  dx[i] <- lm(z1[(pulses[i]-5):(pulses[i]+5)]~c(1:11))$coefficients[2]
  # numer <- 0
  # denom <- 0
  # for (theta in 1:5) {
  #   numer <- numer + theta*(z1[pulses[i]+theta] - z1[pulses[i]-theta])
  #   denom <- denom + 2*theta^2
  # }
  # dx[i] <- (numer / denom)
}

# par(mfrow=c(2,1))
pitch$seo <- dx[-1]
ts$seo <- signal::interp1(pitch$t, dx,
                          seq(0, len / sr, by = 0.005))
# ts[which(ts$orig %in% kil),'seo'] <- NA
# plot(ts$seo, pch=20)

kil <- which(pitch$seo < 0.005)
kil <- pitch[c(kil, kil-1),'t']
ts[which(ts$orig %in% kil),'f0'] <- NA

plot(ts$f0, ylim=c(50,250), pch=20)
plot(ts$seo, pch=20)

# matlab_comp <- read.table('inst/extdata/audio/output.txt')
# colnames(matlab_comp) <- matlab_comp[1,]
# matlab_comp <- matlab_comp[-1,]
# plot(matlab_comp[which(matlab_comp$Filename == '1.mat'),]$t_ms,
#  matlab_comp[which(matlab_comp$Filename == '1.mat'),]$soe, pch=20,
#  xlim = c(0, (len/sr)*1000))
#
# plot(pitch$t, dx[-1], pch=20, xlim=c(0, len/sr))
#
# plot(matlab_comp[which(matlab_comp$Filename == '1.mat'),]$t_ms,
#      matlab_comp[which(matlab_comp$Filename == '1.mat'),]$sF0, pch=20,
#      xlim = c(0, (len/sr)*1000), ylim=c(60,180))
# abline(h=seq(0,200,by=10), lty='dotted')
# plot(ts$t, ts$f0, pch=20, xlim=c(0, len/sr), ylim=c(60,180))
# abline(h=seq(0,200,by=10), lty='dotted')
