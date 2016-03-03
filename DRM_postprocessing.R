
###############################
# 1. Reset Default Plot Display
###############################

# Reset default setting of plot display (one figure instead of four figures)
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar()) 


#########################
# 2. Preprocess DRM Data
#########################

# Subtract the fixed flux at the right boundary
drm0=-Qdrm-right.discharge 

# Make a time series of the same length as drm0 dataset
tt=data.frame(seq(1,nrow(mod),length.out = nrow(data.frame(drm0))))
colnames(tt) <- c("tt")

# Make function of drm
drm=approx(tt$tt,drm0, xout = 1:nrow(mod))$y

# Delete first row so it will have the same length as forc dataset
mod=mod[-1,]
drm=drm[-1]

##################
# 3. Calculate NS 
##################

# Calculate the performance of WALRUS 
NS.walrus= 1- sum((mod$Q - forc$Q)^2, na.rm=TRUE) / 
  sum((forc$Q- mean(forc$Q, na=TRUE))^2, na.rm=TRUE)

# Calculate the performance of DRM 
NS.drm= 1- sum((drm - forc$Q)^2, na.rm=TRUE) / 
  sum((forc$Q- mean(forc$Q, na=TRUE))^2, na.rm=TRUE)


##########
# 4. Plot
##########

Sys.setlocale("LC_TIME", "C")
plot(strptime(forc$date,"%Y%m%d%H"), mgp = c(2.5, 1, 0), mod$Q , type="l", 
     ylab=expression(paste("Discharge [mm h"^{-1},"]")), xlab="Date", col="2")
lines(strptime(forc$date,"%Y%m%d%H"),forc$Q) 
lines(strptime(forc$date,"%Y%m%d%H"),drm, col=3)
legend("topleft", c( "Qobs", "Qwalrus", "Qdrm"), col=c(1:3), lwd=1, bg='white', box.col = "white")
legend("topright",c(paste(  "NS WALRUS:", round(NS.walrus, digits = 2)),
                    paste("NS DRM:", round(NS.drm, digits = 2))), bty="n",  cex=0.8)
box(which = "plot", lty = "solid")


