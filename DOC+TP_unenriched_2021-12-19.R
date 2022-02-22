# Estimate DOC:TP for unenriched lakes
# SRC 2021-12-14

rm(list = ls())
graphics.off()

# Organize DOC

doc0 = read.csv(file='cascade_carbon_v0.3.csv')
print(doc0[1,])

doc1 = subset(doc0,select=c(lakeid,year4,daynum,depth_id,doc))

Rdoc0 = subset(doc1,subset=(lakeid=='R' & depth_id<= 3))
Rdoc1 = subset(Rdoc0,subset=(year4>=2003 & year4<=2012))
mindoy = min(Rdoc1$daynum,na.rm=T)
maxdoy = max(Rdoc1$daynum,na.rm=T)
Rdoc1$tstep = Rdoc1$year4 + ( (Rdoc1$daynum - mindoy)/((maxdoy-mindoy)+1) )
#
Tdoc0 = subset(doc1,subset=(lakeid=='T' & depth_id<= 3))
Tdoc1 = subset(Tdoc0,subset=(year4>=2003 & year4<=2012))
mindoy = min(Tdoc1$daynum,na.rm=T)
maxdoy = max(Tdoc1$daynum,na.rm=T)
Tdoc1$tstep = Tdoc1$year4 + ( (Tdoc1$daynum - mindoy)/((maxdoy-mindoy)+1) )
#
windows()
par(mfrow=c(2,1),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
plot(Rdoc1$tstep,Rdoc1$doc,type='p',pch=19,col='blue',xlab='Day',
     ylab='DOC, Peter')
plot(Tdoc1$tstep,Tdoc1$doc,type='p',pch=19,col='red',xlab='Day',
     ylab='DOC, Tuesday')

# Organize TP
tp0 = read.csv(file='cascade_nutrients_v0.3.csv')
print(tp0[1,])

tp1 = subset(tp0,select=c(lakeid,year4,daynum,depth_id,tn_ug,tp_ug))

Rtp0 = subset(tp1,subset=(lakeid=='R' & depth_id<= 3))
#Rtp1 = subset(Rtp0,subset=(year4>=2003 & year4<=2012))
Rtp1 = subset(Rtp0,subset=(year4==2012))
Rtp1$tp_ug = ifelse(Rtp1$tp_ug>0,Rtp1$tp_ug,NA)

mindoy = min(Rtp1$daynum,na.rm=T)
maxdoy = max(Rtp1$daynum,na.rm=T)
Rtp1$tstep = Rtp1$year4 + ( (Rtp1$daynum - mindoy)/((maxdoy-mindoy)+1) )
#
Ttp0 = subset(tp1,subset=(lakeid=='T' & depth_id<= 3))
#Ttp1 = subset(Ttp0,subset=(year4>=2003 & year4<=2012))
Ttp1 = subset(Ttp0,subset=(year4==2012))
Ttp1$tp_ug = ifelse(Ttp1$tp_ug>0,Ttp1$tp_ug,NA)
mindoy = min(Ttp1$daynum,na.rm=T)
maxdoy = max(Ttp1$daynum,na.rm=T)
Ttp1$tstep = Ttp1$year4 + ( (Ttp1$daynum - mindoy)/((maxdoy-mindoy)+1) )
#
windows()
par(mfrow=c(2,1),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
plot(Rtp1$tstep,Rtp1$tp_ug,type='p',pch=19,col='blue',xlab='Day',
     ylab='TP, Peter')
plot(Ttp1$tstep,Ttp1$tp_ug,type='p',pch=19,col='red',xlab='Day',
     ylab='TP, Tuesday')
#
windows()
par(mfrow=c(2,1),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
plot(Rtp1$tstep,Rtp1$tn_ug,type='p',pch=19,col='blue',xlab='Day',
     ylab='TN, Peter')
plot(Ttp1$tstep,Ttp1$tn_ug,type='p',pch=19,col='red',xlab='Day',
     ylab='TN, Tuesday')

# Attempt to pair DOC and nutrient data for Peter
Rdoc.tp0 = merge(Rdoc1,Rtp1,by=c('year4','daynum'))
Rdocnut = na.omit(Rdoc.tp0)
Rdocnut$CPmass = Rdocnut$doc*1000/Rdocnut$tp_ug
Rdocnut$CNmass = Rdocnut$doc*1000/Rdocnut$tn_ug
Rdocnut$NPmass = Rdocnut$tn_ug/Rdocnut$tp_ug

windows()
par(mfrow=c(2,1),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
plot(Rdocnut$tstep.x,Rdocnut$CPmass,log='y',type='p',pch=19,col='blue',xlab='Day',
     ylab='DOC:TP, Peter')
grid()
plot(Rdocnut$tstep.x,Rdocnut$CNmass,type='p',pch=19,col='blue',xlab='Day',
     ylab='DOC:TN, Peter')
grid()

# Attempt to pair DOC and nutrient data for Tuesday
Tdoc.tp0 = merge(Tdoc1,Ttp1,by=c('year4','daynum'))
Tdocnut = na.omit(Tdoc.tp0)
Tdocnut$CPmass = Tdocnut$doc*1000/Tdocnut$tp_ug
Tdocnut$CNmass = Tdocnut$doc*1000/Tdocnut$tn_ug
Tdocnut$NPmass = Tdocnut$tn_ug/Tdocnut$tp_ug

windows()
par(mfrow=c(2,1),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
plot(Tdocnut$tstep.x,Tdocnut$CPmass,log='y',type='p',pch=19,col='blue',xlab='Day',
     ylab='DOC:TP, Tuesday')
grid()
plot(Tdocnut$tstep.x,Tdocnut$CNmass,type='p',pch=19,col='blue',xlab='Day',
     ylab='DOC:TN, Tuesday')
grid()

meanse = function(x) {  # return mean, se, and N
        xbar = mean(x,na.rm=T)
        xsd = sd(x,na.rm=T)
        Nx = length(x)
        xse = xsd/sqrt(Nx)
        outvec = c(xbar,xse,Nx)
        return(outvec)
}

# Summarize DOC, P, N, DOC:P, DOC:N, N:P
Rnuts = matrix(0,nr=6,nc=3) # columns are mean, se, N; rows are the 6 variates
rdum = meanse(Rdocnut$doc)
Rnuts[1,] = rdum
rdum = meanse(Rdocnut$tp_ug)
Rnuts[2,] = rdum
rdum = meanse(Rdocnut$tn_ug)
Rnuts[3,] = rdum
rdum = meanse(Rdocnut$CPmass)
Rnuts[4,] = rdum
rdum= meanse(Rdocnut$CNmass)
Rnuts[5,] = rdum
rdum = meanse(Rdocnut$NPmass)
Rnuts[6,] = rdum
#
Tnuts = matrix(0,nr=6,nc=3) # columns are mean, se, N; rows are the 6 variates
rdum = meanse(Tdocnut$doc)
Tnuts[1,] = rdum
rdum = meanse(Tdocnut$tp_ug)
Tnuts[2,] = rdum
rdum = meanse(Tdocnut$tn_ug)
Tnuts[3,] = rdum
rdum = meanse(Tdocnut$CPmass)
Tnuts[4,] = rdum
rdum= meanse(Tdocnut$CNmass)
Tnuts[5,] = rdum
rdum = meanse(Tdocnut$NPmass)
Tnuts[6,] = rdum

# Print data summaries
print('',quote=F)
print('Peter Lake',quote=F)
print(Rnuts)
print('',quote=F)
print('Tuesday Lake',quote=F)
print(Tnuts)

# Test barplot
lid = c('Peter','Tuesday')
cvec = c('lightskyblue1','goldenrod1')
yval = c(Rnuts[1,1],Tnuts[1,1])
eval = c(Rnuts[1,2],Tnuts[1,2])
elow = yval-(2*eval)
ehigh = yval+(2*eval)

windows()
par(mfrow=c(1,1),mar=c(4,4.5,2,2)+0.1,cex.axis=1.6,cex.lab=1.6)
barcenters = barplot(height = c(Rnuts[1,1],Tnuts[1,1]),width=0.5,beside=T,
                     ylim=c(0,11),col=cvec,ylab='DOC')
segments(barcenters,elow,barcenters,ehigh,lwd=2)
arrows(barcenters,elow,barcenters,ehigh,lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# Try layout with a row of plots for DOC, TP, TN and a row for ratios
m.plot = rbind(c(1,2,3),c(4,5,6))
print(m.plot)

windows(height=8,width=12)
layout(m.plot)
par(mar=c(4,4.5,2,2)+0.1,cex.axis=2,cex.lab=2)
# plot 1
yval = c(Rnuts[1,1],Tnuts[1,1])
eval = c(Rnuts[1,2],Tnuts[1,2])
elow = yval-(2*eval)
ehigh = yval+(2*eval)
barcent1 = barplot(height = yval,width=0.5,beside=T,
                     ylim=c(0,11),col=cvec,ylab='DOC, mg/L',
                   names.arg=lid)
segments(barcent1,elow,barcent1,ehigh,lwd=2)
arrows(barcent1,elow,barcent1,ehigh,lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
# plot 2
yval = c(Rnuts[2,1],Tnuts[2,1])
eval = c(Rnuts[2,2],Tnuts[2,2])
elow = yval-(2*eval)
ehigh = yval+(2*eval)
barcent2 = barplot(height = yval,width=0.5,beside=T,
                     ylim=c(0,25),col=cvec,ylab='TP, ug/L',
                   names.arg=lid)
segments(barcent2,elow,barcent2,ehigh,lwd=2)
arrows(barcent2,elow,barcent2,ehigh,lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
# plot 3
yval = c(Rnuts[3,1],Tnuts[3,1])
eval = c(Rnuts[3,2],Tnuts[3,2])
elow = yval-(2*eval)
ehigh = yval+(2*eval)
barcent3 = barplot(height = yval,width=0.5,beside=T,
                   ylim=c(0,555),col=cvec,ylab='TN, ug/L',
                   names.arg=lid)
segments(barcent3,elow,barcent3,ehigh,lwd=2)
arrows(barcent3,elow,barcent3,ehigh,lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
# plot 4
yval = c(Rnuts[4,1],Tnuts[4,1])
eval = c(Rnuts[4,2],Tnuts[4,2])
elow = yval-(2*eval)
ehigh = yval+(2*eval)
barcent4 = barplot(height = yval,width=0.5,beside=T,
                   ylim=c(0,4000),col=cvec,ylab='DOC:TP',
                   names.arg=lid)
segments(barcent4,elow,barcent4,ehigh,lwd=2)
arrows(barcent4,elow,barcent4,ehigh,lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
# plot 5
yval = c(Rnuts[5,1],Tnuts[5,1])
eval = c(Rnuts[5,2],Tnuts[5,2])
elow = yval-(2*eval)
ehigh = yval+(2*eval)
barcent5 = barplot(height = yval,width=0.5,beside=T,
                   ylim=c(0,25),col=cvec,ylab='DOC:TN',
                   names.arg=lid)
segments(barcent5,elow,barcent5,ehigh,lwd=2)
arrows(barcent5,elow,barcent5,ehigh,lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
# plot 6
yval = c(Rnuts[6,1],Tnuts[6,1])
eval = c(Rnuts[6,2],Tnuts[6,2])
elow = yval-(2*eval)
ehigh = yval+(2*eval)
barcent6 = barplot(height = yval,width=0.5,beside=T,
                   ylim=c(0,220),col=cvec,ylab='TN:TP',
                   names.arg=lid)
segments(barcent6,elow,barcent6,ehigh,lwd=2)
arrows(barcent6,elow,barcent6,ehigh,lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
