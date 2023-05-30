#########################################################
##                 Andrew B. Hall                      ##
##       Ph.D. Candidate, Harvard University           ##
##             andrewbenjaminhall.com                  ##
##              hall@fas.harvard.edu                   ##
##    Replication code for graphics included in:       ##
##   "What Happens When Extremists Win Primaries?"     ##
#########################################################

### Note: run Stata code first!

setwd("~/Dropbox/PrimaryPower/Replication")
library(foreign)

###################
#### Figure 1 #####
###################

	data <- read.dta("compare_for_r.dta")
	
	reg <- lm(data$dwnom1~data$winner_hall_snyder_score)
	
	pdf(file="compare.pdf")
	plot(x=data$winner_hall_snyder_score, y=data$dwnom1, xlab="Contribution-based Estimated Ideology", ylab="Observed DW-NOMINATE Score", yaxt="n", cex.lab=1.4, col=ifelse(data$dem==1, "dodgerblue", "red"), bty="l")
	abline(reg, col="black")
	axis(side=2, las=1)
	dev.off()
	
	# overall correlation
	cor(data$winner_hall_snyder_score, data$dwnom1)
	
	# Also report party-specific correlations for footnote 11
	cor(data$winner_hall_snyder_score[data$dem==1], data$dwnom1[data$dem==1])
	cor(data$winner_hall_snyder_score[data$dem==0], data$dwnom1[data$dem==0])
	

###################
#### Figure 2 #####
###################
	
	data <- read.dta("primary_analysis.dta")
	
	# subset graph to only elections within .2 margin and above median distance
	rd.data <- subset(data, data$margin <= .2 & data$absdist > .1095)
	
	# make the binned averages, first the bins to the left
	count <- 1
	bin.size <- .02
	binx.left <- vector(length=length(seq(-.2, 0, bin.size)))
	biny.left <- vector(length=length(binx.left))
	last <- -.2
	for(j in seq(-.2, 0, bin.size)) {
		biny.left[count] <- mean(rd.data$dv[rd.data$rv >= j-bin.size & rd.data$rv < j], na.rm=T)
		binx.left[count] <- (j+last)/2
		last <- j
		count <- count + 1
	}
	
	# now the bins on the right
	count <- 1
	bin.size <- .02
	binx.right <- vector(length=length(seq(0.02,.2, bin.size)))
	biny.right <- vector(length=length(binx.right))
	last <- 0
	for(j in seq(0.02,.2, bin.size)) {
		biny.right[count] <- mean(rd.data$dv[rd.data$rv >= j-bin.size & rd.data$rv < j], na.rm=T)
		binx.right[count] <- (j+last)/2
		last <- j
		count <- count + 1
	}
	
	###
	reg1 <- lm(rd.data$dv[rd.data$rv < 0] ~ rd.data$rv[rd.data$rv < 0])
	reg2 <- lm(rd.data$dv[rd.data$rv > 0] ~ rd.data$rv[rd.data$rv > 0])
	
	fits1 <- reg1$coefficients[1] + reg1$coefficients[2] * rd.data$rv[rd.data$rv<0]
	fits2 <- reg2$coefficients[1] + reg2$coefficients[2] * rd.data$rv[rd.data$rv>0]
	
	sizes <- 1
	
	pdf(file="rd_plot.pdf")
	#par(mfrow=c(1,3), oma=c(3,3,3,3), mar=c(5,.5,5,.5))
	plot(x=rd.data$rv, y=rd.data$dv, col="white", xlab="Extreme Candidate Primary Election Winning Margin", yaxt="n", ylab="General Election Vote Share", cex.lab=1.5, cex.axis=.9, ylim=c(.35,.8), main="", cex.main=1.8, cex.axis=1.4, xaxt="n", xlim=c(-0.2, 0.2))
	abline(v=0, col="gray10", lty=2)
	points(x=rd.data$rv, y=rd.data$dv, pch=16, col="gray50", cex=.7)
	points(x=binx.left, y=biny.left, cex=1.7, col="black", pch=16)
	points(x=binx.right, y=biny.right, cex=1.7, pch=16)
	lines(x=rd.data$rv[rd.data$rv<0], y=fits1, lwd=4, col="black")
	lines(x=rd.data$rv[rd.data$rv>0], y=fits2, lwd=4, col="black")
	axis(side=2, las=1, cex.axis=.9, at=seq(0, 1, .2), labels=seq(0, 1, 0.2), cex.axis=1.4)
	axis(side=1, at=seq(-0.2, 0.2, 0.1), labels=seq(-0.2, 0.2, 0.1), cex.axis=1.4)
	text(x=0.18,y=0.37, paste("N=", nrow(rd.data), sep=""), cex=1.4)
	
	#mtext(side=2, line=2.4, "General Election Vote Share", cex=.9)
	
	dev.off()

###################
#### Figure 3 #####
###################

### See Stata Code

###################
#### Figure 4 #####
###################

	rm(list=ls())
	library(foreign)
	data <- read.dta("cutoff_vote_for_r.dta")
	data <- subset(data, data$cutoff < .39)
	
	cols <- rep("black", length(data$cutoff))
	cols[12] <- "red"
	
	pdf(file="cutoff_vote.pdf", height=6, width=10)
	mat <- matrix(c(1,2),2)
	layout(mat, heights=c(1.15,.45))
	par(oma=c(3,3,3,3), mar=c(1,5, .2, 5))
	plot(x=data$cutoff, y=data$est, pch=16, xlim=c(0.009, .39), ylim=c(-.6, .025), col="white", yaxt="n", ylab="Estimated Effect on Vote Share", xaxt="n", xlab="")
	axis(side=2, las=1)
	
	text(x=c(.046, .11, .21, .38), y=rep(-.55, 4), c("25th", "50th", "75th", "95th"))
	text(x=c(.046, .11, .21, .38), y=rep(-.585, 4), rep("Percentile", 4))
	abline(h=0, col="darkred", lty=1, lwd=.5)
	#segments(x0=c(.046, .11, .21, .38), x1=c(.046, .11, .21, .38), y0=.1, y1=-.62, lty=2)
	segments(x0=data$cutoff, x1=data$cutoff, y0=data$lower, y1=data$upper, col="gray60")
	points(x=data$cutoff, y=data$est, pch=16, col=cols, cex=1.2)
	arrows(x0=.102, x1=.107, y0=-.25, y1=-.16, angle=20, length=.1)
	text("Reported in", x=.1, y=-.27, cex=.8)
	text("Table 2", x=.1, y=-.3, cex=.8)
	
	plot(x=data$cutoff, y=data$n, col="white", ylim=c(0, max(data$n)), xlab="", ylab="Sample Size", yaxt="n")
	segments(x0=data$cutoff, x1=data$cutoff, y0=0, y1=data$n, lwd=5, col="gray40")
	axis(side=2, las=1)
	mtext(side=1, line=2.5, "Value of Ideological Distance Used as Cutoff")
	
	dev.off()

###################
#### Figure 5 #####
###################

	rm(list=ls())
	library(foreign)
	data <- read.dta("cutoff_win_for_r.dta")
	data <- subset(data, data$cutoff < .39)
	
	
	cols <- rep("black", length(data$cutoff))
	cols[12] <- "red"
	
	pdf(file="cutoff_win.pdf", width=10, height=6)
	mat <- matrix(c(1,2),2)
	layout(mat, heights=c(1.15,.45))
	par(oma=c(3,3,3,3), mar=c(1,5, .2, 5))
	plot(x=data$cutoff, y=data$est, pch=16, xlim=c(0.009, .39), ylim=c(-1, 0.025), col="white", yaxt="n", ylab="Estimated Effect on Victory", xaxt="n", xlab="")
	axis(side=2, las=1)
	#axis(side=1)
	
	text(x=c(.046, .11, .21, .38), y=rep(-.94, 4), c("25th", "50th", "75th", "95th"))
	text(x=c(.046, .11, .21, .38), y=rep(-.985, 4), rep("Percentile", 4))
	abline(h=0, col="darkred", lty=1, lwd=.5)
	segments(x0=data$cutoff, x1=data$cutoff, y0=data$lower, y1=data$upper, col="gray60")
	points(x=data$cutoff, y=data$est, pch=16, col=cols, cex=1.2)
	arrows(x0=.102, x1=.107, y0=-.62, y1=-.48, angle=20, length=.1)
	text("Reported in", x=.102, y=-.65, cex=.8)
	text("Table 2", x=.102, y=-.71, cex=.8)
	
	plot(x=data$cutoff, y=data$n, col="white", ylim=c(0, max(data$n)), xlab="", ylab="Sample Size", yaxt="n")
	segments(x0=data$cutoff, x1=data$cutoff, y0=0, y1=data$n, lwd=5, col="gray40")
	mtext(side=1, line=2.5, "Value of Ideological Distance Used as Cutoff")
	
	axis(side=2, las=1)
	dev.off()

###################
#### Figure 6 #####
###################
	
	data <- read.dta("dwnom_for_r.dta")
	
	data.dem <- data[data$dem==1,]
	data.rep <- data[data$dem==0,]
	
	fits.dem <- ksmooth(x=-data.dem$cand_abs, y=data.dem$dwnom1, bandwidth=.11)
	fits.rep <- ksmooth(x=data.rep$cand_abs, y=data.rep$dwnom1, bandwidth=.11)
	
	dem.mat <- matrix(nrow=10000, ncol=nrow(data.dem))
	rep.mat <- matrix(nrow=10000, ncol=nrow(data.rep))
	
	set.seed(02138)
	
	for (b in 1:10000) {
		rows.dem <- sample(1:nrow(data.dem), size=nrow(data.dem), replace=T)
		rows.rep <- sample(1:nrow(data.rep), size=nrow(data.rep), replace=T)
		d.dem <- data.dem[rows.dem,]
		d.rep <- data.rep[rows.rep,]
		dem.mat[b,1:nrow(data.dem)] <- ksmooth(x=-d.dem$cand_abs, y=d.dem$dwnom1, bandwidth=.11)$y
		rep.mat[b,1:nrow(data.rep)] <- ksmooth(x=d.rep$cand_abs, y=d.rep$dwnom1, bandwidth=.11)$y
	}
	
	dem.lowers <- apply(dem.mat, 2, quantile, .025)
	dem.uppers <- apply(dem.mat, 2, quantile, .975)
	rep.lowers <- apply(rep.mat, 2, quantile, .025)
	rep.uppers <- apply(rep.mat, 2, quantile, .975)
	
	
	pdf(file="dwnom_plots.pdf", height=6, width=10)
	par(mfrow=c(1,2), oma=c(3,3,3,3), mar=c(5,2,5,.2))
	
	
	plot(x=-data.dem$cand_abs, y=data.dem$dwnom1, col="white", xlab="Donor Score of Democratic Primary Winner", ylab="District DW-NOM Score", main="Democratic Primaries", yaxt="n")
	text(x=-data.dem$cand_abs, y=data.dem$dwnom1, ifelse(data.dem$dv_win==1, "D", "R"), col=ifelse(data.dem$dv_win==1, "blue", "red"), cex=.8)
	lines(x=fits.dem$x, y=fits.dem$y, col="black", lwd=3)
	lines(x=fits.dem$x, y=dem.lowers, lty=2, lwd=2, col="gray20")
	lines(x=fits.dem$x, y=dem.uppers, lty=2, lwd=2, col="gray20")
	axis(side=2, las=1)
	mtext(side=2, "District DW-NOM Score", line=3)
	
	
	plot(x=data.rep$cand_abs, y=data.rep$dwnom1, col="white", xlab="Donor Score of Republican Primary Winner", ylab="", main="Republican Primaries", yaxt="n")
	text(x=data.rep$cand_abs, y=data.rep$dwnom1, ifelse(data.rep$dv_win==1, "R", "D"), col=ifelse(data.rep$dv_win==1, "red", "blue"), cex=.8)
	lines(x=fits.rep$x, y=fits.rep$y, col="black", lwd=3)
	lines(x=fits.rep$x, y=rep.lowers, lty=2, lwd=2, col="gray20")
	lines(x=fits.rep$x, y=rep.uppers, lty=2, lwd=2, col="gray20")
	dev.off()

###################
#### Figure 7 #####
###################


	data <- read.dta("downstream_for_r.dta")
	
	pdf(file="downstream.pdf")
	par(mfrow=c(2,1), oma=c(3,3,3,3), mar=c(1,5,1,5))
	
	plot(x=data$B7, y=data$est_win, ylim=c(-.7, .1), col="white", xaxt="n", yaxt="n", xlab="", ylab="Effect on Victory")
	segments(x0=data$B7, x1=data$B7, y0=data$lower_win, y1=data$upper_win)
	points(x=data$B7, y=data$est_win, pch=16)
	axis(side=2, las=1, at=seq(-.6, 0, .2), labels=seq(-.6, 0, .2))
	abline(h=0, col="darkred")
	
	
	plot(x=data$B7, y=data$est_vote, ylim=c(-.6, .1), col="white",yaxt="n", xlab="", ylab="Effect on Vote Share", xaxt="n")
	segments(x0=data$B7, x1=data$B7, y0=data$lower_vote, y1=data$upper_vote)
	points(x=data$B7, y=data$est_vote, pch=16)
	axis(side=2, las=1, at=seq(-.6, 0, .2), labels=seq(-.6, 0, .2))
	axis(side=1, at=1:5, labels=0:4)
	mtext(side=1, "Terms Downstream", line=2)
	abline(h=0, col="darkred")
	dev.off()


###################
#### Figure 8 #####
###################
	
	data <- read.dta("downdwnom_for_r.dta")
	
	pdf("dwnom_downstream.pdf", height=6, width=10)
	plot(x=data$B7, y=data$est_dem,  col="white",yaxt="n", xlab="", ylab="Effect on Roll-Call Voting", ylim=c(-1.4, 1), xlim=c(-1.25,4.25), bty="l", xaxt="n")
	segments(x0=data$B7-.15, x1=data$B7-.15, y0=data$lower_vote, y1=data$upper_vote, col="blue", lwd=3)
	points(x=data$B7-.15, y=data$est_dem, pch=16, col="blue", cex=1.5)
	axis(side=2, las=1)
	segments(x0=data$B7+.15, x1=data$B7+.15, y0=data$lower_win, y1=data$upper_win, col="red", lwd=3)
	points(x=data$B7+.15, y=data$est_rep, col="red", pch=17, cex=1.5)
	mtext(side=1, "Terms Downstream", line=2)
	legend("bottomleft", pch=c(16, 17), c("Democratic Primaries", "Republican Primaries"), col=c("blue", "red"))
	abline(h=0, lty=2, col="black")
	arrows(x0=-.4, x1=-.4, y=0, y1=1, angle=40, lwd=3, col="darkgray")
	arrows(x0=-.4, x1=-.4, y=0, y1=-1, angle=40, col="darkgray", lwd=3)
	axis(side=1, at=0:4, labels=0:4)
	text(x=-1, y=.5, "More", cex=1.3, col="red")
	text(x=-1, y=.4, "Conservative", cex=1.3, col="red")
	text(x=-1, y=-.5, "More", cex=1.3, col="blue")
	text(x=-1, y=-.6, "Liberal", cex=1.3, col="blue")
	dev.off()
	
###################
#### Figure 9 #####
###################

	
	rm(list=ls())
	data <- read.dta("cand_characteristics.dta")
	pdf(file="cand_characteristics.pdf")
	plot(x=data$est, y=1:4, col="white", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-.5, .5), ylim=c(0.3,4.7), cex.lab=1.4)
	abline(v=0, lty=2, col="red")
	text("Probability Female", x=-0.31, y=1.3, cex=1.5)
	text("Probability Experienced", x=-0.27, y=4.3, cex=1.5)
	
	text("Probability Incumbent", x=-0.28, y=2.3, cex=1.5)
	text("Share of Donations", x=-0.28, y=3.3, cex=1.5)
	
	segments(x0=data$lower, x1=data$upper, y0=1:4, y1=1:4, lwd=3, col="darkgray")
	points(x=data$est, y=1:4, pch=16, cex=1.8)
	axis(side=1, cex.axis=1.4)
	mtext(side=1, cex=1.5, line=3.5, "RDD Diff Between Extremist and Moderate Bare-Winners")
	dev.off()
	

##########################################
############## APPENDIX ##################
##########################################
	
	
###################
#### Figure A1 ####
###################

	# Note: ignore warning messages that R outputs

	data <- read.dta("comparison_for_r.dta")
	
	pdf(file="density_comparison.pdf", height=8, width=23)
	c <- 2.7
	par(mfrow=c(1,3), oma=c(3,1,3,1), mar=c(4,3.5,1,3.5))
	d1 <- density(data[data$in_analysis==1,]$pnv, na.rm=T)
	d2 <- density(data$pnv, na.rm=T)
	plot(d2, xaxt="n", yaxt="n", ylab="", xlab="", main="", cex.lab=c, bty="n", cex.axis=c, tck=0,  mgp=c(3,1,-1), lty=2)
	lines(d1, lty=1)
	axis(side=1, cex.axis=c, tck=0, mgp=c(3,1,-1))
	legend("topright", c("All Districts", "In Analysis"), lty=c(2, 1), cex=c)
	mtext(side=1, line=4.5, cex=c-.3, "District Dem Normal Vote")
	
	num.open.g <- sum(data$open_general)/nrow(data)
	num.not.open.g <- sum(nrow(data) - sum(data$open_general))/nrow(data)
	num.open.g2 <- sum(data[data$in_analysis==1,]$open_general)/nrow(data[data$in_analysis==1,])
	num.not.open.g2 <- (sum(nrow(data[data$in_analysis==1,]) - sum(data[data$in_analysis==1,]$open_general)))/nrow(data[data$in_analysis==1,])
	
	holder <- 0
	
	barplot(c(num.open.g, num.open.g2, holder, num.not.open.g, num.not.open.g2), col=c("darkgreen", "darkgreen", "white", "darkgreen", "darkgreen"), border=c("black", "black", "white", "black", "black"), density=c(100, 20, 0, 100, 20), yaxt="n", cex.axis=c, cex.lab=c)
	mtext(side=1, "Fraction In \n Open Seat", line=4.8, at = 1.3, cex=c-.5)
	mtext(side=1, "Fraction With \n Incumbent", line=4.8, at = 4.9, cex=c-.5)
	legend("topleft", fill=c("darkgreen", "darkgreen"), density=c(100, 20), c("All Elections", "In Analysis"), cex=2.7)
	axis(side=2, las=1, cex.axis=c)
	
	
	
	data <- read.dta("over_time_for_r.dta")
	data <- data[data$year>1978,]
	data.dem <- data[data$party=="D",]
	data.rep <- data[data$party=="R",]
	
	data.dem.a <- data.dem[data.dem$in_analysis==1,]
	data.rep.a <- data.rep[data.rep$in_analysis==1,]
	y.tot1 <- data.dem[data.dem$in_analysis==0,]$num + data.rep[data.rep$in_analysis==0,]$num
	y.tot2 <- data.dem[data.dem$in_analysis==1,]$num + data.rep[data.rep$in_analysis==1,]$num
	
	total <- y.tot1 + y.tot2
	
	plot(x=data.dem.a$year, y=data.dem.a$num, type="l", ylim=c(0,50), col="blue", lty=2, lwd=4, yaxt="n", ylab="", xlab="", bty="n", cex.axis=c, cex.lab=c, tck=0, mgp=c(3,1,-1))
	lines(x=data.rep.a$year, y=data.rep.a$num, type="l", col="red", lwd=4)
	axis(side=2, las=1, cex.axis=c)
	#dev.off()
	text(x=1999, y=40, col="blue", "# Dem Cases", cex=c)
	text(x=1985, y=6, col="red", "# Rep Cases", cex=c)
	
	dev.off()


###################
#### Figure A2 ####
###################
	
	rm(list=ls())
	
	data1 <- read.csv("graph1.csv")
	data2 <- read.csv("graph2.csv")
	data3 <- read.csv("graph3.csv")
	data4 <- read.csv("graph4.csv")
	data5 <- read.csv("graph5.csv")
	data6 <- read.csv("graph6.csv")
	data7 <- read.csv("graph7.csv")
	data <- read.dta("for_balance_plots.dta")
	
	c <- 1.3
	
	pdf(file="balance_tests.pdf")
	par(mfrow=c(4,2), oma=c(3,3,3,3), mar=c(1,1,2,.2))
	plot(x=data$rv, y=data$pres_normal_vote, col="gray70", cex=.5, xaxt="n", xlab="", yaxt="n", ylab="", xlim=c(-.2, .2))
	abline(v=0, col="red")
	points(x=data1$rv, y=data1$pres_normal_vote, pch=16, cex=c)
	axis(side=2, las=1, at=seq(0, 0.35, 0.1), labels=seq(0, 0.35, 0.1))
	mtext(side=3, "Presidential Vote Share", line=1, font=2)
	
	plot(x=data$rv, y=data$prim_share, col="gray70", cex=.5, xaxt="n", xlab="", yaxt="n", ylab="", xlim=c(-.2, .2))
	abline(v=0, col="red")
	points(x=data2$rv, y=data2$prim_share, pch=16, cex=c)
	axis(side=4, las=1, at=seq(0, 1, 0.5), labels=seq(0, 1, 0.5))
	mtext(side=3, "Extremist Share of Primary Money", line=1, font=2)
	
	
	plot(x=data$rv, y=data$prim_pac_share, col="gray70", cex=.5, xaxt="n", xlab="", yaxt="n", ylab="", xlim=c(-.2, .2))
	abline(v=0, col="red")
	points(x=data3$rv, y=data3$prim_pac_share, pch=16, cex=c)
	axis(side=2, las=1, at=seq(0, 1, 0.5), labels=seq(0, 1, 0.5))
	mtext(side=3, "Extremist Share of PAC Primary Money", line=1, font=2)
	
	
	plot(x=data$rv, y=data$prim_total, col="gray70", cex=.5, xaxt="n", xlab="", yaxt="n", ylab="", ylim=c(0,700000), xlim=c(-.2, .2))
	abline(v=0, col="red")
	points(x=data4$rv, y=data4$prim_total, pch=16, cex=c)
	axis(side=4, las=1, at=seq(0,700000, 200000), labels=seq(0,7, 2))
	mtext(side=3, "Extremist Total Primary Money (100k)", line=1, font=2)
	
	
	plot(x=data$rv, y=data$abs_dw_lag, col="gray70", cex=.5, xaxt="n", xlab="", yaxt="n", ylab="", xlim=c(-.2, .2))
	abline(v=0, col="red")
	points(x=data5$rv, y=data5$abs_dw_lag, pch=16, cex=c)
	axis(side=2, las=1, at=seq(0, 1, 0.5), labels=seq(0, 1, 0.5))
	#axis(side=1)
	mtext(side=3, "Lagged DW-NOMINATE", line=1, font=2)
	
	
	plot(x=data$rv, y=data$abs_lag_wnom, col="gray70", cex=.5, xaxt="n", xlab="", yaxt="n", ylab="", xlim=c(-.2, .2))
	abline(v=0, col="red")
	points(x=data6$rv, y=data6$abs_lag_wnom, pch=16, cex=c)
	#axis(side=4, las=1, at=seq(0, 1, 0.5), labels=seq(0, 1, 0.5))
	#axis(side=1)
	mtext(side=3, "Lagged W-NOMINATE", line=1, font=2)
	axis(side=1)
	
	
	plot(x=data$rv, y=data$dv_lag, col="gray70", cex=.5, xaxt="n", xlab="", yaxt="n", ylab="", xlim=c(-.2, .2))
	abline(v=0, col="red")
	points(x=data7$rv, y=data7$dv_lag, pch=16, cex=c)
	axis(side=2, las=1, at=seq(0, 1, 0.5), labels=seq(0, 1, 0.5))
	axis(side=1)
	mtext(side=3, "Lagged Vote Share", line=1, font=2)
	mtext(side=1, line=2.5, "Extremist Win Margin")
	
	
	dev.off()
	
###################
#### Figure A3 ####
###################
	
	rm(list=ls())
	data <- read.dta("for_robust_dv.dta")
	attach(data)
	lw<-4
	
	pdf("rdd_robust_dv.pdf", height=6, width=10)
	plot(x=B5, y=B1, type="l", lwd=lw, col="pink", xlab="RDD Margin", ylab="Estimated Effect of Extremist Win", lty=1, cex.lab=1.3, cex.main=1.4, xlim=c(3,50), xaxt="n", ylim=c(-.15,0.05), main="On General-Election Vote Share")
	abline(h=0, lty=2)
	axis(side=1, at=seq(5,50,5), labels=seq(5,50,5))
	lines(x=B5, y=B2, type="l", lwd=lw, col="black", lty=2)
	lines(x=B5, y=B3, type="l", lwd=lw, col="darkgreen", lty=3)
	lines(x=B5, y=B4, type="l", lwd=lw, col="purple", lty=4)
	legend("bottomright", c("Linear", "Quadratic", "Cubic", "Quartic"), lty=c(1,2,3,4), col=c("pink", "black", "darkgreen", "purple"))
	dev.off()
	
###################
#### Figure A4 ####
###################


	rm(list=ls())
	data <- read.dta("for_robust_dv_win.dta")
	attach(data)
	lw<-4
	
	pdf("rdd_robust_dv_win.pdf", height=6, width=10)
	plot(x=B5, y=B1, type="l", lwd=lw, col="pink", xlab="RDD Margin", ylab="Estimated Effect of Extremist Win", lty=1, cex.lab=1.3, cex.main=1.4, xlim=c(3,50), xaxt="n", ylim=c(-.6,0.1), main="On General-Election Victory")
	abline(h=0, lty=2)
	axis(side=1, at=seq(5,50,5), labels=seq(5,50,5))
	lines(x=B5, y=B2, type="l", lwd=lw, col="black", lty=2)
	lines(x=B5, y=B3, type="l", lwd=lw, col="darkgreen", lty=3)
	lines(x=B5, y=B4, type="l", lwd=lw, col="purple", lty=4)
	legend("bottomright", c("Linear", "Quadratic", "Cubic", "Quartic"), lty=c(1,2,3,4), col=c("pink", "black", "darkgreen", "purple"))
	dev.off()


###################
#### Figure A5 ####
###################

	data <- read.dta("for_robust_dv_locallinear.dta")
	
	pdf(file="dv_local_linear.pdf", height=5, width=8)
	plot(x=data$B4, y=data$B1, col="white", xlab="RDD Bandwidth", ylab="Estimate", yaxt="n", xaxt="n", ylim=c(-.25,.05))
	abline(h=0, col="red", lty=2)
	segments(x0=data$B4, x1=data$B4, y0=data$B2, y1=data$B3, col="gray80")
	points(x=data$B4, y=data$B1, pch=16, cex=.7)
	axis(side=2, las=1)
	axis(side=1)
	dev.off()


###################
#### Figure A6 ####
###################

	data <- read.dta("for_robust_dv_win_locallinear.dta")
	
	pdf(file="dv_win_local_linear.pdf", height=5, width=8)
	plot(x=data$B4, y=data$B1, col="white", xlab="RDD Bandwidth", ylab="Estimate", yaxt="n", xaxt="n", ylim=c(-1,.2))
	abline(h=0, col="red", lty=2)
	segments(x0=data$B4, x1=data$B4, y0=data$B2, y1=data$B3, col="gray80")
	points(x=data$B4, y=data$B1, pch=16, cex=.7)
	axis(side=2, las=1)
	axis(side=1)
	dev.off()

###################
#### Figure A7 ####
###################
	
	data <- read.dta("for_robust_qual.dta")
	
	pdf(file="qual_local_linear.pdf", height=5, width=8)
	plot(x=data$B4, y=data$B1, col="white", xlab="RDD Bandwidth", ylab="Estimate", yaxt="n", xaxt="n", ylim=c(-.8,.6))
	abline(h=0, col="red", lty=2)
	segments(x0=data$B4, x1=data$B4, y0=data$B2, y1=data$B3, col="gray80")
	points(x=data$B4, y=data$B1, pch=16, cex=.7)
	axis(side=2, las=1)
	axis(side=1)
	dev.off()
	



