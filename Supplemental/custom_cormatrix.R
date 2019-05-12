custom_cormatrix = function(data, cor.method = "spearman",size=1,log.transform=FALSE,group=NULL){
	
	check_packages = function(names){
    for(name in names){
        if (!(name %in% installed.packages()))
            install.packages(name, repos="http://cran.us.r-project.org") #if package not installed, install the package
        library(name, character.only=TRUE,warn.conflicts=FALSE,quietly=TRUE)
    }
	}
	check_packages("RColorBrewer")
  
# turns character columns into factor
character_columns = names(data)[sapply(data, is.character)]
if (length(character_columns) > 0){
  data[character_columns] <- lapply(data[character_columns] , factor)
}

# turns all factor columns into numeric
factor_columns = names(data)[ sapply(data, is.factor) ]  
data[,factor_columns] = as.data.frame(sapply(data[,factor_columns], as.numeric))

if (log.transform==TRUE){
  if (sum(data[,-which(names(data) %in% factor_columns)]<0, na.rm=TRUE) > 0) {
    print("Data contains values less than 0 and cannot be log transformed. Use data[data <0] to check values.")
  }else {
    if(length(factor_columns) >0){
    data[,-which(names(data) %in% factor_columns)] = as.data.frame(sapply(data[,-which(names(data) %in% factor_columns)], function(x) log(x+1)))
    }else{ 
      data = as.data.frame(sapply(data, function(x) log(x+1)))
  } 
  
  }
}
data2 = data
    
text.size= (22.5/ncol(data))*size
  
# makes color scheme depending on crrelation coefficient
cols = brewer.pal(11, "RdBu")   # goes from red to white to blue
pal = colorRampPalette(cols)
cor_colors = data.frame(correlation = seq(-1,1,0.01), correlation_color = pal(201)[1:201])  # assigns a color for each r correlation value
cor_colors$correlation_color = as.character(cor_colors$correlation_color)

# prints correlation coefficent and p value
panel.cor <- function(x, y, digits=2, cex.cor){
  par(usr = c(0, 1, 0, 1))
  u <- par('usr') 
  names(u) <- c("xleft", "xright", "ybottom", "ytop")
  r <- cor(x, y,method=cor.method,use="complete.obs")   # can change correlation coefficient method by replacing "spearman" with "pearson" or "kendall" 
  test <- cor.test(x,y)
  bgcolor = cor_colors[2+(-r+1)*100,2]    # converts correlation coefficient into a specific color
  do.call(rect, c(col = bgcolor, as.list(u))) # colors the correlation box
  
  # prints "Insignificant" if p < 0.05
  if (test$p.value> 0.05){
    text(0.5,0.5,"Insignificant",cex=text.size)  # change cex value to change size of "Insignificant"
  } else{
    text(0.5, 0.75, paste("r=",round(r,2)),cex=text.size*1.25) # prints correlatoin coefficient. change cex value to change size of correlation coefficient r (ex: cex=1)
    text(.5, .25, paste("p=",formatC(test$p.value, format = "e", digits = 1)), cex=text.size*1.25)  # prints p value in scientific notation format.
    abline(h = 0.5, lty = 2) # draws a horizontal line between correlation coefficient and p value
  }
}

# makes scatterplot
if (is.null(group)){
panel.smooth<-function (x, y, col = "black", bg = NA, pch = 19, cex = 1.2, col.smooth = "blue", span = 2/3, iter = 3, ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), lwd=2.5, 
          col = col.smooth, ...)}
}else{
	panel.smooth<-function (x, y, col = data[,group], bg = NA, pch = 19, cex = 1.2, col.smooth = "blue", span = 2/3, iter = 3, ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), lwd=2.5, 
          col = col.smooth, ...)}
}
	

# makes histogram
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)}

# combines correlation coefficient scatterplot, and histogram into a matrix
  pairs(data2,lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist,cex.labels=text.size*1.25)
}