percentage = function(x){
    output =  paste(as.character(comma(round(x*100,2))), "%", sep=""  )
    
    return(output) 
}
