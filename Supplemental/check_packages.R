check_packages = function(names){
    for(name in names){
        if (!(name %in% installed.packages()))
            install.packages(name, repos="http://cran.us.r-project.org") #if package not installed, install the package
        
        library(name, character.only=TRUE)
    }
}

