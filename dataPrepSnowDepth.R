                      ### ### ### ### ### ###  ### ### ### ### ### ### 
                      #  FORMATTING AND PREPARING SNOW DEPTH DATA    #
                      #   FOR ANALYSIS OF HUMAN INFLUENCE ON WOLF    #
                      #        DISTRIBUTIONS AND BEHAVIORS           #
                      #                                              #
                      #         Kristin Barker | Summer 2019         #
                      #           kristinjbarker@gmail.com           #
                      ### ### ### ### ### ###  ### ### ### ### ### ### 


################################################################################################## #

### ### ### ### ### #
####  | SETUP |  ####
### ### ### ### ### #


  
  #### Set working directory and filepath to spatial data ####

      setwd("C:\\Users\\Kristin\\Box Sync\\Documents\\HumanInfluenceOnWolves")
      datDir <- "C:\\Users\\Kristin\\Box Sync\\Documents\\Data"

  

  #### Install and load any necessary packages you don't already have ####
  
    
    # list of packages needed
    packages <- c(
      "raster",        ## for parts of justin clapp's code
      "maps",          ## for parts of justin clapp's code
      "maptools",      ## for cluster algorithm & kmlPoints
      "rgdal",         ## for cluster algorithm & spatial/shapefile work
      "rgeos",         ## gDifference (clip) - nevermind, use erase to retain sf
      "sp",            ## spatial work
      "sf",            ## for spatial work like the kids are doing it these days
      "lubridate",     ## manipulate datetime data inside dplyr pipes
      "dplyr")         ## data manipulation and general awesomeness
    
    
    # Check whether the packages listed above are installed, 
    # install any you don't already have, then load them all 
    # (code by Steven Worthington: https://gist.github.com/stevenworthington/3178163)   
    ipak <- function(pkg){
      new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
      if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
      sapply(pkg, require, character.only = TRUE)
    }    
    ipak(packages) 
    rm(ipak, packages)

    
  
  #### Define spatial projections ####
    
    ll <- CRS("+init=epsg:4326") # WGS 84
    utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N 
    aea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
    
    
  #### Increase memory limit ####
    
    memory.limit(size = 7500000)
 

################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### #
####   | WORK IN PROGRESS |  ####
### ### ### ### ### ### ### ### #
 

    # 1. download 1 file
    # 2. unzip and store 1 file
    # 3. convert 1 file to geotif
    # 4. download, unzip, and store >1 file
    # 5. identify all files of interest
    # 6. download, unzip, store, and convert all files of interest
    # 7. profit

    
    # 1. download a .tar file from noaa's snodas dataset of snow depth
        
        temp <- tempfile()
        download.file("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/2005/01_Jan/SNODAS_20050101.tar", temp)
        data <- read.table(unz(temp, "us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")) # newp
        data <- read.table(unz(temp, "SNODAS_20050101.dat")) # newp, obvs
        data <- read.delim(unz(temp, "us_ssmv11036tS__T0001TTNATS2005010105HP001.dat"))
        unz(temp, "us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
          # description "C:\\Users\\Kristin\\AppData\\Local\\Temp\\Rtmps7ngmp\\file674052ee7324:us_ssmv11036tS__T0001TTNATS2005010105HP001.dat"
        data <- read.table(unz("C:\\Users\\Kristin\\AppData\\Local\\Temp\\Rtmps7ngmp\\file674052ee7324:us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")) # newp argument "filename" is missing, with no default
        data <- read.table(unz("C:\\Users\\Kristin\\AppData\\Local\\Temp\\Rtmps7ngmp", "file674052ee7324:us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")) # newp can't open connection
        data <- read.table(unz("C:/Users/Kristin/AppData/Local/Temp/Rtmps7ngmp", "file674052ee7324:us_ssmv11036tS__T0001TTNATS2005010105HP001.dat"))
        data <- read.table(unz("C:\\Users\\Kristin\\AppData\\Local\\Temp\\Rtmps7ngmp\\file674052ee7324", "file674052ee7324:us_ssmv11036tS__T0001TTNATS2005010105HP001.dat"))
        unz(temp, "us_ssmv11036tS__T0001TTNATS2005010105HP001.dat") # this is a description of a connection. it works i think.
        read.table(unz(temp, "us_ssmv11036tS__T0001TTNATS2005010105HP001.dat"), skip = 3)
        unlink(temp)
        
        
        
        # can you do it from the files you manually unzipped?
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
        # oh. false. eff. 
        # us_ssmv11036tS__T0001TTNATS2005010105HP001.dat pasted filename
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
        # wtfffff
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\swe2019.csv")
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv01025SlL00T0024TTNATS2005010105DP001.txt")
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\swe2019.csv")
        
        # can you read it if it's elsewhere
        unz(getwd(), "us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
        read.table(unz(getwd(), "us_ssmv11036tS__T0001TTNATS2005010105HP001.dat"))
        # no
        
        # quick try again after extracting a different file with the '' option unchecked in winzip as per snodas instructions
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv01025SlL00T0024TTNATS2005010205DP001.txt")
        # ok nvm seriously stop working on this
    
    
    
    
    #### KRISTIN YOU LEFT OFF HERE ####
    
    # for some reason R can't recognize .dat files or .txt files
    # including both the ones you use r to download into a temp file and the ones you manually download and extract to your machine
    # at a total loss as to why not. 
    # unz(temp, "us_ssmv11036tS__T0001TTNATS2005010105HP001.dat") # prints a descrip like it's real and r found it
    # but file.exists prints FALSE even when you paste the filename in
    # so unz finds it but file.exists doesn't and you need to figure that out i guess
    # file.exists can find other file types in the same folder (but not the .dat or .txt files that were extracted there by winzip)
    # gotta have something to do with your not understanding how compression/decompression works
        
        # here's a questionable researchgate QnA https://www.researchgate.net/post/How_do_I_read_dat_files_in_R
        # this is where you pull files from ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/2005/01_Jan/
        # data descrips etc are in the User Guide tab here https://nsidc.org/data/g02158
        # i guess worst case you can do all this manually by changing the file extensions to .bil and reading into arcmap
            # as per instructions here https://www.nohrsc.noaa.gov/archived_data/instructions.html
        
        
        
    
    
    
    
    
    
    # 6. a) identify the files you want
      
      # rep years/months and search for that pattern maybe? eg 2005/01 2005/02 2005/03
      # you want just the snow depth files in each of the /01 /02 /03 subfolders
      
      # all snow depth files will start with "us_ssmv11036tS__T0001TTNATS"
      # followed immediately (sep = "") by YYYYMMDD
      # followed immediately by "05HP001.dat" or "05HP001.txt" (one of each)
          
      baseurl <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/"
      yrs <- c("2005", "2007", "2008", "2009", "2010", "2011", "2012", 
                 "2013", "2014", "2015", "2016", "2017", "2018", "2019")
      mnths <- c("01", "02", "03")
      
    # 6. f) convert .dat (and .txt??) files to geotiff
    
      # conversion notes: you can use gdal but it would be nicer to use it thru r...
      # https://nsidc.org/support/how/how-do-i-convert-snodas-binary-files-geotiff-or-netcdf
      
    
################################################################################################## #
    
    
    #### stolen code to patch together ####
    
# https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data    
temp <- tempfile()
download.file("http://www.newcl.org/data/zipfiles/a1.zip",temp)
data <- read.table(unz(temp, "a1.dat"))
unlink(temp)

# http://r.789695.n4.nabble.com/download-file-td852019.html
# you want to download the file in a binary fashion, i.e. use
# argument mode="wb", otherwise your binary tar file will be corrupt. 

# may require RCurl?
url = "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE1nnn/GSE1297/suppl/"
filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filenames <- strsplit(filenames, "\r\n")
filenames = unlist(filenames)
filenames

# [1] "filelist.txt"    "GSE1297_RAW.tar"

for (filename in filenames) {
download.file(paste(url, filename, sep = ""), paste(getwd(), "/", filename,
sep = ""))
}



################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | RAW DATA |  ####
### ### ### ### ### ### #
 
    
 