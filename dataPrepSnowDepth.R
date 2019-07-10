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
        
        
        
        
        ## take 2, next day
        
        ## first opened manually-downloaded files and realized the .dat and .txt files are actually still compressed
        ## unzipped this one us_ssmv11036tS__T0001TTNATS2005010105HP001.dat it's the exact same file name as before
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
        # ha. now it's true. ok so issue #1 is that you have to double-unzip the damn files
        
        # let's play with the data to see how to make raster/spatial etc
        
        z <- read.table("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
    
        # oh right i extracted those before i read the instructions on unchecking that box, delete and redownload
        
        
        ## alright manual steps are:
        
        # 1. 
        
        # unzip .tar for one day (data arranged in folders per month then per year)
          # this extracts a bunch of snow data, each one includes a .dat and a .txt file
        # unzip snow depth files, which start with us_ssmv11036tS...
        # OH these have a secret .gz extension; that's gotta be part of the issue see if r can see those
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat") #F
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat.gz") #T
        # A HA tricky mothafuckas. 
        # ok that solves one problem but keep going manually to make sure you understand how to manipulate files
        
        # 2. 
        
        # unzip .dat (and .txt?) files for snow depth that were in that .tar along with a bunch of other files
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
        z <- read.table("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
        # eh fuck it's still all fucked up, one obs of some fucked up character-looking mumbojumbo
        z <- read.delim("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat")
        # and this gives 0 obs 
        z <- read.table("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat", skip = 3)
        # 0 obs
        
        
        # let's make this a little less unweildy... er, more weildy...
        f <- "C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001.dat"
        z <- read.delim(f)
        z <- read.delim(f, header = TRUE) # 0 obs of 1 vrbl

        
        # error is "incomplete final line". 
        # this means 'last line of the file doesn't end with an End Of Line (EOL) character (linefeed (\n) or carriage return+linefeed (\r\n))'
        # the way you're supposed to fix it is by opening the file andhitting enter at the end of the last row
        # which is fucking stupid and i'm not going to do it
        # so i need to use r to append an end of line character
        
        test <- cat("\n", file = find.file(f), append = TRUE) # NULL value .
        z <- read.delim(f, header = TRUE, fileEncoding = "UTF-8")
        z <- read.delim(f, fileEncoding = "UTF-8")
        
        # i opened the file in notpad and just hit save as, appended "_kjb" to end of file name. i think it made it a txt
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001_kjb.dat") #F
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001_kjb.txt") # yep
        g <- "C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001_kjb.txt"
        zz <- read.delim(g) # same incomplete final line warning
        zz <- read.delim(g, header = TRUE, fileEncoding = "UTF-16") # haha so many problems
        
        # opened _kjb.txt in notepad++ again, went to last line (l 56810), hit enter, saved and closed
        file.exists("C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Environment\\snowDepth\\testing123\\us_ssmv11036tS__T0001TTNATS2005010105HP001_kjb.txt")
        zz <- read.delim(g) # same error wtf
        
        # mk read through the info about how to open/convert files using gdal and/or arcmap
        # i think i'm hosed for using r for this
        # SO
        # i can either say fuck it and stick with swe for now (which i don't love)
        # or figure out the python/gdal thing which will take some time but appears doable
        # or i can use the arcmap instructions which will also take time but doesn't require learning a new thing
        # let's spend a bit of time investigating the arc option
        
        # yeah looks like arcmap will be easiest but ihave to do it manually for 30files*3months*14years, dear god 1260
        
        # HERE is a really cool python script to handle this (i think) but you'd need to pester owen for help understanding it
        # https://github.com/dshean/snowtools/blob/master/snowtools/get_snodas.py
        

        
    # misc previous notes and resources from take 1
        
        
        
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
 
    
 