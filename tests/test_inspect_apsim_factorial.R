## Testing the factorial feature for inspect_apsim
require(apsimx)

run.factorial.apsim <- FALSE

extd.dir <- system.file("extdata", package = "apsimx")

tmp.dir <- tempdir()

if(run.factorial.apsim){
  
  ## There are two folders in 'maize-factorial'
  inspect_apsim("maize-factorial.apsim", src.dir = extd.dir,
                node = "Clock", parm = "start_date",
                root = "IA-CC_Canisteo_No-Cover",
                print.path = TRUE)
  
  inspect_apsim("maize-factorial.apsim", src.dir = extd.dir,
                node = "Clock", parm = "start_date",
                root = "IA-CC_Canisteo_Cover",
                print.path = TRUE)
  
  ## Edit them
  pp1 <- inspect_apsim("maize-factorial.apsim", src.dir = extd.dir,
                        node = "Clock", parm = "start_date",
                        root = "IA-CC_Canisteo_No-Cover",
                        print.path = TRUE)
  
  edit_apsim("maize-factorial.apsim", 
             src.dir = extd.dir,
             wrt.dir = tmp.dir,
             node = "Other", 
             parm.path = pp1, 
             value = "01/02/1999")
  
  inspect_apsim("maize-factorial-edited.apsim", 
                src.dir = tmp.dir,
                node = "Clock", parm = "start_date",
                root = "IA-CC_Canisteo_No-Cover",
                print.path = TRUE)
  
  ## Second example
  pp2 <- inspect_apsim("maize-factorial.apsim", src.dir = extd.dir,
                       node = "Clock", parm = "start_date",
                       root = "IA-CC_Canisteo_No-Cover",
                       print.path = TRUE)
 
  edit_apsim("maize-factorial.apsim", 
             src.dir = extd.dir,
             wrt.dir = tmp.dir,
             node = "Other", 
             parm.path = pp2, 
             value = "01/02/2009")
  
  inspect_apsim("maize-factorial-edited.apsim", 
                src.dir = tmp.dir,
                node = "Clock", parm = "start_date",
                root = "IA-CC_Canisteo_No-Cover",
                print.path = TRUE)
  
  
}