## A few steps
require(apsimx)
apsimx_options(warn.versions = FALSE)

run.factorial.example <- TRUE

if(run.factorial.example){

  ex.dir <- auto_detect_apsimx_examples()
  
  ## Copy 'Factorial' example to current directory
  fact.ex <- paste0(ex.dir,"/Factorial.apsimx")
  file.exists(fact.ex)
  file.copy(from = fact.ex,
            to = ".", copy.mode = FALSE)
  
  ## I ran these commands at the command line
  ## sed -i'.bak' -e 's/\bWeatherFiles/%root%\\\\\\\\Examples\\\\\\\\WeatherFiles/g *

  inspect_apsimx_replacement("Factorial", src.dir = ".",
                             root = list("Experiment",1),
                             node = "Base",
                             node.child = "Weather")
  for(i in 1:5){
    edit_apsimx_replacement("Factorial", src.dir = ".",
                            root = list("Experiment",i),
                            node = "Base", 
                            node.child = "Weather",
                            parm = "FileName",
                            overwrite = TRUE,
                            verbose = TRUE,
                            value = paste0(ex.dir,"/WeatherFiles/lincoln.met"))
  }

  inspect_apsimx_replacement("Factorial",
                             root = list("Experiment",2),
                             node = "Base",
                             node.child = "Weather")

  fact <- apsimx("Factorial.apsimx")
  file.remove("Factorial.apsimx")
  file.remove("Factorial.db")
}

run.factorial.example2.inspect <- TRUE

if(run.factorial.example2.inspect){

  ex.dir <- auto_detect_apsimx_examples()

  ## Inspect factorial
  inspect_apsimx("Factorial", src.dir = ex.dir,
                 root = c("RangeExperiment","Base2"),
                 node = "Weather")

  inspect_apsimx("Factorial", src.dir = ex.dir,
                 root = c("RangeExperiment","Base2"),
                 node = "Clock")
  
  
}

run.factorial.example2.edit <- TRUE

if(run.factorial.example2.edit){
  
  ex.dir <- auto_detect_apsimx_examples()
  ## Copy 'Factorial' example to current directory
  fact.ex <- paste0(ex.dir,"/Factorial.apsimx")
  file.exists(fact.ex)
  file.copy(from = fact.ex,
            to = ".", copy.mode = FALSE)
  
  factor.label <- c("^Experiment", "RangeExperiment", "OperationsExpt", "Compound", "ManagerExpt")
  bases <- c("Base","Base2","Base3","Base4","Base5")
  
  for(i in 1:5){
    edit_apsimx("Factorial.apsimx", 
                node = "Weather",
                overwrite = TRUE,
                root = c(factor.label[i],bases[i]), 
                value = paste0(ex.dir,"/WeatherFiles/lincoln.met"))
  }
  
  fact2 <- apsimx("Factorial.apsimx")
  file.remove("Factorial.apsimx")
  file.remove("Factorial.db")
  
}
