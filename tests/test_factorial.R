## A few steps

run.factorial.example <- FALSE

if(run.factorial.example){
  ## Copy 'Factorial' example to current directory
  fact.ex <- paste0(ex.dir,"/Factorial.apsimx")
  file.exists(fact.ex)
  file.copy(from = fact.ex,
            to = ".", copy.mode = FALSE)

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
}