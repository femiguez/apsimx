## Run a few tests for the examples

run.example.tests <- FALSE

ex.dir <- auto_detect_apsimx_examples()

examples <- c("Barley","Maize","Oats", 
              "Sugarcane", "Wheat")

if(run.example.tests){
  for(i in examples){
    ex.tst <- apsimx_example(i)
    cat("Ran: ", i, "\n")
  }
}
