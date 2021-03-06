# given example
library(AdaptR)
## Complete example of the multiple steps in applying AdaptR using data example provided for Drosophila jambulina

# 1. Set the filepath to the example data set
filepath.data <- paste0(getwd(), "/inst/extdata")

# 2. Create text files to describe the file path to each input variable, and the output variable
write.table((file.path(filepath.data,"Tmax",paste0("Tmax",seq(1:159),".asc"))), file = file.path(filepath.data,"Tmax","Tmax_filenames.txt"), eol = "\\n", row.names = FALSE, col.names = FALSE, quote=FALSE )
write.table((file.path(filepath.data,"Habitat",paste0("Habitat",seq(1:159),".asc"))), file = file.path(filepath.data,"Habitat","Habitat_filenames.txt"), eol = "\\n", row.names = FALSE, col.names = FALSE, quote=FALSE )
write.table((file.path(filepath.data,"compact_grids",paste0("demo_compact_grids_T",seq(1:159)))), file = file.path(filepath.data,"compact_grids","demo_compact_grids_output_filenames.txt"), eol = "\\n", row.names = FALSE, col.names = FALSE, quote=FALSE )

# 3. Run the grid compactor
CompactGrids(compactor.parameter.file.name = file.path(filepath.data,"species_inputs","Jambulina__grid_compactor_parameter_file.txt"),
             ncols = 100,
             nrows = 79,
             n.env.vars = 2,
             n.time.points = 159,
             raw.env.grids.name.files = c(file.path(filepath.data,"Tmax","Tmax_filenames.txt"),file.path(filepath.data,"Habitat","Habitat_filenames.txt")),
             output.env.name.file = file.path(filepath.data,"compact_grids","demo_compact_grids_output_filenames.txt"))

# 4. Generate a dispersal kernel for the drosophila example

Dispersal_Neighbourhood(radius=5, 
                        type="neg.power", 
                        params=c(1,1), 
                        output.name="Dispersal_relfile_L1_K1_rad5",
                        output.directory=file.path(filepath.data,"species_inputs"),
                        dispersal.plot=TRUE)  

# 5. Create text files to describe the file path to the compact grids
write.table(paste0("demo_compact_grids_T",rep(1:159, length.out=159)), file = file.path(filepath.data,"species_inputs","compact_series_names.txt"), eol = "\\n", row.names = FALSE, col.names = FALSE, quote=FALSE )

# 6. Run the AdaptR model

run.name = "jambulina_test"
parameter.file.name = file.path(filepath.data,"outputs","jambulina_test_parameters.txt")
ncols = 100
nrows = 79
output.folder.path = file.path(filepath.data,"outputs")
verbose.outputs = FALSE
n.time.points = 159
n.env.vars = 2
env.vars.names = c("MaxTemp", "Other_Maxent")
env.grids.folder.path = file.path(filepath.data,"compact_grids")
env.grids.name.file = file.path(filepath.data,"species_inputs","compact_series_names.txt")
species.initial.grid = file.path(filepath.data,"species_inputs","demo_jambulia_initial_distribution.asc")
minimum.survival.percentage = 5
resident.population.weighting = 1000
dispersal.neighbourhood.file = file.path(filepath.data,"species_inputs","Dispersal_relfile_L1_K1_rad5.dna")
species.location.file = file.path(filepath.data,"species_inputs","demo_locations_out.txt")
env.lower.thresholds = c(19.47,0.99)
env.upper.thresholds = c(37.94547,1.01)
env.low.adaptation = c(FALSE,FALSE)
env.high.adaptation = c(TRUE,FALSE)
adapt.limit = c(40,0)
heritability = c(0.53,0)
fitness.cost = c(0.05,0)
adapt.threshold.grids = c(FALSE,FALSE)
phenotypic.sd.grid = c(FALSE,FALSE)
phenotypic.sd.value = c(1.106,0)
plasticity = c(1.106,0)


AdaptR(run.name = "jambulina_test",
       parameter.file.name = file.path(filepath.data,"outputs","jambulina_test_parameters.txt"),
       ncols = 100,
       nrows = 79,
       output.folder.path = file.path(filepath.data,"outputs"),
       verbose.outputs = FALSE,
       n.time.points = 159,
       n.env.vars = 2,
       env.vars.names = c("MaxTemp", "Other_Maxent"),
       env.grids.folder.path = file.path(filepath.data,"compact_grids"),
       env.grids.name.file = file.path(filepath.data,"species_inputs","compact_series_names.txt"),
       species.initial.grid = file.path(filepath.data,"species_inputs","demo_jambulia_initial_distribution.asc"),
       minimum.survival.percentage = 5,
       resident.population.weighting = 1000,
       dispersal.neighbourhood.file = file.path(filepath.data,"species_inputs","Dispersal_relfile_L1_K1_rad5.dna"),
       species.location.file = file.path(filepath.data,"species_inputs","demo_locations_out.txt"),
       env.lower.thresholds = c(19.47,0.99),
       env.upper.thresholds = c(37.94547,1.01),
       env.low.adaptation = c(FALSE,FALSE),
       env.high.adaptation = c(TRUE,FALSE),
       adapt.limit = c(40,0),
       heritability = c(0.53,0),
       fitness.cost = c(0.05,0),
       adapt.threshold.grids = c(FALSE,FALSE),
       phenotypic.sd.grid = c(FALSE,FALSE),
       phenotypic.sd.value = c(1.106,0),
       plasticity = c(1.106,0))

# 7. Map the predictions of the model (at the last time point)
map.single.run(output.folder.path = file.path(filepath.data,"outputs"), 
               run.name = "jambulina_test")
