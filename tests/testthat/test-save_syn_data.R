### tests voor save_syn_data.R

### happy path: werkt functie zoals verwacht bij normale invoer?
# verwacht resultaat is: opgeslagen bestand in gespecificeerde path met gespecificeerde naam

test_that("save_syn_data() slaat correct een synobject op als CSV-bestand", {
  syn_obj <- list(syn = data.frame(a = 1:3, b = 4:6))
  # Gebruik een tijdelijke directory voor veiligheid
  temp_dir <- tempdir()
  # Vervang alle backslashes door schuine strepen voor consistentie en voeg een schuine streep toe aan het einde
  temp_dir <- gsub("\\", "/", temp_dir, fixed = TRUE)
  if (!grepl("/$", temp_dir)) {temp_dir <- paste0(temp_dir, "/")}
  # Maak een bestandspad voor de CSV binnen de tijdelijke directory
  # ook voor Rds nodig om met withr::defer het CSV en RDS bestand weer te verwijderen
  file_pathCSV <- file.path(temp_dir, "syndata_v01.csv", fsep = "")
  file_pathRDS <- file.path(temp_dir, "syndata_v01.Rds", fsep = "")
  # Voeg opruimen toe met withr::defer() om de bestanden te verwijderen na de test
  withr::defer({ if (file.exists(file_pathCSV)) file.remove(file_pathCSV) })
  withr::defer({ if (file.exists(file_pathRDS)) file.remove(file_pathRDS) })
  # Roep de functie aan en voeg debug-informatie toe
  save_syn_data(syn_object = syn_obj, path = temp_dir, file_name = "syndata_v01", overwrite = TRUE)
  # Controleer of het csv-bestand bestaat op de juiste locatie
  expect_true(file.exists(file_pathCSV), info = paste("Bestand bestaat niet op:", file_pathCSV))
  }
  )


#' save_syn_data(syn_object = syn_object, file_name = "syndata_v01",
#' path = path, overwrite = FALSE)

test_that("save_syn_data() slaat correct een synobject op als Rds-bestand", {
  syn_obj <- list(syn = data.frame(a = 1:3, b = 4:6))
  # Gebruik een tijdelijke directory voor veiligheid
  temp_dir <- tempdir()
  # Vervang alle backslashes door schuine strepen voor consistentie en voeg een schuine streep toe aan het einde
  temp_dir <- gsub("\\", "/", temp_dir, fixed = TRUE)
  if (!grepl("/$", temp_dir)) {temp_dir <- paste0(temp_dir, "/")}
  # Maak een bestandspad voor de Rds binnen de tijdelijke directory
  # ook voor CSV nodig om met withr::defer het CSV en RDS bestand weer te verwijderen
  file_pathCSV <- file.path(temp_dir, "syndata_v01.csv")
  file_pathRDS <- file.path(temp_dir, "syndata_v01.Rds")
  # Voeg opruimen toe met withr::defer() om de bestanden te verwijderen na de test
  withr::defer({ if (file.exists(file_pathCSV)) file.remove(file_pathCSV) })
  withr::defer({ if (file.exists(file_pathRDS)) file.remove(file_pathRDS) })
  # Roep de functie aan en voeg debug-informatie toe
  save_syn_data(syn_object = syn_obj, path = temp_dir, file_name = "syndata_v01", overwrite = TRUE)
  # Controleer of het Rds-bestand bestaat op de juiste locatie
  expect_true(file.exists(file_pathRDS), info = paste("Bestand bestaat niet op:", file_pathRDS))
}
)


### edge case test: test of het werkt bij een grensgeval
# Controleert of de functie een foutmelding geeft als een bestand al bestaat en niet mag worden overschreven.

test_that("save_syn_data() error bij het overschrijven van een bestaand bestand", {
  # create syn_obj
  syn_obj <- list(syn = data.frame(a = 1:3, b = 4:6))

  # use a temp directory
  path <- gsub("\\", "/", tempdir(), fixed = TRUE)
  if (!grepl("/$", path)) {path <- paste0(path, "/")}

  # specify file_name
  file_name <- "syndata_v01"

    # save syn data
  save_syn_data(syn_object = syn_obj, path = path, file_name = file_name, overwrite = FALSE)

  # save syn data again, when overwrite = FALSE to test if this gives an error
  expect_error(save_syn_data(syn_object = syn_obj, path = path, file_name = file_name,
                             overwrite = FALSE),
               regexp = "overwrite")

  # Maak een bestandspad voor csv en Rds binnen de tijdelijke directory
  file_pathCSV <- file.path(path, paste0(file_name, ".csv"))
  file_pathRDS <- file.path(path, paste0(file_name, ".Rds"))
  # Voeg opruimen toe met withr::defer() om de bestanden te verwijderen na de test
  withr::defer({ if (file.exists(file_pathCSV)) file.remove(file_pathCSV) })
  withr::defer({ if (file.exists(file_pathRDS)) file.remove(file_pathRDS) })
  }
)


### error handling en error message tests

### error handling test
# leeg object:  Test of de functie een foutmelding geeft bij een leeg synthetisch object.
test_that("save_syn_data() warning bij het invoeren van een leeg object", {
  syn_obj <- list()
  # use a temp directory
  path <- gsub("\\", "/", tempdir(), fixed = TRUE)
  if (!grepl("/$", path)) {path <- paste0(path, "/")}

  # we verwachten eerst de warning dat syn_obj een leeg object is, en daarna de error
  # message dat het syn_object geen $syn bevat
  expect_error(expect_warning(save_syn_data(syn_object = syn_obj, path = path, file_name = "syndata_v01"),
               regexp = "syn_object you want to save is empty"))

})


### error handling test
# Test of de functie een foutmelding geeft als het synthetische object geen $syn bevat.

test_that("save_syn_data() error bij het ontbreken van syn_obj$syn", {
  syn_obj <- data.frame(a = 1:3, b = 4:6)
  # use a temp directory
  path <- gsub("\\", "/", tempdir(), fixed = TRUE)
  if (!grepl("/$", path)) {path <- paste0(path, "/")}

  expect_error(save_syn_data(syn_object = syn_obj, path = path, file_name = "syndata_v01"),
               regexp = "data input is incorrect")
})

