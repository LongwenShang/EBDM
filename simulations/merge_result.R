results_dir <- "./results"
rda_files <- list.files(results_dir, pattern = "^setting[1-4]_.*\\.rda$", full.names = TRUE)

result_list_by_setting <- list(setting1 = list(), setting2 = list(),
                               setting3 = list(), setting4 = list())

for (file in rda_files) {
  temp_env <- new.env()
  load(file, envir = temp_env)
  obj_name <- ls(temp_env)[1]
  result_data <- get(obj_name, envir = temp_env)
  
  setting_id <- sub(".*setting([1-4])_.*", "setting\\1", basename(file))
  result_list_by_setting[[setting_id]][[file]] <- result_data
}
result1 <- data.frame(do.call(rbind, result_list_by_setting$setting1))
result2 <- data.frame(do.call(rbind, result_list_by_setting$setting2))
result3 <- data.frame(do.call(rbind, result_list_by_setting$setting3))
result4 <- data.frame(do.call(rbind, result_list_by_setting$setting4))

save(result1, file = "./result_1.rda")
save(result2, file = "./result_2.rda")
save(result3, file = "./result_3.rda")
save(result4, file = "./result_4.rda")

results_all <- rbind(result1, result2, result3, result4)
save(results_all, file = "./results_all.rda")
