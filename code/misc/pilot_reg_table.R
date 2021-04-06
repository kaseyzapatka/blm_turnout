load("temp/ga_models.RData")
models1_ga <- models1
ses_cl_ga <- ses_cl

load("temp/nc_models.RData")
models1_nc <- models1
ses_cl_nc <- ses_cl
rm(models1, ses_cl)

load("temp/oh_models.RData")
models1_oh <- models1
ses_cl_oh <- ses_cl
rm(models1, ses_cl)
##############################################

stargazer(models1_ga, models1_nc, models1_oh,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Distance", "Distance Sq.", "Lived within 5 Miles",
                               "Democrat", "Black", "White", "Age", "Female",
                               "Median Income", "Population Density", "Share with Some College"),
          dep.var.labels.include = FALSE,
          title = "\\label{tab:reg} General Election Turnout, 2010 {--} 2018",
          table.placement = "H",
          omit.stat = c("f", "ser"),
          table.layout = "-cm#-t-a-s-n",
          out = "./temp/bigreg.tex",
          out.header = F,
          notes = "TO REPLACE",
          se = c(ses_cl_ga, ses_cl_nc, ses_cl_oh),
          column.labels = c("Georgia", "North Carolina", "Ohio"),
          column.separate = c(3, 3, 3),
          omit = c("reg_date", "v14", "v16", "v18", "county"),
          add.lines = list(c("County Fixed Effects", rep("X", 9)),
                           c("General Election Turout, 2012 - 2018", rep("X", 9))),
          star.cutoffs = c(0.05, 0.01, 0.001))

j <- fread("./temp/bigreg.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{10}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$. \\\\Robust standard errors (clustered by county) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))

insert1 <- "\\resizebox{1\\textwidth}{!}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(3.1, nrow(j) + 1 - 0.01))) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  select(-n)


write.table(j, "./temp/big_reg_formatted.tex", quote = F, col.names = F,
            row.names = F)

