# Chase Abram
# R_est
# Assignment 2, Internation Macro and Trade
# Fall 2020


# packages <- c("foreign", "fixest", "dplyr", "xtable")
# lapply(packages, library, character.only = TRUE)

# df <- read.dta("col_regfile09.dta")
# df <- df %>% filter(flow > 0)

lf = log(df$flow)
df <- cbind(df, logflow = lf)

ld <- log(df$distw)
df <- cbind(df, logdist = ld)

start_time <- Sys.time()
my_reg <- feols(fml = logflow ~ logdist + contig 
                + comlang_off|year^iso_o+year^iso_d, 
                data = df)
end_time <- Sys.time()

print(end_time - start_time)

# s <- summary(my_reg)
etable(my_reg, tex = TRUE)






