### Gravity Assignment - Jeanne - Julia


## Import Package
using FixedEffectModels
using StatFiles
using DataFrames
using RegressionTables
using TimerOutputs


## Import data
data = DataFrame(load("col_regfile09.dta"))

## Keep only positive trade flow & generate log & only post 2000
data = data[data.flow .> 0,:]
data = data[data.year .>= 1948,:]
data.lflow = log.(data.flow)
data.flow1 = log.(data.flow.+1)
data.ldistw = log.(data.distw)

## timed regression
time_output = TimerOutput()
@timeit time_output "reg" reg_outcome = reg(data, @formula(lflow ~ contig + comlang_off + ldistw +
    fe(iso_o)&fe(year) + fe(iso_d)&fe(year)), Vcov.robust())

regtable(reg_outcome, renderSettings=latexOutput())
show(time_output, allocations = false)
