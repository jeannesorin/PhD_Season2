using SparseArrays
using LinearAlgebra
using Plots
S_tot = 1000
cost = 0
r = 0.05
delta = 1.0/1.05
N = 501
# part a
steps = Array(0:2:S_tot)
# part b
A = Array(0:2:S_tot)
nA = length(A)
# part c
u1(y) = 2*(sqrt(y))
u2(y) = 5*y - .05*(y^2)
# part d
action_utility1 = zeros(nA)
action_utility2 = zeros(nA)
for i = 1:nA
    action_utility1[i] = u1((i-1)*2)
    action_utility2[i] = u2((i-1)*2)
end
numrows = N
numcols = nA
U1 = [ y<=x ? action_utility1[y] : -Inf for x in 1:numrows, y in 1:numcols]
U2 = [ y<=x ? action_utility2[y] : -Inf for x in 1:numrows, y in 1:numcols]
# part e
Uindex = [ y <= x ? x-y+1 : 1 for x in 1:numrows, y in 1:numcols]
new_numcols = N*nA
# part f
T = [x == Uindex[y] ? 1 : 0  for x in 1: numrows, y in 1:new_numcols]
for i = 1:N
    block_begin = nA*(i-1)+1
    block_end = nA*i
    T[:,block_begin:block_end] = transpose(T[:,block_begin:block_end])
end
T = sparse(T)
# part g
# initialize a Vnext and V matrix
for utility in 1:2
    if utility == 1
        U = U1
    else
        U = U2
    end
    global V = [0.0 for i = 1:N]
    global C = [0.0 for i = 1:N]
    distance = 10.0
    while distance > 1e-8
    # given V, calculate Vnext
        V_old = V
        Vnext = [0.0 for i = 1:N, j =  1:nA]
        for i = 1:nA
            block_begin = nA*(i-1)+1
            block_end = nA*i
            T_next = T[:, block_begin: block_end]
            vals = T_next * V
            Vnext[:,i] = vals
        end
        # using Vnext, update V
        (V, C) = findmax((U + delta*Vnext), dims = 2)
        distance = norm(V-V_old)
    end
    # part h
    # solving for T_opt using C
    T_opt = zeros(N, N)
    for i in 1:N
        extract = C[i][2]
        state = C[i][1]
        next_state = state - extract + 1
        T_opt[state, next_state] = 1
    end
    # part i
    stock = [N]
    indices = Array(1:N)
    for t in 1:80
        today_i = stock[t]
        tomorrows = T_opt*indices
        tomorrow_i = tomorrows[today_i]
        append!(stock, tomorrow_i)
    end
    stock = 2*stock .- 2
    extraction = []
    for t in 1:80
        extract = stock[t] - stock[t+1]
        append!(extraction, extract)
    end
    x = 1:80
    y = extraction
    plot(x, y)
    p = plot(x, y,
        xlabel = "Time",
        ylabel = "Extraction",
        linecolor = :dodgerblue,
        fontfamily = "Courier New, monospace",
        legend = false
    )
    png(p, string(figpath, "p1_utility", string(utility), ".png"))
end
