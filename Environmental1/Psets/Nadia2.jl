using SparseArrays
using LinearAlgebra
using Plots
using Interpolations
using Plotly
S_tot = 1000
cost = 0
r = 0.05
delta = 1.0/1.05
N = 501
figpath = "/Users/nadialucas/Dropbox/Second year/PPHA 44320/computational_pset/"
# part a
steps = Array(0:2:S_tot)
# part b
A = Array(0:2:S_tot)
nA = length(A)
sqrt_actions = Array(0: sqrt(S_tot)/500: sqrt(S_tot))
actions = [i^2 for i in sqrt_actions]
# part c
u1(y) = 2*(sqrt(y))
u2(y) = 5*y - .05*(y^2)
# part d
action_utility1 = zeros(nA)
action_utility2 = zeros(nA)
for i = 1:nA
    action_utility1[i] = u1(actions[i])
    action_utility2[i] = u2(actions[i])
end
numrows = N
numcols = nA
U1 = [ actions[y]<=steps[x] ? action_utility1[y] : -Inf for x in 1:numrows, y in 1:numcols]
U2 = [ actions[y]<=steps[x] ? action_utility2[y] : -Inf for x in 1:numrows, y in 1:numcols]
# linear interpolation
# part e
next_state = [ actions[y] <= steps[x] ? steps[x] - actions[y] : 0 for x in 1:numrows, y in 1:numcols]
T = fill(0.0, (N, N*nA))
for i = 1:N
    for j = 1:nA
        lo = searchsortedlast(steps, next_state[i, j])
        hi = searchsortedfirst(steps, next_state[i, j])
        if lo == hi
            T[i, ((j-1)*nA + lo)] = 1
        else
            prob_lo = 1-(next_state[i, j] - steps[lo])/2
            prob_hi = 1-prob_lo
            T[i, ((j-1)*nA + lo)] = prob_lo
            T[i, ((j-1)*nA + hi)] = prob_hi
        end
    end
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
        extract = actions[C[i][2]]
        state = steps[C[i][1]]
        state_i = C[i][1]
        next = state - extract
        # now linearly interpolate
        lo = searchsortedlast(steps, next)
        hi = searchsortedfirst(steps, next)
        if lo == hi
            T_opt[state_i, lo] = 1
        else
            prob_lo = 1-(next - steps[lo])/2
            prob_hi = 1-prob_lo
            T_opt[state_i, lo] = prob_lo
            T_opt[state_i, hi] = prob_hi
        end
    end
    # part i
    stock = [convert(Float64, S_tot)]
    for t in 1:80
        today_stock = stock[t]
        tomorrows = T_opt*steps
        lo = searchsortedlast(steps, today_stock)
        hi = searchsortedfirst(steps, today_stock)
        if lo == hi
            tomorrow_stock = tomorrows[lo]
        else
            prob_lo = 1-(today_stock - steps[lo])/2
            prob_hi = 1-prob_lo
            tomorrow_stock = prob_lo * tomorrows[lo] + prob_hi * tomorrows[hi]
        end
        append!(stock, tomorrow_stock)
    end
    extraction = []
    for t in 1:80
        extract = stock[t] - stock[t+1]
        append!(extraction, extract)
    end
    x = 1:80
    y = extraction
    p = plot(x, y,
        xlabel = "Time",
        ylabel = "Extraction",
        linecolor = :dodgerblue,
        fontfamily = "Courier New, monospace",
        legend = false
    )
    png(p, string(figpath, "p2_utility", string(utility), ".png"))
end
