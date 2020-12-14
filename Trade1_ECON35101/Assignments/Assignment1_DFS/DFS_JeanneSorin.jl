### DFS Assignment 1 - Jeanne Sorin

## Import Packages
using Plots
using DelimitedFiles
using Printf
using Random

## Define Functions
# Define A(z) schedule
function A_f(a)
    a_f = a[:,1]
    a_h = a[:,2]
    a_f ./ a_h
end

# Define B(z) schedule
function B_f(L, b)
    L_f = L[1]
    L_h = L[2]
    υ = cumsum(b)
    (υ ./ (1 .- υ)) .* (L_f / L_h)
end

# Define Plot
function plot_f(a, b, L)
    N = size(a)[1]
    z = LinRange(1,N,N)
    A = A_f(a)
    B = B_f(L, b)
    p = Plots.plot(z[1:N-10], [A[1:N-10], B[1:N-10]], label = ["A(z)" "B(z)"],fmt = :png)
    xlabel!("z")
    ylabel!("omega")
    display(p)
end

# Check functions
function check_a_f(a)
    A = A_f(a)
    # verify that a has dimension N-by-2 (where N>2) and is non-negative
    size(a)[1] > 2 || error("Error: N <= 2")
    size(a)[2] == 2 || error("Error: a does not have dimension N-by-2")
    all(>=(0),a) || error("Error: a is not non-negative")

    # verify that A = a[:,1]./a[:,2] is monotone decreasing (equation 1 in DFS)
    for i in 1:(size(A)[1]-1)
        A[i,1] >= A[i+1,1] || error("Error: A is not monotone decreasing")
    end
end

function check_b_f(b, a)
    # verify that b is a vector of dimension N (the same length as A), strictly positive, and that sum(b)==1
    size(b)[1] == size(a)[1] || error("Error: b is not a vector of dimension N (same length as A)")
    all(>=(0),b) || error("Error: b is not non-negative")
    isapprox(sum(b), 1) || error("Error: sum(b) != 1")
end

function check_g_f(g)
    # verify that g is a scalar in (0,1] (as assumed in DFS III.B)
    isa(g, Union{Number,AbstractString,Char,Bool}) && (g <= 1) && (g > 0) || error("Error: g is not a scalar in (0, 1]")
end


### Solver
# Define solver (manual solver equating the A and B schedule)
function DFS1977solver(a::Array{Float64,2}, b::Array{Float64,1}, L::Array{Float64,1}, g::Float64)
    ### Checks
    check_a_f(a)
    check_b_f(b,a)
    check_g_f(g)

    ### Define A & B
    A = A_f(a)
    B = B_f(L, b)
    N = size(a)[1]
    z = LinRange(1,N,N)
    L_f = L[1]
    L_d = L[2]

    ### Solve for z_bar & z_bar_star
    # eq 21: z_bar = A^{-1}(gw) --> with w being each element of A,
    # find z_bar by minimizing abs(A - g*w)
    z_bar_v = [findmin(abs.(A .- wA .* g))[2] for wA in A]
    z_bar_star_v = [findmin(abs.(A .- wA ./ g))[2] for wA in A]

    ### Solve for λ and λ_star
    λ = [sum(b[1:z_bar_e]) for z_bar_e in z_bar_v]
    λ_star = 1 .- [sum(b[1:z_bar_star_e]) for z_bar_star_e in z_bar_star_v]

    ### Solve for ω_bar
    intersec = findmin(abs.(A .- (1 .- λ_star) ./ (1 .- λ)) .* (L_f / L_d))[2]
    ω_bar = A[intersec]

    ### Solve for z_bar_eq and z_bar_star_eq
    z_bar = findmin(abs.(A .- ω_bar .* g))[2]
    z_bar_star = findmin(abs.(A .- ω_bar ./ g))[2]

    z_bar, z_bar_star, ω_bar
end


### Welfare Function
function DFS1977welfare(a::Array{Float64,2}, b::Array{Float64,1}, L::Array{Float64,1}, g::Float64)
    N = size(a)[1]
    # Set numeraire good
    wh = 1.0

    # Obtain z_bar & ω_bar
    z_bar, z_bar_star, ω_bar = DFS1977solver(a, b, L, g)
    # Recover wf
    wf = wh / ω_bar

    # Compute prices assuming perfect competition
    ph = a[:,2] .* wh
    pf = a[:,1] .* wf

    # Compute welfare before & after trade for both the home & foreign countries
    # Note that when importing, the price must be adjusted for transport costs g
    welfare_h = log(wh) - sum(b.*log.(ph))
    welfare_h_trade = log(wh) - (sum(b[1:z_bar-1].*log.(ph[1:z_bar-1])) + sum(b[z_bar:N].*log.(pf[z_bar:N] ./ g)))

    welfare_f = log(wf) - sum(b.*log.(pf))
    welfare_f_trade = log(wf) - (sum(b[1:z_bar_star].*log.(ph[1:z_bar_star] ./ g)) + sum(b[z_bar_star+1:N].*log.(pf[z_bar_star+1:N])))

    gft_home = welfare_h_trade - welfare_h
    gft_foreign = welfare_f_trade - welfare_f

    gft_home, gft_foreign, welfare_h, welfare_h_trade, welfare_f, welfare_f_trade
end


##################################################
### Run these big boys (to uncomment to produce figures in the latex doc)
a = readdlm("DFS1977_example_a.txt", '\t')
b = readdlm("DFS1977_example_b.txt", '\t')[:,1]
L = [1., 1.]

## Plot
plot_f(a, b, L)
png("Fig1")

## Solver for g=1.0, g=0.9
DFS1977solver(a, b, L, 1.0)
DFS1977solver(a, b, L, 0.9)


## Welfare Gains / Comparative Statics
# Gains from trade with initial vectors
gft_home1, gft_foreign1, welfare_h1, welfare_h_trade1, welfare_f1, welfare_f_trade1 = DFS1977welfare(a, b, L, 1.0)
# New a such that a* is uniformly lower (foreign more productive)
a_progress = copy(a)
a_progress[:,1] = 0.5.*a_progress[:,1]
gft_home2, gft_foreign2, welfare_h2, welfare_h_trade2, welfare_f2, welfare_f_trade2 = DFS1977welfare(a_progress, b, L, 1.0)

# Check that both countries experience an increase in welfare when Foreign experiences uniform techno progress
welfare_h_trade1 <= welfare_h_trade2 || error("Home doesn't experience an increase in welfare")
welfare_f_trade1 <= welfare_f_trade2 || error("Foreign doesn't experience an increase in welfare")


# Gains from Trade for different g
vector = LinRange(0.01,1,100)
home_gft = [DFS1977welfare(a, b, L, v)[1] for v in vector]
foreign_gft = [DFS1977welfare(a, b, L, v)[2] for v in vector]
plot(vector, [home_gft, foreign_gft],  label = ["gft home" "gft foreign"])
xlabel!("g")
ylabel!("GFT")
png("Fig2_WelfareGains_g")



## Different equilibria : same volume of trade, different gains from trade
## Example: moving B
# An example (does not generalize to other a and b vectors as such,
# my guess is that on the traded interval, the shape of A balances the change in
# the shape of B, such that the traded volume is the same.
# Generalize it would require a little more work on how exactly (analytically)
# adjusting B)
a = readdlm("DFS1977_example_a.txt", '\t')
L = [1., 1.]

a_original = copy(a)
N = size(a_original)[1]
b1 = ones(N) ./ N
z_bar3, z_bar_star3, ω_bar3 = DFS1977solver(a_original, b1, L, 0.9)
gft_home3, gft_foreign3, welfare_h3, welfare_h_trade3, welfare_f3, welfare_f_trade3 = DFS1977welfare(a_original, b1, L, 0.9)

b2 = copy(b1)
minv = findmin(b2)[1] / (N/2 + 1)
for i in 1:(N/2)
    j = Int(i)
    b2[Int((N/2 +1)-j)] = b2[Int((N/2 +1)-j)] .+ minv .* j
    b2[Int((N/2)+j)] = b2[Int((N/2)+j)] .- minv .* j
end
# Manual adjustments to get the approximation round up nicely
b2[1] = 0
b2[N] = 1 - sum(b2[1:N-1])
plot(b1)
plot!(b2)
z_bar4, z_bar_star4, ω_bar4 = DFS1977solver(a_original, b2, L, 0.9)
gft_home4, gft_foreign4, welfare_h4, welfare_h_trade4, welfare_f4, welfare_f_trade4 = DFS1977welfare(a_original, b2, L, 0.9)

# Check
z_bar3 - z_bar_star3 == z_bar4 - z_bar_star4 ||error("Error: Different volume of trade")
print(" Volumes of trade are ", z_bar3 - z_bar_star3, " and ", z_bar4 - z_bar_star4, "\n")
gft_home4 != gft_home3 ||error("Error: Same gft for home")
print(" GTF home are ", gft_home3, " and ", gft_home4, "\n")
gft_foreign4 != gft_foreign3 ||error("Error: Same gft for home")
print(" GTF foreign are ", gft_foreign3, " and ", gft_foreign4, "\n")

plot_f(a, b1, L)
N = size(a)[1]
z = LinRange(1,N,N)
B2 = B_f(L, b2)
plot!(B2[1:N-10], label="B(Z)'")
png("Fig3")
