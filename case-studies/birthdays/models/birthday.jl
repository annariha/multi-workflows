import ReparametrizableDistributions: TScaleHierarchy
birthday(
    # // data
    x,
    y,
    day_of_week,
    n_dow,
    day_of_year,
    n_doy,
    doy_type,
    n_floating_groups,
    floating_days,
    floating_day_groups,
    # // discretization parameters
    gp1_boundary_factor,
    gp2_boundary_factor,
    gp3_boundary_factor,
    # // parametrization parameters
    gp1_shift,
    gp1_centeredness,
    gp2_shift,
    gp2_centeredness,
    gp3_shift,
    gp3_centeredness,
    doy_centeredness,
    # // convenience
    likelihood=!ismissing(y)
) = ReparametrizablePosterior((;
    gp1=HSGP(
        Normal(), Normal(), Normal(), 
        x=x, boundary_factor=gp1_boundary_factor,
        intercept_shift=gp1_shift, centeredness=gp1_centeredness
    ),
    # Below is here because I forgot to remove it in the Stan model, but will do later
    gp2_intercept=Normal(), 
    gp2=PHSGP(
        Normal(), Normal(), 
        x=x, boundary_factor=gp2_boundary_factor,
        centeredness=gp2_centeredness
    ),
    # I should remove the intercept from the below GP
    gp3=HSGP(
        Normal(), Normal(), Normal(), 
        x=x, boundary_factor=gp3_boundary_factor,
        intercept_shift=gp3_shift, centeredness=gp3_centeredness
    ),
    dow_raw=Product([Normal() for i in 1:n_dow]),
    doy_raw=(
        ScaleHierarchy(Normal(), doy_centeredness),
        TScaleHierarchy(Normal(3,1), Normal(), doy_centeredness),
        RHS(100., 1., 2., 100., .1, doy_centeredness)
    )[doy_type],
    floating_raw=Product([Normal() for i in 1:n_floating_groups]),
    sigma_raw=Normal()

)) do invariants

    # gp1_intercept is included in gp1.y
    intercept = zeros(size(y))
    if n_dow > 1
        dow = demean(invariants.dow_raw.draw);
        if length(gp3_centeredness) > 0
            intercept += exp.(invariants.gp3.y .- invariants.gp3.intercept.intercept) .* dow[day_of_week];
        else
            intercept += dow[day_of_week];
        end
    end
    if n_doy > 1
        doy = demean(invariants.doy_raw.weights)
        intercept += doy[day_of_year];
    end
    intercept[floating_days] .= invariants.floating_raw.draw[floating_day_groups] .- invariants.gp1.intercept.intercept
    y_mean = intercept .+ invariants.gp1.y .+ invariants.gp2.y 
    y_sd = exp.(invariants.sigma_raw.draw)
    lpdf = likelihood ? sum(logpdf.(Normal.(y_mean, y_sd), y)) : 0
    (;lpdf, y_mean, y_sd)
end

birthday(data) = birthday(
    # // data
    data["x"] |> Vector{Float64},
    data["y"] |> Vector{Float64},
    data["day_of_week"] |> Vector{Int64},
    data["n_dow"] |> Int64,
    data["day_of_year"] |> Vector{Int64},
    data["n_doy"] |> Int64,
    data["doy_type"] |> Int64,
    data["n_floating_groups"] |> Int64,
    data["floating_days"] |> Vector{Int64},
    data["floating_day_groups"] |> Vector{Int64},
    # // discretization parameters
    data["gp1_boundary_factor"] |> Float64,
    data["gp2_boundary_factor"] |> Float64,
    data["gp3_boundary_factor"] |> Float64,
    # // parametrization parameters
    data["gp1_shift"] |> Vector{Float64},
    data["gp1_centeredness"] |> Vector{Float64},
    data["gp2_shift"] |> Vector{Float64},
    data["gp2_centeredness"] |> Vector{Float64},
    data["gp3_shift"] |> Vector{Float64},
    data["gp3_centeredness"] |> Vector{Float64},
    data["doy_centeredness"] |> Vector{Float64},
    # // convenience
    data["likelihood"] != 0
    
)