function generateProject(name::String, svd_path, parent_dir::String=pwd())
    device = readSVD(svd_path)    
    has_suffix = endswith(name, ".jl")
    projName = has_suffix ? name : name*".jl"
    moduleName = has_suffix ? name[begin:end-3] : name
    projdir = joinpath(parent_dir, projName)
    project_exists = isdir(projdir)
    srcpath = joinpath(projdir, "src")
    mainModuleFile = joinpath(srcpath, projName)
    if project_exists
        @warn "Project directory already exists - make sure to bump its version after regenerating!"
        @info "Removing existing src/peripherals directory"
        rm(joinpath(srcpath, "peripherals.jl"))
        rm(joinpath(srcpath, "peripherals"); recursive=true, force=true)
        mkpath(srcpath)
        open(mainModuleFile, "w") do io
            print(io, """
            module $moduleName

            # This is an empty module, so Pkg is happy

            end # module
            """)
        end
    else
        cd(parent_dir) do
            Pkg.generate(projName)
        end
    end
    # Project was generated, copy SVD
    svd_proj_path = joinpath(srcpath, "SVD")
    mkpath(svd_proj_path)
    cp(svd_path, joinpath(svd_proj_path, basename(svd_path)); force=true)
    # generate the project dir first, so there's no need to precompile all of the definitions
    Pkg.activate(projdir)
    Pkg.pkg"add MCUCommon"
    Pkg.compat("MCUCommon", "0.1.5")
    open(joinpath(projdir, ".gitignore"), "w") do io
        println(io, "Manifest.toml")
    end
    # write out the definitions
    open(mainModuleFile, "w") do io
        print(io, """
        module $moduleName

        # This project was generated from the SVD file found under `src/SVD`.

        #=
        The following is the original license text of the SVD file.
        Its license may not necessarily apply to this generated code.

        """)
        print(io, @something device.licenseText Some("No license text found!"))
        print(io, """

        =#

        using MCUCommon: Register, Field

        include("peripherals.jl")
        
        end # module""")
    end
    generateJuliaDefinitions(srcpath, device)
    println("Done!")
end
