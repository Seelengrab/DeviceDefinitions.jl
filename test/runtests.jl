using SVD2Julia
using Test

@testset "All Tests" begin

@testset "Basic Example" begin
    svd_path = joinpath(@__DIR__, "example.svd")
    device = SVD2Julia.readSVD(svd_path)
    @testset "Read basic example" begin
        @test device isa SVD2Julia.Device
    end
    @testset "Generate Basic Example" begin
        mktempdir() do path
            SVD2Julia.generateProject("Example", svd_path, path)
            @test "Example.jl" in readdir(path)
            for (obj, _file) in (("src",false), 
                                 ("Project.toml",true), 
                                 ("Manifest.toml",true), 
                                 (".gitignore",true))
                @test obj in readdir(joinpath(path, "Example.jl"))
                if _file
                    @test isfile(joinpath(path, "Example.jl", obj))
                else
                    @test isdir(joinpath(path, "Example.jl", obj))
                end
            end
            for (obj, _file) in (("peripherals.jl",true), 
                                 ("peripherals",false), 
                                 ("SVD",false), 
                                 ("Example.jl",true))
                @test obj in readdir(joinpath(path, "Example.jl", "src"))
                if _file
                    @test isfile(joinpath(path, "Example.jl", "src", obj))
                else
                    @test isdir(joinpath(path, "Example.jl", "src", obj))
                end
            end
            @test length(readdir(joinpath(path, "Example.jl", "src", "peripherals"))) == length(device.peripherals)
        end
    end
end


end # all tests
