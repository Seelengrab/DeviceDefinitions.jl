using SVD2Julia
using Test

@testset "All Tests" begin

svd = SVD2Julia.readSVD(joinpath(@__DIR__, "example.svd"))
@test svd isa SVD2Julia.Device

end # all tests
