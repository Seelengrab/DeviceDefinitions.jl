function generateJuliaDefinitions(dir::String, d::Device)
    isfile(dir) && throw(ArgumentError("Directory `$dir` already exists!"))
    mkpath(dir)
    path = joinpath(dir, "peripherals.jl")
    open(path, "w") do io
        generateJuliaDefinitions(io, d, dir)
    end
end

function generateJuliaDefinitions(io::IO, d::Device, basedir="")
    println(io, "module Peripherals\n")
    npath = joinpath(basedir, "peripherals")
    mkpath(npath)
    for p in d.peripherals
        generatePeripheralDefinition(p, npath)
        println(io, "include(\"peripherals/$(lowercase(string(p.name))).jl\")")
    end
    println(io, "\nend # module")
end

function generatePeripheralDefinition(p::Peripheral, path="")
    npath = joinpath(path, lowercase(p.name.value)*".jl")
    desc = something(p.description, "No description of peripheral!")
    open(npath, "w") do io
        print(io, """
        \"\"\"
            $(p.name)

        $desc
        \"\"\"
        """)
        println(io, "module ", p.name, "\n")
        println(io, "const baseAddress = Ptr{UInt32}(", p.baseAddress.value, ")")
        ab = something(p.addressBlock)
        println(io)
        header = """
        using MCUCommon: @regdef, Read, Write, ReadWrite, ReadWriteOnce
        using ..$(p.name): baseAddress
        """
        for r in @something p.registers Some(())
            generateRegisterDefinition(io,
                    r,
                    string(@something p.prependToName Some("")),
                    string(@something p.appendToName Some("")),
                    header)
        end
        for c in @something p.clusters Some(())
            generateClusterDefinition(io,
                    c,
                    string(@something p.prependToName Some("")),
                    string(@something p.appendToName Some("")),
                    header)
        end
        println(io, "end # peripheral\n")
    end
end

function generateClusterDefinition(io::IO, c::Cluster, prefix::AbstractString, postfix::AbstractString, moduleHeader)
    desc = something(c.description, "No description of register!")
    println(io, """
    \"\"\"
        $(c.name)

    $desc
    \"\"\"
    module $(c.name)Mod
    """)
    println(io, moduleHeader)
    print(io, """
    const regAddress = baseAddress + $(c.addressOffset)
    @regdef struct $prefix$(c.name)(regAddress)
    """)
    for r in @something c.registers Some(())
        generateRegisterDefinition(io,
                r,
                prefix,
                postfix,
                moduleHeader)
    end
    for c in @something c.clusters Some(())
        generateClusterDefinition(io,
                c,
                prefix,
                postfix,
                moduleHeader)
    end
    println(io, "end # cluster\n")
end

function generateRegisterDefinition(io::IO, r::Register, prefix::AbstractString, postfix::AbstractString, moduleHeader)
    desc = something(r.description, "No description of register!")
    println(io, """
    \"\"\"
        $(r.name)

    $desc
    \"\"\"
    module $(r.name)Mod
    """)
    println(io, moduleHeader)
    print(io, """
    const regAddress = baseAddress + $(r.addressOffset)
    @regdef struct $prefix$(r.name)$(postfix)Struct(regAddress)
    """)

    lastOffset = lastWidth = 0
    fielditr = @something r.fields Some(())
    # isempty(fielditr) && @warn "Register has no fields!" Register=r.name
    for f in fielditr
        if lastOffset + lastWidth < first(f.bitRange)
            println(io, "\t_:", first(f.bitRange) - (lastOffset+lastWidth))
        end
        println(io, "\t", f.name, ":", length(f.bitRange), "::", f.access)
        lastOffset = first(f.bitRange)
        lastWidth = length(f.bitRange)
    end
    rsize = something(r.rpg.size, (;value=0)).value
    if lastOffset + lastWidth < rsize
        println(io, "\t", "_:", rsize - (lastOffset + lastWidth))
    end

    println(io, """
    end
    const Reg = $prefix$(r.name)$(postfix)Struct
    """)
    for f in r.fields
        generateFieldDefinition(io, r, f)
    end
    println(io, "end # register $(r.name)\n")
end

function generateFieldDefinition(io::IO, r::Register, f::Field)
    desc = something(f.description, "Field has no description!")
    println(io, """
    \"\"\"
        $(f.name)

    $desc
    \"\"\"
    $(f.name)
    """)
end

