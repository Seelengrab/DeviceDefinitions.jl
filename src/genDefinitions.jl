struct Value
    name::String
    description::String
    value::String
end

function toAccess(s::AbstractString)
    s == "read-only" && return Read
    s == "write-only" && return Write
    s == "read-write" && return ReadWrite
    s == "read-writeOnce" && return ReadWriteOnce
    @warn "Unknown Access mode string" Mode=s
    return Unknown
end

struct Field
    name::String
    description::String
    bitOffset::UInt
    bitWidth::UInt
    access::Access
    enumeratedValues::Vector{Value}
end

offset(f::Field) = f.bitOffset

function Base.show(io::IO, m::MIME"text/plain", f::Field)
    indent = get(io, :indent, 0)
    inner = indent + 1
    println(io, " "^indent, "Field:")
    println(io, " "^inner,  "name: ",   f.name)
    println(io, " "^inner,  "desc: ",   f.description)
    println(io, " "^inner,  "offset: ", f.bitOffset)
    println(io, " "^inner,  "width: ",  f.bitWidth)
    println(io, " "^inner,  "access: ", f.access)
    nothing
end

struct Register
    name::String
    description::String
    offset::UInt
    size::UInt
    access::Access
    resetValue::UInt32
    resetMask::UInt32
    fields::Vector{Field}
end

offset(r::Register) = r.offset

function Base.show(io::IO, m::MIME"text/plain", r::Register)
    indent = get(io, :indent, 0)
    inner = indent + 1
    println(io, " "^indent, "Register:")
    println(io, " "^inner, "name: ", r.name)
    println(io, " "^inner, "desc: ", r.description)
    println(io, " "^inner, "offset: ", r.offset)
    println(io, " "^inner, "size: ", r.size)
    println(io, " "^inner, "access: ", r.access)
    print(io, " "^inner, "resetValue: ")
    show(io, m, r.resetValue)
    println(io)
    print(io, " "^inner, "resetMask: ")
    show(io, m, r.resetMask)
    println(io)

    if get(io, :compact, false)
        println(io, " "^inner, length(r.fields), " fields")
        return
    end
    innerIO = IOContext(io, :indent => indent+2)
    for f in r.fields
        show(innerIO, m, f)
    end
    nothing
end

struct Block
    offset::UInt32
    size::UInt32
    usage::String
end

struct Peripheral
    name::String
    groupName::String
    description::String
    baseAddress::UInt32
    addressBlock::Block
    prependToName::String
    registers::Vector{Register}
end

name(p::Peripheral) = p.name

function Base.show(io::IO, m::MIME"text/plain", p::Peripheral)
    println(io, "Peripheral: ")
    println(io, " name: ", p.name)
    println(io, " desc: ", p.description)
    print(io, " addresses: ")
    show(io, m, p.baseAddress)
    print(io, "-")
    show(io, m, p.baseAddress + p.addressBlock.offset + p.addressBlock.size)
    println(io)
    println(io, " groupName: ", p.groupName)
    println(io, " prependToName: ", p.prependToName)

    get(io, :compact, false) && return
    innerIO = IOContext(io, :compact => true, :indent => 2)
    for r in p.registers
        show(innerIO, m, r)
    end
    nothing
end

function buildValue!(f::Field, raw)
    v = Value(
        raw["name"],
        raw["description"],
        raw["value"]
    )
    push!(f.enumeratedValues, v)
end

function buildField!(r::Register, raw)
    f = Field(
        raw["name"],
        raw["description"],
        UInt(parse(Int, raw["bitOffset"])),
        UInt(parse(Int, raw["bitWidth"])),
        toAccess(raw["access"]),
        Value[]
    )
    push!(r.fields, f)

    !haskey(raw, "enumeratedValues") && return
    obj = raw["enumeratedValues"]["enumeratedValue"]
    if obj isa XMLDict.XMLDictElement
        buildValue!(f, obj)
        return
    end
    for valraw in obj
        buildValue!(f, valraw)
    end
end

function buildRegister!(p::Peripheral, raw)
    r = Register(
        raw["name"],
        raw["description"],
        UInt(parse(Int, raw["addressOffset"])),
        UInt(parse(Int, raw["size"])),
        toAccess(raw["access"]),
        parse(UInt, raw["resetValue"]),
        parse(UInt, raw["resetMask"]),
        Field[]
    )
    !haskey(raw, "fields") && return
    obj = raw["fields"]["field"]
    if obj isa XMLDict.XMLDictElement
        # there is only one field
        buildField!(r, obj)
        push!(p.registers, r)
        return
    end
    
    for fieldraw in obj
        buildField!(r, fieldraw)
    end
    sort!(r.fields; by=offset)

    push!(p.registers, r)
end

function buildBlock(raw)
    Block(
        parse(UInt32, raw["offset"]),
        parse(UInt32, raw["size"]),
        raw["usage"]
    )
end

function buildPeripherals(xmldict)
    peris = Peripheral[]

    local prev
    for praw in xmldict["peripherals"]["peripheral"]
        p = Peripheral(
            praw["name"], 
            praw["groupName"], 
            praw["description"],
            parse(UInt, praw["baseAddress"]),
            buildBlock(praw["addressBlock"]),
            haskey(praw, "prependToName") ? praw["prependToName"] : "_",
            Register[]
        )
        if !haskey(praw, "registers")
            append!(p.registers, prev.registers)
            continue
        end
        prev = p
        obj = praw["registers"]["register"]
        if obj isa XMLDict.XMLDictElement
            # there is only one register
            buildRegister!(p, obj)
            push!(peris, p)
            continue
        end
        for regraw in obj
            buildRegister!(p, regraw)
        end

        sort!(p.registers; by=offset)
        push!(peris, p)
    end

    peris
end

function generateJuliaDefinitions(dir::String, v::Vector{Peripheral}) 
    isfile(dir) && throw(ArgumentError("Directory `$dir` already exists!"))
    mkpath(dir)
    path = joinpath(dir, "peripherals.jl")
    open(path, "w") do io
        generateJuliaDefinitions(io, v, dir)
    end
end

function generateJuliaDefinitions(io::IO, v::Vector{Peripheral}, basedir="")
    println(io, "module Peripherals\n")
    npath = joinpath(basedir, "peripherals")
    mkpath(npath)
    for p in v
        generatePeripheralDefinition(p, npath)
        println(io, "include(\"peripherals/$(lowercase(p.name)).jl\")")
    end
    println(io, "\nend # module")
end

function generatePeripheralDefinition(p::Peripheral, path="")
    npath = joinpath(path, lowercase(p.name)*".jl")
    open(npath, "w") do io
        print(io, """
        \"\"\"
            $(p.name)

        $(p.description)
        \"\"\"
        """)
        println(io, "module ", p.name, "\n")
        println(io, "const baseAddress = Ptr{UInt32}(", p.baseAddress, ")")
        println(io, "const addressBlockOffset = ", p.addressBlock.offset)
        println(io, "const addressBlockSize = ", p.addressBlock.size)
        println(io)
        header = """
        using MCUCommon: @regdef, Read, Write, ReadWrite, ReadWriteOnce
        using ..$(p.name): baseAddress
        """
        for r in p.registers
            generateRegisterDefinition(io, r, p.prependToName, header)
        end
        println(io, "end # peripheral\n")
    end
end

function generateRegisterDefinition(io::IO, r::Register, prefix::AbstractString, moduleHeader)
     # TODO: do proper expansion of registers with counting things
    '%' in r.name && @info "Found derived register" Register=r.name
    san_name = replace(r.name, '%' => '_', '[' => '_', ']' => '_')
    println(io, """
    \"\"\"
        $(r.name)

    $(r.description)
    \"\"\"
    module $(san_name)Mod
    """)
    println(io, moduleHeader)
    print(io, """
    const regAddress = baseAddress + $(UInt(r.offset))
    @regdef struct $prefix$san_name(regAddress)
    """)

    lastOffset = lastWidth = 0
    for f in r.fields
        if lastOffset + lastWidth < f.bitOffset
            println(io, "\t_:", f.bitOffset - (lastOffset+lastWidth))
        end
        println(io, "\t", f.name, ":", f.bitWidth, "::", f.access)
        lastOffset = f.bitOffset
        lastWidth = f.bitWidth
    end
    if lastOffset + lastWidth < r.size
        println(io, "\t", "_:", r.size - (lastOffset + lastWidth))
    end

    println(io, """
    end
    const Reg = $prefix$san_name
    """)
    for f in r.fields
        generateFieldDefinition(io, r, f)
    end
    println(io, "end # register $san_name\n")
end

function generateFieldDefinition(io::IO, r::Register, f::Field)
    println(io, """
    \"\"\"
        $(f.name)

    $(f.description)
    \"\"\"
    $(f.name)
    """)
end

function generateProject(name::String, svd_path, parent_dir::String=pwd())
    xml = XMLDict.parse_xml(read(svd_path, String))
    device = buildPeripherals(xml)    
    has_suffix = endswith(name, ".jl")
    projName = has_suffix ? name : name*".jl"
    moduleName = has_suffix ? name[begin:end-3] : name
    projdir = joinpath(parent_dir, projName)
    project_exists = isdir(projdir)
    mkpath(projdir)
    srcpath = joinpath(projdir, "src")
    mainModuleFile = joinpath(srcpath, projName)
    if project_exists
        @warn "Project directory already exists - make sure to bump its version after regenerating!"
        @info "Removing existing src directory"
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
    # generate the project dir first, so there's no need to precompile all of the definitions
    Pkg.activate(projdir)
    Pkg.pkg"add MCUCommon"
    Pkg.compat("MCUCommon", "0.1.3")
    open(joinpath(projdir, ".gitignore"), "w") do io
        println(io, "Manifest.toml")
    end
    # write out the definitions
    open(mainModuleFile, "w") do io
        print(io, """
        module $moduleName

        using MCUCommon: Register, Field

        include("peripherals.jl")
        
        end # module""")
    end
    generateJuliaDefinitions(srcpath, device)
    println("Done!")
end
