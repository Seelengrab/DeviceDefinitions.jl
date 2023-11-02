const Option{T} = Union{Some{T},Nothing}
Base.convert(::Type{Option{T}}, x) where T = Some(x)

const diRegex1 = r"^((?:%s)|(?:%s)[_A-Za-z]{1}[_A-Za-z0-9]*)$"
const diRegex2 = r"^([_A-Za-z]{1}[_A-Za-z0-9]*(?:\[%s\])?)$"
const diRegex3 = r"^([_A-Za-z]{1}[_A-Za-z0-9]*(?:%s)?[_A-Za-z0-9]*)$"
struct DimableIdentifier
    value::String
    function DimableIdentifier(s::AbstractString)
        m = @something match(diRegex1, s) match(diRegex2, s) match(diRegex3, s) Some(nothing)
        isnothing(m) && throw(ArgumentError("Not a dimable identifier: `$s`"))
        return new(only(m))
    end
end
Base.show(io::IO, di::DimableIdentifier) = print(io, di.value)
Base.:(==)(di::DimableIdentifier, s::String) = di.value == s
Base.:(==)(s::String, di::DimableIdentifier) = di.value == s

struct ProtectionString
    value::String
    function ProtectionString(s::AbstractString)
        s in ("s", "n", "p") || throw(ArgumentError("Not a valid protection string: `$s`"))
        return new(s)
    end
end

const numRegex = r"^([0-9]+)\-([0-9]+)$"
const letterRegex = r"^([A-Z])-([A-Z])$"
const listRegex = r"^[_0-9a-zA-Z]+(?:,\s*[_0-9a-zA-Z]+)+$"
const singleListRegex = r"^[_0-9a-zA-Z]+$"
struct DimIndex
    indices::AbstractVector
    function DimIndex(s::AbstractString)
        numRange = match(numRegex, s)
        if !isnothing(numRange)
            lowStr, highStr = numRange
            low = parse(Int, lowStr)
            high = parse(Int, highStr)
            return new(low:high)
        end

        letterRange = match(letterRegex, s)
        if !isnothing(letterRange)
            lowStr, highStr = numRange
            low = only(lowStr)
            high = only(highStr)
            return new(low:high)
        end

        listRange = match(listRegex, s)
        if !isnothing(listRange)
            vals = map(only, eachmatch(singleListRegex, s))
            return new(vals)
        end

        throw(ArgumentError("Given string doesn't match any of the known patterns: `$s`"))
    end
end

const snniRegex = r"^[+]?(0x|0X|#)?([0-9a-fA-Fx]+)([kmgtKMGT]?)$"
struct ScaledNonNegativeInteger
    value::UInt
    scale::String
    function ScaledNonNegativeInteger(s::AbstractString)
        m = match(snniRegex, s)
        isnothing(m) && throw(ArgumentError("Invalid ScaledNonNegativeInteger string: `$s`"))
        if m[1] == "#"
            base = 2
            num  = replace(m[2], "x" => "0")
        elseif m[1] === nothing
            base = 10
            num  = m[2]
        else
            base = 16
            num = m[2]
        end
        new(parse(UInt, num; base), m[3])
    end
end
const SNNI = ScaledNonNegativeInteger
Base.show(io::IO, snni::SNNI) = show(io, snni.value)

const itRegex = r"^([_A-Za-z0-9]*)$"
struct Identifier
    ident::String
    function Identifier(s::AbstractString)
        m = match(itRegex, s)
        isnothing(m) && throw(ArgumentError("Given string is not an identifier: `$s`"))
        return new(only(m))
    end
end
Base.string(id::Identifier) = id.ident

@kwdef mutable struct EnumeratedValue
    name::Identifier
    description::Option{String} = nothing
    value::SNNI
    isDefault::Bool
end

mutable struct DimArrayIndex
    headerEnumName::String
    values::Vector{EnumeratedValue}
end

###
# Special Elements
###

@kwdef mutable struct DimElementGroup
    dim::SNNI
    dimIncrement::SNNI
    dimIndex::Option{DimIndex} = nothing
    dimName::Option{Identifier} = nothing
    dimArrayIndex::Option{DimArrayIndex} = nothing
end
Base.length(deg::DimElementGroup) = deg.dim.value
function Base.iterate(deg::DimElementGroup, state=0)
    state >= deg.dim.value && return nothing
    if !isnothing(deg.dimArrayIndex)
        deg.dimArrayIndex[state+1], state+1
    else
        state, state+1
    end
end

@kwdef mutable struct RegisterPropertiesGroup
    size::Option{ScaledNonNegativeInteger} = nothing
    access::Option{Access} = nothing
    protection::Option{ProtectionString} = nothing
    resetValue::Option{ScaledNonNegativeInteger} = nothing
    resetMask::Option{ScaledNonNegativeInteger} = nothing
end

###
# CPU
###

struct CPUName
    value::String
    function CPUName(s::AbstractString)
        s ∉ cpunames && throw(ArgumentError("Unknown CPU name: `$s`"))
        new(s)
    end
end
const cpunames = ("CM0", "CM0PLUS", "CM0+", "CM1", "CM3", "CM4", "CM7", "CM23", "CM33", "CM35P", "CM55", "CM85", "SC000", "SC300", "ARMV8MML", "ARMV8MBL", "ARMV81MML", "CA5", "CA7", "CA8", "CA9", "CA15", "CA17", "CA53", "CA57", "CA72", "SMC1", "other")

struct Revision
    r::Int
    p::Int
    function Revision(s::AbstractString)
        m = match(r"^r([0-9]*)p([0-9]*)$", s)
        isnothing(m) && throw(ArgumentError("Invalid revision: `$s`"))
        return new(parse(Int, m[1]), parse(Int, m[2]))
    end
end

struct Endian
    value::String
    function Endian(s::AbstractString)
        s in endians || throw(ArgumentError("Invalid endianness: `$s`"))
        new(s)
    end
end
const endians = ("little", "big", "selectable", "other")

mutable struct SauRegionsConfig

end

@kwdef mutable struct CPU
    name::CPUName
    revision::Revision
    endian::Endian
    mpuPresent::Bool
    fpuPresent::Bool
    fpuDP::Option{Bool} = nothing
    dspPresent::Option{Bool} = nothing
    icachePrsent::Option{Bool} = nothing
    dcachePresent::Option{Bool} = nothing
    itcmPresent::Option{Bool} = nothing
    dtcmPresent::Option{Bool} = nothing
    vtorPresent::Option{Bool} = nothing
    nvicPrioBits::SNNI
    vendorSystickConfig::Bool
    deviceNumInterrupts::Option{ScaledNonNegativeInteger} = nothing
    sauNumRegions::Option{ScaledNonNegativeInteger} = nothing
    sauRegionsConfig::Option{SauRegionsConfig} = nothing
end

###
# Core structs
###

@enumx ModifiedWriteValue OneToClear OneToSet OneToToggle ZeroToClear ZeroToSet ZeroToToggle Clear Set Modify
function ModifiedWriteValue.T(s::AbstractString)
    if s == "oneToClear"
        ModifiedWriteValue.OneToClear
    elseif s == "oneToSet"
        ModifiedWriteValue.OneToSet
    elseif s == "oneToToggle"
        ModifiedWriteValue.OneToToggle
    elseif s == "zeroToClear"
        ModifiedWriteValue.ZeroToClear
    elseif s == "zeroToSet"
        ModifiedWriteValue.ZeroToSet
    elseif s == "zeroToToggle"
        ModifiedWriteValue.ZeroToToggle
    elseif s == "clear"
        ModifiedWriteValue.Clear
    elseif s == "set"
        ModifiedWriteValue.Set
    elseif s == "modify"
        ModifiedWriteValue.Modify
    else
        @warn "Unknown ModifiedWriteValue:" Value = s
    end
end

@enumx ReadAction Clear Set Modify ModifyExternal NotModified
function ReadAction.T(s::AbstractString)
    if s == "clear"
        ReadAction.Clear
    elseif s == "set"
        ReadAction.Set
    elseif s == "modify"
        ReadAction.Modify
    elseif s == "modifyExternal"
        ReadAction.ModifyExternal
    elseif s == "notModified"
        ReadAction.NotModified
    else
        @warn "Unknown ReadAction:" Value = s
    end
end

@kwdef mutable struct WriteConstraint
    writeAsRead::Option{Bool} = nothing
    useEnumeratedValues::Option{Bool} = nothing
    range::Option{Tuple{ScaledNonNegativeInteger,ScaledNonNegativeInteger}} = nothing
end

struct BitRange
    extent::UnitRange{Int}
    function BitRange(; kwargs...)
        k = keys(kwargs)
        if :bitOffset in k && :bitWidth in k
            start = parse(Int, kwargs[:bitOffset])
            stop = start + parse(Int, kwargs[:bitWidth]) - 1
            return new(start:stop)
        elseif :lsb in k && :msb in k
            lsb = parse(Int, kwargs[:lsb])
            msb = parse(Int, kwargs[:msb])
            return new(lsb:msb)
        elseif :bitRange in k
            m = match(r"^\[(?<msb>\d+):(?<lsb>\d+)\]$", kwargs[:bitRange])
            lsb = parse(Int, m[:lsb])
            msb = parse(Int, m[:msb])
            return new(lsb:msb)
        end

        throw(ArgumentError("Invalid Bitrange keys given!"))
    end
end
Base.first(br::BitRange) = first(br.extent)
Base.last(br::BitRange) = last(br.extent)
Base.length(br::BitRange) = length(br.extent)
Base.iterate(br::BitRange) = iterate(br.extent)
Base.iterate(br::BitRange, state) = iterate(br.extent, state)

@kwdef mutable struct Field
    derivedFrom::Option{String} = nothing
    parent::Option{Field} = nothing
    deg::DimElementGroup
    name::DimableIdentifier
    description::Option{String} = nothing
    bitRange::BitRange
    access::Option{Access} = nothing
    modifiedWriteValues::ModifiedWriteValue.T = ModifiedWriteValue.Modify
    writeConstraint::Option{WriteConstraint} = nothing
    readAction::ReadAction.T
    enumeratedValues::Vector{EnumeratedValue} = EnumeratedValue[]
end
Base.copy(p::Field) = Field(;(n => getfield(p, n) for n in fieldnames(Field))...)

function Base.show(io::IO, ::MIME"text/plain", f::Field)
    base_indent = get(io, :indent, "")
    indent = base_indent * "  "
    println(io, base_indent, "Field: ",                     f.name.value                   )
    println(io, indent, "description: ",         @something f.description     Some(nothing))
    println(io, indent, "derivedFrom: ",         @something f.derivedFrom     Some(nothing))
    println(io, indent, "bitRange: ",            @something f.bitRange        Some(nothing))
    println(io, indent, "access: ",              @something f.access          Some(nothing))
    println(io, indent, "modifiedWriteValues: ",            f.modifiedWriteValues          )
    println(io, indent, "writeConstraint: ",     @something f.writeConstraint Some(nothing))
    println(io, indent, "readAction: ",                     f.readAction                   )
end

@kwdef mutable struct Register
    derivedFrom::Option{String} = nothing
    parent::Option{Register} = nothing
    deg::DimElementGroup
    name::String
    displayName::Option{String} = nothing
    description::Option{String} = nothing
    alternateGroup::Option{String} = nothing
    alternateRegister::Option{Identifier} = nothing
    addressOffset::SNNI
    rpg::RegisterPropertiesGroup
    dataType::Option{DataType} = nothing
    modifiedWriteValues::ModifiedWriteValue.T = ModifiedWriteValue.Modify
    writeConstraint::Option{WriteConstraint} = nothing
    readAction::ReadAction.T
    fields::Vector{Field} = Field[]
end
Base.copy(p::Register) = Register(;(n => getfield(p, n) for n in fieldnames(Register))...)

function Base.show(io::IO, m::MIME"text/plain", r::Register)
    base_indent = get(io, :indent, "")
    indent = base_indent * "  "
    println(io, base_indent, "Register: ", r.name)
    println(io, indent, "description: ", @something r.description Some(nothing))
    println(io, indent, "derivedFrom: ", @something r.derivedFrom Some(nothing))
    println(io, indent, "alternateGroup: ", @something r.alternateGroup Some(nothing))
    println(io, indent, "alternateRegister: ", @something r.alternateRegister Some(nothing))
    println(io, indent, "addressOffset: ", r.addressOffset)
    println(io, indent, "modifiedWriteValues: ", r.modifiedWriteValues)
    println(io, indent, "readAction: ", r.readAction)
    get(io, :compact, false) && return
    for f in r.fields
        show(IOContext(io, :indent => indent), m, f)
    end
end

@kwdef mutable struct Cluster
    derivedFrom::Option{String} = nothing
    parent::Option{Cluster} = nothing
    deg::DimElementGroup
    name::DimableIdentifier
    description::Option{String} = nothing
    alternateCluster::Option{DimableIdentifier} = nothing
    headerStructName::Option{Identifier} = nothing
    addressOffset::SNNI
    rpg::RegisterPropertiesGroup = RegisterPropertiesGroup()
    registers::Option{Vector{Register}} = nothing
    clusters::Option{Vector{Cluster}} = nothing
end
Base.copy(p::Cluster) = Cluster(;(n => getfield(p, n) for n in fieldnames(Cluster))...)

@enumx Usage Registers Buffer Reserved
function Usage.T(s::String)
    if s == "registers"
        Usage.Registers
    elseif s == "buffer"
        Usage.Buffer
    elseif s == "reserved"
        Usage.Reserved
    else
        @error "Unknown usage:" Usage = s
    end
end

@kwdef mutable struct AddressBlock
    offset::SNNI
    size::SNNI
    usage::Usage.T
    protection::Option{ProtectionString} = nothing
end

@kwdef mutable struct Interrupt
    name::String
    description::Option{String} = nothing
    value::Int
end

@kwdef mutable struct Peripheral
    derivedFrom::Option{DimableIdentifier} = nothing
    parent::Option{Peripheral} = nothing
    deg::DimElementGroup = DimElementGroup(; dim=SNNI("0x0"), dimIncrement=SNNI("0x0"))
    name::DimableIdentifier
    version::Option{String} = nothing
    description::Option{String} = nothing
    alternatePeripheral::Option{DimableIdentifier} = nothing
    groupName::Option{String} = nothing
    prependToName::Option{Identifier} = nothing
    appendToName::Option{Identifier} = nothing
    headerStructName::Option{DimableIdentifier} = nothing
    disableCondition::Option{String} = nothing
    baseAddress::SNNI
    rpg::RegisterPropertiesGroup = RegisterPropertiesGroup()
    addressBlock::Option{Vector{AddressBlock}} = nothing
    interrupt::Option{Vector{Interrupt}} = nothing
    registers::Option{Vector{Register}} = nothing
    clusters::Option{Vector{Cluster}} = nothing
end

function Base.show(io::IO, m::MIME"text/plain", p::Peripheral)
    base_indent = get(io, :indent, "")
    indent = base_indent * "  "
    println(io, base_indent, "Peripheral: ", p.name)
    println(io, indent, "description: ", @something p.description Some(nothing))
    println(io, indent, "derivedFrom: ", @something p.derivedFrom Some(nothing))
    println(io, indent, "alternatePeripheral: ", @something p.alternatePeripheral Some(nothing))
    println(io, indent, "baseAddress: ", p.baseAddress)
    regs = @something p.registers Some(())
    for r in regs
        show(IOContext(io, :indent => indent, :compact => true), m, r)
    end
end
Base.copy(p::Peripheral) = Peripheral(;(n => getfield(p, n) for n in fieldnames(Peripheral))...)

@kwdef mutable struct Device
    vendor::Option{String} = nothing
    vendorID::Option{String} = nothing
    name::String
    series::Option{String} = nothing
    version::String
    description::String
    licenseText::Option{String} = nothing
    cpu::CPU
    headerSystemFilename::Option{String} = nothing
    headerDefinitionsPrefix::Option{String} = nothing
    addressUnitBits::SNNI
    width::SNNI
    rpg::RegisterPropertiesGroup = RegisterPropertiesGroup()
    peripherals::Vector{Peripheral} = Peripheral[]
    vendorExtensions::Vector{Any} = []
end
