using Logging

const descrCleanReplace = r"\\n\n\s+" => "\n\n"
const escapeUnderscore = r"_" => "\\_"
const indentListReplace = r"^-"m => "  -"

function toAccess(s::AbstractString)
    s == "read-only" && return Read
    s == "write-only" && return Write
    s == "read-write" && return ReadWrite
    s == "read-writeOnce" && return ReadWriteOnce
    @warn "Unknown Access mode string" Mode=s
    return Unknown
end

nonComments(xml) = filter(p -> XML.nodetype(p) != XML.Comment, children(xml))

function tryConstruct(::Type{T}, d) where T
    try
        T(;pairs(d)...)
    catch e
        @error "Something is amiss with this $T:" Data=d
        rethrow(e)
    end
end

function readSVD(path)
    dev = readDevice(read(path, Node))
    deriveObjects!(dev)
    expandDimensions!(dev)
    dev
end

function readDevice(xml)
    deviceXML = xml[end]
    d = Dict{Symbol,Any}()
    for c in nonComments(deviceXML)

        if tag(c) in ("vendor", "vendorID", "series", "licenseText", "headerSystemFilename", "headerDefinitionsPrefix")
            if XML.is_simple(c)
                d[Symbol(tag(c))] = Some(simplevalue(c))
            else
                for cc in nonComments(c)
                    if XML.is_simple(c)
                        d[Symbol(tag(c))] = Some(simplevalue(c))
                        break
                    end
                end
            end
        elseif tag(c) in ("name", "description", "version")
            v = replace(simplevalue(c), descrCleanReplace, escapeUnderscore)
            d[Symbol(tag(c))] = replace(v, indentListReplace)
        elseif tag(c) in ("width", "addressUnitBits")
            d[Symbol(tag(c))] = SNNI(simplevalue(c))
        elseif tag(c) == "cpu"
            d[Symbol(tag(c))] = readCPU(c)
        elseif tag(c) == "peripherals"
            d[Symbol(tag(c))] = map(readPeripheral, nonComments(c))
        elseif tag(c) in ("size", "access", "protection", "resetValue", "resetMask")
                rpg = get!(() -> RegisterPropertiesGroup(), d, :rpg)

                if tag(c) == "size"
                    rpg.size = Some(SNNI(simplevalue(c)))
                elseif tag(c) == "access"
                    rpg.access = Some(toAccess(simplevalue(c)))
                elseif tag(c) == "protection"
                    rpg.protection = Some(ProtectionString(simplevalue(c)))
                elseif tag(c) == "resetValue"
                    rpg.resetValue = Some(SNNI(simplevalue(c)))
                elseif tag(c) == "resetMask"
                    rpg.resetMask = Some(SNNI(simplevalue(c)))
                end

                d[:rpg] = rpg
        else
            @warn "Unknown tag:" Thing=:Device Tag=tag(c) Obj=d
        end
    end

    tryConstruct(Device, d)
end

function readCPU(xml)
    d = Dict{Symbol,Any}()
    for c in nonComments(xml)
        if tag(c) in ("fpuDP", "dspPresent", "icachePresent", "dcachePresent",
                      "itcmPresent", "dtcmPresent", "vtorPresent")
            d[Symbol(tag(c))] = Some(parse(Bool, simplevalue(c)))
        elseif tag(c) in ("mpuPresent", "fpuPresent", "vendorSystickConfig")
            d[Symbol(tag(c))] = parse(Bool, simplevalue(c))
        elseif tag(c) == "name"
            d[Symbol(tag(c))] = CPUName(simplevalue(c))
        elseif tag(c) == "revision"
            d[Symbol(tag(c))] = Revision(simplevalue(c))
        elseif tag(c) == "endian"
            d[Symbol(tag(c))] = Endian(simplevalue(c))
        elseif tag(c) == "nvicPrioBits"
            d[Symbol(tag(c))] = SNNI(simplevalue(c))
        elseif tag(c) in ("deviceNumInterrupts", "sauNumRegions")
            d[Symbol(tag(c))] = Some(SNNI(simplevalue(c)))
        else
            @warn "Unknown tag:" Thing=:CPU Tag=tag(c) Obj=d
        end
    end

    tryConstruct(CPU, d)
end

function readPeripheral(xml)
    d = Dict{Symbol,Any}()
    if !isnothing(attributes(xml))
        for (name,val) in pairs(attributes(xml))
            if name == "derivedFrom"
                d[Symbol(name)] = Some(DimableIdentifier(something(val)))
            else
                @warn "Unknown attribute:" Thing=:Register Attribute=name Obj=d
            end
        end
    end
    
    for c in nonComments(xml)
        if tag(c) == "name"
            d[Symbol(tag(c))] = DimableIdentifier(simplevalue(c))

        elseif tag(c) == "baseAddress"
            d[Symbol(tag(c))] = SNNI(simplevalue(c))
            
        elseif tag(c) in ("version", "description", "groupName")
            v = replace(simplevalue(c), descrCleanReplace, escapeUnderscore)
            v = replace(v, indentListReplace)
            d[Symbol(tag(c))] = Some(v)
            
        elseif tag(c) in ("size", "access", "protection", "resetValue", "resetMask")
            rpg = get!(() -> RegisterPropertiesGroup(), d, :rpg)

            if tag(c) == "size"
                rpg.size = Some(SNNI(simplevalue(c)))
            elseif tag(c) == "access"
                rpg.access = Some(toAccess(simplevalue(c)))
            elseif tag(c) == "protection"
                rpg.protection = Some(ProtectionString(simplevalue(c)))
            elseif tag(c) == "resetValue"
                rpg.resetValue = Some(SNNI(simplevalue(c)))
            elseif tag(c) == "resetMask"
                rpg.resetMask = Some(SNNI(simplevalue(c)))
            end

            d[:rpg] = rpg
            
        elseif tag(c) == "addressBlock"
            abs = get!(() -> Some(AddressBlock[]), d, :addressBlock)
            push!(something(abs), readAddressBlock(c))
            
        elseif tag(c) == "interrupt"
            abs = get!(() -> Some(Interrupt[]), d, :interrupt)
            push!(something(abs), readInterrupt(c))
            
        elseif tag(c) == "register"
            abs = get!(() -> Some(Register[]), d, :registers)
            push!(something(abs), readRegister(c))
        elseif tag(c) == "registers"
            rs = get!(() -> Some(Register[]), d, :registers)
            cs = get!(() -> Some(Cluster[]), d, :clusters)
            foreach(nonComments(c)) do cc
                if tag(cc) == "cluster"
                    push!(something(cs), readCluster(cc))
                elseif tag(cc) == "register"
                    push!(something(rs), readRegister(cc))
                end
            end
        elseif tag(c) == "cluster"
            abs = get!(() -> Some(Cluster[]), d, :clusters)
            push!(something(abs), readCluster(c))

        elseif tag(c) in ("prependToName", "appendToName")
            d[Symbol(tag(c))] = Some(Identifier(simplevalue(c)))

        elseif tag(c) in ("headerStructName", "alternatePeripheral")
            d[Symbol(tag(c))] = Some(DimableIdentifier(simplevalue(c)))
            
        else
            @warn "Unknown tag:" Thing=:Peripheral Tag=tag(c) Obj=d
        end
    end

    p = tryConstruct(Peripheral, d)
    !isnothing(p.registers) && sort!(something(p.registers); by=r -> r.addressOffset.value)
    !isnothing(p.clusters) && sort!(something(p.clusters); by=r -> r.addressOffset.value)
    return p
end

function readAddressBlock(xml)
    d = Dict{Symbol,Any}()
    for c in nonComments(xml)
        if tag(c) in ("offset", "size")
            d[Symbol(tag(c))] = SNNI(simplevalue(c))
        elseif tag(c) == "usage"
            d[Symbol(tag(c))] = Usage.T(simplevalue(c))
        elseif tag(c) == "protection"
            d[Symbol(tag(c))] = ProtectionString(simplevalue(c))
        elseif tag(c) == "address"
            d[Symbol(tag(c))] = ProtectionString(simplevalue(c))
        else
            @warn "Unknown tag:" Thing=:AddressBlock Tag=tag(c) Obj=d
        end
    end
    
    tryConstruct(AddressBlock, d)
end

function readInterrupt(xml)
    d = Dict{Symbol, Any}()

    for c in nonComments(xml)
        if tag(c) == "name"
            d[Symbol(tag(c))] = simplevalue(c)

        elseif tag(c) == "description"
            v = replace(simplevalue(c), descrCleanReplace, escapeUnderscore)
            v = replace(v, indentListReplace)
            d[Symbol(tag(c))] = Some(v)

        elseif tag(c) == "value"
            d[Symbol(tag(c))] = parse(Int, simplevalue(c))

        else
            @warn "Unknown tag:" Thing=:Interrupt Tag=tag(c) Obj=d
        end
    end

    tryConstruct(Interrupt, d)
end

function readCluster(xml)
    d = Dict{Symbol,Any}()

    for c in nonComments(xml)
        if tag(c) == "name"
            d[Symbol(tag(c))] = DimableIdentifier(simplevalue(c))

        elseif tag(c) == "addressOffset"
            d[Symbol(tag(c))] = SNNI(simplevalue(c))

        elseif tag(c) == "description"
            v = replace(simplevalue(c), descrCleanReplace, escapeUnderscore)
            v = replace(v, indentListReplace)
            d[Symbol(tag(c))] = Some(v)

        elseif tag(c) == "register"
            abs = get!(() -> Some(Register[]), d, :registers)
            push!(something(abs), readRegister(c))
        elseif tag(c) == "cluster"
            abs = get!(() -> Some(Cluster[]), d, :clusters)
            push!(something(abs), readCluster(c))

        elseif tag(c) in ("dim", "dimIncrement")
            d[Symbol(tag(c))] = SNNI(simplevalue(c))
        elseif tag(c) == "dimIndex"
            d[Symbol(tag(c))] = Some(DimIndex(simplevalue(c)))
        elseif tag(c) == "dimName"
            d[Symbol(tag(c))] = Some(Identifier(simplevalue(c)))
        elseif tag(c) == "dimArrayIndex"
            d[Symbol(tag(c))] = Some(DimArrayIndex(simplevalue(c)))

        elseif tag(c) in ("size", "access", "protection", "resetValue", "resetMask")
            rpg = get!(() -> RegisterPropertiesGroup(), d, :rpg)

            if tag(c) == "size"
                rpg.size = Some(SNNI(simplevalue(c)))
            elseif tag(c) == "access"
                rpg.access = Some(toAccess(simplevalue(c)))
            elseif tag(c) == "protection"
                rpg.protection = Some(ProtectionString(simplevalue(c)))
            elseif tag(c) == "resetValue"
                rpg.resetValue = Some(SNNI(simplevalue(c)))
            elseif tag(c) == "resetMask"
                rpg.resetMask = Some(SNNI(simplevalue(c)))
            end

            d[:rpg] = rpg

        else
            @warn "Unknown tag:" Thing=:Cluster Tag=tag(c) Obj=d
        end
    end

    degKeys = (:dim, :dimIncrement, :dimIndex, :dimName, :dimArrayIndex)
    if any(∈(keys(d)), degKeys)
        degd = Dict{Symbol,Any}()
        for k in degKeys
            degd[k] = get(d, k, nothing)
        end
        d[:deg] = tryConstruct(DimElementGroup, degd)
        for k in degKeys
            delete!(d, k)
        end
    end

    c = tryConstruct(Cluster, d)
    !isnothing(c.registers) && sort!(something(c.registers); by=r->r.addressOffset.value)
    !isnothing(c.clusters) && sort!(something(c.clusters); by=r->r.addressOffset.value)
    c
end

function readRegister(xml)
    d = Dict{Symbol,Any}(:deg => DimElementGroup(;dim=SNNI("0x0"), dimIncrement=SNNI("0x0")), :readAction => ReadAction.NotModified)
    if !isnothing(attributes(xml))
        for (name,val) in pairs(attributes(xml))
            if name in ("derivedFrom", "displayName", "description", "alternateGroup")
                v = replace(val, descrCleanReplace, escapeUnderscore)
                v = replace(v, indentListReplace)
                d[Symbol(name)] = Some(v)
            else
                @warn "Unknown attribute:" Thing=:Register Attribute=name Obj=d
            end
        end
    end

    for c in nonComments(xml)
        if tag(c) == "name"
            d[Symbol(tag(c))] = simplevalue(c)

        elseif tag(c) in ("derivedFrom", "displayName", "description", "alternateGroup")
            v = replace(simplevalue(c), descrCleanReplace, escapeUnderscore)
            v = replace(v, indentListReplace)
            d[Symbol(tag(c))] = Some(v)
            
        elseif tag(c) in ("size", "access", "protection", "resetValue", "resetMask")
            rpg = get!(() -> RegisterPropertiesGroup(), d, :rpg)

            if tag(c) == "size"
                rpg.size = Some(SNNI(simplevalue(c)))
            elseif tag(c) == "access"
                rpg.access = Some(toAccess(simplevalue(c)))
            elseif tag(c) == "protection"
                rpg.protection = Some(ProtectionString(simplevalue(c)))
            elseif tag(c) == "resetValue"
                rpg.resetValue = Some(SNNI(simplevalue(c)))
            elseif tag(c) == "resetMask"
                rpg.resetMask = Some(SNNI(simplevalue(c)))
            end

            d[:rpg] = rpg

        elseif tag(c) == "addressOffset"
            d[Symbol(tag(c))] = SNNI(simplevalue(c))
            
        elseif tag(c) == "fields"
            d[Symbol(tag(c))] = map(readField, nonComments(c))

        elseif tag(c) == "modifiedWriteValues"
            d[Symbol(tag(c))] = ModifiedWriteValue.T(simplevalue(c))

        elseif tag(c) in ("dim", "dimIncrement")
            d[Symbol(tag(c))] = SNNI(simplevalue(c))
        elseif tag(c) == "dimIndex"
            d[Symbol(tag(c))] = Some(DimIndex(simplevalue(c)))
        elseif tag(c) == "dimName"
            d[Symbol(tag(c))] = Some(Identifier(simplevalue(c)))
        elseif tag(c) == "dimArrayIndex"
            d[Symbol(tag(c))] = Some(DimArrayIndex(simplevalue(c)))

        else
            @warn "Unknown tag:" Thing=:Register Tag=tag(c) Obj=d
        end
    end

    degKeys = (:dim, :dimIncrement, :dimIndex, :dimName, :dimArrayIndex)
    if any(∈(keys(d)), degKeys)
        degd = Dict{Symbol,Any}()
        for k in degKeys
            degd[k] = get(d, k, nothing)
        end
        d[:deg] = tryConstruct(DimElementGroup, degd)
        for k in degKeys
            delete!(d, k)
        end
    end

    r = tryConstruct(Register, d)
    sort!(r.fields; by=f->first(f.bitRange))
    r
end

function readField(xml)
    d = Dict{Symbol,Any}(:deg => DimElementGroup(;dim=SNNI("0x0"), dimIncrement=SNNI("0x0")), :readAction => ReadAction.NotModified)

    for c in nonComments(xml)
        if tag(c) == "description"
            v = replace(simplevalue(c), descrCleanReplace, escapeUnderscore)
            v = replace(v, indentListReplace)
            d[Symbol(tag(c))] = Some(v)
        elseif tag(c) == "bitRange"
            d[Symbol(tag(c))] = BitRange(;bitRange=simplevalue(c))
        elseif tag(c) == "access"
            d[Symbol(tag(c))] = toAccess(simplevalue(c))
        elseif tag(c) == "name"
            d[Symbol(tag(c))] = DimableIdentifier(simplevalue(c))
        elseif tag(c) in ("dim", "dimIncrement", "dimIndex", "dimName", "dimArrayIndex")
            deg = d[:deg]

            if tag(c) == "dim"
                deg.name = SNNI(simplevalue(c))
            elseif tag(c) == "dimIncrement"
                deg.name = SNNI(simplevalue(c))
            elseif tag(c) == "dimIndex"
                deg.name = Some(DimIndex(simplevalue(c)))
            elseif tag(c) == "dimName"
                deg.name = Some(Identifier(simplevalue(c)))
            elseif tag(c) == "dimArrayIndex"
                deg.name = Some(DimArrayIndex(simplevalue(c)))
            else
                @warn "Unknown tag:" Thing=:DimElementGroup Tag=tag(c) Obj=d
            end

        elseif tag(c) == "modifiedWriteValues"
            d[Symbol(tag(c))] = ModifiedWriteValue.T(simplevalue(c))

        elseif tag(c) == "enumeratedValues"
            d[Symbol(tag(c))] = map(readEnumeratedValue, nonComments(c))
        
        elseif tag(c) in ("bitOffset", "bitWidth", "lsb", "msb")
            d[Symbol(tag(c))] = simplevalue(c)

        else
            @warn "Unknown tag:" Thing=:Field Tag=tag(c) Obj=d
        end
    end

    if !haskey(d, :bitRange)
        d[:bitRange] = tryConstruct(BitRange, d)
        delete!(d, :lsb)
        delete!(d, :msb)
        delete!(d, :bitWidth)
        delete!(d, :bitOffset)
    end

    tryConstruct(Field, d)
end

function readEnumeratedValue(xml)
    d = Dict{Symbol,Any}(:isDefault => false)

    for c in nonComments(xml)
        XML.nodetype(c) == XML.Comment && continue
        if tag(c) == "name"
            d[Symbol(tag(c))] = Identifier(simplevalue(c))
        elseif tag(c) == "description"
            v = replace(simplevalue(c), descrCleanReplace, escapeUnderscore)
            v = replace(v, indentListReplace)
            d[Symbol(tag(c))] = Some(v)
        elseif tag(c) == "value"
            d[Symbol(tag(c))] = SNNI(simplevalue(c))
        elseif tag(c) == "isDefault"
            d[Symbol(tag(c))] = parse(Bool, simplevalue(c))
        else
            @warn "Unknown tag:" Thing=:EnumeratedValue Tag=tag(c) Obj=d
        end
    end

    tryConstruct(EnumeratedValue, d)
end

function deriveObjects!(dev::Device)
    for p in dev.peripherals
        deriveObjects!(p, dev)
    end
    dev
end

function deriveObjects!(p::Peripheral, d::Device)
    if !isnothing(p.derivedFrom)
        pidx = findfirst(d.peripherals) do candidate
            candidate.name == something(p.derivedFrom)
        end
        isnothing(pidx) && throw(ArgumentError("Could not find parent this Peripheral is derived from! `$(p.name)` should derive from `$(p.derivedFrom)`"))
        parent = d.peripherals[pidx]
        for f in propertynames(p)
            nval = @something getproperty(p, f) getproperty(parent, f) Some(nothing)
            if f === :rpg
                nval = merge(getproperty(p, f), getproperty(parent, f))
            end
            setproperty!(p, f, nval)
        end

        p.derivedFrom = Some(parent.name)
        p.parent = Some(parent)
    end

    p.rpg = merge(p.rpg, d.rpg)

    # this should always be true at this point
    for r in something(p.registers)
        deriveObjects!(r, p)
    end
end

merge(::Nothing, rpg::RegisterPropertiesGroup) = copy(rpg)
merge(rpg::RegisterPropertiesGroup, ::Nothing) = copy(rpg)
function merge(a::RegisterPropertiesGroup, b::RegisterPropertiesGroup)
    size = @something a.size b.size Some(nothing)
    access = @something a.access b.access Some(nothing)
    protection = @something a.protection b.protection Some(nothing)
    resetValue = @something a.resetValue b.resetValue Some(nothing)
    resetMask = @something a.resetMask b.resetMask Some(nothing)
    ret = RegisterPropertiesGroup(size, access, protection, resetValue, resetMask)
    return ret
end

function deriveObjects!(r::Register, p::Union{Cluster,Peripheral})
    if !isnothing(r.derivedFrom)
        pidx = findfirst(p.registers) do candidate
            candidate.name == something(r.derivedFrom)
        end
        isnothing(pidx) && throw(ArgumentError("Could not find parent this Register is derived from! `$(r.name)` should derive from `$(r.derivedFrom)`"))
        parent = p.registers[pidx]
        for f in propertynames(r)
            nval = @something getproperty(r, f) getproperty(parent, f) Some(nothing)
            if f === :rpg
                nval = merge(getproperty(r, f), getproperty(parent, f))
            end
            setproperty!(r, f, nval)
        end

        r.derivedFrom = Some(parent.name)
        r.parent = Some(parent)
    end

    r.rpg = merge(r.rpg, p.rpg)

    # this should always be true at this point
    for f in something(r.fields)
        deriveObjects!(f, r)
    end
end

function deriveObjects!(c::Cluster, p::Union{Peripheral,Cluster})
    if !isnothing(c.derivedFrom)
        pidx = findfirst(p.registers) do candidate
            candidate.name == something(c.derivedFrom)
        end
        isnothing(pidx) && throw(ArgumentError("Could not find parent this Peripheral is derived from! `$(c.name)` should derive from `$(c.derivedFrom)`"))
        parent = p.registers[pidx]
        for f in propertynames(c)
            nval = @something getproperty(c, f) getproperty(parent, f) Some(nothing)
            if f === :rpg
                nval = merge(getproperty(c, f), getproperty(parent, f))
            end
            setproperty!(c, f, nval)
        end

        c.derivedFrom = Some(parent.name)
        c.parent = Some(parent)
    end

    c.rpg = merge(c.rpg, p.rpg)

    for r in @something c.registers Some(())
        deriveObjects!(r, c)
    end
    for cc in @something c.clusters Some(())
        deriveObjects!(cc, c)
    end
end

function deriveObjects!(f::Field, r::Register)
    if !isnothing(f.derivedFrom)
        pidx = findfirst(r.fields) do candidate
            candidate.name == something(f.derivedFrom)
        end
        isnothing(pidx) && throw(ArgumentError("Could not find parent this Peripheral is derived from! `$(f.name)` should derive from `$(f.derivedFrom)`"))
        parent = r.fields[pidx]
        for pn in propertynames(f)
            nval = @something getproperty(f, pn) getproperty(parent, pn) Some(nothing)
            setproperty!(f, pn, nval)
        end

        f.derivedFrom = Some(parent.name)
        f.parent = Some(parent)
    end
end

# expands the peripherals in this device
function expandDimensions!(d::Device)
    idx = firstindex(d.peripherals)
    while idx <= lastindex(d.peripherals)
        p = d.peripherals[idx]
        # first, expand the inner elements
        expandDimensions!(p)
        # then the peripheral itself, to not repeat work unnecessarily
        if !occursin("%s", p.name.value) # nothing to expand
            idx += 1
            continue
        end
        npers = map(p.deg) do repl
            np = copy(p)
            np.name = replace(np.name, r"%s|\[%s\]" => repl)
            np
        end
        splice!(d.peripherals, idx:idx, npers)
        idx += p.deg.dim.value
    end
end

# expands the registers and clusters in this peripheral/Cluster
function expandDimensions!(p::Union{Peripheral,Cluster})
    for maycol in (p.registers, p.clusters)
        col = @something maycol Some(())
        idx = firstindex(col)
        while idx <= lastindex(col)
            o = col[idx]
            expandDimensions!(o)
            if !occursin("%s", o.name isa String ? o.name : o.name.value) # nothing to expand
                idx += 1
                continue
            end
            nobjs = map(enumerate(o.deg)) do (num, repl)
                no = copy(o)
                no.name = if no.name isa String
                    replace(no.name, r"%s|\[%s\]" => repl)
                else
                    DimableIdentifier(replace(no.name.value, r"%s|\[%s\]" => repl))
                end
                nOffset = no.addressOffset.value + (num-1)*o.deg.dimIncrement.value
                SNNIString = "0x"*string(nOffset;base=16)*no.addressOffset.scale
                no.addressOffset = SNNI(SNNIString)
                no
            end
            splice!(col, idx:idx, nobjs)
            idx += o.deg.dim.value + 1
        end
    end
end

# expands the fields in this register
function expandDimensions!(r::Register)
    idx = firstindex(r.fields)
    while idx <= lastindex(r.fields)
        f = r.fields[idx]
        if !occursin("%s", f.name.value) # nothing to expand
            idx += 1
            continue
        end
        nfields = map(enumerate(r.deg)) do (num, repl)
            nf = copy(f)
            nf.name = replace(nf.name, r"%s|\[%s\]" => repl)
            lsb = first(nf.bitRange) + (num-1)*f.deg.dimIncrement.value
            msb = lsb + length(nf.bitRange)
            nf.bitRange = BitRange(;lsb=string(lsb), msb=string(msb))
            nf
        end
        splice!(r.fields, idx:idx, nfields)
        idx += f.deg.dim.value + 1
    end
end
