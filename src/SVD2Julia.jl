module SVD2Julia

using MCUCommon: Access, Read, Write, ReadWrite, ReadWriteOnce, Unknown
using XML
using EnumX

import Pkg

include("structs.jl")
include("parsing.jl")
include("genDefinitions.jl")
include("project.jl")

end # module SVD2Julia
