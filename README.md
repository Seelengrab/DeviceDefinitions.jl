# SVD2Julia

A code generator for creating Julia projects from SVD files.

This package is based on https://www.keil.com/pack/doc/CMSIS/SVD/html/index.html, but not a complete
implementation of the specification. In particular, registers or peripherals that are derived from
other registers or peripherals are not available.