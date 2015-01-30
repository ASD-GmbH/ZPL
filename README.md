# ZPL for .net 

[ZPL (ZeroMQ Property Language)](http://rfc.zeromq.org/spec:4) is a light-weight configuration file format.
This library implements a simple parser and renderer for ZPL usable from any .net application or as a F# script.

## Status

This library is a draft. Build process, nupkg generation, documentation, Paket support are intended.

## Usage

See TPL_specs.cs for usage examples from C#.

ZPL.parse takes a string and returns a ZPL_Set, which is an array of ZPL sections.
ZPL.parse_relaxed additionally supports C and CPP style comments ( // and /* */ ), which are not part of the ZPL standard.



## License

This project is licensed under the GNU LESSER GENERAL PUBLIC LICENSE v3 (LGPLv3).
See the attached LICENSE.txt file for details.

