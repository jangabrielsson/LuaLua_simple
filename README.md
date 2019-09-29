# LuaLua
Lua implemented in vanilla Lua.
Compiles Lua src to a nested Lua function. Used in sandboxed environment where loadstring is not allowed.
~10-30x slower than "native" Lua. Goto and label not omplemented.
Divided up in a parser and a compiler. The parser outputs a parse tree and the compiler creates a nested Lua function of the parse tree. The parser can be parsed by the parser and the parse-tree then compiled by the compiler thus reducing the code foot print.
 
