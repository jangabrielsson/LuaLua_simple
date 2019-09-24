# LuaLua
Lua implemented in vanilla Lua.
Compiles Lua src to a nested Lua function. Used in sandboxed environment where loadstring is not allowed.
~30x slower than "native" Lua.
Divided up in a parser and a compiler. The parser outputs a parse tree and the compiler creates a nested Lua function of the parse tree. The parser can be parsed (and compiled) by the parser to reduce the code foot print.
 
