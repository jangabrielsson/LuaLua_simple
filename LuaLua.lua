--[[
%% properties
%% autostart
--]]

--[[
     LuaLua
     Copyright (c) 2019 Jan Gabrielsson
     Email: jan@gabrielsson.com
     MIT License
--]]

if not _EMULATED and dofile then dofile("lib/json.lua") end -- sorry, you need a json lib from somewhere
function table.maxn(t) local n= 0; for _,_ in ipairs(t) do n=n+1 end return n end

--[[
chunk ::= block
block ::= {stat} [retstat]
stat ::=  ‘;’ | 
varlist ‘=’ explist | 
functioncall | 
label |          -- NOT IMPLEMENTED
break | 
goto Name |      -- NOT IMPLEMENTED
do block end | 
while exp do block end | 
repeat block until exp | 
if exp then block {elseif exp then block} [else block] end | 
for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end | 
for namelist in explist do block end | 
function funcname funcbody | 
local function Name funcbody | 
local namelist [‘=’ explist] 
retstat ::= return [explist] [‘;’]
label ::= ‘::’ Name ‘::’
funcname ::= Name {‘.’ Name} [‘:’ Name]
varlist ::= var {‘,’ var}
var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 
namelist ::= Name {‘,’ Name}
explist ::= exp {‘,’ exp}
exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef | 
  prefixexp | tableconstructor | exp binop exp | unop exp 
prefixexp ::= var | functioncall | ‘(’ exp ‘)’
functioncall ::=  prefixexp args | prefixexp ‘:’ Name args 
args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString 
functiondef ::= function funcbody
funcbody ::= ‘(’ [parlist] ‘)’ block end
parlist ::= namelist [‘,’ ‘...’] | ‘...’
tableconstructor ::= ‘{’ [fieldlist] ‘}’ 
fieldlist ::= field {fieldsep field} [fieldsep]
field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
fieldsep ::= ‘,’ | ‘;’
binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ | 
  ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ | 
  ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | 
  and | or
unop ::= ‘-’ | not | ‘#’ | ‘~’
--]]

local function makeParser()
  local format = string.format
  local function assert(t,str) if not t then error(str,3) end end
  local function mkError(...) error(format(...),3) end

  local function mkStream(tab)
    local p,self=0,{ stream=tab, eof={type='eof', value='', from=tab[#tab].from, to=tab[#tab].to} }
    function self.next() p=p+1 return p<=#tab and tab[p] or self.eof end
    function self.last() return tab[p] or self.eof end
    function self.peek(n) return tab[p+(n or 1)] or self.eof end
    function self.match(t) local v=self.next(); assert(v.type==t,"Expected:"..t); return v.value end
    function self.matchA(t) local v=self.next(); assert(v.type==t,"Expected:"..t); return v end
    function self.test(t) local v=self.peek(); if v.type==t then self.next(); return true else return false end end
    function self.back(t) p=p-1 end
    return self
  end
  local function mkStack()
    local p,st,self=0,{},{}
    function self.push(v) p=p+1 st[p]=v end
    function self.pop(n) n = n or 1; p=p-n; return st[p+n] end
    function self.popn(n,v) v = v or {}; if n > 0 then local p = self.pop(); self.popn(n-1,v); v[#v+1]=p end return v end 
    function self.peek(n) return st[p-(n or 0)] end
    function self.isEmpty() return p<=0 end
    function self.size() return p end    
    function self.dump() for i=1,p do print(json.encode(st[i])) end end
    function self.clear() p,st=0,{} end
    return self
  end 

  local patterns,source,cursor,tokens = {}
  local ptabs = {}

  local function token(idp,pattern, createFn, logFn)
    pattern = "^"..pattern
    local f = function ()
      local res = {string.find(source, pattern)}
      if res[1] then
        if createFn then
          local token = createFn(select(3,table.unpack(res)))
          token.from, token.to = cursor, cursor+res[2]
          table.insert(tokens, token)
        end
        if logFn then logFn(res[3]) end
        source = string.sub(source, res[2]+1)
        cursor = cursor + res[2]
        return true
      end
    end
    for i=1,#idp do 
      local b = idp:byte(i)
      local v = ptabs[b] or {}
      ptabs[b]=v; v[#v+1]=f
    end
  end

  local specT={['(']=true,[')']=true,['{']=true,['}']=true,['[']=true,[']']=true,[',']=true,['.']=true,['=']=true,[';']=true,[':']=true,}
  local opT={['and']=true,['or']=true,['not']=true,}
  local reservedT={
    ['if']=true,['else']=true,['then']=true,['elseif']=true,['while']=true,['repeat']=true,['local']=true,['for']=true,['in']=true,
    ['do']=true,['until']=true,['end']=true,['return']=true,['true']=true,['false']=true,['function']=true,['nil']=true,['break']=true,
  }

  local function TABEsc(str) return str:gsub("\\x(%x%x)",function(s) return string.char(tonumber(s,16)) end) end
  token(" \t\n\r","([%s%c]+)")
  token("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_","([A-Za-z_][%w_]*)", function (w) 
      if reservedT[w] then return {type=w, value=w}
      elseif opT[w] then return {type='operator', value = w}
      else return {type='Name', value=w} end
    end)
  token(".","(%.%.%.?)",function(op) return {type=op=='..' and 'operator' or op, value=op} end)
  token("0123456789","(%d+%.%d+)", function (d) return {type="number", value=tonumber(d)} end)
  token("0123456789","(%d+)", function (d) return {type="number", value=tonumber(d)} end)
  token("\"",'"([^"]*)"', 
    function (s) return {type="string", value=TABEsc(s)} 
    end)
  token( "\'" , "'([^']*)'", function (s) return { type="string", value=TABEsc(s) } end )
  token("-","%-%-%[%[(.-)%-%-%]%]")
  token("[","%[%[(.-)%]%]", function (s) return {type="string", value=s} end)
  token("-","(%-%-.-\n)")
  token("-","(%-%-.*)")
  token("@$=<>!+.-*&|/%^~;:","([@%$=<>!+%.%-*&|/%^~;:][=<>&|:]?)", function (op) return {type= specT[op] and op or "operator", value=op} end)
  token("[]{}(),#%","([{}%(%),%[%]#%%])", function (op) return {type=specT[op] and op or "operator", value=op} end)

  local function dispatch() 
    local v = ptabs[source:byte(1)]
    if v then
      for i=1,#v do if v[i]() then return true end end
    end
  end

  local ESCTab = {['\"'] = "22", ['\''] = "27", ['t'] = "09", ['n'] = "0A", ['r'] = "0D", }
  local function tokenize(src)
--    local t1 = os.clock()
    src = src:gsub('\\([\"t\'nr])',function(c) return "\\x"..ESCTab[c] end)
    source, tokens, cursor = src, {}, 0
    while #source>0 and dispatch() do end
    if #source > 0 then print("Parser failed at " .. source) end
--      print("Time:"..os.clock()-t1)
    return tokens
  end

  local function copyt(t) local res={}; for _,v in ipairs(t) do res[v]=true end return res end -- shallow copying

  local function addVarsToCtx(varList,ctx)
    ctx.l = ctx.l or {}
    for _,v in ipairs(varList) do if v[1]=='var' then ctx.l[v[2]]=true end end
  end

  local gram = {}

  local opers = {
    ['not']={12,1},['#']={12,1},['%neg']={12,1},['%nor']={12,1},['^']={13,2},
    ['+']={8,2}, ['-']={8,2}, ['*']={11,2}, ['/']={11,2}, ['//']={11,2},['%']={11,2},
    ['..']={7,2},
    ['|']={3,2},['~']={4,2},['&']={5,2},['<<']={6,2},['>>']={6,2},
    ['<']={2,2}, ['<=']={2,2}, ['>']={2,2}, ['>=']={2,2}, ['==']={2,2}, ['~=']={2,2},
    ['and']={1,2}, ['or']={0,2}
  }

  local function apply(t,st) return st.push(st.popn(opers[t.value][2],{t.value})) end
  local _samePrio = {['.']=true,[':']=true}
  local function lessp(t1,t2) 
    local v1,v2 = t1.value,t2.value
    if v1==':' and v2=='.' then return true 
    elseif v1=='=' then v1='/' end
    return v1==v2 and _samePrio[v1] or opers[v1][1] < opers[v2][1] 
  end

  local function makeVar(name,ctx)
    while ctx do if ctx.l and ctx.l[name] then return {'var',name} else ctx=ctx.n end end
    return {'glob', name}
  end
  local NIL = "%%N".."IL%%"
  local PREFIXTKNS = {['.']=true, ['(']=true, ['[']=true, [':']=true, ['{']=true}
  
  local pExpr = {
    ['false']=function(inp,st,ops,t,pt,ctx) inp.next(); st.push(false) end,
    ['true']=function(inp,st,ops,t,pt,ctx) inp.next(); st.push(true) end,
--    ['nil']=function(inp,st,ops,t,pt,ctx) inp.next(); st.push(nil) end,
    ['nil']=function(inp,st,ops,t,pt,ctx) inp.next(); st.push(NIL) end,
    ['number']=function(inp,st,ops,t,pt,ctx) inp.next(); st.push(t.value) end,
    ['string']=function(inp,st,ops,t,pt,ctx) inp.next(); st.push(t.value) end,
    ['function']=function(inp,st,ops,t,pt,ctx) 
      inp.next(); 
      local args,varargs,body = gram.funcBody(inp,ctx)
      st.push({'function','expr','fun',"<non>",args,varargs,body})
    end,
    ['{']=function(inp,st,ops,t,pt,ctx) st.push(gram.tableConstructor(inp,ctx)) t.type='}' end,
    ['...']=function(inp,st,ops,t,pt,ctx) inp.next(); st.push({'vararg'}) end,
    ['(']=function(inp,st,ops,t,pt,ctx) 
      inp.next(); 
      local expr = gram.expr(inp,ctx) 
      inp.match(')')
      if PREFIXTKNS[inp.peek().type] then st.push(gram.prefixExp(inp,expr,ctx)) else st.push(expr) end
    end,
    ['Name']=function(inp,st,ops,t,pt,ctx) 
      inp.next();
      local p,v = inp.peek(),makeVar(t.value,ctx)
      if PREFIXTKNS[p.type] then st.push(gram.prefixExp(inp,v,ctx)) else st.push(v) end
    end,
    ['operator']=function(inp,st,ops,t,pt,ctx) 
      if opers[t.value] then
        if t.value == '-' and not(pt.type == 'Name' or pt.type == 'number' or pt.type == '(') then t.value='%neg' end
        while ops.peek() and lessp(t,ops.peek()) do apply(ops.pop(),st) end
        ops.push(t)
        inp.next()
      else mkError("Bad operator '%s'",t.value) end
    end
  }

  local eStop = {['end']=true,[',']=true,[';']=true,['=']=true,[')']=true,[']']=true,['}']=true,}
  for k,v in pairs(reservedT) do eStop[k]=v end
  eStop['true']=nil; eStop['false']=nil; eStop['nil']=nil; eStop['function']=nil
  function gram.expr(inp,ctx)
    local st,ops,t,pt=mkStack(),mkStack(),{type='<START>',value='<START>'}
    while true do
      t,pt = inp.peek(),t
      if t.type=='eof' or eStop and eStop[t.type] then break end
      if not (t.type=='{' and pt.type=='<START>') then
        if pt.type=='{' or t.type~='operator' and pt.type~='<START>' and pt.type~='operator' then break end
      end
      pExpr[t.type](inp,st,ops,t,pt,ctx)
    end
    while not ops.isEmpty() do apply(ops.pop(),st) end
    --st.dump()
    return st.pop()
  end

--[[ My rewrite of the rules...
afterpref ::= '.' prefixexp
afterpref ::= '[' exp ']' afterpref
afterpref ::= '(' args ')' afterpref
afterpref ::= null

prefixexp ::= Name
prefixexp ::= Name . prefixexp
prefixexp ::= Name [ exp ] [ afterpref ]
prefixexp ::= Name ( args ) [ afterpref ]
prefixexp ::= Name : Obj ( args ) [ afterpref ]
prefixexp ::= ( exp ) [ afterpref ]
--]]

--[[
    X = . Name X
    X = [Expr] X
    X = : Name (args) X
    X = (args) X
    (Expr) X
    Var X
--]]

  function gram.prefixExp(inp,r,ctx)  
    if inp.test('.') then
      local n = inp.match('Name')
      return gram.prefixExp(inp,{'aref',r,n},ctx)
    elseif inp.test('[') then     
      local e = gram.expr(inp,ctx)
      inp.match(']')
      return gram.prefixExp(inp,{'aref',r,e},ctx)
    elseif inp.peek().type == '(' or inp.peek().type == '{' then
      local args = gram.args(inp,ctx)
      return gram.prefixExp(inp,{'call',r,args},ctx) 
    elseif inp.test(':') then
      local key = inp.match('Name')
      local args= gram.args(inp,ctx)
      local ep = {'callobj',r,key,args}
      return gram.prefixExp(inp,ep,ctx) 
    else return r end
  end

  function gram.args(inp,ctx)
    local n = inp.next()
    if n.type == '(' then
      local r = gram.exprList(inp,ctx)
      inp.match(')')
      return r
    elseif n.type == '{' then
      inp.back(n)
      return {gram.tableConstructor(inp,ctx)}
    else error("Bad function argument list") end
  end

  function gram.nameList(inp)
    local res={inp.match('Name')}
    while inp.test(',') do res[#res+1]=inp.match('Name') end
    return res
  end

--varlist ::= var {‘,’ var}
--var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 

  function gram.varList(inp,ctx,loc)
    local res = {}
    local p = inp.peek()
    while true do
      local n = inp.matchA('Name')
      local k = inp.peek()
      if PREFIXTKNS[k.type] then
        res[#res+1] = gram.prefixExp(inp,makeVar(n.value,ctx),ctx)
      else res[#res+1]=loc and {'var',n.value} or makeVar(n.value,ctx) end
      if inp.peek().type ~= ',' then break end
      inp.next()
      p=inp.peek()
    end
    return res
  end

  function gram.stat(inp,ctx)
    local n = inp.next()
    local t = n.type
    if t == ';' then return gram.stat(inp,ctx)
    elseif t == 'break' then return {'break'}
    elseif t == '::' then local n = inp.match('Name'); inp.match('::'); return {'label',n}
    elseif t == 'goto' then return {'goto',inp.match('Name')}
    elseif t == 'do' then local b = gram.block(inp,{l={},n=ctx}); inp.match('end'); return b
    elseif t == 'while' then
      local e = gram.expr(inp,ctx)
      inp.match('do')
      local b = gram.block(inp,{l={},n=ctx})
      inp.match('end')
      return {'while',e,b}
    elseif t == 'repeat' then 
      local b = gram.block(inp,{l={},n=ctx});
      inp.match('until') 
      return {'repeat',gram.expr(inp,ctx),b}
    elseif t == 'for' then
--for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end | 
--for namelist in explist do block end | 
      local l = gram.nameList(inp)
      t = inp.next()
      if t.type == '=' then
        local e1,e2,e3,b = gram.expr(inp,ctx),nil,1
        inp.match(',')
        e2 = gram.expr(inp,ctx)
        if inp.test(',') then e3 = gram.expr(inp,ctx) end
        inp.match('do'); b=gram.block(inp,{l=copyt(l),n=ctx}); inp.match('end')
        assert(#l==1,"wrong number of loop variables")
        return {'foridx',l[1],e1,e2,e3,b}
      elseif t.type == 'in' then
        local el,b = gram.exprList(inp,ctx)
        inp.match('do'); b=gram.block(inp,{l=copyt(l),n=ctx}); inp.match('end')
        return {'forlist',l[1],l[2] or '_',el,b}
      else error() end
    elseif t == 'if' then
      local e = gram.expr(inp,ctx)
      inp.match('then')
      local b = gram.block(inp,{l={},n=ctx})
      local eif,els={}
      while inp.test('elseif') do
        local e = gram.expr(inp,ctx)
        inp.match('then')
        local b = gram.block(inp,{l={},n=ctx})
        eif[#eif+1]={e,b}
      end
      if inp.test('else') then 
        els = gram.block(inp,{l={},n=ctx})
      end
      inp.match('end')
      return {'if',e,b,eif,els}
    elseif t == 'Name' then
      inp.back(n)
      local vars=gram.varList(inp,ctx)
      if inp.test('=') then
        local exprs = gram.exprList(inp,ctx)
        return {'assign',vars,exprs}
      else
        assert(#vars==1,"Bad expression1")
        vars = vars[1]
        assert(vars[1]=='call' or vars[1]=='callobj',"Bad expression2")
        return {'nop',vars}
      end
    elseif t == 'function' then
      local name,ft = gram.funcName(inp,ctx)
      local args,varargs,body = gram.funcBody(inp,ctx)
      return {'nop',{'function','glob',ft,name,args,varargs,body}}
    elseif t == 'local' then
      if inp.test('function') then
--      local name,ft = gram.funcName(inp)
        local name,ft = makeVar(inp.match('Name'),ctx),'fun'
        local args,varargs,body = gram.funcBody(inp,ctx)
        addVarsToCtx(name,ctx)
        return {'nop',{'function','loc',ft,name,args,varargs,body}}
      else 
        local vars,exprs = gram.varList(inp,ctx,true),{}
        addVarsToCtx(vars,ctx)
        if inp.test('=') then
          exprs = gram.exprList(inp,ctx)
        end
        return {'local',vars,exprs}
      end
    end
    inp.back(n)
  end

  function gram.exprList(inp,ctx)
    local res,i = {gram.expr(inp,ctx)},2
    while inp.test(',') do
      res[i]=gram.expr(inp,ctx) i=i+1
    end
    return res
  end

  local bends = {['end']=true,['elseif']=true,['else']=true,['until']=true,['return']=true,['eof']=true,}

  function gram.block(inp,ctx)
    local s = {gram.stat(inp,ctx)}
    if #s>0 then
      while not bends[inp.peek().type] do 
        local t=gram.stat(inp,ctx) 
        if t== nil then break else s[#s+1]=t end
      end
    end
    if inp.test('return') then
      local re = gram.exprList(inp,ctx)
      s[#s+1] = {'return'..(#re < 2 and #re or 'n'),re}
    end
    inp.test(';') -- optional
    return {'block',s}
  end

  function gram.field(inp,ctx) 
    if inp.test('[') then
      local e1 = gram.expr(inp,ctx)
      inp.match(']')
      inp.match('=')
      return {gram.expr(inp,ctx),e1}
    elseif inp.peek().type == 'Name' and inp.peek(2).type == '=' then
      local n = inp.next()
      inp.match('=')
      return {gram.expr(inp,ctx),n.value}
    else
      return {gram.expr(inp,ctx)}
    end
  end

  function gram.tableConstructor(inp,ctx)
    inp.match('{') 
    --if inp.test('}') then return {'quote',{}} end
    local res = {gram.field(inp,ctx)}
    while inp.peek().type == ',' or inp.peek().type == ';' do
      inp.next()
      if inp.peek().type == '}' then break end
      res[#res+1]=gram.field(inp,ctx)
    end
    inp.match('}')
    return {'table',res}
  end
--[[
functiondef ::= function funcbody
funcbody ::= ‘(’ [parlist] ‘)’ block end
parlist ::= namelist [‘,’ ‘...’] | ‘...’
--]]

  function gram.funcBody(inp,ctx)
    inp.match('(')
    local p = inp.peek()
    local varargs,args=false,{}
    if p.type == '...' then
      varargs=true
      inp.next()
    elseif p.type == 'Name' then
      args={inp.match('Name')}
      while inp.test(',') do 
        p=inp.peek()
        if inp.test('...') then  varargs=true break end
        args[#args+1]=inp.match('Name') 
      end
    end 
    inp.match(')')
    local b = gram.block(inp,{l=copyt(args),n=ctx})
    inp.match('end')
    return args,varargs,b
  end

--funcname ::= Name {‘.’ Name} [‘:’ Name]

  function gram.funcName(inp,ctx)
    local t = makeVar(inp.match('Name'),ctx)
    while true do
      local p = inp.peek()
      if inp.test('.') then
        local n = inp.match('Name')
        t = {'aref',t,n}
      elseif inp.test(':') then
        local n = inp.match('Name')
        return {'aref',t,n},'obj'
      end
      break
    end
    return t,'fun'
  end

  return function(str)
    local tkns = mkStream(tokenize(str))
    return gram.block(tkns,{l={}})
  end
end

function makeCompiler()
  local format,compile,NIL = string.format,nil,"%%N".."IL%%"
  local function assert(t,str) if not t then error(str,3) end end
  local function mkError(...) error(format(...),3) end
  local function lookupVar(var,env) 
    while env do 
      local l = env.locals if l[var] then return l[var] else env=env.next end 
    end 
  end
  local function setVar(var,val,env) local l = lookupVar(var,env) if l then l[1]=val else _ENV[var]=val end end
  local function mkAssignment(ctx,lh,loc)
    if lh[1]=='var' then 
      local var = lh[2] return 
      function(val,env) if loc then env.locals[var]={} end setVar(var,val,env) end
    elseif lh[1]=='glob' then 
      local var = lh[2] return 
      function(val,env) _ENV[var]=val end
    elseif lh[1]=='aref' then 
      local e,key = compile(ctx,lh[2]),compile(ctx,lh[3])
      return function(val,env) e(env)[key(env)]=val end
    else mkError("Bad assignment '%s'",json.encode(lh)) end
  end
  local function evalList(list,env) 
    if #list==0 then return {}
    elseif #list==1 then return {list[1](env)}
    else
      local res,lm1 = {},#list-1
      for i=1,lm1 do res[i]=list[i](env) end
      local vs = {list[#list](env)}
      for i=1,table.maxn(vs) do res[lm1+i]=vs[i] end
      return res
    end
  end
  local function breakBlock(fun,...) -- return value instead like 'return' 
    local stat,res=pcall(fun,...)
    if not stat and (type(res)~='table' or res[1]~='%break%') then error(res) 
    elseif stat then return res end
  end

  local compF = {
    ['block'] = function(ctx,_,stmts2)
      local stmts = {}
      for _,e in ipairs(stmts2 or {}) do stmts[#stmts+1]=compile(ctx,e) end
      return function(env,locals)
        local a = stmts2 -- for debugging...
        local nenv,r = {locals=locals or {},next=env}
        for i=1,#stmts do r = stmts[i](nenv) if r then return r end end
      end
    end,
    ['local'] = function(ctx,_,vars,exprs2)
      local exprs = {}
      for _,e in ipairs(exprs2 or {}) do exprs[#exprs+1]=compile(ctx,e) end
      return function(env)
        local ls,vals = env.locals,evalList(exprs,env)
        for i=1,#vars do ls[vars[i][2]]={vals[i]} end
      end
    end,
    ['break'] = function(ctx,_) return function(env) error({'%break%'}) end end,
    ['while'] = function(ctx,_,test,body)
      test,body = compile(ctx,test),compile(ctx,body)
      return function(env)
        return breakBlock(function() 
            while test(env) do 
              local r = body(env) if r then return r end 
            end 
          end)
      end
    end,
    ['repeat'] = function(ctx,_,test,body)
      test,body = compile(ctx,test),compile(ctx,body)
      return function(env)
        return breakBlock(function() repeat local r = body(env) if r then return r end until test(env) end)
      end
    end,
    ['call'] = function(ctx,_,fun1,vals)
      local args = {}
      local fun = compile(ctx,fun1)
      for i=1,table.maxn(vals) do args[#args+1]=compile(ctx,vals[i]) end
      return function(env)
        local kk = fun1
        local vals = evalList(args,env)
        local ff = fun(env)
        return fun(env)(table.unpack(vals))
      end
    end,
    ['callobj'] = function(ctx,_,obj,key,vals)
      local args = {}
      obj = compile(ctx,obj)
      for i=1,table.maxn(vals) do args[#args+1]=compile(ctx,vals[i]) end
      return function(env)
        local vals = evalList(args,env)
        local o = obj(env); 
        return o[key](o,table.unpack(vals))
      end
    end,
    ['if'] = function(ctx,_,test,body,eifs,els) --{'if',e,b,eif,els}
      test,body,els=compile(ctx,test),body and compile(ctx,body),els and compile(ctx,els)
      if #eifs==0 then return function(env) if test(env) then return body(env) elseif els then return els(env) end end end
      local eifs2={}
      for _,c in ipairs(eifs or {}) do  eifs2[#eifs2+1]={compile(ctx,c[1]),compile(ctx,c[2]) } end
      return function(env)
        if test(env) then return body(env) 
        else
          for _,c in ipairs(eifs2) do if c[1](env) then return c[2](env) end end
          if els then return els(env) end
        end
      end
    end, 
    ['+']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) + b(env) end end,
    ['-']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) - b(env) end end,
    ['*']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) * b(env) end end,
    ['/']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) / b(env) end end,
    ['%']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) % b(env) end end,
    ['^']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) ^ b(env) end end,
    ['==']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) == b(env) end end,
    ['~=']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) ~= b(env) end end,
    ['>']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) > b(env) end end,
    ['>=']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) >= b(env) end end,
    ['<']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) < b(env) end end,
    ['<=']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) <= b(env) end end,
    ['and']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) and b(env) end end,
    ['or']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env) or b(env) end end,
    ['not']=function(ctx,_,a) a=compile(ctx,a) return function(env) return not a(env) end end,
    ['#']=function(ctx,_,a) a=compile(ctx,a) return function(env) return #a(env) end end,
    ['%neg']=function(ctx,_,a) a=compile(ctx,a) return function(env) return -a(env) end end,
    ['..']=function(ctx,_,a,b) a,b=compile(ctx,a),compile(ctx,b) return function(env) return a(env)..b(env) end end,
    ['assign']=function(ctx,_,lhs,args) 
      local lhsc,argsc={},{}
      for _,c in ipairs(args) do argsc[#argsc+1]=compile(ctx,c) end
      for _,l in ipairs(lhs) do lhsc[#lhsc+1]=mkAssignment(ctx,l) end
      return function(env)
        local values = evalList(argsc,env)
        for i=1,#lhsc do lhsc[i](values[i],env) end
      end
    end,
    ['var']=function(ctx,_,var) 
      return function(env)
        local l = lookupVar(var,env)
        if l then return l[1] else return _ENV[var] end
      end
    end,
    ['glob']=function(ctx,_,var) 
      return function(env) return _ENV[var] end
    end,
    ['nop']=function(ctx,_,expr) expr = compile(ctx,expr) return function(env) expr(env) end end,
    ['quote']=function(ctx,_,val) return function(env) return val end end,
    ['table']=function(ctx,_,tab)
      if #tab==0 then return function(env) return {} end end
      local keyvals={}
      for _,c in ipairs(tab) do keyvals[#keyvals+1]={compile(ctx,c[1]),c[2] and compile(ctx,c[2])} end
      return function(env)
        local res,kn={},#keyvals
        for i=1,kn-1 do local c = keyvals[i]; if c[2] then res[c[2](env)]=c[1](env) else res[#res+1]=c[1](env) end end
        kn = keyvals[kn]
        if kn[2] then res[kn[2](env)]=kn[1](env)
        else
          local vs = {kn[1](env)}
          for _,v in ipairs(vs) do res[#res+1]=v end
        end
        return res
      end
    end,    
    ['vararg']=function(ctx,_)
      return function(env) 
        local l=env.locals['...']   ---lookupVar('...',env); 
        if l then return table.unpack(l[1]) else return nil end
      end
    end,
    ['aref']=function(ctx,_,expr,ind)
      expr,ind=compile(ctx,expr),compile(ctx,ind)
      return function(env) return expr(env)[ind(env)] end
    end,
    ['return0']=function(ctx,_) return function(env) return {} end end,  -- Optimization      
    ['return1']=function(ctx,_,expr) expr=compile(ctx,expr[1]) return function(env) return {expr(env)} end end,  
    ['returnn']=function(ctx,_,exprs1)
      local exprs={}
      for _,c in ipairs(exprs1) do exprs[#exprs+1]=compile(ctx,c) end
      return function(env) return evalList(exprs,env) end
    end,            
    ['function']=function(ctx,_,loc,ft,name,params,varargs,body)
      body = compile(ctx,body) local n2 = name
      name = name ~= '<non>' and mkAssignment(ctx,name,loc=='loc')
      return function(env)
        local f = function(...)
          local args,nlocals,n = {...},{},0
          if ft=='obj' then nlocals['self'] = args[1]; n=1 end
          for i=1,#params do nlocals[params[i]]={args[i+n]} end
          if varargs then
            local va,j = {},1
            for i=#params+1,#args-n do va[j]=args[i+n] j=j+1 end
            nlocals['...']={va}
          end
          local r = body(env,nlocals)
          return table.unpack(r or {})
        end
        if name then name(f,env) end -- set local or global
        return f
      end
    end,
    ['foridx']=function(ctx,_,var,e1,e2,e3,b) --         
      e1,e2,e3,b=compile(ctx,e1),compile(ctx,e2),compile(ctx,e3),compile(ctx,b)
      local f = function(env)
        local start,stop,step=e1(env),e2(env),e3(env)
        local locals = {[var]={}}
        for i=start,stop,step do 
          locals[var][1]=i
          local r = b(env,locals) if r then return r end 
        end
      end
      return function(env) return breakBlock(f,env) end
    end,
    ['forlist']=function(ctx,_,v1,v2,el,b) --         
      el,b=compile(ctx,el[1]),compile(ctx,b)
      local f = function(env) 
        local idx = {[v1]={},[v2]={}}
        for k,v in el(env) do idx[v1][1],idx[v2][1]=k,v 
          local r = b(env,idx) if r then return r end 
        end 
      end
      return function(env) return breakBlock(f,env) end
    end,
  }

  compile = function(ctx,expr)
    ctx = ctx or {}
    if type(expr) == 'table' then
      if compF[expr[1]] then return compF[expr[1]](ctx,table.unpack(expr)) end
      error("Bad expr:"..json.encode(expr))
    elseif expr==NIL then 
      return function(env) return nil end
    else return function(env) return expr end end
  end

  return compile
end

-- Define loadstring and loadfile
do
  local parser = makeParser()
  local compiler = makeCompiler()

  function loadstring2(str)
    local cr = parser(str)
    --print(json.encode(cr))
    local ppp = json.encode(cr)
    local fun = compiler(nil,cr)
    return function() return table.unpack(fun(nil,{}) or {}) end,cr
  end

  function loadfile2(file)
    local f = io.open(file)
    if not f then error("No such file:"..file) end
    local src = f:read("*all")
    f:close()
    return loadstring2(src)
  end

  function updateParser()
    local fo = io.open("luaParser_v1.json","w")
    local fi = io.open("parser2.lua")
    local src = fi:read("*all")
    src = parser(src)
    fo:write(json.encode(src))
    fi:close()
    fo:close()
  end

  function loadstringP(cr)
    local fun = compiler(nil,cr)
    return function() return table.unpack(fun(nil,{}) or {}) end,cr
  end
end

--updateParser()

str = "function fib1(x) if x==0 then return 0 elseif x==1 then return 1 else return fib1(x-1)+fib1(x-2) end end"
loadstring2(str)()
function fib2(x) if x==0 then return 0 elseif x==1 then return 1 else return fib2(x-1)+fib2(x-2) end end

local n,m = 20,1
t0 = os.clock()
for i=1,m do fib1(n) end
t1 = (os.clock()-t0)/m

t2 = os.clock()
for i=1,m do fib2(n) end
t3 = (os.clock()-t2)/m

print("Fibonacci (native):"..t3.."s")
print("Fibonacci (emulated):"..t1.."s")
print("Factor slower:"..t1/t3)

--  loadfile2("EventRunner3.lua")
--  loadfile2("LuaLua.lua")

local strs = {
  "a = 6; return a+a",{12},
  "return (function(x) return x+x end)(7)",{14},
  "return {['a']=6+8,['b']=2+3,}",{{a=14,b=5}},
  "return (function (a,b) return a+b end)(7,8)",{7+8},
  "return (function (a,...) return a+({...})[1] end)(3,4)",{7},
  "return 3+4*8",{3+4*8},
  "return (3+4)*8",{(3+4)*8},
  "return -3*8",{-3*8},
  "return 3+-9",{3+-9},
  "b=42 return b",{42},
  "return {1,2,3}",{{1,2,3}},
  "a={b=9}; a['c']=8 return a.c",{8},
  "return ({42})[1]",{({42})[1]},
  "local a,b=9,17; return a+b",{9+17},
  "local d = { e = e, f = function(a) return a+2 end}; return d.f(7)",{9},
  "a={b = {c = {42}}}; x=a.b.c[1]; return x,x",{42,42},
  "a = {} a.foo = 10 return a.foo",{10},
  "a = {} function a.foo(x) return x+x end return a.foo(5)",{10},
--  "fibaro:call(44,'turnOn')",{},
  "foo ={} function foo:test(x) return x*2 end return foo:test(10)",{20},
  "function foo(...) return {...} end return foo(1,2,3)",{{1,2,3}},
  "function foo(a,...) return {...} end return foo(1,2,3)",{{2,3}},
  "bar={} function bar:foo(...) return {...} end return bar:foo(1,2,3)",{{1,2,3}},
  "bar={} function bar:foo(a,...) return {...} end return bar:foo(1,2,3)",{{2,3}},
  "for i=1,3 do end",{},
  "local s = 0 for k,v in pairs({a=4,b=3,c=2}) do s=s+v end return s",{9},
  "local i,s=1,0; while true do s=s+i i=i+1; if i>4 then break end end return s",{10},
  "local i,s=2,0; repeat i=i+1; s=s+i if i>4 then break end until false return s",{12},
  "function test(a) return function(b) return a+b end end return test(8)(9)",{17},
  "return select(3,table.unpack({2,3,4,5}))",{{4,5}},
  "local function foo(x) return x+x end return foo(6)",{12},
  "local a=0 if a==1 then a=2 elseif a==3 then a=7 elseif a==6 then a=7 end return a",{0},
}

function test(str,res)
  local fun,code = loadstring2(str)
  --print(json.encode(code))
  local s = {fun()}
  if json.encode(s)==json.encode(res) then 
    print("OK - "..json.encode(s[1]))
  else 
    print(json.encode(code))
    print("ERROR "..json.encode(s)) 
  end
end

for i=1,#strs,2 do test(strs[i],strs[i+1]) end
