--[[============================================================
--=
--=  Art handling
--=
--=-------------------------------------------------------------
--=
--=  Art Command
--=  by Marcus 'ReFreezed' Thunström
--=
--==============================================================

	loadArtFile

--============================================================]]

local MAX_LOOPS = 10000



local COMMANDS = {
	-- command = { {argName1,defaultValue}, ... }, -- defaultValue=nil means the value can be any type.

	-- Language.
	["set" ] = { {"var",""}, {"value",nil} }, -- Set variable.
	["setx"] = { {"var",""}, {"value",nil} }, -- Set existing variable.

	["do" ] = { },
	["for"] = { {"var","I"}, {"from",1},{"to",0},{"step",1} },
	["end"] = { }, -- Dummy, for error message.

	-- Special commands: "func", user defined functions.

	-- Settings, app.
	["backdrop"] = { {"r",0},{"g",0},{"b",0},{"a",1} }, -- (Not part of the image.)
	["zoom"    ] = { {"zoom",1} },

	-- Settings, init.
	["canvas"] = { {"w",0--[[=auto]]},{"h",0--[[=auto]]}, {"aa",1} },

	-- Settings, dynamic.
	["round"] = { {"round",true} },

	-- State.
	["push"] = { },
	["pop" ] = { },

	["color"] = { {"r",1},{"g",1},{"b",1},{"a",1} },
	["grad" ] = { {"r",1},{"g",1},{"b",1},{"a",1}, {"rr",1},{"gg",1},{"bb",1},{"aa",1}, {"angle",0}, {"scale",1} },
	["font" ] = { {"size",12} },

	["makemask"] = { {"clear",true} },
	["mask"    ] = { {"mask",true} },

	["origin"] = { },
	["move"  ] = { {"x",0},{"y",0} },
	["rotate"] = { {"a",0} },
	["scale" ] = { {"x",1},{"y",1/0--[[=x]]} },

	-- Drawing.
	["fill"] = { {"r",0},{"g",0},{"b",0},{"a",1} }, -- A rectangle that covers the whole screen.

	["rect"  ] = { {"mode","fill"}, {"x",0},{"y",0}, {"w",10},{"h",10}, {"ax",0},{"ay",0}, {"thick",1} },
	["circle"] = { {"mode","fill"}, {"x",0},{"y",0}, {"r",5}, {"segs",0--[[=auto]]},       {"thick",1} },
	["text"  ] = { {"x",0},{"y",0}, {"text",""}, {"ax",0},{"ay",0}, {"wrap",1/0},{"align","left"} },
}



local function Art()return{
	canvas   = nil,
	backdrop = {0,0,0,0},
	zoom     = 1.0,
}end

local function Context()return{
	art = nil,

	path   = "",
	source = "",
	lines  = {},

	scopeStack = {--[[ scopeStackEntry1, ... ]]},
	callStack  = {--[[ [funcInfo1]=true, ... ]]},

	canvasToMask = nil,
	maskCanvas   = nil,

	-- Settings.
	canvasW = 1,
	canvasH = 1,
	msaa    = 1,
	round   = false,

	-- State (in addition to love.graphics).  @Incomplete: Push/pop this stuff too.
	colorImage = nil,
}end

local function FunctionInfo()return{
	token     = nil,
	arguments = {},
	tokens    = {},
}end

local function ScopeStackEntry()return{
	functions = {--[[ [funcName1]=funcInfo, ... ]]},
	variables = {--[[ [varName1 ]=value   , ... ]]},
}end



local function parseError(context, pos, s, ...)
	printFileErrorAt(context.path, context.source, pos, s, ...)
end
local function parseWarning(context, pos, s, ...)
	printFileWarningAt(context.path, context.source, pos, s, ...)
end

-- tokenError  ( context, token=atEnd, format, ... )
-- tokenWarning( context, token=atEnd, format, ... )
local function tokenError(context, tok, s, ...)
	local pos = (tok and tok.position) or context.source:find"%S%s*$" or #context.source
	printFileErrorAt(context.path, context.source, pos, s, ...)
end
local function tokenWarning(context, tok, s, ...)
	local pos = (tok and tok.position) or context.source:find"%S%s*$" or #context.source
	printFileWarningAt(context.path, context.source, pos, s, ...)
end



-- value|nil, stackIndex = findInStack( context, member, key )
local function findInStack(context, member, k)
	for i = #context.scopeStack, 1, -1 do
		local v = context.scopeStack[i][member][k]
		if v ~= nil then  return v, i  end
	end
	return nil -- Not found!
end



local function ensureCanvas(context)
	if context.art.canvas then  return  end

	local settingsCanvas = {
		format = "rgba16",
		msaa   = (context.msaa > 1 and context.msaa or nil),
	}
	local settingsMask = {
		format = "r16",
		msaa   = settingsCanvas.msaa,
	}

	context.art.canvas   = LG.newCanvas(context.canvasW,context.canvasH, settingsCanvas)
	context.canvasToMask = LG.newCanvas(context.canvasW,context.canvasH, settingsCanvas)
	context.maskCanvas   = LG.newCanvas(context.canvasW,context.canvasH, settingsMask)

	context.maskCanvas:setFilter("nearest") -- This fixes weird fuzziness when applying mask.
	-- context.art.canvas:setFilter("nearest") -- Maybe there should be an app setting for this. @Incomplete

	shaderSend(shaderMain     , "maskMode"       , false)
	shaderSend(shaderMain     , "useColorTexture", false)
	shaderSend(shaderApplyMask, "mask"           , context.maskCanvas)

	LG.setCanvas(context.art.canvas)
	LG.setShader(shaderMain)
end



local function maybeRound(context, x)
	return context.round and round(x) or x
end



local function maybeApplyMask(context)
	if LG.getCanvas() ~= context.canvasToMask then  return  end

	LG.push("all")
	LG.reset()
	LG.setCanvas(context.art.canvas)
	LG.setBlendMode("alpha", "premultiplied")
	LG.setShader(shaderApplyMask)
	LG.draw(context.canvasToMask)
	LG.pop()

	--[[ DEBUG
	LG.push("all")
	LG.reset()
	LG.setBlendMode("replace")
	LG.draw(context.canvasToMask)
	-- LG.draw(context.maskCanvas)
	LG.present()
	LG.sleep(1)
	LG.pop()
	--]]
end



local function tokenize(context, s, path)
	local pos     = 1
	local ln      = 1
	local tokens  = {}
	local lastTok = nil
	local gotName = false

	while true do
		pos = s:match("^[^%S\n]*()", pos)
		if pos > #s then  break  end

		local startPos    = pos
		local lastWasName = gotName
		gotName           = false

		-- Line break.
		if s:find("^\n", pos) then
			pos = pos + 1
			ln  = ln  + 1

			if lastTok and lastTok.type ~= "linebreak" then
				table.insert(tokens, {position=startPos, type="linebreak"})
			end

		-- Name: name OR +name OR -name
		elseif s:find("^[-+]?%l", pos) then
			local mod,namePos,name; mod, namePos, name, pos = s:match("^([-+]?)()(%l+)()", pos)
			table.insert(tokens, {position=namePos, type="name", value=name, hasAttachment=false})

			if mod == "" then
				gotName = true

			else
				tokens[#tokens].hasAttachment = true
				table.insert(tokens, {position=startPos, type="literal", value=(mod=="+")})

				if s:find("^[^%s;#]", pos) then
					parseWarning(context, pos, "Value right after '%s%s' does not belong to it.", mod, name)
				end
			end

		-- Username: Name OR -Name
		elseif s:find("^%-?%u", pos) then
			local negate,namePos,name; negate, namePos, name, pos = s:match("^(%-?)()(%u[%w]*)()", pos)
			table.insert(tokens, {position=namePos, type="username", value=name, negated=(negate=="-")})

			if lastWasName and lastTok.type == "name" then  lastTok.hasAttachment = true  end

		-- Number: N
		elseif s:find("^%-?%.?%d", pos) then
			local nStr; nStr, pos = s:match("^(%-?%.?%d[-+%w%%]*)()", pos)
			local n

			if     nStr:find"%%$"    then  n = tonumber(nStr:sub(1, -2)) ; n = n and n/100
			elseif nStr:find"deg$"   then  n = tonumber(nStr:sub(1, -4)) ; n = n and math.rad(n)
			elseif nStr:find"turns$" then  n = tonumber(nStr:sub(1, -6)) ; n = n and n*TAU
			else                           n = tonumber(nStr           )  end
			if not n then  return (parseError(context, startPos, "Failed parsing number '%s'.", nStr))  end

			table.insert(tokens, {position=startPos, type="literal", value=n})

			if lastWasName and lastTok.type == "name" then  lastTok.hasAttachment = true  end

		-- String: "S" OR "S OR 'S' OR 'S
		elseif s:find("^[\"']", pos) then
			local q      = s:sub(pos, pos)
			local buffer = {}
			local pat    = "["..s:sub(pos, pos).."\\\n]"

			while true do
				local i1 = pos + 1
				pos      = s:find(pat, i1) or #s+1

				if pos > i1 then
					table.insert(buffer, s:sub(i1, pos-1))
				end

				if s:find("^\\", pos) then
					pos = pos + 1
					if     s:find("^\\", pos) then  table.insert(buffer, "\\")
					elseif s:find("^n" , pos) then  table.insert(buffer, "\n")
					elseif s:find('^"' , pos) then  table.insert(buffer, '"')
					elseif s:find("^'" , pos) then  table.insert(buffer, "'")
					else
						return (parseError(context, pos-1, "Invalid escape sequence in string."))
					end
				elseif s:find("^\n", pos) then
					ln = ln + 1 -- '\n'
					break
				else
					break
				end
			end

			table.insert(tokens, {position=startPos, type="literal", value=table.concat(buffer)})

			if not s:find("^\n", pos) then
				pos = pos + 1 -- '"' OR "'"
			end

			if lastWasName and lastTok.type == "name" then  lastTok.hasAttachment = true  end

		-- Parenthesis: (lua_expression)
		elseif s:find("^%(", pos) then
			local i1    = pos
			local depth = 1

			while true do
				-- Simplified Lua parsing, o'hoy!
				pos          = s:find("[-\"'%[\n()]", pos+1)
				local luaPos = pos

				if not pos or s:find("^\n", pos) then
					return (parseError(context, startPos, "Missing end parens."))

				elseif s:find("^%(", pos) then
					depth = depth + 1

				elseif s:find("^%)", pos) then
					depth = depth - 1
					if depth == 0 then  break  end

				elseif s:find("^[\"']", pos) then -- Quoted string.
					local pat = "["..s:sub(pos, pos).."\\\n]"
					while true do
						pos = s:find(pat, pos+1)
						if not pos or s:find("^\n", pos) then
							return (parseError(context, luaPos, "Missing end of Lua string."))
						elseif s:find("^\\", pos) then
							pos = pos + 1 -- '\'
						else
							break
						end
					end

				elseif s:find("^%[", pos) then -- Maybe a long-form string.
					if s:find("^=*%[", pos+1) then -- String.
						local _; _, pos = s:find("^(=*)%[[^\n]-%]%1%]", pos+1)
						if not pos then  return (parseError(context, luaPos, "Missing end of Lua string."))  end
						pos = pos - 1
					else
						-- void
					end

				elseif s:find("^%-", pos) then -- Maybe a comment.
					if not s:find("^%-", pos+1) then
						-- void
					elseif s:find("^%[=*%[", pos+2) then -- Long-form comment.
						local _; _, pos = s:find("^(=*)%[[^\n]-%]%1%]", pos+3)
						if not pos then  return (parseError(context, startPos, "Missing end parens."))  end
					else -- Line comment.
						return (parseError(context, startPos, "Missing end parens."))
					end

				else
					return (parseError(context, pos, "Internal error: Unhandled case in parenthesis."))
				end
			end

			local i2 = pos -- Should be at ')'.
			pos      = pos + 1 -- ')'

			table.insert(tokens, {position=startPos, type="expression", value=s:sub(i1, i2)})

			if lastWasName and lastTok.type == "name" then  lastTok.hasAttachment = true  end

		-- Command separator: ;
		elseif s:find("^;", pos) then
			table.insert(tokens, {position=startPos, type=";"})
			pos = pos + 1

		-- Comment: #comment
		elseif s:find("^#", pos) then
			pos = s:match("^[^\n]*\n?()", pos+1)
			ln  = ln + 1

			if lastTok and lastTok.type ~= "linebreak" then
				table.insert(tokens, {position=startPos, type="linebreak"})
			end

		else
			return (parseError(context, pos, "Unexpected character."))
		end

		lastTok = tokens[#tokens]
	end

	return tokens
end



-- bool = isToken( token|nil, tokenType [, tokenValue=any ] )
local function isToken(tok, tokType, tokValue)
	return tok ~= nil and tok.type == tokType and (tokValue == nil or tok.value == tokValue)
end

local function validateVariableName(context, tokForError, term, var)
	if not var:find"^%w+$" then  tokenError(context, tokForError, "Bad %s '%s'. Must only contain alphanumeric characters.", term, var) ; return false  end
	if not var:find"^%u"   then  tokenError(context, tokForError, "Bad %s '%s'. Must start with an uppercase letter."      , term, var) ; return false  end
	return true
end

local function parseArguments(context, tokens, tokPos, commandOrFuncName, argInfos, args, visited)
	while true do
		if not tokens[tokPos] or isToken(tokens[tokPos], "linebreak") or isToken(tokens[tokPos], ";") then
			return tokPos+1
		end

		local startTok = tokens[tokPos]
		local argName  = ""
		local v, argInfo

		-- Explicit name.
		if isToken(startTok, "name") then
			argName = startTok.value

			argInfo = itemWith1(argInfos, 1, argName)
			if not argInfo then  return (tokenError(context, startTok, "No argument '%s' for '%s'.", argName, commandOrFuncName))  end

			if visited[argName] then
				return (tokenError(context, startTok, "Duplicate argument '%s'.", argName))
			elseif not startTok.hasAttachment then
				return (parseError(context, startTok.position+#argName, "Missing value for argument '%s'.", argName))
			end

			tokPos = tokPos + 1 -- the name
		end

		-- Value.
		if isToken(tokens[tokPos], "literal") then
			v = tokens[tokPos].value

		elseif isToken(tokens[tokPos], "username") then
			local var = tokens[tokPos].value
			v         = findInStack(context, "variables", var) -- @Incomplete: Proper upvalues (which require lexical scope).
			if v == nil then  return (tokenError(context, tokens[tokPos], "No variable '%s'.", var))  end

			if tokens[tokPos].negated then
				if type(v) ~= "number" then  return (parseError(context, tokens[tokPos].position-1, "Cannot negate value. ('%s' contains a %s)", var, type(v)))  end
				v = -v
			end

		elseif isToken(tokens[tokPos], "expression") then
			local expr = tokens[tokPos].value

			local chunk, err = loadstring("return"..expr, "@", "t", setmetatable({}, { -- @Robustness: Don't use loadstring()!
				__index = function(_, k, v)  return findInStack(context, "variables", k)  end, -- @Incomplete: Proper upvalues (which require lexical scope).
			}))
			if not chunk then  return (tokenError(context, tokens[tokPos], "Invalid expression %s. (Lua: %s)", expr, (err:gsub("^:%d+: ", ""))))  end

			local ok, vOrErr = pcall(chunk)
			if not ok then  return (tokenError(context, tokens[tokPos], "Failed evaluating expression %s. (Lua: %s)", expr, (vOrErr:gsub("^:%d+: ", ""))))  end
			v = vOrErr

		else
			return (tokenError(context, tokens[tokPos], "Failed parsing argument value."))
		end

		-- Implicit name.
		if argName == "" then
			-- We use the value type to determine the argument (which might be a bit confusing).
			for _, _argInfo in ipairs(argInfos) do
				if not visited[_argInfo[1]] and (type(_argInfo[2]) == type(v) or _argInfo[2] == nil) then
					argName = _argInfo[1]
					argInfo = _argInfo
					break
				end
			end

			if argName == "" then  return (tokenError(context, startTok, "Unknown argument of type '%s' for '%s'.", type(v), commandOrFuncName))  end

		-- Validate value.
		elseif not (type(v) == type(argInfo[2]) or argInfo[2] == nil) then
			return (tokenError(context, tokens[tokPos], "Bad value for argument '%s'. (Expected %s, got %s)", argName, type(argInfo[2]), type(v)))
		end

		args   [argName] = v
		visited[argName] = true
		tokPos           = tokPos + 1 -- the value
	end
end

local function collectBodyTokens(context, tokens, tokForError, tokPos, outTokens)
	local depth         = 1
	local expectCommand = true

	while true do
		local tok = tokens[tokPos]
		if not tok then  return (tokenError(context, tokForError, "Missing end of body."))  end

		if isToken(tok, "linebreak") or isToken(tok, ";") then
			expectCommand = true

		elseif not expectCommand then
			-- void

		elseif isToken(tok, "name", "do") or isToken(tok, "name", "for") or isToken(tok, "name", "func") then -- @Volatile
			depth = depth + 1

		elseif isToken(tok, "name", "end") then
			depth = depth - 1
			if depth == 0 then  return tokPos+1  end

		else
			expectCommand = false
		end

		table.insert(outTokens, tok)
		tokPos = tokPos + 1
	end
end

local runBlock

-- tokenPosition|nil = runCommand( context, tokens, tokenPosition )
local function runCommand(context, tokens, tokPos)
	local startTok = tokens[tokPos]

	--
	-- Function call.
	--
	if isToken(tokens[tokPos], "username") then
		local funcName = tokens[tokPos].value
		local funcInfo = findInStack(context, "functions", funcName)
		if not funcInfo then  return tokenError(context, startTok, "No function '%s'.", funcName)  end
		tokPos = tokPos + 1 -- username

		if context.callStack[funcInfo] then
			return tokenError(context, startTok, "Recursively calling '%s'. (%s:%d)", funcName, context.path, getLineNumber(context.source, funcInfo.token.position))
		end

		-- Parse arguments.
		local argInfos = {}
		local args     = {}
		local visited  = {}

		for i, argName in ipairs(funcInfo.arguments) do
			argInfos[i] = {(argName:gsub("^.", string.lower)), nil} -- Argument "FooBar" gets the name "fooBar".  @Speed @Memory
		end

		tokPos = parseArguments(context, tokens, tokPos, funcName, argInfos, args, visited)
		if not tokPos then  return nil  end

		-- Run function.
		local entry = ScopeStackEntry()

		for i, argInfo in ipairs(argInfos) do
			if not visited[argInfo[1]] then  return tokenError(context, startTok, "Missing argument '%s' for '%s'.", argInfo[1], funcName)  end
			entry.variables[funcInfo.arguments[i]] = args[argInfo[1]]
		end

		table.insert(context.scopeStack, entry)
		context.callStack[funcInfo] = true

		local bodyTokPos = runBlock(context, funcInfo.tokens, 1)
		if not bodyTokPos              then  return nil  end
		if funcInfo.tokens[bodyTokPos] then  return (tokenError(context, funcInfo.tokens[bodyTokPos], "Unexpected token."))  end

		table.remove(context.scopeStack)
		context.callStack[funcInfo] = nil

		return tokPos
	end

	if not isToken(tokens[tokPos], "name") then
		return (tokenError(context, startTok, "Expected a command."))
	end
	local command = tokens[tokPos].value
	tokPos        = tokPos + 1 -- name

	--
	-- Language.
	--
	if command == "func" then
		if not isToken(tokens[tokPos], "username") then  return (tokenError(context, tokens[tokPos], "Expected a name for the function."))  end
		local funcName = tokens[tokPos].value
		local entry    = getLast(context.scopeStack)
		if entry.functions[funcName] then  tokenWarning(context, tokens[tokPos], "Duplicate function '%s' in the same scope. Replacing.", funcName)  end
		tokPos = tokPos + 1 -- username

		local funcInfo            = FunctionInfo()
		funcInfo.token            = startTok
		entry.functions[funcName] = funcInfo

		while isToken(tokens[tokPos], "username") do
			local argName = tokens[tokPos].value
			table.insert(funcInfo.arguments, argName)
			tokPos = tokPos + 1 -- username
		end

		if not (isToken(tokens[tokPos], "linebreak") or isToken(tokens[tokPos], ";")) then
			return (tokenError(context, tokens[tokPos], "Expected argument for function '%s'.", funcName))
		end

		return (collectBodyTokens(context, tokens, startTok, tokPos, funcInfo.tokens)) -- May return nil.
	end

	--
	-- Normal command.
	--
	local commandInfo = COMMANDS[command]
	if not commandInfo then  return (tokenError(context, startTok, "Unknown command '%s'.", command))  end

	local args    = {}
	local visited = {}

	for _, argInfo in ipairs(commandInfo) do
		args[argInfo[1]] = argInfo[2] -- Fill with default values.  @Speed: This is not necessary.
	end

	if (command == "set" or command == "setx" or command == "for") and isToken(tokens[tokPos], "username") and not tokens[tokPos].negated then
		-- Special case: Treat `set X` as `set "X"` (and same with 'setx' and 'for').
		args   .var = tokens[tokPos].value
		visited.var = true
		tokPos      = tokPos + 1
	end

	tokPos = parseArguments(context, tokens, tokPos, command, COMMANDS[command], args, visited)
	if not tokPos then  return nil  end

	if command == "set" then
		if args.var   == ""  then  return (tokenError(context, startTok, "Missing variable name."))  end
		if args.value == nil then  return (tokenError(context, startTok, "Missing value to assign to '%s'.", args.var))  end
		if not validateVariableName(context, startTok, "variable name", args.var) then  return nil  end

		getLast(context.scopeStack).variables[args.var] = args.value

	elseif command == "setx" then
		if args.var   == ""  then  return (tokenError(context, startTok, "Missing variable name."))  end
		if args.value == nil then  return (tokenError(context, startTok, "Missing value to assign to '%s'.", args.var))  end
		if not validateVariableName(context, startTok, "variable name", args.var) then  return nil  end

		local oldV, stackIndex = findInStack(context, "variables", args.var) -- @Incomplete: Proper upvalues (which require lexical scope).
		if oldV == nil then  return (tokenError(context, startTok, "No existing variable '%s'.", args.var))  end

		context.scopeStack[stackIndex].variables[args.var] = args.value

	elseif command == "do" then
		-- Get block body.
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens)
		if not tokPos then  return nil  end

		-- Run block.
		local entry = ScopeStackEntry()
		table.insert(context.scopeStack, entry)

		local bodyTokPos = runBlock(context, bodyTokens, 1)
		if not bodyTokPos         then  return nil  end
		if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "Unexpected token."))  end

		table.remove(context.scopeStack)

	elseif command == "for" then
		if args.var  == "" then  return (tokenError(context, startTok, "Empty variable name."))  end
		if args.step == 0  then  return (tokenError(context, startTok, "Step is zero."))  end
		if not validateVariableName(context, startTok, "variable name", args.var) then  return nil  end

		-- Get loop body.
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens)
		if not tokPos then  return nil  end

		-- Run loop.
		local entry = ScopeStackEntry()
		table.insert(context.scopeStack, entry)

		local loops = 0

		for n = args.from, args.to, args.step do
			loops = loops + 1

			if loops >= MAX_LOOPS then -- Maybe there should be a MAX_DRAW_OPERATIONS too? MAX_LOOPS could probably be higher then. @Incomplete
				tokenError(context, startTok, "Max loops exceeded. Breaking.")
				break
			end

			entry.variables[args.var] = n

			local bodyTokPos = runBlock(context, bodyTokens, 1)
			if not bodyTokPos         then  return nil  end
			if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "Unexpected token."))  end
		end

		table.remove(context.scopeStack)

	--
	-- Settings, app.
	--
	elseif command == "backdrop" then
		context.art.backdrop[1] = args.r
		context.art.backdrop[2] = args.g
		context.art.backdrop[3] = args.b
		context.art.backdrop[4] = args.a

	elseif command == "zoom" then
		context.art.zoom = args.zoom

	--
	-- Settings, init.
	--
	elseif command == "canvas" then
		if context.art.canvas then  return (tokenError(context, startTok, "Cannot use '%s' after drawing commands.", command))  end
		context.canvasW = (args.w >= 1) and args.w or LG.getWidth()
		context.canvasH = (args.h >= 1) and args.h or LG.getHeight()
		context.msaa    = args.aa^2

	--
	-- Settings, dynamic.
	--
	elseif command == "round" then
		context.round = args.round

	--
	-- State.
	--
	elseif command == "push" then
		LG.push("all")

	elseif command == "pop" then
		if    LG.getStackDepth() == 0
		then  tokenWarning(context, startTok, "Too many 'pop' commands! Ignoring.")
		else  LG.pop()  end

	elseif command == "color" then
		LG.setColor(args.r, args.g, args.b, args.a)
		shaderSend(shaderMain, "useColorTexture", false)
		if context.colorImage then
			context.colorImage:release()
			context.colorImage = nil
		end

	elseif command == "grad" then
		if context.colorImage then
			context.colorImage:release()
		end
		context.colorImage = newImageUsingPalette({"abc","abc"}, {
			a = {lerp4(args.r,args.g,args.b,args.a, args.rr,args.gg,args.bb,args.aa, 0/2)}, -- @Cleanup: Better argument names.
			b = {lerp4(args.r,args.g,args.b,args.a, args.rr,args.gg,args.bb,args.aa, 1/2)},
			c = {lerp4(args.r,args.g,args.b,args.a, args.rr,args.gg,args.bb,args.aa, 2/2)},
		})
		shaderSend    (shaderMain, "useColorTexture"   , true)
		shaderSend    (shaderMain, "colorTexture"      , context.colorImage)
		shaderSendVec4(shaderMain, "colorTextureLayout", math.cos(args.angle),math.sin(args.angle), args.scale,1)

	elseif command == "font" then
		LG.setNewFont(args.size)

	elseif command == "makemask" then
		ensureCanvas(context)
		maybeApplyMask(context)

		LG.setCanvas(context.maskCanvas)
		if args.clear then  LG.clear(0, 0, 0, 1)  end
		shaderSend(shaderMain, "maskMode", false)

		LG.setColor(1, 1, 1) -- Should we undo this when we exit makemask mode? Probably not as other things, like font, don't.

	elseif command == "mask" then
		ensureCanvas(context)
		maybeApplyMask(context)

		if args.mask then
			LG.setCanvas(context.canvasToMask)
			LG.clear(0, 0, 0, 0)
		else
			LG.setCanvas(context.art.canvas)
		end
		shaderSend(shaderMain, "maskMode", false)

	elseif command == "origin" then
		LG.origin()
	elseif command == "move" then
		LG.translate(maybeRound(context,args.x), maybeRound(context,args.y))
	elseif command == "rotate" then
		LG.rotate(args.a)
	elseif command == "scale" then
		LG.scale(args.x, (args.y == 1/0 and args.x or args.y))

	--
	-- Drawing.
	--
	elseif command == "fill" then
		ensureCanvas(context)
		LG.push("all")
		LG.origin()
		LG.setColor(args.r, args.g, args.b, args.a)
		LG.rectangle("fill", -1,-1, context.art.canvas:getWidth()+2,context.art.canvas:getHeight()+2) -- Not sure if the bleeding is necessary (if msaa>1).
		LG.pop()

	elseif command == "rect" then
		ensureCanvas(context)

		local x = maybeRound(context, args.x-args.ax*args.w)
		local y = maybeRound(context, args.y-args.ay*args.h)
		local w = maybeRound(context, args.w)
		local h = maybeRound(context, args.h)

		if     args.mode == "fill" then  drawRectangleFill(x,y, w,h)
		elseif args.mode == "line" then  drawRectangleLine(x,y, w,h, args.thick)
		else
			return (tokenError(context, startTok, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode))
		end

	elseif command == "circle" then
		-- @Incomplete: Ellipses.
		ensureCanvas(context)

		local x    = maybeRound(context, args.x)
		local y    = maybeRound(context, args.y)
		local segs = (args.segs >= 3) and args.segs or math.max(math.floor(args.r*TAU/10), 64)

		if     args.mode == "fill" then  drawCircleFill(x,y, args.r, segs)
		elseif args.mode == "line" then  drawCircleLine(x,y, args.r, segs, args.thick)
		else
			return (tokenError(context, startTok, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode))
		end

	elseif command == "text" then
		if not (args.align == "left" or args.align == "center" or args.align == "right" or args.align == "justify") then
			return (tokenError(context, startTok, "Bad alignment '%s'. Must be 'left', 'center', 'right' or 'justify'.", args.align))
		end

		ensureCanvas(context)

		local font         = LG.getFont()
		local w, textLines = font:getWrap(args.text, args.wrap)
		local h            = (#textLines-1) * math.floor(font:getHeight()*font:getLineHeight()) + font:getHeight() -- @Incomplete: Line height argument.

		LG.printf(args.text, maybeRound(context,args.x-args.ax*w),maybeRound(context,args.y-args.ay*h), w, args.align)

	else
		return (tokenError(context, startTok, "Unimplemented command '%s'.", command))
	end
	return tokPos
end

-- tokenPosition|nil = runBlock( context, tokens, tokenPosition )
--[[local]] function runBlock(context, tokens, tokPos)
	while true do
		while isToken(tokens[tokPos], "linebreak") or isToken(tokens[tokPos], ";") do
			tokPos = tokPos + 1
		end
		if not tokens[tokPos] then  return tokPos  end

		tokPos = runCommand(context, tokens, tokPos)
		if not tokPos then  return nil  end
	end
end



local function cleanup(context, success)
	LG.setCanvas(nil)

	if not success and context.art.canvas then  context.art.canvas:release()  end

	if context.canvasToMask then  context.canvasToMask:release()  end
	if context.maskCanvas   then  context.maskCanvas  :release()  end
end

function _G.loadArtFile(path)
	print("Loading "..path.."...")
	LG.reset()

	local context   = Context()
	context.art     = Art()
	context.path    = path
	context.canvasW = LG.getWidth()
	context.canvasH = LG.getHeight()

	local file, err = io.open(context.path) -- @Incomplete: Use PhysFS.
	if not file then
		print("Error: "..err)
		return nil
	end
	context.source = file:read"*a"
	file:close()

	local tokens = tokenize(context, context.source, context.path)
	if not tokens then  return nil  end

	local entry           = ScopeStackEntry()
	entry.variables.True  = true -- @Robustness: Make these constant.
	entry.variables.False = false
	entry.variables.Huge  = 1/0
	entry.variables.Pi    = math.pi
	entry.variables.Tau   = TAU

	table.insert(context.scopeStack, entry)

	local tokPos = runBlock(context, tokens, 1) -- Now things will happen!
	if not tokPos then
		cleanup(context, false)
		return nil
	end
	if tokens[tokPos] then
		tokenError(context, tokens[tokPos], "Unexpected token.")
		cleanup(context, false)
		return nil
	end

	assert(#context.scopeStack == 1)

	if context.art.canvas then
		maybeApplyMask(context)
	end
	cleanup(context, true)

	print("Loading "..context.path.."... done!")
	return context.art.canvas and context.art
end


