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
	-- command = { {argName1,defaultValue}, ... },

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

	path  = "",
	lines = {},

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
	bodyLn1 = 0,
	bodyLn2 = 0,

	-- [1] = argName1, ...
}end

local function ScopeStackEntry()return{
	functions = {--[[ [funcName1]=funcInfo, ... ]]},
	variables = {--[[ [varName1 ]=value   , ... ]]},
}end



-- value|nil, stackIndex = findInStack( context, member, key )
local function findInStack(context, member, k)
	for i = #context.scopeStack, 1, -1 do
		local v = context.scopeStack[i][member][k]
		if v ~= nil then  return v, i  end
	end
	return nil -- Not found!
end



local function findNextUnnamedArgument(argInfos, orderN, vType)
	if not orderN[vType] then  return nil  end -- Unsupported type.

	orderN[vType] = orderN[vType] + 1
	local n       = 0

	for _, argInfo in ipairs(argInfos) do
		if type(argInfo[2]) == vType or argInfo[2] == nil then -- @Incomplete: Handle the "any" type (defaultValue=nil) better. (Should they update orderN for all types?)
			n = n + 1
			if n == orderN[vType] then  return argInfo  end
		end
	end

	return nil
end



local function parseCommand(line)
	if line:match"^%u" then
		return line:match"^(%a+)(.*)" -- Function name for call.
	else
		return line:match"^(%l+)(.*)" -- Normal command.
	end
end

-- number|nil, kind = parseNumber( numberString )
local function parseNumber(nStr)
	if nStr:find"%%$" then
		local n = tonumber(nStr:sub(1, -2))
		return n and n/100, "percent"

	elseif nStr:find"deg$" then
		local n = tonumber(nStr:sub(1, -4))
		return n and math.rad(n), "degrees"

	elseif nStr:find"turns$" then
		local n = tonumber(nStr:sub(1, -6))
		return n and n*TAU, "degrees"

	else
		return tonumber(nStr), "number"
	end
end

local function validateVariableName(context, ln, term, var)
	if not var:find"^%a+$" then  printFileError(context, ln, "Bad %s '%s'. Must only contain letters."          , term, var) ; return false  end
	if not var:find"^%u"   then  printFileError(context, ln, "Bad %s '%s'. Must start with an uppercase letter.", term, var) ; return false  end
	return true
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



-- nextLineNumber|nil = processCommandLine( context, lineNumber )
local function processCommandLine(context, ln)
	local line = context.lines[ln]
	if line == "" then  return ln+1  end

	local command, argsStr = parseCommand(line)
	if not command then  return printFileError(context, ln, "Bad line format: %s", line)  end

	--
	-- Function declaration.
	--
	if command == "func" then
		local funcName, funcArgsStr = argsStr:match"(%S+)(.*)"
		if not funcName then  return printFileError(context, ln, "Missing function name after 'func'.")  end
		if not validateVariableName(context, ln, "function name", funcName) then  return nil  end -- @UX: Need to @Revisit.

		if getLast(context.scopeStack).functions[funcName] then  printFileWarning(context, ln, "Duplicate function '%s' in the same scope. Replacing.", funcName)  end

		local funcInfo                                  = FunctionInfo()
		funcInfo.bodyLn1                                = ln + 1
		getLast(context.scopeStack).functions[funcName] = funcInfo

		for argName in funcArgsStr:gmatch"%S+" do
			if argName:find"^#" then  break  end
			if not validateVariableName(context, ln, "argument name", argName) then  return nil  end
			table.insert(funcInfo, argName)
		end

		-- @Cleanup: Define getEndOfBlock() or something.
		local endLn = ln
		local depth = 1

		while true do
			endLn = endLn + 1
			line  = context.lines[endLn]
			if not line then  return printFileError(context, ln, "Missing end of function '%s'.", funcName)  end

			command = (line == "") and "" or parseCommand(line)
			if not command then  return printFileError(context, endLn, "Bad line format: %s", line)  end

			if command == "do" or command == "for" or command == "func" then -- @Volatile
				depth = depth + 1

			elseif command == "end" then
				depth = depth - 1

				if depth == 0 then
					funcInfo.bodyLn2 = endLn - 1
					return endLn+1
				end
			end
		end

	--
	-- Function call.
	--
	elseif command:find"^%u" then
		local funcInfo = findInStack(context, "functions", command)
		if not funcInfo then  return printFileError(context, ln, "No function '%s'.", command)  end

		if context.callStack[funcInfo] then
			return printFileError(context, ln, "Recursively calling '%s'. (%s:%d)", command, context.path, funcInfo.bodyLn1-1)
		end

		local entry = ScopeStackEntry()

		-- Arguments.
		-- @Copypaste from below.
		local argN         = 0
		local ignoreBefore = 1

		for pos, argStr in argsStr:gmatch"()(%S+)" do
			if pos < ignoreBefore then
				-- void

			-- Comment
			elseif argStr:find"^#" then
				break

			else
				argN          = argN + 1
				local argName = funcInfo[argN]

				if not argName then
					return printFileError(context, ln, "Too many arguments.")

				-- Number: N
				elseif argStr:find"^%-?%.?%d" then
					local n = parseNumber(argStr)
					if not n then  return printFileError(context, ln, "Failed parsing number: %s", argStr)  end

					entry.variables[argName] = n

				-- String: "S" OR "S OR 'S' OR 'S
				-- @Incomplete: Newlines.
				elseif argStr:find"^[\"']" then
					local q  = argStr:sub(1, 1)
					local i1 = pos + 1
					local i  = argsStr:find(q, i1, true)

					if i then
						if argsStr:find("^%S", i+1) then  return printFileError(context, ln, "Garbage after %s: %s", argsStr:sub(i1-1, i), argsStr:match("^%S+", i+1))  end
						entry.variables[argName] = argsStr:sub(i1, i-1)
						ignoreBefore             = i + 1
					else
						entry.variables[argName] = argsStr:sub(i1)
						break
					end

				-- Variable: Var
				elseif argStr:find"^%-?%u" then
					local negate, var = argStr:match"^(%-?)(.+)"
					if not validateVariableName(context, ln, "variable name", var) then  return nil  end

					local v = findInStack(context, "variables", var) -- @Incomplete: Proper upvalues (which require lexical scope).
					if v == nil then  return printFileError(context, ln, "No variable '%s'.", var)  end

					if negate == "-" then
						if type(v) ~= "number" then  return printFileError(context, ln, "Cannot negate value. (%s is a %s.)", var, type(v))  end
						v = -v
					end

					entry.variables[argName] = v

				-- Parenthesis: (expression)
				elseif argStr:find"^%(" then
					local expr, nextPos = argsStr:match("^(%b())()", pos) -- @Incomplete: Handle ')' in strings and comments in the expression.
					if not expr then  return printFileError(context, ln, "Missing end parens: %s", argsStr:sub(pos))  end

					if argsStr:find("^%S", nextPos) then  return printFileError(context, ln, "Garbage after %s: %s", expr, argsStr:match("^%S+", nextPos))  end

					local chunk, err = loadstring("return"..expr, "@", "t", setmetatable({}, { -- @Robustness: Don't use loadstring()!
						__index = function(_, k, v)  return findInStack(context, "variables", k)  end, -- @Incomplete: Proper upvalues (which require lexical scope).
					}))
					if not chunk then  return printFileError(context, ln, "Invalid expression %s. (Lua: %s)", expr, (err:gsub("^:%d+: ", "")))  end

					local ok, vOrErr = pcall(chunk)
					if not ok then  return printFileError(context, ln, "Failed evaluating expression %s. (Lua: %s)", expr, (vOrErr:gsub("^:%d+: ", "")))  end
					local v = vOrErr

					entry.variables[argName] = v
					ignoreBefore             = nextPos

				else
					return printFileError(context, ln, "Failed parsing argument #%d: %s", argN, argStr)
				end
			end
		end

		if funcInfo[argN+1] then
			return printFileError(context, ln, "Too few arguments. (Expected %d, got %d)", #funcInfo, argN)
		end

		-- Run function.
		table.insert(context.scopeStack, entry)
		context.callStack[funcInfo] = true

		local bodyLn = funcInfo.bodyLn1

		while bodyLn <= funcInfo.bodyLn2 do -- @Robustness: Make sure we don't overshoot the body (which shouldn't happen unless there's a bug somewhere).
			bodyLn = processCommandLine(context, bodyLn)
			if not bodyLn then  return printFileMessage(context, ln, "in '%s' (%s:%d)", command, context.path, funcInfo.bodyLn1-1)  end
		end

		table.remove(context.scopeStack)
		context.callStack[funcInfo] = nil

		return ln+1
	end

	--
	-- Normal command.
	--
	local commandInfo = COMMANDS[command]
	if not commandInfo then  return printFileError(context, ln, "Bad command '%s'.", command)  end

	local args = {}

	for _, argInfo in ipairs(commandInfo) do
		args[argInfo[1]] = argInfo[2]
	end

	local argN         = 0
	local ignoreBefore = 1
	local orderN       = {number=0, string=0, boolean=0}
	local visited      = {--[[ [k1]=true, ... ]]}

	for pos, argStr in argsStr:gmatch"()(%S+)" do
		if pos < ignoreBefore then
			-- void

		-- Comment
		elseif argStr:find"^#" then
			break

		else
			argN = argN + 1

			-- Set flag: X
			if type(args[argStr]) == "boolean" then -- @Incomplete: Handle the "any" type.
				local k = argStr

				if visited[k] then  return printFileError(context, ln, "Duplicate argument '%s'. (At: %s)", k, argStr)  end
				visited[k] = true

				args[k] = true

			-- Unset flag: !X
			elseif argStr:find"^!%l" then
				local k = argStr:sub(2)
				if type(args[k]) ~= "boolean" then  return printFileError(context, ln, "Argument '%s' is not a boolean.", k)  end -- @Incomplete: Handle the "any" type.

				if visited[k] then  return printFileError(context, ln, "Duplicate argument '%s'. (At: %s)", k, argStr)  end
				visited[k] = true

				args[k] = false

			-- Number: nameN OR N
			elseif argStr:find"^%l*%-?%.?%d" then
				local k, nStr = argStr:match"^(%l*)(.+)"

				-- Named.
				if k ~= "" then
					if not itemWith1(commandInfo, 1, k) then  return printFileError(context, ln, "Unknown argument '%s'.", k)  end

				-- Ordered.
				else
					local argInfo = findNextUnnamedArgument(commandInfo, orderN, "number")
					if not argInfo then  return printFileError(context, ln, "Too many unnamed arguments of type 'number'. (At: %s)", argStr)  end -- @UX: Better message if the command take no arguments of the type.
					k = argInfo[1]
				end

				if visited[k] then  return printFileError(context, ln, "Duplicate argument '%s'. (At: %s)", k, argStr)  end
				visited[k] = true

				if not (type(args[k]) == "number" or args[k] == nil) then -- @Incomplete: Handle the "any" type better.
					return printFileError(context, ln, "Argument '%s' is not a number.", k)
				end

				local n, nKind = parseNumber(nStr)
				if not n then  return printFileError(context, ln, "Failed parsing number: %s", nStr)  end

				args[k] = n

			-- String: name"S" OR name"S OR name'S' OR name'S OR "S" OR "S OR 'S' OR 'S
			-- @Incomplete: Newlines.
			elseif argStr:find"^%l*[\"']" then
				local k, q = argStr:match"^(%l*)(.)"
				local i1   = pos + #k + 1
				local i    = argsStr:find(q, i1, true)

				-- Named.
				if k ~= "" then
					if not itemWith1(commandInfo, 1, k) then  return printFileError(context, ln, "Unknown argument '%s'.", k)  end

				-- Ordered.
				else
					local argInfo = findNextUnnamedArgument(commandInfo, orderN, "string")
					if not argInfo then  return printFileError(context, ln, "Too many unnamed arguments of type 'string'. (At: %s)", argStr)  end -- @UX: Better message if the command take no arguments of the type.
					k = argInfo[1]
				end

				if visited[k] then  return printFileError(context, ln, "Duplicate argument '%s'. (At: %s)", k, argStr)  end
				visited[k] = true

				if not (type(args[k]) == "string" or args[k] == nil) then -- @Incomplete: Handle the "any" type better.
					return printFileError(context, ln, "Argument '%s' is not a string.", k)
				end

				if i then
					if argsStr:find("^%S", i+1) then  return printFileError(context, ln, "Garbage after %s: %s", argsStr:sub(i1-1, i), argsStr:match("^%S+", i+1))  end
					args[k]      = argsStr:sub(i1, i-1)
					ignoreBefore = i + 1
				else
					args[k] = argsStr:sub(i1)
					break
				end

			-- Special case: Treat `set X` as `set "X"` (and same with 'setx' and 'for').
			elseif (command == "set" or command == "setx" or command == "for") and argN == 1 and argStr:find"^%u" then
				orderN.string = 1
				args.var      = argStr

			-- Variable: nameVar OR Var
			elseif argStr:find"^%l*%-?%u" then
				local k, negate, var = argStr:match"^(%l*)(%-?)(.+)"
				if not validateVariableName(context, ln, "variable name", var) then  return nil  end

				local v = findInStack(context, "variables", var) -- @Incomplete: Proper upvalues (which require lexical scope).
				if v == nil then  return printFileError(context, ln, "No variable '%s'.", var)  end

				if negate == "-" then
					if type(v) ~= "number" then  return printFileError(context, ln, "Cannot negate value. (%s is a %s.)", var, type(v))  end
					v = -v
				end

				-- Named.
				if k ~= "" then
					if not itemWith1(commandInfo, 1, k) then  return printFileError(context, ln, "Unknown argument '%s'.", k)  end

				-- Ordered.
				else
					-- This might be confusing! We're using the type of the variable's value to assign an argument number.
					local argInfo = findNextUnnamedArgument(commandInfo, orderN, type(v))
					if not argInfo then  return printFileError(context, ln, "Too many unnamed arguments of type %s. (At: %s)", type(v), argStr)  end -- @UX: Better message if the command take no arguments of the type.
					k = argInfo[1]
				end

				if visited[k] then  return printFileError(context, ln, "Duplicate argument '%s'. (At: %s)", k, argStr)  end
				visited[k] = true

				if not (type(args[k]) == type(v) or args[k] == nil) then -- @Incomplete: Handle the "any" type better.
					return printFileError(context, ln, "Argument '%s' is not a %s.", k, type(args[k]))
				end

				args[k] = v

			-- Parenthesis: name(expression) OR (expression)
			elseif argStr:find"^%l*%(" then
				local k, expr, nextPos = argsStr:match("^(%l*)(%b())()", pos) -- @Incomplete: Handle ')' in strings and comments in the expression.
				if not expr then  return printFileError(context, ln, "Missing end parens: %s", argsStr:sub(pos))  end

				if argsStr:find("^%S", nextPos) then  return printFileError(context, ln, "Garbage after %s: %s", expr, argsStr:match("^%S+", nextPos))  end

				local chunk, err = loadstring("return"..expr, "@", "t", setmetatable({}, { -- @Robustness: Don't use loadstring()!
					__index = function(_, k, v)  return findInStack(context, "variables", k)  end, -- @Incomplete: Proper upvalues (which require lexical scope).
				}))
				if not chunk then  return printFileError(context, ln, "Invalid expression %s. (Lua: %s)", expr, (err:gsub("^:%d+: ", "")))  end

				local ok, vOrErr = pcall(chunk)
				if not ok then  return printFileError(context, ln, "Failed evaluating expression %s. (Lua: %s)", expr, (vOrErr:gsub("^:%d+: ", "")))  end
				local v = vOrErr

				-- Named.
				if k ~= "" then
					if not itemWith1(commandInfo, 1, k) then  return printFileError(context, ln, "Unknown argument '%s'.", k)  end

				-- Ordered.
				else
					local argInfo = findNextUnnamedArgument(commandInfo, orderN, type(v))
					if not argInfo then  return printFileError(context, ln, "Too many unnamed arguments of type '%s'. (At: %s)", type(v), expr)  end -- @UX: Better message if the command take no arguments of the type.
					k = argInfo[1]
				end

				if visited[k] then  return printFileError(context, ln, "Duplicate argument '%s'. (At: %s)", k, argStr)  end
				visited[k] = true

				if not (type(args[k]) == type(v) or args[k] == nil) then -- @Incomplete: Handle the "any" type better.
					return printFileError(context, ln, "Argument '%s' is not a %s.", k, type(args[k]))
				end

				args[k]      = v
				ignoreBefore = nextPos

			else
				return printFileError(context, ln, "Failed parsing argument #%d: %s", argN, argStr)
			end
		end
	end

	-- Language.
	if command == "set" then
		if args.var   == ""  then  return printFileError(context, ln, "Missing variable name.")  end
		if args.value == nil then  return printFileError(context, ln, "Missing value to assign to '%s'.", args.var)  end
		if not validateVariableName(context, ln, "variable name", args.var) then  return nil  end

		getLast(context.scopeStack).variables[args.var] = args.value

	elseif command == "setx" then
		if args.var   == ""  then  return printFileError(context, ln, "Missing variable name.")  end
		if args.value == nil then  return printFileError(context, ln, "Missing value to assign to '%s'.", args.var)  end
		if not validateVariableName(context, ln, "variable name", args.var) then  return nil  end

		local v, stackIndex = findInStack(context, "variables", args.var) -- @Incomplete: Proper upvalues (which require lexical scope).
		if v == nil then  return printFileError(context, ln, "No existing variable '%s'.", args.var)  end

		context.scopeStack[stackIndex].variables[args.var] = args.value

	elseif command == "do" then
		-- Get block body.  @Cleanup: Define getEndOfBlock() or something.
		local bodyLn1 = ln + 1
		local bodyLn2 = ln
		local depth   = 1

		while true do
			bodyLn2 = bodyLn2 + 1
			line    = context.lines[bodyLn2]
			if not line then  return printFileError(context, ln, "Missing end of do-block.")  end

			command = (line == "") and "" or parseCommand(line)
			if not command then  return printFileError(context, bodyLn2, "Bad line format: %s", line)  end

			if command == "do" or command == "for" or command == "func" then -- @Volatile
				depth = depth + 1

			elseif command == "end" then
				depth = depth - 1

				if depth == 0 then
					bodyLn2 = bodyLn2 - 1
					break
				end
			end
		end

		-- Run block.
		local entry = ScopeStackEntry()
		table.insert(context.scopeStack, entry)

		local bodyLn = bodyLn1

		while bodyLn <= bodyLn2 do -- @Robustness: Make sure we don't overshoot the body (which shouldn't happen unless there's a bug somewhere).
			bodyLn = processCommandLine(context, bodyLn)
			if not bodyLn then  return nil  end
		end

		table.remove(context.scopeStack)
		ln = bodyLn2 + 1

	elseif command == "for" then
		if args.var  == "" then  return printFileError(context, ln, "Missing variable name.")  end
		if args.step == 0  then  return printFileError(context, ln, "Step is zero.")  end

		-- Get loop body.  @Cleanup: Define getEndOfBlock() or something.
		local bodyLn1 = ln + 1
		local bodyLn2 = ln
		local depth   = 1

		while true do
			bodyLn2 = bodyLn2 + 1
			line    = context.lines[bodyLn2]
			if not line then  return printFileError(context, ln, "Missing end of for-loop.")  end

			command = (line == "") and "" or parseCommand(line)
			if not command then  return printFileError(context, bodyLn2, "Bad line format: %s", line)  end

			if command == "do" or command == "for" or command == "func" then -- @Volatile
				depth = depth + 1

			elseif command == "end" then
				depth = depth - 1

				if depth == 0 then
					bodyLn2 = bodyLn2 - 1
					break
				end
			end
		end

		-- Run loop.
		local entry = ScopeStackEntry()
		table.insert(context.scopeStack, entry)

		local loops = 0

		for n = args.from, args.to, args.step do
			loops = loops + 1

			if loops >= MAX_LOOPS then -- Maybe there should be a MAX_DRAW_OPERATIONS too? MAX_LOOPS could probably be higher then. @Incomplete
				printFileError(context, bodyLn1, "Max loops exceeded. Breaking.")
				break
			end

			entry.variables[args.var] = n
			local bodyLn              = bodyLn1

			while bodyLn <= bodyLn2 do -- @Robustness: Make sure we don't overshoot the body (which shouldn't happen unless there's a bug somewhere).
				bodyLn = processCommandLine(context, bodyLn)
				if not bodyLn then  return nil  end
			end
		end

		table.remove(context.scopeStack)
		ln = bodyLn2 + 1

	-- Setting, app.
	elseif command == "backdrop" then
		context.art.backdrop[1] = args.r
		context.art.backdrop[2] = args.g
		context.art.backdrop[3] = args.b
		context.art.backdrop[4] = args.a

	elseif command == "zoom" then
		context.art.zoom = args.zoom

	-- Settings, init.
	elseif command == "canvas" then
		if context.art.canvas then  return printFileError(context, ln, "Cannot use '%s' after drawing commands.", command)  end
		context.canvasW = (args.w >= 1) and args.w or LG.getWidth()
		context.canvasH = (args.h >= 1) and args.h or LG.getHeight()
		context.msaa    = args.aa^2

	-- Settings, dynamic.
	elseif command == "round" then
		context.round = args.round

	-- State.
	elseif command == "push" then
		LG.push("all")
	elseif command == "pop" then
		if    LG.getStackDepth() == 0
		then  printFileWarning(context, ln, "Too many 'pop' commands! Ignoring.")
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

	-- Drawing.
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
			return printFileError(context, ln, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode)
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
			return printFileError(context, ln, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode)
		end

	elseif command == "text" then
		if not (args.align == "left" or args.align == "center" or args.align == "right" or args.align == "justify") then
			return printFileError(context, ln, "Bad alignment '%s'. Must be 'left', 'center', 'right' or 'justify'.", args.align)
		end

		ensureCanvas(context)

		local font         = LG.getFont()
		local w, textLines = font:getWrap(args.text, args.wrap)
		local h            = (#textLines-1) * math.floor(font:getHeight()*font:getLineHeight()) + font:getHeight() -- @Incomplete: Line height argument.

		LG.printf(args.text, maybeRound(context,args.x-args.ax*w),maybeRound(context,args.y-args.ay*h), w, args.align)

	elseif command == "end" then
		return printFileError(context, ln, "Unexpected '%s'.", command)
	else
		printFileWarning(context, ln, "@Incomplete: Command '%s'. Ignoring.", command)
	end

	return ln+1
end



function _G.loadArtFile(path)
	print("Loading "..path.."...")
	LG.reset()

	local context   = Context()
	context.art     = Art()
	context.path    = path
	context.canvasW = LG.getWidth()
	context.canvasH = LG.getHeight()

	for line in io.lines(context.path) do -- @Incomplete: Use PhysFS.
		line = line:gsub("^%s+", "")
		table.insert(context.lines, (line:find"^#" and "" or line))
	end

	local entry           = ScopeStackEntry()
	entry.variables.True  = true
	entry.variables.False = false
	entry.variables.Huge  = 1/0
	entry.variables.Pi    = math.pi
	entry.variables.Tau   = TAU

	table.insert(context.scopeStack, entry)

	local ln = 1

	while context.lines[ln] do
		ln = processCommandLine(context, ln)
		if not ln then
			LG.setCanvas(nil)
			if context.art.canvas   then  context.art.canvas  :release()  end
			if context.canvasToMask then  context.canvasToMask:release()  end
			if context.maskCanvas   then  context.maskCanvas  :release()  end
			return nil
		end
	end
	assert(#context.scopeStack == 1)

	if context.art.canvas then
		maybeApplyMask(context)
	end
	LG.setCanvas(nil)

	print("Loading "..context.path.."... done!")

	if context.canvasToMask then  context.canvasToMask:release()  end
	if context.maskCanvas   then  context.maskCanvas  :release()  end
	return context.art.canvas and context.art
end


