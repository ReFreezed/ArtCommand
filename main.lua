--[[============================================================
--=
--=  Art Command
--=  by Marcus 'ReFreezed' Thunström
--=
--=  $ love . pathToArtcmd
--=
--============================================================]]

local TAU = 2*math.pi

local LG = love.graphics

local theArt     = nil
local shaderMain = nil
local shaderMask = nil
local checkerImage, checkerQuad



local function printFileMessage(pathOrContext, ln, s, ...)
	print(string.format("%s:%d: "..s, (pathOrContext.path or pathOrContext), ln, ...))
end

local function printFileError(pathOrContext, ln, s, ...)
	print(string.format("%s:%d: Error: "..s, (pathOrContext.path or pathOrContext), ln, ...))
end

local function printFileWarning(pathOrContext, ln, s, ...)
	print(string.format("%s:%d: Warning: "..s, (pathOrContext.path or pathOrContext), ln, ...))
end



local function getLast(arr)
	return arr[#arr]
end

local function itemWith1(arr, k, v)
	for i = 1, #arr do
		if arr[i][k] == v then  return arr[i], i  end
	end
	return nil
end



local COMMANDS = {
	-- command = { {argName1,defaultValue}, ... },

	-- Settings, init.
	canvas = { {"w",0--[[=auto]]},{"h",0--[[=auto]]}, {"aa",1} },

	-- Settings, dynamic.
	round   = { },
	noround = { },

	-- State.
	push     = { },
	pop      = { },
	color    = { {"r",1},{"g",1},{"b",1},{"a",1} },
	font     = { {"size",12} },
	makemask = { },
	mask     = { },
	nomask   = { },
	set      = { {"var",""}, {"value",nil} },
	origin   = { },
	move     = { {"x",0},{"y",0} },
	rotate   = { {"a",0} },
	scale    = { {"x",1},{"y",1/0--[[=x]]} },

	-- Drawing.
	backdrop = { {"r",0},{"g",0},{"b",0},{"a",1} },
	clear    = { {"r",0},{"g",0},{"b",0},{"a",1} },
	circle   = { {"x",0},{"y",0}, {"r",5}, {"segs",0--[[=auto]]}, {"fill",true},{"line",false} },
	rect     = { {"x",0},{"y",0}, {"w",10},{"h",10}, {"ax",0},{"ay",0}, {"fill",true},{"line",false} },
	text     = { {"x",0},{"y",0}, {"text",""}, {"ax",0},{"ay",0} },
}

local CONSTANTS = {
	["true" ] = true,
	["false"] = false,
	["huge" ] = 1/0,
	["-huge"] = -1/0,
	["pi"   ] = math.pi,
	["-pi"  ] = -math.pi,
	["tau"  ] = TAU,
	["-tau" ] = -TAU,
}

local function StackEntry()
	return {
		funcs = {--[[ [funcName1]=funcInfo, ... ]]},
		vars  = {--[[ [varName1 ]=value   , ... ]]},
	}
end

local function findInStack(context, member, k)
	for i = #context.stack, 1, -1 do
		local v = context.stack[i][member][k]
		if v ~= nil then  return v  end
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

local function ensureCanvas(context)
	if context.art.canvas then  return  end

	context.art.canvas = LG.newCanvas(context.canvasW,context.canvasH, {msaa=(context.msaa > 1 and context.msaa or nil)})
	context.maskCanvas = LG.newCanvas(context.canvasW,context.canvasH, {format="r8"})

	shaderMain:send("useMask", false)
	shaderMain:send("mask", context.maskCanvas)

	LG.setCanvas(context.art.canvas)
	LG.setShader(shaderMain)
end

local function maybeRound(context, x)
	return context.doRound and math.floor(x+.5) or x
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
		if not funcName             then  return printFileError(context, ln, "Missing function name after 'func'.")  end
		if not funcName:find"^%a+$" then  return printFileError(context, ln, "Bad function name '%s'. Must only contain letters.", funcName)  end -- @UX: Not a good rule. Maybe require space after the command?
		if not funcName:find"^%u"   then  return printFileError(context, ln, "Bad function name '%s'. Must start with an uppercase letter.", funcName)  end

		if getLast(context.stack).funcs[funcName] then  printFileWarning(context, ln, "Duplicate function '%s' in the same scope. Replacing.", funcName)  end

		local funcInfo                         = {bodyLn1=ln+1, bodyLn2=0, --[[argName1, ...]]}
		getLast(context.stack).funcs[funcName] = funcInfo

		for argName in funcArgsStr:gmatch"%S+" do
			if argName:find"^#" then  break  end
			table.insert(funcInfo, argName) -- @Incomplete: Validate variable name format.
		end

		local endLn = ln
		local depth = 1

		while true do
			endLn = endLn + 1
			line  = context.lines[endLn]
			if not line then  return printFileError(context, ln, "Missing end of function '%s'.", funcName)  end

			command = (line == "") and "" or parseCommand(line)
			if not command then  return printFileError(context, endLn, "Bad line format: %s", line)  end

			if command == "for" or command == "func" then -- @Volatile
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
	-- For-loop.
	--
	elseif command == "for" then
		return printFileError(context, ln, "@Incomplete: Command '%s'.", command)

	--
	-- Function call.
	--
	elseif command:find"^%u" then
		local funcInfo = findInStack(context, "funcs", command)
		if not funcInfo then  return printFileError(context, ln, "No function '%s'.", command)  end

		if context.callstack[funcInfo] then
			return printFileError(context, ln, "Recursively calling '%s'. (%s:%d)", command, context.path, funcInfo.bodyLn1-1)
		end

		local entry = StackEntry()

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

					entry.vars[argName] = n

				-- String: "S" OR "S OR 'S' OR 'S
				-- @Incomplete: Newlines.
				elseif argStr:find"^[\"']" then
					local q  = argStr:sub(1, 1)
					local i1 = pos + 1
					local i  = argsStr:find(q, i1, true)

					if    i
					then  entry.vars[argName] = argsStr:sub(i1, i-1) ; ignoreBefore = i+1 ; if argsStr:find("^%S", i+1) then  return printFileError(context, ln, "Garbage after %s", argsStr:sub(pos, i))  end
					else  entry.vars[argName] = argsStr:sub(i1     ) ; break  end

				-- Variable: Var
				elseif argStr:find"^%u" then
					local var = argStr -- @Incomplete: Validate variable name format.

					local v = getLast(context.stack).vars[var]--findInStack(context, "vars", var) -- @Incomplete: Upvalues (which require lexical scope).
					if v == nil then  return printFileError(context, ln, "No variable '%s'.", var)  end

					entry.vars[argName] = v

				-- Constant: K
				elseif CONSTANTS[argStr] then
					entry.vars[argName] = CONSTANTS[argStr]

				-- Parenthesis: (expression)
				elseif argStr:find"^%(" then
					local expr, nextPos = argsStr:match("^(%b())()", pos) -- @Incomplete: Handle ')' in strings and comments in the expression.
					if not expr then  return printFileError(context, ln, "Missing end parens: %s", argsStr:sub(pos))  end

					local chunk, err = loadstring("return"..expr, "@", "t", setmetatable({}, { -- @Robustness: Don't use loadstring()!
						__index = function(_, k, v)  return CONSTANTS[k] or getLast(context.stack).vars[k]  end,
					}))
					if not chunk then  return printFileError(context, ln, "Invalid expression '%s'. (Lua: %s)", expr, (err:gsub("^:%d+: ", "")))  end

					local ok, vOrErr = pcall(chunk)
					if not ok then  return printFileError(context, ln, "Failed evaluating expression '%s'. (Lua: %s)", expr, (vOrErr:gsub("^:%d+: ", "")))  end
					local v = vOrErr

					entry.vars[argName] = v

					ignoreBefore = nextPos
					if argsStr:find("^%S", nextPos) then  return printFileError(context, ln, "Garbage after '%s': %s", expr, argsStr:match("^%S+", pos+#expr))  end

				else
					return printFileError(context, ln, "Failed parsing argument #%d: %s", argN, argStr)
				end
			end
		end

		if funcInfo[argN+1] then
			return printFileError(context, ln, "Too few arguments. (Expected %d, got %d)", #funcInfo, argN)
		end

		-- Run function.
		table.insert(context.stack, entry)
		context.callstack[funcInfo] = true

		local funcLn = funcInfo.bodyLn1

		while funcLn <= funcInfo.bodyLn2 do -- @Robustness: Make sure we don't overshoot the body (which shouldn't happen unless there's a bug somewhere).
			funcLn = processCommandLine(context, funcLn)
			if not funcLn then  return printFileMessage(context, ln, "in '%s' (%s:%d)", command, context.path, funcInfo.bodyLn1-1)  end
		end

		table.remove(context.stack)
		context.callstack[funcInfo] = nil

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

				if    i
				then  args[k] = argsStr:sub(i1, i-1) ; ignoreBefore = i+1 ; if argsStr:find("^%S", i+1) then  return printFileError(context, ln, "Garbage after %s", argsStr:sub(pos+#k, i))  end
				else  args[k] = argsStr:sub(i1     ) ; break  end

			-- Special case: Treat `set X` as `set "X"`.
			elseif command == "set" and argN == 1 and argStr:find"^%u" then
				orderN.string = 1
				args.var      = argStr -- @Incomplete: Validate variable name format.

			-- Variable: nameVar OR Var
			elseif argStr:find"^%l*%u" then
				local k, var = argStr:match"^(%l*)(.+)" -- @Incomplete: Validate variable name format.

				local v = getLast(context.stack).vars[var]--findInStack(context, "vars", var) -- @Incomplete: Upvalues (which require lexical scope).
				if v == nil then  return printFileError(context, ln, "No variable '%s'.", var)  end

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

			-- Constant: K
			-- @Incomplete: nameK, somehow. For now parentheses can be used.
			elseif CONSTANTS[argStr] then
				local k = "" -- @Temp
				local v = CONSTANTS[argStr]

				-- Named.
				if k ~= "" then
					if not itemWith1(commandInfo, 1, k) then  return printFileError(context, ln, "Unknown argument '%s'.", k)  end

				-- Ordered.
				else
					local argInfo = findNextUnnamedArgument(commandInfo, orderN, type(v))
					if not argInfo then  return printFileError(context, ln, "Too many unnamed arguments of type '%s'. (At: %s)", type(v), argStr)  end -- @UX: Better message if the command take no arguments of the type.
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

				local chunk, err = loadstring("return"..expr, "@", "t", setmetatable({}, { -- @Robustness: Don't use loadstring()!
					__index = function(_, k, v)  return CONSTANTS[k] or getLast(context.stack).vars[k]  end,
				}))
				if not chunk then  return printFileError(context, ln, "Invalid expression '%s'. (Lua: %s)", expr, (err:gsub("^:%d+: ", "")))  end

				local ok, vOrErr = pcall(chunk)
				if not ok then  return printFileError(context, ln, "Failed evaluating expression '%s'. (Lua: %s)", expr, (vOrErr:gsub("^:%d+: ", "")))  end
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

				args[k] = v

				ignoreBefore = nextPos
				if argsStr:find("^%S", nextPos) then  return printFileError(context, ln, "Garbage after '%s': %s", expr, argsStr:match("^%S+", pos+#k+#expr))  end

			else
				return printFileError(context, ln, "Failed parsing argument #%d: %s", argN, argStr)
			end
		end
	end

	-- Settings, init.
	if command == "canvas" then
		if context.art.canvas then  return printFileError(context, ln, "Cannot use '%s' after drawing commands.", command)  end
		context.canvasW = (args.w >= 1) and args.w or LG.getWidth()
		context.canvasH = (args.h >= 1) and args.h or LG.getHeight()
		context.msaa    = args.aa^2

	-- Settings, dynamic.
	elseif command == "round" then
		context.doRound = true
	elseif command == "noround" then
		context.doRound = false

	-- State.
	elseif command == "push" then
		LG.push("all")
	elseif command == "pop" then
		if    love.graphics.getStackDepth() == 0
		then  printFileWarning(context, ln, "Too many 'pop' commands! Ignoring.")
		else  LG.pop()  end

	elseif command == "color" then
		LG.setColor(args.r, args.g, args.b, args.a)

	elseif command == "font" then
		LG.setNewFont(args.size)

	elseif command == "makemask" then
		ensureCanvas(context)
		LG.setCanvas(context.maskCanvas)
		LG.setShader(shaderMask)
		LG.clear(0, 0, 0, 1)
	elseif command == "mask" then
		ensureCanvas(context)
		shaderMain:send("useMask", true)
		LG.setCanvas(context.art.canvas)
		LG.setShader(shaderMain)
	elseif command == "nomask" then
		ensureCanvas(context)
		shaderMain:send("useMask", false)
		LG.setCanvas(context.art.canvas)
		LG.setShader(shaderMain)

	elseif command == "set" then
		if args.var   == ""  then  return printFileError(context, ln, "Missing variable name.")  end
		if args.value == nil then  return printFileError(context, ln, "Missing value to assign to '%s'.", args.var)  end
		getLast(context.stack).vars[args.var] = args.value

	elseif command == "origin" then
		LG.origin()
	elseif command == "move" then
		LG.translate(maybeRound(context,args.x), maybeRound(context,args.y))
	elseif command == "rotate" then
		LG.rotate(args.a)
	elseif command == "scale" then
		LG.scale(args.x, (args.y == 1/0 and args.x or args.y))

	-- Drawing.
	elseif command == "backdrop" then
		context.art.backdrop[1] = args.r
		context.art.backdrop[2] = args.g
		context.art.backdrop[3] = args.b
		context.art.backdrop[4] = args.a

	elseif command == "clear" then
		ensureCanvas(context)
		LG.clear(args.r, args.g, args.b, args.a)

	elseif command == "circle" then
		ensureCanvas(context)
		local segs = (args.segs >= 3) and args.segs or math.max(64, math.floor(args.r*TAU/10))
		if args.fill then  LG.circle("fill", maybeRound(context,args.x),maybeRound(context,args.y), args.r, segs)  end
		if args.line then  LG.circle("line", maybeRound(context,args.x),maybeRound(context,args.y), args.r, segs)  end
	elseif command == "rect" then
		ensureCanvas(context)
		if args.fill then  LG.rectangle("fill", maybeRound(context,args.x-args.ax*args.w),maybeRound(context,args.y-args.ay*args.h), maybeRound(context,args.w),maybeRound(context,args.h))  end
		if args.line then  LG.rectangle("line", maybeRound(context,args.x-args.ax*args.w),maybeRound(context,args.y-args.ay*args.h), maybeRound(context,args.w),maybeRound(context,args.h))  end
	elseif command == "text" then
		ensureCanvas(context)
		local font         = LG.getFont()
		local w, textLines = font:getWrap(args.text, 1/0)
		local h            = (#textLines-1) * math.floor(font:getHeight()*font:getLineHeight()) + font:getHeight()
		LG.print(args.text, maybeRound(context,args.x-args.ax*w),maybeRound(context,args.y-args.ay*h))

	else
		printFileWarning(context, ln, "@Incomplete: Command '%s'. Ignoring.", command)
	end

	return ln+1
end

local function loadArtFile(path)
	print("Loading "..path.."...")
	LG.reset()

	local context = {
		art = {
			canvas   = nil,
			backdrop = {0,0,0,0},
		},

		path       = path,
		lines      = {},
		stack      = {--[[ stackEntry1, ... ]]},
		callstack  = {--[[ [funcInfo1]=true, ... ]]},
		maskCanvas = nil,

		canvasW = LG.getWidth(),
		canvasH = LG.getHeight(),
		msaa    = 1,
		doRound = false,
	}

	for line in love.filesystem.lines(context.path) do
		line = line:gsub("^%s+", "")
		table.insert(context.lines, (line:find"^#" and "" or line))
	end

	table.insert(context.stack, StackEntry())
	local ln = 1

	while context.lines[ln] do
		ln = processCommandLine(context, ln)
		if not ln then
			LG.setCanvas(nil)
			if context.canvas     then  context.canvas    :release()  end
			if context.maskCanvas then  context.maskCanvas:release()  end
			return nil
		end
	end

	assert(#context.stack == 1)
	LG.setCanvas(nil)

	print("Loading "..context.path.."... done!")

	if context.maskCanvas then  context.maskCanvas:release()  end
	return context.art.canvas and context.art
end



function love.load(args, rawArgs)
	io.stdout:setvbuf("no")
	io.stderr:setvbuf("no")

	_G.arg = nil

	shaderMain = LG.newShader[[//GLSL
		uniform bool useMask;
		uniform Image mask;

		vec4 effect(vec4 loveColor, Image tex, vec2 texPos, vec2 screenPosPx) {
			vec4 pixel = Texel(tex, texPos);
			if (useMask)  pixel.a *= Texel(mask, screenPosPx/love_ScreenSize.xy).r;
			return pixel * loveColor;
		}
	]]
	shaderMask = LG.newShader[[//GLSL
		vec4 effect(vec4 loveColor, Image tex, vec2 texPos, vec2 screenPosPx) {
			return vec4(1, 1, 1, Texel(tex, texPos).a);
		}
	]]

	local imageData = love.image.newImageData(8, 8, "rgba8", (([[
		xxxx____
		xxxx____
		xxxx____
		xxxx____
		____xxxx
		____xxxx
		____xxxx
		____xxxx
	]]):gsub("%s+", ""):gsub(".", {x="\255\255\255\0", _="\255\255\255\255"})))
	checkerImage = LG.newImage(imageData)
	checkerImage:setWrap("repeat", "repeat")
	checkerQuad = LG.newQuad(0,0, 8,8, checkerImage:getDimensions())

	thePath = args[1] or "art/test.artcmd" -- @Incomplete: Support external files.
end



function love.keypressed(key)
	if key == "escape" then
		love.event.quit()

	elseif key == "s" and love.keyboard.isDown("lctrl","rctrl") then
		if theArt then
			print("Saving output.png...")
			theArt.canvas:newImageData():encode("png", "output.png")
			print("Saving output.png... done!")
		end
	end
end



local theModtime       = -1/0
local modtimeCheckTime = 0

function love.update(dt)
	modtimeCheckTime = modtimeCheckTime - dt

	if modtimeCheckTime < 0 then
		modtimeCheckTime = 0.20

		local info    = love.filesystem.getInfo(thePath, "file")
		local modtime = info and info.modtime

		if modtime and modtime ~= theModtime then
			theModtime = modtime
			local art  = loadArtFile(thePath)

			for i = 1, LG.getStackDepth() do
				LG.pop()
			end
			LG.reset()

			if art then
				if theArt then  theArt.canvas:release()  end
				theArt = art

				love.window.setTitle(string.format(
					"%s (%dx%d) - Art Command",
					thePath:gsub("^.*[/\\]", ""),
					theArt.canvas:getWidth(), theArt.canvas:getHeight()
				))
			end
		end
	end
end



function love.draw()
	local ww,wh = LG.getDimensions()

	LG.clear(.4, .4, .4, 1)

	checkerQuad:setViewport(0,0, ww,wh)
	LG.setColor(.6, .6, .6)
	LG.draw(checkerImage, checkerQuad)

	if theArt then
		if theArt.backdrop[4] > 0 then
			LG.setColor(theArt.backdrop)
			LG.rectangle("fill", 0,0, ww,wh)
		end

		-- LG.setBlendMode("alpha", "premultiplied")
		LG.setColor(1, 1, 1)
		LG.draw(theArt.canvas
			, math.max(math.floor((ww-theArt.canvas:getWidth ())/2), 0)
			, math.max(math.floor((wh-theArt.canvas:getHeight())/2), 0)
		)

	else
		local text = "No art loaded"
		local w    = LG.getFont():getWidth(text)
		local h    = LG.getFont():getHeight()
		local x    = math.floor((ww-w)/2)
		local y    = math.floor((wh-h)/2)

		LG.setColor(0, 0, 0)
		LG.rectangle("fill", x-4,y-2, w+8,h+4)

		LG.setColor(1, 1, 1)
		LG.print(text, x,y)
	end
end



function love.errorhandler(err)
	print(debug.traceback(tostring(err), 2))
end
love.errhand = love.errorhandler


