--[[============================================================
--=
--=  Art Command
--=  by Marcus 'ReFreezed' Thunström
--=
--=  $ love ArtCommand.love pathToArt [options]
--=
--=  Options:
--=      --out path
--=          Where to save the output to. Default is in same folder as the input path.
--=
--============================================================]]

local MAX_LOOPS           = 10000
local MAX_CIRCLE_SEGMENTS = 128
local TAU                 = 2*math.pi

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



local LG = love.graphics
local hotLoader

local A = { -- Assets.
	images = {},
	quads  = {},
}

local thePathIn       = ""
local thePathOut      = "" -- Empty means auto.
local thePathIsTest   = false
local theArt          = nil
local shaderMain      = nil
local shaderMakeMask  = nil
local shaderApplyMask = nil



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

local vec2 = {0,0}
local vec3 = {0,0,0}
local vec4 = {0,0,0,0}

local function shaderSend(shader, var, ...)
	pcall(shader.send, shader, var, ...)
end
local function shaderSendVec2(shader, var, x,y)
	vec2[1], vec2[2] = x, y
	pcall(shader.send, shader, var, vec2)
end
local function shaderSendVec3(shader, var, x,y,z)
	vec3[1], vec3[2], vec3[3] = x, y, z
	pcall(shader.send, shader, var, vec3)
end
local function shaderSendVec4(shader, var, x,y,z,w)
	vec4[1], vec4[2], vec4[3], vec4[4] = x, y, z, w
	pcall(shader.send, shader, var, vec4)
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

	shaderSend(shaderMain     , "useColorTexture", false)
	shaderSend(shaderApplyMask, "mask"           , context.maskCanvas)

	LG.setCanvas(context.art.canvas)
	LG.setShader(shaderMain)
end

local function round(v)
	return math.floor(v+.5)
end

local function clamp(v, min,max)
	return math.max(math.min(v, max), min)
end

local function lerp(v1,v2, t)
	return v1 + t*(v2-v1)
end
local function lerp4(r1,g1,b1,a1, r2,g2,b2,a2, t)
	return lerp(r1,r2, t)
	     , lerp(g1,g2, t)
	     , lerp(b1,b2, t)
	     , lerp(a1,a2, t)
end

-- colorValue8    = toColor32( colorValue )
-- r8, g8, b8, a8 = toColor32( r, g, b, a )
local function toColor32(r, g, b, a)
	if g then
		return clamp(round(r*255), 0, 255)
		     , clamp(round(g*255), 0, 255)
		     , clamp(round(b*255), 0, 255)
		     , clamp(round(a*255), 0, 255)
	else
		return clamp(round(r*255), 0, 255)
	end
end

local function maybeRound(context, x)
	return context.round and round(x) or x
end

local function validateVariableName(context, ln, term, var)
	if not var:find"^%a+$" then  printFileError(context, ln, "Bad %s '%s'. Must only contain letters."          , term, var) ; return false  end
	if not var:find"^%u"   then  printFileError(context, ln, "Bad %s '%s'. Must start with an uppercase letter.", term, var) ; return false  end
	return true
end

local function newImageUsingPalette(pixelRows, palette)
	local imageData = love.image.newImageData(#pixelRows[1], #pixelRows)

	for row, pixelRow in ipairs(pixelRows) do
		for col = 1, #pixelRow do
			local k       = pixelRow:sub(col, col)
			local pixel   = palette[k] or error("No color for '"..k.."'.")
			local r,g,b,a = unpack(pixel)
			imageData:setPixel(col-1,row-1, r,g,b,a) -- @Speed
		end
	end

	return LG.newImage(imageData)
end

local function drawRectangleFill(x,y, w,h)
	local iw,ih = A.images.rectangle:getDimensions()
	LG.draw(A.images.rectangle, x,y, 0, w/iw,h/ih)
end

local mesh, vertices = nil, {
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
}

local function drawRectangleLine(x,y, w,h, lw)
	--
	--   1 2 3 4
	-- 1 2-----4  10=2
	--   |\\  /|
	-- 2 | 1-3/|  9=1
	--   | | | |
	-- 3 |/7-5 |
	--   |/  \\|
	-- 4 8-----6
	--
	if not mesh then
		mesh = LG.newMesh(vertices, "strip", "stream")
		mesh:setTexture(A.images.rectangle)
	end

	local x1 =   - lw*.5
	local x2 =     lw*.5
	local x3 = w - lw*.5
	local x4 = w + lw*.5
	local y1 =   - lw*.5
	local y2 =     lw*.5
	local y3 = h - lw*.5
	local y4 = h + lw*.5

	local lwU = lw / (x4-x1)
	local lwV = lw / (y4-y1)

	-- Outer.
	vertices[2][1],vertices[2][2], vertices[2][3],vertices[2][4] = x1,y1, 0,0
	vertices[4][1],vertices[4][2], vertices[4][3],vertices[4][4] = x4,y1, 1,0
	vertices[6][1],vertices[6][2], vertices[6][3],vertices[6][4] = x4,y4, 1,1
	vertices[8][1],vertices[8][2], vertices[8][3],vertices[8][4] = x1,y4, 0,1

	-- Inner.
	vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4] = x2,y2, lwU,lwV
	vertices[3][1],vertices[3][2], vertices[3][3],vertices[3][4] = x3,y2, 1-lwU,lwV
	vertices[5][1],vertices[5][2], vertices[5][3],vertices[5][4] = x3,y3, 1-lwU,1-lwV
	vertices[7][1],vertices[7][2], vertices[7][3],vertices[7][4] = x2,y3, lwU,1-lwV

	vertices[ 9][1],vertices[ 9][2], vertices[ 9][3],vertices[ 9][4] = vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4]
	vertices[10][1],vertices[10][2], vertices[10][3],vertices[10][4] = vertices[2][1],vertices[2][2], vertices[2][3],vertices[2][4]
	mesh:setVertices(vertices)
	LG.draw(mesh, x,y)
end

local vertices = {{0,0, .5,.5, 1,1,1,1}}
local mesh     = nil

for i = 2, MAX_CIRCLE_SEGMENTS+2 do
	table.insert(vertices, {0,0, 0,0, 1,1,1,1})
end

local function drawCircleFill(x,y, radius, segs)
	if not mesh then
		mesh = LG.newMesh(vertices, "fan", "stream")
		mesh:setTexture(A.images.rectangle)
	end

	segs             = clamp(segs, 3, MAX_CIRCLE_SEGMENTS)
	local deltaAngle = TAU / segs

	for i = 2, segs+2 do
		local angle = i * deltaAngle
		local c     = math.cos(angle)
		local s     = math.sin(angle)
		vertices[i][1],vertices[i][2], vertices[i][3],vertices[i][4] = radius*c,radius*s, .5+.5*c,.5+.5*s
	end

	mesh:setVertices(vertices, 1, segs+2)
	mesh:setDrawRange(         1, segs+2)
	LG.draw(mesh, x,y)
end

local vertices = {}
local mesh     = nil

for i = 1, 2*MAX_CIRCLE_SEGMENTS+2 do
	table.insert(vertices, {0,0, 0,0, 1,1,1,1})
end

local function drawCircleLine(x,y, radius, segs, lw)
	if not mesh then
		mesh = LG.newMesh(vertices, "strip", "stream")
		mesh:setTexture(A.images.rectangle)
	end

	segs             = clamp(segs, 3, MAX_CIRCLE_SEGMENTS)
	local radius1    = radius - lw*.5
	local radius2    = radius + lw*.5
	local deltaAngle = TAU / segs
	local radiusUv   = (1 - lw/radius2) / 2

	for i = 1, segs do
		local angle = i * deltaAngle
		local c     = math.cos(angle)
		local s     = math.sin(angle)
		vertices[2*i-1][1],vertices[2*i-1][2], vertices[2*i-1][3],vertices[2*i-1][4] = radius1*c,radius1*s, .5+.5*c,.5+.5*s
		vertices[2*i  ][1],vertices[2*i  ][2], vertices[2*i  ][3],vertices[2*i  ][4] = radius2*c,radius2*s, .5+radiusUv*c,.5+radiusUv*s
	end

	vertices[2*segs+1][1],vertices[2*segs+1][2], vertices[2*segs+1][3],vertices[2*segs+1][4] = vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4]
	vertices[2*segs+2][1],vertices[2*segs+2][2], vertices[2*segs+2][3],vertices[2*segs+2][4] = vertices[2][1],vertices[2][2], vertices[2][3],vertices[2][4]

	mesh:setVertices(vertices, 1, 2*segs+2)
	mesh:setDrawRange(         1, 2*segs+2)
	LG.draw(mesh, x,y)
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
		LG.setShader(shaderMakeMask)
		if args.clear then  LG.clear(0, 0, 0, 1)  end
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
		LG.setShader(shaderMain)

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

local function loadArtFile(path)
	print("Loading "..path.."...")
	LG.reset()

	local context   = Context()
	context.art     = Art()
	context.path    = path
	context.canvasW = LG.getWidth()
	context.canvasH = LG.getHeight()

	for line in io.lines(context.path) do
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

local function tryLoadingTheArtFile()
	local art = loadArtFile(thePathIn)

	for i = 1, LG.getStackDepth() do
		LG.pop()
	end
	LG.reset()

	if not art then  return  end

	if theArt then  theArt.canvas:release()  end
	theArt = art

	love.window.setTitle(string.format(
		"%s (%dx%d) - Art Command",
		thePathIn:gsub("^.*[/\\]", ""),
		theArt.canvas:getWidth(), theArt.canvas:getHeight()
	))
end



function love.load(args, rawArgs)
	io.stdout:setvbuf("no")
	io.stderr:setvbuf("no")

	-- Parse arguments.
	_G.arg             = nil -- Aaarrrgh!!!
	local parseOptions = true
	local i            = 1

	while args[i] do
		local arg = args[i]
		print(i, arg)

		if not (parseOptions and arg:find"^%-") then
			if arg       == "" then  error("[Options] Path cannot be empty.", 0)  end
			if thePathIn ~= "" then  error("[Options] Path already specified.", 0)  end
			thePathIn = arg

		elseif arg == "--" then
			parseOptions = false

		elseif arg == "--out" then
			if thePathOut ~= "" then  error("[Options] Ouput path already specified.", 0)  end
			thePathOut = args[i+1] or error("[Options] Missing path after "..arg..".", 0)
			i          = i + 1

		else
			error("[Options] Unknown option "..arg..".", 0)
		end

		i = i + 1
	end

	if thePathIn == "" then
		thePathIn     = "art/test.artcmd"
		thePathOut    = ""
		thePathIsTest = true
	end

	-- Load stuff.
	hotLoader = require"hotLoader"

	shaderMain = LG.newShader[[//GLSL
		uniform bool useColorTexture;
		uniform Image colorTexture;
		uniform vec4 colorTextureLayout; // {directionX,directionY, scaleX,scaleY}

		// vec2 rotate(vec2 v, vec2 angle) {
		// 	return vec2(v.x*angle.x-v.y*angle.y, v.x*angle.y+v.y*angle.x);
		// }
		vec2 rotateCcw(vec2 v, vec2 angle) {
			return vec2(v.x*angle.x+v.y*angle.y, -v.x*angle.y+v.y*angle.x);
		}

		vec4 effect(vec4 loveColor, Image tex, vec2 texPos, vec2 screenPosPx) {
			vec4 color = Texel(tex, texPos);

			if (useColorTexture) {
				vec2 colorTexPos = texPos;

				colorTexPos  = colorTexPos*2 - vec2(1, 1); // Center.
				colorTexPos  = rotateCcw(colorTexPos, colorTextureLayout.xy); // Rotate.
				colorTexPos /= colorTextureLayout.zw; // Scale.
				colorTexPos  = (colorTexPos + vec2(1, 1)) * .5;

				color *= Texel(colorTexture, colorTexPos);

			} else {
				color *= loveColor;
			}

			return color;
		}
	]]
	shaderMakeMask = LG.newShader[[//GLSL  @Cleanup: Merge with shaderMain so colorTexture etc. work in masks too.
		vec4 effect(vec4 loveColor, Image tex, vec2 texPos, vec2 screenPosPx) {
			vec4 color = Texel(tex, texPos) * loveColor;
			return vec4((color.r+color.g+color.b)/3, 0, 0, color.a); // One way of doing it.
		}
	]]
	shaderApplyMask = LG.newShader[[//GLSL
		uniform Image mask;

		vec4 effect(vec4 loveColor, Image tex, vec2 texPos, vec2 screenPosPx) {
			vec4 pixel      = Texel(tex , texPos);
			vec4 maskPixel  = Texel(mask, texPos);
			pixel.rgba     *= maskPixel.r;
			return pixel;
		}
	]]

	--[[ @Cleanup
	shaderMain      = LG.newShader("src/shaders/main.gl")
	shaderMakeMask  = LG.newShader("src/shaders/makeMask.gl")
	shaderApplyMask = LG.newShader("src/shaders/applyMask.gl")
	if DEV then
		hotLoader.monitor("src/shaders/main.gl", function(path)  shaderMain      = LG.newShader("src/shaders/main.gl"     ) ; tryLoadingTheArtFile()  end)
		hotLoader.monitor("src/shaders/main.gl", function(path)  shaderMakeMask  = LG.newShader("src/shaders/makeMask.gl" ) ; tryLoadingTheArtFile()  end)
		hotLoader.monitor("src/shaders/main.gl", function(path)  shaderApplyMask = LG.newShader("src/shaders/applyMask.gl") ; tryLoadingTheArtFile()  end)
	end
	]]

	A.images.rectangle = newImageUsingPalette({
		"xx",
		"xx",
	}, {x={1,1,1,1}})

	A.images.checker = newImageUsingPalette({
		"xxxx____",
		"xxxx____",
		"xxxx____",
		"xxxx____",
		"____xxxx",
		"____xxxx",
		"____xxxx",
		"____xxxx",
	}, {x={1,1,1,0}, _={1,1,1,1}})
	A.images.checker:setWrap("repeat", "repeat")
	A.quads.checker = LG.newQuad(0,0, 8,8, A.images.checker:getDimensions())

	if love.system.getOS() == "Windows" then -- hotLoader works best on Windows.
		hotLoader.allowExternalPaths(true)
		hotLoader.monitor(thePathIn, function(path)
			tryLoadingTheArtFile()
		end)
		tryLoadingTheArtFile()
	end
end



local function fixFormatAndDemultiplyAlpha(imageData16)
	local iw,ih      = imageData16:getDimensions()
	local imageData8 = love.image.newImageData(iw,ih, "rgba8")
	local pointer16  = require"ffi".cast("uint16_t*", imageData16:getFFIPointer())
	local pointer8   = require"ffi".cast("uint8_t*" , imageData8 :getFFIPointer())

	for i = 0, 4*iw*ih-1, 4 do
		pointer8[i  ] = (pointer16[i  ] * 255) / pointer16[i+3]
		pointer8[i+1] = (pointer16[i+1] * 255) / pointer16[i+3]
		pointer8[i+2] = (pointer16[i+2] * 255) / pointer16[i+3]
		pointer8[i+3] = pointer16[i+3] / 257
	end

	return imageData8
end

-- success, error = writeFile( path, dataString )
local function writeFile(path, data)
	local file, err = io.open(path, "wb")
	if not file then  return false, err  end

	file:write(data)
	file:close()

	return true
end

function love.keypressed(key)
	if key == "escape" then
		love.event.quit()

	elseif key == "s" and love.keyboard.isDown("lctrl","rctrl") then
		if not theArt then  return  end

		if thePathIsTest then
			local pathOut = "output.png"
			print("Saving "..pathOut.."...")
			fixFormatAndDemultiplyAlpha(theArt.canvas:newImageData()):encode("png", pathOut)
			print("Saving "..pathOut.."... done!")

		else
			local pathOut = (thePathOut ~= "") and thePathOut or thePathIn:gsub("%.[^.]+$", "")..".png"
			if pathOut == thePathIn then
				print("Error: Input and output paths are the same: "..pathOut)
				return
			end

			print("Saving "..pathOut.."...")

			local fileData = fixFormatAndDemultiplyAlpha(theArt.canvas:newImageData()):encode("png")
			local ok, err  = writeFile(pathOut, fileData:getString())

			if not ok then
				print("Error: "..err)
				return
			end

			print("Saving "..pathOut.."... done!")
		end
	end
end



local theModtime       = -1/0
local modtimeCheckTime = 0

function love.update(dt)
	if love.system.getOS() == "Windows" then -- hotLoader works best on Windows.
		hotLoader.update(dt)

	else
		modtimeCheckTime = modtimeCheckTime - dt

		if modtimeCheckTime < 0 then
			modtimeCheckTime = 0.20

			local info    = love.filesystem.getInfo(thePathIn, "file")
			local modtime = info and info.modtime

			if modtime and modtime ~= theModtime then
				theModtime = modtime
				tryLoadingTheArtFile()
			end
		end
	end
end



function love.draw()
	if not LG.isActive() then  return  end

	local ww,wh = LG.getDimensions()

	LG.reset()
	LG.clear(.4, .4, .4, 1)

	A.quads.checker:setViewport(0,0, ww,wh)
	LG.setColor(.6, .6, .6)
	LG.draw(A.images.checker, A.quads.checker)

	if theArt then
		if theArt.backdrop[4] > 0 then
			LG.setColor(theArt.backdrop)
			LG.rectangle("fill", 0,0, ww,wh)
		end

		LG.translate(math.floor(ww/2),math.floor(wh/2))
		LG.scale(theArt.zoom)
		LG.translate(-math.floor(ww/2),-math.floor(wh/2))

		local w = theArt.canvas:getWidth()
		local h = theArt.canvas:getHeight()
		local x = math.max(math.floor((ww-w)/2), 0)
		local y = math.max(math.floor((wh-h)/2), 0)
		LG.setColor(0, 0, 0)
		LG.rectangle("fill", x-1,y-1, w+2,1)
		LG.rectangle("fill", x-1,y+h, w+2,1)
		LG.rectangle("fill", x-1,y-1, 1,h+2)
		LG.rectangle("fill", x+w,y-1, 1,h+2)

		LG.setColor(1, 1, 1)
		LG.setBlendMode("alpha", "premultiplied")
		LG.draw(theArt.canvas, x,y)

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

	LG.present()
end



function love.errorhandler(err)
	print(debug.traceback(tostring(err), 2))
end
love.errhand = love.errorhandler



function love.run()
	love.load(love.arg.parseGameArguments(arg), arg)
	love.timer.step()

	return function()
		love.event.pump()

		for name, a,b,c,d,e,f in love.event.poll() do
			if name == "quit" and not (love.quit and love.quit()) then
				return a or 0
			end
			love.handlers[name](a,b,c,d,e,f)
		end

		love.update(love.timer.step())
		love.draw()
		love.timer.sleep(.001)
	end
end


