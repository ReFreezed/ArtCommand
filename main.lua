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



local function printFileMessage(path, ln, s, ...)
	print(string.format("%s:%d: "..s, path, ln, ...))
end

local function printFileError(path, ln, s, ...)
	print(string.format("%s:%d: Error: "..s, path, ln, ...))
end

local function printFileWarning(path, ln, s, ...)
	print(string.format("%s:%d: Warning: "..s, path, ln, ...))
end



local function StackEntry()
	return {
		funcs = {--[[ [funcName1]=funcInfo, ... ]]},
		vars  = {--[[ [varName1 ]=value   , ... ]]},
	}
end

local function findInStack(stack, member, k)
	for i = #stack, 1, -1 do
		local v = stack[i][member][k]
		if v ~= nil then  return v  end
	end
	return nil -- Not found!
end

local function findNextUnnamedArgument(argInfos, orderN, vType)
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

local function loadArtFile(path)
	print("Loading "..path.."...")
	LG.reset()

	local art = {
		canvas   = nil,
		backdrop = {0,0,0,0},
	}

	local lines      = {}
	local maskCanvas = nil

	local canvasW = LG.getWidth()
	local canvasH = LG.getHeight()
	local msaa    = 1
	local doRound = false

	for line in love.filesystem.lines(path) do
		line = line:gsub("^%s+", "")
		table.insert(lines, (line:find"^#" and "" or line))
	end

	local function ensureCanvas()
		if art.canvas then  return  end

		art.canvas = LG.newCanvas(canvasW,canvasH, {msaa=(msaa > 1 and msaa or nil)})
		maskCanvas = LG.newCanvas(canvasW,canvasH, {format="r8"})

		shaderMain:send("useMask", false)
		shaderMain:send("mask", maskCanvas)

		LG.setCanvas(art.canvas)
		LG.setShader(shaderMain)
	end

	local function maybeRound(x)
		return doRound and math.floor(x+.5) or x
	end

	-- nextLineNumber|nil = processCommandLine( lineNumber, line, stack )
	local function processCommandLine(ln, line, stack)
		if line == "" then  return ln+1  end

		local command, argsStr = parseCommand(line)
		if not command then  return printFileError(path, ln, "Bad line format: %s", line)  end

		--
		-- Function declaration.
		--
		if command == "func" then
			local funcName, funcArgsStr = argsStr:match"(%S+)(.*)"
			if not funcName             then  return printFileError(path, ln, "Missing function name after 'func'.")  end
			if not funcName:find"^%a+$" then  return printFileError(path, ln, "Bad function name '%s'. Must only contain letters.", funcName)  end -- @UX: Not a good rule. Maybe require space after the command?
			if not funcName:find"^%u"   then  return printFileError(path, ln, "Bad function name '%s'. Must start with an uppercase letter.", funcName)  end

			if stack[#stack].funcs[funcName] then  printFileWarning(path, ln, "Duplicate function '%s' in the same scope. Replacing.", funcName)  end

			local funcInfo                = {bodyLn1=ln+1, bodyLn2=0, --[[argName1, ...]]}
			stack[#stack].funcs[funcName] = funcInfo

			for funcArg in funcArgsStr:gmatch"%S+" do
				if funcArg:find"^#" then  break  end
				table.insert(funcInfo, funcArg)
			end

			local endLn = ln
			local depth = 1

			while true do
				endLn = endLn + 1
				line  = lines[endLn]
				if not line then  return printFileError(path, ln, "Missing end of function '%s'.", funcName)  end

				command = (line == "") and "" or parseCommand(line)
				if not command then  return printFileError(path, endLn, "Bad line format: %s", line)  end

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
			return printFileError(path, ln, "@Incomplete: Command '%s'.", command)

		--
		-- Function call.
		--
		elseif command:find"^%u" then
			local funcInfo = findInStack(stack, "funcs", command)
			if not funcInfo then  return printFileError(path, ln, "No function '%s'.", command)  end

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
					argN = argN + 1

					if not funcInfo[argN] then
						return printFileError(path, ln, "Too many arguments.")

					-- Number: N
					elseif argStr:find"^%-?%.?%d" then
						local n = parseNumber(argStr)
						if not n then  return printFileError(path, ln, "Failed parsing number: %s", argStr)  end

						entry.vars[funcInfo[argN]] = n

					-- String: "S" OR "S OR 'S' OR 'S
					-- @Incomplete: Newlines.
					elseif argStr:find"^[\"']" then
						local q  = argStr:sub(1, 1)
						local i1 = pos + 1
						local i  = argsStr:find(q, i1, true)

						if    i
						then  entry.vars[funcInfo[argN]] = argsStr:sub(i1, i-1) ; ignoreBefore = i+1 ; if argsStr:find("^%S", i+1) then  return printFileError(path, ln, "Garbage after %s", argsStr:sub(pos+#k, i))  end
						else  entry.vars[funcInfo[argN]] = argsStr:sub(i1     ) ; break  end

					-- Variable: Var
					elseif argStr:find"^%u" then
						local var = argStr -- @Incomplete: Validate variable name.

						local v = stack[#stack].vars[var]--findInStack(stack, "vars", var) -- @Incomplete: Upvalues (which require lexical scope).
						if v == nil then  return printFileError(path, ln, "No variable '%s'.", var)  end

						entry.vars[funcInfo[argN]] = v

					else
						return printFileError(path, ln, "Failed parsing argument #%d: %s", argN, argStr)
					end

					print(funcInfo[argN], entry.vars[funcInfo[argN]])
				end
			end

			if funcInfo[argN+1] then
				return printFileError(path, ln, "Too few arguments. (Expected %d, got %d)", #funcInfo, argN)
			end

			-- Run function.
			local funcLn = funcInfo.bodyLn1
			table.insert(stack, entry)

			while funcLn <= funcInfo.bodyLn2 do -- @Robustness: Make sure we don't overshoot the body (which shouldn't happen unless there's a bug somewhere).
				funcLn = processCommandLine(funcLn, lines[funcLn], stack)
				if not funcLn then  return printFileMessage(path, ln, "in '%s' (%s:%d)", command, path, funcInfo.bodyLn1-1)  end
			end

			table.remove(stack)
			return ln+1
		end

		--
		-- Normal command.
		--
		local commandInfo = COMMANDS[command]
		if not commandInfo then  return printFileError(path, ln, "Bad command '%s'.", command)  end

		local args = {}

		for _, argInfo in ipairs(commandInfo) do
			args[argInfo[1]] = argInfo[2]
		end

		local argN         = 0
		local ignoreBefore = 1
		local orderN       = {number=0, string=0}
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
					args[argStr] = true

				-- Unset flag: !X
				elseif argStr:find"^!%l" then
					local k = argStr:sub(2)
					if type(args[k]) ~= "boolean" then  return printFileError(path, ln, "Argument '%s' is not a boolean.", k)  end -- @Incomplete: Handle the "any" type.

					args[k] = false

				-- Number: nameN OR N
				elseif argStr:find"^%l*%-?%.?%d" then
					local k, nStr = argStr:match"^(%l*)(.+)"

					-- Named.
					if k ~= "" then
						if not (type(args[k]) == "number" or args[k] == nil) then -- @Incomplete: Handle the "any" type better.
							return printFileError(path, ln, "Argument '%s' is not a number.", k)
						end

					-- Ordered.
					else
						local argInfo = findNextUnnamedArgument(commandInfo, orderN, "number")
						if not argInfo then  return printFileError(path, ln, "Too many unnamed arguments of type 'number'. (At: %s)", argStr)  end -- @UX: Better message if the command take no arguments of the type.
						k = argInfo[1]
					end

					if visited[k] then  return printFileError(path, ln, "Duplicate argument '%s'.", k)  end
					visited[k] = true

					local n, nKind = parseNumber(nStr)
					if not n then  return printFileError(path, ln, "Failed parsing number: %s", nStr)  end

					args[k] = n

				-- String: name"S" OR name"S OR name'S' OR name'S OR "S" OR "S OR 'S' OR 'S
				-- @Incomplete: Newlines.
				elseif argStr:find"^%l*[\"']" then
					local k, q = argStr:match"^(%l*)(.)"
					local i1   = pos + #k + 1
					local i    = argsStr:find(q, i1, true)

					-- Named.
					if k ~= "" then
						if not (type(args[k]) == "string" or args[k] == nil) then -- @Incomplete: Handle the "any" type better.
							return printFileError(path, ln, "Argument '%s' is not a string.", k)
						end

					-- Ordered.
					else
						local argInfo = findNextUnnamedArgument(commandInfo, orderN, "string")
						if not argInfo then  return printFileError(path, ln, "Too many unnamed arguments of type 'string'. (At: %s)", argStr)  end -- @UX: Better message if the command take no arguments of the type.
						k = argInfo[1]
					end

					if visited[k] then  return printFileError(path, ln, "Duplicate argument '%s'.", k)  end
					visited[k] = true

					if    i
					then  args[k] = argsStr:sub(i1, i-1) ; ignoreBefore = i+1 ; if argsStr:find("^%S", i+1) then  return printFileError(path, ln, "Garbage after %s", argsStr:sub(pos+#k, i))  end
					else  args[k] = argsStr:sub(i1     ) ; break  end

				-- Special case: Treat `set X` as `set "X"`.
				elseif command == "set" and argN == 1 and argStr:find"^%u" then
					orderN.string = 1
					args.var      = argStr -- @Incomplete: Validate variable name.

				-- Variable: nameVar OR Var
				elseif argStr:find"^%l*%u" then
					local k, var = argStr:match"^(%l*)(.+)" -- @Incomplete: Validate variable name.

					-- Named.
					if k ~= "" then

					-- Ordered.
					else
						local v = stack[#stack].vars[var]--findInStack(stack, "vars", var) -- @Incomplete: Upvalues (which require lexical scope).
						if v == nil then  return printFileError(path, ln, "No variable '%s'.", var)  end

						-- This might be confusing! We're using the type of the variable's value to assign an argument number.
						local argInfo = findNextUnnamedArgument(commandInfo, orderN, type(v))
						if not argInfo then  return printFileError(path, ln, "Too many unnamed arguments of type %s. (At: %s)", type(v), argStr)  end -- @UX: Better message if the command take no arguments of the type.
						k = argInfo[1]
					end

					if visited[k] then  return printFileError(path, ln, "Duplicate argument '%s'.", k)  end
					visited[k] = true

					local v = stack[#stack].vars[var]--findInStack(stack, "vars", var) -- @Incomplete: Upvalues (which require lexical scope).  @Speed: Duplicate work, sometimes.
					if v == nil then  return printFileError(path, ln, "No variable '%s'.", var)  end

					if not (type(args[k]) == type(v) or args[k] == nil) then -- @Incomplete: Handle the "any" type better.
						return printFileError(path, ln, "Argument '%s' is not a %s.", k, type(args[k]))
					end

					args[k] = v

				else
					return printFileError(path, ln, "Failed parsing argument #%d: %s", argN, argStr)
				end
			end
		end

		-- Settings, init.
		if command == "canvas" then
			if art.canvas then  return printFileError(path, ln, "Cannot use '%s' after drawing commands.", command)  end
			canvasW = (args.w >= 1) and args.w or LG.getWidth()
			canvasH = (args.h >= 1) and args.h or LG.getHeight()
			msaa    = args.aa^2

		-- Settings, dynamic.
		elseif command == "round" then
			doRound = true
		elseif command == "noround" then
			doRound = false

		-- State.
		elseif command == "push" then
			LG.push("all")
		elseif command == "pop" then
			if    love.graphics.getStackDepth() == 0
			then  printFileWarning(path, ln, "Too many 'pop' commands! Ignoring.")
			else  LG.pop()  end

		elseif command == "color" then
			LG.setColor(args.r, args.g, args.b, args.a)

		elseif command == "font" then
			LG.setNewFont(args.size)

		elseif command == "makemask" then
			ensureCanvas()
			LG.setCanvas(maskCanvas)
			LG.setShader(shaderMask)
			LG.clear(0, 0, 0, 1)
		elseif command == "mask" then
			ensureCanvas()
			shaderMain:send("useMask", true)
			LG.setCanvas(art.canvas)
			LG.setShader(shaderMain)
		elseif command == "nomask" then
			ensureCanvas()
			shaderMain:send("useMask", false)
			LG.setCanvas(art.canvas)
			LG.setShader(shaderMain)

		elseif command == "set" then
			if args.var   == ""  then  return printFileError(path, ln, "Missing variable name.")  end
			if args.value == nil then  return printFileError(path, ln, "Missing value to assign to '%s'.", args.var)  end
			stack[#stack].vars[args.var] = args.value

		elseif command == "origin" then
			LG.origin()
		elseif command == "move" then
			LG.translate(maybeRound(args.x), maybeRound(args.y))
		elseif command == "rotate" then
			LG.rotate(args.a)
		elseif command == "scale" then
			LG.scale(args.x, (args.y == 1/0 and args.x or args.y))

		-- Drawing.
		elseif command == "backdrop" then
			art.backdrop[1] = args.r
			art.backdrop[2] = args.g
			art.backdrop[3] = args.b
			art.backdrop[4] = args.a

		elseif command == "clear" then
			ensureCanvas()
			LG.clear(args.r, args.g, args.b, args.a)

		elseif command == "circle" then
			ensureCanvas()
			local segs = (args.segs >= 3) and args.segs or math.max(64, math.floor(args.r*TAU/10))
			if args.fill then  LG.circle("fill", maybeRound(args.x),maybeRound(args.y), args.r, segs)  end
			if args.line then  LG.circle("line", maybeRound(args.x),maybeRound(args.y), args.r, segs)  end
		elseif command == "rect" then
			ensureCanvas()
			if args.fill then  LG.rectangle("fill", maybeRound(args.x-args.ax*args.w),maybeRound(args.y-args.ay*args.h), maybeRound(args.w),maybeRound(args.h))  end
			if args.line then  LG.rectangle("line", maybeRound(args.x-args.ax*args.w),maybeRound(args.y-args.ay*args.h), maybeRound(args.w),maybeRound(args.h))  end
		elseif command == "text" then
			ensureCanvas()
			local font         = LG.getFont()
			local w, textLines = font:getWrap(args.text, 1/0)
			local h            = (#textLines-1) * math.floor(font:getHeight()*font:getLineHeight()) + font:getHeight()
			LG.print(args.text, maybeRound(args.x-args.ax*w),maybeRound(args.y-args.ay*h))

		else
			printFileWarning(path, ln, "@Incomplete: Command '%s'. Ignoring.", command)
		end

		return ln+1
	end

	local stack = {StackEntry()}
	local ln    = 1

	while lines[ln] do
		ln = processCommandLine(ln, lines[ln], stack)
		if not ln then  return nil  end
	end

	assert(#stack == 1)
	LG.setCanvas(nil)

	print("Loading "..path.."... done!")
	return art.canvas and art
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


