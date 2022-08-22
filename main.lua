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

local theCanvas  = nil
local shaderMain = nil
local shaderMask = nil
local checkerImage, checkerQuad



local function printFileError(path, ln, s, ...)
	print(string.format("%s:%d: Error: "..s, path, ln, ...))
end

local function printFileWarning(path, ln, s, ...)
	print(string.format("%s:%d: Warning: "..s, path, ln, ...))
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
	-- Settings, init.
	aa = { {"n",1} },

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
	origin   = { },
	move     = { {"x",0},{"y",0} },
	rotate   = { {"a",0} },
	scale    = { {"x",1},{"y",1/0--[[=x]]} },

	-- Drawing.
	clear  = { {"r",0},{"g",0},{"b",0},{"a",1} },
	circle = { {"x",0},{"y",0}, {"r",10}, {"segs",0}, {"fill",true},{"line",false} },
	rect   = { {"x",0},{"y",0}, {"w",10},{"h",10}, {"ax",0},{"ay",0}, {"fill",true},{"line",false} },
	text   = { {"x",0},{"y",0}, {"text",""}, {"ax",0},{"ay",0} },
}

local function loadArtFile(path)
	print("Loading "..path.."...")
	LG.reset()

	local ln = 0

	local canvas     = nil
	local maskCanvas = nil
	local msaa       = 1
	local doRound    = false

	local function ensureCanvas()
		if canvas then  return  end

		canvas     = LG.newCanvas(LG.getWidth(),LG.getHeight(), {msaa=(msaa > 1 and msaa or nil)})
		maskCanvas = LG.newCanvas(LG.getWidth(),LG.getHeight(), {format="r8"})

		shaderMain:send("useMask", false)
		shaderMain:send("mask", maskCanvas)

		LG.setCanvas{ canvas--[[, stencil=true]] }
		LG.setShader(shaderMain)
	end

	local function maybeRound(x)
		return doRound and math.floor(x+.5) or x
	end

	for line in love.filesystem.lines(path) do
		line = line:gsub("^%s+", "")
		ln   = ln + 1

		if not (line == "" or line:find"^#") then
			local command, argsStr = line:match"^(%a+)(.*)"
			if not command then  return printFileError(path, ln, "Bad line format: %s", line)  end

			local commandInfo = COMMANDS[command]
			if not commandInfo then  return printFileError(path, ln, "Bad command '%s'.", command)  end

			local args = {}

			for _, argInfo in ipairs(commandInfo) do
				args[argInfo[1]] = argInfo[2]
			end

			local orderN       = 0
			local ignoreBefore = 1

			for pos, argStr in argsStr:gmatch"()(%S+)" do
				if pos < ignoreBefore then
					-- void

				-- Set flag: X
				elseif type(args[argStr]) == "boolean" then
					args[argStr] = true

				-- Unset flag: !X
				elseif argStr:find"^!" then
					local k = argStr:sub(2)
					if type(args[k]) ~= "boolean" then  return printFileError(path, ln, "Argument '%s' is not a boolean.", k)  end

					args[k] = false

				-- Named string: name"S" OR name"S OR name'S' OR name'S
				-- @Incomplete: Newlines.
				elseif argStr:find'^%a+"' then
					local k = argStr:match"^%a+"
					if type(args[k]) ~= "string" then  return printFileError(path, ln, "Argument '%s' is not a string.", k)  end

					local i = argsStr:find('"', pos+#k+1, true)

					if    i
					then  args[k] = argsStr:sub(pos+#k+1, i-1) ; ignoreBefore = i + 1
					else  args[k] = argsStr:sub(pos+#k+1     ) ; break  end

				elseif argStr:find"^%a+'" then
					local k = argStr:match"^%a+"
					if type(args[k]) ~= "string" then  return printFileError(path, ln, "Argument '%s' is not a string.", k)  end

					local i = argsStr:find("'", pos+#k+1, true)

					if    i
					then  args[k] = argsStr:sub(pos+#k+1, i-1) ; ignoreBefore = i + 1
					else  args[k] = argsStr:sub(pos+#k+1     ) ; break  end

				-- Named number: nameN
				elseif argStr:find"^%a+%-?%.?%d" then
					local k, nStr = argStr:match"^(%a+)(.+)"
					if type(args[k]) ~= "number" then  return printFileError(path, ln, "Argument '%s' is not a number.", k)  end

					local n, nKind = parseNumber(nStr)
					if not n then  return printFileError(path, ln, "Failed parsing number: %s", nStr)  end

					args[k] = n

				-- Ordered number: N
				elseif argStr:find"^%-?%.?%d" then
					orderN        = orderN + 1
					local argInfo = commandInfo[orderN]
					if not argInfo then  return printFileError(path, ln, "Too many ordered arguments. (At #%d: %s)", orderN, argStr)  end

					if type(argInfo[2]) ~= "number" then  return printFileError(path, ln, "Argument #%d (%s) is not a number.", orderN, argInfo[1])  end

					local n, nKind = parseNumber(argStr)
					if not n then  return printFileError(path, ln, "Failed parsing number: %s", argStr)  end

					args[argInfo[1]] = n

				else
					return printFileError(path, ln, "Failed parsing argument: %s", argStr)
				end
			end

			-- Settings, init.
			if command == "aa" then
				if canvas then  return printFileError(path, ln, "Cannot use '%s' after drawing commands.", command)  end
				msaa = args.n^2

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
				LG.setCanvas{ canvas--[[, stencil=true]] }
				LG.setShader(shaderMain)
			elseif command == "nomask" then
				ensureCanvas()
				shaderMain:send("useMask", false)
				LG.setCanvas{ canvas--[[, stencil=true]] }
				LG.setShader(shaderMain)

			elseif command == "origin" then
				LG.origin()
			elseif command == "move" then
				LG.translate(maybeRound(args.x), maybeRound(args.y))
			elseif command == "rotate" then
				LG.rotate(args.a)
			elseif command == "scale" then
				LG.scale(args.x, (args.y == 1/0 and args.x or args.y))

			-- Drawing.
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
				local font     = LG.getFont()
				local w, lines = font:getWrap(args.text, 1/0)
				local h        = (#lines-1) * math.floor(font:getHeight()*font:getLineHeight()) + font:getHeight()
				LG.print(args.text, maybeRound(args.x-args.ax*w),maybeRound(args.y-args.ay*h))

			else
				printFileWarning(path, ln, "@Incomplete: Command '%s'. Ignoring.", command)
			end
		end
	end

	LG.setCanvas(nil)

	print("Loading "..path.."... done!")
	return canvas
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
		if theCanvas then
			print("Saving output.png...")
			theCanvas:newImageData():encode("png", "output.png")
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
			theModtime   = modtime
			local canvas = loadArtFile(thePath)

			for i = 1, LG.getStackDepth() do
				LG.pop()
			end
			LG.reset()

			if canvas then
				if theCanvas then  theCanvas:release()  end
				theCanvas = canvas
			end
		end
	end
end



function love.draw()
	LG.clear(.4, .4, .4, 1)

	checkerQuad:setViewport(0,0, LG.getDimensions())
	LG.setColor(.6, .6, .6)
	LG.draw(checkerImage, checkerQuad)

	if theCanvas then
		-- LG.setBlendMode("alpha", "premultiplied")
		LG.setColor(1, 1, 1)
		LG.draw(theCanvas)
	end
end



function love.errorhandler(err)
	print(debug.traceback(tostring(err), 2))
end
love.errhand = love.errorhandler


