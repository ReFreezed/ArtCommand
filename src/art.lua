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

local DEFAULT_ART_SIZE = 500
local MAX_LOOPS        = 100000

local STOP_CONTINUE = nil
local STOP_ONE      = 1
local STOP_ALL      = 2

local WRAP_TO_LOVE_WRAP = {
	["clamp"]  = "clamp",
	["cut"]    = "clampzero",
	["repeat"] = "repeat",
	["mirror"] = "mirroredrepeat",
}
local CHANNEL_MAPPING_INDICES = {
	["r"] = 0,
	["g"] = 1,
	["b"] = 2,
	["a"] = 3,
	["0"] = 4,
	["1"] = 5,
}



local COMMANDS = {--[[
	command1 = { {name=argName1,value=defaultValue}, ... }, -- defaultValue=nil means the value can be any type.
	...
--]]}

local env = setmetatable({}, {__index=function(env, k)
	if k == "tau" then  return TAU  end
	error(k)
end})

for line in LF.lines"data/all.commands" do
	local protected, command, argInfosStr = line:gsub("#.*$", ""):match"^(!?)(%a+)%s*(.*)$"

	if command then
		COMMANDS[command] = {protected=(protected=="!")}

		for k, vStr in argInfosStr:gmatch"(%a+)=(%S+)" do
			-- Alias.
			if vStr:find("&", 1, true) then
				COMMANDS[command][k] = {}
				for argName in vStr:gmatch"%a+" do
					table.insert(COMMANDS[command][k], argName)
				end
			-- Argument.
			else
				vStr    = vStr:gsub("%b()$", "")
				local v = assert(loadstring("return "..vStr, nil, "t", env))()
				table.insert(COMMANDS[command], {name=k,value=v})
			end
		end

		if DEV and not itemWith1(COMMANDS[command], "name", "debug") then
			table.insert(COMMANDS[command], {name="debug",value=false})
		end
	end
end

local SCOPE_ENTER_COMMANDS = {
	["do"]   = true,
	["for"]  = true,
	["fori"] = true,
	["func"] = true,
	["if"]   = true,
	-- There's only one scope exit command: "end".
}



local function Art()return{
	canvas = nil,

	backdrop = {0,0,0,0},

	zoom       = 1.0,
	zoomFilter = false,
}end

local function Context()return{
	art = nil,

	path    = "",
	isLocal = false,
	source  = "",

	commands = {}, -- Initially a copy of COMMANDS.
	readonly = {--[[ [varName1]=variable, ... ]]}, -- variable = {token=declaredAtToken, value=value}

	scopeStack = {--[[ scopeStackEntry1, ... ]]},
	callStack  = {--[[ [funcInfo1]=true, ... ]]}, -- @Cleanup: Bad name. Not a stack - just bookkeeping.
	gfxStack   = {--[[ gfxState1, ... ]]},

	gfxState = nil, -- (including love.graphics transform)

	workCanvas1 = nil,
	workCanvas2 = nil,

	images      = {--[[ [path1]=image|canvas, ... ]]},
	buffers     = {--[[ [name1]=image, ... ]]},
	fontsByPath = {--[[ [path1]=font, ... ]]}, -- Image fonts.
	fontsBySize = {--[[ [path1]={[size1]=font,...}, ... ]]}, -- Vector fonts.

	points = {{x=0,y=0, a=0/0,b=0/0, s=""}},

	reportedWarnings = {--[[ [idObject1]=true, ... ]]},

	rng = nil, -- RandomGenerator

	-- Settings.
	canvasWidth  = DEFAULT_ART_SIZE,
	canvasHeight = DEFAULT_ART_SIZE,
	canvasMsaa   = 1,
}end

local function FunctionInfo()return{
	token     = nil,
	name      = "",
	arguments = {},
	tokens    = {},
}end

local function ScopeStackEntry()return{
	functions = {--[[ [funcName1]=functionInfo, ... ]]},
	variables = {--[[ [varName1 ]=variable    , ... ]]}, -- variable = {token=declaredAtToken, value=value}

	callerToken    = nil,
	calledFunction = nil, -- FunctionInfo
}end

local function GfxState()return{
	canvas          = nil, -- @Cleanup: Store bufferName instead.
	useMask         = "",
	maskAlphaMode   = false,
	blendMode       = "alpha", -- "alpha" | "replace" | "screen" | "add" | "subtract" | "multiply" | "lighten" | "darken"
	channelMapping  = {0,1,2,3},
	writeToChannels = {true,true,true,true},

	-- Color.
	colorMode           = "flatcolor", -- "flatcolor" | "gradient" | "texture"
	flatColor           = {1,1,1,1},
	gradient            = {--[[ r1,g1,b1,a1, ... ]]},
	colorTexture        = nil,
	colorTextureRadial  = false,
	colorTextureSmooth  = false,
	colorTextureFit     = true, -- Used with colorTextureRadial.
	colorTextureScaleX  = 1.0,
	colorTextureScaleY  = 1.0,
	colorTextureAngle   = 0.0,
	colorTextureOffsetX = 0.0,
	colorTextureOffsetY = 0.0,
	colorTextureBuffer  = "",
	colorTextureWrapX   = "clamp",
	colorTextureWrapY   = "clamp",

	-- Font.
	font = A.fonts.artDefault,
}end

local function resetGfxState(gfxState)
	gfxState.canvas          = nil
	gfxState.useMask         = ""
	gfxState.maskAlphaMode   = false
	gfxState.blendMode       = "alpha"
	updateVec4(gfxState.channelMapping , 0,1,2,3)
	updateVec4(gfxState.writeToChannels, true,true,true,true)

	gfxState.colorMode = "flatcolor"
	updateVec4(gfxState.flatColor, 1,1,1,1)
	table.clear(gfxState.gradient)
	gfxState.colorTexture        = nil -- @Memory: Release image. (Consider gfxStack!)
	gfxState.colorTextureRadial  = false
	gfxState.colorTextureSmooth  = false
	gfxState.colorTextureFit     = true
	gfxState.colorTextureScaleX  = 1
	gfxState.colorTextureScaleY  = 1
	gfxState.colorTextureAngle   = 0
	gfxState.colorTextureOffsetX = 0
	gfxState.colorTextureOffsetY = 0
	gfxState.colorTextureBuffer  = ""
	gfxState.colorTextureWrapX   = "clamp"
	gfxState.colorTextureWrapY   = "clamp"

	gfxState.font = A.fonts.artDefault
end

local function copyGfxState(gfxState)return{
	canvas          = gfxState.canvas,
	useMask         = gfxState.useMask,
	maskAlphaMode   = gfxState.maskAlphaMode,
	blendMode       = gfxState.blendMode,
	channelMapping  = {unpack(gfxState.channelMapping)},
	writeToChannels = {unpack(gfxState.writeToChannels)},

	colorMode           = gfxState.colorMode,
	flatColor           = {unpack(gfxState.flatColor)},
	gradient            = {unpack(gfxState.gradient)},
	colorTexture        = gfxState.colorTexture,
	colorTextureRadial  = gfxState.colorTextureRadial,
	colorTextureSmooth  = gfxState.colorTextureSmooth,
	colorTextureFit     = gfxState.colorTextureFit,
	colorTextureScaleX  = gfxState.colorTextureScaleX,
	colorTextureScaleY  = gfxState.colorTextureScaleY,
	colorTextureAngle   = gfxState.colorTextureAngle,
	colorTextureOffsetX = gfxState.colorTextureOffsetX,
	colorTextureOffsetY = gfxState.colorTextureOffsetY,
	colorTextureBuffer  = gfxState.colorTextureBuffer,
	colorTextureWrapX   = gfxState.colorTextureWrapX,
	colorTextureWrapY   = gfxState.colorTextureWrapY,

	font = gfxState.font,
}end

local function moveGfxState(fromGfxState, toGfxState)
	toGfxState.canvas          = fromGfxState.canvas
	toGfxState.useMask         = fromGfxState.useMask
	toGfxState.maskAlphaMode   = fromGfxState.maskAlphaMode
	toGfxState.blendMode       = fromGfxState.blendMode
	toGfxState.channelMapping  = fromGfxState.channelMapping
	toGfxState.writeToChannels = fromGfxState.writeToChannels

	toGfxState.colorMode           = fromGfxState.colorMode
	toGfxState.flatColor           = fromGfxState.flatColor
	toGfxState.gradient            = fromGfxState.gradient
	toGfxState.colorTexture        = fromGfxState.colorTexture
	toGfxState.colorTextureRadial  = fromGfxState.colorTextureRadial
	toGfxState.colorTextureSmooth  = fromGfxState.colorTextureSmooth
	toGfxState.colorTextureFit     = fromGfxState.colorTextureFit
	toGfxState.colorTextureScaleX  = fromGfxState.colorTextureScaleX
	toGfxState.colorTextureScaleY  = fromGfxState.colorTextureScaleY
	toGfxState.colorTextureAngle   = fromGfxState.colorTextureAngle
	toGfxState.colorTextureOffsetX = fromGfxState.colorTextureOffsetX
	toGfxState.colorTextureOffsetY = fromGfxState.colorTextureOffsetY
	toGfxState.colorTextureBuffer  = fromGfxState.colorTextureBuffer
	toGfxState.colorTextureWrapX   = fromGfxState.colorTextureWrapX
	toGfxState.colorTextureWrapY   = fromGfxState.colorTextureWrapY

	toGfxState.font = fromGfxState.font
end



local artFilesBeingLoaded     = {--[[ [path1]=recursionDepth, ... ]]}
local artFileRecursionAllowed = {--[[ [path1]=count|nil, ... ]]}



local function printCallStack(context, pos)
	local relevantEntries = {}

	for i = #context.scopeStack, 1, -1 do
		if context.scopeStack[i].callerToken then
			table.insert(relevantEntries, context.scopeStack[i])
		end
	end

	print("Stack traceback:")

	printf("\t%s:%d%s",
		context.path,
		getLineNumber(context.source, pos),
		relevantEntries[1] and " ("..relevantEntries[1].calledFunction.name..")" or ""
	)

	for i, entry in ipairs(relevantEntries) do
		printf("\t%s:%d%s",
			context.path,
			getLineNumber(context.source, entry.callerToken.position),
			relevantEntries[i+1] and " ("..relevantEntries[i+1].calledFunction.name..")" or ""
		)
	end
end

local function parseError(context, pos, s, ...)
	printFileErrorAt(context.path, context.source, pos, s, ...)
	printCallStack(context, pos)
end
local function parseWarning(context, pos, s, ...)
	printFileWarningAt(context.path, context.source, pos, s, ...)
end

-- position = getTokenPosition( context, token|nil )
local function getTokenPosition(context, tok)
	return (tok and tok.position) or context.source:find"%S%s*$" or #context.source
end

-- tokenError       ( context, token=atEnd, format, ... )
-- tokenErrorNoStack( context, token=atEnd, format, ... )
-- tokenWarning     ( context, token=atEnd, format, ... )
-- tokenMessage     ( context, token=atEnd, format, ... )
local function tokenError(context, tok, s, ...)
	local pos = getTokenPosition(context, tok)
	printFileErrorAt(context.path, context.source, pos, s, ...)
	printCallStack(context, pos)
end
local function tokenErrorNoStack(context, tok, s, ...)  printFileErrorAt  (context.path, context.source, getTokenPosition(context, tok), s, ...)  end
local function tokenWarning     (context, tok, s, ...)  printFileWarningAt(context.path, context.source, getTokenPosition(context, tok), s, ...)  end
local function tokenMessage     (context, tok, s, ...)  printFileMessageAt(context.path, context.source, getTokenPosition(context, tok), s, ...)  end



-- object|nil, stackIndex = findInStack( context, member, currentPosition, key )
local function findInStack(context, member, pos, k)
	for i = #context.scopeStack, 1, -1 do
		local obj = context.scopeStack[i][member][k]
		if obj and obj.token.position < pos then  return obj, i  end
	end
	return nil -- Not found!
end



-- gfxStateSetCanvas( context, canvas=main )
local function gfxStateSetCanvas(context, canvas)
	context.gfxState.canvas = canvas or context.art.canvas
end



-- applyCanvas( context, shader=main )
local function applyCanvas(context, shader)
	shader = shader or A.shaders.main

	local gfxState     = context.gfxState
	local makeMaskMode = (gfxState.canvas:getFormat() == "r16")

	shaderSend(shader, "makeMaskMode", makeMaskMode)

	if gfxState.useMask == "" or makeMaskMode then
		shaderSend(shader, "useMask", false)
	else
		local bufCanvas = context.buffers[gfxState.useMask]
		shaderSend    (shader, "useMask"      , true)
		shaderSend    (shader, "mask"         , bufCanvas)
		shaderSendVec2(shader, "maskSize"     , bufCanvas:getDimensions())
		shaderSend    (shader, "maskAlphaMode", gfxState.maskAlphaMode)
	end

	LG.setCanvas(gfxState.canvas)
	LG.setShader(shader.shader)
end

-- applyColor( context, shader=main, shapeToDraw, relativeShapeWidth,relativeShapeHeight )
-- shapeToDraw = "rectangle" | "circle"
local function applyColor(context, shader, shape, w,h)
	shader = shader or A.shaders.main
	w      = math.abs(w)
	h      = math.abs(h)

	local gfxState = context.gfxState

	LG.setBlendMode(gfxState.blendMode, "premultiplied")
	LG.setColorMask(unpack(gfxState.writeToChannels))
	shaderSendVec4(shader, "channelMapping", unpack(gfxState.channelMapping))

	if gfxState.colorMode == "flatcolor" then
		local r,g,b,a = unpack(gfxState.flatColor)
		LG.setColor(r*a, g*a, b*a, a)
		shaderSend(shader, "useColorTexture", false)
		return
	end

	local texture      = gfxState.colorTexture
	local radial       = false
	local smooth       = false
	local sx           = 1 -- colorTextureLayout
	local sy           = 1 -- colorTextureLayout
	local dirX         = 1 -- colorTextureLayout
	local dirY         = 0 -- colorTextureLayout
	local radialOffset = 1
	local preSx        = 1
	local preSy        = 1
	local postSx       = 1
	local postSy       = 1

	if gfxState.colorMode == "gradient" then
		if not texture then
			local grad          = gfxState.gradient
			local pixelRowChars = {}
			local palette       = {}

			for i = 1, #grad/4 do
				local c    = string.char(i-1)
				palette[c] = {grad[i*4-3], grad[i*4-2], grad[i*4-1], grad[i*4]}
				table.insert(pixelRowChars, c)
			end

			local pixelRows       = {table.concat(pixelRowChars)}
			texture               = newImageUsingPalette(pixelRows, palette)
			gfxState.colorTexture = texture
		end

		if gfxState.colorTextureRadial then
			local iw     = texture:getWidth()
			local scale  = (gfxState.colorTextureFit and math.min(h/w, 1)) or (shape == "rectangle" and math.sqrt(w^2+h^2)/w) or math.max(h/w, 1)
			radial       = true
			sx           = gfxState.colorTextureScaleX * iw/(iw-1) * scale
			sy           = gfxState.colorTextureScaleY * iw/(iw-1) * scale * w/h
			radialOffset = .5/iw

		else
			-- When shape="rectangle" and the gradient scale is 1, one edge of the
			-- gradient should touch one corner, and the other edge should touch
			-- the opposite, no matter the gradient's angle.
			local angle                  = gfxState.colorTextureAngle
			local scaledAngle            = math.atan2(math.sin(angle)*h/w, math.cos(angle))
			local angleCompensationScale = (shape == "rectangle") and math.abs(math.sin(scaledAngle))+math.abs(math.cos(scaledAngle)) or 1

			local iw = texture:getWidth()
			sx       = gfxState.colorTextureScaleX * iw/(iw-1) * angleCompensationScale -- colorTextureScaleY isn't used.

			dirX = math.cos(scaledAngle)
			dirY = math.sin(scaledAngle)
		end

		smooth = gfxState.colorTextureSmooth
		texture:setWrap("clamp")

	elseif gfxState.colorMode == "texture" then
		texture = context.buffers[gfxState.colorTextureBuffer] or error(gfxState.colorTextureBuffer)

		local iw,ih = texture:getDimensions()
		local angle = gfxState.colorTextureAngle
		local scale = math.max((h/w)*(iw/ih), 1) -- Make the texture fill the shape (unrotated) by default.

		if gfxState.colorTextureFit then
			-- Consider the rotation when filling.
			-- @Incomplete: Handle shape=="circle".
			local normalizedW  = 1 -- The base unit.
			local normalizedH  = h/w
			local normalizedIw = scale
			local normalizedIh = scale * ih/iw
			local boundsW      = normalizedW*math.abs(math.cos(angle)) + normalizedH*math.abs(math.sin(angle))
			local boundsH      = normalizedW*math.abs(math.sin(angle)) + normalizedH*math.abs(math.cos(angle))
			scale              = scale * math.max(boundsW/normalizedIw, boundsH/normalizedIh)
		end

		dirX = math.cos(angle)
		dirY = math.sin(angle)

		preSx = scale
		preSy = scale * w/h

		postSx = gfxState.colorTextureScaleX
		postSy = gfxState.colorTextureScaleY * ih/iw

		texture:setWrap(WRAP_TO_LOVE_WRAP[gfxState.colorTextureWrapX], WRAP_TO_LOVE_WRAP[gfxState.colorTextureWrapY])

	else
		error(gfxState.colorMode)
	end

	shaderSend    (shader, "useColorTexture"         , true)
	shaderSend    (shader, "colorTexture"            , texture)
	shaderSend    (shader, "colorTextureRadial"      , radial)
	shaderSend    (shader, "colorTextureSmooth"      , smooth)
	shaderSendVec4(shader, "colorTextureLayout"      , sx,sy, dirX,dirY)
	shaderSend    (shader, "colorTextureRadialOffset", radialOffset)
	shaderSendVec2(shader, "colorTextureOffset"      , gfxState.colorTextureOffsetX,gfxState.colorTextureOffsetY)
	shaderSendVec4(shader, "colorTextureScale"       , preSx,preSy, postSx,postSy)
	shaderSend    (shader, "gradientSize"            , #gfxState.gradient/4)
end



local function ensureCanvasAndInitted(context)
	if context.art.canvas then  return  end

	local settingsCanvas = {
		format = "rgba16",
		msaa   = (context.canvasMsaa > 1 and context.canvasMsaa^2 or nil),
	}
	context.art.canvas   = LG.newCanvas(context.canvasWidth,context.canvasHeight, settingsCanvas)
	context.workCanvas1  = LG.newCanvas(context.canvasWidth,context.canvasHeight, {format="rgba16"}) -- No MSAA!
	context.workCanvas2  = LG.newCanvas(context.canvasWidth,context.canvasHeight, {format="rgba16"})

	gfxStateSetCanvas(context, nil)
end



local function validateVariableName(context, tokForError, term, varName)
	if not varName:find"^%w+$" then  tokenError(context, tokForError, "Bad %s '%s'. Must only contain alphanumeric characters.", term, varName) ; return false  end
	if not varName:find"^%u"   then  tokenError(context, tokForError, "Bad %s '%s'. Must start with an uppercase letter."      , term, varName) ; return false  end
	return true
end

local function tokenize(context, path)
	local s       = context.source
	local pos     = 1
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

			if lastTok and lastTok.type ~= "linebreak" then
				table.insert(tokens, {position=startPos, type="linebreak"})
			end

		-- Declaration: Name=
		-- Assignment:  Name^=
		-- (Declaration is same as `set  Name`.)
		-- (Assignment  is same as `setx Name`.)
		elseif s:find("^%a%w*[ \t]*^?=", pos) then
			local namePos,name, punctPos,punct, _pos = s:match("^()(%w+)%s*()(^?=)()", pos)
			pos = _pos

			table.insert(tokens, {position=punctPos, type="name"    , value=(punct == "^=" and "setx" or "set"), hasAttachment=false}) -- @UX: Fix error messages mentioning 'set' or 'setx'.
			table.insert(tokens, {position=namePos , type="username", value=name, negated=false})

			if not validateVariableName(context, getLast(tokens), "variable name", name) then  return nil  end

		-- Name: name OR +name OR -name
		elseif s:find("^[-+]?%l", pos) then
			local mod, namePos,name, _pos = s:match("^([-+]?)()(%l+)()", pos)
			pos = _pos

			table.insert(tokens, {position=namePos, type="name", value=name, hasAttachment=false})

			if mod == "" then
				gotName = true

			else
				tokens[#tokens].hasAttachment = true
				table.insert(tokens, {position=startPos, type="literal", value=(mod=="+")})

				if s:find("^[^%s,;#]", pos) and not s:find("^[-+]%l", pos) then
					parseWarning(context, pos, "Value right after '%s%s' does not belong to the argument.", mod, name)
				end
			end

		-- Username: Name OR -Name
		-- @Incomplete: +Name
		elseif s:find("^%-?%u", pos) then
			local negate, namePos,name, _pos = s:match("^(%-?)()(%u%w*)()", pos)
			pos = _pos

			table.insert(tokens, {position=namePos, type="username", value=name, negated=(negate=="-")})

			if lastWasName and lastTok.type == "name" then  lastTok.hasAttachment = true  end

		-- Number: N OR +N OR -N
		elseif s:find("^[-+]?%.?%d", pos) then
			local nStr, _pos = s:match("^%+?([-+.%w]*%%?)()", pos)
			pos = _pos
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
				else
					break
				end
			end

			table.insert(tokens, {position=startPos, type="literal", value=table.concat(buffer)})

			if not s:find("^\n", pos) then
				pos = pos + 1 -- '"' OR "'"
			end

			if lastWasName and lastTok.type == "name" then  lastTok.hasAttachment = true  end

		-- Brackets: (lua_expression) OR {lua_table_constructor}
		elseif s:find("^[({]", pos) then
			local i1    = pos
			local pat   = s:find("^%(", pos) and "[-\"'%[\n%w_()]" or "[-\"'%[\n%w_{}]"
			local depth = 1

			while true do
				-- Simplified Lua parsing, o'hoy!
				pos          = s:find(pat, pos+1)
				local luaPos = pos

				if not pos or s:find("^\n", pos) then
					return (parseError(context, startPos, "Missing end bracket."))

				elseif s:find("^[({]", pos) then
					depth = depth + 1

				elseif s:find("^[)}]", pos) then
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
						local _, _pos = s:find("^(=*)%[[^\n]-%]%1%]", pos+1)
						if not _pos then  return (parseError(context, luaPos, "Missing end of Lua string."))  end
						pos = _pos
					else
						-- void
					end

				elseif s:find("^%-", pos) then -- Maybe a comment.
					if not s:find("^%-", pos+1) then
						-- void
					elseif s:find("^%[=*%[", pos+2) then -- Long-form comment.
						local _, _pos = s:find("^(=*)%[[^\n]-%]%1%]", pos+3)
						if not _pos then  return (parseError(context, startPos, "Missing end bracket."))  end
						pos = _pos
					else -- Line comment.
						return (parseError(context, startPos, "Missing end bracket."))
					end

				elseif s:find("^[%a_]", pos) then -- Identifier/keyword.
					local _, _pos = s:find("^[%w_]*", pos+1)
					if s:sub(pos, _pos) == "function" then  return (parseError(context, luaPos, "Disallowed Lua code."))  end
					pos = _pos

				elseif s:find("^%d", pos) then -- Number.
					local _, _pos = s:find("^[%w_]+", pos)
					pos = _pos

				else
					return (parseError(context, pos, "Internal error: Unhandled case in brackets."))
				end
			end

			local i2 = pos -- Should be at ')' or '}'.
			pos      = pos + 1 -- ')' or '}'

			table.insert(tokens, {position=startPos, type="expression", value=s:sub(i1, i2)})

			if lastWasName and lastTok.type == "name" then  lastTok.hasAttachment = true  end

		-- Command separators: , OR ;
		elseif s:find("^[,;]", pos) then
			table.insert(tokens, {position=startPos, type=s:sub(pos, pos)})
			pos = pos + 1

		-- Comment: #comment
		elseif s:find("^#", pos) then
			pos = s:match("^[^\n]*\n?()", pos+1)

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

local function parseArguments(context, tokens, tokPos, commandOrFuncName, argInfos, args, visited)
	while true do
		if not tokens[tokPos] or isToken(tokens[tokPos], "linebreak") or isToken(tokens[tokPos], ",") or isToken(tokens[tokPos], ";") then
			return tokPos
		end

		local startTok = tokens[tokPos]
		local argName0 = ""
		local argName  = ""
		local argNames = nil
		local v, argInfo

		-- Explicit name.
		if isToken(startTok, "name") then
			argName0 = startTok.value
			argName  = argName0

			if argInfos[argName] then -- One-to-multiple argument mapping.
				argNames = argInfos[argName]
				argName  = argNames[1]
				argInfo  = itemWith1(argInfos, "name", argName) or error(argName)

				for _, _argName in ipairs(argNames) do
					if visited[_argName] then
						return (tokenError(context, startTok, "Duplicate argument '%s' (as part of '%s').", _argName, argName0))
					end
				end

			else
				argInfo = itemWith1(argInfos, "name", argName)
				if not argInfo then  return (tokenError(context, startTok, "No argument '%s' for '%s'.", argName, commandOrFuncName))  end

				if visited[argName] then  return (tokenError(context, startTok, "Duplicate argument '%s'.", argName))  end
			end

			if not startTok.hasAttachment then
				return (parseError(context, startTok.position+#argName0, "Missing value for argument '%s'.", argName0))
			end

			tokPos = tokPos + 1 -- the name
		end

		local valueStartTok = tokens[tokPos]

		-- Value.
		if isToken(tokens[tokPos], "literal") then
			v = tokens[tokPos].value

		elseif isToken(tokens[tokPos], "username") then
			local varName = tokens[tokPos].value
			local var     = findInStack(context, "variables", tokens[tokPos].position, varName) or context.readonly[varName]
			if not var then  return (tokenError(context, tokens[tokPos], "No variable '%s'.", varName))  end
			v = var.value

			if tokens[tokPos].negated then
				if type(v) ~= "number" then  return (parseError(context, tokens[tokPos].position-1, "Cannot negate value. ('%s' contains a %s)", varName, type(v)))  end
				v = -v
			end

		elseif isToken(tokens[tokPos], "expression") then
			local expr = tokens[tokPos].value

			local env = setmetatable({}, {
				__index = function(_, k)
					local var = findInStack(context, "variables", tokens[tokPos].position, k) or context.readonly[k]
					if var then  return var.value  end
					error("No variable '"..tostring(k).."'.", 0)
				end,
			})

			local chunk, err = loadstring("return"..expr, "@", "t", env) -- @Robustness: Don't use loadstring()!
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
				if not visited[_argInfo.name] and (type(_argInfo.value) == type(v) or _argInfo.value == nil) then
					argName = _argInfo.name
					break
				end
			end

			if argName == "" then  return (tokenError(context, startTok, "Unknown argument of type '%s' for '%s'.", type(v), commandOrFuncName))  end

		-- Validate value.
		elseif not (type(v) == type(argInfo.value) or argInfo.value == nil) then
			return (tokenError(context, tokens[tokPos], "Bad value for argument '%s'. (Expected %s, got %s)", argName0, type(argInfo.value), type(v)))
		end

		-- Finalize argument.
		if argNames then
			for _, _argName in ipairs(argNames) do
				args   [_argName] = v
				visited[_argName] = valueStartTok
			end
		else
			args   [argName] = v
			visited[argName] = valueStartTok
		end
		tokPos = tokPos + 1 -- the value
	end
end

local function collectBodyTokens(context, tokens, tokForError, tokPos, outTokens, lookForElseOrElseif)
	local depth         = 1
	local stack         = {}
	local expectCommand = true

	while true do
		local tok = tokens[tokPos]
		if not tok then  return (tokenError(context, tokForError, "Missing end of body."))  end

		if isToken(tok, "linebreak") or isToken(tok, ";") then
			expectCommand = true

		elseif not expectCommand then
			-- void

		elseif isToken(tok, "name") and SCOPE_ENTER_COMMANDS[tok.value] then
			depth = depth + 1
			table.insert(stack, tok)

		elseif isToken(tok, "name", "end") then
			depth = depth - 1
			table.remove(stack)
			if depth == 0 then  return tokPos+1  end

		elseif isToken(tok, "name", "else") or isToken(tok, "name", "elseif") then
			if lookForElseOrElseif and depth == 1 then
				return tokPos+1
			elseif isToken(stack[#stack], "name", "if") or isToken(stack[#stack], "name", "elseif") then
				stack[#stack] = tok -- Exit if-block and enter else/elseif-block.
			else
				tokenErrorNoStack(context, tok, "Unexpected '%s'.", tok.value)
				if stack[1] then  tokenMessage(context, stack[#stack], "...block started here.")  end
				printCallStack(context, getTokenPosition(context, tok))
				return nil
			end

		else
			expectCommand = false
		end

		table.insert(outTokens, tok)
		tokPos = tokPos + 1
	end
end

local function pushTransformAndReset(blendMode)
	LG.push()
	LG.reset()
	LG.setBlendMode(blendMode, "premultiplied")
end

local function pushGfxState(context)
	table.insert(context.gfxStack, copyGfxState(context.gfxState))
	LG.push() -- Only the transform.
end

-- success = popGfxState( context )
local function popGfxState(context)
	local gfxState = table.remove(context.gfxStack)
	if not gfxState then  return false  end

	moveGfxState(gfxState, context.gfxState)
	LG.pop()

	return true
end

local function isCanvasReferencedInStack(context, canvas)
	for _, gfxState in ipairs(context.gfxStack) do
		if canvas == gfxState.canvas then  return true  end
	end
	return false
end

local function initWorkCanvas(canvas, workCanvas)
	local filter = canvas:getFilter()
	canvas:setFilter("nearest")
	LG.setCanvas(workCanvas)
	LG.clear(0, 0, 0, 0)
	LG.draw(canvas)
	canvas:setFilter(filter)
end

-- applyEffect( context, shader|current, callback )
-- outputCanvas = callback( context, readCanvas, writeCanvas )
local function applyEffect(context, shader, cb)
	ensureCanvasAndInitted(context)

	local w , h  = context.gfxState.canvas:getDimensions()
	local cw, ch = context.workCanvas1:getDimensions()

	pushTransformAndReset("replace")
	LG.setScissor(0,0, cw,ch) -- @Incomplete: Properly test if this improves performance. (It doesn't seem to.) 2022-09-16
	initWorkCanvas(context.gfxState.canvas, context.workCanvas1)

	if shader then
		shaderSendVec2(shader, "texelClamp", (w-.5)/cw,(h-.5)/ch)
		LG.setShader(shader.shader)
	end

	local canvasOut = cb(context, context.workCanvas1, context.workCanvas2)

	LG.setCanvas(context.gfxState.canvas)
	LG.setShader(nil)
	LG.setColor(1, 1, 1)
	canvasOut:setFilter("nearest")
	LG.draw(canvasOut)
	canvasOut:setFilter("linear")
	LG.pop()
	LG.setScissor()
end

-- image|canvas = getOrLoadImage( context, tokenForError, pathIdentifier, recursionsAllowed )
-- Returns nil on error.
local function getOrLoadImage(context, tokForError, pathIdent, recursionsAllowed)
	local imageOrCanvas = context.images[pathIdent]
	if imageOrCanvas then  return imageOrCanvas  end

	local path = makePathAbsolute(pathIdent, (context.path:gsub("[^/\\]+$", "")))

	if pathIdent:find"%.artcmd$" then
		local startRecursion = (recursionsAllowed >= 1 and not artFileRecursionAllowed[path])

		if
			(artFileRecursionAllowed[path] and  artFilesBeingLoaded[path]       > artFileRecursionAllowed[path]) or
			(startRecursion                and (artFilesBeingLoaded[path] or 0) > recursionsAllowed            )
		then
			local imageData = love.image.newImageData(1,1, "rgba8", ("\0"):rep(1*1*4)) -- @Incomplete: Use the size of the image being loaded.
			imageOrCanvas   = LG.newImage(imageData) ; imageData:release()

			context.images[pathIdent] = imageOrCanvas
			return imageOrCanvas
		end

		pushGfxState(context) -- @Cleanup (Using LG.push/pop() is probably fine here, I think.)

		if startRecursion then  artFileRecursionAllowed[path] = recursionsAllowed  end
		local art = loadArtFile(path, context.isLocal)
		if startRecursion then  artFileRecursionAllowed[path] = nil  end

		if not art then  return (tokenError(context, tokForError, "Could not load .artcmd image."))  end
		imageOrCanvas = art.canvas

		assert(popGfxState(context))

	else
		local s, err = readFile(false, path)
		if not s then  return (tokenError(context, tokForError, "Could not read '%s'. (%s)", path, err))  end

		local fileData           = LF.newFileData(s, path)
		local ok, imageDataOrErr = pcall(love.image.newImageData, fileData) ; fileData:release()
		if not ok then  return (tokenError(context, tokForError, "Could not load '%s'. (%s)", pathIdent, imageDataOrErr))  end
		local imageData = normalizeImageAndMultiplyAlpha(imageDataOrErr)
		imageOrCanvas   = LG.newImage(imageData) ; imageData:release()
	end

	context.images[pathIdent] = imageOrCanvas
	return imageOrCanvas
end

local function pointsToCoords(points)
	local coords = {}
	for _, point in ipairs(points) do
		table.insert(coords, point.x)
		table.insert(coords, point.y)
	end
	return coords
end

local function tostringFloat(n)
	return tostring(n):gsub("^%-?%d+$", "%0.0")
end

local function setArgumentsToDefault(args, commandInfo)
	for _, argInfo in ipairs(commandInfo) do
		args[argInfo.name] = argInfo.value
	end
end

-- passes = evaluateCondition( context, command, tokenForError, conditionValue )
-- Returns nil on error.
local function evaluateCondition(context, command, tokForError, v)
	if     type(v) == "boolean" then  return v
	elseif type(v) == "number"  then  return (v ~= 0) and (v == v)
	elseif type(v) == "string"  then  return (v ~= "")
	elseif type(v) == "table"   then  return (v[1] ~= nil)
	else
		return (tokenError(context, tokForError, "[%s] Unsupported value type '%s'.", command, type(v)))
	end
end

local function formatForPrinting(v)
	-- @Robustness: Don't print invalid UTF-8!
	if type(v) ~= "table" then  return tostring(v)  end

	local strings = {}
	for _, item in ipairs(v) do
		table.insert(strings, tostring(item))
	end

	return "{" .. table.concat(strings, ", ") .. "}"
end

local runBlock

-- tokenPosition|nil, stop = runCommand( context, tokens, tokenPosition, commandToken|nil ) -- commandToken must be set for sequences.
local function runCommand(context, tokens, tokPos, commandTok)
	local startTok   = tokens[tokPos]
	local isSequence = commandTok ~= nil

	--
	-- Function call.
	--
	if isToken((commandTok or tokens[tokPos]), "username") then
		if not commandTok and tokens[tokPos].negated then  return (parseError(context, tokens[tokPos].position-1, "Unexpected character."))  end

		local funcName = (commandTok or tokens[tokPos]).value
		local funcInfo = findInStack(context, "functions", (commandTok or tokens[tokPos]).position, funcName)
		if not funcInfo   then  return (tokenError(context, startTok, "[Call] No function '%s'.", funcName))  end
		if not commandTok then  tokPos = tokPos + 1  end -- username

		if context.callStack[funcInfo] then
			return (tokenError(context, startTok, "[Call] Recursively calling '%s'. (%s:%d)", funcName, context.path, getLineNumber(context.source, funcInfo.token.position)))
		end

		-- Parse arguments.
		local argInfos = {}
		local args     = {}
		local visited  = {}

		for i, argName in ipairs(funcInfo.arguments) do
			argInfos[i] = {name=(argName:lower()), value=nil} -- Argument "FooBar" gets the name "foobar".  @Speed @Memory
		end

		tokPos = parseArguments(context, tokens, tokPos, funcName, argInfos, args, visited)
		if not tokPos then  return nil  end

		-- Run function.
		local entry          = ScopeStackEntry()
		entry.callerToken    = startTok
		entry.calledFunction = funcInfo

		for i, argInfo in ipairs(argInfos) do
			if not visited[argInfo.name] then  return (tokenError(context, startTok, "[Call] Missing argument '%s' for '%s'.", argInfo.name, funcName))  end
			entry.variables[funcInfo.arguments[i]] = {token=funcInfo.token, value=args[argInfo.name]}
		end

		table.insert(context.scopeStack, entry)
		context.callStack[funcInfo] = true

		local bodyTokPos, stop = runBlock(context, funcInfo.tokens, 1)
		if not bodyTokPos              then  return nil  end
		if funcInfo.tokens[bodyTokPos] then  return (tokenError(context, funcInfo.tokens[bodyTokPos], "[Call] Unexpected token."))  end

		table.remove(context.scopeStack)
		context.callStack[funcInfo] = nil

		if stop == STOP_ALL then  return 1/0, STOP_ALL  end
		return tokPos, STOP_CONTINUE
	end

	if not isToken((commandTok or tokens[tokPos]), "name") then
		return (tokenError(context, startTok, "Expected a command."))
	end
	local command = (commandTok or tokens[tokPos]).value
	if not commandTok then  tokPos = tokPos + 1  end -- name

	--
	-- Function.
	--
	if command == "func" then
		if not isToken(tokens[tokPos], "username") then  return (tokenError(context, tokens[tokPos], "[%s] Expected a name for the function.", command))  end
		if tokens[tokPos].negated                  then  return (parseError(context, tokens[tokPos].position-1, "[%s] Unexpected character.", command))  end

		local funcName = tokens[tokPos].value
		if context.readonly[funcName] then  return (tokenError(context, tokens[tokPos], "Name '%s' is reserved.", funcName))  end

		local entry = getLast(context.scopeStack)
		if entry.functions[funcName] and not context.reportedWarnings[tokens[tokPos]] then
			tokenWarning(context, tokens[tokPos],
				"[%s] Duplicate function '%s' in the same scope. Replacing. (%s:%d)",
				command, funcName, context.path, getLineNumber(context.source, entry.functions[funcName].token.position)
			)
			context.reportedWarnings[tokens[tokPos]] = true
		end

		tokPos = tokPos + 1 -- username

		local funcInfo            = FunctionInfo()
		funcInfo.token            = startTok
		funcInfo.name             = funcName
		entry.functions[funcName] = funcInfo

		while isToken(tokens[tokPos], "username") do
			if tokens[tokPos].negated then  return (parseError(context, tokens[tokPos].position-1, "Unexpected character."))  end

			local argName = tokens[tokPos].value
			if context.readonly[argName] then  return (tokenError(context, tokens[tokPos], "Name '%s' is reserved.", argName))  end

			table.insert(funcInfo.arguments, argName)
			tokPos = tokPos + 1 -- username
		end

		if not (isToken(tokens[tokPos], "linebreak") or isToken(tokens[tokPos], ";")) then
			return (tokenError(context, tokens[tokPos], "[%s] Expected argument for function '%s'.", command, funcName))
		end

		return collectBodyTokens(context, tokens, startTok, tokPos, funcInfo.tokens, false), STOP_CONTINUE -- May return nil.
	end

	--
	-- Normal command.
	--
	local commandInfo = context.commands[command]
	if not commandInfo then  return (tokenError(context, startTok, "Unknown command '%s'.", command))  end

	local args    = {}
	local visited = {}
	setArgumentsToDefault(args, commandInfo) -- @Speed: This is not necessary, just convenient.

	-- Special cases: Treat `set X` as `set "X"` (including some more commands).
	if
		(command == "set" or command == "setx" or command == "add" or command == "rem" or command == "for" or command == "fori")
		and isToken(tokens[tokPos], "username")
		and not tokens[tokPos].negated
	then
		args   .var = tokens[tokPos].value
		visited.var = tokens[tokPos]
		tokPos      = tokPos + 1
	end

	-- Parse arguments and run command.
	tokPos = parseArguments(context, tokens, tokPos, command, context.commands[command], args, visited)
	if not tokPos then  return nil  end

	local isFollowedBySequence = isToken(tokens[tokPos], ",")

	--
	-- Language.
	--
	----------------------------------------------------------------
	if command == "set" or command == "setx" then
		if args.var   == ""  then  return (tokenError(context, (visited.var   or startTok), "[%s] Missing variable name."          , command))  end
		if args.value == nil then  return (tokenError(context, (visited.value or startTok), "[%s] Missing value to assign to '%s'.", command, args.var))  end
		if not validateVariableName(context, (visited.var or startTok), "variable name", args.var) then  return nil  end

		local var

		if command == "set" then
			if context.readonly[args.var] then  return (tokenError(context, (visited.var or startTok), "Name '%s' is reserved.", args.var))  end

			local vars     = getLast(context.scopeStack).variables
			var            = vars[args.var] or {token=startTok, value=nil}
			vars[args.var] = var

		else
			var = findInStack(context, "variables", tokens[tokPos].position, args.var)
			if not var then  return (tokenError(context, (visited.var or startTok), "[%s] No existing variable '%s'.", command, args.var))  end
		end

		var.value = args.value

	elseif command == "add" then
		if args.var   == ""  then  return (tokenError(context, (visited.var   or startTok), "[%s] Missing variable name."          , command))  end
		if args.value == nil then  return (tokenError(context, (visited.value or startTok), "[%s] Missing value to assign to '%s'.", command, args.var))  end
		if not validateVariableName(context, (visited.var or startTok), "variable name", args.var) then  return nil  end

		local var = findInStack(context, "variables", tokens[tokPos].position, args.var)
		if not var                    then  return (tokenError(context, (visited.var or startTok), "[%s] No existing variable '%s'.", command, args.var))  end
		if type(var.value) ~= "table" then  return (tokenError(context, (visited.var or startTok), "[%s] '%s' is not an array. (It is a %s)", command, args.var, type(var.value)))  end
		table.insert(var.value, args.value)

	elseif command == "rem" then
		if args.var == "" then  return (tokenError(context, (visited.var or startTok), "[%s] Missing variable name.", command))  end
		if not validateVariableName(context, (visited.var or startTok), "variable name", args.var) then  return nil  end

		local var = findInStack(context, "variables", tokens[tokPos].position, args.var)
		if not var                    then  return (tokenError(context, (visited.var or startTok), "[%s] No existing variable '%s'.", command, args.var))  end
		if type(var.value) ~= "table" then  return (tokenError(context, (visited.var or startTok), "[%s] '%s' is not an array. (It is a %s)", command, args.var, type(var.value)))  end

		local i = args.i
		if i < 0 then  i = #var.value + 1 + i  end
		if var.value[i] == nil then  return (tokenError(context, (visited.var or startTok), "[%s] Index %s is out-of-bounds. (Array length is %d)", command, args.i, #var.value))  end
		table.remove(var.value, i)

	----------------------------------------------------------------
	elseif command == "do" then
		if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "[%s] Invalid ','. Cannot chain '%s'.", command, command))  end

		-- Get block body.
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens, true)
		if not tokPos then  return nil  end

		-- Run block.
		table.insert(context.scopeStack, ScopeStackEntry())

		local bodyTokPos, stop = runBlock(context, bodyTokens, 1)
		if not bodyTokPos         then  return nil  end
		if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "[%s] Unexpected token.", command))  end

		table.remove(context.scopeStack)
		if stop == STOP_ALL then  return 1/0, STOP_ALL  end

	----------------------------------------------------------------
	elseif command == "if" then
		if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "[if] Invalid ','. Cannot chain 'if'."))  end
		if not visited.value            then  return (tokenError(context, (commandTok or startTok), "[if] Missing value."))  end

		local executedSomeBlock = false

		-- if
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens, true)
		if not tokPos then  return nil  end

		local passes = evaluateCondition(context, command, (visited.value or startTok), args.value)
		if passes == nil then  return nil  end

		if passes then
			-- Run block.
			table.insert(context.scopeStack, ScopeStackEntry())

			local bodyTokPos, stop = runBlock(context, bodyTokens, 1)
			if not bodyTokPos         then  return nil  end
			if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "[if] Unexpected token."))  end

			table.remove(context.scopeStack)
			if stop == STOP_ALL then  return 1/0, STOP_ALL  end
			executedSomeBlock = true
		end

		-- elseif
		while isToken(tokens[tokPos-1], "name", "elseif") do
			startTok = tokens[tokPos-1]

			-- @Copypaste from above.
			table.clear(args)
			table.clear(visited)
			setArgumentsToDefault(args, commandInfo) -- @Speed: This is not necessary, just convenient.

			-- @Incomplete @Speed: Don't evaluate arguments if executedSomeBlock=true. (Low priority because evaluating expressions currently has no side effects. 2022-09-19)
			tokPos = parseArguments(context, tokens, tokPos, command, context.commands[command], args, visited)
			if not tokPos then  return nil  end

			if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "[elseif] Invalid ','. Cannot chain 'elseif' using ','."))  end
			if not visited.value            then  return (tokenError(context, startTok, "[elseif] Missing value."))  end
			--

			table.clear(bodyTokens)
			tokPos = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens, true)
			if not tokPos then  return nil  end

			if not executedSomeBlock then
				passes = evaluateCondition(context, "elseif", (visited.value or startTok), args.value)
				if passes == nil then  return nil  end

				if passes then
					-- Run block.
					table.insert(context.scopeStack, ScopeStackEntry())

					local bodyTokPos, stop = runBlock(context, bodyTokens, 1)
					if not bodyTokPos         then  return nil  end
					if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "[elseif] Unexpected token."))  end

					table.remove(context.scopeStack)
					if stop == STOP_ALL then  return 1/0, STOP_ALL  end
					executedSomeBlock = true
				end
			end
		end

		-- else
		if isToken(tokens[tokPos-1], "name", "else") then
			startTok = tokens[tokPos-1]
			if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "[else] Invalid ','. Cannot chain 'else'."))  end

			table.clear(bodyTokens)
			tokPos = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens, false)
			if not tokPos then  return nil  end

			if not executedSomeBlock then
				-- Run block.
				table.insert(context.scopeStack, ScopeStackEntry())

				local bodyTokPos, stop = runBlock(context, bodyTokens, 1)
				if not bodyTokPos         then  return nil  end
				if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "[else] Unexpected token."))  end

				table.remove(context.scopeStack)
				if stop == STOP_ALL then  return 1/0, STOP_ALL  end
			end
		end

	----------------------------------------------------------------
	elseif command == "for" or command == "fori" then
		if args.var  == ""                         then  return (tokenError(context, (visited.var  or startTok), "[%s] Empty variable name.", command))  end
		if command == "for"  and not visited.to    then  return (tokenError(context, startTok                  , "[%s] Missing loop range." , command))  end
		if command == "fori" and not visited.value then  return (tokenError(context, startTok                  , "[%s] Missing value."      , command))  end
		if args.step == 0                          then  return (tokenError(context, (visited.step or startTok), "[%s] Step is zero."       , command))  end
		if not validateVariableName(context, (visited.var or startTok), "variable name", args.var) then  return nil  end

		if context.readonly[args.var] then  return (tokenError(context, (visited.var or startTok), "Name '%s' is reserved.", args.var))  end

		if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "[%s] Invalid ','. Cannot chain '%s'.", command, command))  end -- It'd be nice to chain loops though! @UX

		-- Get loop body.
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens, false)
		if not tokPos then  return nil  end

		-- Run loop.
		local entry = ScopeStackEntry()
		table.insert(context.scopeStack, entry)

		local from = (command == "fori") and 1   or args.from
		local to   = (command == "fori") and 1/0 or args.to
		local step = (command == "fori") and 1   or args.step

		if args.rev then
			from, to, step = to, from, -step
		end

		local loops   = 0
		local iterVar = {token=startTok, value=nil}
		local stopAll = false

		for n = from, to, step do
			if command == "fori" and args.value[n] == nil then  break  end

			loops = loops + 1
			if loops >= MAX_LOOPS then -- Maybe there should be a MAX_DRAW_OPERATIONS too? MAX_LOOPS could probably be higher then. @Incomplete
				tokenError(context, startTok, "[%s] Max loops exceeded. Breaking.", command)
				break
			end

			table.clear(entry.functions)
			table.clear(entry.variables)

			iterVar.value             = (command == "for") and n or args.value[n]
			entry.variables[args.var] = iterVar

			local bodyTokPos, stop = runBlock(context, bodyTokens, 1)
			if not bodyTokPos         then  return nil              end
			if stop == STOP_ONE       then  break                   end
			if stop == STOP_ALL       then  stopAll = true ; break  end
			if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "[%s] Unexpected token.", command))  end
		end

		table.remove(context.scopeStack)
		if stopAll then  return 1/0, STOP_ALL  end

	----------------------------------------------------------------
	elseif command == "stop" then
		if args.all then  print("Stopping all!")  end
		return 1/0, (args.all and STOP_ALL or STOP_ONE)

	----------------------------------------------------------------
	elseif command == "assert" then
		if not visited.value then  return (tokenError(context, startTok, "[%s] Missing value.", command))  end

		if     type(args.value) == "boolean" then  if not args.value                               then  return (tokenError(context, (visited.value or startTok), "Assertion failed! (False)"))  end
		elseif type(args.value) == "string"  then  if args.value == ""                             then  return (tokenError(context, (visited.value or startTok), "Assertion failed! (Empty string)"))  end
		elseif type(args.value) == "number"  then  if args.value == 0 or args.value ~= args.value  then  return (tokenError(context, (visited.value or startTok), "Assertion failed! (Zero or NaN)"))  end
		elseif type(args.value) == "table"   then  if args.value[1] == nil                         then  return (tokenError(context, (visited.value or startTok), "Assertion failed! (Empty array)"))  end
		else
			return (tokenError(context, visited.value, "[%s] Unsupported value type '%s'.", command, type(args.value)))
		end

	elseif command == "print" then
		if isSequence then
			io.write("\t", formatForPrinting(args.value))
		else
			local ln = getLineNumber(context.source, startTok.position)
			io.write(context.path, ":", ln, ": ", formatForPrinting(args.value))
		end
		if not isFollowedBySequence then
			io.write("\n")
		end

	----------------------------------------------------------------
	elseif command == "default" then
		if args.cmd == "" then  return (tokenError(context, (visited.cmd or startTok), "[%s] Missing command name." , command))  end
		if args.arg == "" then  return (tokenError(context, (visited.arg or startTok), "[%s] Missing argument name.", command))  end

		if args.cmd == "func" then  return (tokenError(context, (visited.cmd or startTok), "[%s] Invalid command '%s'.", command, args.cmd))  end

		local commandInfo = context.commands[args.cmd]
		if not commandInfo then  return (tokenError(context, (visited.cmd or startTok), "[%s] No command '%s'.", command, args.cmd))  end

		local defaultCommandInfo = COMMANDS[args.cmd]
		if defaultCommandInfo.protected then
			return (tokenError(context, (visited.cmd or startTok), "[%s] Cannot set default values for command '%s'.", command, args.cmd))
		end

		local argInfo = itemWith1(commandInfo, "name", args.arg)
		if not argInfo then  return (tokenError(context, (visited.arg or startTok), "[%s] Command '%s' has no argument '%s'.", command, args.cmd, args.arg))  end
		local defaultArgInfo = itemWith1(defaultCommandInfo, "name", argInfo.name)

		if visited.value then
			if not (type(args.value) == type(defaultArgInfo.value) or defaultArgInfo.value == nil) then
				return (tokenError(context, (visited.value or startTok),
					"[%s] Bad value for argument '%s' of command '%s'. (Expected %s, got %s)",
					command, argInfo.name, args.cmd, type(defaultArgInfo.value), type(args.value)
				))
			end
			argInfo.value = args.value
		else
			argInfo.value = defaultArgInfo.value
		end

	--
	-- Settings, app.
	--
	----------------------------------------------------------------
	elseif command == "backdrop" then
		context.art.backdrop[1] = args.r
		context.art.backdrop[2] = args.g
		context.art.backdrop[3] = args.b
		context.art.backdrop[4] = args.a

	----------------------------------------------------------------
	elseif command == "zoom" then
		context.art.zoom       = args.zoom
		context.art.zoomFilter = args.filter

	--
	-- Settings, init.
	--
	----------------------------------------------------------------
	elseif command == "canvas" then
		if context.art.canvas then  return (tokenError(context, startTok, "[%s] Cannot use 'canvas' after drawing commands.", command))  end

		context.canvasWidth  = (args.w >= 1) and args.w or DEFAULT_ART_SIZE
		context.canvasHeight = (args.h >= 1) and args.h or DEFAULT_ART_SIZE
		context.canvasMsaa   = args.aa

		context.readonly.CanvasW.value = context.canvasWidth
		context.readonly.CanvasH.value = context.canvasHeight

	--
	-- State.
	--
	----------------------------------------------------------------
	elseif command == "push" then
		ensureCanvasAndInitted(context)
		pushGfxState(context)

	elseif command == "pop" then
		if not popGfxState(context) then -- Will fail if there was no push - otherwise ensureCanvasAndInitted() will have been called.
			tokenWarning(context, startTok, "pop: Too many 'pop' commands. Ignoring.")
		end

	elseif command == "reset" then
		ensureCanvasAndInitted(context)
		if args.gfx then
			resetGfxState(context.gfxState)
			context.gfxState.canvas = context.art.canvas
		end
		if args.transform then
			LG.origin()
		end

	----------------------------------------------------------------
	elseif command == "color" or command == "grey" then
		ensureCanvasAndInitted(context)

		local gfxState     = context.gfxState
		gfxState.colorMode = "flatcolor"

		if command == "color" then  updateVec4(gfxState.flatColor, args.r,args.g,args.b,args.a)
		else                        updateVec4(gfxState.flatColor, args.grey,args.grey,args.grey,args.a)  end

		if gfxState.colorTexture then
			-- @Memory: Release image. (Consider gfxStack!)
			gfxState.colorTexture = nil
		end

	----------------------------------------------------------------
	elseif command == "grad" then
		ensureCanvasAndInitted(context)

		local gfxState               = context.gfxState
		gfxState.colorMode           = "gradient"
		gfxState.colorTextureRadial  = args.radial
		gfxState.colorTextureSmooth  = args.smooth
		gfxState.colorTextureFit     = args.fit
		gfxState.colorTextureScaleX  = args.sx
		gfxState.colorTextureScaleY  = args.sy
		gfxState.colorTextureAngle   = args.rot
		gfxState.colorTextureOffsetX = args.x
		gfxState.colorTextureOffsetY = args.y

		if gfxState.colorTexture then
			-- @Memory: Release image. (Consider gfxStack!)
			gfxState.colorTexture = nil
		end

		if args.clear and not isSequence then
			table.clear(gfxState.gradient)
		end
		table.insert(gfxState.gradient, args.r)
		table.insert(gfxState.gradient, args.g)
		table.insert(gfxState.gradient, args.b)
		table.insert(gfxState.gradient, args.a)

	----------------------------------------------------------------
	elseif command == "texture" then
		if not context.buffers[args.buf]     then  return (tokenError(context, (visited.name  or startTok), "[%s] No buffer '%s'."   , command, args.buf  ))  end
		if not WRAP_TO_LOVE_WRAP[args.modex] then  return (tokenError(context, (visited.modex or startTok), "[%s] Invalid mode '%s'.", command, args.modex))  end
		if not WRAP_TO_LOVE_WRAP[args.modey] then  return (tokenError(context, (visited.modey or startTok), "[%s] Invalid mode '%s'.", command, args.modey))  end

		ensureCanvasAndInitted(context)

		local gfxState               = context.gfxState
		gfxState.colorMode           = "texture"
		gfxState.colorTextureFit     = args.fit
		gfxState.colorTextureScaleX  = args.sx
		gfxState.colorTextureScaleY  = args.sy
		gfxState.colorTextureAngle   = args.rot
		gfxState.colorTextureOffsetX = args.x
		gfxState.colorTextureOffsetY = args.y
		gfxState.colorTextureBuffer  = args.buf
		gfxState.colorTextureWrapX   = args.modex
		gfxState.colorTextureWrapY   = args.modey

		if gfxState.colorTexture then
			-- @Memory: Release image. (Consider gfxStack!)
			gfxState.colorTexture = nil
		end

	----------------------------------------------------------------
	elseif command == "scalecolor" then
		local gfxState = context.gfxState

		gfxState.flatColor[1] = (gfxState.flatColor[1] - args.base) * args.r + args.base
		gfxState.flatColor[2] = (gfxState.flatColor[2] - args.base) * args.g + args.base
		gfxState.flatColor[3] = (gfxState.flatColor[3] - args.base) * args.b + args.base
		gfxState.flatColor[4] = (gfxState.flatColor[4] - args.base) * args.a + args.base

		for i = 1, #gfxState.gradient, 4 do
			gfxState.gradient[i  ] = (gfxState.gradient[i  ] - args.base) * args.r + args.base
			gfxState.gradient[i+1] = (gfxState.gradient[i+1] - args.base) * args.g + args.base
			gfxState.gradient[i+2] = (gfxState.gradient[i+2] - args.base) * args.b + args.base
			gfxState.gradient[i+3] = (gfxState.gradient[i+3] - args.base) * args.a + args.base
		end

	elseif command == "addcolor" then
		local gfxState = context.gfxState

		gfxState.flatColor[1] = gfxState.flatColor[1] + args.r
		gfxState.flatColor[2] = gfxState.flatColor[2] + args.g
		gfxState.flatColor[3] = gfxState.flatColor[3] + args.b
		gfxState.flatColor[4] = gfxState.flatColor[4] + args.a

		for i = 1, #gfxState.gradient, 4 do
			gfxState.gradient[i  ] = gfxState.gradient[i  ] + args.r
			gfxState.gradient[i+1] = gfxState.gradient[i+1] + args.g
			gfxState.gradient[i+2] = gfxState.gradient[i+2] + args.b
			gfxState.gradient[i+3] = gfxState.gradient[i+3] + args.a
		end

	elseif command == "overlaycolor" then
		local gfxState = context.gfxState

		gfxState.flatColor[1] = math.lerp(gfxState.flatColor[1], args.r, args.a)
		gfxState.flatColor[2] = math.lerp(gfxState.flatColor[2], args.g, args.a)
		gfxState.flatColor[3] = math.lerp(gfxState.flatColor[3], args.b, args.a)

		for i = 1, #gfxState.gradient, 4 do
			gfxState.gradient[i  ] = math.lerp(gfxState.gradient[i  ], args.r, args.a)
			gfxState.gradient[i+1] = math.lerp(gfxState.gradient[i+1], args.g, args.a)
			gfxState.gradient[i+2] = math.lerp(gfxState.gradient[i+2], args.b, args.a)
		end

	----------------------------------------------------------------
	elseif command == "blend" then
		if not (
			args.mode == "alpha" or args.mode == "replace" or args.mode == "screen" or args.mode == "add" or args.mode == "subtract"
			or args.mode == "multiply" or args.mode == "lighten" or args.mode == "darken"
		) then
			return (tokenError(context, (visited.mode or startTok),
				"[%s] Bad blend mode '%s'. Must be 'alpha', 'replace', 'screen', 'add', 'subtract', 'multiply', 'lighten' or 'darken'.",
				command, args.mode
			))
		end
		context.gfxState.blendMode = args.mode

	----------------------------------------------------------------
	elseif command == "image" then
		if args.path == "" then  return (tokenError(context, (visited.path or startTok), "[%s] Missing path.", command))  end

		ensureCanvasAndInitted(context)

		local texture = getOrLoadImage(context, startTok, args.path, args.recurse) -- @Incomplete: Handle args.recurse having different values for the same path.
		if not texture then  return nil  end

		local bufName = visited.buf and args.buf or args.path
		if bufName == "" then  return (tokenError(context, (visited.buf or startTok), "[%s] Missing buffer name.", command))  end

		local iw,ih        = texture:getDimensions()
		local msaa         = visited.aa and args.aa or context.canvasMsaa
		local canvasFormat = "rgba16"

		-- :ReuseOrCreateBuffer
		if not (
			context.buffers[bufName]
			and context.buffers[bufName]:getWidth () == iw
			and context.buffers[bufName]:getHeight() == ih
			and context.buffers[bufName]:getMSAA  () == msaa
			and context.buffers[bufName]:getFormat() == canvasFormat
		) then
			if iw > context.canvasWidth or ih > context.canvasHeight then -- @Cleanup: Validate size in getOrLoadImage().
				return (tokenError(context, (visited.path or startTok),
					"[%s] Image size cannot currently be larger than the canvas. (Canvas is %dx%d, got %dx%d)",
					command, context.canvasWidth,context.canvasHeight, iw,ih
				))
			end

			if context.buffers[bufName] then
				-- Don't accidentally release the active canvas, or one in the stack!
				LG.setCanvas(nil)
				if isCanvasReferencedInStack(context, context.buffers[bufName]) then
					return (tokenError(context, (visited.buf or visited.path or startTok), "[%s] Cannot replace existing buffer '%s' with image at this point.", command, bufName))
				end
				context.buffers[bufName]:release()
			end

			context.buffers[bufName] = LG.newCanvas(iw,ih, {format=canvasFormat, msaa=(msaa > 1 and msaa^2 or nil)})
			gfxStateSetCanvas(context, context.buffers[bufName])
		end

		texture:setFilter("nearest")
		pushTransformAndReset("replace")
		LG.setCanvas(context.buffers[bufName])
		LG.clear(0, 0, 0, 0)
		LG.draw(texture)
		LG.pop()

	----------------------------------------------------------------
	elseif command == "font" then
		local fonts = context.fontsBySize[args.path] or {}
		local font  = fonts[args.size]

		if font then
			-- void

		elseif args.path == "" then
			font = LG.newFont(args.size)

		else
			local path = makePathAbsolute(args.path, (context.path:gsub("[^/\\]+$", "")))

			local s, err = readFile(false, path)
			if not s then  return (tokenError(context, (visited.path or startTok), "[%s] Could not read '%s'. (%s)", command, args.path, err))  end

			local fileData      = LF.newFileData(s, path) -- @Speed: Cache this (though people probably don't use too many sizes of the same font per art piece).
			local ok, fontOrErr = pcall(LG.newFont, fileData, args.size) ; fileData:release()
			if not ok then  return (tokenError(context, (visited.path or startTok), "[%s] Could not load '%s'. (%s)", command, args.path, fontOrErr))  end
			font = fontOrErr
		end

		context.fontsBySize[args.path] = fonts
		fonts[args.size]               = font
		context.gfxState.font          = font

	----------------------------------------------------------------
	elseif command == "imagefont" then
		if args.path == "" then  return (tokenError(context, (visited.path or startTok), "[%s] Missing path.", command))  end
		local font = context.fontsByPath[args.path]

		if not font then
			local fontPath = makePathAbsolute(args.path, (context.path:gsub("[^/\\]+$", "")))

			local fontStr, err = readFile(false, fontPath)
			if not fontStr then  return (tokenError(context, (visited.path or startTok), "[%s] Could not read '%s'. (%s)", command, args.path, err))  end

			local fontFileData = LF.newFileData(fontStr, fontPath)
			local ok, fontOrErr

			-- LÖVE ImageFont.
			if visited.chars then
				ok, fontOrErr = pcall(LG.newImageFont, fontFileData, args.chars, args.spacing)

			-- BMFont.
			else
				if visited.spacing then  return (tokenError(context, (visited.spacing or startTok), "[%s] Cannot modify the glyph spacing for BMFonts.", command))  end

				local imagePath0 = ""
				local pos        = 1

				for _pos, _imagePath in fontStr:gmatch'%f[^\n%z]()page%f[ ][^\n]- file="([^\n]-)"' do
					if imagePath0 ~= "" then
						return (tokenError(context, (visited.path or startTok),
							"[%s] Failed loading BMFont...\n"
							.." %s:%d: Font references multiple image files, which is not supported.",
							command, fontPath, getLineNumber(fontStr, _pos)
						))
					end
					imagePath0 = _imagePath
					pos        = _pos
				end

				if imagePath0 == "" then
					return (tokenError(context, (visited.path or startTok),
						"[%s] Failed loading BMFont...\n"
						.." %s: Font seem to reference no image file. (Is this not a BMFont?)",
						command, fontPath
					))
				end

				local imagePath = makePathAbsolute(imagePath0, (fontPath:gsub("[^/\\]+$", "")))

				local imageStr, err = readFile(false, imagePath)
				if not imageStr then
					return (tokenError(context, (visited.path or startTok),
						"[%s] Failed loading BMFont...\n"
						.." %s:%d: Could not read referenced image '%s'. (%s)",
						command, fontPath, getLineNumber(fontStr, pos), imagePath0, err
					))
				end

				local imageFileData = LF.newFileData(imageStr, imagePath)
				ok, fontOrErr       = pcall(LG.newFont, fontFileData, imageFileData) ; imageFileData:release()
			end

			fontFileData:release()
			if not ok then  return (tokenError(context, (visited.path or startTok), "[%s] Could not load '%s'. (%s)", command, args.path, fontOrErr))  end
			font = fontOrErr
		end

		context.fontsByPath[args.path] = font
		context.gfxState.font          = font

	----------------------------------------------------------------
	elseif command == "mask" then
		ensureCanvasAndInitted(context)

		if args.mask then
			local bufName = args.buf
			if bufName == "" then  return (tokenError(context, (visited.buf or startTok), "[%s] Missing mask buffer name.", command))  end

			local bufCanvas = context.buffers[bufName]
			if not bufCanvas then  return (tokenError(context, (visited.buf or startTok), "[%s] No buffer '%s' to use as mask.", command, bufName))  end

			context.gfxState.useMask       = bufName
			context.gfxState.maskAlphaMode = args.alpha

			if context.gfxState.canvas == bufCanvas then
				gfxStateSetCanvas(context, nil) -- :SetMainBuffer
			end

		else
			if visited.buf and args.buf ~= "" then  tokenWarning(context, (visited.buf or startTok), "[%s] Cannot specify a mask buffer when disabling the mask. Ignoring."  , command)  end
			if visited.maskAlphaMode          then  tokenWarning(context, (visited.buf or startTok), "[%s] Cannot specify mask alpha mode when disabling the mask. Ignoring.", command)  end

			context.gfxState.useMask = "" -- :DisableMask
		end

	----------------------------------------------------------------
	elseif command == "map" then
		if not CHANNEL_MAPPING_INDICES[args.r] then  return (tokenError(context, (visited.r or startTok), "[%s] Invalid channel '%s'. Must be 'r', 'g', 'b', 'a', '0' or '1'.", command, args.r))  end
		if not CHANNEL_MAPPING_INDICES[args.g] then  return (tokenError(context, (visited.g or startTok), "[%s] Invalid channel '%s'. Must be 'r', 'g', 'b', 'a', '0' or '1'.", command, args.g))  end
		if not CHANNEL_MAPPING_INDICES[args.b] then  return (tokenError(context, (visited.b or startTok), "[%s] Invalid channel '%s'. Must be 'r', 'g', 'b', 'a', '0' or '1'.", command, args.b))  end
		if not CHANNEL_MAPPING_INDICES[args.a] then  return (tokenError(context, (visited.a or startTok), "[%s] Invalid channel '%s'. Must be 'r', 'g', 'b', 'a', '0' or '1'.", command, args.a))  end

		context.gfxState.channelMapping[1] = CHANNEL_MAPPING_INDICES[args.r]
		context.gfxState.channelMapping[2] = CHANNEL_MAPPING_INDICES[args.g]
		context.gfxState.channelMapping[3] = CHANNEL_MAPPING_INDICES[args.b]
		context.gfxState.channelMapping[4] = CHANNEL_MAPPING_INDICES[args.a]

	----------------------------------------------------------------
	elseif command == "channel" then
		context.gfxState.writeToChannels[1] = args.r
		context.gfxState.writeToChannels[2] = args.g
		context.gfxState.writeToChannels[3] = args.b
		context.gfxState.writeToChannels[4] = args.a

	----------------------------------------------------------------
	elseif command == "origin" then
		LG.origin()
	elseif command == "move" then
		LG.translate(args.x, args.y)
	elseif command == "rotate" then
		LG.rotate(args.rot)
	elseif command == "scale" then
		LG.scale(args.x, args.y)
	elseif command == "shear" then
		LG.shear(args.x, args.y)

	----------------------------------------------------------------
	elseif command == "setbuf" then
		if args.template ~= "" and not context.buffers[args.template] then
			return (tokenError(context, (visited.template or startTok), "[%s] No buffer '%s' to use as template.", command, args.template))
		end

		ensureCanvasAndInitted(context)

		local bufName      = (args.mask and not visited.name) and "mask"  or args.name
		local iw           = visited.w                        and args.w  or (context.buffers[args.template] or context.art.canvas):getWidth ()
		local ih           = visited.h                        and args.h  or (context.buffers[args.template] or context.art.canvas):getHeight()
		local msaa         = visited.aa                       and args.aa or (context.buffers[args.template] or context.art.canvas):getMSAA  ()
		local canvasFormat = args.mask                        and "r16"   or "rgba16"
		local texture

		-- Main buffer (the "canvas").
		if bufName == "" then
			if args.template ~= "" then  return (tokenError(context, startTok, "[%s] Missing buffer name.", command))  end

			if visited.w or visited.h then
				return (tokenError(context, (visited.w or visited.h or startTok), "[%s] Cannot set the size of the main buffer. Use the 'canvas' command instead.", command))
			end

			gfxStateSetCanvas(context, nil) -- :SetMainBuffer

		-- Custom buffer. :ReuseOrCreateBuffer
		else
			-- Reuse existing.
			if
				context.buffers[bufName]
				and context.buffers[bufName]:getWidth () == iw
				and context.buffers[bufName]:getHeight() == ih
				and context.buffers[bufName]:getMSAA  () == msaa
				and context.buffers[bufName]:getFormat() == canvasFormat
			then
				gfxStateSetCanvas(context, context.buffers[bufName])

				if args.clear and not (args.template ~= "" and args.template == bufName) then
					LG.setCanvas(context.gfxState.canvas)
					LG.clear(0, 0, 0, (args.mask and 1 or 0))
				end

			-- Create new.
			else
				if iw < 1 then  return (tokenError(context, (visited.w or startTok), "[%s] The width must be a positive number." , command))  end
				if ih < 1 then  return (tokenError(context, (visited.h or startTok), "[%s] The height must be a positive number.", command))  end

				if iw > context.canvasWidth or ih > context.canvasHeight then
					return (tokenError(context, (iw > context.canvasWidth and visited.w or visited.h or startTok),
						"[%s] Buffer size cannot currently be larger than the canvas. (Canvas is %dx%d, got %dx%d)",
						command, context.canvasWidth,context.canvasHeight, iw,ih
					))
				end

				if context.buffers[bufName] then
					-- Don't accidentally release the active canvas, or one in the stack!
					LG.setCanvas(nil)
					if isCanvasReferencedInStack(context, context.buffers[bufName]) then
						return (tokenError(context, (visited.buf or startTok), "[%s] Cannot replace existing buffer '%s' at this point.", command, bufName))
					end
					context.buffers[bufName]:release()
				end

				context.buffers[bufName] = LG.newCanvas(iw,ih, {format=canvasFormat, msaa=(msaa > 1 and msaa^2 or nil)})
				gfxStateSetCanvas(context, context.buffers[bufName])

				if args.mask then
					LG.setCanvas(context.gfxState.canvas)
					LG.clear(0, 0, 0, 1) -- Non-masks are already cleared (to [0,0,0,0]).
				end
			end

			-- Make sure the current buffer isn't used as mask.
			if context.gfxState.useMask == bufName then
				context.gfxState.useMask = "" -- :DisableMask
			end
		end

		if args.template ~= "" and args.template ~= bufName then
			context.buffers[args.template]:setFilter("nearest")
			pushTransformAndReset("replace")
			LG.setCanvas(context.gfxState.canvas)
			LG.draw(context.buffers[args.template])
			LG.pop()
		end

	----------------------------------------------------------------
	elseif command == "randseed" then
		if visited.seed then
			context.rng = context.rng or love.math.newRandomGenerator()
			context.rng:setSeed(args.seed)
		else
			context.rng = nil
		end

	--
	-- Drawing.
	--
	----------------------------------------------------------------
	elseif command == "point" or command == "relpoint" then
		if args.clear and not isSequence then
			table.clear(context.points)
		end

		local point = {x=args.x,y=args.y, a=args.a,b=args.b, s=args.s}

		if command == "relpoint" then
			local prevPoint = getLast(context.points)
			point.x         = prevPoint.x + point.x
			point.y         = prevPoint.y + point.y
		end

		local len                         = #context.points + 1
		context.points[len]               = point
		context.readonly.PointCount.value = len

	----------------------------------------------------------------
	elseif command == "fill" then
		ensureCanvasAndInitted(context)

		local gfxState = context.gfxState
		local cw,ch    = gfxState.canvas:getDimensions()

		-- Save minimal state.
		local colorMode = gfxState.colorMode
		local color     = {unpack(gfxState.flatColor)}

		gfxState.colorMode = "flatcolor"
		updateVec4(gfxState.flatColor, args.r,args.g,args.b,args.a)

		-- Fill!
		-- Note: We don't use clear() because of shaders and stuff. This is a drawing operation!
		applyCanvas(context, nil)
		applyColor(context, nil, "rectangle", cw,ch)

		LG.push()
		LG.origin()
		LG.rectangle("fill", -1,-1, cw+2,ch+2) -- Not sure if the bleeding is necessary (if msaa>1).
		LG.pop()

		-- Restore state.
		gfxState.colorMode = colorMode
		updateVec4(gfxState.flatColor, unpack(color))

	----------------------------------------------------------------
	elseif command == "rect" then
		if not (args.mode == "fill" or args.mode == "line") then
			return (tokenError(context, (visited.mode or startTok), "[%s] Bad draw mode '%s'. Must be 'fill' or 'line'.", command, args.mode))
		end

		local lw = (args.mode == "fill") and 0 or args.thick

		ensureCanvasAndInitted(context)

		applyCanvas(context, nil)
		applyColor(context, nil, "rectangle", args.w+lw,args.h+lw)

		local segs = (
			args.segs > 0
			and args.segs
			or  math.round(math.max(math.max(args.tlrx,args.tlry, args.trrx,args.trry, args.brrx,args.brry, args.blrx,args.blry) * TAU/10, 64) * .25)
		)

		LG.push()
		if args.origin then  LG.origin()  end
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-args.ax*args.w, -args.ay*args.h)

		if     args.mode == "fill" then  drawRectangleFill(0,0, args.w,args.h, args.tlrx,args.tlry, args.trrx,args.trry, args.brrx,args.brry, args.blrx,args.blry, segs)
		elseif args.mode == "line" then  drawRectangleLine(0,0, args.w,args.h, args.tlrx,args.tlry, args.trrx,args.trry, args.brrx,args.brry, args.blrx,args.blry, segs, lw)
		else error(args.mode) end

		LG.pop()

	----------------------------------------------------------------
	elseif command == "circle" then
		if not (args.mode == "fill" or args.mode == "fillclosed" or args.mode == "line" or args.mode == "linepie" or args.mode == "lineclosed") then
			return (tokenError(context, (visited.mode or startTok), "[%s] Bad draw mode '%s'. Must be 'fill' or 'line'.", command, args.mode))
		end

		local lw = (args.mode == "fill" or args.mode == "fillclosed") and 0 or args.thick

		local angle1 = args.from
		local angle2 = math.clamp(args.to, args.from-TAU, args.from+TAU)

		local segs = (
			args.segs > 0
			and args.segs
			or  math.round(math.max(math.max(args.rx, args.ry) * TAU/10, 64) * math.abs(angle2-angle1)/TAU)
		)

		ensureCanvasAndInitted(context)

		applyCanvas(context, nil)
		applyColor(context, nil, "circle", args.rx+.5*lw,args.ry+.5*lw)

		LG.push()
		if args.origin then  LG.origin()  end
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-(args.ax*2-1)*args.rx, -(args.ay*2-1)*args.ry)

		if     args.mode == "fill"       then  drawCircleFill(0,0, args.rx,args.ry, angle1,angle2, false   , segs)
		elseif args.mode == "fillclosed" then  drawCircleFill(0,0, args.rx,args.ry, angle1,angle2, true    , segs)
		elseif args.mode == "line"       then  drawCircleLine(0,0, args.rx,args.ry, angle1,angle2, "open"  , segs, lw)
		elseif args.mode == "linepie"    then  drawCircleLine(0,0, args.rx,args.ry, angle1,angle2, "pie"   , segs, lw)
		elseif args.mode == "lineclosed" then  drawCircleLine(0,0, args.rx,args.ry, angle1,angle2, "closed", segs, lw)
		else error(args.mode) end

		LG.pop()

	----------------------------------------------------------------
	elseif command == "poly" then
		if not (args.mode == "fill" or args.mode == "strip" or args.mode == "line") then
			return (tokenError(context, (visited.mode or startTok), "[%s] Bad draw mode '%s'. Must be 'fill' or 'line'.", command, args.mode))
		end

		if not context.points[3] then  return (tokenError(context, startTok, "[%s] Not enough points added.", command))  end

		local coords = pointsToCoords(context.points)
		local lw     = (args.mode == "line") and args.thick or 0

		local x1 =  1/0
		local x2 = -1/0
		local y1 =  1/0
		local y2 = -1/0

		for i = 1, #coords, 2 do
			x1 = math.min(x1, coords[i  ])
			x2 = math.max(x2, coords[i  ])
			y1 = math.min(y1, coords[i+1])
			y2 = math.max(y2, coords[i+1])
		end

		local colorW = x2 - x1 + lw
		local colorH = y2 - y1 + lw

		if not args.shift then
			x1, y1 = 0, 0
		end

		ensureCanvasAndInitted(context)

		applyCanvas(context, nil)
		applyColor(context, nil, "rectangle", colorW,colorH)

		LG.push()
		if args.origin then  LG.origin()  end
		LG.translate(args.x+x1, args.y+y1)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-args.ax*(x2-x1), -args.ay*(y2-y1))
		LG.translate(-x1, -y1)

		if     args.mode == "fill"  then  drawPolygonFill(coords, false)
		elseif args.mode == "strip" then  drawPolygonFill(coords, true)
		elseif args.mode == "line"  then  drawPolygonLine(coords, lw)
		else error(args.mode) end

		LG.pop()

	----------------------------------------------------------------
	elseif command == "line" or command == "bezier" then
		if not context.points[2] then  return (tokenError(context, startTok, "[%s] Not enough points added.", command))  end

		local coords = pointsToCoords(context.points)

		if command == "bezier" then
			local curve = love.math.newBezierCurve(coords)
			coords      = curve:render(args.depth)
			curve:release()
		end

		local x1 =  1/0
		local x2 = -1/0
		local y1 =  1/0
		local y2 = -1/0

		for i = 1, #coords, 2 do
			x1 = math.min(x1, coords[i  ])
			x2 = math.max(x2, coords[i  ])
			y1 = math.min(y1, coords[i+1])
			y2 = math.max(y2, coords[i+1])
		end

		local colorW = x2 - x1 + args.thick -- Note: We assume the line extends by the thickness in both directions from the coords
		local colorH = y2 - y1 + args.thick --       which isn't exactly correct, but it's good enough. That's art, baby!

		if not args.shift then
			x1, y1 = 0, 0
		end

		ensureCanvasAndInitted(context)

		applyCanvas(context, nil)
		applyColor(context, nil, "rectangle", colorW,colorH)

		LG.push()
		if args.origin then  LG.origin()  end
		LG.translate(args.x+x1, args.y+y1)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-args.ax*(x2-x1), -args.ay*(y2-y1))
		LG.translate(-x1, -y1)
		drawLine(coords, args.thick)
		LG.pop()

	----------------------------------------------------------------
	elseif command == "text" then
		if not (args.align == "left" or args.align == "center" or args.align == "right" or args.align == "justify") then
			return (tokenError(context, (visited.align or startTok), "[%s] Bad alignment '%s'. Must be 'left', 'center', 'right' or 'justify'.", command, args.align))
		end

		ensureCanvasAndInitted(context)

		local font         = context.gfxState.font
		local w, textLines = font:getWrap(args.text, args.wrap)
		local h            = (#textLines-1) * math.floor(font:getHeight()*args.lineh) + font:getHeight()

		if not font:hasGlyphs(args.text) then
			tokenWarning(context, (visited.text or startTok), "[%s] The current font is missing some glyphs for the text. Skipping those characters.", command)
		end

		font:setFilter(args.filter and "linear" or "nearest")
		font:setLineHeight(args.lineh)
		applyCanvas(context, nil)
		applyColor(context, nil, "rectangle", 1,1) -- @Incomplete: Handle gradients for text/glyphs like the other shapes, somehow.
		LG.setFont(font)

		LG.push()
		if args.origin then  LG.origin()  end
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(math.round(-args.ax*w), math.round(-args.ay*h)) -- Note: Text is the only thing we round the origin for.
		shaderSend(A.shaders.main, "textBlendFix", true)
		LG.printf(args.text, 0,0, w, args.align)
		shaderSend(A.shaders.main, "textBlendFix", false)
		LG.pop()

	----------------------------------------------------------------
	elseif command == "buf" or command == "pat" then
		ensureCanvasAndInitted(context)

		local k       = (command == "buf" and "name" or "buf")
		local bufName = args[k]
		local texture = (bufName == "") and context.art.canvas or context.buffers[bufName]
		if not texture then  return (tokenError(context, (visited[k] or startTok), "[%s] No buffer '%s'.", command, bufName))  end

		if bufName == "" then
			if texture == context.gfxState.canvas then
				return (tokenError(context, (visited[k] or startTok), "[%s] Cannot draw canvas to itself.", command, bufName))
			end
		elseif texture == context.gfxState.canvas then
			gfxStateSetCanvas(context, nil) -- :SetMainBuffer
			-- LG.setCanvas(nil) ; texture:newImageData():encode("png", "buffer.png"):release() -- DEBUG
		end

		local iw,ih = texture:getDimensions()
		applyCanvas(context, nil)
		applyColor(context, nil, "rectangle", iw,ih)
		texture:setFilter(args.filter and "linear" or "nearest")

		LG.push()
		if args.origin then  LG.origin()  end
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-args.ax*iw, -args.ay*ih)

		if command == "buf" then
			texture:setWrap("clamp")
			LG.draw(texture)

		else
			LG.scale(iw, ih)
			local cw ,ch = context.gfxState.canvas:getDimensions()
			local u1, v1 = LG.inverseTransformPoint(0 , 0 )
			local u2, v2 = LG.inverseTransformPoint(cw, 0 )
			local u3, v3 = LG.inverseTransformPoint(cw, ch)
			local u4, v4 = LG.inverseTransformPoint(0 , ch)

			LG.origin()
			texture:setWrap((args.mirrorx and "mirroredrepeat" or "repeat"), (args.mirrory and "mirroredrepeat" or "repeat"))
			drawQuad(texture,  0,0, cw,0, cw,ch, 0,ch,  u1,v1, u2,v2, u3,v3, u4,v4)
		end

		LG.pop()

	----------------------------------------------------------------
	elseif command == "iso" then
		local glslInitPoints = {}
		local initValue, defaultA,defaultB, glslDebug, glslEachPoint

		if args.mode == "sdf" then
			initValue, defaultA,defaultB, glslDebug, glslEachPoint = -999999, 0,0, ".04*(result-$threshold)", [[//GLSL
				#define RADIUS z//a
				float distToEdge = distance(texUv, points[i].xy) - points[i].RADIUS;
				result           = -min(-result, distToEdge);
			]]
		elseif args.mode == "sdfsmooth" then
			initValue, defaultA,defaultB, glslDebug, glslEachPoint = -999999, 0,0, ".04*(result-$threshold)", [[//GLSL
				#define RADIUS z//a
				float distToEdge = distance(texUv, points[i].xy) - points[i].RADIUS;
				result           = -sdfSmoothMin(-result, distToEdge);
			]]
		elseif args.mode == "metaball" then
			initValue, defaultA,defaultB, glslDebug, glslEachPoint = -1, 1,0, "result-$threshold", [[//GLSL
				#define WEIGHT z//a
				#define OFFSET w//b
				float dist = max(distance(texUv, points[i].xy)-points[i].OFFSET, 0);
				result     = result + points[i].WEIGHT / dist;
			]]
		elseif args.mode == "metaballramp" then
			initValue, defaultA,defaultB, glslDebug, glslEachPoint = -1, 1,2, "result-$threshold", [[//GLSL
				#define RADIUS z//a
				#define HEIGHT w//b
				#define WEIGHT w//b
				float dist  = distance(texUv, points[i].xy);
				float r     = max(points[i].RADIUS, 0);
				float h     = points[i].HEIGHT;
				float halfH = max(abs(h/2), 1.); // Without the clamping, a small height would increase the area of influence way beyond the specified radius.
				result      = result + h * smoothstep(-r/(halfH*halfH), r/halfH, r-dist); // We want the smoothstep to happen around the radius.
				// result      = result + 2*points[i].WEIGHT * smoothstep(-2*r, 0, -dist); // WEIGHT!=1 messes with threshold=0 in an unpredictable way. No good.
			]]
		else
			return (tokenError(context, (visited.mode or startTok), "[%s] Bad mode '%s'. Must be 'sdf' or 'metaball'.", command, args.mode))
		end

		for i, point in ipairs(context.points) do
			table.insert(glslInitPoints, F(
				"points[%d] = vec4(%s,%s, %s,%s);",
				i - 1,
				tostringFloat(point.x),
				tostringFloat(point.y),
				tostringFloat((point.a == point.a) and point.a or defaultA),
				tostringFloat((point.b == point.b) and point.b or defaultB)
			))
		end

		local glsl = [[//GLSL
			#include "_main.gl"

			float sdfSmoothMin(float a, float b) {
				float res = exp(-$k*a) + exp(-$k*b);
				return -log(max(.0001, res)) / $k;
			}

			vec4 effect(vec4 loveColor, Image tex, vec2 texUv, vec2 screenPos) {
				vec4 points[$pointCount];
				$INIT_POINTS

				float result = $initValue;

				for (int i = 0; i < $pointCount; i++) {
					$EACH_POINT
				}

				if ($debug) {
					float v = $DEBUG;
					return (v < 0) ? vec4(vec3(max(.7+.7*v,0)), 1) : vec4(1, vec2(max(1-v,0)), 1);
				} else if (result > $threshold && result < $limit) {
					return applyMainEffect(vec4(1), texUv, screenPos, loveColor);
				} else {
					discard;
				}

			}
		]]
		glsl = (glsl
			:gsub("%$([%a_][%w_]*)", {
				INIT_POINTS = table.concat(glslInitPoints, "\n"),
				EACH_POINT  = glslEachPoint,
				DEBUG       = glslDebug,
			})
			:gsub("%$([%a_][%w_]*)", {
				pointCount = "("..tostring(#context.points)..")",
				initValue  = "("..tostringFloat(initValue)..")",
				threshold  = "("..tostringFloat(args.thres)..")",
				limit      = "("..tostringFloat(args.limit)..")",
				k          = "("..tostringFloat(args.k)..")",
				debug      = "("..tostring(args.debug)..")",
			})
			:gsub("%$([%a_][%w_]*)", error)
		)

		local ok, shaderOrErr = pcall(newShader, glsl, "<sdf>")
		if not ok then
			tokenError(context, startTok, "[%s] Internal error: Failed creating shader. (%s)", command, shaderOrErr)
			if DEV then
				local ln = 1
				print("1: "..glsl:gsub("\n", function()
					ln = ln + 1
					return "\n"..ln..": "
				end))
			end
			return nil
		end

		ensureCanvasAndInitted(context)

		applyCanvas(context, shaderOrErr)
		applyColor(context, shaderOrErr, "circle", 1,1) -- @Incomplete: Handle gradients better for isolines.
		LG.push()

		if args.origin then  LG.origin()  end
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)

		local cw, ch = context.gfxState.canvas:getDimensions()
		local u1, v1 = LG.inverseTransformPoint(0 , 0 )
		local u2, v2 = LG.inverseTransformPoint(cw, 0 )
		local u3, v3 = LG.inverseTransformPoint(cw, ch)
		local u4, v4 = LG.inverseTransformPoint(0 , ch)

		LG.origin()
		drawQuad(A.images.rectangle,  0,0, cw,0, cw,ch, 0,ch,  u1,v1, u2,v2, u3,v3, u4,v4)

		LG.pop()
		shaderOrErr.shader:release()

	----------------------------------------------------------------
	elseif command == "quad" then
		-- @Incomplete: Maybe use context.points instead? Point format would be {x,y,u,v}.
		local x1 = math.min(args.cax, args.cbx, args.ccx, args.cdx) -- Similar to polygons/lines.
		local x2 = math.max(args.cax, args.cbx, args.ccx, args.cdx)
		local y1 = math.min(args.cay, args.cby, args.ccy, args.cdy)
		local y2 = math.max(args.cay, args.cby, args.ccy, args.cdy)

		local colorW = x2 - x1
		local colorH = y2 - y1

		if not args.shift then
			x1, y1 = 0, 0
		end

		ensureCanvasAndInitted(context)

		local bufName = args.buf
		local texture = (bufName == "") and context.art.canvas or context.buffers[bufName]
		if not texture then  return (tokenError(context, (visited.buf or startTok), "[%s] No buffer '%s'.", command, bufName))  end

		if bufName == "" then
			if texture == context.gfxState.canvas then
				return (tokenError(context, (visited.buf or startTok), "[%s] Cannot draw canvas to itself.", command, bufName))
			end
		elseif texture == context.gfxState.canvas then
			gfxStateSetCanvas(context, nil) -- :SetMainBuffer
		end

		applyCanvas(context, A.shaders.quad)
		applyColor(context, A.shaders.quad, "rectangle", colorW,colorH)
		texture:setFilter(args.filter and "linear" or "nearest")
		texture:setWrap("clamp")

		LG.push()
		if args.origin then  LG.origin()  end
		LG.translate(args.x+x1, args.y+y1)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-args.ax*(x2-x1), -args.ay*(y2-y1))
		LG.translate(-x1, -y1)
		drawPerspectiveCorrectQuad(texture,
			args.cax,args.cay, args.cbx,args.cby, args.ccx,args.ccy, args.cdx,args.cdy,
			args.cau,args.cav, args.cbu,args.cbv, args.ccu,args.ccv, args.cdu,args.cdv
		)
		LG.pop()

	--
	-- Effects.
	--
	----------------------------------------------------------------
	elseif command == "boxblur" then
		applyEffect(context, A.shaders.fxBlurBox, function(context, canvasRead, canvasWrite)
			shaderSend    (A.shaders.fxBlurBox, "radius"   , math.clamp(args.x, 0, 1000))
			shaderSendVec2(A.shaders.fxBlurBox, "direction", .5, 0)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead

			shaderSend    (A.shaders.fxBlurBox, "radius"   , math.clamp(args.y, 0, 1000))
			shaderSendVec2(A.shaders.fxBlurBox, "direction", 0, .5)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead

			return canvasRead
		end)

	elseif command == "blur" then
		applyEffect(context, A.shaders.fxBlurGaussian, function(context, canvasRead, canvasWrite)
			local BLUR_SIZE  = 13 -- 5|9|13 (See fxBlurGaussian.gl)
			local BLUR_REACH = ((BLUR_SIZE==5 and 2.5) or (BLUR_SIZE==13 and 3.5) or 3) / BLUR_SIZE -- Magic inaccurate numbers... @Cleanup

			-- @Incomplete: These loops are probably not exactly correct. I think we wanna
			-- double the radius each iteration (and iterate fewer times).
			shaderSendVec2(A.shaders.fxBlurGaussian, "direction", BLUR_REACH,0)
			for _ = 1, math.clamp(args.x, 0, 1000) do
				LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
				canvasRead, canvasWrite = canvasWrite, canvasRead
			end
			shaderSendVec2(A.shaders.fxBlurGaussian, "direction", 0,BLUR_REACH)
			for _ = 1, math.clamp(args.y, 0, 1000) do
				LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
				canvasRead, canvasWrite = canvasWrite, canvasRead
			end

			return canvasRead
		end)

	----------------------------------------------------------------
	elseif command == "contrast" then
		applyEffect(context, A.shaders.fxContrast, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxContrast, "params", args.r,args.g,args.b, args.amount) -- rgb is a channel filter.
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "brightness" then
		applyEffect(context, A.shaders.fxBrightness, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxBrightness, "params", args.r,args.g,args.b, args.amount) -- rgb is a channel filter.
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "saturation" then
		applyEffect(context, A.shaders.fxSaturation, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxSaturation, "params", args.r,args.g,args.b, args.amount) -- rgb is a channel filter.
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "gamma" then
		applyEffect(context, A.shaders.fxGamma, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxGamma, "params", args.r,args.g,args.b, args.amount) -- rgb is a channel filter.
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "replace" then
		applyEffect(context, A.shaders.fxReplace, function(context, canvasRead, canvasWrite)
			shaderSendVec3(A.shaders.fxReplace, "target", args.r,args.g,args.b)
			shaderSendVec2(A.shaders.fxReplace, "params", args.reach, args.ramp)
			applyColor(context, A.shaders.fxReplace, "rectangle", context.gfxState.canvas:getDimensions())
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "levels" then
		applyEffect(context, A.shaders.fxLevels, function(context, canvasRead, canvasWrite)
			shaderSendVec2(A.shaders.fxLevels, "rangeIn" , args.infrom,args.into)
			shaderSendVec2(A.shaders.fxLevels, "rangeOut", args.outfrom,args.outto)
			shaderSend    (A.shaders.fxLevels, "middle"  , args.mid)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "thres" then
		applyEffect(context, A.shaders.fxThreshold, function(context, canvasRead, canvasWrite)
			shaderSend(A.shaders.fxThreshold, "threshold", args.thres)
			shaderSend(A.shaders.fxThreshold, "alphaMode", args.alpha)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "overlay" then -- Fade pixels toward color.
		applyEffect(context, A.shaders.fxOverlay, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxOverlay, "params", args.r,args.g,args.b,args.a)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "tint" then -- Change hue and saturation, leave brighness.
		applyEffect(context, A.shaders.fxTint, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxTint, "params", args.r,args.g,args.b,args.a)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "remap" then
		if not CHANNEL_MAPPING_INDICES[args.r] then  return (tokenError(context, (visited.r or startTok), "[%s] Invalid channel '%s'. Must be 'r', 'g', 'b', 'a', '0' or '1'.", command, args.r))  end
		if not CHANNEL_MAPPING_INDICES[args.g] then  return (tokenError(context, (visited.g or startTok), "[%s] Invalid channel '%s'. Must be 'r', 'g', 'b', 'a', '0' or '1'.", command, args.g))  end
		if not CHANNEL_MAPPING_INDICES[args.b] then  return (tokenError(context, (visited.b or startTok), "[%s] Invalid channel '%s'. Must be 'r', 'g', 'b', 'a', '0' or '1'.", command, args.b))  end
		if not CHANNEL_MAPPING_INDICES[args.a] then  return (tokenError(context, (visited.a or startTok), "[%s] Invalid channel '%s'. Must be 'r', 'g', 'b', 'a', '0' or '1'.", command, args.a))  end

		applyEffect(context, A.shaders.fxRemap, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxRemap, "mappings", CHANNEL_MAPPING_INDICES[args.r],CHANNEL_MAPPING_INDICES[args.g],CHANNEL_MAPPING_INDICES[args.b],CHANNEL_MAPPING_INDICES[args.a])
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	--
	-- Generators.
	--
	----------------------------------------------------------------
	elseif command == "noise" or command == "clouds" then
		ensureCanvasAndInitted(context)

		local gfxState = context.gfxState
		local canvas   = gfxState.canvas
		local cw,ch    = canvas:getDimensions()
		local iw,ih    = A.images.rectangle:getDimensions()

		applyCanvas(context, A.shaders.generateNoise)
		applyColor(context, A.shaders.generateNoise, "rectangle", 1,1)

		local r,g,b,a = unpack(gfxState.flatColor)
		shaderSend    (A.shaders.generateNoise, "clouds"       , (command == "clouds"))
		shaderSendVec3(A.shaders.generateNoise, "offset"       , args.x,args.y,(args.z or 0))
		shaderSendVec2(A.shaders.generateNoise, "scale"        , args.sx,args.sy)
		shaderSendVec4(A.shaders.generateNoise, "gradientColor", r*a,g*a,b*a,a)

		LG.push()
		LG.origin()
		LG.draw(A.images.rectangle, 0,0, 0, cw/iw,ch/ih)
		LG.pop()

	elseif command == "random" then
		ensureCanvasAndInitted(context)

		local canvas = context.gfxState.canvas
		local cw,ch  = canvas:getDimensions()

		local rng       = love.math.newRandomGenerator(args.seed) -- Note: We don't use context.rng - this should be separate.
		local imageData = love.image.newImageData(cw,ch)

		imageData:mapPixel(function(x,y, r,g,b,a) -- @Speed
			r = rng:random()
			g = rng:random()
			b = rng:random()

			if rng:random() > args.level then  return 0,0,0,0  end

			return r
			     , math.lerp(r, g, args.color)
			     , math.lerp(r, b, args.color)
			     , 1
		end)

		local image = LG.newImage(imageData)
		image:setFilter("nearest")

		applyCanvas(context, nil)
		applyColor(context, nil, "rectangle", cw,ch)

		LG.push()
		LG.origin()
		LG.draw(image)
		LG.pop()

		image:release()
		imageData:release()

	----------------------------------------------------------------
	elseif command == "end" or command == "else" or command == "elseif" then
		return (tokenError(context, startTok, "Unexpected '%s'.", command))
	else
		return (tokenError(context, startTok, "Internal error: Unimplemented command '%s'.", command))
	end
	return tokPos, STOP_CONTINUE
end

-- tokenPosition|nil, stop = runBlock( context, tokens, tokenPosition )
--[[local]] function runBlock(context, tokens, tokPos)
	local stop

	while true do
		while isToken(tokens[tokPos], "linebreak") or isToken(tokens[tokPos], ";") do
			tokPos = tokPos + 1
		end
		if not tokens[tokPos] then  return tokPos  end

		local commandTok = tokens[tokPos]

		tokPos, stop = runCommand(context, tokens, tokPos, nil)
		if not tokPos       then  return nil                 end
		if stop == STOP_ONE then  return 1/0, STOP_CONTINUE  end
		if stop == STOP_ALL then  return 1/0, STOP_ALL       end

		if not (isToken(tok, "name") and SCOPE_ENTER_COMMANDS[tok.value]) then
			while isToken(tokens[tokPos], ",") do
				tokPos       = tokPos + 1 -- ','
				tokPos, stop = runCommand(context, tokens, tokPos, commandTok)
				if not tokPos       then  return nil                 end
				if stop == STOP_ONE then  return 1/0, STOP_CONTINUE  end
				if stop == STOP_ALL then  return 1/0, STOP_ALL       end
			end
		elseif isToken(tokens[tokPos], ",") then -- Comma after 'end'.
			return (tokenError(context, tokens[tokPos], "Invalid ','."))
		end
	end
end



local function cleanup(context, loveGfxStackDepth, success)
	for i = loveGfxStackDepth+1, LG.getStackDepth() do
		LG.pop() -- We probably don't have to call popGfxState() on the whole gfxStack, but we could if needed.
	end
	LG.reset()

	if not success and context.art.canvas then  context.art.canvas:release()  end

	if context.workCanvas1  then  context.workCanvas1:release()  end
	if context.workCanvas2  then  context.workCanvas2:release()  end

	for _, texture in pairs(context.images     ) do  texture:release()  end
	for _, canvas  in pairs(context.buffers    ) do  canvas :release()  end
	for _, font    in pairs(context.fontsByPath) do  font   :release()  end

	for _, fonts in pairs(context.fontsBySize) do
		for _, font in pairs(fonts) do  font:release()  end
	end
end

local function getReversedArray(arr)
	local reversed = {}
	for i = #arr, 1, -1 do
		table.insert(reversed, arr[i])
	end
	return reversed
end

-- art|nil = loadArtFile( path, isLocal )
-- Note: art.canvas has premultiplied alpha.
function _G.loadArtFile(path, isLocal)
	artFilesBeingLoaded[path] = artFilesBeingLoaded[path] or 0

	if artFilesBeingLoaded[path] > (artFileRecursionAllowed[path] or 0) then
		print("Error: File is already being loaded: "..path)
		return nil
	end

	if artFilesBeingLoaded[path] == 0 then  printf("Loading %s...", path)  end
	local startTime = love.timer.getTime()
	LG.reset()

	local context    = Context()
	context.art      = Art()
	context.gfxState = GfxState()
	context.path     = path
	context.isLocal  = isLocal

	local s, err = readTextFile(isLocal, path)
	if not s then  print("Error: "..err) ; return nil  end
	context.source = s

	local tokens = tokenize(context, path)
	if not tokens then  return nil  end

	for command, commandInfo in pairs(COMMANDS) do
		context.commands[command] = {} -- @Cleanup: Only copy the default values, not all command info!
		for k, argInfoOrAliases in pairs(commandInfo) do
			if type(k) == "number" then -- Argument.
				table.insert(context.commands[command], {name=argInfoOrAliases.name, value=argInfoOrAliases.value})
			else -- Alias.
				context.commands[command][k] = argInfoOrAliases
			end
		end
	end

	local function getRandom(...)
		return context.rng and context.rng:random(...) or love.math.random(...)
	end

	local function getRandomCircle(radius, x, y)
		local a = getRandom()
		local b = getRandom()
		if b < a then  a, b = b, a  end

		local angle = a / b * TAU
		b           = (radius or 1) * b

		return {
			X = (x or 0) + b*math.cos(angle),
			Y = (y or 0) + b*math.sin(angle),
		}
	end

	local vars     = context.readonly
	local dummyTok = {position=1, type="dummy"}

	-- Constants.
	vars.True  = {token=dummyTok, value=true}
	vars.False = {token=dummyTok, value=false}
	vars.Huge  = {token=dummyTok, value=1/0}
	vars.Nan   = {token=dummyTok, value=0/0}
	vars.Pi    = {token=dummyTok, value=math.pi}
	vars.Tau   = {token=dummyTok, value=TAU}

	-- Dynamic values.
	vars.WindowW    = {token=dummyTok, value=LG.getWidth()}
	vars.WindowH    = {token=dummyTok, value=LG.getHeight()}
	vars.CanvasW    = {token=dummyTok, value=context.canvasWidth}
	vars.CanvasH    = {token=dummyTok, value=context.canvasHeight}
	vars.PointCount = {token=dummyTok, value=#context.points}

	-- These functions are just for the Lua/bracket expressions.
	-- @Incomplete @Robustness: Argument validation.

	-- Functions: misc.
	vars.num     = {token=dummyTok, value=tonumber}
	vars.rev     = {token=dummyTok, value=function(v)  return type(v) == "table" and getReversedArray(v) or string.reverse(v)  end}
	vars.sel     = {token=dummyTok, value=function(n, ...)  return (select(n, ...))  end}
	vars.selx    = {token=dummyTok, value=select}
	vars.str     = {token=dummyTok, value=tostring}
	vars.type    = {token=dummyTok, value=type}

	vars.default = {token=dummyTok, value=function(command, k, defaultDefault) -- default( command, argumentName [, getDefaultDefault=false ] )
		local commandInfo = (defaultDefault and COMMANDS or context.commands)[command] or errorf(2, "No command '%s'.", tostring(command))
		local argInfo     = itemWith1(commandInfo, "name", k)                          or errorf(2, "Command '%s' has no argument '%s'.", command, tostring(k))
		return argInfo.value
	end}

	-- Functions: math and numbers.
	vars.abs    = {token=dummyTok, value=math.abs}
	vars.acos   = {token=dummyTok, value=math.acos}
	vars.asin   = {token=dummyTok, value=math.asin}
	vars.atan   = {token=dummyTok, value=function(y, x)  return x and math.atan2(y, x) or math.atan(y)  end} -- atan( y [, x=1 ] )
	vars.ceil   = {token=dummyTok, value=math.ceil}
	vars.cos    = {token=dummyTok, value=math.cos}
	vars.cosh   = {token=dummyTok, value=math.cosh}
	vars.deg    = {token=dummyTok, value=math.deg}
	vars.exp    = {token=dummyTok, value=math.exp}
	vars.floor  = {token=dummyTok, value=math.floor}
	vars.fmod   = {token=dummyTok, value=math.fmod}
	vars.frac   = {token=dummyTok, value=function(n)  local int, frac = math.modf(n) ; return frac  end}
	vars.frexp  = {token=dummyTok, value=function(n)  local m, e = math.frexp(n) ; return {M=m, E=e}  end}
	vars.frexpe = {token=dummyTok, value=function(n)  local m, e = math.frexp(n) ; return e  end}
	vars.frexpm = {token=dummyTok, value=function(n)  return (math.frexp(n))  end}
	vars.int    = {token=dummyTok, value=function(n)  return (math.modf(n))  end}
	vars.ldexp  = {token=dummyTok, value=math.ldexp}
	vars.len    = {token=dummyTok, value=function(x, y, z)  return math.sqrt(x*x + (y and y*y or 0) + (z and z*z or 0))  end} -- len( x [, y=0, z=0 ] )
	vars.lerp   = {token=dummyTok, value=math.lerp}
	vars.log    = {token=dummyTok, value=math.log} -- log( n [, base=e ] )
	vars.max    = {token=dummyTok, value=math.max}
	vars.min    = {token=dummyTok, value=math.min}
	vars.modf   = {token=dummyTok, value=function(n)  local int, frac = math.modf(n) ; return {I=int, F=frac}  end}
	vars.noise  = {token=dummyTok, value=love.math.noise} -- noise( x [, y, z, w ] )
	vars.norm   = {token=dummyTok, value=math.normalize} -- normalize( v1, v2, v )
	vars.rad    = {token=dummyTok, value=math.rad}
	vars.rand   = {token=dummyTok, value=getRandom}
	vars.randf  = {token=dummyTok, value=function(n1, n2)  return n2 and n1+(n2-n1)*getRandom() or (n1 or 1)*getRandom()  end} -- randf( [[ n1=0, ] n2=1 ] )
	vars.randc  = {token=dummyTok, value=getRandomCircle} -- randc( [ radius=1, centerX=0, centerY=0 ] )
	vars.round  = {token=dummyTok, value=math.round}
	vars.sin    = {token=dummyTok, value=math.sin}
	vars.sinh   = {token=dummyTok, value=math.sinh}
	vars.sqrt   = {token=dummyTok, value=math.sqrt}
	vars.tan    = {token=dummyTok, value=math.tan}
	vars.tanh   = {token=dummyTok, value=math.tanh}

	-- Functions: OS.
	vars.clock = {token=dummyTok, value=os.clock}
	vars.date  = {token=dummyTok, value=os.date}
	vars.env   = {token=dummyTok, value=os.getenv}
	vars.time  = {token=dummyTok, value=os.time}

	-- Functions: strings.
	vars.byte   = {token=dummyTok, value=string.byte} -- @Robustness: Handle the string metatable.
	vars.char   = {token=dummyTok, value=string.char}
	vars.find   = {token=dummyTok, value=string.find}
	vars.format = {token=dummyTok, value=F}
	vars.gsub   = {token=dummyTok, value=function(s, pat, repl, n)  return (string.gsub(s, pat, repl, n))  end}
	vars.lower  = {token=dummyTok, value=string.lower}
	vars.match  = {token=dummyTok, value=string.match}
	vars.rep    = {token=dummyTok, value=string.rep}
	vars.sub    = {token=dummyTok, value=string.sub}
	vars.upper  = {token=dummyTok, value=string.upper}
	-- Also: rev

	-- Functions: arrays and tables.
	vars.unpack = {token=dummyTok, value=unpack}
	vars.concat = {token=dummyTok, value=table.concat}
	vars.sort   = {token=dummyTok, value=function(arr)     arr = {unpack(arr)} ; table.sort(arr                                         ) ; return arr  end}
	vars.sortby = {token=dummyTok, value=function(arr, k)  arr = {unpack(arr)} ; table.sort(arr, function(a, b)  return a[k] < b[k]  end) ; return arr  end}
	-- Also: rev

	-- Functions: coordinate system.
	vars.screen  = {token=dummyTok, value=function(x, y)  x, y = LG.transformPoint       (x, y) ; return {X=x, Y=y}  end}
	vars.screenx = {token=dummyTok, value=function(x, y)  x, y = LG.transformPoint       (x, y) ; return x           end}
	vars.screeny = {token=dummyTok, value=function(x, y)  x, y = LG.transformPoint       (x, y) ; return y           end}
	vars.world   = {token=dummyTok, value=function(x, y)  x, y = LG.inverseTransformPoint(x, y) ; return {X=x, Y=y}  end}
	vars.worldx  = {token=dummyTok, value=function(x, y)  x, y = LG.inverseTransformPoint(x, y) ; return x           end}
	vars.worldy  = {token=dummyTok, value=function(x, y)  x, y = LG.inverseTransformPoint(x, y) ; return y           end}

	-- Functions: points.
	vars.point    = {token=dummyTok, value=function(i)  local point = context.points[i] ; return point and {x=point.x,y=point.y, a=point.a,b=point.b, s=point.s}  end}
	vars.pointx   = {token=dummyTok, value=function(i)  local point = context.points[i] ; return point and point.x  end}
	vars.pointy   = {token=dummyTok, value=function(i)  local point = context.points[i] ; return point and point.y  end}
	vars.pointa   = {token=dummyTok, value=function(i)  local point = context.points[i] ; return point and point.a  end}
	vars.pointb   = {token=dummyTok, value=function(i)  local point = context.points[i] ; return point and point.b  end}
	vars.pointstr = {token=dummyTok, value=function(i)  local point = context.points[i] ; return point and point.s  end}

	-- Functions: images and buffers.
	vars.imagewh = {token=dummyTok, value=function(path)  if not context.images[path] then errorf(2, "No image '%s' loaded.", path) end ; local w, h = context.images[path]:getDimensions() ; return {W=w, H=h}  end}
	vars.imagew  = {token=dummyTok, value=function(path)  return context.images[path] and context.images[path]:getWidth () or errorf(2, "No image '%s' loaded.", path)  end}
	vars.imageh  = {token=dummyTok, value=function(path)  return context.images[path] and context.images[path]:getHeight() or errorf(2, "No image '%s' loaded.", path)  end}

	vars.bufwh = {token=dummyTok, value=function(name)  if not context.buffers[name] then errorf(2, "No buffer '%s'.", name) end ; local w, h = context.buffers[name]:getDimensions() ; return {W=w, H=h}  end}
	vars.bufw  = {token=dummyTok, value=function(name)  return context.buffers[name] and context.buffers[name]:getWidth () or errorf(2, "No buffer '%s'.", name)  end}
	vars.bufh  = {token=dummyTok, value=function(name)  return context.buffers[name] and context.buffers[name]:getHeight() or errorf(2, "No buffer '%s'.", name)  end}
	vars.bufaa = {token=dummyTok, value=function(name)  return context.buffers[name] and context.buffers[name]:getMSAA()   or errorf(2, "No buffer '%s'.", name)  end}

	local entry = ScopeStackEntry()
	table.insert(context.scopeStack, entry)
	local loveGfxStackDepth = LG.getStackDepth()

	artFilesBeingLoaded[path] = artFilesBeingLoaded[path] + 1
	local tokPos              = runBlock(context, tokens, 1) -- Now things will happen!
	artFilesBeingLoaded[path] = artFilesBeingLoaded[path] - 1

	if not tokPos then
		cleanup(context, loveGfxStackDepth, false)
		return nil
	end
	if tokens[tokPos] then
		tokenError(context, tokens[tokPos], "Unexpected token.")
		cleanup(context, loveGfxStackDepth, false)
		return nil
	end

	assert(#context.scopeStack == 1)

	-- Success!
	cleanup(context, loveGfxStackDepth, true)

	if not context.art.canvas then
		print("Nothing was rendered!") -- Should we call ensureCanvasAndInitted()? Especially if an art file loads another.
	end

	if artFilesBeingLoaded[path] == 0 then  printf("Loading %s... done in %.2f seconds!", path, love.timer.getTime()-startTime)  end
	return context.art.canvas and context.art
end


