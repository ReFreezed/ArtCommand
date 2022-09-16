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
local MAX_LOOPS        = 10000

local STOP_CONTINUE = nil
local STOP_ONE      = 1
local STOP_ALL      = 2



local COMMANDS = {--[[
	command1 = { {argName1,defaultValue}, ... }, -- defaultValue=nil means the value can be any type.
	...
--]]}

local env = setmetatable({}, {__index=function(env, k)
	if k == "tau" then  return TAU  end
	error(k)
end})

for line in LF.lines"data/all.commands" do
	local command, argInfosStr = line:gsub("#.*$", ""):match"^(%a+)%s*(.*)$"

	if command then
		COMMANDS[command] = {}

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
				table.insert(COMMANDS[command], {k,v})
			end
		end
	end
end



local function Art()return{
	canvas   = nil,
	backdrop = {0,0,0,0},
	zoom     = 1.0,
}end

local function Context()return{
	art = nil,

	path    = "",
	isLocal = false,
	source  = "",

	scopeStack = {--[[ scopeStackEntry1, ... ]]},
	callStack  = {--[[ [funcInfo1]=true, ... ]]}, -- @Cleanup: Bad name. Not a stack - just bookkeeping.
	gfxStack   = {--[[ gfxState1, ... ]]},

	gfxState = nil, -- (including love.graphics transform)

	workCanvas1  = nil,
	workCanvas2  = nil,
	canvasToMask = nil,
	maskCanvas   = nil,

	images      = {--[[ [path1]=image|canvas, ... ]]},
	buffers     = {--[[ [name1]=image, ... ]]},
	fontsByPath = {--[[ [path1]=font, ... ]]}, -- Image fonts.
	fontsBySize = {--[[ [path1]={[size1]=font,...}, ... ]]}, -- Vector fonts.

	points = {{x=0,y=0, a=0/0,b=0/0, s=""}},

	-- Settings.
	canvasW = DEFAULT_ART_SIZE,
	canvasH = DEFAULT_ART_SIZE,
	msaa    = 1,
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
	stackType = "user", -- "user" | "makemask" | "applymask"

	-- "user"
	-- Color.
	colorMode           = "flatcolor", -- "flatcolor" | "gradient" | "texture"
	blendMode           = "alpha", -- "alpha" | "replace" | "screen" | "add" | "subtract" | "multiply" | "lighten" | "darken"
	flatColor           = {1,1,1,1},
	gradient            = {--[[ r1,g1,b1,a1, ... ]]},
	colorTexture        = nil,
	colorTextureRadial  = false,
	colorTextureFit     = true, -- Used with colorTextureRadial.
	colorTextureScaleX  = 1.0,
	colorTextureScaleY  = 1.0,
	colorTextureAngle   = 0.0,
	colorTextureOffsetX = 0.0,
	colorTextureOffsetY = 0.0,
	colorTextureBuffer  = "",
	-- Font.
	font = LG.getFont(),

	-- "makemask" & "applymask"
	canvas         = nil,
	fallbackCanvas = nil, -- @Cleanup: We only have this because of buffers. Add a currentBufferName field instead and infer what canvas to use from gfxStack! (Note: Separate from colorTextureBuffer.)

	-- "makemask"
	makeMaskMode = false,
}end

local function copyGfxState(gfxState,stackType)return{
	stackType = stackType or error("Missing 'stackType' argument."),

	colorMode           = (stackType == "user" or nil) and gfxState.colorMode,
	blendMode           = (stackType == "user" or nil) and gfxState.blendMode,
	flatColor           = (stackType == "user" or nil) and {unpack(gfxState.flatColor)},
	gradient            = (stackType == "user" or nil) and {unpack(gfxState.gradient)},
	colorTexture        = (stackType == "user" or nil) and gfxState.colorTexture,
	colorTextureRadial  = (stackType == "user" or nil) and gfxState.colorTextureRadial,
	colorTextureFit     = (stackType == "user" or nil) and gfxState.colorTextureFit,
	colorTextureScaleX  = (stackType == "user" or nil) and gfxState.colorTextureScaleX,
	colorTextureScaleY  = (stackType == "user" or nil) and gfxState.colorTextureScaleY,
	colorTextureAngle   = (stackType == "user" or nil) and gfxState.colorTextureAngle,
	colorTextureOffsetX = (stackType == "user" or nil) and gfxState.colorTextureOffsetX,
	colorTextureOffsetY = (stackType == "user" or nil) and gfxState.colorTextureOffsetY,
	colorTextureBuffer  = (stackType == "user" or nil) and gfxState.colorTextureBuffer,
	font                = (stackType == "user" or nil) and gfxState.font,

	canvas         = (stackType == "makemask" or stackType == "applymask" or nil) and gfxState.canvas,
	fallbackCanvas = (stackType == "makemask" or stackType == "applymask" or nil) and gfxState.fallbackCanvas,

	makeMaskMode = (stackType == "makemask" or nil) and gfxState.makeMaskMode,
}end

local function moveGfxState(fromGfxState, toGfxState)
	if fromGfxState.stackType == "user" then
		toGfxState.colorMode           = fromGfxState.colorMode
		toGfxState.blendMode           = fromGfxState.blendMode
		toGfxState.flatColor           = fromGfxState.flatColor
		toGfxState.gradient            = fromGfxState.gradient
		toGfxState.colorTexture        = fromGfxState.colorTexture
		toGfxState.colorTextureRadial  = fromGfxState.colorTextureRadial
		toGfxState.colorTextureFit     = fromGfxState.colorTextureFit
		toGfxState.colorTextureScaleX  = fromGfxState.colorTextureScaleX
		toGfxState.colorTextureScaleY  = fromGfxState.colorTextureScaleY
		toGfxState.colorTextureAngle   = fromGfxState.colorTextureAngle
		toGfxState.colorTextureOffsetX = fromGfxState.colorTextureOffsetX
		toGfxState.colorTextureOffsetY = fromGfxState.colorTextureOffsetY
		toGfxState.colorTextureBuffer  = fromGfxState.colorTextureBuffer
		toGfxState.font                = fromGfxState.font
	end
	if fromGfxState.stackType == "makemask" or fromGfxState.stackType == "applymask" then
		toGfxState.canvas         = fromGfxState.canvas
		toGfxState.fallbackCanvas = fromGfxState.fallbackCanvas
	end
	if fromGfxState.stackType == "makemask" then
		toGfxState.makeMaskMode = fromGfxState.makeMaskMode
	end
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

-- tokenError  ( context, token=atEnd, format, ... )
-- tokenWarning( context, token=atEnd, format, ... )
-- tokenMessage( context, token=atEnd, format, ... )
local function tokenError(context, tok, s, ...)
	local pos = (tok and tok.position) or context.source:find"%S%s*$" or #context.source
	printFileErrorAt(context.path, context.source, pos, s, ...)
	printCallStack(context, pos)
end
local function tokenWarning(context, tok, s, ...)
	local pos = (tok and tok.position) or context.source:find"%S%s*$" or #context.source
	printFileWarningAt(context.path, context.source, pos, s, ...)
end
local function tokenMessage(context, tok, s, ...)
	local pos = (tok and tok.position) or context.source:find"%S%s*$" or #context.source
	printFileMessageAt(context.path, context.source, pos, s, ...)
end



-- object|nil, stackIndex = findInStack( context, member, currentPosition, key )
local function findInStack(context, member, pos, k)
	for i = #context.scopeStack, 1, -1 do
		local obj = context.scopeStack[i][member][k]
		if obj and obj.token.position < pos then  return obj, i  end
	end
	return nil -- Not found!
end



-- gfxStateSetCanvas( context, canvas, fallbackCanvas=canvas )
local function gfxStateSetCanvas(context, canvas, fallbackCanvas)
	context.gfxState.canvas         = canvas
	context.gfxState.fallbackCanvas = fallbackCanvas or canvas
end



-- applyCanvas( context, shader=main )
local function applyCanvas(context, shader)
	shader = shader or A.shaders.main

	shaderSend(shader, "makeMaskMode", context.gfxState.makeMaskMode) -- Maybe not the best place for this, but eh...

	LG.setCanvas(context.gfxState.canvas)
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

	if gfxState.colorMode == "flatcolor" then
		local r,g,b,a = unpack(gfxState.flatColor)
		LG.setColor(r*a, g*a, b*a, a)
		shaderSend(shader, "useColorTexture", false)
		return
	end

	local texture      = gfxState.colorTexture
	local radial       = false
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

	else
		error(gfxState.colorMode)
	end

	shaderSend    (shader, "useColorTexture"         , true)
	shaderSend    (shader, "colorTexture"            , texture)
	shaderSend    (shader, "colorTextureRadial"      , radial)
	shaderSendVec4(shader, "colorTextureLayout"      , sx,sy, dirX,dirY)
	shaderSend    (shader, "colorTextureRadialOffset", radialOffset)
	shaderSendVec2(shader, "colorTextureOffset"      , gfxState.colorTextureOffsetX,gfxState.colorTextureOffsetY)
	shaderSendVec4(shader, "colorTextureScale"       , preSx,preSy, postSx,postSy)
end



local function ensureCanvasAndInitted(context)
	if context.art.canvas then  return  end

	local settingsCanvas = {
		format = "rgba16",
		msaa   = (context.msaa > 1 and context.msaa or nil),
	}
	context.art.canvas   = LG.newCanvas(context.canvasW,context.canvasH, settingsCanvas)
	context.workCanvas1  = LG.newCanvas(context.canvasW,context.canvasH, {format="rgba16"})
	context.workCanvas2  = LG.newCanvas(context.canvasW,context.canvasH, {format="rgba16"})
	context.canvasToMask = LG.newCanvas(context.canvasW,context.canvasH, settingsCanvas)
	context.maskCanvas   = LG.newCanvas(context.canvasW,context.canvasH, {format="r16", msaa=settingsCanvas.msaa})

	-- context.art.canvas:setFilter("nearest") -- Maybe there should be an app setting for this. @Incomplete
	context.canvasToMask:setFilter("nearest") -- Fixes mask fuzziness caused by MSAA.
	context.maskCanvas  :setFilter("nearest") -- Fixes mask fuzziness caused by MSAA.

	gfxStateSetCanvas(context, context.art.canvas, nil)
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

		-- Name: name OR +name OR -name
		elseif s:find("^[-+]?%l", pos) then
			local mod,namePos,name; mod, namePos, name, pos = s:match("^([-+]?)()(%l+)()", pos)
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
			local negate,namePos,name; negate, namePos, name, pos = s:match("^(%-?)()(%u[%w]*)()", pos)
			table.insert(tokens, {position=namePos, type="username", value=name, negated=(negate=="-")})

			if lastWasName and lastTok.type == "name" then  lastTok.hasAttachment = true  end

		-- Number: N OR +N OR -N
		elseif s:find("^[-+]?%.?%d", pos) then
			local nStr; nStr, pos = s:match("^%+?([-+.%w]*%%?)()", pos)
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

		-- Brackets: (lua_expression) OR {lua_expression_1,...}
		elseif s:find("^[({]", pos) then
			local i1    = pos
			local pat   = s:find("^%(", pos) and "[-\"'%[\n()]" or "[-\"'%[\n{}]"
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
						if not pos then  return (parseError(context, startPos, "Missing end bracket."))  end
					else -- Line comment.
						return (parseError(context, startPos, "Missing end bracket."))
					end

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

local function validateVariableName(context, tokForError, term, varName)
	if not varName:find"^%w+$" then  tokenError(context, tokForError, "Bad %s '%s'. Must only contain alphanumeric characters.", term, varName) ; return false  end
	if not varName:find"^%u"   then  tokenError(context, tokForError, "Bad %s '%s'. Must start with an uppercase letter."      , term, varName) ; return false  end
	return true
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
				argInfo  = itemWith1(argInfos, 1, argName) or error(argName)

				for _, _argName in ipairs(argNames) do
					if visited[_argName] then
						return (tokenError(context, startTok, "Duplicate argument '%s' (as part of '%s').", _argName, argName0))
					end
				end

			else
				argInfo = itemWith1(argInfos, 1, argName)
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
			local var     = findInStack(context, "variables", tokens[tokPos].position, varName)
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
					local var = findInStack(context, "variables", tokens[tokPos].position, k)
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
				if not visited[_argInfo[1]] and (type(_argInfo[2]) == type(v) or _argInfo[2] == nil) then
					argName = _argInfo[1]
					break
				end
			end

			if argName == "" then  return (tokenError(context, startTok, "Unknown argument of type '%s' for '%s'.", type(v), commandOrFuncName))  end

		-- Validate value.
		elseif not (type(v) == type(argInfo[2]) or argInfo[2] == nil) then
			return (tokenError(context, tokens[tokPos], "Bad value for argument '%s'. (Expected %s, got %s)", argName0, type(argInfo[2]), type(v)))
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

		elseif isToken(tok, "name", "do") or isToken(tok, "name", "if") or isToken(tok, "name", "for") or isToken(tok, "name", "fori") or isToken(tok, "name", "func") then -- @Volatile
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
				tokenError(context, tok, "Unexpected '%s'.", tok.value)
				if stack[1] then  tokenMessage(context, stack[#stack], "...block started here.")  end
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

-- pushGfxState( context, stackType )
-- stackType: see GfxState.stackType
local function pushGfxState(context, stackType)
	local gfxState = copyGfxState(context.gfxState, stackType)

	if stackType == "user" then
		LG.push() -- Only the transform - we save everything else manually. (Beware of LÖVE's stack limit! Maybe we should only use our own stack, for maximum @Robustness.)

	elseif stackType == "makemask" then
		-- void

	elseif stackType == "applymask" then
		-- void

	else
		error(stackType)
	end

	table.insert(context.gfxStack, gfxState)
end

-- status = popGfxState( context, stackType )
-- status = "success" | "emptystack" | "error"
-- stackType: see GfxState.stackType
local function popGfxState(context, stackType)
	local gfxState = getLast(context.gfxStack)
	if not gfxState                    then  return "emptystack"  end
	if gfxState.stackType ~= stackType then  return "error"       end

	moveGfxState(table.remove(context.gfxStack), context.gfxState)

	if stackType == "user" then
		LG.pop()

	elseif stackType == "makemask" then
		assert(gfxState.makeMaskMode == false) -- The caller should've handled this.
		gfxStateSetCanvas(context, gfxState.canvas, gfxState.fallbackCanvas)

	elseif stackType == "applymask" then
		shaderSend(A.shaders.applyMask, "mask", context.maskCanvas)

		LG.setCanvas(nil) -- Before push/pop. Not sure it affects canvas switching. Probably doesn't matter.
		pushTransformAndReset("alpha") -- Should we use gfxState.blendMode (probably not), and/or maybe warn if gfxState.blendMode~="alpha"?
		LG.setCanvas(gfxState.canvas)
		LG.setShader(A.shaders.applyMask.shader)
		LG.draw(context.canvasToMask)
		LG.pop()

		gfxStateSetCanvas(context, gfxState.canvas, gfxState.fallbackCanvas)

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

		--[[ DEBUG
		local imageData       = context.maskCanvas:newImageData()
		local imageDataToSave = love.image.newImageData(imageData:getDimensions())
		imageDataToSave:mapPixel(function(x,y, r,g,b,a)
			local v = imageData:getPixel(x,y)
			return v,v,v,1
		end)
		imageData:release()
		imageDataToSave:encode("png", "mask.png"):release()
		imageDataToSave:release()
		--]]

	else
		error(stackType)
	end

	return "success"
end

-- success = maybeApplyMask( context, tokenForError )
local function maybeApplyMask(context, tokForError)
	if not itemWith1(context.gfxStack, "stackType", "applymask") then  return true  end

	local status = popGfxState(context, "applymask")
	if status == "error" then
		return (tokenError(context, tokForError, "Could not apply mask. (Unbalanced push and pop commands?)"))
	elseif status ~= "success" then
		return (tokenError(context, tokForError, "Internal error. (%s)", status))
	end

	return true
end

local function isCanvasReferenced(context, canvas)
	if canvas == context.gfxState.canvas or canvas == context.gfxState.fallbackCanvas then  return true  end
	for _, gfxState in ipairs(context.gfxStack) do
		if canvas == gfxState.canvas or canvas == gfxState.fallbackCanvas then  return true  end
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

-- applyEffect( context, callback )
-- outputCanvas = callback( context, readCanvas, writeCanvas )
local function applyEffect(context, cb)
	ensureCanvasAndInitted(context)

	pushTransformAndReset("replace")
	initWorkCanvas(context.gfxState.canvas, context.workCanvas1) -- @Incomplete: Handle canvas not being the same size as workCanvas.

	local canvasOut = cb(context, context.workCanvas1, context.workCanvas2)

	LG.setCanvas(context.gfxState.canvas)
	LG.setShader(nil)
	LG.setColor(1, 1, 1)
	canvasOut:setFilter("nearest")
	LG.draw(canvasOut)
	canvasOut:setFilter("linear")
	LG.pop()
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

		pushGfxState(context, "user") -- @Cleanup

		if startRecursion then  artFileRecursionAllowed[path] = recursionsAllowed  end
		local art = loadArtFile(path, context.isLocal)
		if startRecursion then  artFileRecursionAllowed[path] = nil  end

		if not art then  return (tokenError(context, tokForError, "Could not load .artcmd image."))  end
		imageOrCanvas = art.canvas

		assert(popGfxState(context, "user") == "success")

	else
		local s, err = readFile(false, path)
		if not s then  return (tokenError(context, tokForError, "Could not read '%s'. (%s)", path, err))  end

		local fileData           = LF.newFileData(s, path)
		local ok, imageDataOrErr = pcall(love.image.newImageData, fileData) ; fileData:release()
		if not ok then  return (tokenError(context, tokForError, "Could not load '%s'. (%s)", pathIdent, imageDataOrErr))  end
		local imageData = normalizeImageAndMultiplyAlpha(imageDataOrErr)
		imageOrCanvas   = LG.newImage(imageData) ; imageData:release()
	end

	-- imageOrCanvas:setFilter("nearest") -- Fixes fuzziness caused by MSAA, but also messes up scaling and rotation etc. Is there a good solution here? For now, just use a 'filter' argument.
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
		args[argInfo[1]] = argInfo[2]
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
			argInfos[i] = {(argName:lower()), nil} -- Argument "FooBar" gets the name "foobar".  @Speed @Memory
		end

		tokPos = parseArguments(context, tokens, tokPos, funcName, argInfos, args, visited)
		if not tokPos then  return nil  end

		-- Run function.
		local entry          = ScopeStackEntry()
		entry.callerToken    = startTok
		entry.calledFunction = funcInfo

		for i, argInfo in ipairs(argInfos) do
			if not visited[argInfo[1]] then  return (tokenError(context, startTok, "[Call] Missing argument '%s' for '%s'.", argInfo[1], funcName))  end
			entry.variables[funcInfo.arguments[i]] = {token=funcInfo.token, value=args[argInfo[1]]}
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
		local entry    = getLast(context.scopeStack)

		if entry.functions[funcName] then
			-- @Incomplete: Suppress duplicate warnings.
			tokenWarning(context, tokens[tokPos],
				"[%s] Duplicate function '%s' in the same scope. Replacing. (%s:%d)",
				command, funcName, context.path, getLineNumber(context.source, entry.functions[funcName].token.position)
			)
		end

		tokPos = tokPos + 1 -- username

		local funcInfo            = FunctionInfo()
		funcInfo.token            = startTok
		funcInfo.name             = funcName
		entry.functions[funcName] = funcInfo

		while isToken(tokens[tokPos], "username") do
			if tokens[tokPos].negated then  return (parseError(context, tokens[tokPos].position-1, "Unexpected character."))  end

			local argName = tokens[tokPos].value
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
	local commandInfo = COMMANDS[command]
	if not commandInfo then  return (tokenError(context, startTok, "Unknown command '%s'.", command))  end

	local args    = {}
	local visited = {}
	setArgumentsToDefault(args, commandInfo) -- @Speed: This is not necessary, just convenient.

	-- Initialize arguments (dynamic values only).
	if command == "setbuf" then
		args.w  = context.canvasW
		args.h  = context.canvasH
		args.aa = context.msaa
	end

	-- Special cases.
	if (command == "set" or command == "setx" or command == "add" or command == "rem" or command == "for" or command == "fori") and isToken(tokens[tokPos], "username") and not tokens[tokPos].negated then
		-- Treat `set X` as `set "X"` (and same with 'setx' and 'for').
		args   .var = tokens[tokPos].value
		visited.var = tokens[tokPos]
		tokPos      = tokPos + 1
	end

	-- Parse arguments and run command.
	tokPos = parseArguments(context, tokens, tokPos, command, COMMANDS[command], args, visited)
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

		local passes = evaluateCondition(context, command, visited.value, args.value)
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

			-- @Incomplete @Speed: Don't evaluate arguments if executedSomeBlock=true. (Low priority because evaluating expressions currently has no side effects.)
			tokPos = parseArguments(context, tokens, tokPos, command, COMMANDS[command], args, visited)
			if not tokPos then  return nil  end

			if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "[elseif] Invalid ','. Cannot chain 'elseif' using ','."))  end
			if not visited.value            then  return (tokenError(context, startTok, "[elseif] Missing value."))  end
			--

			table.clear(bodyTokens)
			tokPos = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens, true)
			if not tokPos then  return nil  end

			if not executedSomeBlock then
				passes = evaluateCondition(context, "elseif", visited.value, args.value)
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

		if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "[%s] Invalid ','. Cannot chain '%s'.", command, command))  end -- It'd be nice to chain loops though! @UX

		-- Get loop body.
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens, false)
		if not tokPos then  return nil  end

		-- Run loop.
		local entry = ScopeStackEntry()
		table.insert(context.scopeStack, entry)

		local from = (command == "fori") and 1           or args.from
		local to   = (command == "fori") and #args.value or args.to
		local step = (command == "fori") and 1           or args.step

		if args.rev then
			from, to, step = to, from, -step
		end

		local loops   = 0
		local iterVar = {token=startTok, value=nil}
		local stopAll = false

		for n = from, to, step do
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

		if     type(args.value) == "boolean" then  if not args.value                               then  return (tokenError(context, visited.value, "Assertion failed! (False)"))  end
		elseif type(args.value) == "string"  then  if args.value == ""                             then  return (tokenError(context, visited.value, "Assertion failed! (Empty string)"))  end
		elseif type(args.value) == "number"  then  if args.value == 0 or args.value ~= args.value  then  return (tokenError(context, visited.value, "Assertion failed! (Zero or NaN)"))  end
		elseif type(args.value) == "table"   then  if args.value[1] == nil                         then  return (tokenError(context, visited.value, "Assertion failed! (Empty array)"))  end
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
		context.art.zoom = args.zoom

	--
	-- Settings, init.
	--
	----------------------------------------------------------------
	elseif command == "canvas" then
		if context.art.canvas then  return (tokenError(context, startTok, "[%s] Cannot use 'canvas' after drawing commands.", command))  end

		context.canvasW = (args.w >= 1) and args.w or DEFAULT_ART_SIZE
		context.canvasH = (args.h >= 1) and args.h or DEFAULT_ART_SIZE
		context.msaa    = args.aa^2

		context.scopeStack[1].variables.CanvasWidth .value = context.canvasW
		context.scopeStack[1].variables.CanvasHeight.value = context.canvasH

	--
	-- State.
	--
	----------------------------------------------------------------
	elseif command == "push" then
		ensureCanvasAndInitted(context)
		pushGfxState(context, "user")

	----------------------------------------------------------------
	elseif command == "pop" then
		local status = popGfxState(context, "user") -- Will fail if there was no push - otherwise ensureCanvasAndInitted() will have been called.

		if status == "error" then
			if getLast(context.gfxStack).stackType == "makemask" then
				return (tokenError(context, startTok, "[%s] Unbalanced push and pop commands during 'makemask'.", command))
			elseif getLast(context.gfxStack).stackType == "applymask" then
				return (tokenError(context, startTok, "[%s] Unbalanced push and pop commands during 'mask'.", command))
			else
				return (tokenError(context, startTok, "[%s] Unbalanced push and pop commands.", command))
			end

		elseif status == "emptystack" then
			tokenWarning(context, startTok, "pop: Too many 'pop' commands. Ignoring.")

		elseif status ~= "success" then
			return (tokenError(context, startTok, "[%s] Internal error. (%s)", command, status))
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
		if not context.buffers[args.buf] then  return (tokenError(context, (visited.name or startTok), "[%s] No buffer '%s'.", command, args.buf))  end

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
				if visited.spacing then  return (tokenError(context, visited.spacing, "[%s] Cannot modify the glyph spacing for BMFonts.", command))  end

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
	elseif command == "makemask" then
		ensureCanvasAndInitted(context)
		if not maybeApplyMask(context, startTok) then  return nil  end -- 'makemask' also exits 'mask' mode.

		if context.gfxState.makeMaskMode then  return (tokenError(context, startTok, "[%s] Already making a mask.", command))  end

		pushGfxState(context, "makemask")

		gfxStateSetCanvas(context, context.maskCanvas, nil)
		if args.clear then
			LG.setCanvas(context.gfxState.canvas)
			LG.clear(0, 0, 0, 1)
		end

		context.gfxState.makeMaskMode = true

	----------------------------------------------------------------
	elseif command == "mask" then
		ensureCanvasAndInitted(context)
		if not maybeApplyMask(context, startTok) then  return nil  end

		-- 'mask' also exits 'makemask' mode.
		if context.gfxState.makeMaskMode and popGfxState(context, "makemask") ~= "success" then
			return (tokenError(context, startTok, "[%s] Could not finish making mask. (Unbalanced push and pop commands?)", command))
		end

		if args.mask then
			-- @UX: Could we enable the mask just by setting a flag, and not use a separate canvas? That'd be great.
			pushGfxState(context, "applymask")
			gfxStateSetCanvas(context, context.canvasToMask, nil)
			LG.setCanvas(context.gfxState.canvas)
			LG.clear(0, 0, 0, 0)
		else
			-- void  We would apply the mask, but we just did that!
		end

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
		ensureCanvasAndInitted(context)

		local bufName = args.name
		local iw      = args.w
		local ih      = args.h

		local imageOrCanvas

		-- Main buffer.
		if bufName == "" then
			if args.path ~= "" then  return (tokenError(context, (visited.name or startTok), "[%s] Missing buffer name to load image to.", command))  end

			gfxStateSetCanvas(context, context.gfxState.fallbackCanvas, nil) -- :SetMainBuffer

		-- Custom buffer.
		else
			if args.path ~= "" then
				imageOrCanvas = getOrLoadImage(context, startTok, args.path, args.recurse)
				if not imageOrCanvas then  return nil  end

				iw,ih = imageOrCanvas:getDimensions()
			end

			-- Reuse existing canvas.
			if
				context.buffers[bufName]
				and context.buffers[bufName]:getWidth () == iw
				and context.buffers[bufName]:getHeight() == ih
				and context.buffers[bufName]:getMSAA  () == args.aa
			then
				gfxStateSetCanvas(context, context.buffers[bufName], context.gfxState.fallbackCanvas)
				LG.setCanvas(context.gfxState.canvas)
				if args.clear then
					LG.clear(0, 0, 0, 0)
				end

			-- Create new canvas.
			else
				if context.buffers[bufName] then
					LG.setCanvas(nil) -- Don't accidentally release the current canvas!
					context.buffers[bufName]:release()
				end

				context.buffers[bufName] = LG.newCanvas(iw,ih, {format="rgba16", msaa=args.aa})
				gfxStateSetCanvas(context, context.buffers[bufName], context.gfxState.fallbackCanvas)
			end
		end

		if args.path ~= "" then
			imageOrCanvas:setFilter("nearest")
			pushTransformAndReset("replace")
			LG.setCanvas(context.gfxState.canvas)
			LG.draw(imageOrCanvas)
			LG.pop()
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

		table.insert(context.points, point)

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
	elseif command == "image" then
		if args.path == "" then  return (tokenError(context, (visited.path or startTok), "[%s] Missing path.", command))  end

		ensureCanvasAndInitted(context)

		local imageOrCanvas = getOrLoadImage(context, startTok, args.path, args.recurse)
		if not imageOrCanvas then  return nil  end

		local iw,ih = imageOrCanvas:getDimensions()

		imageOrCanvas:setFilter(args.filter and "linear" or "nearest")
		applyCanvas(context, nil)
		applyColor(context, nil, "rectangle", iw,ih)

		LG.draw(imageOrCanvas, args.x,args.y, args.rot, args.sx,args.sy, args.ax*iw,args.ay*ih, args.kx,args.ky)

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
			gfxStateSetCanvas(context, context.gfxState.fallbackCanvas, nil) -- This is like an automatic `setbuf""`. :SetMainBuffer
			-- LG.setCanvas(nil) ; texture:newImageData():encode("png", "buffer.png"):release() -- DEBUG
		elseif isCanvasReferenced(context, texture) then
			return (tokenError(context, (visited[k] or startTok), "[%s] Cannot draw buffer '%s' at this point.", command, bufName))
		end

		local iw,ih = texture:getDimensions()

		if command == "buf" then
			texture:setFilter(args.filter and "linear" or "nearest")
			applyCanvas(context, nil)
			applyColor(context, nil, "rectangle", iw,ih)

			LG.draw(texture, args.x,args.y, args.rot, args.sx,args.sy, args.ax*iw,args.ay*ih, args.kx,args.ky)

		else
			local cw,ch = context.gfxState.canvas:getDimensions()

			texture:setFilter(args.filter and "linear" or "nearest")
			texture:setWrap((args.mirrorx and "mirroredrepeat" or "repeat"), (args.mirrory and "mirroredrepeat" or "repeat"))
			applyCanvas(context, nil)
			applyColor(context, nil, "rectangle", iw,ih)
			LG.push()

			LG.translate(args.x, args.y)
			LG.rotate(args.rot)
			LG.scale(args.sx, args.sy)
			LG.shear(args.kx, args.ky)
			LG.translate(-args.ax*iw, -args.ay*ih)
			LG.scale(iw, ih)

			local u1, v1 = LG.inverseTransformPoint(0 , 0 )
			local u2, v2 = LG.inverseTransformPoint(cw, 0 )
			local u3, v3 = LG.inverseTransformPoint(cw, ch)
			local u4, v4 = LG.inverseTransformPoint(0 , ch)

			LG.origin()
			drawQuad(texture,  0,0, cw,0, cw,ch, 0,ch,  u1,v1, u2,v2, u3,v3, u4,v4)

			LG.pop()
			texture:setWrap("clamp")
		end

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
					return applyMainEffect(vec4(1), texUv, loveColor);
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

		local cw,ch = context.gfxState.canvas:getDimensions()

		applyCanvas(context, shaderOrErr)
		applyColor(context, shaderOrErr, "circle", 1,1) -- @Incomplete: Handle gradients better for isolines.
		LG.push()

		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)

		local u1, v1 = LG.inverseTransformPoint(0 , 0 )
		local u2, v2 = LG.inverseTransformPoint(cw, 0 )
		local u3, v3 = LG.inverseTransformPoint(cw, ch)
		local u4, v4 = LG.inverseTransformPoint(0 , ch)

		LG.origin()
		drawQuad(A.images.rectangle,  0,0, cw,0, cw,ch, 0,ch,  u1,v1, u2,v2, u3,v3, u4,v4)

		LG.pop()
		shaderOrErr.shader:release()

	--
	-- Effects.
	--
	----------------------------------------------------------------
	elseif command == "boxblur" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			LG.setShader(A.shaders.fxBlurBox.shader)

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
		applyEffect(context, function(context, canvasRead, canvasWrite)
			local BLUR_SIZE  = 13 -- 5|9|13 (See fxBlurGaussian.gl)
			local BLUR_REACH = ((BLUR_SIZE==5 and 2.5) or (BLUR_SIZE==13 and 3.5) or 3) / BLUR_SIZE -- Magic inaccurate numbers... @Cleanup

			-- @Incomplete: These loops are probably not exactly correct. I think we wanna
			-- double the radius each iteration (and iterate fewer times).
			LG.setShader(A.shaders.fxBlurGaussian.shader)
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
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxContrast, "params", args.r,args.g,args.b,args.amount) -- rgb is a channel filter.
			LG.setShader(A.shaders.fxContrast.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "brightness" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxBrightness, "params", args.r,args.g,args.b,args.amount) -- rgb is a channel filter.
			LG.setShader(A.shaders.fxBrightness.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "saturation" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxSaturation, "params", args.r,args.g,args.b,args.amount) -- rgb is a channel filter.
			LG.setShader(A.shaders.fxSaturation.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "gamma" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxGamma, "params", args.r,args.g,args.b,args.amount) -- rgb is a channel filter.
			LG.setShader(A.shaders.fxGamma.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "levels" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec2(A.shaders.fxLevels, "rangeIn" , args.infrom,args.into)
			shaderSendVec2(A.shaders.fxLevels, "rangeOut", args.outfrom,args.outto)
			shaderSend    (A.shaders.fxLevels, "middle"  , args.mid)
			LG.setShader(A.shaders.fxLevels.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "thres" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSend(A.shaders.fxThreshold, "threshold", args.thres)
			LG.setShader(A.shaders.fxThreshold.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "overlay" then -- Fade pixels toward color.
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxOverlay, "params", args.r,args.g,args.b,args.a)
			LG.setShader(A.shaders.fxOverlay.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "tint" then -- Change hue and saturation, leave brighness.
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxTint, "params", args.r,args.g,args.b,args.a)
			LG.setShader(A.shaders.fxTint.shader)
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

		shaderSend    (A.shaders.generateNoise, "clouds", (command == "clouds"))
		shaderSendVec3(A.shaders.generateNoise, "offset", args.x,args.y,(args.z or 0))
		shaderSendVec2(A.shaders.generateNoise, "scale" , args.sx,args.sy)

		if gfxState.colorMode == "gradient" then
			applyColor(context, nil, "rectangle", 1,1) -- Generates colorTexture.
			shaderSend(A.shaders.generateNoise, "useColorTexture" , true)
			shaderSend(A.shaders.generateNoise, "colorTexture"    , gfxState.colorTexture)
			shaderSend(A.shaders.generateNoise, "colorTextureSize", gfxState.colorTexture:getWidth())
		else
			local r,g,b,a = unpack(gfxState.flatColor)
			shaderSend    (A.shaders.generateNoise, "useColorTexture", false)
			shaderSendVec4(A.shaders.generateNoise, "color0"         , 0,0,0,a)
			shaderSendVec4(A.shaders.generateNoise, "color1"         , r*a, g*a, b*a, a)
		end

		pushTransformAndReset(gfxState.blendMode)
		LG.setCanvas(canvas)
		LG.setShader(A.shaders.generateNoise.shader)
		LG.draw(A.images.rectangle, 0,0, 0, cw/iw,ch/ih)
		LG.pop()

	elseif command == "random" then
		ensureCanvasAndInitted(context)

		local canvas = context.gfxState.canvas
		local cw,ch  = canvas:getDimensions()

		local rng       = love.math.newRandomGenerator(args.seed)
		local imageData = love.image.newImageData(cw,ch)

		imageData:mapPixel(function(x,y, r,g,b,a) -- @Speed
			r = rng:random()
			g = rng:random()
			b = rng:random()

			if rng:random() > args.level then  return 0, 0, 0, 0  end

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

		if not (isToken(commandTok, "name", "do") or isToken(commandTok, "name", "if") or isToken(commandTok, "name", "for") or isToken(commandTok, "name", "fori") or isToken(commandTok, "name", "func")) then -- @Volatile
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

	if context.workCanvas1  then  context.workCanvas1 :release()  end
	if context.workCanvas2  then  context.workCanvas2 :release()  end
	if context.canvasToMask then  context.canvasToMask:release()  end
	if context.maskCanvas   then  context.maskCanvas  :release()  end

	for _, texture in pairs(context.images     ) do  texture:release()  end
	for _, canvas  in pairs(context.buffers    ) do  canvas :release()  end
	for _, font    in pairs(context.fontsByPath) do  font   :release()  end

	for _, fonts in pairs(context.fontsBySize) do
		for _, font in pairs(fonts) do  font:release()  end
	end
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

	local entry = ScopeStackEntry()
	local vars  = entry.variables
	local tok1  = tokens[1]

	vars.True  = {token=tok1, value=true} -- @Robustness: Make these constant.
	vars.False = {token=tok1, value=false}
	vars.Huge  = {token=tok1, value=1/0}
	vars.Nan   = {token=tok1, value=0/0}
	vars.Pi    = {token=tok1, value=math.pi}
	vars.Tau   = {token=tok1, value=TAU}

	vars.WindowWidth  = {token=tok1, value=LG.getWidth()}
	vars.WindowHeight = {token=tok1, value=LG.getHeight()}
	vars.CanvasWidth  = {token=tok1, value=context.canvasW}
	vars.CanvasHeight = {token=tok1, value=context.canvasH}

	-- These are just for the Lua/bracket expressions.  @Incomplete: Argument validation.  @Robustness: Make these constant (although their names already make them).
	vars.num     = {token=tok1, value=tonumber}
	vars.sel     = {token=tok1, value=function(n, ...)  return (select(n, ...))  end}
	vars.selx    = {token=tok1, value=select}
	vars.str     = {token=tok1, value=tostring}
	vars.type    = {token=tok1, value=type}
	vars.abs     = {token=tok1, value=math.abs}
	vars.acos    = {token=tok1, value=math.acos}
	vars.asin    = {token=tok1, value=math.asin}
	vars.atan    = {token=tok1, value=function(y, x)  return x and math.atan2(y, x) or math.atan(y)  end} -- atan( y [, x=1 ] )
	vars.ceil    = {token=tok1, value=math.ceil}
	vars.cos     = {token=tok1, value=math.cos}
	vars.cosh    = {token=tok1, value=math.cosh}
	vars.deg     = {token=tok1, value=math.deg}
	vars.exp     = {token=tok1, value=math.exp}
	vars.floor   = {token=tok1, value=math.floor}
	vars.fmod    = {token=tok1, value=math.fmod}
	vars.frac    = {token=tok1, value=function(n)  local int, frac = math.modf(n) ; return frac  end}
	vars.frexpe  = {token=tok1, value=function(n)  local m, e = math.frexp(n) ; return e  end}
	vars.frexpm  = {token=tok1, value=function(n)  return (math.frexp(n))  end}
	vars.int     = {token=tok1, value=function(n)  return (math.modf(n))  end}
	vars.ldexp   = {token=tok1, value=math.ldexp}
	vars.log     = {token=tok1, value=math.log} -- log( n [, base=e ] )
	vars.lerp    = {token=tok1, value=math.lerp}
	vars.max     = {token=tok1, value=math.max}
	vars.min     = {token=tok1, value=math.min}
	vars.rad     = {token=tok1, value=math.rad}
	vars.rand    = {token=tok1, value=love.math.random}
	vars.randf   = {token=tok1, value=function(n1, n2)  return n2 and n1+(n2-n1)*love.math.random() or (n1 or 1)*love.math.random()  end} -- randomf( [[ n1=0, ] n2=1 ] )
	vars.round   = {token=tok1, value=math.round}
	vars.sin     = {token=tok1, value=math.sin}
	vars.sinh    = {token=tok1, value=math.sinh}
	vars.sqrt    = {token=tok1, value=math.sqrt}
	vars.tan     = {token=tok1, value=math.tan}
	vars.tanh    = {token=tok1, value=math.tanh}
	vars.clock   = {token=tok1, value=os.clock}
	vars.date    = {token=tok1, value=os.date}
	vars.env     = {token=tok1, value=os.getenv}
	vars.time    = {token=tok1, value=os.time}
	vars.byte    = {token=tok1, value=string.byte} -- @Robustness: Handle the string metatable.
	vars.char    = {token=tok1, value=string.char}
	vars.find    = {token=tok1, value=string.find}
	vars.format  = {token=tok1, value=F}
	vars.gsub    = {token=tok1, value=function(s, pat, repl, n)  return (string.gsub(s, pat, repl, n))  end}
	vars.lower   = {token=tok1, value=string.lower}
	vars.match   = {token=tok1, value=string.match}
	vars.rep     = {token=tok1, value=string.rep}
	vars.rev     = {token=tok1, value=string.reverse} -- @Incomplete: Also reverse arrays.
	vars.sub     = {token=tok1, value=string.sub}
	vars.upper   = {token=tok1, value=string.upper}
	vars.unpack  = {token=tok1, value=unpack}
	vars.concat  = {token=tok1, value=table.concat}
	vars.sort    = {token=tok1, value=function(arr)     arr = {unpack(arr)} ; table.sort(arr                                         ) ; return arr  end}
	vars.sortby  = {token=tok1, value=function(arr, k)  arr = {unpack(arr)} ; table.sort(arr, function(a, b)  return a[k] < b[k]  end) ; return arr  end}

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

	if context.art.canvas and not maybeApplyMask(context, nil) then -- @Incomplete: Check that maybeApplyMask() works here after all our gfxStack changes. 2022-08-31
		cleanup(context, loveGfxStackDepth, false)
		return nil
	end

	-- Success!
	cleanup(context, loveGfxStackDepth, true)

	if not context.art.canvas then
		print("Nothing was rendered!") -- Should we call ensureCanvasAndInitted()? Especially if an art file loads another.
	end

	if artFilesBeingLoaded[path] == 0 then  printf("Loading %s... done in %.2f seconds!", path, love.timer.getTime()-startTime)  end
	return context.art.canvas and context.art
end


