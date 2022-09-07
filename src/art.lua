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
	callStack  = {--[[ [funcInfo1]=true, ... ]]},
	gfxStack   = {--[[ gfxState1, ... ]]},

	gfxState = nil, -- (including love.graphics)

	workCanvas1  = nil,
	workCanvas2  = nil,
	canvasToMask = nil,
	maskCanvas   = nil,

	images      = {--[[ [path1]=image|canvas, ... ]]},
	layers      = {--[[ [name1]=image, ... ]]},
	fontsByPath = {--[[ [path1]=font, ... ]]}, -- Image fonts.
	fontsBySize = {--[[ [path1]={[size1]=font,...}, ... ]]}, -- Vector fonts.

	points = {},

	-- Settings.
	canvasW = DEFAULT_ART_SIZE,
	canvasH = DEFAULT_ART_SIZE,
	msaa    = 1,
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

local function GfxState()return{
	stackType = "user", -- "user" | "makemask" | "applymask"

	-- "user"
	-- Color.
	colorMode          = "flatcolor", -- "flatcolor" | "gradient"
	flatColor          = {1,1,1,1},
	gradient           = {--[[ r1,g1,b1,a1, ... ]]},
	colorTexture       = nil,
	colorTextureRadial = false,
	colorTextureFit    = true, -- Used with colorTextureRadial.
	colorTextureScaleX = 1.0,
	colorTextureScaleY = 1.0,
	colorTextureAngle  = 0.0,
	-- Font.
	font = LG.getFont(),

	-- "makemask" & "applymask"
	canvas         = nil,
	fallbackCanvas = nil, -- @Cleanup: We only have this because of layers. Add a currentLayerName field instead and infer what canvas to use from gfxStack!

	-- "makemask"
	makeMaskMode = false,
}end

local function copyGfxState(gfxState,stackType)return{
	stackType = stackType or error("Missing 'stackType' argument."),

	colorMode          = (stackType == "user" or nil) and gfxState.colorMode,
	flatColor          = (stackType == "user" or nil) and {unpack(gfxState.flatColor)},
	gradient           = (stackType == "user" or nil) and {unpack(gfxState.gradient)},
	colorTexture       = (stackType == "user" or nil) and gfxState.colorTexture,
	colorTextureRadial = (stackType == "user" or nil) and gfxState.colorTextureRadial,
	colorTextureFit    = (stackType == "user" or nil) and gfxState.colorTextureFit,
	colorTextureScaleX = (stackType == "user" or nil) and gfxState.colorTextureScaleX,
	colorTextureScaleY = (stackType == "user" or nil) and gfxState.colorTextureScaleY,
	colorTextureAngle  = (stackType == "user" or nil) and gfxState.colorTextureAngle,
	font               = (stackType == "user" or nil) and gfxState.font,

	canvas         = (stackType == "makemask" or stackType == "applymask" or nil) and gfxState.canvas,
	fallbackCanvas = (stackType == "makemask" or stackType == "applymask" or nil) and gfxState.fallbackCanvas,

	makeMaskMode = (stackType == "makemask" or nil) and gfxState.makeMaskMode,
}end

local function moveGfxState(fromGfxState, toGfxState)
	if fromGfxState.stackType == "user" then
		toGfxState.colorMode          = fromGfxState.colorMode
		toGfxState.flatColor          = fromGfxState.flatColor
		toGfxState.gradient           = fromGfxState.gradient
		toGfxState.colorTexture       = fromGfxState.colorTexture
		toGfxState.colorTextureRadial = fromGfxState.colorTextureRadial
		toGfxState.colorTextureFit    = fromGfxState.colorTextureFit
		toGfxState.colorTextureScaleX = fromGfxState.colorTextureScaleX
		toGfxState.colorTextureScaleY = fromGfxState.colorTextureScaleY
		toGfxState.colorTextureAngle  = fromGfxState.colorTextureAngle
		toGfxState.font               = fromGfxState.font
	end
	if fromGfxState.stackType == "makemask" or fromGfxState.stackType == "applymask" then
		toGfxState.canvas         = fromGfxState.canvas
		toGfxState.fallbackCanvas = fromGfxState.fallbackCanvas
	end
	if fromGfxState.stackType == "makemask" then
		toGfxState.makeMaskMode = fromGfxState.makeMaskMode
	end
end



local function parseError(context, pos, s, ...)
	printFileErrorAt(context.path, context.source, pos, s, ...)
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
end
local function tokenWarning(context, tok, s, ...)
	local pos = (tok and tok.position) or context.source:find"%S%s*$" or #context.source
	printFileWarningAt(context.path, context.source, pos, s, ...)
end
local function tokenMessage(context, tok, s, ...)
	local pos = (tok and tok.position) or context.source:find"%S%s*$" or #context.source
	printFileMessageAt(context.path, context.source, pos, s, ...)
end



-- value|nil, stackIndex = findInStack( context, member, key )
local function findInStack(context, member, k)
	for i = #context.scopeStack, 1, -1 do
		local v = context.scopeStack[i][member][k]
		if v ~= nil then  return v, i  end
	end
	return nil -- Not found!
end



-- gfxStateSetCanvas( context, canvas, fallbackCanvas=canvas )
local function gfxStateSetCanvas(context, canvas, fallbackCanvas)
	context.gfxState.canvas         = canvas
	context.gfxState.fallbackCanvas = fallbackCanvas or canvas
end



local function applyCanvas(context)
	shaderSend(A.shaders.main.shader, "makeMaskMode", context.gfxState.makeMaskMode) -- Maybe not the best place for this, but eh...

	LG.setCanvas(context.gfxState.canvas)
	LG.setShader(A.shaders.main.shader)
end

-- applyColor( context, shapeToDraw, relativeShapeWidth,relativeShapeHeight )
-- shapeToDraw = "rectangle" | "circle"
local function applyColor(context, shape, w,h)
	w = math.abs(w)
	h = math.abs(h)

	LG.setBlendMode("alpha", "premultiplied")

	local gfxState = context.gfxState

	if gfxState.colorMode == "flatcolor" then
		local r,g,b,a = unpack(gfxState.flatColor)
		LG.setColor(r*a, g*a, b*a, a)
		shaderSend(A.shaders.main.shader, "useColorTexture", false)
		return
	end

	local sx           = 1 -- For colorTextureLayout.
	local sy           = 1
	local dirOrOffsetX = 1
	local dirY         = 0

	if gfxState.colorMode == "gradient" then
		if not gfxState.colorTexture then
			local grad          = gfxState.gradient
			local pixelRowChars = {}
			local palette       = {}

			for i = 1, #grad/4 do
				local c    = string.char(i-1)
				palette[c] = {grad[i*4-3], grad[i*4-2], grad[i*4-1], grad[i*4]}
				table.insert(pixelRowChars, c)
			end

			local pixelRows       = {table.concat(pixelRowChars)}
			gfxState.colorTexture = newImageUsingPalette(pixelRows, palette)
		end

		if gfxState.colorTextureRadial then
			local iw     = gfxState.colorTexture:getWidth()
			local scale  = (gfxState.colorTextureFit and math.min(h/w, 1)) or (shape == "rectangle" and math.sqrt(w^2+h^2)/w) or math.max(h/w, 1)
			sx           = gfxState.colorTextureScaleX * iw/(iw-1) * scale -- colorTextureScaleY isn't used.
			sy           = sx * w/h
			dirOrOffsetX = .5/iw

		else
			-- When shape="rectangle" and the gradient scale is 1, one edge of the
			-- gradient should touch one corner, and the other edge should touch
			-- the opposite, no matter the gradient's angle.
			local angle                  = gfxState.colorTextureAngle
			local scaledAngle            = math.atan2(math.sin(angle)*h/w, math.cos(angle))
			local angleCompensationScale = (shape == "rectangle") and math.abs(math.sin(scaledAngle))+math.abs(math.cos(scaledAngle)) or 1

			local iw = gfxState.colorTexture:getWidth()
			sx       = gfxState.colorTextureScaleX * iw/(iw-1) * angleCompensationScale -- colorTextureScaleY isn't used.

			dirOrOffsetX = math.cos(scaledAngle)
			dirY         = math.sin(scaledAngle)
		end

	else
		error(gfxState.colorMode)
	end

	shaderSend    (A.shaders.main.shader, "useColorTexture"   , true)
	shaderSend    (A.shaders.main.shader, "colorTextureRadial", gfxState.colorTextureRadial)
	shaderSend    (A.shaders.main.shader, "colorTexture"      , gfxState.colorTexture)
	shaderSendVec4(A.shaders.main.shader, "colorTextureLayout", sx,sy, dirOrOffsetX,dirY)
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

	shaderSend(A.shaders.main.shader, "textBlendFix"   , false)
	shaderSend(A.shaders.main.shader, "makeMaskMode"   , false)
	shaderSend(A.shaders.main.shader, "useColorTexture", false)

	gfxStateSetCanvas(context, context.art.canvas, nil)

	applyCanvas(context)
	LG.setShader(A.shaders.main.shader)
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

				if s:find("^[^%s,;#]", pos) then
					parseWarning(context, pos, "Value right after '%s%s' does not belong to it.", mod, name)
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

local function validateVariableName(context, tokForError, term, var)
	if not var:find"^%w+$" then  tokenError(context, tokForError, "Bad %s '%s'. Must only contain alphanumeric characters.", term, var) ; return false  end
	if not var:find"^%u"   then  tokenError(context, tokForError, "Bad %s '%s'. Must start with an uppercase letter."      , term, var) ; return false  end
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

			local env = setmetatable({}, {
				__index = function(_, k, v)
					local v = findInStack(context, "variables", k) -- @Incomplete: Proper upvalues (which require lexical scope).
					if v ~= nil then  return v  end
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
				visited[_argName] = true
			end
		else
			args   [argName] = v
			visited[argName] = true
		end
		tokPos = tokPos + 1 -- the value
	end
end

local function collectBodyTokens(context, tokens, tokForError, tokPos, outTokens, lookForElse)
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

		elseif isToken(tok, "name", "do") or isToken(tok, "name", "if") or isToken(tok, "name", "for") or isToken(tok, "name", "func") then -- @Volatile
			depth = depth + 1
			table.insert(stack, tok)

		elseif isToken(tok, "name", "end") then
			depth = depth - 1
			table.remove(stack)
			if depth == 0 then  return tokPos+1  end

		elseif isToken(tok, "name", "else") then
			if lookForElse and depth == 1 then
				return tokPos+1
			elseif isToken(stack[#stack], "name", "if") then
				stack[#stack] = tok -- Exit if-block and enter else-block.
			else
				tokenError(context, tok, "Unexpected 'else'.")
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
		shaderSend(A.shaders.applyMask.shader, "mask", context.maskCanvas)

		LG.setCanvas(nil) -- Before push/pop. Not sure it affects canvas switching. Probably doesn't matter.
		pushTransformAndReset("alpha")
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

-- image|canvas|nil = getOrLoadImage( context, tokenForError, pathIdentifier )
-- Returns nil on error.
local function getOrLoadImage(context, tokForError, pathIdent)
	local imageOrCanvas = context.images[pathIdent]
	if imageOrCanvas then  return imageOrCanvas  end

	local path = makePathAbsolute(pathIdent, (context.path:gsub("[^/\\]+$", "")))

	if pathIdent:find"%.artcmd$" then
		pushGfxState(context, "user") -- @Cleanup

		local art = loadArtFile(path, context.isLocal)
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
		local funcInfo = findInStack(context, "functions", funcName)
		if not funcInfo   then  return (tokenError(context, startTok, "No function '%s'.", funcName))  end
		if not commandTok then  tokPos = tokPos + 1  end -- username

		if context.callStack[funcInfo] then
			return (tokenError(context, startTok, "Recursively calling '%s'. (%s:%d)", funcName, context.path, getLineNumber(context.source, funcInfo.token.position)))
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
		local entry = ScopeStackEntry()

		for i, argInfo in ipairs(argInfos) do
			if not visited[argInfo[1]] then  return (tokenError(context, startTok, "Missing argument '%s' for '%s'.", argInfo[1], funcName))  end
			entry.variables[funcInfo.arguments[i]] = args[argInfo[1]]
		end

		table.insert(context.scopeStack, entry)
		context.callStack[funcInfo] = true

		local bodyTokPos, stop = runBlock(context, funcInfo.tokens, 1)
		if not bodyTokPos              then  return nil  end
		if funcInfo.tokens[bodyTokPos] then  return (tokenError(context, funcInfo.tokens[bodyTokPos], "Unexpected token."))  end

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
		if not isToken(tokens[tokPos], "username") then  return (tokenError(context, tokens[tokPos], "Expected a name for the function."))  end
		if tokens[tokPos].negated                  then  return (parseError(context, tokens[tokPos].position-1, "Unexpected character."))  end
		local funcName = tokens[tokPos].value
		local entry    = getLast(context.scopeStack)
		if entry.functions[funcName] then  tokenWarning(context, tokens[tokPos], "Duplicate function '%s' in the same scope. Replacing.", funcName)  end
		tokPos = tokPos + 1 -- username

		local funcInfo            = FunctionInfo()
		funcInfo.token            = startTok
		entry.functions[funcName] = funcInfo

		while isToken(tokens[tokPos], "username") do
			if tokens[tokPos].negated then  return (parseError(context, tokens[tokPos].position-1, "Unexpected character."))  end

			local argName = tokens[tokPos].value
			table.insert(funcInfo.arguments, argName)
			tokPos = tokPos + 1 -- username
		end

		if not (isToken(tokens[tokPos], "linebreak") or isToken(tokens[tokPos], ";")) then
			return (tokenError(context, tokens[tokPos], "Expected argument for function '%s'.", funcName))
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

	for _, argInfo in ipairs(commandInfo) do
		args[argInfo[1]] = argInfo[2] -- Fill with default values.  @Speed: This is not necessary, just convenient.
	end

	-- Initialize arguments (dynamic values only).
	if command == "setlayer" then
		args.w  = context.canvasW
		args.h  = context.canvasH
		args.aa = context.msaa
	end

	-- Special cases.
	if (command == "set" or command == "setx" or command == "for") and isToken(tokens[tokPos], "username") and not tokens[tokPos].negated then
		-- Treat `set X` as `set "X"` (and same with 'setx' and 'for').
		args   .var = tokens[tokPos].value
		visited.var = true
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
	if command == "set" then
		if args.var   == ""  then  return (tokenError(context, startTok, "Missing variable name."))  end
		if args.value == nil then  return (tokenError(context, startTok, "Missing value to assign to '%s'.", args.var))  end
		if not validateVariableName(context, startTok, "variable name", args.var) then  return nil  end

		getLast(context.scopeStack).variables[args.var] = args.value

	----------------------------------------------------------------
	elseif command == "setx" then
		if args.var   == ""  then  return (tokenError(context, startTok, "Missing variable name."))  end
		if args.value == nil then  return (tokenError(context, startTok, "Missing value to assign to '%s'.", args.var))  end
		if not validateVariableName(context, startTok, "variable name", args.var) then  return nil  end

		local oldV, stackIndex = findInStack(context, "variables", args.var) -- @Incomplete: Proper upvalues (which require lexical scope).
		if oldV == nil then  return (tokenError(context, startTok, "No existing variable '%s'.", args.var))  end

		context.scopeStack[stackIndex].variables[args.var] = args.value

	----------------------------------------------------------------
	elseif command == "do" or command == "if" then
		if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "Invalid ','."))  end

		-- Get block body.
		local trueTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, trueTokens, true)
		if not tokPos then  return nil  end

		local falseTokens = {}
		if isToken(tokens[tokPos-1], "name", "else") then
			tokPos = collectBodyTokens(context, tokens, tokens[tokPos-1], tokPos, falseTokens, false)
			if not tokPos then  return nil  end
		end

		-- Run block.
		local isTrue = (command == "do")
		if not isTrue and type(args.value) == "boolean" and args.value                                    then  isTrue = true  end
		if not isTrue and type(args.value) == "number"  and args.value ~= 0 and args.value == args.value  then  isTrue = true  end
		if not isTrue and type(args.value) == "string"  and args.value ~= ""                              then  isTrue = true  end

		local bodyTokens = isTrue and trueTokens or falseTokens

		if bodyTokens[1] then
			local entry = ScopeStackEntry()
			table.insert(context.scopeStack, entry)

			local bodyTokPos, stop = runBlock(context, bodyTokens, 1)
			if not bodyTokPos         then  return nil  end
			if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "Unexpected token."))  end

			table.remove(context.scopeStack)

			if stop == STOP_ALL then  return 1/0, STOP_ALL  end
		end

	----------------------------------------------------------------
	elseif command == "for" then
		if args.var  == "" then  return (tokenError(context, startTok, "Empty variable name."))  end
		if args.step == 0  then  return (tokenError(context, startTok, "Step is zero."))  end
		if not validateVariableName(context, startTok, "variable name", args.var) then  return nil  end

		if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "Invalid ','."))  end

		-- Get loop body.
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens, false)
		if not tokPos then  return nil  end

		-- Run loop.
		local entry = ScopeStackEntry()
		table.insert(context.scopeStack, entry)

		local loops   = 0
		local stopAll = false

		for n = args.from, args.to, args.step do
			loops = loops + 1

			if loops >= MAX_LOOPS then -- Maybe there should be a MAX_DRAW_OPERATIONS too? MAX_LOOPS could probably be higher then. @Incomplete
				tokenError(context, startTok, "Max loops exceeded. Breaking.")
				break
			end

			entry.variables[args.var] = n

			local bodyTokPos, stop = runBlock(context, bodyTokens, 1)
			if not bodyTokPos         then  return nil              end
			if stop == STOP_ONE       then  break                   end
			if stop == STOP_ALL       then  stopAll = true ; break  end
			if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "Unexpected token."))  end
		end

		table.remove(context.scopeStack)

		if stopAll then  return 1/0, STOP_ALL  end

	----------------------------------------------------------------
	elseif command == "stop" then
		if args.all then  print("Stopping all!")  end
		return 1/0, (args.all and STOP_ALL or STOP_ONE)

	----------------------------------------------------------------
	elseif command == "assert" then
		if not args.value then  return (tokenError(context, startTok, "Assertion failed!"))  end

	elseif command == "print" then
		-- @Robustness: Don't print invalid UTF-8!
		if isSequence then
			io.write("\t", tostring(args.value))
		else
			local ln = getLineNumber(context.source, startTok.position)
			io.write(context.path, ":", ln, ": ", tostring(args.value))
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
		if context.art.canvas then  return (tokenError(context, startTok, "Cannot use '%s' after drawing commands.", command))  end

		context.canvasW = (args.w >= 1) and args.w or DEFAULT_ART_SIZE
		context.canvasH = (args.h >= 1) and args.h or DEFAULT_ART_SIZE
		context.msaa    = args.aa^2

		context.scopeStack[1].variables.CanvasWidth  = context.canvasW
		context.scopeStack[1].variables.CanvasHeight = context.canvasH

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
				return (tokenError(context, startTok, "Unbalanced push and pop commands during 'makemask'."))
			elseif getLast(context.gfxStack).stackType == "applymask" then
				return (tokenError(context, startTok, "Unbalanced push and pop commands during 'mask'."))
			else
				return (tokenError(context, startTok, "Unbalanced push and pop commands."))
			end

		elseif status == "emptystack" then
			tokenWarning(context, startTok, "Too many 'pop' commands. Ignoring.")

		elseif status ~= "success" then
			return (tokenError(context, startTok, "Internal error. (%s)", status))
		end

	----------------------------------------------------------------
	elseif command == "color" or command == "grey" then
		ensureCanvasAndInitted(context)

		local gfxState     = context.gfxState
		gfxState.colorMode = "flatcolor"
		table.clear(gfxState.gradient)

		if command == "color" then  updateVec4(gfxState.flatColor, args.r,args.g,args.b,args.a)
		else                        updateVec4(gfxState.flatColor, args.grey,args.grey,args.grey,args.a)  end

		if gfxState.colorTexture then
			-- @Memory: Release image. (Consider gfxStack!)
			gfxState.colorTexture = nil
		end

	----------------------------------------------------------------
	elseif command == "grad" then
		ensureCanvasAndInitted(context)

		local gfxState              = context.gfxState
		gfxState.colorMode          = "gradient"
		gfxState.colorTextureRadial = args.radial
		gfxState.colorTextureFit    = args.fit
		gfxState.colorTextureScaleX = args.scale -- colorTextureScaleY isn't used.  @Incomplete: For radial gradients it makes sense to scale y!
		gfxState.colorTextureAngle  = args.rot

		if gfxState.colorTexture then
			-- @Memory: Release image. (Consider gfxStack!)
			gfxState.colorTexture = nil
		end

		if not isSequence then
			table.clear(gfxState.gradient)
		end
		table.insert(gfxState.gradient, args.r)
		table.insert(gfxState.gradient, args.g)
		table.insert(gfxState.gradient, args.b)
		table.insert(gfxState.gradient, args.a)

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
			if not s then  return (tokenError(context, startTok, "Could not read '%s'. (%s)", path, err))  end

			local fileData      = LF.newFileData(s, path) -- @Speed: Cache this (though people probably don't use too many sizes of the same font per art piece).
			local ok, fontOrErr = pcall(LG.newFont, fileData, args.size) ; fileData:release()
			if not ok then  return (tokenError(context, startTok, "Could not load '%s'. (%s)", path, fontOrErr))  end
			font = fontOrErr
		end

		context.fontsBySize[args.path] = fonts
		fonts[args.size]               = font
		context.gfxState.font          = font

	----------------------------------------------------------------
	elseif command == "imagefont" then
		local font = context.fontsByPath[args.path]

		if not font then
			local fontPath = makePathAbsolute(args.path, (context.path:gsub("[^/\\]+$", "")))

			local fontStr, err = readFile(false, fontPath)
			if not fontStr then  return (tokenError(context, startTok, "Could not read '%s'. (%s)", fontPath, err))  end

			local fontFileData = LF.newFileData(fontStr, fontPath)
			local ok, fontOrErr

			-- BMFont.
			if args.chars == "" then
				if args.spacing ~= 0 then  return (tokenError(context, startTok, "Cannot modify the glyph spacing for BMFonts."))  end

				local imagePath0 = ""
				local pos        = 1

				for _pos, _imagePath in fontStr:gmatch'%f[^\n%z]()page%f[ ][^\n]- file="([^\n]-)"' do
					if imagePath0 ~= "" then
						return (tokenError(context, startTok,
							"Failed loading BMFont...\n %s:%d: Font references multiple image files, which is not supported.",
							fontPath, getLineNumber(fontStr, _pos)
						))
					end
					imagePath0 = _imagePath
					pos        = _pos
				end
				if imagePath0 == "" then
					return (tokenError(context, startTok, "Failed loading BMFont...\n %s: Font seem to reference no image file.", fontPath))
				end

				local imagePath = makePathAbsolute(imagePath0, (fontPath:gsub("[^/\\]+$", "")))

				local imageStr, err = readFile(false, imagePath)
				if not imageStr then
					return (tokenError(context, startTok,
						"Failed loading BMFont...\n %s:%d: Could not read referenced image '%s'. (%s)",
						fontPath, getLineNumber(fontStr, pos), imagePath0, err
					))
				end

				local imageFileData = LF.newFileData(imageStr, imagePath)
				ok, fontOrErr       = pcall(LG.newFont, fontFileData, imageFileData) ; imageFileData:release()

			-- LÖVE ImageFont.
			else
				ok, fontOrErr = pcall(LG.newImageFont, fontFileData, args.chars, args.spacing)
			end

			fontFileData:release()
			if not ok then  return (tokenError(context, startTok, "Could not load '%s'. (%s)", fontPath, fontOrErr))  end
			font = fontOrErr
		end

		context.fontsByPath[args.path] = font
		context.gfxState.font          = font

	----------------------------------------------------------------
	elseif command == "makemask" then
		ensureCanvasAndInitted(context)
		if not maybeApplyMask(context, startTok) then  return nil  end -- 'makemask' also exits 'mask' mode.

		if context.gfxState.makeMaskMode then  return (tokenError(context, startTok, "Already making a mask."))  end

		pushGfxState(context, "makemask")

		gfxStateSetCanvas(context, context.maskCanvas, nil)
		if args.clear then
			LG.setCanvas(context.gfxState.canvas)
			LG.clear(0, 0, 0, 1)
		end

		context.gfxState.makeMaskMode = true
		updateVec4(context.gfxState.flatColor, 1,1,1,1) -- Should we undo this when we exit makemask mode? Probably not as other things, like font, don't. (Should we also update colorMode? Is this line even a good idea?)

	----------------------------------------------------------------
	elseif command == "mask" then
		ensureCanvasAndInitted(context)
		if not maybeApplyMask(context, startTok) then  return nil  end

		-- 'mask' also exits 'makemask' mode.
		if context.gfxState.makeMaskMode and popGfxState(context, "makemask") ~= "success" then
			return (tokenError(context, startTok, "Could not finish making mask. (Unbalanced push and pop commands?)"))
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
	elseif command == "setlayer" then
		ensureCanvasAndInitted(context)

		local layerName = args.name
		local iw        = args.w
		local ih        = args.h

		local imageOrCanvas

		if args.path ~= "" then
			imageOrCanvas = getOrLoadImage(context, startTok, args.path)
			if not imageOrCanvas then  return nil  end

			iw,ih = imageOrCanvas:getDimensions()
		end

		-- No layer.
		if layerName == "" then
			if args.path ~= "" then  return (tokenError(context, startTok, "Missing layer name."))  end

			gfxStateSetCanvas(context, context.gfxState.fallbackCanvas, nil) -- :SetNoLayer

		-- Yes layer, reuse existing canvas.
		elseif
			context.layers[layerName]
			and context.layers[layerName]:getWidth () == iw
			and context.layers[layerName]:getHeight() == ih
			and context.layers[layerName]:getMSAA  () == args.aa
		then
			gfxStateSetCanvas(context, context.layers[layerName], context.gfxState.fallbackCanvas)
			LG.setCanvas(context.gfxState.canvas)
			if args.clear then
				LG.clear(0, 0, 0, 0)
			end

		-- Yes layer, create new canvas.
		else
			if context.layers[layerName] then
				LG.setCanvas(nil) -- Don't accidentally release the current canvas!
				context.layers[layerName]:release()
			end

			context.layers[layerName] = LG.newCanvas(iw,ih, {format="rgba16", msaa=args.aa})
			gfxStateSetCanvas(context, context.layers[layerName], context.gfxState.fallbackCanvas)
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
	elseif command == "point" then
		if not isSequence then
			table.clear(context.points)
		end
		table.insert(context.points, args.x)
		table.insert(context.points, args.y)

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
		applyCanvas(context)
		applyColor(context, "rectangle", cw,ch)

		LG.push()
		LG.origin()
		LG.rectangle("fill", -1,-1, cw+2,ch+2) -- Not sure if the bleeding is necessary (if msaa>1).
		LG.pop()

		-- Restore state.
		gfxState.colorMode = colorMode
		updateVec4(gfxState.flatColor, unpack(color))

	----------------------------------------------------------------
	elseif command == "rect" then
		if not (args.mode == "fill" or args.mode == "line") then  return (tokenError(context, startTok, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode))  end

		ensureCanvasAndInitted(context)

		applyCanvas(context)
		applyColor(context, "rectangle", args.w,args.h)

		local segs = (
			args.segs > 0
			and args.segs
			or  math.round(math.max(math.max(args.rx, args.ry) * TAU/10, 64) * .25)
		)

		LG.push()
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-args.ax*args.w, -args.ay*args.h)

		if     args.mode == "fill" then  drawRectangleFill(0,0, args.w,args.h, args.rx,args.ry, segs)
		elseif args.mode == "line" then  drawRectangleLine(0,0, args.w,args.h, args.rx,args.ry, segs, args.thick)
		else error(args.mode) end

		LG.pop()

	----------------------------------------------------------------
	elseif command == "circle" then
		if not (args.mode == "fill" or args.mode == "fillclosed" or args.mode == "line" or args.mode == "linepie" or args.mode == "lineclosed") then
			return (tokenError(context, startTok, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode))
		end

		ensureCanvasAndInitted(context)

		local angle1 = args.from
		local angle2 = math.clamp(args.to, args.from-TAU, args.from+TAU)

		local segs = (
			args.segs > 0
			and args.segs
			or  math.round(math.max(math.max(args.rx, args.ry) * TAU/10, 64) * math.abs(angle2-angle1)/TAU)
		)

		applyCanvas(context)
		applyColor(context, "circle", args.rx,args.ry)

		LG.push()
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-(args.ax*2-1)*args.rx, -(args.ay*2-1)*args.ry)

		if     args.mode == "fill"       then  drawCircleFill(0,0, args.rx,args.ry, angle1,angle2, false   , segs)
		elseif args.mode == "fillclosed" then  drawCircleFill(0,0, args.rx,args.ry, angle1,angle2, true    , segs)
		elseif args.mode == "line"       then  drawCircleLine(0,0, args.rx,args.ry, angle1,angle2, "open"  , segs, args.thick)
		elseif args.mode == "linepie"    then  drawCircleLine(0,0, args.rx,args.ry, angle1,angle2, "pie"   , segs, args.thick)
		elseif args.mode == "lineclosed" then  drawCircleLine(0,0, args.rx,args.ry, angle1,angle2, "closed", segs, args.thick)
		else error(args.mode) end

		LG.pop()

	----------------------------------------------------------------
	elseif command == "poly" then
		if not (args.mode == "fill" or args.mode == "line") then  return (tokenError(context, startTok, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode))  end

		if not context.points[1] then  return (tokenError(context, startTok, "No points added."))  end
		if not context.points[5] then  return (tokenError(context, startTok, "Not enough points added."))  end

		if args.mode == "fill" and not love.math.isConvex(context.points) then
			return (tokenError(context, startTok, "The added points form a concave shape. Filled polygons must be convex."))
		end

		ensureCanvasAndInitted(context)

		local x1 =  1/0
		local x2 = -1/0
		local y1 =  1/0
		local y2 = -1/0

		for i = 1, #context.points, 2 do
			x1 = math.min(x1, context.points[i  ])
			x2 = math.max(x2, context.points[i  ])
			y1 = math.min(y1, context.points[i+1])
			y2 = math.max(y2, context.points[i+1])
		end

		local colorW = x2 - x1
		local colorH = y2 - y1

		if not args.shift then
			x1, y1 = 0, 0
		end

		applyCanvas(context)
		applyColor(context, "rectangle", colorW,colorH)

		LG.push()
		LG.translate(args.x+x1, args.y+y1)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-args.ax*(x2-x1), -args.ay*(y2-y1))
		LG.translate(-x1, -y1)

		if     args.mode == "fill" then  drawPolygonFill(context.points)
		elseif args.mode == "line" then  drawPolygonLine(context.points, args.thick)
		else error(args.mode) end

		LG.pop()

	----------------------------------------------------------------
	elseif command == "line" or command == "bezier" then
		if not context.points[1] then  return (tokenError(context, startTok, "No points added."))  end
		if not context.points[3] then  return (tokenError(context, startTok, "Not enough points added."))  end

		local points = context.points

		if command == "bezier" then
			local curve = love.math.newBezierCurve(points)
			points      = curve:render(args.depth)
			curve:release()
		end

		ensureCanvasAndInitted(context)

		local x1 =  1/0
		local x2 = -1/0
		local y1 =  1/0
		local y2 = -1/0

		for i = 1, #points, 2 do
			x1 = math.min(x1, points[i  ])
			x2 = math.max(x2, points[i  ])
			y1 = math.min(y1, points[i+1])
			y2 = math.max(y2, points[i+1])
		end

		local colorW = x2 - x1
		local colorH = y2 - y1

		if not args.shift then
			x1, y1 = 0, 0
		end

		applyCanvas(context)
		applyColor(context, "rectangle", colorW,colorH)

		LG.push()
		LG.translate(args.x+x1, args.y+y1)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(-args.ax*(x2-x1), -args.ay*(y2-y1))
		LG.translate(-x1, -y1)
		drawLine(points, args.thick)
		LG.pop()

	----------------------------------------------------------------
	elseif command == "text" then
		if not (args.align == "left" or args.align == "center" or args.align == "right" or args.align == "justify") then
			return (tokenError(context, startTok, "Bad alignment '%s'. Must be 'left', 'center', 'right' or 'justify'.", args.align))
		end

		ensureCanvasAndInitted(context)

		local font         = context.gfxState.font
		local w, textLines = font:getWrap(args.text, args.wrap)
		local h            = (#textLines-1) * math.floor(font:getHeight()*args.lineh) + font:getHeight()

		if not font:hasGlyphs(args.text) then
			tokenWarning(context, startTok, "Current font is missing some glyphs for the text. Skipping those characters.")
		end

		font:setFilter(args.filter and "linear" or "nearest")
		font:setLineHeight(args.lineh)
		applyCanvas(context)
		applyColor(context, "rectangle", 1,1) -- @Incomplete: Handle gradients for text/glyphs like the other shapes, somehow.
		LG.setFont(font)

		LG.push()
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.scale(args.sx, args.sy)
		LG.shear(args.kx, args.ky)
		LG.translate(math.round(-args.ax*w), math.round(-args.ay*h)) -- Note: Text is the only thing we round the origin for.
		shaderSend(A.shaders.main.shader, "textBlendFix", true)
		LG.printf(args.text, 0,0, w, args.align)
		shaderSend(A.shaders.main.shader, "textBlendFix", false)
		LG.pop()

	----------------------------------------------------------------
	elseif command == "image" then
		if args.path == "" then  return (tokenError(context, startTok, "Missing path."))  end

		ensureCanvasAndInitted(context)

		local imageOrCanvas = getOrLoadImage(context, startTok, args.path)
		if not imageOrCanvas then  return nil  end

		local iw,ih = imageOrCanvas:getDimensions()

		imageOrCanvas:setFilter(args.filter and "linear" or "nearest")
		applyCanvas(context)
		applyColor(context, "rectangle", iw*args.sx,ih*args.sy)

		LG.draw(imageOrCanvas, args.x,args.y, args.rot, args.sx,args.sy, args.ax*iw,args.ay*ih, args.kx,args.ky)

	----------------------------------------------------------------
	elseif command == "layer" then
		local layerName = args.name
		if layerName == "" then  return (tokenError(context, startTok, "Missing name."))  end

		ensureCanvasAndInitted(context)

		local texture = context.layers[layerName]
		if not texture then
			return (tokenError(context, startTok, "No layer '%s'", layerName))
		end

		if texture == context.gfxState.canvas then
			gfxStateSetCanvas(context, context.gfxState.fallbackCanvas, nil) -- This is like an automatic `setlayer""`. :SetNoLayer
			-- LG.setCanvas(nil) ; texture:newImageData():encode("png", "layer.png") -- DEBUG
			-- return (tokenError(context, startTok, "Cannot draw layer '%s' as its currently active.", layerName))
		elseif isCanvasReferenced(context, texture) then
			return (tokenError(context, startTok, "Cannot draw layer '%s' at this point.", layerName))
		end

		local iw,ih = texture:getDimensions()

		texture:setFilter(args.filter and "linear" or "nearest")
		applyCanvas(context)
		applyColor(context, "rectangle", iw*args.sx,ih*args.sy)

		LG.draw(texture, args.x,args.y, args.rot, args.sx,args.sy, args.ax*iw,args.ay*ih, args.kx,args.ky)

	----------------------------------------------------------------
	elseif command == "pattern" then
		local layerName = args.layer
		if layerName == "" then  return (tokenError(context, startTok, "Missing layer name."))  end

		ensureCanvasAndInitted(context)

		local texture = context.layers[layerName]
		if not texture then  return (tokenError(context, startTok, "No layer '%s'.", layerName))  end

		if texture == context.gfxState.canvas then
			gfxStateSetCanvas(context, context.gfxState.fallbackCanvas, nil) -- This is like an automatic `setlayer""`. :SetNoLayer
		elseif isCanvasReferenced(context, texture) then
			return (tokenError(context, startTok, "Cannot draw layer '%s' at this point.", layerName))
		end

		local cw,ch = context.gfxState.canvas:getDimensions()
		local iw,ih = texture:getDimensions()

		texture:setFilter(args.filter and "linear" or "nearest")
		texture:setWrap((args.mirrorx and "mirroredrepeat" or "repeat"), (args.mirrory and "mirroredrepeat" or "repeat"))
		applyCanvas(context)
		applyColor(context, "rectangle", iw*args.sx,ih*args.sy)
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

	--
	-- Effects.
	--
	----------------------------------------------------------------
	elseif command == "boxblur" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			LG.setShader(A.shaders.fxBlurBox.shader)

			shaderSend    (A.shaders.fxBlurBox.shader, "radius"   , math.clamp(args.x, 0, 1000))
			shaderSendVec2(A.shaders.fxBlurBox.shader, "direction", .5, 0)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead

			shaderSend    (A.shaders.fxBlurBox.shader, "radius"   , math.clamp(args.y, 0, 1000))
			shaderSendVec2(A.shaders.fxBlurBox.shader, "direction", 0, .5)
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
			shaderSendVec2(A.shaders.fxBlurGaussian.shader, "direction", BLUR_REACH,0)
			for _ = 1, math.clamp(args.x, 0, 1000) do
				LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
				canvasRead, canvasWrite = canvasWrite, canvasRead
			end
			shaderSendVec2(A.shaders.fxBlurGaussian.shader, "direction", 0,BLUR_REACH)
			for _ = 1, math.clamp(args.y, 0, 1000) do
				LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
				canvasRead, canvasWrite = canvasWrite, canvasRead
			end

			return canvasRead
		end)

	----------------------------------------------------------------
	elseif command == "contrast" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxContrast.shader, "params", args.r,args.g,args.b,args.amount) -- rgb is a channel filter.
			LG.setShader(A.shaders.fxContrast.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "brightness" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxBrightness.shader, "params", args.r,args.g,args.b,args.amount) -- rgb is a channel filter.
			LG.setShader(A.shaders.fxBrightness.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "saturation" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxSaturation.shader, "params", args.r,args.g,args.b,args.amount) -- rgb is a channel filter.
			LG.setShader(A.shaders.fxSaturation.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "gamma" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxGamma.shader, "params", args.r,args.g,args.b,args.amount) -- rgb is a channel filter.
			LG.setShader(A.shaders.fxGamma.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "levels" then
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec2(A.shaders.fxLevels.shader, "rangeIn" , args.rangeinfrom,args.rangeinto)
			shaderSendVec2(A.shaders.fxLevels.shader, "rangeOut", args.rangeoutfrom,args.rangeoutto)
			shaderSend    (A.shaders.fxLevels.shader, "middle"  , args.mid)
			LG.setShader(A.shaders.fxLevels.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "overlay" then -- Fade pixels toward color.
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxOverlay.shader, "params", args.r,args.g,args.b,args.a)
			LG.setShader(A.shaders.fxOverlay.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	elseif command == "tint" then -- Change hue and saturation, leave brighness.
		applyEffect(context, function(context, canvasRead, canvasWrite)
			shaderSendVec4(A.shaders.fxTint.shader, "params", args.r,args.g,args.b,args.a)
			LG.setShader(A.shaders.fxTint.shader)
			LG.setCanvas(canvasWrite) ; LG.draw(canvasRead)
			canvasRead, canvasWrite = canvasWrite, canvasRead
			return canvasRead
		end)

	--
	-- Generators.
	--
	----------------------------------------------------------------
	elseif command == "noise" then
		ensureCanvasAndInitted(context)

		local gfxState = context.gfxState
		local canvas   = gfxState.canvas
		local cw,ch    = canvas:getDimensions()
		local iw,ih    = A.images.rectangle:getDimensions()

		shaderSendVec3(A.shaders.generateNoise.shader, "offset", args.x,args.y,args.z)
		shaderSendVec2(A.shaders.generateNoise.shader, "scale" , args.sx,args.sy)

		if gfxState.colorMode == "gradient" then
			applyColor(context, "rectangle", 1,1) -- Generates colorTexture.
			shaderSend(A.shaders.generateNoise.shader, "useColorTexture" , true)
			shaderSend(A.shaders.generateNoise.shader, "colorTexture"    , gfxState.colorTexture)
			shaderSend(A.shaders.generateNoise.shader, "colorTextureSize", gfxState.colorTexture:getWidth())
		else
			local r,g,b,a = unpack(gfxState.flatColor)
			shaderSend    (A.shaders.generateNoise.shader, "useColorTexture", false)
			shaderSendVec4(A.shaders.generateNoise.shader, "color0"         , 0,0,0,a)
			shaderSendVec4(A.shaders.generateNoise.shader, "color1"         , r*a, g*a, b*a, a)
		end

		pushTransformAndReset("alpha")
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

		applyCanvas(context)
		applyColor(context, "rectangle", cw,ch)

		LG.push()
		LG.origin()
		LG.draw(image)
		LG.pop()

		image:release()
		imageData:release()

	----------------------------------------------------------------
	elseif command == "end" or command == "else" then
		return (tokenError(context, startTok, "Unexpected '%s'.", command))
	else
		return (tokenError(context, startTok, "Unimplemented command '%s'.", command))
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

		if not (isToken(commandTok, "name", "do") or isToken(commandTok, "name", "if") or isToken(commandTok, "name", "for") or isToken(commandTok, "name", "func")) then -- @Volatile
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
	for _, canvas  in pairs(context.layers     ) do  canvas :release()  end
	for _, font    in pairs(context.fontsByPath) do  font   :release()  end

	for _, fonts in pairs(context.fontsBySize) do
		for _, font in pairs(fonts) do  font:release()  end
	end
end

local artFilesBeingLoaded = {}

-- art|nil = loadArtFile( path, isLocal )
-- Note: art.canvas has premultiplied alpha.
function _G.loadArtFile(path, isLocal)
	if artFilesBeingLoaded[path] then
		print("Error: File is already being loaded: "..path)
		return nil
	end

	printf("Loading %s...", path)
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

	entry.variables.True  = true -- @Robustness: Make these constant.
	entry.variables.False = false
	entry.variables.Huge  = 1/0
	entry.variables.Nan   = 0/0
	entry.variables.Pi    = math.pi
	entry.variables.Tau   = TAU

	entry.variables.WindowWidth  = LG.getWidth()
	entry.variables.WindowHeight = LG.getHeight()
	entry.variables.CanvasWidth  = context.canvasW
	entry.variables.CanvasHeight = context.canvasH

	-- There are just for the Lua expressions.  @Robustness: Make these constant (although their names already make them).
	entry.variables.num     = tonumber
	entry.variables.str     = tostring
	entry.variables.type    = type
	entry.variables.abs     = math.abs
	entry.variables.acos    = math.acos
	entry.variables.asin    = math.asin
	entry.variables.atan    = function(y, x)  return x and math.atan2(y, x) or math.atan(y)  end -- atan( y [, x=1 ] )
	entry.variables.ceil    = math.ceil
	entry.variables.cos     = math.cos
	entry.variables.cosh    = math.cosh
	entry.variables.deg     = math.deg
	entry.variables.exp     = math.exp
	entry.variables.floor   = math.floor
	entry.variables.fmod    = math.fmod
	entry.variables.frac    = function(n)  local int, frac = math.modf(n) ; return frac  end
	entry.variables.frexpe  = function(n)  local m, e = math.frexp(n) ; return e  end
	entry.variables.frexpm  = function(n)  return (math.frexp(n))  end
	entry.variables.int     = function(n)  return (math.modf(n))  end
	entry.variables.ldexp   = math.ldexp
	entry.variables.log     = math.log -- log( n [, base=e ] )
	entry.variables.max     = math.max
	entry.variables.min     = math.min
	entry.variables.rad     = math.rad
	entry.variables.rand    = love.math.random
	entry.variables.randf   = function(n1, n2)  return n2 and n1+(n2-n1)*love.math.random() or n1*love.math.random()  end -- randomf( [ n1=0, ] n2 )
	entry.variables.round   = math.round
	entry.variables.sin     = math.sin
	entry.variables.sinh    = math.sinh
	entry.variables.sqrt    = math.sqrt
	entry.variables.tan     = math.tan
	entry.variables.tanh    = math.tanh
	entry.variables.clock   = os.clock
	entry.variables.date    = os.date
	entry.variables.env     = os.getenv
	entry.variables.time    = os.time
	entry.variables.byte    = string.byte -- @Robustness: Handle the string metatable.
	entry.variables.char    = string.char
	entry.variables.find    = string.find
	entry.variables.format  = string.format
	entry.variables.gsub    = string.gsub
	entry.variables.lower   = string.lower
	entry.variables.match   = string.match
	entry.variables.rep     = string.rep
	entry.variables.reverse = string.reverse
	entry.variables.sub     = string.sub
	entry.variables.upper   = string.upper

	table.insert(context.scopeStack, entry)
	local loveGfxStackDepth = LG.getStackDepth()

	artFilesBeingLoaded[path] = true
	local tokPos              = runBlock(context, tokens, 1) -- Now things will happen!
	artFilesBeingLoaded[path] = nil

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

	printf("Loading %s... done in %.2f seconds!", path, love.timer.getTime()-startTime)
	return context.art.canvas and context.art
end


