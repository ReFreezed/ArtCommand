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



local COMMANDS = {
	-- command = { {argName1,defaultValue}, ... }, -- defaultValue=nil means the value can be any type.

	-- Language.
	["set" ] = { {"var",""}, {"value",nil} }, -- Set variable.
	["setx"] = { {"var",""}, {"value",nil} }, -- Set existing variable.

	["do" ] = { },
	["for"] = { {"var","I"}, {"from",1},{"to",0},{"step",1} },
	["end"] = { }, -- Dummy, for error message.

	["stop"] = { {"all",false} },

	["assert"] = { {"value",nil} },
	["print" ] = { {"value",nil} },

	-- Special commands: "func", user defined functions.

	-- Settings, app. (Doesn't affect the image.)
	["backdrop"] = { {"r",0},{"g",0},{"b",0},{"a",1}, rgb={"r","g","b"} },
	["zoom"    ] = { {"zoom",1} }, -- There is also autoZoom!

	-- Settings, init.
	["canvas"] = { {"w",0--[[=DEFAULT_ART_SIZE]]},{"h",0--[[=DEFAULT_ART_SIZE]]}, {"aa",1}, size={"w","h"} },

	-- State.
	["push"] = { },
	["pop" ] = { },

	["color"] = { {"r",1},{"g",1},{"b",1},{"a",1}, rgb={"r","g","b"} },
	["grey" ] = { {"grey",1},{"a",1} }, -- Short for `color rgbN`.
	["grad" ] = { {"r",1},{"g",1},{"b",1},{"a",1}, {"angle",0}, {"scale",1}, {"radial",false},{"fit",true}, rgb={"r","g","b"} },
	["font" ] = { {"size",12} },

	["makemask"] = { {"clear",true} },
	["mask"    ] = { {"mask",true} },

	["origin"] = { },
	["move"  ] = { {"x",0},{"y",0}, xy={"x","y"} },
	["rotate"] = { {"rot",0} },
	["scale" ] = { {"x",1},{"y",1}, scale={"x","y"} },

	["setlayer"] = { {"name",""}, {"w",0--[[=context.canvasW]]},{"h",0--[[=context.canvasH]]}, {"aa",0--[[=context.msaa]]}, size={"w","h"} },

	-- Drawing.
	["fill"] = { {"r",0},{"g",0},{"b",0},{"a",1}, rgb={"r","g","b"} }, -- A rectangle that covers the whole screen.

	["rect"  ] = { {"mode","fill"}, {"x",0},{"y",0}, {"w",10},{"h",10}, {"ax",0},{"ay",0}, {"rot",0}, {"thick",1}, anchor={"ax","ay"}, size={"w","h"}, xy={"x","y"} },
	["circle"] = { {"mode","fill"}, {"x",0},{"y",0}, {"rx",5},{"ry",5}, {"ax",.5},{"ay",.5}, {"rot",0}, {"segs",0--[[=auto]]}, {"thick",1}, r={"rx","ry"}, anchor={"ax","ay"}, xy={"x","y"} },
	["text"  ] = { {"text",""}, {"x",0},{"y",0}, {"ax",0},{"ay",0}, {"rot",0}, {"wrap",1/0},{"align","left"}, anchor={"ax","ay"}, xy={"x","y"} },
	["image" ] = { {"path",""}, {"x",0},{"y",0}, {"ax",0},{"ay",0}, {"sx",1},{"sy",1}, {"rot",0}, {"filter","linear"}, anchor={"ax","ay"}, scale={"sx","sy"}, xy={"x","y"} },
	["layer" ] = { {"name",""}, {"x",0},{"y",0}, {"ax",0},{"ay",0}, {"sx",1},{"sy",1}, {"rot",0}, {"filter","linear"}, anchor={"ax","ay"}, scale={"sx","sy"}, xy={"x","y"} },
}



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

	canvasToMask = nil,
	maskCanvas   = nil,

	fonts  = {--[[ [size1]=font        , ... ]]},
	images = {--[[ [path1]=image       , ... ]]},
	layers = {--[[ [name1]=image|canvas, ... ]]},

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



-- gfxStateSetCanvas( context, canvas, fallbackCanvas=canvas )
local function gfxStateSetCanvas(context, canvas, fallbackCanvas)
	context.gfxState.canvas         = canvas
	context.gfxState.fallbackCanvas = fallbackCanvas or canvas
end



local function applyCanvas(context)
	LG.setCanvas(context.gfxState.canvas)
	shaderSend(shaderMain, "makeMaskMode", context.gfxState.makeMaskMode) -- Maybe not the best place for this, but eh...
end

-- applyColor( context, shapeToDraw, relativeShapeWidth,relativeShapeHeight )
-- shapeToDraw = "rectangle" | "circle"
local function applyColor(context, shape, w,h)
	local gfxState = context.gfxState

	if gfxState.colorMode == "flatcolor" then
		LG.setColor(gfxState.flatColor)
		shaderSend(shaderMain, "useColorTexture", false)
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

	shaderSend    (shaderMain, "useColorTexture"   , true)
	shaderSend    (shaderMain, "colorTextureRadial", gfxState.colorTextureRadial)
	shaderSend    (shaderMain, "colorTexture"      , gfxState.colorTexture)
	shaderSendVec4(shaderMain, "colorTextureLayout", sx,sy, dirOrOffsetX,dirY)
end



local function ensureCanvasAndInitted(context)
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

	shaderSend(shaderMain, "makeMaskMode"   , false)
	shaderSend(shaderMain, "useColorTexture", false)

	gfxStateSetCanvas(context, context.art.canvas, nil)

	applyCanvas(context)
	LG.setShader(shaderMain)
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
		shaderSend(shaderApplyMask, "mask", context.maskCanvas)

		LG.setCanvas(nil) -- Before push/pop. Not sure it affects canvas switching. Probably doesn't matter.
		LG.push("all")
		LG.reset()
		LG.setCanvas(gfxState.canvas)
		LG.setBlendMode("alpha", "premultiplied")
		LG.setShader(shaderApplyMask)
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

local runBlock

-- tokenPosition|nil, stop = runCommand( context, tokens, tokenPosition, commandToken|nil ) -- commandToken must be set for sequences.
local function runCommand(context, tokens, tokPos, commandTok)
	local startTok   = tokens[tokPos]
	local isSequence = commandTok ~= nil

	--
	-- Function call.
	--
	if isToken((commandTok or tokens[tokPos]), "username") then
		local funcName = (commandTok or tokens[tokPos]).value
		local funcInfo = findInStack(context, "functions", funcName)
		if not funcInfo then  return (tokenError(context, startTok, "No function '%s'.", funcName))  end
		if not commandTok then
			tokPos = tokPos + 1 -- username
		end

		if context.callStack[funcInfo] then
			return (tokenError(context, startTok, "Recursively calling '%s'. (%s:%d)", funcName, context.path, getLineNumber(context.source, funcInfo.token.position)))
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

		return collectBodyTokens(context, tokens, startTok, tokPos, funcInfo.tokens), STOP_CONTINUE -- May return nil.
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
		if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "Invalid ','."))  end

		-- Get block body.
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens)
		if not tokPos then  return nil  end

		-- Run block.
		local entry = ScopeStackEntry()
		table.insert(context.scopeStack, entry)

		local bodyTokPos, stop = runBlock(context, bodyTokens, 1)
		if not bodyTokPos         then  return nil  end
		if bodyTokens[bodyTokPos] then  return (tokenError(context, bodyTokens[bodyTokPos], "Unexpected token."))  end

		table.remove(context.scopeStack)

		if stop == STOP_ALL then  return 1/0, STOP_ALL  end

	elseif command == "for" then
		if args.var  == "" then  return (tokenError(context, startTok, "Empty variable name."))  end
		if args.step == 0  then  return (tokenError(context, startTok, "Step is zero."))  end
		if not validateVariableName(context, startTok, "variable name", args.var) then  return nil  end

		if isToken(tokens[tokPos], ",") then  return (tokenError(context, tokens[tokPos], "Invalid ','."))  end

		-- Get loop body.
		local bodyTokens = {}
		tokPos           = collectBodyTokens(context, tokens, startTok, tokPos, bodyTokens)
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

	elseif command == "stop" then
		if args.all then  print("Stopping all!")  end
		return 1/0, (args.all and STOP_ALL or STOP_ONE)

	elseif command == "assert" then
		if not args.value then  return (tokenError(context, startTok, "Assertion failed!"))  end

	elseif command == "print" then
		print(args.value)

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

		context.canvasW = (args.w >= 1) and args.w or DEFAULT_ART_SIZE
		context.canvasH = (args.h >= 1) and args.h or DEFAULT_ART_SIZE
		context.msaa    = args.aa^2

		context.scopeStack[1].variables.CanvasWidth  = context.canvasW
		context.scopeStack[1].variables.CanvasHeight = context.canvasH

	--
	-- State.
	--
	elseif command == "push" then
		ensureCanvasAndInitted(context)
		pushGfxState(context, "user")

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

	elseif command == "color" or command == "grey" then
		ensureCanvasAndInitted(context)

		local gfxState     = context.gfxState
		gfxState.colorMode = "flatcolor"
		table.clear(gfxState.gradient)

		if command == "color" then  updateColor(gfxState.flatColor, args.r,args.g,args.b,args.a)
		else                        updateColor(gfxState.flatColor, args.grey,args.grey,args.grey,args.a)  end

		if gfxState.colorTexture then
			-- @Memory: Release image. (Consider gfxStack!)
			gfxState.colorTexture = nil
		end

	elseif command == "grad" then
		ensureCanvasAndInitted(context)

		local gfxState              = context.gfxState
		gfxState.colorMode          = "gradient"
		gfxState.colorTextureRadial = args.radial
		gfxState.colorTextureFit    = args.fit
		gfxState.colorTextureScaleX = args.scale -- colorTextureScaleY isn't used.  @Incomplete: For radial gradients it makes sense to scale y!
		gfxState.colorTextureAngle  = args.angle

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

	elseif command == "font" then
		context.fonts[args.size] = context.fonts[args.size] or LG.newFont(args.size)
		context.gfxState.font    = context.fonts[args.size]

	elseif command == "makemask" then
		ensureCanvasAndInitted(context)
		if not maybeApplyMask(context, startTok) then  return nil  end -- 'makemask' also exits 'mask' mode.

		if context.gfxState.makeMaskMode then  return (tokenError(context, startTok, "Already making a mask."))  end

		pushGfxState(context, "makemask")

		gfxStateSetCanvas(context, context.maskCanvas, nil)
		if args.clear then
			applyCanvas(context)
			LG.clear(0, 0, 0, 1)
		end

		context.gfxState.makeMaskMode = true
		updateColor(context.gfxState.flatColor, 1,1,1,1) -- Should we undo this when we exit makemask mode? Probably not as other things, like font, don't. (Should we also update colorMode?)

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
			applyCanvas(context)
			LG.clear(0, 0, 0, 0)
		else
			-- void  We would apply the mask, but we just did that!
		end

	elseif command == "origin" then
		LG.origin()
	elseif command == "move" then
		LG.translate(args.x, args.y)
	elseif command == "rotate" then
		LG.rotate(args.rot)
	elseif command == "scale" then
		LG.scale(args.x, (args.y ~= args.y and args.x or args.y))

	elseif command == "setlayer" then
		ensureCanvasAndInitted(context)

		if args.name == "" then
			gfxStateSetCanvas(context, context.gfxState.fallbackCanvas, nil) -- :SetNoLayer

		else
			if context.layers[args.name] then
				LG.setCanvas(nil) -- Don't accidentally release the current canvas!
				context.layers[args.name]:release()
			end

			context.layers[args.name] = LG.newCanvas(args.w,args.h, {format="rgba16", msaa=args.aa})
			gfxStateSetCanvas(context, context.layers[args.name], context.gfxState.fallbackCanvas)
			applyCanvas(context)
			LG.clear(0, 0, 0, 0) -- Needed? I think the values are 0 by default.
		end

	--
	-- Drawing.
	--
	elseif command == "fill" then
		ensureCanvasAndInitted(context)

		applyCanvas(context)
		LG.setColor(args.r, args.g, args.b, args.a)

		-- Note: We don't use clear() because of shaders and stuff. This is a drawing operation!
		LG.push()
		LG.origin()
		LG.rectangle("fill", -1,-1, context.gfxState.canvas:getWidth()+2,context.gfxState.canvas:getHeight()+2) -- Not sure if the bleeding is necessary (if msaa>1).
		LG.pop()

	elseif command == "rect" then
		if not (args.mode == "fill" or args.mode == "line") then  return (tokenError(context, startTok, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode))  end

		ensureCanvasAndInitted(context)

		applyCanvas(context)
		applyColor(context, "rectangle", args.w,args.h)

		LG.push()
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.translate(-args.ax*args.w, -args.ay*args.h)

		if     args.mode == "fill" then  drawRectangleFill(0,0, args.w,args.h)
		elseif args.mode == "line" then  drawRectangleLine(0,0, args.w,args.h, args.thick)
		else error(args.mode) end

		LG.pop()

	elseif command == "circle" then
		if not (args.mode == "fill" or args.mode == "line") then  return (tokenError(context, startTok, "Bad draw mode '%s'. Must be 'fill' or 'line'.", args.mode))  end

		ensureCanvasAndInitted(context)

		local segs = (args.segs > 0) and math.max(args.segs, 3) or math.max(math.floor(math.max(args.rx,args.ry)*TAU/10), 64)

		applyCanvas(context)
		applyColor(context, "circle", args.rx,args.ry)

		LG.push()
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.translate(-(args.ax*2-1)*args.rx, -(args.ay*2-1)*args.ry)

		if     args.mode == "fill" then  drawCircleFill(0,0, args.rx,args.ry, segs)
		elseif args.mode == "line" then  drawCircleLine(0,0, args.rx,args.ry, segs, args.thick)
		else error(args.mode) end

		LG.pop()

	elseif command == "text" then
		if not (args.align == "left" or args.align == "center" or args.align == "right" or args.align == "justify") then
			return (tokenError(context, startTok, "Bad alignment '%s'. Must be 'left', 'center', 'right' or 'justify'.", args.align))
		end

		ensureCanvasAndInitted(context)

		local font         = context.gfxState.font
		local w, textLines = font:getWrap(args.text, args.wrap)
		local h            = (#textLines-1) * math.floor(font:getHeight()*font:getLineHeight()) + font:getHeight() -- @Incomplete: Line height argument.

		applyCanvas(context)
		applyColor(context, "rectangle", 1,1) -- @Incomplete: Handle gradients for text like the other "shapes", somehow.
		LG.setFont(font)

		LG.push()
		LG.translate(args.x, args.y)
		LG.rotate(args.rot)
		LG.translate(round(-args.ax*w), round(-args.ay*h)) -- Note: Text is the only thing we round the origin for.
		LG.printf(args.text, 0,0, w, args.align)
		LG.pop()

	elseif command == "image" then
		if args.path == ""                                           then  return (tokenError(context, startTok, "Missing path."))  end
		if not (args.filter == "linear" or args.filter == "nearest") then  return (tokenError(context, startTok, "Bad filter '%s'. Must be 'linear' or 'nearest'.", args.filter))  end

		ensureCanvasAndInitted(context)

		local imageOrCanvas = context.images[args.path]

		if not imageOrCanvas then
			local path = makePathAbsolute(args.path, (context.path:gsub("[^/\\]+$", "")))

			if args.path:find"%.artcmd$" then
				pushGfxState(context, "user")

				local art = loadArtFile(path, context.isLocal)
				if not art then  return (tokenError(context, startTok, "Could not load .artcmd image."))  end
				imageOrCanvas = art.canvas

				assert(popGfxState(context, "user") == "success")

				local imageData = fixImageDataForSaving(imageOrCanvas:newImageData()) ; imageOrCanvas:release() -- @Speed: Is there a way to use imageOrCanvas directly? (See :PremultipliedArtCanvas)
				-- imageData:encode("png", "image.png"):release() -- DEBUG
				imageOrCanvas = LG.newImage(imageData) ; imageData:release()

			else
				local s, err = readFile(false, path)
				if not s then  return (tokenError(context, startTok, "Could not read '%s'. (%s)", path, err))  end

				local fileData       = LF.newFileData(s, path)
				local ok, imageOrErr = pcall(LG.newImage, fileData) ; fileData:release()
				if not ok then  return (tokenError(context, startTok, "Could not load '%s'. (%s)", path, imageOrErr))  end
				imageOrCanvas = imageOrErr
			end

			-- imageOrCanvas:setFilter("nearest") -- Fixes weird fuzziness, but also messes up scaling and rotation etc. Is there a good solution here? For now, just use a 'filter' argument.
			context.images[args.path] = imageOrCanvas
		end

		local iw,ih = imageOrCanvas:getDimensions()

		imageOrCanvas:setFilter(args.filter)
		applyCanvas(context)
		applyColor(context, "rectangle", iw*args.sx,ih*args.sy)

		-- if imageOrCanvas:typeOf("Canvas") then
		-- 	LG.push("all")
		-- 	LG.setBlendMode("alpha", "premultiplied") -- This doesn't work well with loveColor.a<1! Can the shader do something to fix it? :PremultipliedArtCanvas
		-- 	LG.draw(imageOrCanvas)
		-- 	LG.pop()
		-- else
			LG.draw(imageOrCanvas, args.x,args.y, args.rot, args.sx,args.sy, args.ax*iw,args.ay*ih)
		-- end

	elseif command == "layer" then
		local layerName = args.name
		if layerName == "" then  return (tokenError(context, startTok, "Missing name."))  end

		local imageOrCanvas = context.layers[layerName]
		if not imageOrCanvas then
			return (tokenError(context, startTok, "No layer '%s'", layerName))
		end

		if imageOrCanvas:typeOf("Canvas") then
			if imageOrCanvas == context.gfxState.canvas then
				gfxStateSetCanvas(context, context.gfxState.fallbackCanvas, nil) -- This is like an automatic `setlayer""`. :SetNoLayer
				-- return (tokenError(context, startTok, "Cannot draw layer '%s' as its currently active.", layerName))
			elseif isCanvasReferenced(context, imageOrCanvas) then
				return (tokenError(context, startTok, "Cannot draw layer '%s' at this point.", layerName))
			end

			LG.setCanvas(nil) -- Don't accidentally call newImageData() on the current canvas!
			local imageData = fixImageDataForSaving(imageOrCanvas:newImageData()) ; imageOrCanvas:release() -- @Speed: Is there a way to use imageOrCanvas directly? (See :PremultipliedArtCanvas)
			-- imageData:encode("png", "layer.png"):release() -- DEBUG
			imageOrCanvas = LG.newImage(imageData) ; imageData:release()

			context.layers[layerName] = imageOrCanvas
		end

		local iw,ih = imageOrCanvas:getDimensions()

		imageOrCanvas:setFilter(args.filter)
		applyCanvas(context)
		applyColor(context, "rectangle", iw*args.sx,ih*args.sy)

		LG.draw(imageOrCanvas, args.x,args.y, args.rot, args.sx,args.sy, args.ax*iw,args.ay*ih)

	elseif command == "end" then
		return (tokenError(context, startTok, "Unexpected 'end'."))
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

		if not (isToken(commandTok, "name", "do") or isToken(commandTok, "name", "for") or isToken(commandTok, "name", "func")) then -- @Volatile
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

	if context.canvasToMask then  context.canvasToMask:release()  end
	if context.maskCanvas   then  context.maskCanvas  :release()  end

	for _, font          in pairs(context.fonts ) do  font         :release()  end
	for _, image         in pairs(context.images) do  image        :release()  end
	for _, imageOrCanvas in pairs(context.layers) do  imageOrCanvas:release()  end
end

local artFilesBeingLoaded = {}

-- art|nil = loadArtFile( path, isLocal )
-- Note: art.canvas has premultiplied alpha.
function _G.loadArtFile(path, isLocal)
	if artFilesBeingLoaded[path] then
		print("Error: File is already being loaded: "..path)
		return nil
	end

	print("Loading "..path.."...")
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
	entry.variables.round   = function(n)  return math.floor(n+.5)  end
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

	print("Loading "..path.."... done!")
	return context.art.canvas and context.art
end


