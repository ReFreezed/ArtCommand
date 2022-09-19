--[[============================================================
--=
--=  Misc. functions
--=
--=-------------------------------------------------------------
--=
--=  Art Command
--=  by Marcus 'ReFreezed' Thunström
--=
--==============================================================

	errorf
	F
	getLast, itemWith1, indexOf
	getLineNumber
	makePathAbsolute
	printf, printFileMessage, printFileError, printFileWarning, printFileMessageAt, printFileErrorAt, printFileWarningAt
	toColor32
	updateVec4

--============================================================]]



_G.F = string.format



function _G.printf(s, ...)
	print(F(s, ...))
end

function _G.printFileMessage(pathOrContext, ln, s, ...)
	printf("%s:%d: "..s, (pathOrContext.path or pathOrContext), ln, ...)
end
function _G.printFileError(pathOrContext, ln, s, ...)
	printf("%s:%d: Error: "..s, (pathOrContext.path or pathOrContext), ln, ...)
end
function _G.printFileWarning(pathOrContext, ln, s, ...)
	printf("%s:%d: Warning: "..s, (pathOrContext.path or pathOrContext), ln, ...)
end

local function _printFileMessageAt(f, path, source, pos, s, ...)
	local lineEndPos     = source:find("\n", pos) or #source+1
	local startToCurrent = source:sub(1, lineEndPos-1)
	local ln             = getLineNumber(startToCurrent, #startToCurrent)
	local lastFewLines   = startToCurrent:reverse():match"^[^\n]*\n*[^\n]*":reverse() -- Last 2 lines.
	local lastLine       = lastFewLines:match"[^\n]*$"
	local lineStartPos   = lineEndPos - #lastLine
	local arrowLength    = pos - lineStartPos
	local text           = lastFewLines:gsub("\n\n+", "\n"):gsub("[^\n]+", "> %0"):gsub("\t", "    ")

	local _, tabCount = lastLine:sub(1, arrowLength):gsub("\t", "\t")
	arrowLength       = arrowLength + tabCount*3

	f(path, ln, s, ...)
	print(">")
	print(text)
	print(">-"..("-"):rep(arrowLength).."^")
end
function _G.printFileMessageAt(path, source, pos, s, ...)
	_printFileMessageAt(printFileMessage, path, source, pos, s, ...)
end
function _G.printFileErrorAt(path, source, pos, s, ...)
	_printFileMessageAt(printFileError, path, source, pos, s, ...)
end
function _G.printFileWarningAt(path, source, pos, s, ...)
	_printFileMessageAt(printFileWarning, path, source, pos, s, ...)
end



function _G.errorf(level, s, ...)
	error(F(s, ...), 1+level)
end



function _G.getLast(arr)
	return arr[#arr]
end

function _G.itemWith1(arr, k, v)
	for i = 1, #arr do
		if arr[i][k] == v then  return arr[i], i  end
	end
	return nil
end

function _G.indexOf(arr, v)
	for i = 1, #arr do
		if arr[i] == v then  return i  end
	end
	return nil
end



-- colorValue8    = toColor32( colorValue )
-- r8, g8, b8, a8 = toColor32( r, g, b, a )
function _G.toColor32(r, g, b, a)
	if g then
		return math.clamp(math.round(r*255), 0, 255)
		     , math.clamp(math.round(g*255), 0, 255)
		     , math.clamp(math.round(b*255), 0, 255)
		     , math.clamp(math.round(a*255), 0, 255)
	else
		return math.clamp(math.round(r*255), 0, 255)
	end
end



function _G.getLineNumber(s, i)
	local _, nlCount = s:sub(1, i):gsub("\n", "\n")
	return nlCount + 1
end



function _G.updateVec4(vec4, x,y,z,w)
	vec4[1] = x
	vec4[2] = y
	vec4[3] = z
	vec4[4] = w
end



function _G.makePathAbsolute(path, baseDirPrefix)
	return (path:find"^~?[/\\]" or path:find"^%a:") and path or baseDirPrefix..path
end


