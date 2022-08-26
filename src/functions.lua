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

	getLast, itemWith1
	getLineNumber
	lerp, lerp4
	printFileMessage, printFileError, printFileWarning, printFileErrorAt, printFileWarningAt
	round, clamp
	toColor32
	updateVec4
	writeFile

--============================================================]]



function _G.printFileMessage(pathOrContext, ln, s, ...)
	print(string.format("%s:%d: "..s, (pathOrContext.path or pathOrContext), ln, ...))
end
function _G.printFileError(pathOrContext, ln, s, ...)
	print(string.format("%s:%d: Error: "..s, (pathOrContext.path or pathOrContext), ln, ...))
end
function _G.printFileWarning(pathOrContext, ln, s, ...)
	print(string.format("%s:%d: Warning: "..s, (pathOrContext.path or pathOrContext), ln, ...))
end

local function printFileMessageAt(f, path, source, pos, s, ...)
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
function _G.printFileErrorAt(path, source, pos, s, ...)
	printFileMessageAt(printFileError, path, source, pos, s, ...)
end
function _G.printFileWarningAt(path, source, pos, s, ...)
	printFileMessageAt(printFileWarning, path, source, pos, s, ...)
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



function _G.round(v)
	return math.floor(v+.5)
end

function _G.clamp(v, min,max)
	return math.max(math.min(v, max), min)
end



function _G.lerp(v1,v2, t)
	return v1 + t*(v2-v1)
end

function _G.lerp4(r1,g1,b1,a1, r2,g2,b2,a2, t)
	return lerp(r1,r2, t)
	     , lerp(g1,g2, t)
	     , lerp(b1,b2, t)
	     , lerp(a1,a2, t)
end



-- colorValue8    = toColor32( colorValue )
-- r8, g8, b8, a8 = toColor32( r, g, b, a )
function _G.toColor32(r, g, b, a)
	if g then
		return clamp(round(r*255), 0, 255)
		     , clamp(round(g*255), 0, 255)
		     , clamp(round(b*255), 0, 255)
		     , clamp(round(a*255), 0, 255)
	else
		return clamp(round(r*255), 0, 255)
	end
end



-- success, error = writeFile( path, dataString )
function _G.writeFile(path, data)
	local file, err = io.open(path, "wb") -- @Incomplete: Use PhysFS.
	if not file then  return false, err  end

	file:write(data)
	file:close()

	return true
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


