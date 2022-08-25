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
	lerp, lerp4
	printFileMessage, printFileError, printFileWarning
	round, clamp
	toColor32
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


