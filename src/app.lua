--[[============================================================
--=
--=  App
--=
--=-------------------------------------------------------------
--=
--=  Art Command
--=  by Marcus 'ReFreezed' Thunström
--=
--==============================================================

	fixImageDataForSaving, normalizeImageAndMultiplyAlpha

--============================================================]]

_G.TAU = 2*math.pi



_G.A = { -- Assets.
	images  = {},
	quads   = {},
	shaders = {},
}

local thePathIn     = ""
local thePathOut    = "" -- Empty means auto.
local thePathIsTest = false -- @Cleanup: Make this into _G.isLocal or something.
local theArt        = nil

local autoZoom  = false
local pixelated = false



local function tryLoadingTheArtFile()
	local art = loadArtFile(thePathIn, thePathIsTest)
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
	local parseOptions = true
	local i            = 1

	while args[i] do
		local arg = args[i]

		if not (parseOptions and arg:find"^%-") then
			if arg       == "" then  error("[ProgramArguments] Path cannot be empty.", 0)  end
			if thePathIn ~= "" then  error("[ProgramArguments] Multiple paths specified.", 0)  end
			thePathIn = arg

		elseif arg == "--" then
			parseOptions = false

		elseif arg == "--out" then
			if thePathOut ~= "" then  error("[ProgramArguments] Multiple output paths specified.", 0)  end
			thePathOut = args[i+1] or error("[ProgramArguments] Missing path after "..arg..".", 0)
			i          = i + 1

		else
			error("[ProgramArguments] Unknown option "..arg..".", 0)
		end

		i = i + 1
	end

	if thePathIn == "" then
		thePathIn     = "art/test.artcmd"
		thePathOut    = ""
		thePathIsTest = true
	end

	-- Load stuff.
	require"table.clear"

	_G.LF = love.filesystem
	_G.LG = love.graphics

	_G.ffi   = require"ffi"
	_G.math  = require"src.math"
	_G.mathv = math.v

	require"functions" -- First!
	require"art"
	require"draw"
	require"filesystem"

	for _, filename in ipairs(LF.getDirectoryItems"src/shaders") do
		local name = filename:match"^(.+)%.gl$"

		if name then
			local path      = "src/shaders/" .. filename
			A.shaders[name] = LG.newShader(path)

			if DEV then
				require"hotLoader".monitor(path, function(path)
					A.shaders[name] = LG.newShader(path)
					tryLoadingTheArtFile()
				end)
			end
		end
	end

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

	initFilesystem()
end



-- imageData8 = fixImageDataForSaving( imageData16 )
-- Note: imageData16 is released.
function _G.fixImageDataForSaving(imageData16)
	local iw,ih      = imageData16:getDimensions()
	local imageData8 = love.image.newImageData(iw,ih, "rgba8")
	local pointer16  = ffi.cast("uint16_t*", imageData16:getFFIPointer())
	local pointer8   = ffi.cast("uint8_t*" , imageData8 :getFFIPointer())

	local anyTransparent = false
	local anyVisible     = false

	-- [[ Convert format and demultiply alpha.
	for i = 0, 4*iw*ih-1, 4 do
		if pointer16[i+3] == 0 then
			anyTransparent = true
		else
			anyVisible    = true
			pointer8[i  ] = (pointer16[i  ] * 255) / pointer16[i+3]
			pointer8[i+1] = (pointer16[i+1] * 255) / pointer16[i+3]
			pointer8[i+2] = (pointer16[i+2] * 255) / pointer16[i+3]
			pointer8[i+3] = pointer16[i+3] / 257
		end
	end
	--]]

	-- [=[ Fix colors of transparent pixels.  @Speed: This is slow for images with large areas of transparent pixels.
	if anyTransparent and anyVisible then
		--
		-- Calculate distance-to-visible-pixel map.
		--
		local distMap        = love.image.newImageData(iw,ih, "r16")
		local distMapPointer = ffi.cast("uint16_t*", distMap:getFFIPointer())
		ffi.fill(distMapPointer, distMap:getSize(), 255) -- Fill with maximum distance.

		-- Smear x.
		for y = 0, ih-1 do
			local dist = 255^2-1
			for x = 0, iw-1, 1 do -- x+
				local i           = y*iw + x
				dist              = (pointer8[i*4+3] > 0) and 0 or math.min(dist+1, distMapPointer[i])
				distMapPointer[i] = dist
			end
		end
		for y = 0, ih-1 do
			local dist = 255^2-1
			for x = iw-1, 0, -1 do -- x-
				local i           = y*iw + x
				dist              = (pointer8[i*4+3] > 0) and 0 or math.min(dist+1, distMapPointer[i])
				distMapPointer[i] = dist
			end
		end

		-- Smear y.
		for x = 0, iw-1 do
			local dist = 255^2-1
			for y = 0, ih-1, 1 do -- y+
				local i           = y*iw + x
				dist              = (pointer8[i*4+3] > 0) and 0 or math.min(dist+1, distMapPointer[i])
				distMapPointer[i] = dist
			end
		end
		for x = 0, iw-1 do
			local dist = 255^2-1
			for y = ih-1, 0, -1 do -- y-
				local i           = y*iw + x
				dist              = (pointer8[i*4+3] > 0) and 0 or math.min(dist+1, distMapPointer[i])
				distMapPointer[i] = dist
			end
		end

		--[[ DEBUG: Visualize map.
		local lowResDistMap = love.image.newImageData(iw,ih)
		local highDist      = 0
		distMap:mapPixel(function(x,y, r)
			highDist = math.max(highDist, r)
			return r,0,0,1
		end)
		lowResDistMap:mapPixel(function(x,y, r,g,b,a)
			local v = distMap:getPixel(x,y)/highDist
			return v,v,v,1
		end)
		lowResDistMap:encode("png", "dist.png"):release()
		lowResDistMap:release()
		--]]

		--
		-- Fix colors.
		--
		for y = 0, ih-1 do
			for x = 0, iw-1 do
				local i    = y*iw + x
				local dist = distMapPointer[i]

				if dist > 0 then
					local y       = y
					local iTarget = i * 4
					-- assert(pointer8[iTarget+3] == 0)

					while dist > 0 do
						local _dist = dist

						if x > 0    and distMapPointer[(y  )*iw+(x-1)] < dist then  x, dist = x-1, dist-1  end
						if y > 0    and distMapPointer[(y-1)*iw+(x  )] < dist then  y, dist = y-1, dist-1  end
						if x < iw-1 and distMapPointer[(y  )*iw+(x+1)] < dist then  x, dist = x+1, dist-1  end
						if y < ih-1 and distMapPointer[(y+1)*iw+(x  )] < dist then  y, dist = y+1, dist-1  end

						assert(dist < _dist) -- One of the conditions better have matched, lest the map is incorrect!
					end

					local iSource       = (y*iw + x) * 4
					pointer8[iTarget  ] = pointer8[iSource  ]
					pointer8[iTarget+1] = pointer8[iSource+1]
					pointer8[iTarget+2] = pointer8[iSource+2]
				end
			end
		end

		distMap:release()
	end--if anyTransparent
	--]=]

	--[[ DEBUG: All opaque!
	for i = 3, 4*iw*ih-1, 4 do
		pointer8[i] = 255
	end
	--]]

	imageData16:release()
	return imageData8
end

-- imageData16 = normalizeImageAndMultiplyAlpha( imageData )
-- Note: imageData is released.
function _G.normalizeImageAndMultiplyAlpha(imageData)
	local iw,ih       = imageData:getDimensions()
	local imageData16 = love.image.newImageData(iw,ih, "rgba16")

	imageData16:mapPixel(function(x,y, r,g,b,a) -- @Speed
		r,g,b,a = imageData:getPixel(x,y)
		return r*a, g*a, b*a, a
	end)

	imageData:release()
	return imageData16
end

function love.keypressed(key)
	if key == "escape" then
		love.event.quit()

	elseif key == "space" then
		autoZoom = not autoZoom
	elseif key == "f" then
		pixelated = not pixelated

	elseif key == "s" and love.keyboard.isDown("lctrl","rctrl") then
		if not theArt then  return  end

		if thePathIsTest then
			local pathOut = "output.png"
			print("Saving "..pathOut.."...")

			local imageData = fixImageDataForSaving(theArt.canvas:newImageData()) -- @Incomplete: A flag to disable the transparent pixel color fix.
			imageData:encode("png", pathOut):release() ; imageData:release()

			print("Saving "..pathOut.."... done!")

		else
			local pathOut = (thePathOut ~= "") and thePathOut or thePathIn:gsub("%.[^.]+$", "")..".png"
			if pathOut == thePathIn then
				print("Error: Input and output paths are the same: "..pathOut)
				return
			end

			print("Saving "..pathOut.."...")

			local imageData = fixImageDataForSaving(theArt.canvas:newImageData()) -- @Incomplete: A flag to disable the transparent pixel color fix.
			local fileData  = imageData:encode("png") ; imageData:release()
			local s         = fileData:getString()    ; fileData :release()
			local ok, err   = writeFile(thePathIsTest, pathOut, s)

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
	if DEV then
		require"hotLoader".update(dt)
	end

	modtimeCheckTime = modtimeCheckTime - dt

	if modtimeCheckTime < 0 then
		modtimeCheckTime = 0.20
		local modtime    = getFileModificationTime(thePathIsTest, thePathIn)

		if modtime and modtime ~= theModtime then
			theModtime = modtime
			tryLoadingTheArtFile()
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
		local cw = theArt.canvas:getWidth()
		local ch = theArt.canvas:getHeight()
		local x  = math.floor((ww - cw) / 2)
		local y  = math.floor((wh - ch) / 2)

		if not autoZoom then
			x = math.max(x, 0)
			y = math.max(y, 0)
		end

		if theArt.backdrop[4] > 0 then
			LG.setColor(theArt.backdrop)
			LG.rectangle("fill", 0,0, ww,wh)
		end

		LG.translate(math.floor(ww/2),math.floor(wh/2))
		LG.scale(autoZoom and math.min(ww/cw, wh/ch) or theArt.zoom)
		LG.translate(-math.floor(ww/2),-math.floor(wh/2))

		LG.setColor(0, 0, 0)
		LG.rectangle("fill", x-1,y-1 , cw+2,1)
		LG.rectangle("fill", x-1,y+ch, cw+2,1)
		LG.rectangle("fill", x-1,y-1 , 1,ch+2)
		LG.rectangle("fill", x+cw,y-1, 1,ch+2)

		LG.setColor(1, 1, 1)
		LG.setBlendMode("alpha", "premultiplied")
		theArt.canvas:setFilter(pixelated and "nearest" or "linear")
		LG.draw(theArt.canvas, x,y)

	else
		local text = "No art loaded"
		local w    = LG.getFont():getWidth(text) -- @Incomplete: Use predictable font.
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



if DEV then
	function love.errorhandler(err)
		print(debug.traceback(tostring(err), 2))
	end
	love.errhand = love.errorhandler
end



function love.run()
	local rawArgs = arg
	_G.arg        = nil -- Aaarrrgh!!!

	love.load(love.arg.parseGameArguments(rawArgs), rawArgs)
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


