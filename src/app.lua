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
	newShader

--============================================================]]

_G.TAU = 2*math.pi
_G.DEV = love.filesystem.getInfo"local/dev" ~= nil

local VERSION_STRING = "v1.0"



local GUI_TEXT_PADDING_X = 8
local GUI_TEXT_PADDING_Y = 4
local GUI_SPACING        = 8

local GUI_BOX_SPEED   = 20
local GUI_INPUT_WIDTH = 180

local GUI_STATUS_DISPLAY_TIME = 1.50
local GUI_STATUS_SPEED        = 1.5

local GUI_OPACITY = .8



_G.guiMode          = true
local guiBox        = nil
local guiVisibility = 0

local guiPressedItem = nil
local guiFocusedItem = nil



_G.A = { -- Assets.
	fonts   = {},
	images  = {},
	quads   = {},
	shaders = {},
}

local thePathIn     = ""
local thePathOut    = "" -- Empty means auto.
local thePathIsTest = false -- @Cleanup: Make this into _G.isLocal or something.
local theArt        = nil

local autoZoom = 1 --  1 (use art zoom)  |  2 (zoom, linear)  |  3 (zoom, pixelated)
local AUTO_ZOOM_TEXTS = {"auto-zoom = off", "auto-zoom = on", "auto-zoom = on (pixelated)"}



local statusText = ""
local statusTime = -9999.00

local function setStatus(s, ...)
	statusText = F(s, ...)
	statusTime = love.timer.getTime()
end



local function freeTheArt()
	if not theArt then  return  end
	theArt.canvas:release()
	theArt = nil
end

local function tryLoadingTheArtFile(showSuccessStatus)
	local art = loadArtFile(thePathIn, thePathIsTest)
	collectgarbage()

	if not art then
		setStatus("Failed loading %s", thePathIn)
		return
	end

	setStatus("%s", (showSuccessStatus and "Loaded "..thePathIn:gsub("^.*[/\\]", "") or ""))

	freeTheArt()
	theArt = art

	love.window.setTitle(F(
		"%s (%dx%d) - Art Command %s",
		thePathIn:gsub("^.*[/\\]", ""),
		theArt.canvas:getWidth(), theArt.canvas:getHeight(),
		VERSION_STRING
	))
end

-- saveTheArt( fileType=auto )
local function saveTheArt(fileType)
	if not theArt then
		setStatus("No art to save!")
		return
	end

	if thePathIsTest then
		fileType      = fileType or "png"
		local pathOut = "output."..fileType

		print("Saving "..pathOut.."...")

		local imageData = fixImageDataForSaving(theArt.canvas:newImageData()) -- @Incomplete: A flag to disable the transparent pixel color fix.
		imageData:encode(fileType, pathOut):release() ; imageData:release()

		print("Saving "..pathOut.."... done!")
		setStatus("Saved %s", pathOut)

	else
		fileType = fileType or (thePathOut:find"%.[Tt][Gg][Aa]$" and "tga" or "png")

		local pathOut = (thePathOut ~= "") and thePathOut or thePathIn:gsub("%.[^.]+$", "").."."..fileType
		if pathOut == thePathIn then
			print("Error: Input and output paths are the same: "..pathOut)
			setStatus("Failed saving %s: Same as input path.", pathOut)
			return
		end

		print("Saving "..pathOut.."...")

		local imageData = fixImageDataForSaving(theArt.canvas:newImageData()) -- @Incomplete: A flag to disable the transparent pixel color fix.
		local fileData  = imageData:encode(fileType) ; imageData:release()
		local s         = fileData:getString()       ; fileData :release()
		local ok, err   = writeFile(thePathIsTest, pathOut, s)

		if not ok then
			print("Error: "..err)
			setStatus("Failed saving %s: %s", pathOut, err)
			return
		end

		print("Saving "..pathOut.."... done!")
		setStatus("Saved %s", pathOut)
	end
end



local function includeShader(paths, lines, lineFiles, lineLines, s, path)
	if paths[path] then  return  end
	paths[path] = true
	table.insert(paths, path)

	local ln = 0

	for line in s:gsub("\n", " \n"):gmatch"[^\n]+" do
		ln                = ln + 1
		local includeName = line:match'^%s*#include%s+"(.-)"'

		if includeName then
			local includePath = "src/shaders/"..includeName..".gl"
			includeShader(paths, lines, lineFiles, lineLines, assert(LF.read(includePath)), includePath)
		else
			table.insert(lines, line)
			lineFiles[#lines] = path
			lineLines[#lines] = ln
		end
	end
end

function _G.newShader(s, path)
	local paths     = {}
	local lines     = {}
	local lineFiles = {}
	local lineLines = {}

	includeShader(paths, lines, lineFiles, lineLines, s, path)

	local ok, loveShaderOrErr = pcall(LG.newShader, table.concat(lines, "\n"))
	if ok then  return {shader=loveShaderOrErr, path=path, paths=paths}  end
	local err = loveShaderOrErr

	err = err:gsub("\nLine (%d+): ([^\n]*)", function(lnStr, s)
		local ln = tonumber(lnStr)
		s = (s
			:gsub("^%u+: ", "")
			:gsub("^'' : ", "")
			:gsub(" : ", ": ")
		)
		if lineFiles[ln] then  return "\n\t"..lineFiles[ln]..":"..lineLines[ln]..": "..s
		else                   return '\n\t[meta "'..path..'"]:'..lnStr..": "..s  end
	end)

	err = err:gsub("\nERROR: %d+ compilation errors.  No code generated.$", "")

	error(path..": "..err, 0)
end

local onShaderPathModified

local function loadShader(name, path)
	local shader    = newShader(assert(LF.read(path)), path)
	A.shaders[name] = shader

	if DEV then
		for _, path in ipairs(shader.paths) do
			require"hotLoader".monitor(path, onShaderPathModified)
		end
	end
end

--[[local]] function onShaderPathModified(path)
	local loadedAny = false

	for name, shader in pairs(A.shaders) do
		if indexOf(shader.paths, path) then
			local ok, err = pcall(loadShader, name, shader.path)
			if ok then
				loadedAny = true
			else
				print(err)
			end
		end
	end

	if loadedAny then
		tryLoadingTheArtFile(false)
	end
end

local function programArgumentError(s)
	if guiMode and not DEV then
		love.window.setMode(600,400)
	end
	error("[ProgramArguments] "..s, 0)
end

local function updateGuiLayout()
	local ww,wh = LG.getDimensions()

	guiBox.w = ww
	guiBox.h = A.fonts.gui:getHeight() + 2*(GUI_TEXT_PADDING_Y+GUI_SPACING)
	guiBox.x = 0
	guiBox.y = wh - guiBox.h

	local x = guiBox.x + GUI_SPACING
	local y = guiBox.y + GUI_SPACING

	for _, guiItem in ipairs(guiBox) do
		if guiItem.type == "button" then
			guiItem.w = A.fonts.gui:getWidth(guiItem.text) + 2*GUI_TEXT_PADDING_X
			guiItem.h = A.fonts.gui:getHeight()            + 2*GUI_TEXT_PADDING_Y
		elseif guiItem.type == "text" then
			guiItem.w = A.fonts.gui:getWidth(guiItem.text)
			guiItem.h = A.fonts.gui:getHeight() + 2*GUI_TEXT_PADDING_Y
		elseif guiItem.type == "input" then
			guiItem.w = GUI_INPUT_WIDTH         + 2*GUI_TEXT_PADDING_X
			guiItem.h = A.fonts.gui:getHeight() + 2*GUI_TEXT_PADDING_Y
			guiItem.field:setFont(A.fonts.gui)
			guiItem.field:setWidth(guiItem.w-2*GUI_TEXT_PADDING_X)
		else
			error(guiItem.type)
		end

		guiItem.x = x
		guiItem.y = y
		x         = x + guiItem.w + GUI_SPACING
	end

	if x < ww then
		local offsetX = math.floor((ww-x)/2)

		for _, guiItem in ipairs(guiBox) do
			guiItem.x = guiItem.x + offsetX
		end
	end
end

function love.load(args, rawArgs)
	io.stdout:setvbuf("no")
	io.stderr:setvbuf("no")

	print("Starting Art Command.")

	require"love.window"

	-- Parse arguments.
	local parseOptions = true
	local i            = 1

	while args[i] do
		local arg = args[i]

		if not (parseOptions and arg:find"^%-") then
			if arg       == "" then  programArgumentError("Path cannot be empty.")  end
			if thePathIn ~= "" then  programArgumentError("Multiple paths specified.")  end
			thePathIn = arg

		elseif arg == "--" then
			parseOptions = false

		elseif arg == "--nogui" then
			_G.guiMode = false
			_G.DEV     = false

		elseif arg == "--out" then
			if thePathOut ~= "" then  programArgumentError("Multiple output paths specified.")  end
			thePathOut = args[i+1] or programArgumentError("Missing path after "..arg..".")
			i          = i + 1

		else
			programArgumentError("Unknown option "..arg..".")
		end

		i = i + 1
	end

	if thePathIn == "" then
		thePathIn     = "tests/all.artcmd"
		thePathOut    = ""
		thePathIsTest = true
	end

	-- Create window.
	if not guiMode then
		love.window.setMode(1,1, { -- Sneaky window, because OpenGL is annoying.
			stencil    = false,
			borderless = true,
			x          = 0,
			y          = 0,
		})

	else
		local ww = DEV and 1000 or 800
		local wh = DEV and 800  or 600

		love.window.setMode(ww,wh, {
			stencil   = false,
			resizable = true,
			display   = DEV and 2 or 1,
		})

		love.window.setTitle("Art Command "..VERSION_STRING)
		-- if not love.filesystem.isFused() then  love.window.setIcon(love.image.newImageData("gfx/appIcon32.png"))  end -- @Incomplete

		love.graphics.clear()
		love.graphics.print("Loading...", love.graphics.getWidth()/2,love.graphics.getHeight()/2)
		love.graphics.present()
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
		local name = filename:match"^(%a.*)%.gl$"
		if name then
			loadShader(name, "src/shaders/"..filename)
		end
	end

	A.fonts.artDefault = LG.newFont(12)
	A.fonts.gui        = LG.newFont(13)
	A.fonts.status     = LG.newFont(14)

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
	A.images.checker:setWrap("repeat")
	A.quads.checker = LG.newQuad(0,0, 8,8, A.images.checker:getDimensions())

	initFilesystem()

	if guiMode then
		love.keyboard.setKeyRepeat(true)

		guiBox = {x=0,y=0, w=0,h=0,
			{type="button", x=0,y=0, w=0,h=0, name="zoom"    , text="Auto-zoom"},
			{type="button", x=0,y=0, w=0,h=0, name="reload"  , text="Reload"},
			{type="button", x=0,y=0, w=0,h=0, name="save"    , text="Export"},
			{type="text"  , x=0,y=0, w=0,h=0,                  text="Export path:"},
			{type="input" , x=0,y=0, w=0,h=0, name="savePath", placeholder="(auto)", field=require"InputField"(thePathOut)},
		}

		updateGuiLayout()
	end

	if not guiMode then
		tryLoadingTheArtFile(true)
		if not theArt then  error("No art loaded.", 0)  end

		saveTheArt(nil)

		print("Quitting.")
		love.event.quit()
	end
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

function love.keypressed(key, scancode, isRepeat)
	if guiFocusedItem then
		if key == "escape" or key == "return" or key == "kpenter" then
			if not guiFocusedItem.field:isBusy() then
				guiFocusedItem = nil
			end
		elseif guiFocusedItem.field:keypressed(key, isRepeat) then
			if guiFocusedItem.name == "savePath" then
				thePathOut = guiFocusedItem.field:getText()
			end
			-- void
		end

	elseif key == "escape" then
		love.event.quit()

	elseif key == "space" then
		if isRepeat then  return  end
		autoZoom = autoZoom % 3 + 1
		setStatus(AUTO_ZOOM_TEXTS[autoZoom])

	elseif key == "r" and love.keyboard.isDown("lctrl","rctrl") then
		if isRepeat then  return  end
		tryLoadingTheArtFile(true)

	elseif key == "s" and love.keyboard.isDown("lctrl","rctrl") then
		if isRepeat then  return  end
		saveTheArt(love.keyboard.isDown("lshift","rshift") and "tga" or "png")
	end
end

function love.textinput(text)
	if guiFocusedItem then
		guiFocusedItem.field:textinput(text)
		if guiFocusedItem.name == "savePath" then
			thePathOut = guiFocusedItem.field:getText()
		end
	end
end



local function isOverRect(rect, mx,my)
	return mx >= rect.x and my >= rect.y and mx < rect.x+rect.w and my < rect.y+rect.h
end

local isMeasuring   = false
local measureStartX = 0
local measureStartY = 0

function love.mousepressed(mx,my, mbutton, pressCount)
	if mbutton == 1 then
		guiFocusedItem = nil

		for _, guiItem in ipairs(guiBox) do
			if isOverRect(guiItem, mx,my) then
				love.mouse.setGrabbed(true)
				guiPressedItem = guiItem

				if guiPressedItem.type == "input" then
					guiFocusedItem = guiItem
					guiPressedItem.field:mousepressed(mx-(guiPressedItem.x+GUI_TEXT_PADDING_X), my-(guiPressedItem.y+GUI_TEXT_PADDING_Y), mbutton, pressCount)
				end

				return
			end
		end

		if isOverRect(guiBox, mx,my) then  return  end

		isMeasuring   = true
		measureStartX = mx
		measureStartY = my
		love.mouse.setGrabbed(true)
	end
end

function love.mousemoved(mx,my, dx,dy)
	if guiPressedItem and guiPressedItem.type == "input" then
		guiPressedItem.field:mousemoved(mx-(guiPressedItem.x+GUI_TEXT_PADDING_X), my-(guiPressedItem.y+GUI_TEXT_PADDING_Y))
	end
end

function love.mousereleased(mx,my, mbutton, pressCount)
	if mbutton == 1 then
		if guiPressedItem then
			love.mouse.setGrabbed(false)

			if guiPressedItem.type == "input" then
				guiPressedItem.field:mousereleased(mx-(guiPressedItem.x+GUI_TEXT_PADDING_X), my-(guiPressedItem.y+GUI_TEXT_PADDING_Y), mbutton, pressCount)
			end

			if not isOverRect(guiPressedItem, mx,my) then
				-- void
			elseif guiPressedItem.name == "zoom" then
				autoZoom = autoZoom % 3 + 1
				setStatus(AUTO_ZOOM_TEXTS[autoZoom])
			elseif guiPressedItem.name == "reload" then
				tryLoadingTheArtFile(true)
			elseif guiPressedItem.name == "save" then
				saveTheArt(nil)
			end

			guiPressedItem = nil

		elseif isMeasuring then
			isMeasuring = false
			love.mouse.setGrabbed(false)
		end
	end
end



local theModtime       = -1/0
local modtimeCheckTime = 0

function love.update(dt)
	if DEV then
		require"hotLoader".update(dt)
	end

	-- Hot-load art.
	modtimeCheckTime = modtimeCheckTime - dt

	if modtimeCheckTime < 0 then
		modtimeCheckTime = 0.20
		local modtime    = getFileModificationTime(thePathIsTest, thePathIn)

		if modtime and modtime ~= theModtime then
			theModtime = modtime
			tryLoadingTheArtFile(false)
		end
	end

	-- Update GUI.
	local showGui = (guiPressedItem ~= nil) or (guiFocusedItem ~= nil) or (love.window.hasMouseFocus() and not isMeasuring and love.mouse.getY() > guiBox.y-20)
	guiVisibility = math.clamp01(guiVisibility + GUI_BOX_SPEED*dt*(showGui and 1 or -1))
	-- guiVisibility = 1 -- DEBUG
end



local function drawTextBox(text, x,y, ax,ay)
	local w = LG.getFont():getWidth(text) + 2*GUI_TEXT_PADDING_X
	local h = LG.getFont():getHeight()    + 2*GUI_TEXT_PADDING_Y
	x       = math.clamp(math.floor(x-w*ax), 0, LG.getWidth ()-w)
	y       = math.clamp(math.floor(y-h*ay), 0, LG.getHeight()-h)

	LG.setColor(0, 0, 0, GUI_OPACITY)
	LG.rectangle("fill", x,y, w,h)

	LG.setColor(1, 1, 1)
	LG.print(text, x+GUI_TEXT_PADDING_X,y+GUI_TEXT_PADDING_Y)
end

function love.draw()
	if not LG.isActive() then  return  end

	local ww,wh = LG.getDimensions()
	local mx,my = love.mouse.getPosition()

	LG.reset()
	LG.clear(.4, .4, .4, 1)
	LG.setFont(A.fonts.gui)

	-- Art.
	local artX     = 0
	local artY     = 0
	local artScale = 1

	A.quads.checker:setViewport(0,0, ww,wh)
	LG.setColor(.6, .6, .6)
	LG.draw(A.images.checker, A.quads.checker)

	if theArt then
		local artW = theArt.canvas:getWidth()
		local artH = theArt.canvas:getHeight()
		artScale   = (autoZoom > 1) and math.min(ww/artW, wh/artH) or theArt.zoom
		artX       = math.floor((ww - artW*artScale) / 2)
		artY       = math.floor((wh - artH*artScale) / 2)

		if autoZoom == 1 then
			artX = math.max(artX, 0)
			artY = math.max(artY, 0)
		end

		if theArt.backdrop[4] > 0 then
			LG.setColor(theArt.backdrop)
			LG.rectangle("fill", 0,0, ww,wh)
		end

		LG.push()

		LG.translate(artX, artY)
		LG.scale(artScale)

		if theArt.backdrop[4] >= .8 and (theArt.backdrop[1] + theArt.backdrop[2] + theArt.backdrop[3]) < 3*.3 then
			LG.setColor(1, 1, 1, .4)
		else
			LG.setColor(0, 0, 0)
		end
		LG.rectangle("fill", -1,-1  , artW+2,1)
		LG.rectangle("fill", -1,artH, artW+2,1)
		LG.rectangle("fill", -1,0   , 1,artH  )
		LG.rectangle("fill", artW,0 , 1,artH  )

		theArt.canvas:setFilter((autoZoom == 2 and "linear") or (autoZoom == 3 and "nearest") or (theArt.zoomFilter and "linear" or "nearest"))
		LG.setColor(1, 1, 1)
		LG.setBlendMode("alpha", "premultiplied")
		LG.draw(theArt.canvas)
		LG.setBlendMode("alpha")

		LG.pop()

	else
		local text = "No art loaded"
		local w    = A.fonts.gui:getWidth(text)
		local h    = A.fonts.gui:getHeight()
		local x    = math.floor((ww-w)/2)
		local y    = math.floor((wh-h)/2)

		LG.setColor(0, 0, 0)
		LG.rectangle("fill", x-4,y-2, w+8,h+4)

		LG.setColor(1, 1, 1)
		LG.print(text, x,y)
	end

	-- GUI.
	LG.push()
	LG.translate(0, (1-guiVisibility) * (wh-guiBox.y))

	LG.setColor(0, 0, 0, GUI_OPACITY)
	LG.rectangle("fill", guiBox.x,guiBox.y, guiBox.w,guiBox.h)

	local highlightedGuiItem = nil

	for _, guiItem in ipairs(guiBox) do
		local highlight = (guiItem == guiPressedItem or not guiPressedItem) and not isMeasuring and isOverRect(guiItem, mx,my)

		if highlight then
			highlightedGuiItem = guiItem
		end

		if guiItem.type == "button" then
			if highlight then
				LG.setColor(1, 1, 1, .3)
				LG.rectangle("fill", guiItem.x,guiItem.y, guiItem.w,guiItem.h)
				LG.setColor(1, 1, 1)
				LG.rectangle("line", guiItem.x+.5,guiItem.y+.5, guiItem.w-1,guiItem.h-1)
			else
				LG.setColor(1, 1, 1, .5)
				LG.rectangle("line", guiItem.x+.5,guiItem.y+.5, guiItem.w-1,guiItem.h-1)
			end

			LG.setColor(1, 1, 1)
			LG.print(guiItem.text, guiItem.x+GUI_TEXT_PADDING_X,guiItem.y+GUI_TEXT_PADDING_Y)

		elseif guiItem.type == "input" then
			if guiItem == guiFocusedItem then
				LG.setColor(1, 1, 1, .3)
				LG.rectangle("fill", guiItem.x,guiItem.y, guiItem.w,guiItem.h)
				LG.setColor(1, 1, 1)
				LG.rectangle("line", guiItem.x+.5,guiItem.y+.5, guiItem.w-1,guiItem.h-1)
			elseif highlight then
				LG.setColor(1, 1, 1, .3)
				LG.rectangle("fill", guiItem.x,guiItem.y, guiItem.w,guiItem.h)
				LG.setColor(1, 1, 1)
				LG.rectangle("line", guiItem.x+.5,guiItem.y+.5, guiItem.w-1,guiItem.h-1)
			else
				LG.setColor(1, 1, 1, .5)
				LG.rectangle("line", guiItem.x+.5,guiItem.y+.5, guiItem.w-1,guiItem.h-1)
			end

			LG.setScissor(guiItem.x+1, guiItem.y+1, guiItem.w-2, guiItem.h-2)

			if guiItem == guiFocusedItem then
				LG.setColor(.6, .6, .7)
				for _, x, y, w, h in guiItem.field:eachSelection() do
					love.graphics.rectangle("fill", guiItem.x+GUI_TEXT_PADDING_X+x, guiItem.y+GUI_TEXT_PADDING_Y+y, w, h)
				end
			end

			if guiItem.field:getText() == "" then
				LG.setColor(1, 1, 1, .5)
				love.graphics.print(guiItem.placeholder, guiItem.x+GUI_TEXT_PADDING_X, guiItem.y+GUI_TEXT_PADDING_Y)
			else
				LG.setColor(1, 1, 1)
				for _, text, x, y in guiItem.field:eachVisibleLine() do
					love.graphics.print(text, guiItem.x+GUI_TEXT_PADDING_X+x, guiItem.y+GUI_TEXT_PADDING_Y+y)
				end
			end

			if guiItem == guiFocusedItem and guiItem.field:getBlinkPhase()%1.00 < .5 then
				local x, y, h = guiItem.field:getCursorLayout()
				LG.setColor(1, 1, 1, .8)
				love.graphics.rectangle("fill", guiItem.x+GUI_TEXT_PADDING_X+x-1, guiItem.y+GUI_TEXT_PADDING_Y+y, 2, h)
			end

			LG.setScissor()

		elseif guiItem.type == "text" then
			LG.setColor(1, 1, 1)
			LG.print(guiItem.text, guiItem.x,guiItem.y+GUI_TEXT_PADDING_Y)
		end
	end

	LG.pop()

	local mouseGuiItem = (guiPressedItem or highlightedGuiItem)
	love.mouse.setCursor((mouseGuiItem and mouseGuiItem.type == "input") and love.mouse.getSystemCursor"ibeam" or nil)

	-- Extra info.
	if love.keyboard.isDown"tab" then
		LG.setColor(0, 0, 0)
		LG.rectangle("fill", 0,0, ww,4*A.fonts.gui:getHeight()+2*4)
		LG.setColor(1, 1, 1)
		LG.print(F(
			"Art: %s"
			.."\nSize: %dx%d (%.1f megapixels)"
			.."\nTextureMemory: %d MiB"
			.."\nLuaMemory: %.2f MiB"
			, thePathIn
			, (theArt and theArt.canvas:getWidth() or 0), (theArt and theArt.canvas:getHeight() or 0), (theArt and theArt.canvas:getWidth()*theArt.canvas:getHeight() or 0)/1000^2
			, LG.getStats().texturememory/1024^2
			, collectgarbage"count"/1024
		), 4,4)
	end

	-- Status.
	if statusText ~= "" then
		local w = A.fonts.status:getWidth(statusText) + 2*GUI_TEXT_PADDING_X
		local h = A.fonts.status:getHeight()          + 2*GUI_TEXT_PADDING_Y
		local x = math.floor((ww - w) / 2)
		local y = -GUI_STATUS_SPEED * h * math.max(love.timer.getTime()-statusTime-GUI_STATUS_DISPLAY_TIME, 0)^2

		LG.setColor(0, 0, 0, GUI_OPACITY)
		LG.rectangle("fill", x,y, w,h)

		LG.setColor(1, 1, 1)
		LG.setFont(A.fonts.status)
		LG.print(statusText, x+GUI_TEXT_PADDING_X,y+GUI_TEXT_PADDING_Y)
		LG.setFont(A.fonts.gui)
	end

	-- Mouse info.
	if theArt and love.window.hasMouseFocus() and not mouseGuiItem then
		if isMeasuring then
			local x1 = math.min(measureStartX, mx)
			local x2 = math.max(measureStartX, mx)
			local y1 = math.min(measureStartY, my)
			local y2 = math.max(measureStartY, my)
			local w  = x2 - x1
			local h  = y2 - y1

			LG.setColor(1, 1, 1, .5)
			LG.rectangle("line", x1+.5,y1+.5, w-1,h-1)

			drawTextBox(F("%d", math.round(math.abs(w/artScale))), (measureStartX+mx)/2,y1, .5,1)
			drawTextBox(F("%d", math.round(math.abs(h/artScale))), x1,(measureStartY+my)/2, 1,.5)

		elseif my < guiBox.y then
			local text = F("[%d, %d]", (mx-artX)/artScale,(my-artY)/artScale)
			drawTextBox(text, mx+20,my+10, 0,0)
		end
	end

	LG.present()
end



function love.resize(ww,wh)
	updateGuiLayout()
end

function love.filedropped(file)
	freeTheArt()

	thePathIn        = file:getFilename()
	thePathOut       = ""
	thePathIsTest    = false
	theModtime       = -1/0
	modtimeCheckTime = 0

	itemWith1(guiBox, "name", "savePath").field:setText(thePathOut)

	-- love.update will load the new file.
end



local loveErrorHandler = love.errorhandler or love.errhand

function love.errorhandler(err)
	if DEV then
		print(debug.traceback(tostring(err), 2))
	elseif guiMode then
		return loveErrorHandler(err)
	else
		print("Error: "..tostring(err))
	end
end
love.errhand = love.errorhandler



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


