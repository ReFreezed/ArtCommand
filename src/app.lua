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

	fixImageDataForSaving

--============================================================]]

_G.TAU = 2*math.pi



_G.A = { -- Assets.
	images = {},
	quads  = {},
}

_G.shaderMain      = nil
_G.shaderApplyMask = nil

local hotLoader

local thePathIn     = ""
local thePathOut    = "" -- Empty means auto.
local thePathIsTest = false -- @Cleanup: Make this into _G.isLocal or something.
local theArt        = nil

local autoZoom = false



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
	_G.LG     = love.graphics
	hotLoader = require"hotLoader"

	require"functions" -- First!
	require"art"
	require"draw"

	_G.shaderMain      = LG.newShader("src/shaders/main.gl")
	_G.shaderApplyMask = LG.newShader("src/shaders/applyMask.gl")
	if DEV then
		hotLoader.monitor("src/shaders/main.gl"     , function(path)  _G.shaderMain      = LG.newShader("src/shaders/main.gl"     ) ; tryLoadingTheArtFile()  end)
		hotLoader.monitor("src/shaders/applyMask.gl", function(path)  _G.shaderApplyMask = LG.newShader("src/shaders/applyMask.gl") ; tryLoadingTheArtFile()  end)
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

	-- Start app.
	if love.system.getOS() == "Windows" then -- hotLoader works best on Windows.
		hotLoader.allowExternalPaths(true)
		hotLoader.monitor(thePathIn, function(path)
			tryLoadingTheArtFile()
		end)
		tryLoadingTheArtFile()
	end
end



-- imageData8 = fixImageDataForSaving( imageData16 )
-- Note: imageData16 is released.
function _G.fixImageDataForSaving(imageData16)
	local iw,ih          = imageData16:getDimensions()
	local imageData8     = love.image.newImageData(iw,ih, "rgba8")
	local pointer16      = require"ffi".cast("uint16_t*", imageData16:getFFIPointer())
	local pointer8       = require"ffi".cast("uint8_t*" , imageData8 :getFFIPointer())
	local anyTransparent = false

	-- Convert format and demultiply alpha.
	for i = 0, 4*iw*ih-1, 4 do
		if pointer16[i+3] == 0 then
			anyTransparent = true
		else
			pointer8[i  ] = (pointer16[i  ] * 255) / pointer16[i+3]
			pointer8[i+1] = (pointer16[i+1] * 255) / pointer16[i+3]
			pointer8[i+2] = (pointer16[i+2] * 255) / pointer16[i+3]
			pointer8[i+3] = pointer16[i+3] / 257
		end
	end

	-- Fix colors of transparent pixels.
	if anyTransparent then
		for y = 0, ih-1 do
			for x = 0, iw-1 do
				local i = (y*iw+x)*4

				if pointer8[i+3] == 0 then
					local count = 0
					local r     = 0
					local g     = 0
					local b     = 0

					for radius = 1, math.max(iw, ih) do -- @Polish @Speed: The max limit could be lower.
						for offset = 0, radius do
							-- cw
							do
								do -- up
									local x,y = x+offset,y-radius ; local i = (x>=0 and y>=0 and x<iw and y<ih) and (y*iw+x)*4 or nil ; local a = i and pointer8[i+3] or 0
									if a > 0 then  count, r,g,b = count+1, r+pointer8[i],g+pointer8[i+1],b+pointer8[i+2]  end
								end
								do -- down
									local x,y = x-offset,y+radius ; local i = (x>=0 and y>=0 and x<iw and y<ih) and (y*iw+x)*4 or nil ; local a = i and pointer8[i+3] or 0
									if a > 0 then  count, r,g,b = count+1, r+pointer8[i],g+pointer8[i+1],b+pointer8[i+2]  end
								end
								do -- left
									local x,y = x-radius,y-offset ; local i = (x>=0 and y>=0 and x<iw and y<ih) and (y*iw+x)*4 or nil ; local a = i and pointer8[i+3] or 0
									if a > 0 then  count, r,g,b = count+1, r+pointer8[i],g+pointer8[i+1],b+pointer8[i+2]  end
								end
								do -- right
									local x,y = x+radius,y+offset ; local i = (x>=0 and y>=0 and x<iw and y<ih) and (y*iw+x)*4 or nil ; local a = i and pointer8[i+3] or 0
									if a > 0 then  count, r,g,b = count+1, r+pointer8[i],g+pointer8[i+1],b+pointer8[i+2]  end
								end
							end

							-- ccw
							if offset > 0 and offset < radius then -- Avoid cardinal directions and corners as they overlap with cw.
								do -- up
									local x,y = x-offset,y-radius ; local i = (x>=0 and y>=0 and x<iw and y<ih) and (y*iw+x)*4 or nil ; local a = i and pointer8[i+3] or 0
									if a > 0 then  count, r,g,b = count+1, r+pointer8[i],g+pointer8[i+1],b+pointer8[i+2]  end
								end
								do -- down
									local x,y = x+offset,y+radius ; local i = (x>=0 and y>=0 and x<iw and y<ih) and (y*iw+x)*4 or nil ; local a = i and pointer8[i+3] or 0
									if a > 0 then  count, r,g,b = count+1, r+pointer8[i],g+pointer8[i+1],b+pointer8[i+2]  end
								end
								do -- left
									local x,y = x-radius,y+offset ; local i = (x>=0 and y>=0 and x<iw and y<ih) and (y*iw+x)*4 or nil ; local a = i and pointer8[i+3] or 0
									if a > 0 then  count, r,g,b = count+1, r+pointer8[i],g+pointer8[i+1],b+pointer8[i+2]  end
								end
								do -- right
									local x,y = x+radius,y-offset ; local i = (x>=0 and y>=0 and x<iw and y<ih) and (y*iw+x)*4 or nil ; local a = i and pointer8[i+3] or 0
									if a > 0 then  count, r,g,b = count+1, r+pointer8[i],g+pointer8[i+1],b+pointer8[i+2]  end
								end
							end

							if count > 0 then  break  end
						end--for offset
						if count > 0 then  break  end
					end--for radius

					if count > 0 then
						pointer8[i  ] = r / count
						pointer8[i+1] = g / count
						pointer8[i+2] = b / count
					end
				end--if transparent
			end--for x
		end--for y
	end--if anyTransparent

	--[[ DEBUG
	for i = 3, 4*iw*ih-1, 4 do
		pointer8[i] = 255
	end
	--]]

	imageData16:release()
	return imageData8
end

function love.keypressed(key)
	if key == "escape" then
		love.event.quit()

	elseif key == "space" then
		autoZoom = not autoZoom

	elseif key == "s" and love.keyboard.isDown("lctrl","rctrl") then
		if not theArt then  return  end

		if thePathIsTest then
			local pathOut = "output.png"
			print("Saving "..pathOut.."...")

			local imageData = fixImageDataForSaving(theArt.canvas:newImageData())
			imageData:encode("png", pathOut):release() ; imageData:release()

			print("Saving "..pathOut.."... done!")

		else
			local pathOut = (thePathOut ~= "") and thePathOut or thePathIn:gsub("%.[^.]+$", "")..".png"
			if pathOut == thePathIn then
				print("Error: Input and output paths are the same: "..pathOut)
				return
			end

			print("Saving "..pathOut.."...")

			local imageData = fixImageDataForSaving(theArt.canvas:newImageData())
			local fileData  = imageData:encode("png") ; imageData:release()
			local s         = fileData:getString()    ; fileData :release()
			local ok, err   = writeFile(pathOut, s, thePathIsTest)

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
	if love.system.getOS() == "Windows" then -- hotLoader works best on Windows.
		hotLoader.update(dt)

	else
		modtimeCheckTime = modtimeCheckTime - dt

		if modtimeCheckTime < 0 then
			modtimeCheckTime = 0.20

			local info    = love.filesystem.getInfo(thePathIn, "file")
			local modtime = info and info.modtime

			if modtime and modtime ~= theModtime then
				theModtime = modtime
				tryLoadingTheArtFile()
			end
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
		LG.draw(theArt.canvas, x,y)

	else
		local text = "No art loaded"
		local w    = LG.getFont():getWidth(text)
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


