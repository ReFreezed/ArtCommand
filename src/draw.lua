--[[============================================================
--=
--=  Drawing and image functions
--=
--=-------------------------------------------------------------
--=
--=  Art Command
--=  by Marcus 'ReFreezed' Thunström
--=
--==============================================================

	drawCircleFill, drawCircleLine
	drawRectangleFill, drawRectangleLine
	newImageUsingPalette
	shaderSend*

--============================================================]]

local MAX_CIRCLE_SEGMENTS = 128



function _G.newImageUsingPalette(pixelRows, palette)
	local imageData = love.image.newImageData(#pixelRows[1], #pixelRows)

	for row, pixelRow in ipairs(pixelRows) do
		for col = 1, #pixelRow do
			local k       = pixelRow:sub(col, col)
			local pixel   = palette[k] or error("No color for '"..k.."'.")
			local r,g,b,a = unpack(pixel)
			imageData:setPixel(col-1,row-1, r,g,b,a) -- @Speed
		end
	end

	return LG.newImage(imageData)
end



local vec2 = {0,0}
local vec3 = {0,0,0}
local vec4 = {0,0,0,0}

function _G.shaderSend(shader, var, ...)
	pcall(shader.send, shader, var, ...)
end
function _G.shaderSendVec2(shader, var, x,y)
	vec2[1], vec2[2] = x, y
	pcall(shader.send, shader, var, vec2)
end
function _G.shaderSendVec3(shader, var, x,y,z)
	vec3[1], vec3[2], vec3[3] = x, y, z
	pcall(shader.send, shader, var, vec3)
end
function _G.shaderSendVec4(shader, var, x,y,z,w)
	vec4[1], vec4[2], vec4[3], vec4[4] = x, y, z, w
	pcall(shader.send, shader, var, vec4)
end



function _G.drawRectangleFill(x,y, w,h)
	local iw,ih = A.images.rectangle:getDimensions()
	LG.draw(A.images.rectangle, x,y, 0, w/iw,h/ih)
end



local mesh, vertices = nil, {
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
	{0,0, 0,0, 1,1,1,1},
}

function _G.drawRectangleLine(x,y, w,h, lw)
	--
	--   1 2 3 4
	-- 1 2-----4  10=2
	--   |\\  /|
	-- 2 | 1-3/|  9=1
	--   | | | |
	-- 3 |/7-5 |
	--   |/  \\|
	-- 4 8-----6
	--
	if not mesh then
		mesh = LG.newMesh(vertices, "strip", "stream")
		mesh:setTexture(A.images.rectangle)
	end

	local x1 =   - lw*.5
	local x2 =     lw*.5
	local x3 = w - lw*.5
	local x4 = w + lw*.5
	local y1 =   - lw*.5
	local y2 =     lw*.5
	local y3 = h - lw*.5
	local y4 = h + lw*.5

	local lwU = lw / (x4-x1)
	local lwV = lw / (y4-y1)

	-- Outer.
	vertices[2][1],vertices[2][2], vertices[2][3],vertices[2][4] = x1,y1, 0,0
	vertices[4][1],vertices[4][2], vertices[4][3],vertices[4][4] = x4,y1, 1,0
	vertices[6][1],vertices[6][2], vertices[6][3],vertices[6][4] = x4,y4, 1,1
	vertices[8][1],vertices[8][2], vertices[8][3],vertices[8][4] = x1,y4, 0,1

	-- Inner.
	vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4] = x2,y2, lwU,lwV
	vertices[3][1],vertices[3][2], vertices[3][3],vertices[3][4] = x3,y2, 1-lwU,lwV
	vertices[5][1],vertices[5][2], vertices[5][3],vertices[5][4] = x3,y3, 1-lwU,1-lwV
	vertices[7][1],vertices[7][2], vertices[7][3],vertices[7][4] = x2,y3, lwU,1-lwV

	vertices[ 9][1],vertices[ 9][2], vertices[ 9][3],vertices[ 9][4] = vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4]
	vertices[10][1],vertices[10][2], vertices[10][3],vertices[10][4] = vertices[2][1],vertices[2][2], vertices[2][3],vertices[2][4]
	mesh:setVertices(vertices)
	LG.draw(mesh, x,y)
end



local vertices = {{0,0, .5,.5, 1,1,1,1}}
local mesh     = nil

for i = 2, MAX_CIRCLE_SEGMENTS+2 do
	table.insert(vertices, {0,0, 0,0, 1,1,1,1})
end

function _G.drawCircleFill(x,y, rx,ry, segs)
	if not mesh then
		mesh = LG.newMesh(vertices, "fan", "stream")
		mesh:setTexture(A.images.rectangle)
	end

	segs             = clamp(segs, 3, MAX_CIRCLE_SEGMENTS)
	local deltaAngle = TAU / segs

	for i = 2, segs+2 do
		local angle = i * deltaAngle
		local c     = math.cos(angle)
		local s     = math.sin(angle)
		vertices[i][1],vertices[i][2], vertices[i][3],vertices[i][4] = rx*c,ry*s, .5+.5*c,.5+.5*s
	end

	mesh:setVertices(vertices, 1, segs+2)
	mesh:setDrawRange(         1, segs+2)
	LG.draw(mesh, x,y)
end



local vertices = {}
local mesh     = nil

for i = 1, 2*MAX_CIRCLE_SEGMENTS+2 do
	table.insert(vertices, {0,0, 0,0, 1,1,1,1})
end

function _G.drawCircleLine(x,y, rx,ry, segs, lw)
	if not mesh then
		mesh = LG.newMesh(vertices, "strip", "stream")
		mesh:setTexture(A.images.rectangle)
	end

	-- @Polish: Calculate final line thickness better for low segment counts (and sharper angles)
	-- as the resulting lines get thinner the way we do things here.
	segs             = clamp(segs, 3, MAX_CIRCLE_SEGMENTS)
	local rxInner    = rx - lw*.5
	local ryInner    = ry - lw*.5
	local rxOuter    = rx + lw*.5
	local ryOuter    = ry + lw*.5
	local deltaAngle = TAU / segs
	local ruInner    = (1 - lw/rxOuter) / 2
	local rvInner    = (1 - lw/ryOuter) / 2

	for i = 1, segs do
		local angle = i * deltaAngle
		local c     = math.cos(angle)
		local s     = math.sin(angle)
		vertices[2*i-1][1],vertices[2*i-1][2], vertices[2*i-1][3],vertices[2*i-1][4] = rxInner*c,ryInner*s, .5+.5*c,.5+.5*s
		vertices[2*i  ][1],vertices[2*i  ][2], vertices[2*i  ][3],vertices[2*i  ][4] = rxOuter*c,ryOuter*s, .5+ruInner*c,.5+rvInner*s
	end

	vertices[2*segs+1][1],vertices[2*segs+1][2], vertices[2*segs+1][3],vertices[2*segs+1][4] = vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4]
	vertices[2*segs+2][1],vertices[2*segs+2][2], vertices[2*segs+2][3],vertices[2*segs+2][4] = vertices[2][1],vertices[2][2], vertices[2][3],vertices[2][4]

	mesh:setVertices(vertices, 1, 2*segs+2)
	mesh:setDrawRange(         1, 2*segs+2)
	LG.draw(mesh, x,y)
end


