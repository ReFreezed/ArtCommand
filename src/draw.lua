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
	drawLine
	drawPolygonFill, drawPolygonLine
	drawRectangleFill, drawRectangleLine
	newImageUsingPalette
	shaderSend*

--============================================================]]

local MAX_CIRCLE_SEGMENTS = 128



local function copyVertexXyuv(vertices, fromI, toI)
	vertices[toI][1] = vertices[fromI][1]
	vertices[toI][2] = vertices[fromI][2]
	vertices[toI][3] = vertices[fromI][3]
	vertices[toI][4] = vertices[fromI][4]
end



-- Note: The image has premultiplied alpha.
function _G.newImageUsingPalette(pixelRows, palette)
	local imageData = love.image.newImageData(#pixelRows[1], #pixelRows)

	for row, pixelRow in ipairs(pixelRows) do
		for col = 1, #pixelRow do
			local k       = pixelRow:sub(col, col)
			local pixel   = palette[k] or error("No color for '"..k.."'.")
			local r,g,b,a = unpack(pixel)
			imageData:setPixel(col-1,row-1, r*a,g*a,b*a,a) -- @Speed
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
function _G.shaderSendVec2(shader, var, x,y, _)
	vec2[1] = x or error("Missing argument 'x'.", 2)
	vec2[2] = y or error("Missing argument 'y'.", 2)
	if _ then      error("Too many arguments."  , 2)  end
	pcall(shader.send, shader, var, vec2)
end
function _G.shaderSendVec3(shader, var, x,y,z, _)
	vec3[1] = x or error("Missing argument 'x'.", 2)
	vec3[2] = y or error("Missing argument 'y'.", 2)
	vec3[3] = z or error("Missing argument 'z'.", 2)
	if _ then      error("Too many arguments."  , 2)  end
	pcall(shader.send, shader, var, vec3)
end
function _G.shaderSendVec4(shader, var, x,y,z,w, _)
	vec4[1] = x or error("Missing argument 'x'.", 2)
	vec4[2] = y or error("Missing argument 'y'.", 2)
	vec4[3] = z or error("Missing argument 'z'.", 2)
	vec4[4] = w or error("Missing argument 'w'.", 2)
	if _ then      error("Too many arguments."  , 2)  end
	pcall(shader.send, shader, var, vec4)
end



local vertices = {}
local mesh     = nil

local function _drawLine(connected, points, lw, circleMode, circleX,circleY)
	if not points[connected and 6 or 4] then  return  end

	-- Prepare vertices.
	local pointCount      = .5 * #points
	local additionalCount = connected and 2 or 0

	if not vertices[2*pointCount+additionalCount] then
		local allocationSize = math.max(#vertices, 2*16)

		while allocationSize < 2*pointCount+additionalCount do
			allocationSize = 2*allocationSize
		end

		for i = #vertices+1, allocationSize do
			table.insert(vertices, {0,0, 0,0, 1,1,1,1})
		end

		if mesh then
			mesh:release()
			mesh = nil
		end
	end

	mesh = mesh or LG.newMesh(vertices, "strip", "stream")

	-- Make polygon line.
	local lwHalf = .5 * lw

	for pointI = 1, pointCount do
		local i     = 2 * pointI
		local prevI = 2 * ((pointI-2) % pointCount + 1)
		local nextI = 2 * ((pointI  ) % pointCount + 1)

		local x     = points[i    -1]
		local y     = points[i      ]
		local prevX = points[prevI-1]
		local prevY = points[prevI  ]
		local nextX = points[nextI-1]
		local nextY = points[nextI  ]

		local dir1X,dir1Y   = mathv.normalize(x-prevX,y-prevY)
		local dir2X,dir2Y   = mathv.normalize(nextX-x,nextY-y)
		local edge1X,edge1Y = mathv.scale(lwHalf,lwHalf, mathv.perpendicular(dir1X,dir1Y)) -- Offset from [x,y] to the edge.
		local edge2X,edge2Y = mathv.scale(lwHalf,lwHalf, mathv.perpendicular(dir2X,dir2Y))

		local x1,y1 = math.getLineLineIntersection(
			x+edge1X,y+edge1Y, x+edge1X+dir1X,y+edge1Y+dir1Y,
			x+edge2X,y+edge2Y, x+edge2X-dir2X,y+edge2Y-dir2Y
		)
		if not x1 then  x1,y1 = x,y  end -- Parallel lines!  @Incomplete: Use bevel instead of miter in sharp corners. (Also, add argument: lineJoin=auto|miter|bevel|none)
		local x2 = 2*x - x1
		local y2 = 2*y - y1

		vertices[i-1][1],vertices[i-1][2] = x1,y1
		vertices[i  ][1],vertices[i  ][2] = x2,y2
	end

	-- Correct line edges if not polygon.
	if not connected then
		local lastI = 2 * pointCount

		local x11 = points[1      ]
		local y11 = points[2      ]
		local x12 = points[3      ]
		local y12 = points[4      ]
		local x21 = points[lastI-3]
		local y21 = points[lastI-2]
		local x22 = points[lastI-1]
		local y22 = points[lastI  ]

		local dir1X,dir1Y   = mathv.normalize(x12-x11,y12-y11)
		local dir2X,dir2Y   = mathv.normalize(x22-x21,y22-y21)
		local edge1X,edge1Y = mathv.scale(lwHalf,lwHalf, mathv.perpendicular(dir1X,dir1Y)) -- Offset from [x,y] to the edge.
		local edge2X,edge2Y = mathv.scale(lwHalf,lwHalf, mathv.perpendicular(dir2X,dir2Y))

		vertices[1      ][1], vertices[1      ][2] = x11+edge1X, y11+edge1Y
		vertices[2      ][1], vertices[2      ][2] = x11-edge1X, y11-edge1Y
		vertices[lastI-1][1], vertices[lastI-1][2] = x22+edge2X, y22+edge2Y
		vertices[lastI  ][1], vertices[lastI  ][2] = x22-edge2X, y22-edge2Y
	end

	-- Set UVs.
	if circleMode then
		local maxDistSq = 0

		for i = 1, 2*pointCount do
			local distSq = math.distanceSq(circleX,circleY, vertices[i][1],vertices[i][2])
			maxDistSq    = math.max(maxDistSq, distSq)
		end

		local maxDistDouble = 2 * math.sqrt(maxDistSq)

		for i = 1, 2*pointCount do
			vertices[i][3] = .5 + (vertices[i][1] - circleX) / maxDistDouble
			vertices[i][4] = .5 + (vertices[i][2] - circleY) / maxDistDouble
		end

	else
		local x1 =  1/0
		local x2 = -1/0
		local y1 =  1/0
		local y2 = -1/0

		for i = 1, 2*pointCount do
			x1 = math.min(x1, vertices[i][1])
			x2 = math.max(x2, vertices[i][1])
			y1 = math.min(y1, vertices[i][2])
			y2 = math.max(y2, vertices[i][2])
		end

		local w = x2 - x1
		local h = y2 - y1

		for i = 1, 2*pointCount do
			vertices[i][3] = (vertices[i][1] - x1) / w
			vertices[i][4] = (vertices[i][2] - y1) / h
		end
	end

	-- Draw mesh.
	if connected then
		copyVertexXyuv(vertices, 1, 2*pointCount+1)
		copyVertexXyuv(vertices, 2, 2*pointCount+2)
	end

	mesh:setVertices(vertices, 1, 2*pointCount+additionalCount)
	mesh:setDrawRange(         1, 2*pointCount+additionalCount)
	LG.draw(mesh)
end



function _G.drawLine(points, lw)
	_drawLine(false, points, lw, false, 0,0)
end



local corner = {}
local points = {}

local function drawRoundedRectangle(fill, x,y, w,h, rx,ry, segs, lw)
	table.clear(corner)
	table.clear(points)

	rx = math.min(rx, w/2)
	ry = math.min(ry, h/2)

	segs            = math.max(segs, 1)
	local angleStep = .25*TAU / segs

	for i = 0, segs do
		local angle = i*angleStep
		table.insert(corner, rx*(1-math.cos(angle)))
		table.insert(corner, ry*(1-math.sin(angle)))
	end

	local cornerCount = #corner
	local i1x         = 2*rx < w and 0 or 1
	local i1y         = 2*ry < h and 0 or 1

	for i = i1y, segs do -- tl
		table.insert(points, x+  corner[2*i+1])
		table.insert(points, y+  corner[2*i+2])
	end
	for i = i1x, segs do -- tr
		table.insert(points, x+w-corner[cornerCount-2*i-1])
		table.insert(points, y+  corner[cornerCount-2*i  ])
	end
	for i = i1y, segs do -- br
		table.insert(points, x+w-corner[2*i+1])
		table.insert(points, y+h-corner[2*i+2])
	end
	for i = i1x, segs do -- bl
		table.insert(points, x+  corner[cornerCount-2*i-1])
		table.insert(points, y+h-corner[cornerCount-2*i  ])
	end

	if    fill
	then  drawPolygonFill(points) -- @Speed: Might possibly wanna draw the mesh as a fan.
	else  drawPolygonLine(points, lw)  end
end



function _G.drawRectangleFill(x,y, w,h, rx,ry, segs)
	if w < 0 then  x, w = x+w, -w  end
	if h < 0 then  y, h = y+h, -h  end

	if rx > 0 and ry > 0 then
		drawRoundedRectangle(true, x,y, w,h, rx,ry, segs, 0)

	else
		local iw,ih = A.images.rectangle:getDimensions()
		LG.draw(A.images.rectangle, x,y, 0, w/iw,h/ih)
	end
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

function _G.drawRectangleLine(x,y, w,h, rx,ry, segs, lw)
	if w < 0 then  x, w = x+w, -w  end
	if h < 0 then  y, h = y+h, -h  end

	if rx > 0 and ry > 0 then
		drawRoundedRectangle(false, x,y, w,h, rx,ry, segs, lw)
		return
	end

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
	mesh = mesh or LG.newMesh(vertices, "strip", "stream")

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



local vertices = {}
local mesh     = nil

for i = 1, MAX_CIRCLE_SEGMENTS+2 do
	table.insert(vertices, {0,0, 0,0, 1,1,1,1})
end

function _G.drawCircleFill(x,y, rx,ry, angle1,angle2, closed, segs)
	mesh = mesh or LG.newMesh(vertices, "fan", "stream")

	segs            = math.clamp(segs, 3, MAX_CIRCLE_SEGMENTS)
	local angleStep = (angle2 - angle1) / segs

	for i = 2, segs+2 do
		local angle = angle1 + (i-2) * angleStep
		local c     = math.cos(angle)
		local s     = math.sin(angle)
		vertices[i][1],vertices[i][2], vertices[i][3],vertices[i][4] = rx*c,ry*s, .5+.5*c,.5+.5*s
	end

	if    closed
	then  copyVertexXyuv(vertices, 2, 1)
	else  vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4] = 0,0, .5,.5  end

	mesh:setVertices(vertices, 1, segs+2)
	mesh:setDrawRange(         1, segs+2)
	LG.draw(mesh, x,y)
end



local vertices = {}
local mesh     = nil

for i = 1, 2*MAX_CIRCLE_SEGMENTS+2 do
	table.insert(vertices, {0,0, 0,0, 1,1,1,1})
end

local points = {}

-- drawCircleLine( x,y, rx,ry, angle1,angle2, arcMode, segs, lineWidth )
-- arcMode = "open" | "pie" | "closed"
function _G.drawCircleLine(x,y, rx,ry, angle1,angle2, arcMode, segs, lw)
	local connected = arcMode ~= "open" or angle2 > angle1+(TAU-1e-10) or angle2 < angle1-(TAU-1e-10)
	segs            = math.clamp(segs, 3, MAX_CIRCLE_SEGMENTS)
	local angleStep = (angle2 - angle1) / segs

	table.clear(points)

	if arcMode == "pie" then
		table.insert(points, x)
		table.insert(points, y)
	end

	for i = 0, segs + (connected and arcMode == "open" and -1 or 0) do
		local angle = angle1 + i*angleStep
		table.insert(points, x+rx*math.cos(angle))
		table.insert(points, y+ry*math.sin(angle))
	end

	_drawLine(connected, points, lw, true, x,y)
end



local vertices = {}
local mesh     = nil

function _G.drawPolygonFill(points)
	local pointCount = .5 * #points

	if not vertices[pointCount] then
		local allocationSize = math.max(#vertices, 16)

		while allocationSize < pointCount do
			allocationSize = 2*allocationSize
		end

		for i = #vertices+1, allocationSize do
			table.insert(vertices, {0,0, 0,0, 1,1,1,1})
		end

		if mesh then
			mesh:release()
			mesh = nil
		end
	end

	mesh = mesh or LG.newMesh(vertices, "fan", "stream")

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

	local w = x2 - x1
	local h = y2 - y1

	for i = 1, pointCount do
		local x = points[2*i-1]
		local y = points[2*i  ]

		vertices[i][1],vertices[i][2], vertices[i][3],vertices[i][4] = x,y, (x-x1)/w,(y-y1)/h
	end

	mesh:setVertices(vertices, 1, pointCount)
	mesh:setDrawRange(         1, pointCount)
	LG.draw(mesh)
end



function _G.drawPolygonLine(points, lw)
	_drawLine(true, points, lw, false, 0,0)
end


