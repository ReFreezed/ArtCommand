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
	drawQuad, drawPerspectiveCorrectQuad
	drawRectangleFill, drawRectangleLine
	newImageUsingPalette
	shaderSend*

--============================================================]]

local MAX_CIRCLE_SEGMENTS = 128



local function copyCommonVertexXyuv(vertices, fromI, toI)
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
	pcall(shader.shader.send, shader.shader, var, ...)
end
function _G.shaderSendVec2(shader, var, x,y, _)
	vec2[1] = x or error("Missing argument 'x'.", 2)
	vec2[2] = y or error("Missing argument 'y'.", 2)
	if _ then      error("Too many arguments."  , 2)  end
	pcall(shader.shader.send, shader.shader, var, vec2)
end
function _G.shaderSendVec3(shader, var, x,y,z, _)
	vec3[1] = x or error("Missing argument 'x'.", 2)
	vec3[2] = y or error("Missing argument 'y'.", 2)
	vec3[3] = z or error("Missing argument 'z'.", 2)
	if _ then      error("Too many arguments."  , 2)  end
	pcall(shader.shader.send, shader.shader, var, vec3)
end
function _G.shaderSendVec4(shader, var, x,y,z,w, _)
	vec4[1] = x or error("Missing argument 'x'.", 2)
	vec4[2] = y or error("Missing argument 'y'.", 2)
	vec4[3] = z or error("Missing argument 'z'.", 2)
	vec4[4] = w or error("Missing argument 'w'.", 2)
	if _ then      error("Too many arguments."  , 2)  end
	pcall(shader.shader.send, shader.shader, var, vec4)
end



local vertices = {}
local mesh     = nil

local function _drawLine(connected, coords, lw, circleMode, circleX,circleY, circleRx,circleRy)
	-- @Incomplete @Robustness: Handle overlapping coords.
	if not coords[connected and 6 or 4] then  return  end

	-- Prepare vertices.
	local coordCount      = .5 * #coords
	local additionalCount = connected and 2 or 0

	if not vertices[2*coordCount+additionalCount] then
		local allocationSize = math.max(#vertices, 2*16)

		while allocationSize < 2*coordCount+additionalCount do
			allocationSize = 2*allocationSize
		end

		for i = #vertices+1, allocationSize do
			table.insert(vertices, {0,0, 0,0})
		end

		if mesh then
			mesh:release()
			mesh = nil
		end
	end

	mesh = mesh or LG.newMesh({{"VertexPosition","float",2},{"VertexTexCoord","float",2}}, vertices, "strip", "stream")

	-- Make polygon line.
	local lwHalf = .5 * lw

	for coordI = 1, coordCount do
		local i     = 2 * coordI
		local prevI = 2 * ((coordI-2) % coordCount + 1)
		local nextI = 2 * ((coordI  ) % coordCount + 1)

		local x     = coords[i    -1]
		local y     = coords[i      ]
		local prevX = coords[prevI-1]
		local prevY = coords[prevI  ]
		local nextX = coords[nextI-1]
		local nextY = coords[nextI  ]

		local dir1X,dir1Y   = mathv.normalize(x-prevX,y-prevY)
		local dir2X,dir2Y   = mathv.normalize(nextX-x,nextY-y)
		local edge1X,edge1Y = mathv.scale(lwHalf,lwHalf, mathv.perpendicular(dir1X,dir1Y)) -- Offset from [x,y] to the edge.
		local edge2X,edge2Y = mathv.scale(lwHalf,lwHalf, mathv.perpendicular(dir2X,dir2Y))

		local x1,y1 = math.getLineLineIntersection(
			x+edge1X,y+edge1Y, x+edge1X+dir1X,y+edge1Y+dir1Y,
			x+edge2X,y+edge2Y, x+edge2X-dir2X,y+edge2Y-dir2Y
		)
		if not x1 then  x1,y1 = x+edge1X,y+edge1Y  end -- Straight line or 180° bend!  @Incomplete: Use bevel instead of miter in sharp corners. (Also, add argument: lineJoin=auto|miter|bevel|none)
		local x2 = 2*x - x1
		local y2 = 2*y - y1

		vertices[i-1][1],vertices[i-1][2] = x1,y1
		vertices[i  ][1],vertices[i  ][2] = x2,y2
	end

	-- Correct line edges if not polygon.
	if not connected then
		local lastI = 2 * coordCount

		local x11 = coords[1      ]
		local y11 = coords[2      ]
		local x12 = coords[3      ]
		local y12 = coords[4      ]
		local x21 = coords[lastI-3]
		local y21 = coords[lastI-2]
		local x22 = coords[lastI-1]
		local y22 = coords[lastI  ]

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
		local denomX = 2 * (circleRx + lw/2)
		local denomY = 2 * (circleRy + lw/2)

		for i = 1, 2*coordCount do
			vertices[i][3] = .5 + (vertices[i][1] - circleX) / denomX
			vertices[i][4] = .5 + (vertices[i][2] - circleY) / denomY
		end

	else
		local x1 =  1/0
		local x2 = -1/0
		local y1 =  1/0
		local y2 = -1/0

		for i = 1, 2*coordCount do
			x1 = math.min(x1, vertices[i][1])
			x2 = math.max(x2, vertices[i][1])
			y1 = math.min(y1, vertices[i][2])
			y2 = math.max(y2, vertices[i][2])
		end

		local w = x2 - x1
		local h = y2 - y1

		for i = 1, 2*coordCount do
			vertices[i][3] = (vertices[i][1] - x1) / w
			vertices[i][4] = (vertices[i][2] - y1) / h
		end
	end

	-- Draw mesh.
	if connected then
		copyCommonVertexXyuv(vertices, 1, 2*coordCount+1)
		copyCommonVertexXyuv(vertices, 2, 2*coordCount+2)
	end

	mesh:setVertices(vertices, 1, 2*coordCount+additionalCount)
	mesh:setDrawRange(         1, 2*coordCount+additionalCount)
	LG.draw(mesh)
end



function _G.drawLine(coords, lw)
	_drawLine(false, coords, lw, false, 0,0, 0,0)
end



local coords = {}

local function maybeAddCoords(x,y, lastX,lastY, offsetX,offsetY)
	x = x + offsetX
	y = y + offsetY

	if math.abs(x-lastX)+math.abs(y-lastY) > 1e-10 then
		table.insert(coords, x)
		table.insert(coords, y)
	end

	return x,y
end

local function drawRoundedRectangle(fill, x,y, w,h, tlx,tly, trx,try, brx,bry, blx,bly, segs, lw)
	table.clear(coords)

	local ratioTx = w / (tlx+trx)
	local ratioBx = w / (blx+brx)
	local ratioLy = h / (tly+bly)
	local ratioRy = h / (try+bry)
	if ratioTx < 1 then  tlx,trx = tlx*ratioTx,trx*ratioTx  end
	if ratioBx < 1 then  blx,brx = blx*ratioBx,brx*ratioBx  end
	if ratioLy < 1 then  tly,bly = tly*ratioLy,bly*ratioLy  end
	if ratioRy < 1 then  try,bry = try*ratioRy,bry*ratioRy  end

	segs              = math.max(segs, 1)
	local angleStep   = .25*TAU / segs
	local lastX,lastY = 1/0, 1/0

	if tlx > 0 and tly > 0 then
		for i = 0, segs do
			local angle = i*angleStep --+ .00*TAU
			lastX,lastY = maybeAddCoords(x,y, lastX,lastY,   tlx*(1-math.cos(angle)),  tly*(1-math.sin(angle)))
		end
	else
		lastX,lastY = maybeAddCoords(x,y, lastX,lastY, 0,0)
	end
	if trx > 0 and try > 0 then
		for i = 0, segs do
			local angle = i*angleStep + .25*TAU
			lastX,lastY = maybeAddCoords(x,y, lastX,lastY, w-trx*(1+math.cos(angle)),  try*(1-math.sin(angle)))
		end
	else
		lastX,lastY = maybeAddCoords(x,y, lastX,lastY, w,0)
	end
	if brx > 0 and bry > 0 then
		for i = 0, segs do
			local angle = i*angleStep + .50*TAU
			lastX,lastY = maybeAddCoords(x,y, lastX,lastY, w-brx*(1+math.cos(angle)),h-bry*(1+math.sin(angle)))
		end
	else
		lastX,lastY = maybeAddCoords(x,y, lastX,lastY, w,h)
	end
	if blx > 0 and bly > 0 then
		for i = 0, segs do
			local angle = i*angleStep + .75*TAU
			lastX,lastY = maybeAddCoords(x,y, lastX,lastY,   blx*(1-math.cos(angle)),h-bly*(1+math.sin(angle)))
		end
	else
		lastX,lastY = maybeAddCoords(x,y, lastX,lastY, 0,h)
	end

	if    fill
	then  drawPolygonFill(coords, false) -- @Speed: Might possibly wanna draw the mesh as a fan.
	else  drawPolygonLine(coords, lw)  end
end



function _G.drawRectangleFill(x,y, w,h, tlx,tly, trx,try, brx,bry, blx,bly, segs)
	if w < 0 then  x, w = x+w, -w  end
	if h < 0 then  y, h = y+h, -h  end

	if (tlx > 0 and tly > 0) or (trx > 0 and try > 0) or (brx > 0 and bry > 0) or (blx > 0 and bly > 0) then
		drawRoundedRectangle(true, x,y, w,h, tlx,tly, trx,try, brx,bry, blx,bly, segs, 0)
	else
		local iw,ih = A.images.rectangle:getDimensions()
		LG.draw(A.images.rectangle, x,y, 0, w/iw,h/ih)
	end
end



local mesh, vertices = nil, {
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
}

function _G.drawRectangleLine(x,y, w,h, tlx,tly, trx,try, brx,bry, blx,bly, segs, lw)
	if w < 0 then  x, w = x+w, -w  end
	if h < 0 then  y, h = y+h, -h  end

	if (tlx > 0 and tly > 0) or (trx > 0 and try > 0) or (brx > 0 and bry > 0) or (blx > 0 and bly > 0) then
		drawRoundedRectangle(false, x,y, w,h, tlx,tly, trx,try, brx,bry, blx,bly, segs, lw)
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
	mesh = mesh or LG.newMesh({{"VertexPosition","float",2},{"VertexTexCoord","float",2}}, vertices, "strip", "stream")

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
	table.insert(vertices, {0,0, 0,0})
end

function _G.drawCircleFill(x,y, rx,ry, angle1,angle2, closed, segs)
	mesh = mesh or LG.newMesh({{"VertexPosition","float",2},{"VertexTexCoord","float",2}}, vertices, "fan", "stream")

	segs            = math.clamp(segs, 3, MAX_CIRCLE_SEGMENTS)
	local angleStep = (angle2 - angle1) / segs

	for i = 2, segs+2 do
		local angle = angle1 + (i-2) * angleStep
		local c     = math.cos(angle)
		local s     = math.sin(angle)
		vertices[i][1],vertices[i][2], vertices[i][3],vertices[i][4] = rx*c,ry*s, .5+.5*c,.5+.5*s
	end

	if    closed
	then  copyCommonVertexXyuv(vertices, 2, 1)
	else  vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4] = 0,0, .5,.5  end

	mesh:setVertices(vertices, 1, segs+2)
	mesh:setDrawRange(         1, segs+2)
	LG.draw(mesh, x,y)
end



local coords = {}

-- drawCircleLine( x,y, rx,ry, angle1,angle2, arcMode, segs, lineWidth )
-- arcMode = "open" | "pie" | "closed"
function _G.drawCircleLine(x,y, rx,ry, angle1,angle2, arcMode, segs, lw)
	local connected = arcMode ~= "open" or angle2 > angle1+(TAU-1e-10) or angle2 < angle1-(TAU-1e-10)
	segs            = math.clamp(segs, 3, MAX_CIRCLE_SEGMENTS)
	local angleStep = (angle2 - angle1) / segs

	table.clear(coords)

	if arcMode == "pie" then
		table.insert(coords, x)
		table.insert(coords, y)
	end

	for i = 0, segs + (connected and arcMode == "open" and -1 or 0) do
		local angle = angle1 + i*angleStep
		table.insert(coords, x+rx*math.cos(angle))
		table.insert(coords, y+ry*math.sin(angle))
	end

	_drawLine(connected, coords, lw, true, x,y, rx,ry)
end



local vertices = {}
local mesh     = nil

function _G.drawPolygonFill(coords, asStrip)
	local isConvex  = asStrip or love.math.isConvex(coords)
	local triangles = nil

	if not isConvex then
		local ok;ok, triangles = pcall(love.math.triangulate, coords)

		if not ok then
			local points = {}
			for i = 1, #coords, 2 do
				table.insert(points, {x=coords[i], y=coords[i+1]})
			end
			triangles = require"triangulate"(points)

			for _, tri in ipairs(triangles) do
				tri[1],tri[2],     tri[3],tri[4],     tri[5],tri[6] =
				tri[1].x,tri[1].y, tri[2].x,tri[2].y, tri[3].x,tri[3].y
			end

			--[[
			if DEV and 1==0 then
				print("Error: Failed triangulating polygon. ("..table.concat(coords, ", ")..")")
			else
				print("Error: Failed triangulating polygon.") -- @UX: Alert the user.
			end
			isConvex = true -- Fallback. Visual errors may appear.
			--]]
		end
	end

	local vertCount = isConvex and .5*#coords or 3*#triangles

	if not vertices[vertCount] then
		local allocationSize = math.max(#vertices, 16)

		while allocationSize < vertCount do
			allocationSize = 2*allocationSize
		end

		for i = #vertices+1, allocationSize do
			table.insert(vertices, {0,0, 0,0})
		end

		if mesh then
			mesh:release()
			mesh = nil
		end
	end

	mesh = mesh or LG.newMesh({{"VertexPosition","float",2},{"VertexTexCoord","float",2}}, vertices, "fan", "stream")

	local x1 =  1/0
	local x2 = -1/0
	local y1 =  1/0
	local y2 = -1/0

	for i = 1, #coords, 2 do
		x1 = math.min(x1, coords[i  ])
		x2 = math.max(x2, coords[i  ])
		y1 = math.min(y1, coords[i+1])
		y2 = math.max(y2, coords[i+1])
	end

	local w = x2 - x1
	local h = y2 - y1

	if isConvex then
		mesh:setDrawMode(asStrip and "strip" or "fan")

		for vertI = 1, vertCount do
			-- @Incomplete @Robustness: Handle overlapping coords.
			local x = coords[2*vertI-1]
			local y = coords[2*vertI  ]

			vertices[vertI][1],vertices[vertI][2], vertices[vertI][3],vertices[vertI][4] = x,y, (x-x1)/w,(y-y1)/h
		end

	else
		mesh:setDrawMode("triangles")
		local vertI = 0

		for _, tri in ipairs(triangles) do
			for i = 1, 5, 2 do
				-- @Incomplete @Robustness: Handle overlapping coords.
				vertI   = vertI + 1
				local x = tri[i  ]
				local y = tri[i+1]

				vertices[vertI][1],vertices[vertI][2], vertices[vertI][3],vertices[vertI][4] = x,y, (x-x1)/w,(y-y1)/h
			end
		end
	end

	mesh:setVertices(vertices, 1, vertCount)
	mesh:setDrawRange(         1, vertCount)
	LG.draw(mesh)
end



function _G.drawPolygonLine(coords, lw)
	_drawLine(true, coords, lw, false, 0,0, 0,0)
end



local vertices = {
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
	{0,0, 0,0},
}
local mesh = nil

function _G.drawQuad(texture, x1,y1,x2,y2,x3,y3,x4,y4, u1,v1,u2,v2,u3,v3,u4,v4)
	mesh = mesh or LG.newMesh({{"VertexPosition","float",2},{"VertexTexCoord","float",2}}, vertices, "fan", "stream")

	vertices[1][1],vertices[1][2], vertices[1][3],vertices[1][4] = x1,y1, u1,v1
	vertices[2][1],vertices[2][2], vertices[2][3],vertices[2][4] = x2,y2, u2,v2
	vertices[3][1],vertices[3][2], vertices[3][3],vertices[3][4] = x3,y3, u3,v3
	vertices[4][1],vertices[4][2], vertices[4][3],vertices[4][4] = x4,y4, u4,v4

	mesh:setTexture(texture)
	mesh:setVertices(vertices)
	LG.draw(mesh)
end



local vertices = {
	{0,0,0, 0,0},
	{0,0,0, 0,0},
	{0,0,0, 0,0},
	{0,0,0, 0,0},
}
local mesh = nil

-- z = calculateZ( point1, point2, vanishingPoint )
local function calculateZ(x1,y1, x2,y2, vapoX,vapoY)
	local dx1    = x1 - vapoX
	local dy1    = y1 - vapoY
	local dx2    = x2 - vapoX
	local dy2    = y2 - vapoY
	local len2Sq = dx2*dx2 + dy2*dy2
	return math.sqrt((dx1*dx1 + dy1*dy1) * len2Sq) / len2Sq -- Thanks, quickmath.com!
	-- return math.sqrt(dx1*dx1+dy1*dy1) / math.sqrt(dx2*dx2+dy2*dy2)
end

-- Note: Current shader ought to be A.shaders.quad!
function _G.drawPerspectiveCorrectQuad(texture, x1,y1,x2,y2,x3,y3,x4,y4, u1,v1,u2,v2,u3,v3,u4,v4)
	mesh = mesh or LG.newMesh({{"VertexPosition","float",3},{"VertexTexCoord","float",2}}, vertices, "fan", "stream")

	local z1 = 1 -- Reference corner - is always 1.
	local z2 = 1
	local z3 = 1
	local z4 = 1

	local vapo23And41X,vapo23And41Y = math.getLineLineIntersection(x2,y2,x3,y3, x4,y4,x1,y1)
	local vapo12And34X,vapo12And34Y = math.getLineLineIntersection(x1,y1,x2,y2, x3,y3,x4,y4)

	if vapo23And41X then  z4 = calculateZ(x1,y1, x4,y4, vapo23And41X,vapo23And41Y)  end
	if vapo12And34X then  z2 = calculateZ(x1,y1, x2,y2, vapo12And34X,vapo12And34Y)  end

	if vapo23And41X and vapo12And34X then
		local vapo13X,vapo13Y = math.getLineLineIntersection(x1,y1,x3,y3, vapo23And41X,vapo23And41Y,vapo12And34X,vapo12And34Y)
		z3 = not vapo13X and 1 or
		     calculateZ(x1,y1, x3,y3, vapo13X,vapo13Y)
	elseif vapo23And41X then
		z3 = calculateZ(x2,y2, x3,y3, vapo23And41X,vapo23And41Y)
	elseif vapo12And34X then
		z3 = calculateZ(x4,y4, x3,y3, vapo12And34X,vapo12And34Y)
	end

	vertices[1][1],vertices[1][2],vertices[1][3], vertices[1][4],vertices[1][5] = x1,y1,1/z1, 0,0
	vertices[2][1],vertices[2][2],vertices[2][3], vertices[2][4],vertices[2][5] = x2,y2,1/z2, 1/z2,0
	vertices[3][1],vertices[3][2],vertices[3][3], vertices[3][4],vertices[3][5] = x3,y3,1/z3, 1/z3,1/z3
	vertices[4][1],vertices[4][2],vertices[4][3], vertices[4][4],vertices[4][5] = x4,y4,1/z4, 0,1/z4

	shaderSendVec4(A.shaders.quad, "quadX", u1,u2,u3,u4)
	shaderSendVec4(A.shaders.quad, "quadY", v1,v2,v3,v4)

	mesh:setTexture(texture)
	mesh:setVertices(vertices)
	LG.draw(mesh)
end


