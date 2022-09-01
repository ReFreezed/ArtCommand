--[[============================================================
--=
--=  Lua math extension module
--=  - Written by Marcus 'ReFreezed' Thunström
--=
--============================================================]]



local math = {}
for k, v in pairs(_G.math) do  math[k] = v  end

local pi  = math.pi
local tau = 2*pi

math.nan = 0/0
math.tau = tau

local mathv  = {}
local math3d = {}
math.v     = mathv
math.three = math3d



local abs    = math.abs
local atan2  = math.atan2
local cos    = math.cos
local deg    = math.deg
local exp    = math.exp
local floor  = math.floor
local max    = math.max
local min    = math.min
local random = math.random
local sin    = math.sin
local sqrt   = math.sqrt

local aabbCornerFarthestFromPoint
local avarage
local boundingBox
local clamp, clamp01, clamp11, clampInt
local cos01, sin01
local distance, distanceSq, taxicabDistance, distanceFromPointToLine, distanceFromPointToLineSq
local integral, fraction
local isFinite, isInt
local lerpExact, lerp, lerp2, lerp3, lerp4, lerpColors, lerp01, lerpInt, tween, normalize, remap
local moveTowards, damp
local normalizeAngle, deltaAngle, angleDistance
local pathfindAstar
local pointOnLineClosestToPoint, pointOnSegmentClosestToPoint
local powKeepSign, squareKeepSign
local randomf, chance, randomCircle, randomSign, around
local raytraceGrid, floodGrid, floodGridWithDiagonals, floodGridFast, flood
local round, roundEven
local sign, sign0
local sum

-- Vectors (math.v.*)
--    format
--    abs
--    add, sub, mul, div, eq, lt, le
--    angle, angleDeg
--    clamp
--    distance, distanceSq
--    dot, cross
--    epsilon
--    floor, ceil, round
--    len, lenSq
--    min, max
--    normalize
--    perpendicular
--    projectOn
--    rotate, rotateCw, rotateCcw, rotateQuarters
--    scale
--    swap
--    translate, translateTowards, moveTowards, damp, lerpExact, lerp
--    withMinLength, withMaxLength

-- Intersections
local isAabbIntersectingAabb
local isCircleIntersectingAabb
local isCircleIntersectingCircle
local isCircleIntersectingRectangle
local isLineIntersectingAabb,          getLineAabbIntersections
local isLineIntersectingCircle,        getLineCircleIntersections
local isLineIntersectingLine,          getLineLineIntersection
local isLineIntersectingSegment,       getLineSegmentIntersection
local isLineIntersectingRectangle,     getLineRectangleIntersections
local isLineIntersectingTriangle,      getLineTriangleIntersections
local isSegmentIntersectingAabb,       getSegmentAabbIntersections
local isSegmentIntersectingCircle,     getSegmentCircleIntersections
local isSegmentIntersectingSegment,    getSegmentSegmentIntersection
local isSegmentIntersectingRectangle,  getSegmentRectangleIntersections
local isSegmentIntersectingTriangle,   getSegmentTriangleIntersections
local isSegmentIntersectingPolygon,    getSegmentPolygonIntersections
local isPointIntersectingAabb
local isPointIntersectingCircle
local isPointIntersectingLine
local isPointIntersectingRectangle
local isPointIntersectingTriangle
local isPointIntersectingPolygon
local isRectangleIntersectingAabb
local isRectangleIntersectingRectangle

-- Interactions
local bounce

-- 3D (math.three.*)
--    distance, distanceSq, taxicabDistance
--    cross



--==============================================================
--==============================================================
--==============================================================

local getPolygonSegment
local getRectCorners, getRectNormals
local getSideOfLine
local reverseTable

local function NOT_IMPLEMENTED(fName)
	return function() error(fName.."() is not implemented yet!", 2) end
end



-- corners = getRectCorners( rectCenter, rectSize, rectAngle [, corners={} ] )
-- corners = { x1,y1, x2,y2, x3,y3, x4,y4 }
function getRectCorners(x,y, w,h, angle, corners)
	local wHalf = w*0.5
	local hHalf = h*0.5

	local ax, ay = mathv.add(x, y, mathv.rotate( wHalf, -hHalf, angle))
	local bx, by = mathv.add(x, y, mathv.rotate( wHalf,  hHalf, angle))
	local cx, cy = mathv.add(x, y, mathv.rotate(-wHalf,  hHalf, angle))
	local dx, dy = mathv.add(x, y, mathv.rotate(-wHalf, -hHalf, angle))

	corners = corners or {}
	corners[1], corners[2] = ax, ay
	corners[3], corners[4] = bx, by
	corners[5], corners[6] = cx, cy
	corners[7], corners[8] = dx, dy
	return corners
end

-- normals = getRectNormals( rectAngle [, normals={} ] )
-- normals = { x1,y1, x2,y2, x3,y3, x4,y4 }
function getRectNormals(angle, normals)
	local ax,ay = mathv.rotate(1,0, angle)
	local bx,by = mathv.rotate(ax,ay, 1*tau/4)
	local cx,cy = mathv.rotate(ax,ay, 2*tau/4)
	local dx,dy = mathv.rotate(ax,ay, 3*tau/4)

	normals = normals or {}
	normals[1], normals[2] = ax, ay
	normals[3], normals[4] = bx, by
	normals[5], normals[6] = cx, cy
	normals[7], normals[8] = dx, dy
	return normals
end



-- sign = getSideOfLine( linePoint1, linePoint2, pointOfInterest )
-- sign is >0 if pointOfInterest is in the clockwise direction relative to the line vector.
function getSideOfLine(lineX1,lineY1, lineX2,lineY2, px,py)
	return sign0( (lineX2-lineX1)*(py-lineY1) - (px-lineX1)*(lineY2-lineY1) )
end



function reverseTable(t)
	local len = #t
	local ir  = len

	for il = 1, len*0.5 do
		t[il], t[ir] = t[ir], t[il]
		ir = ir-1
	end

	return t
end



function getPolygonSegment(i, argCount, ...)
	return
		(select((i  ),            ...)),
		(select((i+1),            ...)),
		(select((i+1)%argCount+1, ...)),
		(select((i+1)%argCount+2, ...))
end



--==============================================================
--==============================================================
--==============================================================



-- number = avarage( number1, number2... )
function avarage(...)
	return sum(...)/select("#", ...)
end
math.avarage = avarage



-- xMin, xMax, yMin, yMax = boundingBox( x, y, width, height, rotation, anchorX, anchorY )
function boundingBox(x, y, w, h, rotation, ax, ay)
	local c = cos(rotation)
	local s = sin(rotation)

	local p1x, p1y = -w*ax, -h*ay
	local p2x, p2y = p1x+w, p1y
	local p3x, p3y = p1x+w, p1y+h
	local p4x, p4y = p1x,   p1y+h

	p1x, p1y = c*p1x-s*p1y, s*p1x+c*p1y
	p2x, p2y = c*p2x-s*p2y, s*p2x+c*p2y
	p3x, p3y = c*p3x-s*p3y, s*p3x+c*p3y
	p4x, p4y = c*p4x-s*p4y, s*p4x+c*p4y

	return
		x+min(p1x, p2x, p3x, p4x),
		x+max(p1x, p2x, p3x, p4x),
		y+min(p1y, p2y, p3y, p4y),
		y+max(p1y, p2y, p3y, p4y)
end
math.boundingBox = boundingBox



-- number = clamp( number, min, max )
function clamp(n, nMin, nMax)
	return min(max(n, nMin), nMax)
end
math.clamp = clamp

-- number = clamp01( number )
function clamp01(n)
	return min(max(n, 0), 1)
end
math.clamp01 = clamp01

-- number = clamp11( number )
function clamp11(n)
	return min(max(n, -1), 1)
end
math.clamp11 = clamp11

-- integer = clampInt( number, min, max )
function clampInt(n, nMin, nMax)
	return round(clamp(n, nMin, nMax))
end
math.clampInt = clampInt



-- bool = isFinite( number )
function isFinite(n)
	return n ~= 1/0 and n ~= -1/0 and n == n
end
math.isFinite = isFinite

-- bool = isInt( value )
function isInt(v)
	return type(v) == "number" and v == floor(v) and v == v
end
math.isInt = isInt



-- angle = normalizeAngle( angle )
function normalizeAngle(angle)
	return (angle + pi) % tau - pi
end
math.normalizeAngle = normalizeAngle

-- angle = deltaAngle( sourceAngle, angle )
function deltaAngle(a, b)
	local delta = b - a
	return atan2(sin(delta), cos(delta))
end
math.deltaAngle = deltaAngle

-- angle = angleDistance( angleA, angleB )
function angleDistance(a, b)
	return abs(deltaAngle(a, b))
end
math.angleDistance = angleDistance



function lerpExact(v1, v2, t)
	return (1-t)*v1 + t*v2
end
math.lerpExact = lerpExact

function lerp(v1, v2, t)
	return v1 + (v2-v1)*t
end
function lerp2(v11,v12, v21,v22, t)
	return v11 + (v21-v11)*t,
	       v12 + (v22-v12)*t
end
function lerp3(v11,v12,v13, v21,v22,v23, t)
	return v11 + (v21-v11)*t,
	       v12 + (v22-v12)*t,
	       v13 + (v23-v13)*t
end
function lerp4(v11,v12,v13,v14, v21,v22,v23,v24, t)
	return v11 + (v21-v11)*t,
	       v12 + (v22-v12)*t,
	       v13 + (v23-v13)*t,
	       v14 + (v24-v14)*t
end
math.lerp  = lerp
math.lerp2 = lerp2
math.lerp3 = lerp3
math.lerp4 = lerp4

-- r,g,b,a = lerpColors( colors, t )
-- colors  = { r1,g1,b1,a1, ... }
function lerpColors(colors, t)
	local len    = #colors * .25
	local iFloat = clamp(1+t*(len-1), 1, len)
	local i1     = floor(iFloat)
	local i2     = min(i1+1, len)
	local iFrac  = iFloat % 1

	i1 = i1*4 - 3
	i2 = i2*4 - 3

	return colors[i1  ] + (colors[i2  ]-colors[i1  ]) * iFrac,
	       colors[i1+1] + (colors[i2+1]-colors[i1+1]) * iFrac,
	       colors[i1+2] + (colors[i2+2]-colors[i1+2]) * iFrac,
	       colors[i1+3] + (colors[i2+3]-colors[i1+3]) * iFrac
end
math.lerpColors = lerpColors

-- v = lerp01( v1, v2, t )
function lerp01(v1, v2, t)
	return lerp(v1, v2, clamp(t, 0, 1))
end
math.lerp01 = lerp01

-- integer = lerpInt( v1, v2, t )
function lerpInt(v1, v2, t)
	return round(lerp(v1, v2, t))
end
math.lerpInt = lerpInt

-- x [, y... ] = tween( t, x1 [, y1... ], x2 [, y2... ] )
local values = {}
function tween(t, ...)
	local dimensions = select("#", ...)/2
	for i = 1, dimensions do
		local v1 = select(i,            ...)
		local v2 = select(i+dimensions, ...)
		values[i] = (1-t)*v1+t*v2
	end
	return unpack(values, i, dimensions)
end
math.tween = tween

function normalize(v1, v2, v)
	return (v1 == v2) and 0 or (v-v1)/(v2-v1)
end
math.normalize = normalize

function remap(v, old1,old2, new1,new2)
	return lerp(new1, new2, normalize(old1, old2, v))
end
math.remap = remap



-- value, targetReached = moveTowards( currentValue, targetValue, speed )
function moveTowards(current, target, speed)
	if current < target then
		current = min(current+speed, target)
	elseif current > target then
		current = max(current-speed, target)
	end
	return current, (current == target)
end
math.moveTowards = moveTowards

function damp(current, target, lambda, dt)
	-- http://www.rorydriscoll.com/2016/03/07/frame-rate-independent-damping-using-lerp/
	return lerp(current, target, 1-exp(-lambda*dt))
	-- return lerp(current, target, 1-smoothing^dt)
end
math.damp = damp



-- number = powKeepSign( number, exponent )
function powKeepSign(n, exponent)
	return (n < 0) and -(-n)^exp or n^exp
end
math.powKeepSign = powKeepSign

function squareKeepSign(n)
	return (n < 0) and -n*n or n*n
end
math.squareKeepSign = squareKeepSign



-- float = randomf( [ minFloat=0, ] maxFloat )
function randomf(nMin, nMax)
	if not nMax then  return nMin*random()  end

	return lerp(nMin, nMax, random())
end
math.randomf = randomf

-- bool = chance( [ probablitityOfTrue=0.5 ] )
function chance(v)
	return random() < (v or 0.5)
end
math.chance = chance

-- x, y = randomCircle( [ radius=1, centerX=0, centerY=0 ] )
-- https://stackoverflow.com/a/5838991
function randomCircle(radius, x, y)
	radius = radius or 1

	local a = random()
	local b = random()
	if b < a then a, b = b, a end

	local angle = a/b*tau

	b = radius*b

	return
		(x or 0) + b*cos(angle),
		(y or 0) + b*sin(angle)
end
math.randomCircle = randomCircle

-- sign = randomSign( )
-- sign = -1|1
function randomSign()
	return chance() and -1 or 1
end
math.randomSign = randomSign

-- Return a random number in the range offset-maxDistance..offset+maxDistance
-- n = around( [ maxDistance=1, offset=0 ] )
function around(maxDist, offset)
	maxDist = maxDist or 1
	return (offset or 0)+randomf(-maxDist, maxDist)
end
math.around = around



-- number = round( number [, precision=1 ] )
function round(n, precision)
	return
		precision
		and floor(n*precision+0.5)/precision
		or  floor(n+0.5)
end
math.round = round

-- number = roundEven( number [, precision=1 ] )
function roundEven(n, precision)
	return round(n, (precision or 1)*0.5)
end
math.roundEven = roundEven



-- sign = sign( number )
-- Note: sign(0) returns 1.
function sign(n)
	return (n < 0 and -1 or 1)
end
math.sign = sign

-- sign = sign0( number )
-- Note: sign0(0) returns 0.
function sign0(n)
	return (n < 0 and -1 or n > 0 and 1 or 0)
end
math.sign0 = sign0



-- value = sum( number1, number2... )
function sum(...)
	local theSum = 0
	for i = 1, select("#", ...) do
		theSum = theSum+select(i, ...)
	end
	return theSum
end
math.sum = sum



-- distance = distance( x1, y1, x2, y2 )
function distance(x1, y1, x2, y2)
	return sqrt(distanceSq(x1, y1, x2, y2))
end
math.distance = distance

-- distanceSquared = distanceSq( x1, y1, x2, y2 )
function distanceSq(x1, y1, x2, y2)
	local dx = x2-x1
	local dy = y2-y1
	return dx*dx+dy*dy
end
math.distanceSq = distanceSq

-- distance = taxicabDistance( x1, y1, x2, y2 )
function taxicabDistance(x1, y1, x2, y2)
	return abs(x2-x1)+abs(y2-y1)
end
math.taxicabDistance = taxicabDistance

-- distance = distanceFromPointToLineSq( linePoint1, linePoint2, pointOfInterest )
function distanceFromPointToLineSq(x1,y1, x2,y2, px,py)
	-- https://www.randygaul.net/2014/07/23/distance-point-to-line-segment/
    local ax, ay = x2-x1, y2-y1
    local bx, by = x1-px, y1-py
    local proj   = mathv.dot(ax,ay, bx,by)
    if proj == 0 then  return 0  end -- Is 0 the correct value to return?
    local c      = proj / mathv.dot(ax,ay, ax,ay)
    local dx, dy = bx-ax*c, by-ay*c
    return mathv.dot(dx,dy, dx,dy)
end
math.distanceFromPointToLineSq = distanceFromPointToLineSq

function distanceFromPointToLine(x1,y1, x2,y2, px,py)
	return sqrt(distanceFromPointToLineSq(x1,y1, x2,y2, px,py))
end
math.distanceFromPointToLine = distanceFromPointToLine



-- x, y = raytraceGrid( point1X, point1Y, point2X, point2Y, collisionChecker [, infinite=false ] )
-- Returns nil if there's no collision.
-- isCollision = collisionChecker( x, y )
-- http://playtechs.blogspot.se/2007/03/raytracing-on-grid.html
function raytraceGrid(x1, y1, x2, y2, collChecker, infinite)
	local dx = abs(x2 - x1)
	local dy = abs(y2 - y1)

	local x = floor(x1)
	local y = floor(y1)

	local n = 1

	local xDir
	local yDir
	local err

	if dx == 0 then
		xDir = 0
		err  = 1/0

	elseif x2 > x1 then
		xDir = 1
		n    = n + floor(x2) - x
		err  = (floor(x1) + 1 - x1) * dy

	else
		xDir = -1
		n    = n + x - floor(x2)
		err  = (x1 - floor(x1)) * dy
	end

	if dy == 0 then
		yDir = 0
		err  = -1/0

	elseif y2 > y1 then
		yDir = 1
		n    = n + floor(y2) - y
		err  = err - (floor(y1) + 1 - y1) * dx

	else
		yDir = -1
		n    = n + y - floor(y2)
		err  = err - (y1 - floor(y1)) * dx
	end

	for _ = 1, infinite and 1/0 or n do
		if collChecker(x, y) then  return x, y  end

		if err > 0 then
			y   = y + yDir
			err = err - dx
		else
			x   = x + xDir
			err = err + dy
		end
	end

	return nil
end
math.raytraceGrid = raytraceGrid

-- floodedIndices = floodGrid             ( gridWidth, gridHeight, startIndex,     visitFunction )
-- floodedIndices = floodGrid             ( gridWidth, gridHeight, startX, startY, visitFunction )
-- floodedIndices = floodGridWithDiagonals( gridWidth, gridHeight, startIndex,     visitFunction )
-- floodedIndices = floodGridWithDiagonals( gridWidth, gridHeight, startX, startY, visitFunction )
-- includeIndexInFlood = visitFunction( index, x, y )
-- Grid indices go from 1 at the top left of the grid to the bottom right, line by line, i.e.:
--   1 2 3
--   4 5 6
--   7 8 9
-- Return nil from visitFunction() to stop the flood.
do
	local floor  = math.floor
	local insert = table.insert
	local remove = table.remove

	local function iToXy(i, w)
		i = i-1
		return i%w, floor(i/w)
	end
	local function xyToI(x, y, w)
		return y*w+x+1
	end

	local indicesToVisit = {}

	local function floodInit(w, h, x, y, visiter)
		local i
		if visiter then
			i = xyToI(x, y, w)
		else
			i, visiter = x, y
			x, y = iToXy(i, w)
		end

		local indices = {i}
		local ignore  = {[i]=true}

		if indicesToVisit[i] then -- In case floodGrid() was wrapped in a pcall() and an error happened in visiter().
			for i = 1, #indicesToVisit do
				indicesToVisit[i] = nil
			end
		end

		return x, y, i, indices, ignore, visiter
	end

	function floodGrid(w, h, x, y, visiter)
		local i, indices, ignore
		x, y, i, indices, ignore, visiter = floodInit(w, h, x, y, visiter)

		if x > 0   then  insert(indicesToVisit, i-1) ; ignore[i-1] = true  end
		if y > 0   then  insert(indicesToVisit, i-w) ; ignore[i-w] = true  end
		if x < w-1 then  insert(indicesToVisit, i+1) ; ignore[i+1] = true  end
		if y < h-1 then  insert(indicesToVisit, i+w) ; ignore[i+w] = true  end

		local include

		while true do
			i = remove(indicesToVisit)
			if not i then  break  end

			x, y = iToXy(i, w)

			include = visiter(i, x, y)
			if include == nil then
				break
			elseif include then
				insert(indices, i)
				if x > 0   and not ignore[i-1] then  insert(indicesToVisit, i-1) ; ignore[i-1] = true  end
				if y > 0   and not ignore[i-w] then  insert(indicesToVisit, i-w) ; ignore[i-w] = true  end
				if x < w-1 and not ignore[i+1] then  insert(indicesToVisit, i+1) ; ignore[i+1] = true  end
				if y < h-1 and not ignore[i+w] then  insert(indicesToVisit, i+w) ; ignore[i+w] = true  end
			end
		end

		return indices
	end
	math.floodGrid = floodGrid

	function floodGridWithDiagonals(w, h, x, y, visiter)
		local i, indices, ignore
		x, y, i, indices, ignore, visiter = floodInit(w, h, x, y, visiter)

		if x > 0               then  insert(indicesToVisit, i-1) ; ignore[i-1  ] = true  end
		if y > 0               then  insert(indicesToVisit, i-w) ; ignore[i-w  ] = true  end
		if x < w-1             then  insert(indicesToVisit, i+1) ; ignore[i+1  ] = true  end
		if y < h-1             then  insert(indicesToVisit, i+w) ; ignore[i+w  ] = true  end
		if x > 0   and y > 0   then  insert(indicesToVisit, i+1) ; ignore[i-w-1] = true  end
		if x < w-1 and y > 0   then  insert(indicesToVisit, i+1) ; ignore[i-w+1] = true  end
		if x > 0   and y < h-1 then  insert(indicesToVisit, i+1) ; ignore[i+w-1] = true  end
		if x < w-1 and y < h-1 then  insert(indicesToVisit, i+1) ; ignore[i+w+1] = true  end

		local include

		while true do
			i = remove(indicesToVisit)
			if not i then  break  end

			x, y = iToXy(i, w)

			include = visiter(i, x, y)
			if include == nil then
				break
			elseif include then
				insert(indices, i)
				if x > 0               and not ignore[i-1  ] then  insert(indicesToVisit, i-1) ; ignore[i-1  ] = true  end
				if y > 0               and not ignore[i-w  ] then  insert(indicesToVisit, i-w) ; ignore[i-w  ] = true  end
				if x < w-1             and not ignore[i+1  ] then  insert(indicesToVisit, i+1) ; ignore[i+1  ] = true  end
				if y < h-1             and not ignore[i+w  ] then  insert(indicesToVisit, i+w) ; ignore[i+w  ] = true  end
				if x > 0   and y > 0   and not ignore[i-w-1] then  insert(indicesToVisit, i+1) ; ignore[i-w-1] = true  end
				if x < w-1 and y > 0   and not ignore[i-w+1] then  insert(indicesToVisit, i+1) ; ignore[i-w+1] = true  end
				if x > 0   and y < h-1 and not ignore[i+w-1] then  insert(indicesToVisit, i+1) ; ignore[i+w-1] = true  end
				if x < w-1 and y < h-1 and not ignore[i+w+1] then  insert(indicesToVisit, i+1) ; ignore[i+w+1] = true  end
			end
		end

		return indices
	end
	math.floodGridWithDiagonals = floodGridWithDiagonals
end

-- A bit faster and less memory consuming than floodGrid().
-- WARNING: There's a high risk of stack overflow for large grids! floodGrid() is the safe option.
-- floodedIndices      = floodGridFast( gridWidth, gridHeight, startIndex, visitFunction )
-- includeIndexInFlood = visitFunction( index )
-- Grid indices go from 1 at the top left of the grid to the bottom right, line by line, i.e.:
--   1 2 3
--   4 5 6
--   7 8 9
do
	local insert = table.insert
	local visits = {}
	local floodAround

	local function maybeFloodFrom(mapLen, w, visiter, indices, i)
		if i < 1 or i > mapLen or visits[i] then  return  end

		visits[i] = true

		if visiter(i) then
			insert(indices, i)
			return floodAround(mapLen, w, visiter, indices, i) -- Tail call.
		end
	end

	function floodAround(mapLen, w, visiter, indices, i)
		if (i-1)%w ~= 0 then
		maybeFloodFrom(mapLen, w, visiter, indices, i-1)
		end
		maybeFloodFrom(mapLen, w, visiter, indices, i-w)
		if (i  )%w ~= 0 then
		maybeFloodFrom(mapLen, w, visiter, indices, i+1)
		end
		maybeFloodFrom(mapLen, w, visiter, indices, i+w)
	end

	function floodGridFast(w, h, i, visiter)
		local mapLen = w*h

		for i = 1, mapLen do
			visits[i] = false
		end
		visits[i] = true

		local indices = {i}
		floodAround(mapLen, w, visiter, indices, i)

		return indices
	end
	math.floodGridFast = floodGridFast
end

--[[ Flood fill benchmark.
local w     = 204
local h     = 20
local loops = 500
-- local h     = 70 -- Stack overflow for floodGridFast().
-- local loops = 1

for _, flooder in ipairs{{"floodGridFast",floodGridFast}, {"floodGrid",floodGrid}} do
	local flood = flooder[2]
	print("flood "..flooder[1])

	collectgarbage()
	collectgarbage("stop")
	local kbytes = collectgarbage"count"
	local time   = os.clock()

	local indices
	for i = 1, loops do
		indices = flood(w, h, h/2*w+w/2+1, function(i, x, y)
			x = x or (i-1)%w
			y = y or floor((i-1)/w)
			return distance(x/w, y/h, .5, .5) < .5
		end)
	end

	time   = os.clock()-time
	kbytes = collectgarbage"count"-kbytes
	collectgarbage("restart")

	local indexMap = {}
	for order, i in ipairs(indices) do
		indexMap[i] = order
	end

	local i = 0
	for y = 0, h-1 do
		for x = 0, w-1 do
			i           = i+1
			local order = indexMap[i]
			io.stdout:write(order and floor(9*order/#indices) or "-")
		end
		io.stdout:write("\n")
	end
	print(tostring(kbytes*1024):reverse():gsub("..?.?", ",%0"):reverse():gsub(",$", ""), time)
	print()
end
--]]

-- flooded = flood( startValue, getConnections )
-- stop    = getConnections( connections, value ) -- Returning true stops the flood.
function flood(startValue, getConnections)
	local flooded = {startValue}
	local ignore  = {[startValue]=true}
	local toVisit = {startValue}
	local conns   = {}

	while toVisit[1] do
		if getConnections(conns, table.remove(toVisit)) then  break  end

		for i, conn in ipairs(conns) do
			if not ignore[conn] then
				ignore[conn] = true
				table.insert(flooded, conn)
				table.insert(toVisit, conn)
			end
			conns[i] = nil
		end
	end

	return flooded
end
math.flood = flood



-- value = cos01( angle )
-- value = 0..1
function cos01(angle)
	return .5 + .5*cos(angle)
end
math.cos01 = cos01

-- value = sin01( angle )
-- value = 0..1
function sin01(angle)
	return 0.5+0.5*sin(angle)
end
math.sin01 = sin01



function integral(v)
	return (math.modf(v))
end
math.integral = integral

function fraction(v)
	local _, fract = math.modf(v)
	return fract
end
math.fraction = fraction



-- point = aabbCornerFarthestFromPoint( aabbCenterX, aabbCenterY, aabbWidth, aabbHeight, pointX, pointY )
function aabbCornerFarthestFromPoint(aabbX, aabbY, aabbWidth, aabbHeight, pointX, pointY)
	return
		aabbX+(pointX < aabbX and aabbWidth  or -aabbWidth )*0.5,
		aabbY+(pointY < aabbY and aabbHeight or -aabbHeight)*0.5
end
math.aabbCornerFarthestFromPoint = aabbCornerFarthestFromPoint



-- path      = pathfindAstar( start, goal, getNeighbors, getDistance [, getHeuristicCostEstimate=getDistance ] )
-- neighbors = getNeighbors( node )
-- distance  = getDistance( node1, node2 )
-- cost      = getHeuristicCostEstimate( node1, node2 )
--
-- https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
do
	local function nodeWithLowestScore(nodeSet, scores)
		local lowestScore = 1/0
		local lowestNode  = nil

		for node in pairs(nodeSet) do -- @Speed: Can we get rid of pairs()?
			local score = scores[node]
			if score and score < lowestScore then
				lowestNode  = node
				lowestScore = score
			end
		end

		return lowestNode
	end

	function pathfindAstar(start, goal, getNeighbors, getDistance, getHeuristicCostEstimate)
		getHeuristicCostEstimate = getHeuristicCostEstimate or getDistance

		-- The set of nodes already evaluated
		local closedSet = {}

		-- The set of currently discovered nodes that are not evaluated yet.
		-- Initially, only the start node is known.
		local openSet = {[start]=true}

		-- For each node, which node it can most efficiently be reached from.
		-- If a node can be reached from many nodes, cameFrom will eventually contain the
		-- most efficient previous step.
		local cameFrom = {} -- An empty map.

		-- For each node, the cost of getting from the start node to that node.
		local scoreFromStart = {} -- Map with default value of Infinity.

		-- The cost of going from start to start is zero.
		scoreFromStart[start] = 0

		-- For each node, the total cost of getting from the start node to the goal
		-- by passing by that node. That value is partly known, partly heuristic.
		local fullScore = {} -- Map with default value of Infinity.

		-- For the first node, that value is completely heuristic.
		fullScore[start] = getHeuristicCostEstimate(start, goal)

		while next(openSet) do -- @Speed: Can we get rid of next()?
			local current = nodeWithLowestScore(openSet, fullScore)

			if current == goal then
				local totalPath = {current}

				while true do
					current = cameFrom[current]
					if not current then  break  end

					table.insert(totalPath, current)
				end

				return reverseTable(totalPath)
			end

			openSet[current]   = nil
			closedSet[current] = true

			for _, neighbor in ipairs(getNeighbors(current)) do
				if closedSet[neighbor] then
					-- Continue. Ignore the neighbor which is already evaluated.

				else
					-- Discover a new node.
					openSet[neighbor] = true

					-- The distance from start to a neighbor.
					-- The "getDistance" function may vary as per the solution requirements.
					local tentativeScoreToStart = (scoreFromStart[current] or 1/0) + getDistance(current, neighbor)
					if tentativeScoreToStart >= (scoreFromStart[neighbor] or 1/0) then
						-- Continue. This is not a better path.

					else
						-- This path is the best until now. Record it!
						cameFrom[neighbor]       = current
						scoreFromStart[neighbor] = tentativeScoreToStart
						fullScore[neighbor]      = scoreFromStart[neighbor] + getHeuristicCostEstimate(neighbor, goal)
					end
				end
			end
		end

		return nil
	end
	math.pathfindAstar = pathfindAstar
end



-- point = pointOnLineClosestToPoint( linePoint1, linePoint2, targetPoint )
function pointOnLineClosestToPoint(lineX1,lineY1, lineX2,lineY2, x,y)
	-- See pointOnSegmentClosestToPoint().
	if lineX1 == lineX2 and lineY1 == lineY2 then  return lineX1,lineY1  end

	local lineX = lineX2-lineX1
	local lineY = lineY2-lineY1

	local lineLen = mathv.len(lineX,lineY)

	local lineNormalizedX = lineX/lineLen
	local lineNormalizedY = lineY/lineLen

	local proj = mathv.dot(x-lineX1,y-lineY1, lineNormalizedX,lineNormalizedY)
	return
		lineX1+lineNormalizedX*proj,
		lineY1+lineNormalizedY*proj
end
math.pointOnLineClosestToPoint = pointOnLineClosestToPoint

-- point = pointOnSegmentClosestToPoint( segmentPoint1, segmentPoint2, targetPoint )
-- http://doswa.com/2009/07/13/circle-segment-intersectioncollision.html
function pointOnSegmentClosestToPoint(segX1,segY1, segX2,segY2, x,y)
	if segX1 == segX2 and segY1 == segY2 then  return segX1,segY1  end

	local segX = segX2-segX1
	local segY = segY2-segY1

	local segLen = mathv.len(segX,segY)

	local segNormalizedX = segX/segLen
	local segNormalizedY = segY/segLen

	local proj = mathv.dot(x-segX1,y-segY1, segNormalizedX,segNormalizedY)
	if proj <= 0      then  return segX1,segY1  end
	if proj >= segLen then  return segX2,segY2  end

	return
		segX1+segNormalizedX*proj,
		segY1+segNormalizedY*proj
end
math.pointOnSegmentClosestToPoint = pointOnSegmentClosestToPoint



--==============================================================
--= Vectors ====================================================
--==============================================================



function mathv.format(x, y)
	return ("Vector(%f, %f)"):format(x, y)
end



-- x, y = v.add( ax, ay,  bx, by  )
-- x, y = v.add( ax, ay,  b,  nil )
-- x, y = v.add( a,  nil, bx, by  )
function mathv.add(ax, ay, bx, by)
	if ay and by then
		return ax+bx, ay+by
	elseif ay then
		return ax+bx, ay+bx
	else
		return ax+bx, ax+by
	end
end

-- x, y = v.sub( ax, ay,  bx, by  )
-- x, y = v.sub( ax, ay,  b,  nil )
-- x, y = v.sub( a,  nil, bx, by  )
function mathv.sub(ax, ay, bx, by)
	if ay and by then
		return ax-bx, ay-by
	elseif ay then
		return ax-bx, ay-bx
	else
		return ax-bx, ax-by
	end
end

-- x, y = v.mul( ax, ay,  bx, by  )
-- x, y = v.mul( ax, ay,  b,  nil )
-- x, y = v.mul( a,  nil, bx, by  )
function mathv.mul(ax, ay, bx, by)
	if ay and by then
		return ax*bx, ay*by
	elseif ay then
		return ax*bx, ay*bx
	else
		return ax*bx, ax*by
	end
end

-- x, y = v.div( ax, ay,  bx, by  )
-- x, y = v.div( ax, ay,  b,  nil )
-- x, y = v.div( a,  nil, bx, by  )
function mathv.div(ax, ay, bx, by)
	if ay and by then
		return ax/bx, ay/by
	elseif ay then
		return ax/bx, ay/bx
	else
		return ax/bx, ax/by
	end
end

function mathv.eq(ax, ay, bx, by)
	return ax == bx and ay == by
end

function mathv.lt(ax, ay, bx, by)
	return ax < bx or (ax == bx and ay < by)
end

function mathv.le(ax, ay, bx, by)
	return (ax == bx and ay == by) or mathv.lt(ax, ay, bx, by)
end



function mathv.abs(x, y)
	return abs(x), abs(y)
end

function mathv.angle(x, y)
	return atan2(y, x)
end
function mathv.angleDeg(x, y)
	return deg(atan2(y, x))
end

function mathv.clamp(x, y, minX, minY, maxX, maxY)
	return
		min(max(x, minX), maxX),
		min(max(y, minY), maxY)
end

function mathv.distance(x, y, otherX, otherY)
	return mathv.len(otherX-x, otherY-y)
end
function mathv.distanceSq(x, y, otherX, otherY)
	return mathv.lenSq(otherX-x, otherY-y)
end

-- Dot projects one vector onto another.
function mathv.dot(x, y, otherX, otherY)
	return x*otherX + y*otherY
end
-- The sign of the 2D cross product tells whether the second vector is on the left or right side of the first vector.
-- The absolute value of the 2D cross product is the sine of the angle in between the two vectors.
-- 2D cross product is also known as perp(endicular) dot product.
-- http://allenchou.net/2013/07/cross-product-of-2d-vectors/
function mathv.cross(x, y, otherX, otherY)
	return x*otherY - y*otherX
end

function mathv.len(x, y)
	return sqrt(x*x + y*y)
end
function mathv.lenSq(x, y)
	return x*x + y*y
end

function mathv.normalize(x, y)
	local len = mathv.len(x, y)
	return x/len, y/len
end

function mathv.perpendicular(x, y)
	return -y, x
end

function mathv.projectOn(x, y, otherX, otherY)
	local otherLenSq = mathv.lenSq(otherX, otherY)
	return
		x*otherX*otherX/otherLenSq,
		y*otherY*otherY/otherLenSq
end

function mathv.rotate(x, y, angle)
	local c = cos(angle)
	local s = sin(angle)
	return x*c-y*s, x*s+y*c
end
function mathv.rotateCw(x, y)
	return -y, x
end
function mathv.rotateCcw(x, y)
	return y, -x
end
function mathv.rotateQuarters(x,y, quarters)
	quarters = quarters%4
	if quarters == 0 then
		return x, y
	elseif quarters == 1 then
		return -y, x -- CW
	elseif quarters == 2 then
		return -x, -y
	else
		return y, -x -- CCW
	end
end

-- x,y = scale( x,y, sx [, sy=sx ] )
function mathv.scale(x, y, sx, sy)
	return x*sx, y*(sy or sx)
end

function mathv.translate(x, y, dx, dy)
	return x+dx, y+dy
end
-- x,y = translateTowards( x,y, dist, angle )
-- x,y = translateTowards( x,y, dist, otherX,otherY )
function mathv.translateTowards(x,y, dist, otherXOrAngle,otherY)
	if otherY then
		otherXOrAngle = atan2(otherY-y, otherXOrAngle-x)
	end
	return
		x + dist*cos(otherXOrAngle), -- @Speed: Use vector math!
		y + dist*sin(otherXOrAngle)
end
-- x,y, targetReached = moveTowards( x,y, targetX,targetY, speed )
function mathv.moveTowards(x,y, targetX,targetY, speed)
	local angle = atan2(targetY-y, targetX-x)
	local targetReachedX, targetReachedY
	x, targetReachedX = moveTowards(x, targetX, speed*abs(cos(angle))) -- @Speed: Use vector math!
	y, targetReachedY = moveTowards(y, targetY, speed*abs(sin(angle)))
	return x,y, (targetReachedX and targetReachedY)
end
function mathv.damp(x1,y1, x2,y2, lambda, dt)
	-- See math.damp().
	local progress = 1-exp(-lambda*dt)
	return lerp(x1, x2, progress), lerp(y1, y2, progress)
end
function mathv.lerpExact(x1,y1, x2,y2, progress)
	return lerpExact(x1, x2, progress), lerpExact(y1, y2, progress)
end
function mathv.lerp(x1,y1, x2,y2, progress)
	return lerp(x1, x2, progress), lerp(y1, y2, progress)
end

-- Get an appropriate epsilon for two vectors.
function mathv.epsilon(x1,y1, x2,y2)
	local dx = x2-x1
	local dy = y2-y1
	return 0.003*(dx*dx+dy*dy)
end

function mathv.min(x1,y1, x2,y2)
	return min(x1, x2), min(y1, y2)
end
function mathv.max(x1,y1, x2,y2)
	return max(x1, x2), max(y1, y2)
end

function mathv.withMaxLength(x1,y1, x2,y2)
	if mathv.lenSq(x1,y1) > mathv.lenSq(x2,y2) then
		return x1,y1
	else
		return x2,y2
	end
end

function mathv.withMinLength(x1,y1, x2,y2)
	if mathv.lenSq(x1,y1) < mathv.lenSq(x2,y2) then
		return x1,y1
	else
		return x2,y2
	end
end
function mathv.withMaxLength(x1,y1, x2,y2)
	if mathv.lenSq(x1,y1) > mathv.lenSq(x2,y2) then
		return x1,y1
	else
		return x2,y2
	end
end

function mathv.floor(x,y)
	return floor(x), floor(y)
end
function mathv.ceil(x,y)
	return ceil(x), ceil(y)
end
-- point = round( point [, precision=1 ] )
function mathv.round(x,y, precision)
	if precision then
		return floor(x*precision+.5)/precision, floor(y*precision+.5)/precision
	else
		return floor(x+.5), floor(y+.5)
	end
end

function mathv.swap(x1,y1, x2,y2)
	return x2,y2, x1,y1
end



--==============================================================
--= Intersections ==============================================
--==============================================================



-- bool = isAabbIntersectingAabb( aCenter, aSize, bCenter, bSize )
function isAabbIntersectingAabb(ax,ay, aw,ah, bx,by, bw,bh)
	return
		abs(bx-ax) < (aw+bw)*0.5 and
		abs(by-ay) < (ah+bh)*0.5
end
math.isAabbIntersectingAabb = isAabbIntersectingAabb

-- bool = isCircleIntersectingAabb( circleCenter, circleRadius, aabbCenter, aabbSize )
function isCircleIntersectingAabb(circleX,circleY, radius, aabbX,aabbY, w,h)
	local wHalf = w*0.5
	local hHalf = h*0.5
	local closestInAabbX, closestInAabbY = mathv.clamp(circleX,circleY, aabbX-wHalf,aabbY-hHalf, aabbX+wHalf,aabbY+hHalf)
	return mathv.lenSq(circleX-closestInAabbX, circleY-closestInAabbY) - radius*radius < 0
end
math.isCircleIntersectingAabb = isCircleIntersectingAabb

-- bool = isCircleIntersectingCircle( aCircleCenter, aCircleRadius, bCircleCenter, bCircleRadius)
function isCircleIntersectingCircle(ax,ay, aRadius, bx,by, bRadius)
	return isPointIntersectingCircle(ax,ay, bx,by, bRadius+aRadius)
end
math.isCircleIntersectingCircle = isCircleIntersectingCircle

-- bool = isCircleIntersectingRectangle( circleCenter, circleRadius, rectCenter, rectSize, rectAngle )
function isCircleIntersectingRectangle(circleX,circleY, radius, rectX,rectY, w,h, angle)
	local circleInRectSpaceX, circleInRectSpaceY = mathv.rotate(circleX-rectX, circleY-rectY, -angle)
	return isCircleIntersectingAabb(circleInRectSpaceX,circleInRectSpaceY, radius, 0,0, w,h)
end
math.isCircleIntersectingRectangle = isCircleIntersectingRectangle

-- bool = isLineIntersectingAabb( linePoint1, linePoint2, rectCenter, rectSize )
function isLineIntersectingAabb(lineX1,lineY1, lineX2,lineY2, rectX,rectY, w,h)
	if lineX1 == lineX2 and lineY1 == lineY2 then  lineX2 = lineX2+1  end

	local ax, ay = rectX-w*.5, rectY-h*.5
	local bx, by = ax+w,       ay
	local cx, cy = ax+w,       ay+h
	local dx, dy = ax,         ay+h
	return
		isLineIntersectingSegment(lineX1,lineY1, lineX2,lineY2, ax,ay, bx,by) or
		isLineIntersectingSegment(lineX1,lineY1, lineX2,lineY2, bx,by, cx,cy) or
		isLineIntersectingSegment(lineX1,lineY1, lineX2,lineY2, cx,cy, dx,dy) or
		isLineIntersectingSegment(lineX1,lineY1, lineX2,lineY2, dx,dy, ax,ay)
end
math.isLineIntersectingAabb = isLineIntersectingAabb
-- point1, point2 = getLineAabbIntersections( linePoint1, linePoint2, aabbCenter, aabbSize )
function getLineAabbIntersections(lineX1,lineY1, lineX2,lineY2, aabbX,aabbY, w,h)
	if lineX1 == lineX2 and lineY1 == lineY2 then  lineX2 = lineX2+1  end

	local ax, ay = aabbX-w*.5, aabbY-h*.5
	local bx, by = ax+w,       ay
	local cx, cy = ax+w,       ay+h
	local dx, dy = ax,         ay+h

	local ix1, iy1 = getLineSegmentIntersection(lineX1,lineY1, lineX2,lineY2, ax,ay, bx,by)
	local ix2, iy2 = getLineSegmentIntersection(lineX1,lineY1, lineX2,lineY2, bx,by, cx,cy)
	local ix3, iy3 = getLineSegmentIntersection(lineX1,lineY1, lineX2,lineY2, cx,cy, dx,dy)
	local ix4, iy4 = getLineSegmentIntersection(lineX1,lineY1, lineX2,lineY2, dx,dy, ax,ay)
	if not (ix1 or ix2 or ix3 or ix4) then  return nil  end

	local lineDx = lineX2-lineX1
	local lineDy = lineY2-lineY1

	local proj1 = ix1 and mathv.dot(ix1,iy1, lineDx,lineDy)
	local proj2 = ix2 and mathv.dot(ix2,iy2, lineDx,lineDy)
	local proj3 = ix3 and mathv.dot(ix3,iy3, lineDx,lineDy)
	local proj4 = ix4 and mathv.dot(ix4,iy4, lineDx,lineDy)

	local theMin = min(proj1 or  1/0, proj2 or  1/0, proj3 or  1/0, proj4 or  1/0)
	local theMax = max(proj1 or -1/0, proj2 or -1/0, proj3 or -1/0, proj4 or -1/0)

	local x1, y1, x2, y2

	if     proj1 == theMin then  x1, y1 = ix1, iy1
	elseif proj2 == theMin then  x1, y1 = ix2, iy2
	elseif proj3 == theMin then  x1, y1 = ix3, iy3
	else                         x1, y1 = ix4, iy4  end

	if     proj1 == theMax then  x2, y2 = ix1, iy1
	elseif proj2 == theMax then  x2, y2 = ix2, iy2
	elseif proj3 == theMax then  x2, y2 = ix3, iy3
	else                         x2, y2 = ix4, iy4  end

	return x1,y1, x2,y2
end
math.getLineAabbIntersections = getLineAabbIntersections

-- bool = isLineIntersectingCircle( linePoint1, linePoint2, circleCenter, circleRadius )
function isLineIntersectingCircle(lineX1,lineY1, lineX2,lineY2, circleX,circleY, radius)
	local x, y = pointOnLineClosestToPoint(lineX1,lineY1, lineX2,lineY2, circleX,circleY)
	return mathv.lenSq(circleX-x, circleY-y) < radius*radius
end
math.isLineIntersectingCircle = isLineIntersectingCircle
-- point1, point2 = getLineCircleIntersections( linePoint1, linePoint2, circleCenter, circleRadius )
function getLineCircleIntersections(lineX1,lineY1, lineX2,lineY2, circleX,circleY, radius)
	-- http://mathworld.wolfram.com/Circle-LineIntersection.html
	local x1 = lineX1-circleX
	local y1 = lineY1-circleY
	local x2 = lineX2-circleX
	local y2 = lineY2-circleY

	local dx = x2-x1
	local dy = y2-y1
	local dr = sqrt(dx^2+dy^2)
	local D  = x1*y2-x2*y1

	local dr_2     = dr*dr
	local dr_2_inv = 1/dr_2 -- Not sure if faster than just doing `/dr_2` four times below...

	local Ddx  = D*dx
	local Ddy  = D*dy
	local rdrD = sqrt(radius*radius * dr_2 - D*D)

	local dySign = sign(dy)

	local xProd = dySign  * rdrD * dx
	local yProd = abs(dy) * rdrD

	local ax = ( Ddy-xProd)*dr_2_inv
	local bx = ( Ddy+xProd)*dr_2_inv
	local ay = (-Ddx-yProd)*dr_2_inv
	local by = (-Ddx+yProd)*dr_2_inv

	-- Make sure A is closest to linePoint1.
	if dySign < 0 then
		ax, bx = bx, ax
		ay, by = by, ay
	end

	-- local discriminant = r_2*dr_2-D_2
	-- discriminant < 0: no intersection
	-- discriminant = 0: tangent
	-- discriminant > 0: intersection

	return
		circleX+ax, circleY+ay,
		circleX+bx, circleY+by
end
math.getLineCircleIntersections = getLineCircleIntersections

-- bool = isLineIntersectingLine( aLinePoint1, aLinePoint2, bLinePoint1, bLinePoint2 )
function isLineIntersectingLine(ax1,ay1, ax2,ay2, bx1,by1, bx2,by2)
	return -(bx2-bx1)*(ay2-ay1) + (ax2-ax1)*(by2-by1) ~= 0
end
math.isLineIntersectingLine = isLineIntersectingLine
-- point = getLineLineIntersection( aLinePoint1, aLinePoint2, bLinePoint1, bLinePoint2 )
function getLineLineIntersection(ax1,ay1, ax2,ay2, bx1,by1, bx2,by2)
	-- See getSegmentSegmentIntersection().
	local dx1 = ax2-ax1
	local dy1 = ay2-ay1
	local dx2 = bx2-bx1
	local dy2 = by2-by1

	local det = -dx2*dy1 + dx1*dy2
	if det == 0 then  return nil  end -- Parallel lines.

	local t = (dx2*(ay1-by1) - dy2*(ax1-bx1)) / det
	return ax1+t*dx1, ay1+t*dy1
end
math.getLineLineIntersection = getLineLineIntersection

-- bool = isLineIntersectingSegment( linePoint1, linePoint2, segmentPoint1, segmentPoint2 )
function isLineIntersectingSegment(lineX1,lineY1, lineX2,lineY2, segX1,segY1, segX2,segY2)
	return getSideOfLine(lineX1,lineY1, lineX2,lineY2, segX1,segY1)
	    ~= getSideOfLine(lineX1,lineY1, lineX2,lineY2, segX2,segY2)
end
math.isLineIntersectingSegment = isLineIntersectingSegment
-- point = getLineSegmentIntersection( linePoint1, linePoint2, segmentPoint1, segmentPoint2 )
function getLineSegmentIntersection(ax1,ay1, ax2,ay2, bx1,by1, bx2,by2)
	-- See getSegmentSegmentIntersection().
	local dx1 = ax2-ax1
	local dy1 = ay2-ay1
	local dx2 = bx2-bx1
	local dy2 = by2-by1

	local det = -dx2*dy1 + dx1*dy2
	if det == 0 then  return nil  end -- Parallel lines.

	local s = (-dy1*(ax1-bx1) + dx1*(ay1-by1)) / det
	if s < 0 or s > 1 then  return nil  end -- No intersection.

	local t = (dx2*(ay1-by1) - dy2*(ax1-bx1)) / det
	return ax1+t*dx1, ay1+t*dy1
end
math.getLineSegmentIntersection = getLineSegmentIntersection

-- bool = isLineIntersectingRectangle( linePoint1, linePoint2, rectCenter, rectSize, rectAngle )
function isLineIntersectingRectangle(lineX1,lineY1, lineX2,lineY2, rectX,rectY, w,h, angle)
	local lineInRectSpaceX1, lineInRectSpaceY1 = mathv.rotate(lineX1-rectX, lineY1-rectY, -angle)
	local lineInRectSpaceX2, lineInRectSpaceY2 = mathv.rotate(lineX2-rectX, lineY2-rectY, -angle)
	return isLineIntersectingAabb(lineInRectSpaceX1,lineInRectSpaceY1, lineInRectSpaceX2,lineInRectSpaceY2, 0,0, w,h)
end
math.isLineIntersectingRectangle = isLineIntersectingRectangle
-- point1, point2 = getLineRectangleIntersections( linePoint1, linePoint2, rectCenter, rectSize, rectAngle )
function getLineRectangleIntersections(lineX1,lineY1, lineX2,lineY2, rectX,rectY, w,h, angle)
	if lineX1 == lineX2 and lineY1 == lineY2 then  lineX2 = lineX2+1  end

	local lineInRectSpaceX1, lineInRectSpaceY1 = mathv.rotate(lineX1-rectX, lineY1-rectY, -angle)
	local lineInRectSpaceX2, lineInRectSpaceY2 = mathv.rotate(lineX2-rectX, lineY2-rectY, -angle)
	local x1,y1, x2,y2 = getLineAabbIntersections(lineInRectSpaceX1,lineInRectSpaceY1, lineInRectSpaceX2,lineInRectSpaceY2, 0,0, w,h)
	if x1 then
		x1, y1 = mathv.rotate(x1, y1, angle)
		x1, y1 = rectX+x1, rectY+y1
	end
	if x2 then
		x2, y2 = mathv.rotate(x2, y2, angle)
		x2, y2 = rectX+x2, rectY+y2
	end
	return x1,y1, x2,y2
end
math.getLineRectangleIntersections = getLineRectangleIntersections

-- bool = isLineIntersectingTriangle( linePoint1, linePoint2, trianglePoint1, trianglePoint2, trianglePoint3 )
function isLineIntersectingTriangle(lineX1,lineY1, lineX2,lineY2, triX1,triY1, triX2,triY2, triX3,triY3)
	if lineX1 == lineX2 and lineY1 == lineY2 then  lineX2 = lineX2+1  end
	return
		isLineIntersectingSegment(lineX1,lineY1, lineX2,lineY2, triX1,triY1, triX2,triY2) or
		isLineIntersectingSegment(lineX1,lineY1, lineX2,lineY2, triX2,triY2, triX3,triY3) or
		isLineIntersectingSegment(lineX1,lineY1, lineX2,lineY2, triX3,triY3, triX1,triY1)
end
math.isLineIntersectingTriangle = isLineIntersectingTriangle
-- point1, point2 = getLineTriangleIntersections( linePoint1, linePoint2, trianglePoint1, trianglePoint2, trianglePoint3 )
function getLineTriangleIntersections(lineX1,lineY1, lineX2,lineY2, triX1,triY1, triX2,triY2, triX3,triY3)
	if lineX1 == lineX2 and lineY1 == lineY2 then  lineX2 = lineX2+1  end

	local ix1, iy1 = getLineSegmentIntersection(lineX1,lineY1, lineX2,lineY2, triX1,triY1, triX2,triY2)
	local ix2, iy2 = getLineSegmentIntersection(lineX1,lineY1, lineX2,lineY2, triX2,triY2, triX3,triY3)
	local ix3, iy3 = getLineSegmentIntersection(lineX1,lineY1, lineX2,lineY2, triX3,triY3, triX1,triY1)
	if not (ix1 or ix2 or ix3) then  return nil  end

	local lineDx = lineX2-lineX1
	local lineDy = lineY2-lineY1

	local proj1 = ix1 and mathv.dot(ix1,iy1, lineDx,lineDy)
	local proj2 = ix2 and mathv.dot(ix2,iy2, lineDx,lineDy)
	local proj3 = ix3 and mathv.dot(ix3,iy3, lineDx,lineDy)

	local theMin = min(proj1 or  1/0, proj2 or  1/0, proj3 or  1/0)
	local theMax = max(proj1 or -1/0, proj2 or -1/0, proj3 or -1/0)

	local x1, y1, x2, y2

	if     proj1 == theMin then  x1, y1 = ix1, iy1
	elseif proj2 == theMin then  x1, y1 = ix2, iy2
	else                         x1, y1 = ix3, iy3  end

	if     proj1 == theMax then  x2, y2 = ix1, iy1
	elseif proj2 == theMax then  x2, y2 = ix2, iy2
	else                         x2, y2 = ix3, iy3  end

	return x1,y1, x2,y2
end
math.getLineTriangleIntersections = getLineTriangleIntersections

-- bool = isSegmentIntersectingAabb( segmentPoint1, segmentPoint2, rectCenter, rectSize )
function isSegmentIntersectingAabb(segX1,segY1, segX2,segY2, rectX,rectY, w,h)
	if isPointIntersectingAabb(segX1,segY1, rectX,rectY, w,h) then  return true  end
	if isPointIntersectingAabb(segX2,segY2, rectX,rectY, w,h) then  return true  end

	local ax, ay = rectX-w*0.5, rectY-h*0.5
	local bx, by = ax+w,        ay
	local cx, cy = ax+w,        ay+h
	local dx, dy = ax,          ay+h
	return
		isSegmentIntersectingSegment(segX1,segY1, segX2,segY2, ax,ay, bx,by) or
		isSegmentIntersectingSegment(segX1,segY1, segX2,segY2, bx,by, cx,cy) or
		isSegmentIntersectingSegment(segX1,segY1, segX2,segY2, cx,cy, dx,dy) or
		isSegmentIntersectingSegment(segX1,segY1, segX2,segY2, dx,dy, ax,ay)
end
math.isSegmentIntersectingAabb = isSegmentIntersectingAabb
-- point1, point2 = getSegmentAabbIntersections( segmentPoint1, segmentPoint2, aabbCenter, aabbSize )
function getSegmentAabbIntersections(segX1,segY1, segX2,segY2, aabbX,aabbY, w,h)
	local ax, ay = aabbX-w*0.5, aabbY-h*0.5
	local bx, by = ax+w,        ay
	local cx, cy = ax+w,        ay+h
	local dx, dy = ax,          ay+h

	local x1, y1 = nil
	if isPointIntersectingAabb(segX1,segY1, aabbX,aabbY, w,h) then
		x1, y1 = segX1, segY1
	else
		local ix1, iy1 = getSegmentSegmentIntersection(segX1,segY1, segX2,segY2, ax,ay, bx,by)
		local ix2, iy2 = getSegmentSegmentIntersection(segX1,segY1, segX2,segY2, bx,by, cx,cy)
		local ix3, iy3 = getSegmentSegmentIntersection(segX1,segY1, segX2,segY2, cx,cy, dx,dy)
		local ix4, iy4 = getSegmentSegmentIntersection(segX1,segY1, segX2,segY2, dx,dy, ax,ay)
		x1, y1 = ix1, iy1
		if ix2 and (not x1 or mathv.lenSq(ix2-segX1, iy2-segY1) < mathv.lenSq(x1-segX1, y1-segY1)) then x1,y1 = ix2,iy2 end
		if ix3 and (not x1 or mathv.lenSq(ix3-segX1, iy3-segY1) < mathv.lenSq(x1-segX1, y1-segY1)) then x1,y1 = ix3,iy3 end
		if ix4 and (not x1 or mathv.lenSq(ix4-segX1, iy4-segY1) < mathv.lenSq(x1-segX1, y1-segY1)) then x1,y1 = ix4,iy4 end
	end

	local x2, y2 = nil
	if isPointIntersectingAabb(segX2,segY2, aabbX,aabbY, w,h) then
		x2, y2 = segX2, segY2
	else
		local ix1, iy1 = getSegmentSegmentIntersection(segX2,segY2, segX1,segY1, ax,ay, bx,by)
		local ix2, iy2 = getSegmentSegmentIntersection(segX2,segY2, segX1,segY1, bx,by, cx,cy)
		local ix3, iy3 = getSegmentSegmentIntersection(segX2,segY2, segX1,segY1, cx,cy, dx,dy)
		local ix4, iy4 = getSegmentSegmentIntersection(segX2,segY2, segX1,segY1, dx,dy, ax,ay)
		x2, y2 = ix1, iy1
		if ix2 and (not x2 or mathv.lenSq(ix2-segX2, iy2-segY2) < mathv.lenSq(x2-segX2, y2-segY2)) then x2,y2 = ix2,iy2 end
		if ix3 and (not x2 or mathv.lenSq(ix3-segX2, iy3-segY2) < mathv.lenSq(x2-segX2, y2-segY2)) then x2,y2 = ix3,iy3 end
		if ix4 and (not x2 or mathv.lenSq(ix4-segX2, iy4-segY2) < mathv.lenSq(x2-segX2, y2-segY2)) then x2,y2 = ix4,iy4 end
	end

	return x1,y1, x2,y2
end
math.getSegmentAabbIntersections = getSegmentAabbIntersections

-- bool = isSegmentIntersectingCircle( circleCenter, circleRadius, segmentPoint1, segmentPoint2 )
function isSegmentIntersectingCircle(circleX,circleY, radius, segX1,segY1, segX2,segY2)
	local x, y = pointOnSegmentClosestToPoint(segX1,segY1, segX2,segY2, circleX,circleY)
	return mathv.lenSq(circleX-x, circleY-y) < radius*radius
end
math.isSegmentIntersectingCircle = isSegmentIntersectingCircle
-- point1, point2 = getSegmentCircleIntersections( circleCenter, circleRadius, segmentPoint1, segmentPoint2 )
function getSegmentCircleIntersections(circleX,circleY, radius, segX1,segY1, segX2,segY2)
	-- http://mathworld.wolfram.com/Circle-LineIntersection.html
	local dx = segX2-segX1
	local dy = segY2-segY1
	-- local dr = sqrt(dx*dx+dy*dy)
	local D = segX1*segY2-segX2*segY1

	local r_2  = radius*radius
	local dr_2 = dx*dx+dy*dy--dr*dr
	local D_2  = D*D

	local Ddx  = D*dx
	local Ddy  = D*dy
	local rdrD = sqrt(r_2*dr_2-D_2)

	local dySign = sign(dy)

	local xProd = dySign  * rdrD * dx
	local yProd = abs(dy) * rdrD

	local dr_2_inv = 1/dr_2 -- Not sure if faster than just doing `/dr_2` four times below...

	local ax = ( Ddy-xProd)*dr_2_inv
	local bx = ( Ddy+xProd)*dr_2_inv
	local ay = (-Ddx-yProd)*dr_2_inv
	local by = (-Ddx+yProd)*dr_2_inv

	-- Make sure A is closest to segmentPoint1.
	if dySign < 0 then
		ax, bx = bx, ax
		ay, by = by, ay
	end

	-- Clamp points inside segment.
	if mathv.lenSq(segX1-circleX,segY1-circleY) < r_2 then
		ax = segX1
		ay = segY1
	end
	if mathv.lenSq(segX2-circleX,segY2-circleY) < r_2 then
		bx = segX2
		by = segY2
	end

	-- local discriminant = r_2*dr_2-D_2
	-- discriminant < 0: no intersection
	-- discriminant = 0: tangent
	-- discriminant > 0: intersection

	return ax,ay, bx,by
end
math.getSegmentCircleIntersections = getSegmentCircleIntersections

-- bool = isSegmentIntersectingSegment( aSegmentPoint1, aSegmentPoint2, bSegmentPoint1, bSegmentPoint2 )
function isSegmentIntersectingSegment(ax1,ay1, ax2,ay2, bx1,by1, bx2,by2)
	return
		getSideOfLine(ax1,ay1, ax2,ay2, bx1,by1) ~= getSideOfLine(ax1,ay1, ax2,ay2, bx2,by2) and
		getSideOfLine(bx1,by1, bx2,by2, ax1,ay1) ~= getSideOfLine(bx1,by1, bx2,by2, ax2,ay2)
end
math.isSegmentIntersectingSegment = isSegmentIntersectingSegment
-- point = getSegmentSegmentIntersection( aSegmentPoint1, aSegmentPoint2, bSegmentPoint1, bSegmentPoint2 )
function getSegmentSegmentIntersection(ax1,ay1, ax2,ay2, bx1,by1, bx2,by2)
	-- https://stackoverflow.com/a/1968345
	local dx1 = ax2-ax1
	local dy1 = ay2-ay1
	local dx2 = bx2-bx1
	local dy2 = by2-by1

	local det = -dx2*dy1 + dx1*dy2
	if det == 0 then  return nil  end -- Parallel lines.

	local s = (-dy1*(ax1-bx1) + dx1*(ay1-by1)) / det
	local t = ( dx2*(ay1-by1) - dy2*(ax1-bx1)) / det

	if s >= 0 and s <= 1 and t >= 0 and t <= 1 then
		return ax1+t*dx1, ay1+t*dy1
	end

	return nil -- No intersection.
end
math.getSegmentSegmentIntersection = getSegmentSegmentIntersection

-- bool = isSegmentIntersectingRectangle( segmentPoint1, segmentPoint2, rectCenter, rectSize, rectAngle )
function isSegmentIntersectingRectangle(segX1,segY1, segX2,segY2, rectX,rectY, w,h, angle)
	local segInRectSpaceX1, segInRectSpaceY1 = mathv.rotate(segX1-rectX, segY1-rectY, -angle)
	local segInRectSpaceX2, segInRectSpaceY2 = mathv.rotate(segX2-rectX, segY2-rectY, -angle)
	return isSegmentIntersectingAabb(segInRectSpaceX1,segInRectSpaceY1, segInRectSpaceX2,segInRectSpaceY2, 0,0, w,h)
end
math.isSegmentIntersectingRectangle = isSegmentIntersectingRectangle
-- point1, point2 = getSegmentRectangleIntersections( segmentPoint1, segmentPoint2, rectCenter, rectSize, rectAngle )
function getSegmentRectangleIntersections(segX1,segY1, segX2,segY2, rectX,rectY, w,h, angle)
	local segInRectSpaceX1, segInRectSpaceY1 = mathv.rotate(segX1-rectX, segY1-rectY, -angle)
	local segInRectSpaceX2, segInRectSpaceY2 = mathv.rotate(segX2-rectX, segY2-rectY, -angle)
	local x1,y1, x2,y2 = getSegmentAabbIntersections(segInRectSpaceX1,segInRectSpaceY1, segInRectSpaceX2,segInRectSpaceY2, 0,0, w,h)
	if x1 then
		x1, y1 = mathv.rotate(x1, y1, angle)
		x1, y1 = rectX+x1, rectY+y1
	end
	if x2 then
		x2, y2 = mathv.rotate(x2, y2, angle)
		x2, y2 = rectX+x2, rectY+y2
	end
	return x1,y1, x2,y2
end
math.getSegmentRectangleIntersections = getSegmentRectangleIntersections

-- bool = isSegmentIntersectingTriangle( segmentPoint1, segmentPoint2, trianglePoint1, trianglePoint2, trianglePoint3 )
function isSegmentIntersectingTriangle(segX1,segY1, segX2,segY2, triX1,triY1, triX2,triY2, triX3,triY3)
	return
		isPointIntersectingTriangle (segX1,segY1, triX1,triY1, triX2,triY2, triX3,triY3) or
		isPointIntersectingTriangle (segX2,segY2, triX1,triY1, triX2,triY2, triX3,triY3) or
		isSegmentIntersectingSegment(segX1,segY1, segX2,segY2, triX1,triY1, triX2,triY2) or
		isSegmentIntersectingSegment(segX1,segY1, segX2,segY2, triX2,triY2, triX3,triY3) or
		isSegmentIntersectingSegment(segX1,segY1, segX2,segY2, triX3,triY3, triX1,triY1)
end
math.isSegmentIntersectingTriangle = isSegmentIntersectingTriangle
-- point1, point2 = getSegmentTriangleIntersections( segmentPoint1, segmentPoint2, trianglePoint1, trianglePoint2, trianglePoint3 )
function getSegmentTriangleIntersections(segX1,segY1, segX2,segY2, triX1,triY1, triX2,triY2, triX3,triY3)
	local x1, y1 = nil
	if isPointIntersectingTriangle(segX1,segY1, triX1,triY1, triX2,triY2, triX3,triY3) then
		x1, y1 = segX1, segY1
	else
		local ix1, iy1 = getSegmentSegmentIntersection(segX1,segY1, segX2,segY2, triX1,triY1, triX2,triY2)
		local ix2, iy2 = getSegmentSegmentIntersection(segX1,segY1, segX2,segY2, triX2,triY2, triX3,triY3)
		local ix3, iy3 = getSegmentSegmentIntersection(segX1,segY1, segX2,segY2, triX3,triY3, triX1,triY1)
		x1, y1 = ix1, iy1
		if ix2 and (not x1 or mathv.lenSq(ix2-segX1, iy2-segY1) < mathv.lenSq(x1-segX1, y1-segY1)) then x1,y1 = ix2,iy2 end
		if ix3 and (not x1 or mathv.lenSq(ix3-segX1, iy3-segY1) < mathv.lenSq(x1-segX1, y1-segY1)) then x1,y1 = ix3,iy3 end
	end

	local x2, y2 = nil
	if isPointIntersectingTriangle(segX2,segY2, triX1,triY1, triX2,triY2, triX3,triY3) then
		x2, y2 = segX2, segY2
	else
		local ix1, iy1 = getSegmentSegmentIntersection(segX2,segY2, segX1,segY1, triX1,triY1, triX2,triY2)
		local ix2, iy2 = getSegmentSegmentIntersection(segX2,segY2, segX1,segY1, triX2,triY2, triX3,triY3)
		local ix3, iy3 = getSegmentSegmentIntersection(segX2,segY2, segX1,segY1, triX3,triY3, triX1,triY1)
		x2, y2 = ix1, iy1
		if ix2 and (not x2 or mathv.lenSq(ix2-segX2, iy2-segY2) < mathv.lenSq(x2-segX2, y2-segY2)) then x2,y2 = ix2,iy2 end
		if ix3 and (not x2 or mathv.lenSq(ix3-segX2, iy3-segY2) < mathv.lenSq(x2-segX2, y2-segY2)) then x2,y2 = ix3,iy3 end
	end

	return x1,y1, x2,y2
end
math.getSegmentTriangleIntersections = getSegmentTriangleIntersections

-- bool = isSegmentIntersectingPolygon( segmentPoint1, segmentPoint2, polygonPoint1, ... )
-- Note: The polygon has to be convex and the points must be ordered clockwise!
function isSegmentIntersectingPolygon(segX1,segY1, segX2,segY2, ...)
	if isPointIntersectingPolygon(segX1,segY1, ...) then  return true  end
	if isPointIntersectingPolygon(segX2,segY2, ...) then  return true  end

	local argCount = select("#", ...)
	for i = 1, argCount, 2 do
		local polyX1, polyY1, polyX2, polyY2 = getPolygonSegment(i, argCount, ...)
		if isSegmentIntersectingSegment(segX1,segY1, segX2,segY2, polyX1,polyY1, polyX2,polyY2) then  return true  end
	end

	return false
end
math.isSegmentIntersectingPolygon = isSegmentIntersectingPolygon
-- point1, point2 = getSegmentPolygonIntersections( segmentPoint1, segmentPoint2, polygonPoint1, ... )
-- Note: The polygon has to be convex and the points must be ordered clockwise!
function getSegmentPolygonIntersections(segX1,segY1, segX2,segY2, ...)
	local argCount = select("#", ...)

	local x1, y1 = nil
	if isPointIntersectingPolygon(segX1,segY1, ...) then
		x1, y1 = segX1, segY1
	else
		for i = 1, argCount, 2 do
			local polyX1, polyY1, polyX2, polyY2 = getPolygonSegment(i, argCount, ...)
			local ix, iy = getSegmentSegmentIntersection(segX1,segY1, segX2,segY2, polyX1,polyY1, polyX2,polyY2)
			if ix and (not x1 or mathv.lenSq(ix-segX1, iy-segY1) < mathv.lenSq(x1-segX1, y1-segY1)) then x1,y1 = ix,iy end
		end
	end

	local x2, y2 = nil
	if isPointIntersectingPolygon(segX2,segY2, ...) then
		x2, y2 = segX2, segY2
	else
		for i = 1, argCount, 2 do
			local polyX1, polyY1, polyX2, polyY2 = getPolygonSegment(i, argCount, ...)
			local ix, iy = getSegmentSegmentIntersection(segX2,segY2, segX1,segY1, polyX1,polyY1, polyX2,polyY2)
			if ix and (not x2 or mathv.lenSq(ix-segX2, iy-segY2) < mathv.lenSq(x2-segX2, y2-segY2)) then x2,y2 = ix,iy end
		end
	end

	return x1,y1, x2,y2
end
math.getSegmentPolygonIntersections = getSegmentPolygonIntersections

-- bool = isPointIntersectingAabb( point, aabbCenter, rectSize )
function isPointIntersectingAabb(px,py, aabbX,aabbY, w,h)
	local diffX, diffY = mathv.abs(aabbX-px, aabbY-py)
	return diffX < w*0.5 and diffY < h*0.5
end
math.isPointIntersectingAabb = isPointIntersectingAabb

-- bool = isPointIntersectingCircle( point, circleCenter, circleRadius )
function isPointIntersectingCircle(px,py, circleX,circleY, radius)
	return mathv.lenSq(circleX-px, circleY-py) < radius*radius
end
math.isPointIntersectingCircle = isPointIntersectingCircle

-- bool = isPointIntersectingLine( pointOfInterest, linePoint1, linePoint2 )
function isPointIntersectingLine(px,py, x1,y1, x2,y2) -- Untested!
	return math.abs(mathv.cross(x1,y1, x2,y2, px,py)) < mathv.getEpsilon(x1,y1, x2,y2)
end
math.isPointIntersectingLine = isPointIntersectingLine

-- bool = isPointIntersectingRectangle( point, rectCenter, rectSize, rectAngle )
function isPointIntersectingRectangle(px,py, rectX,rectY, w,h, angle)
	px, py = mathv.rotate(px-rectX, py-rectY, -angle) -- To rect space.
	return isPointIntersectingAabb(px,py, 0,0, w,h)
end
math.isPointIntersectingRectangle = isPointIntersectingRectangle

-- bool = isPointIntersectingTriangle( point, trianglePoint1, trianglePoint2, trianglePoint3 )
function isPointIntersectingTriangle(px,py, triX1,triY1, triX2,triY2, triX3,triY3)
	return
		getSideOfLine(triX1,triY1, triX2,triY2, px,py) > 0 and
		getSideOfLine(triX2,triY2, triX3,triY3, px,py) > 0 and
		getSideOfLine(triX3,triY3, triX1,triY1, px,py) > 0
end
math.isPointIntersectingTriangle = isPointIntersectingTriangle

-- bool = isPointIntersectingPolygon( point, polygonPoint1, ... )
-- Note: The polygon has to be convex and the points must be ordered clockwise!
function isPointIntersectingPolygon(px,py, ...)
	local argCount = select("#", ...)
	for i = 1, argCount, 2 do
		local polyX1, polyY1, polyX2, polyY2 = getPolygonSegment(i, argCount, ...)
		if getSideOfLine(polyX1,polyY1, polyX2,polyY2, px,py) <= 0 then  return false  end
	end
	return true
end
math.isPointIntersectingPolygon = isPointIntersectingPolygon

-- bool = isRectangleIntersectingAabb( rectCenter, rectSize, rectAngle, aabbCenter, aabbSize )
function isRectangleIntersectingAabb(rectX,rectY, rectW,rectH, angle, aabbX,aabbY, aabbW,aabbH)
	-- @Speed: We can probably do some AABB optimization here.
	return isRectangleIntersectingRectangle(rectX,rectY, rectW,rectH, angle, aabbX,aabbY, aabbW,aabbH, 0)
end
math.isRectangleIntersectingAabb = isRectangleIntersectingAabb

-- bool = isRectangleIntersectingRectangle( aRectCenter, aRectSize, aRectAngle, bRectCenter, bRectSize, bRectAngle )
-- https://gamedevelopment.tutsplus.com/tutorials/collision-detection-using-the-separating-axis-theorem--gamedev-169
do
	local function getMinMax(vectors, axisX,axisY)
		local minProj = mathv.dot(vectors[1],vectors[2], axisX,axisY)
		local maxProj = minProj

		for i = 3, #vectors, 2 do
			local proj = mathv.dot(vectors[i],vectors[i+1], axisX,axisY)
			minProj = min(minProj, proj)
			maxProj = max(maxProj, proj)
		end

		return minProj, maxProj
	end

	local aCorners = {}
	local bCorners = {}
	local normals  = {}

	function isRectangleIntersectingRectangle(ax,ay, aw,ah, aAngle, bx,by, bw,bh, bAngle)
		getRectCorners(ax,ay, aw,ah, aAngle, aCorners)
		getRectCorners(bx,by, bw,bh, bAngle, bCorners)

		getRectNormals(aAngle, normals)
		for i = 1, 8, 2 do
			local normalX, normalY = normals[i], normals[i+1]
			local minProj1, maxProj1 = getMinMax(aCorners, normalX, normalY)
			local minProj2, maxProj2 = getMinMax(bCorners, normalX, normalY)
			if maxProj1 < minProj2 or maxProj2 < minProj1 then  return false  end
		end

		getRectNormals(bAngle, normals)
		for i = 1, 8, 2 do
			local normalX, normalY = normals[i], normals[i+1]
			local minProj1, maxProj1 = getMinMax(aCorners, normalX, normalY)
			local minProj2, maxProj2 = getMinMax(bCorners, normalX, normalY)
			if maxProj1 < minProj2 or maxProj2 < minProj1 then  return false  end
		end

		return true
	end
	math.isRectangleIntersectingRectangle = isRectangleIntersectingRectangle

end



--==============================================================
--= Interactions ===============================================
--==============================================================



-- velocity = bounce( velocity, surfaceNormal [, elasticity=1, friction=0 ] )
-- https://stackoverflow.com/a/573206
function bounce(vx,vy, surfNormX,surfNormY, elasticity, friction)

	-- vx, vy = vx, vy -- v
	surfNormX, surfNormY = mathv.normalize(surfNormX, surfNormY) -- n

	local vTowardsSurfX, vTowardsSurfY = mathv.mul(surfNormX, surfNormY, mathv.dot(vx, vy, surfNormX, surfNormY)) -- u
	local vAlongSurfX,   vAlongSurfY   = vx-vTowardsSurfX, vy-vTowardsSurfY -- w

	friction   = 1-(friction or 0) -- f
	elasticity = (elasticity or 1) -- r

	return
		friction*vAlongSurfX - elasticity*vTowardsSurfX,
		friction*vAlongSurfY - elasticity*vTowardsSurfY
end
math.bounce = bounce



--==============================================================
--= 3D =========================================================
--==============================================================



-- distance = distance( point1, point2 )
function math3d.distance(x1,y1,z1, x2,y2,z2)
	return sqrt(math3d.distanceSq(x1,y1,z1, x2,y2,z2))
end

-- distanceSquared = distanceSq( point1, point2 )
function math3d.distanceSq(x1,y1,z1, x2,y2,z2)
	local dx = x2-x1
	local dy = y2-y1
	local dz = z2-z1
	return dx*dx+dy*dy+dz*dz
end

-- distance = taxicabDistance( point1, point2 )
function math3d.taxicabDistance(x1,y1,z1, x2,y2,z2)
	return abs(x2-x1)+abs(y2-y1)+abs(z2-z1)
end



-- vector = cross( vectorA, vectorB )
function math3d.cross(x1,y1,z1, x2,y2,z2)
	return y1*z2-z1*y2, z1*x2-x1*z2, x1*y2-y1*x2
end



--==============================================================
--==============================================================
--==============================================================

return math
