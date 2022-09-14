--
-- Polygon triangulation - Ear clipping method
--
-- Original code by 2dengine
-- https://2dengine.com/?p=polygons
--

local function reverseArray(arr)
	local len = #arr

	for i1 = 1, len/2 do
		local i2         = len - i1 + 1
		arr[i1], arr[i2] = arr[i2], arr[i1]
	end
end

-- Finds twice the signed area of a polygon.
local function getPolygonSignedArea(poly)
	local area = 0
	local len  = #poly
	local a    = poly[len]

	for i = 1, len do
		local b = poly[i]
		area    = area + (b.x+a.x)*(b.y-a.y)
		a       = b
	end

	return area
end

-- Checks if the winding of a polygon is counter-clockwise.
local function isPolygonCcw(poly)
	return getPolygonSignedArea(poly) > 0
end

-- Finds twice the signed area of a triangle.
local function getTriangleSignedArea(p1,p2,p3)
	return (p1.x-p3.x)*(p2.y-p3.y) - (p1.y-p3.y)*(p2.x-p3.x)
end
 
local function isPointInTriangle(p, p1,p2,p3)
	local p1x, p1y = p1.x-p.x, p1.y-p.y
	local p2x, p2y = p2.x-p.x, p2.y-p.y
	local p3x, p3y = p3.x-p.x, p3.y-p.y

	local ab  = p1x*p2y - p1y*p2x
	local bc  = p2x*p3y - p2y*p3x
	local sab = ab < 0

	if sab ~= (bc < 0) then
		return false
	end

	local ca = p3x*p1y - p3y*p1x
	return sab == (ca < 0)
end
 
local left  = {}
local right = {}

-- Checks if the vertex is an "ear" or "mouth".
local function isEar(p1,p2,p3)
	if getTriangleSignedArea(p1,p2,p3) < 0 then  return false  end

	local q2 = right[p3]

	repeat
		local q1, q3 = left[q2], right[q2]

		if getTriangleSignedArea(q1,q2,q3) <= 0 and isPointInTriangle(q2, p1,p2,p3) then
			return false
		end

		q2 = q3
	until q2 == p1

	return true
end
 
local function triangulatePolygon(poly)
	if not isPolygonCcw(poly) then
		reverseArray(poly) -- Reverse the vertex winding.
	end

	table.clear(left)
	table.clear(right)

	for i = 1, #poly do
		local v           = poly[i]
		left[v], right[v] = poly[i-1], poly[i+1]
	end

	local first, last        = poly[1], poly[#poly]
	left[first], right[last] = last, first

	local out   = {}
	local nskip = 0
	local p2    = first

	while poly[3] and nskip <= #poly do
		local p1, p3 = left[p2], right[p2]

		if poly[4] and isEar(p1,p2,p3) then
			table.insert(out, {p1,p2,p3})

			left[p3], right[p1] = p1 , p3
			left[p2], right[p2] = nil, nil

			nskip = 0
			p2    = p1

		else
			nskip = nskip + 1
			p2    = p3
		end
	end

	return out
end

return triangulatePolygon
