--[[============================================================
--=
--=  Filesystem
--=
--=-------------------------------------------------------------
--=
--=  Art Command
--=  by Marcus 'ReFreezed' Thunström
--=
--==============================================================

	getAppDirectory, getSaveDirectory, getCwd
	getDirectoryItems
	initFilesystem
	normalizePath, makePathAbsolute
	readFile, readTextFile, writeFile, writeTextFile, getFileModificationTime

--============================================================]]

local CDEFS = [[//C
	bool         PHYSFS_mount         (const char *dir, const char *mountPoint, bool appendToPath);
	bool         PHYSFS_unmount       (const char *dir);
	char **      PHYSFS_getSearchPath (void);
	const char * PHYSFS_getWriteDir   (void);
	bool         PHYSFS_setWriteDir   (const char *dir);
	void         PHYSFS_freeList      (void *listVar);
]]

local defaultSearchPaths, defaultWriteDirectory
local currentSearchPaths    = nil
local currentWriteDirectory = nil
local loveLib



local function getSearchPaths()
	if not currentSearchPaths then
		local cPaths = loveLib.PHYSFS_getSearchPath()
		assert(cPaths ~= nil)

		local paths = {}

		for i = 0, 999 do
			if cPaths[i] == nil then  break  end
			table.insert(paths, ffi.string(cPaths[i]))
		end

		loveLib.PHYSFS_freeList(cPaths)
		currentSearchPaths = paths
	end

	return {unpack(currentSearchPaths)}
end

-- directory|nil = getWriteDirectory( )
local function getWriteDirectory()
	if not currentWriteDirectory then
		local cDir            = loveLib.PHYSFS_getWriteDir()
		currentWriteDirectory = (cDir ~= nil) and ffi.string(cDir) or nil
	end
	return currentWriteDirectory
end

-- success = mountSearchPath( directory, mountPoint )
local function mountSearchPath(dir, mountPoint)
	if indexOf(currentSearchPaths, dir)                 then  return true   end
	if not loveLib.PHYSFS_mount(dir, mountPoint, false) then  return false  end
	table.insert(currentSearchPaths, dir)
	return true
end

-- success = setWriteDirectory( directory )
local function setWriteDirectory(dir)
	if dir == currentWriteDirectory        then  return true   end
	if not loveLib.PHYSFS_setWriteDir(dir) then  return false  end
	currentWriteDirectory = dir
	return true
end



function _G.initFilesystem()
	LF.write("_", "") -- Make sure LÖVE has initted filesystem stuff.
	LF.remove("_")

	ffi.cdef(CDEFS)

	if ffi.os == "Windows" then
		local ok, libOrErr = pcall(ffi.load, "love")
		if not ok then  error("[FFI] Failed loading 'love' library. ("..libOrErr..")")  end
		loveLib = libOrErr
	else
		loveLib = ffi.C -- Should we try ffi.load() first and fall back to this?
	end

	defaultSearchPaths    = getSearchPaths()
	defaultWriteDirectory = getWriteDirectory() or error("No write directory available.")

	-- for i, dir in ipairs(defaultSearchPaths) do  print("read"..i, dir)  end
	-- print("write", defaultWriteDirectory)
end



function _G.getAppDirectory()
	--
	-- So annoying to get the program's directory, LÖVE!
	--
	-- Unpacked:
	--   isFused              false
	--   sourceBaseDirectory  C:/Programs
	--   source               C:/Programs/MyApp
	--
	-- .love:
	--   isFused              false
	--   sourceBaseDirectory  C:/Programs/MyApp
	--   source               C:/Programs/MyApp/MyApp.love
	--
	-- .exe (and I assume .app):
	--   isFused              true
	--   sourceBaseDirectory  C:\Programs\MyApp
	--   source               C:\Programs\MyApp\MyApp.exe
	--
	local dir = (
		LF.isFused()                    and LF.getSourceBaseDirectory()
		or LF.getSource():find"%.love$" and LF.getSourceBaseDirectory() -- Not very robust, but what're ya gonna do...
		or LF.getSource()
	)
	return normalizePath(dir)
end

function _G.getSaveDirectory()
	return normalizePath(LF.getSaveDirectory())
end

function _G.getCwd()
	return normalizePath(LF.getWorkingDirectory())
end



-- platformIndependentPath|nil = toPlatformIndependentPath( path )
-- platformIndependentPath will always resolve to an absolute path.
local function toPlatformIndependentPath(path)
	-- @Incomplete: Handle "." and ".." segments.
	-- @Robustness: Proper parsing!

	-- Make absolute.
	if not (path:find"^/" or path:find"^%a:") then -- Note: We don't support "~/foo".
		path = getCwd() .. "/" .. path
	end

	-- Unix-ify drive letter.
	path = path:gsub("^(%a):/?", function(drive)
		return drive:upper() .. "/"
	end)

	-- Make platform independent (and relative).
	path = path:gsub("^/", "")
	if path == "" then  return nil  end

	return path
end

local function toPlatformDependentPath(piPath)
	return (piPath == "" and "/") or (love.system.getOS() == "Windows" and piPath:gsub("^%a", "%1:")) or "/"..piPath
end



-- local function getDirectory(piPath)
-- 	return piPath:match"^(.*)/[^/]+$" or ""
-- end



-- success = prepareToRead( platformIndependentPath )
local function prepareToRead(piPath)
	if love.system.getOS() == "Windows" then
		local drive = piPath:match"^(%a)/"
		return drive ~= nil and mountSearchPath(drive..":/", drive)
	else
		return mountSearchPath("/", "/")
	end
end

-- success = prepareToWrite( platformIndependentPath )
local function prepareToWrite(piPath)
	if love.system.getOS() == "Windows" then
		local drive = piPath:match"^(%a)/"
		return drive ~= nil and setWriteDirectory(drive..":/", drive)
	else
		return setWriteDirectory("/", "/")
	end
end

local function _readFile(isText, isLocal, path)
	local piPath

	if isLocal then
		piPath = path
	else
		piPath = toPlatformIndependentPath(path)
		if not (piPath and prepareToRead(piPath)) then
			return nil, "Could not access '"..path.."'."
		end
	end

	local s, err = LF.read(piPath)
	if not s then  return nil, err  end

	return isText and s:gsub("\r\n", "\n") or s
end

local function _writeFile(isText, isLocal, path, s)
	local piPath

	if isLocal then
		if not setWriteDirectory(defaultWriteDirectory) then
			return false, "Could not access '"..defaultWriteDirectory.."'."
		end
		piPath = path
	else
		piPath = toPlatformIndependentPath(path)
		if not (piPath and prepareToWrite(piPath)) then
			return false, "Could not access '"..path.."'."
		end
	end

	if isText and love.system.getOS() == "Windows" then
		s = s:gsub("\r?\n", "\r\n")
	end

	if not isLocal and love.system.getOS() == "Windows" then
		return LF.write(piPath:gsub("^%a/", ""), s) -- We cannot include the drive letter in the path as there's no concept of a mountPoint like when reading.
	else
		return LF.write(piPath, s)
	end
end

-- contents|nil, error = readFile    ( isLocal, path )
-- contents|nil, error = readTextFile( isLocal, path )
function _G.readFile    (isLocal, path)  return _readFile(false, isLocal, path)  end
function _G.readTextFile(isLocal, path)  return _readFile(true , isLocal, path)  end

-- success, error = writeFile    ( isLocal, path, contents )
-- success, error = writeTextFile( isLocal, path, contents )
function _G.writeFile    (isLocal, path, s)  return _writeFile(false, isLocal, path, s)  end
function _G.writeTextFile(isLocal, path, s)  return _writeFile(true , isLocal, path, s)  end

local info = {}

-- time|nil = getFileModificationTime( isLocal, path )
function _G.getFileModificationTime(isLocal, path)
	local piPath

	if isLocal then
		piPath = path
	else
		piPath = toPlatformIndependentPath(path)
		if not (piPath and prepareToRead(piPath)) then  return nil  end
	end

	return LF.getInfo(piPath, "file", info) and info.modtime
end



function _G.getDirectoryItems(path)
	local piPath

	if isLocal then
		piPath = path
	else
		piPath = toPlatformIndependentPath(path)
		if not (piPath and prepareToRead(piPath)) then
			return nil, "Could not access '"..path.."'."
		end
	end

	return LF.getDirectoryItems(piPath)
end



function _G.normalizePath(path)
	return (path:gsub("\\", "/"))
end

function _G.makePathAbsolute(path, baseDirPrefix)
	return (path:find"^~?/" or path:find"^%a:") and path or baseDirPrefix..path
end


