--[[============================================================
--=
--=  Art Command
--=  by Marcus 'ReFreezed' Thunström
--=
--=  $ love ArtCommand.love pathToArt [options]
--=
--=  Options:
--=      --out path
--=          Where to save the output to. Default is in same folder as the input path.
--=
--============================================================]]

love.filesystem.setRequirePath("?.lua;src/?.lua;lib/?.lua")
require"app"
