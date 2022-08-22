function love.conf(t)
	t.identity = "Art Command"
	t.version  = "11.4"

	t.window.title      = "Art Command"
	t.window.icon       = nil -- @Incomplete
	t.window.width      = 800
	t.window.height     = 600
	t.window.borderless = false
	t.window.resizable  = false
	t.window.minwidth   = 1
	t.window.minheight  = 1
	t.window.display    = 2

	t.modules.audio    = false
	t.modules.data     = true
	t.modules.event    = true
	t.modules.font     = true
	t.modules.graphics = true
	t.modules.image    = true
	t.modules.joystick = false
	t.modules.keyboard = true
	t.modules.math     = true
	t.modules.mouse    = true
	t.modules.physics  = false
	t.modules.sound    = false
	t.modules.system   = true
	t.modules.thread   = false
	t.modules.timer    = true
	t.modules.touch    = false
	t.modules.video    = false
	t.modules.window   = true
end