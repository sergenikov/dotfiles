hs.window.animationDuration = 0

function moveAndResize(scaleX, scaleY, posX, posY)
    -- window
    local win = hs.window.focusedWindow()
    -- frame of focused window
    local f = win:frame()
    -- screen of window
    local screen = win:screen()
    -- screen frame without dock or menu, its height.
    -- its a rectangle, hs.geometry class
    local max = screen:frame()

    -- Window size
    f.w = max.w * (scaleX or 1)
    f.h = max.h * (scaleY or 1)

    -- Window Position
    --  = size  + (width * (desired position or 0))
    f.x = max.x + (max.w * (posX or 0))
    f.y = max.y + (max.h * (posY or 0))
    win:setFrame(f)
end

-- move to top left corner
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "U", function()
  moveAndResize(1/2, 1/2, 0, 0)
end)

-- move to bottom left corner
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "N", function()
  moveAndResize(1/2, 1/2, 0, 1/2)
end)

-- move to top right corner
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "I", function()
  moveAndResize(1/2, 1/2,  1/2, 0)
end)

-- move to bottom right corner
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "M", function()
  moveAndResize(1/2, 1/2,  1/2, 1/2)
end)

-- align right 1/2 screen full height
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + (max.w / 2)
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)

-- align left 1/2 screen full height
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
    -- window
    local win = hs.window.focusedWindow()
    -- frame of focused window
    local f = win:frame()
    -- screen of window
    local screen = win:screen()
    -- screen frame without dock or menu, its height.
    -- its a rectangle, hs.geometry class
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)

-- take all screen space
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Up", function()
    -- window
    local win = hs.window.focusedWindow()
    -- frame of focused window
    local f = win:frame()
    -- screen of window
    local screen = win:screen()
    -- scren frame without dock or menu, its height.
    -- its a rectangle, hs.geometry class
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h
    win:setFrame(f)
end)

-- Replicating some i3 functionality
-- focus on window west(right)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "h", function()
--hs.hotkey.bind({"cmd", "shift"}, "h", function()
    -- window
    local win = hs.window.focusWindowWest()
end)

-- window east(left)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "l", function()
    -- window
    local win = hs.window.focusWindowEast()
end)

-- focus on window below
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "j", function()
    -- window
    local win = hs.window.focusWindowSouth()
end)

-- focus on window above
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "k", function()
    -- window
    local win = hs.window.focusWindowNorth()
end)

-- reload config
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
    hs.reload()
    hs.alert.show("Config reloaded")
end)
