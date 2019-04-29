hs.window.animationDuration = 0

-- prefix = {"cmd", "alt", "ctrl"}
prefix = {"cmd", "ctrl"}

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
hs.hotkey.bind(prefix, "U", function()
  moveAndResize(1/2, 1/2, 0, 0)
end)

-- move to bottom left corner
hs.hotkey.bind(prefix, "N", function()
  moveAndResize(1/2, 1/2, 0, 1/2)
end)

-- move to top right corner
hs.hotkey.bind(prefix, "I", function()
  moveAndResize(1/2, 1/2,  1/2, 0)
end)

-- move to bottom right corner
hs.hotkey.bind(prefix, "M", function()
  moveAndResize(1/2, 1/2,  1/2, 1/2)
end)

-- align right 1/2 screen full height
hs.hotkey.bind(prefix, "Right", function()
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
hs.hotkey.bind(prefix, "Left", function()
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
hs.hotkey.bind(prefix, "Up", function()
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
hs.hotkey.bind(prefix, "h", function()
    -- window
    -- local win = hs.window.focusWindowWest()
    if hs.window.focusedWindow() then
        local win = hs.window.focusedWindow():focusWindowWest()
    else
      hs.alert.show("Config reloaded")	 
    end
end)

-- window east(left)
hs.hotkey.bind(prefix, "l", function()
    -- window
    if hs.window.focusedWindow() then
        local win = hs.window.focusedWindow():focusWindowEast()
    else
      hs.alert.show("No active window")
    end
end)

-- focus on window below
hs.hotkey.bind(prefix, "j", function()
    -- window
    local win = hs.window.focusedWindow():focusWindowSouth()
end)

-- focus on window above
hs.hotkey.bind(prefix, "k", function()
    -- window
    local win = hs.window.focusedWindow():focusWindowNorth()
end)

-- reload config
hs.hotkey.bind(prefix, "R", function()
    hs.reload()
    hs.alert.show("Config reloaded")
end)
