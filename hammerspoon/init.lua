hs.window.animationDuration = 0

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

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
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
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)

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

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "r", function()
    hs.reload()
end)
