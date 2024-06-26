-- HANDLE SCROLLING WITH MOUSE BUTTON PRESSED
local scrollMouseButton = 2
local deferred = false

overrideOtherMouseDown = hs.eventtap.new({ hs.eventtap.event.types.otherMouseDown }, function(e)
    -- print("down")
    local pressedMouseButton = e:getProperty(hs.eventtap.event.properties['mouseEventButtonNumber'])
    if scrollMouseButton == pressedMouseButton 
        then 
            deferred = true
            return true
        end
end)

overrideOtherMouseUp = hs.eventtap.new({ hs.eventtap.event.types.otherMouseUp }, function(e)
     -- print("up")
    local pressedMouseButton = e:getProperty(hs.eventtap.event.properties['mouseEventButtonNumber'])
    if scrollMouseButton == pressedMouseButton 
        then 
            if (deferred) then
                overrideOtherMouseDown:stop()
                overrideOtherMouseUp:stop()
                hs.eventtap.otherClick(e:location(), 0, pressedMouseButton)
                overrideOtherMouseDown:start()
                overrideOtherMouseUp:start()
                return true
            end
            return false
        end
        return false
end)

local oldmousepos = {}
local scrollmult = 6    -- negative multiplier makes mouse work like traditional scrollwheel

dragOtherToScroll = hs.eventtap.new({ hs.eventtap.event.types.otherMouseDragged }, function(e)
    local pressedMouseButton = e:getProperty(hs.eventtap.event.properties['mouseEventButtonNumber'])
    -- print ("pressed mouse " .. pressedMouseButton)
    if scrollMouseButton == pressedMouseButton 
        then 
            -- print("scroll");
            deferred = false
            oldmousepos = hs.mouse.getAbsolutePosition()    
            local dx = e:getProperty(hs.eventtap.event.properties['mouseEventDeltaX'])
            local dy = e:getProperty(hs.eventtap.event.properties['mouseEventDeltaY'])
            local scroll = hs.eventtap.event.newScrollEvent({dx * scrollmult, dy * scrollmult},{},'pixel')
            -- put the mouse back
            hs.mouse.setAbsolutePosition(oldmousepos)
            return true, {scroll}
        else 
            return false, {}
        end 
end)

overrideOtherMouseDown:start()
overrideOtherMouseUp:start()
dragOtherToScroll:start()

-- -- HANDLE SCROLLING WITH MOUSE BUTTON PRESSED
-- local scrollMouseButton = 3
-- local deferred = false

-- overrideOtherMouseDown = hs.eventtap.new({ hs.eventtap.event.types.otherMouseDown }, function(e)
--     -- print(hs.eventtap.event.properties['mouseEventButtonNumber'])
--     local mouseButton = e:getProperty(hs.eventtap.event.properties['mouseEventButtonNumber'])
--     if mouseButton == scrollMouseButton then 
--         deferred = true
--         return true
--     end

--     -- Re-map "button 4" to button middle mouse button (for center-click-to-open-in-new-tabs)
--     if mouseButton == 4 then
--         local point = hs.mouse.getAbsolutePosition()
--         hs.eventtap.event.newMouseEvent(hs.eventtap.event.types['middleMouseDown'], point):post()
--         return true
--     end
-- end)

-- overrideOtherMouseUp = hs.eventtap.new({ hs.eventtap.event.types.otherMouseUp }, function(e)
--     -- print(hs.eventtap.event.properties['mouseEventButtonNumber'])
--     local mouseButton = e:getProperty(hs.eventtap.event.properties['mouseEventButtonNumber'])

--     -- Re-map "button 4" to button middle mouse button (for center-click-to-open-in-new-tabs)
--     if mouseButton == 4 then
--         local point = hs.mouse.getAbsolutePosition()
--         hs.eventtap.event.newMouseEvent(hs.eventtap.event.types['middleMouseUp'], point):post()
--         return true
--     end
-- end)

-- local oldmousepos = {}
-- local scrollmult = 3	-- negative multiplier makes mouse work like traditional scrollwheel

-- dragOtherToScroll = hs.eventtap.new({ hs.eventtap.event.types.otherMouseDragged }, function(e)
--     local mouseButton = e:getProperty(hs.eventtap.event.properties['mouseEventButtonNumber'])
--     -- print ("pressed mouse " .. mouseButton)
--     -- print("scroll")
-- 	-- print(scrollMouseButton)
-- 	-- print(mouseButton)
--     if scrollMouseButton == 3 then 
--         deferred = false
--         oldmousepos = hs.mouse.getAbsolutePosition()    
--         local dx = e:getProperty(hs.eventtap.event.properties['mouseEventDeltaX'])
--         local dy = e:getProperty(hs.eventtap.event.properties['mouseEventDeltaY'])
--         local scroll = hs.eventtap.event.newScrollEvent({dx * scrollmult, dy * scrollmult},{},'pixel')
--         -- put the mouse back
--         hs.mouse.setAbsolutePosition(oldmousepos)
--         return true, {scroll}
--     else 
--         return false, {}
--     end 
-- end)

-- overrideOtherMouseDown:start()
-- overrideOtherMouseUp:start()
-- dragOtherToScroll:start()

