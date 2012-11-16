-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

require("vicious")

-- Custom libraries
require("runonce")
require("calendar2")
require("awesompd/awesompd")


-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.add_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

function gradient(color, to_color, min, max, value)
    local function color2dec(c)
        return tonumber(c:sub(2,3),16), tonumber(c:sub(4,5),16), tonumber(c:sub(6,7),16)
    end

    local factor = 0
    if (value >= max ) then 
        factor = 1  
    elseif (value > min ) then 
        factor = (value - min) / (max - min)
    end 

    local red, green, blue = color2dec(color) 
    local to_red, to_green, to_blue = color2dec(to_color) 

    red   = red   + (factor * (to_red   - red))
    green = green + (factor * (to_green - green))
    blue  = blue  + (factor * (to_blue  - blue))

    -- dec2color
    return string.format("#%02x%02x%02x", red, green, blue)
end

-- {{{ Variable definitions
--get $HOME from the environement system
home   = os.getenv("HOME")
--get XDG_CONFIG
config_dir = awful.util.getdir("config")
-- Themes define colours, icons, and wallpapers
beautiful.init( config_dir .. "/current_theme/theme.lua")
naughty.config.presets.normal.bg = beautiful.bg_focus

local scratch = require("scratch")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Run once on startup
run_once("urxvtd")
run_once("udisks-glue")
run_once("xscreensaver","xscreensaver -nosplash")
run_once("mpd")
run_once("dropbox")

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
--    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
    -- names = { "☠", "⌥", "✇", "⌤", "⍜", "✣", "⌨", "⌘", "☕" },
    tags[s] = awful.tag({ "♨", "⌨", "⚡", "✉", "☕", "❁", "☃", "☄", "☠" }, s, { layouts[2], layouts[2], layouts[10], layouts[2], layouts[1] , layouts[2] , layouts[2] , layouts[2] , layouts[1] })
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- Create a systray
mysystray = widget({ type = "systray" })

-- Custom Widgets
spacer = widget({ type = "textbox" })
spacer.text  = ' | '

-- Initialize Memory widget
memwidget = widget({ type = "textbox" })
vicious.register(memwidget, vicious.widgets.mem, '<span color="white">(Memory: <span color="green">$1% $2MB/$3MB</span>) (Swap: <span color="green">$6MB/$7MB</span>)</span>')

-- Initialize Battery widget
batwidget = widget({ type = "textbox" })
vicious.register(batwidget, vicious.widgets.bat, '<span color="white">Bat: <span color="red">$2%</span></span>', 60, "BAT0")

--RAM GRAPH usage widget
graphmemwidget = awful.widget.graph()
graphmemwidget:set_width(50)
graphmemwidget:set_background_color("#494B4F")
graphmemwidget:set_color("#FF5656")
graphmemwidget:set_gradient_colors({ "#FF5656", "#88A175", "#AECF96" })
vicious.register(graphmemwidget, vicious.widgets.mem, "$1", 3)

-- Initialize Uptime widget
uptimewidget = widget({ type = "textbox" })
vicious.register(uptimewidget, vicious.widgets.uptime, "<span color=\"white\">Uptime: <span color=\"green\">$1d $2h $3m</span></span>")

-- Initialize Disk widget
diskwidget = widget({ type = "textbox" })
--vicious.register(diskwidget, vicious.widgets.fs, "<span color=\"white\">Disk:</span><span color=\"green\"> ${/ size_gb}G / ${/ avail_gb}G</span>", 60)
vicious.register(diskwidget, vicious.widgets.fs,
function (widget, args)
  local text
  -- from light green to light red
  local color = gradient("#AECF96","#FF5656",0,100,args["{/ used_p}"])
  text = string.format("<span color='%s'>%3dGB</span>", color, args["{/ avail_gb}"])

  text = text.." / "..args["{/ size_gb}"].."GB"
  return "<span color=\"white\">Disk: "..text.."</span>"
end )

-- Initialize Net widget
netwidget = widget({ type = "textbox" })
vicious.register(netwidget, vicious.widgets.net,function (widget, args)
   local text
   -- from light green to light red
   if args["{eth0 carrier}"] > 0 then
       local coloreth0down = gradient("#AECF96","#FF5656",0,500,tonumber(args["{eth0 down_kb}"]))
       local coloreth0up = gradient("#AECF96","#FF5656",0,100,tonumber(args["{eth0 up_kb}"]))
       text = string.format("<span color='white'>eth0: <span color='%s'>rx: %4dk</span> <span color='%s'>tx: %4dk</span></span>", coloreth0down, args["{eth0 down_kb}"], coloreth0up, args["{eth0 up_kb}"])
   else text = "<span color='white'>eth0: <span color='red'>down</span></span>" end
   if (args["{wlan0 carrier}"] ~= nil) and (args["{wlan0 carrier}"] > 0) then
       local colorwlan0down = gradient("#AECF96","#FF5656",0,500,tonumber(args["{wlan0 down_kb}"]))
       local colorwlan0up = gradient("#AECF96","#FF5656",0,100,tonumber(args["{wlan0 up_kb}"]))
       text = text..string.format(" | <span color='white'>wlan0: <span color='%s'>rx: %4dk</span> <span color='%s'>tx: %4dk</span></span>", colorwlan0down, args["{wlan0 down_kb}"], colorwlan0up, args["{wlan0 up_kb}"])
   else text = text.." | <span color='white'>wlan0: <span color='red'>down</span></span>" end

   return text
end, 2)


-- Initialize CPU widget
cpuwidget = widget({ type = "textbox" })
--vicious.register(cpuwidget, vicious.widgets.cpu, "<span color=\"white\">CPU: C1: <span color=\"green\">$2%</span> C2: <span color=\"green\">$3%</span> C3: <span color=\"green\">$4%</span> C4: <span color=\"green\">$5%</span></span>")
vicious.register(cpuwidget, vicious.widgets.cpu,
function (widget, args)
  local text
  -- list only real cpu cores
  for i=1,#args do
    -- alerts, if system is stressed
--    if args[i] > 50 then
      -- from light green to light red
      local color = gradient("#AECF96","#FF5656",0,100,args[i])
      args[i] = string.format("<span color='%s'>%3d</span>", color, args[i])
--    end

    -- append to list
    if i > 2 then text = text.." C"..(i-1)..": "..args[i].."%"
    else text = "C"..(i-1)..": "..args[i].."%" end
  end

  return "<span color=\"white\">CPU: "..text.."</span>"
end )

--GraphCPU usage widget
graphcpuwidget = awful.widget.graph()
graphcpuwidget:set_width(50)
graphcpuwidget:set_background_color("#494B4F")
graphcpuwidget:set_color("#FF5656")
graphcpuwidget:set_gradient_colors({ "#FF5656", "#88A175", "#AECF96" })
vicious.register(graphcpuwidget, vicious.widgets.cpu, "$1", 3)


musicwidget = awesompd:create() -- Create awesompd widget
  musicwidget.font = "Droid Sans Mono" -- Set widget font 
  musicwidget.scrolling = true -- If true, the text in the widget will be scrolled
  musicwidget.output_size = 100 -- Set the size of widget in symbols
  musicwidget.update_interval = 10 -- Set the update interval in seconds
  -- Set the folder where icons are located (change username to your login name)
  musicwidget.path_to_icons = "/home/pfault/.config/awesome/awesompd/icons" 
  -- Set the default music format for Jamendo streams. You can change
  -- this option on the fly in awesompd itself.
  -- possible formats: awesompd.FORMAT_MP3, awesompd.FORMAT_OGG
  musicwidget.jamendo_format = awesompd.FORMAT_MP3
  -- If true, song notifications for Jamendo tracks and local tracks will also contain
  -- album cover image.
  musicwidget.show_album_cover = true
  -- Specify how big in pixels should an album cover be. Maximum value
  -- is 100.
  musicwidget.album_cover_size = 50
  -- This option is necessary if you want the album covers to be shown
  -- for your local tracks.
  musicwidget.mpd_config = "/home/pfault/.mpdconf"
  -- Specify the browser you use so awesompd can open links from
  -- Jamendo in it.
  musicwidget.browser = "luakit"
  -- Specify decorators on the left and the right side of the
  -- widget. Or just leave empty strings if you decorate the widget
  -- from outside.
  musicwidget.ldecorator = " "
  musicwidget.rdecorator = " "
  -- Set all the servers to work with (here can be any servers you use)
  musicwidget.servers = {
     { server = "localhost",
          port = 6600 },
     { server = "192.168.0.72",
          port = 6600 } }
  -- Set the buttons of the widget
  musicwidget:register_buttons({ { "", awesompd.MOUSE_LEFT, musicwidget:command_toggle() },
                         { "Control", awesompd.MOUSE_SCROLL_UP, musicwidget:command_prev_track() },
                     { "Control", awesompd.MOUSE_SCROLL_DOWN, musicwidget:command_next_track() },
                     { "", awesompd.MOUSE_SCROLL_UP, musicwidget:command_volume_up() },
                     { "", awesompd.MOUSE_SCROLL_DOWN, musicwidget:command_volume_down() },
                     { "", awesompd.MOUSE_RIGHT, musicwidget:command_show_menu() },
                                 { "", "XF86AudioLowerVolume", musicwidget:command_volume_down() },
                                 { "", "XF86AudioRaiseVolume", musicwidget:command_volume_up() },
                                 { modkey, "Pause", musicwidget:command_playpause() } })
  musicwidget:run() -- After all configuration is done, run the widget

calendar2.addCalendarToWidget(mytextclock, "<span color='green'>%s</span>")

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", height = "15", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        mytextclock,
        s == 1 and mysystray or nil,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end

mystatusbox = awful.wibox({ position = "bottom", height = "15", screen = 1 })
mystatusbox.widgets = {
    {
        cpuwidget,
        spacer,
        graphcpuwidget.widget,
        spacer,
        batwidget,
        spacer,
        memwidget,
        spacer,
        graphmemwidget.widget,
        spacer,
        diskwidget,
        spacer,
        netwidget,
        spacer,
        musicwidget.widget,
        spacer,
        layout = awful.widget.layout.horizontal.leftright
    },
    uptimewidget,
    spacer,
    layout = awful.widget.layout.horizontal.rightleft
}
-- }}}
--
-- Get active outputs
local function outputs()
   local outputs = {}
   local xrandr = io.popen("xrandr -q")
   if xrandr then
      for line in xrandr:lines() do
     output = line:match("^([%w-]+) connected ")
     if output then
        outputs[#outputs + 1] = output
     end
      end
      xrandr:close()
   end

   return outputs
end

local function arrange(out)
   -- We need to enumerate all the way to combinate output. We assume
   -- we want only an horizontal layout.
   local choices  = {}
   local previous = { {} }
   for i = 1, #out do
      -- Find all permutation of length `i`: we take the permutation
      -- of length `i-1` and for each of them, we create new
      -- permutations by adding each output at the end of it if it is
      -- not already present.
      local new = {}
      for _, p in pairs(previous) do
     for _, o in pairs(out) do
        if not awful.util.table.hasitem(p, o) then
           new[#new + 1] = awful.util.table.join(p, {o})
        end
     end
      end
      choices = awful.util.table.join(choices, new)
      previous = new
   end

   return choices
end

-- Build available choices
local function menu()
   local menu = {}
   local out = outputs()
   local choices = arrange(out)

   for _, choice in pairs(choices) do
      local cmd = "xrandr"
      -- Enabled outputs
      for i, o in pairs(choice) do
     cmd = cmd .. " --output " .. o .. " --auto"
     if i > 1 then
        cmd = cmd .. " --right-of " .. choice[i-1]
     end
      end
      -- Disabled outputs
      for _, o in pairs(out) do
     if not awful.util.table.hasitem(choice, o) then
        cmd = cmd .. " --output " .. o .. " --off"
     end
      end

      local label = ""
      if #choice == 1 then
     label = 'Only <span weight="bold">' .. choice[1] .. '</span>'
      else
     for i, o in pairs(choice) do
        if i > 1 then label = label .. " + " end
        label = label .. '<span weight="bold">' .. o .. '</span>'
     end
      end

      menu[#menu + 1] = { label,
              cmd,
                          "/usr/share/icons/Tango/32x32/devices/display.png"}
   end

   return menu
end

-- Display xrandr notifications from choices
local state = { iterator = nil,
        timer = nil,
        cid = nil }
local function xrandr()
   -- Stop any previous timer
   if state.timer then
      state.timer:stop()
      state.timer = nil
   end

   -- Build the list of choices
   if not state.iterator then
      state.iterator = awful.util.table.cycle(menu(),
                    function() return true end)
   end

   -- Select one and display the appropriate notification
   local next  = state.iterator()
   local label, action, icon
   if not next then
      label, icon = "Keep the current configuration", "/usr/share/icons/Tango/32x32/devices/display.png"
      state.iterator = nil
   else
      label, action, icon = unpack(next)
   end
   state.cid = naughty.notify({ text = label,
                icon = icon,
                timeout = 4,
                screen = mouse.screen, -- Important, not all screens may be visible
                font = "Free Sans 18",
                replaces_id = state.cid }).id

   -- Setup the timer
   state.timer = timer { timeout = 4 }
   state.timer:add_signal("timeout",
              function()
                 state.timer:stop()
                 state.timer = nil
                 state.iterator = nil
                 if action then
                awful.util.spawn(action, false)
                 end
              end)
   state.timer:start()
end


-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

-- Escape from keyboard focus trap (eg Flash plugin in Firefox)
    awful.key({ modkey, "Control" }, "Escape", function ()
         awful.util.spawn("xdotool getactivewindow mousemove --window %1 0 0 click --clearmodifiers 2")
    end),

    awful.key({ modkey, "Control" }, "u", function () awful.util.spawn("xscreensaver-command -lock") end),
    awful.key({}, "XF86Display", xrandr),
    awful.key({ modkey, "Control" }, "p", xrandr),
    awful.key({ modkey,           }, "`", function () scratch.drop(terminal, "top", "center", 1, 0.3) end),


    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)
)


clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

musicwidget:append_global_keys()
-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "URxvt" },
      properties = { floating = false } },
    { rule = { name = "irssi" },
      properties = { tag = tags[1][9] } },
    { rule = { class = "MPlayer" },
      properties = { floating = true, tag = tags[1][5], switchtotag = tags[1][5], focus = true } },
    { rule = { class = "libreoffice-" },
      properties = { floating = false, tag = tags[1][7], switchtotag = tags[1][7], focus = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "Gimp" },
      properties = { floating = true, tag = tags[1][6] } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    { rule_any = { class = { "Firefox", "luakit" } },
      properties = { tag = tags[1][3] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
