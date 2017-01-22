--                           --
--    PFault's Awesome 3.5   --
--                           --

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local vicious = require("vicious")
local scratch = require("scratch")
local menubar = require("menubar")
local calendar2 = require('calendar2')
local awesompd = require("awesompd/awesompd")
local lfs = require("lfs")
local hotkeys_popup = require("awful.hotkeys_popup").widget

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
    awesome.connect_signal("debug::error", function (err)
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

function run_once(prg,arg_string,pname,screen)
    if not prg then
        do return nil end
    end

    if not pname then
       pname = prg
    end

    if not arg_string then
        awful.util.spawn_with_shell("pgrep -f -u $USER -x '" .. pname .. "' || (" .. prg .. ")",screen)
    else
        awful.util.spawn_with_shell("pgrep -f -u $USER -x '" .. pname .. " ".. arg_string .."' || (" .. prg .. " " .. arg_string .. ")",screen)
    end
end

-- Run once function

--function run_once(cmd)
--  findme = cmd
--  firstspace = cmd:find(" ")
--  if firstspace then
--     findme = cmd:sub(0, firstspace-1)
--  end
--  awful.util.spawn_with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. cmd .. ")")
--end

-- autostart applications
run_once("urxvtd")
run_once("copyq")
run_once("xss-lock","-- xscreensaver-command -lock")
run_once("mopidy","-q","/usr/bin/python2 /usr/bin/mopidy")
--run_once("mpdas","-c ~/.mpdasrc")
--run_once("wicd-gtk","-t","/usr/bin/python2 -O /usr/share/wicd/gtk/wicd-client.py")
run_once("unclutter","-idle 5")
run_once("compton")
run_once("xscreensaver","-no-splash")


-- Localization

os.setlocale(os.getenv("LANG"))

-- {{{ Variable definitions

home = os.getenv("HOME")
confdir = home .. "/.config/awesome"
scriptdir = confdir .. "/scripts/"
themes = confdir .. "/themes"
active_theme = themes .. "/dust"
language = string.gsub(os.getenv("LANG"), ".utf8", "")

-- Themes define colours, icons, and wallpapers
--beautiful.init("/usr/share/awesome/themes/default/theme.lua")

beautiful.init(active_theme .. "/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor
gui_editor = "gvim"
browser = "chromium"
mail = terminal .. " -e mutt "
chat = terminal .. " -e irssi "
tasks = terminal .. " -e htop "
wifi = terminal .. " -e sudo wifi-menu "
musicplr = terminal .. " -e ncmpcpp "

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

naughty.config.presets.normal.opacity = 0.8
naughty.config.presets.low.opacity = 0.8
naughty.config.presets.critical.opacity = 0.8

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
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

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}


-- {{{ Wallpaper
--if beautiful.wallpaper then
--    for s = 1, screen.count() do
--        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
--    end
--end




-- configuration - edit to your liking
-- seed and "pop a few"
math.randomseed( os.time())
for i=1,1000 do tmp=math.random(0,1000) end
wp_index = 1
wp_timeout  = 30
wp_path = home .. "/Pictures/wallpapers/"


function string.ends(String,End)
    return End=='' or string.sub(String,-string.len(End))==End
end

-- for key,value in pairs(wp_files) do print(key,value) end

-- setup the timer
wp_timer = timer { timeout = wp_timeout }
wp_timer:connect_signal("timeout", function()

  wp_files = {}
  for file in lfs.dir(wp_path) do
    if string.ends(file,".jpg") then
      wp_files[#wp_files+1] = file
    end
  end

  -- set wallpaper to current index
  for s = 1, screen.count() do
      gears.wallpaper.maximized( wp_path .. wp_files[wp_index] , s, true)
  end

  -- stop the timer (we don't need multiple instances running at the same time)
  wp_timer:stop()

  -- get next random index
  wp_index = math.random( 1, #wp_files)

  --restart the timer
  wp_timer.timeout = wp_timeout
  wp_timer:start()
end)

-- initial start when rc.lua is first run
wp_timer:start()
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

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()
calendar2.addCalendarToWidget(mytextclock, '<span weight="bold" foreground="green"><b>%s</b></span>')

graphwidth  = 90
graphheight = 12
pctwidth    = 25
netwidth    = 40
hddwidth    = 50
mpdwidth    = 365

-- {{{ Spacers
space = wibox.widget.textbox()
space:set_text(" ")

comma = wibox.widget.textbox()
comma:set_markup(",")

pipe = wibox.widget.textbox()
pipe:set_markup("<span color='" .. beautiful.bg_em .. "'>|</span>")

tab = wibox.widget.textbox()
tab:set_text("         ")

volspace = wibox.widget.textbox()
volspace:set_text(" ")
-- }}}

function gradient(color, to_color, min, max, value)
    local function color2dec(c)
        return tonumber(c:sub(2,3),16), tonumber(c:sub(4,5),16), tonumber(c:sub(6,7),16)
    end

    function truncate(x)
        return x<0 and math.ceil(x) or math.floor(x)
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
    return string.format("#%02x%02x%02x", truncate(red), truncate(green), truncate(blue))
end



-- {{{ Widgets
-- Core 0 freq
cpufreq = wibox.widget.textbox()
vicious.register(cpufreq, vicious.widgets.cpuinf,
  function(widget, args)
    return string.format("<span color='" .. beautiful.fg_em ..
      "'>@</span>%1.1fGHz", args["{cpu0 ghz}"])
  end, 3000)

-- Initialize CPU widget
cpuwidget = wibox.widget.textbox()
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

  return "<span color='"..beautiful.fg_em.."'>CPU: "..text.."</span>"
end )

--GraphCPU usage widget
graphcpuwidget = awful.widget.graph()
graphcpuwidget:set_width(graphwidth)
graphcpuwidget:set_height(graphheight)
graphcpuwidget:set_background_color(beautiful.bg_widget)
graphcpuwidget:set_color({ type = "linear", from = { 0, 0 }, to = { 0, graphheight }, stops = { { 0, "#"..beautiful.fg_widget }, { 0.5, "#"..beautiful.fg_center_widget }, { 1, "#"..beautiful.fg_end_widget } }})
vicious.register(graphcpuwidget, vicious.widgets.cpu, "$1", 3)

--Mem widget
vicious.cache(vicious.widgets.mem)

memwidget = wibox.widget.textbox()
--vicious.register(memwidget, vicious.widgets.mem, '<span color="'..beautiful.fg_em..'">RAM: <span color="green">$2MB/$3MB</span> SWP: <span color="green">$6MB/$7MB</span></span>')
vicious.register(memwidget, vicious.widgets.mem,
function (widget, args)
  local memcolor = gradient("#AECF96","#FF5656",0,100,args[1])
  local swapcolor = gradient("#AECF96","#FF5656",0,100,args[5])
  return '<span color="'..beautiful.fg_em..'">RAM: <span color="'..memcolor..'">'..args[2]..'MB/</span>'..args[3]..'MB SWP: <span color="'..swapcolor..'">'..args[6]..'MB</span>/'..args[7]..'MB</span>'
end )

--GraphMem widget

graphmemwidget = awful.widget.graph()
graphmemwidget:set_width(graphwidth)
graphmemwidget:set_height(graphheight)
graphmemwidget:set_background_color(beautiful.bg_widget)
graphmemwidget:set_color({ type = "linear", from = { 0, 0 }, to = { 0, graphheight }, stops = { { 0, "#"..beautiful.fg_widget }, { 0.5, "#"..beautiful.fg_center_widget }, { 1, "#"..beautiful.fg_end_widget } }})
--graphmemwidget:set_color("#FF5656")
--graphmemwidget:set_stack(true)
--graphmemwidget:set_stack_colors({ "#FF5656", "#88A175", "#AECF96" })
vicious.register(graphmemwidget, vicious.widgets.mem, "$1", 3)

--FS widget
vicious.cache(vicious.widgets.fs)

-- Root used
rootfsused = wibox.widget.textbox()
vicious.register(rootfsused, vicious.widgets.fs,
  "<span color='" .. beautiful.fg_em .. "'>HDD: </span>${/ used_gb}/${/ size_gb}GB", 97)

-- Root bar
rootfsbar = awful.widget.progressbar()
--rootfsbar:set_vertical(false):set_width(hddwidth):set_height(graphheight)
rootfsbar:set_width(hddwidth)
rootfsbar:set_ticks(false):set_ticks_size(2)
rootfsbar:set_background_color(beautiful.bg_widget)
rootfsbar:set_color({ type = "linear", from = { 0, 0 }, to = { hddwidth, 0 }, stops = { { 0, "#"..beautiful.fg_end_widget }, { 0.5, "#"..beautiful.fg_center_widget }, { 1, "#"..beautiful.fg_widget } }})
vicious.register(rootfsbar, vicious.widgets.fs, "${/ used_p}", 97)

-- Root %
rootfspct = wibox.widget.textbox()
rootfspct.width = pctwidth
vicious.register(rootfspct, vicious.widgets.fs, "${/ used_p}%", 97)

--Net widget
vicious.cache(vicious.widgets.net)

-- TX graph
txgraph = awful.widget.graph()
txgraph:set_width(netwidth):set_height(graphheight)
txgraph:set_background_color(beautiful.bg_widget)
txgraph:set_color({ type = "linear", from = { 0, 0 }, to = { 0, graphheight }, stops = { { 0, "#"..beautiful.fg_widget }, { 0.5, "#"..beautiful.fg_center_widget }, { 1, "#"..beautiful.fg_end_widget } }})
vicious.register(txgraph, vicious.widgets.net, "${wlp2s0 up_kb}")

-- TX total
txwidget = wibox.widget.textbox()
vicious.register(txwidget, vicious.widgets.net,
  "<span color='" .. beautiful.fg_em .. "'>TX wlp2s0: </span>${wlp2s0 tx_mb}MB", 19)

-- TX speed
txspeed = wibox.widget.textbox()
vicious.register(txspeed, vicious.widgets.net, "${wlp2s0 up_kb}Kb", 2)

-- RX graph
rxgraph = awful.widget.graph()
rxgraph:set_width(netwidth):set_height(graphheight)
rxgraph:set_background_color(beautiful.bg_widget)
rxgraph:set_color({ type = "linear", from = { 0, 0 }, to = { 0, graphheight }, stops = { { 0, "#"..beautiful.fg_widget }, { 0.5, "#"..beautiful.fg_center_widget }, { 1, "#"..beautiful.fg_end_widget } }})
vicious.register(rxgraph, vicious.widgets.net, "${wlp2s0 down_kb}")

-- RX total
rxwidget = wibox.widget.textbox()
vicious.register(rxwidget, vicious.widgets.net,
  "<span color='" .. beautiful.fg_em .. "'>RX wlp2s0: </span>${wlp2s0 rx_mb}MB", 17)

-- RX speed
rxspeed = wibox.widget.textbox()
vicious.register(rxspeed, vicious.widgets.net, "${wlp2s0 down_kb}Kb", 2)

-- }}}

-- {{{ Weather
weather = wibox.widget.textbox()
vicious.register(weather, vicious.widgets.weather,
  "<span color='" .. beautiful.fg_em .. "'>${sky}</span> @ ${tempc}°C ${windkmh}kmh${wind}", 1501, "EDDM")
weather:buttons(awful.util.table.join(awful.button({ }, 1,
  function() vicious.force({ weather }) end)))
-- }}}

-- {{{ Volume
-- Cache
vicious.cache(vicious.widgets.volume)

-- Icon
volicon = wibox.widget.imagebox()
volicon:set_image(beautiful.widget_vol)

-- Volume %
volpct = wibox.widget.textbox()
vicious.register(volpct, vicious.widgets.volume, "$1%", nil, "Master -c 1")

-- Buttons
volicon:buttons(awful.util.table.join(
  awful.button({ }, 1, function() awful.spawn("amixer -c 1 -q set Master toggle", false) end),
  awful.button({ }, 4, function() awful.spawn("amixer -c 1 -q set Master 3%+ unmute", false) end),
  awful.button({ }, 5, function() awful.spawn("amixer -c 1 -q set Master 3%- unmute", false) end)))
volpct:buttons(volicon:buttons())
volspace:buttons(volicon:buttons())
-- }}}
--
-- {{{ awesompd

musicwidget = awesompd:create() -- Create awesompd widget
musicwidget.font = "Droid Sans Mono 10" -- Set widget font
musicwidget.scrolling = true -- If true, the text in the widget will be scrolled
musicwidget.output_size = 30 -- Set the size of widget in symbols
musicwidget.update_interval = 10 -- Set the update interval in seconds
-- Set the folder where icons are located (change username to your login name)
musicwidget.path_to_icons = home .. "/.config/awesome/awesompd/icons"
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
musicwidget.mpd_config = home .. "/.mpdconf"
-- Specify the browser you use so awesompd can open links from
-- Jamendo in it.
musicwidget.browser = "chromium"
-- Specify decorators on the left and the right side of the
-- widget. Or just leave empty strings if you decorate the widget
-- from outside.
musicwidget.ldecorator = " -=["
musicwidget.rdecorator = "]=- "
-- Set all the servers to work with (here can be any servers you use)
musicwidget.servers = {
     { server = "localhost",
          port = 6600 } }
-- Set the buttons of the widget
musicwidget:register_buttons({ { "", awesompd.MOUSE_LEFT, musicwidget:command_toggle() },
                         { "Control", awesompd.MOUSE_SCROLL_UP, musicwidget:command_prev_track() },
                     { "Control", awesompd.MOUSE_SCROLL_DOWN, musicwidget:command_next_track() },
                     { "", awesompd.MOUSE_SCROLL_UP, musicwidget:command_volume_up() },
                     { "", awesompd.MOUSE_SCROLL_DOWN, musicwidget:command_volume_down() },
                     { "", awesompd.MOUSE_MIDDLE, function() awful.util.spawn_with_shell(musicplr) end },
                     { "", awesompd.MOUSE_RIGHT, musicwidget:command_show_menu() },
                                 { "", "XF86AudioLowerVolume", musicwidget:command_volume_down() },
                                 { "", "XF86AudioRaiseVolume", musicwidget:command_volume_up() },
                                 { modkey, "Pause", musicwidget:command_playpause() } })
musicwidget:run() -- After all configuration is done, run the widget


-- {{{ Battery
-- Battery attributes
local bat_state  = ""
local bat_charge = 0
local bat_time   = 0
local blink      = true

-- Icon
baticon = wibox.widget.imagebox()
baticon:set_image(beautiful.widget_batfull)

-- Charge %
batpct = wibox.widget.textbox()
vicious.register(batpct, vicious.widgets.bat,
  function(widget, args)
    bat_state  = args[1]
    bat_charge = args[2]
    bat_time   = args[3]

    if args[1] == "-" then
      if bat_charge > 70 then
        baticon:set_image(beautiful.widget_batfull)
      elseif bat_charge > 30 then
        baticon:set_image(beautiful.widget_batmed)
      elseif bat_charge > 10 then
        baticon:set_image(beautiful.widget_batlow)
      else
        baticon:set_image(beautiful.widget_batempty)
      end
    else
      baticon:set_image(beautiful.widget_ac)
      if args[1] == "+" then
        blink = not blink
        if blink then
          baticon:set_image(beautiful.widget_acblink)
        end
      end
    end

    return args[2] .. "%"
  end, nil, "BAT0")

-- Buttons
function popup_bat()
  local state = ""
  if bat_state == "↯" then
    state = "Full"
  elseif bat_state == "↯" then
    state = "Charged"
  elseif bat_state == "+" then
    state = "Charging"
  elseif bat_state == "-" then
    state = "Discharging"
  elseif bat_state == "⌁" then
    state = "Not charging"
  else
    state = "Unknown"
  end

  naughty.notify { text = "Charge : " .. bat_charge .. "%\nState  : " .. state ..
    " (" .. bat_time .. ")", timeout = 5, hover_timeout = 0.5 }
end
batpct:buttons(awful.util.table.join(awful.button({ }, 1, popup_bat)))
baticon:buttons(batpct:buttons())
-- }}}

-- xrandr switcher
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
      state.iterator = awful.util.table.iterate(menu(),
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
   state.timer:connect_signal("timeout",
              function()
                 state.timer:stop()
                 state.timer = nil
                 state.iterator = nil
                 if action then
                awful.spawn(action, false)
                 end
              end)
   state.timer:start()
end


-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))



awful.screen.connect_for_each_screen(function(s)
    -- Create a promptbox for each screen
    awful.tag({"一", "二", "三", "四", "五", "六", "七", "八", "九"}, s, awful.layout.layouts[1])
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", height = 16, screen = s })

        -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
--    left_layout:add(mylauncher)
    left_layout:add(s.mytaglist)
    left_layout:add(space)
    left_layout:add(s.mypromptbox)

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(space)
    right_layout:add(wibox.widget.systray())
    right_layout:add(musicwidget.widget)
    right_layout:add(space)
    right_layout:add(baticon)
    right_layout:add(batpct)
    right_layout:add(volicon)
    right_layout:add(volpct)
    right_layout:add(volspace)
    right_layout:add(mytextclock)
    right_layout:add(s.mylayoutbox)

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(s.mytasklist)
    layout:set_right(right_layout)

    s.mywibox:set_widget(layout)



    s.mygraphbox = awful.wibar({ position = "bottom", height = 12, screen = s })
    local left_graphbox = wibox.layout.fixed.horizontal()
    left_graphbox:add(mylauncher)
    left_graphbox:add(space)
    left_graphbox:add(cpuwidget)
    left_graphbox:add(graphcpuwidget)
    left_graphbox:add(cpufreq)
    left_graphbox:add(tab)
    left_graphbox:add(memwidget)
    left_graphbox:add(graphmemwidget)
    left_graphbox:add(tab)
    left_graphbox:add(rootfsused)
    left_graphbox:add(rootfsbar)
    left_graphbox:add(rootfspct)
    left_graphbox:add(tab)
    left_graphbox:add(txwidget)
    left_graphbox:add(txgraph)
    left_graphbox:add(txspeed)
    left_graphbox:add(tab)
    left_graphbox:add(rxwidget)
    left_graphbox:add(rxgraph)
    left_graphbox:add(rxspeed)

    local right_graphbox = wibox.layout.fixed.horizontal()
    right_graphbox:add(weather)
    right_graphbox:add(space)

    local graphbox_layout = wibox.layout.align.horizontal()
    graphbox_layout:set_left(left_graphbox)
    graphbox_layout:set_right(right_graphbox)

    s.mygraphbox:set_widget(graphbox_layout)

end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"}),

    awful.key({modkey,            }, "F1",     function () awful.screen.focus(1) end),
    awful.key({modkey,            }, "F2",     function () awful.screen.focus(2) end),
    awful.key({modkey,            }, "F3",     function () awful.screen.focus(3) end),
    awful.key({modkey,            }, "F4",     function () awful.screen.focus(4) end),
    awful.key({ modkey, "Shift"   }, "F1", function (c) awful.client.movetoscreen(c, 1) end),
    awful.key({ modkey, "Shift"   }, "F2", function (c) awful.client.movetoscreen(c, 2) end),
    awful.key({ modkey, "Shift"   }, "F3", function (c) awful.client.movetoscreen(c, 3) end),
    awful.key({ modkey, "Shift"   }, "F4", function (c) awful.client.movetoscreen(c, 4) end),

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),

    awful.key({ modkey, "Control" }, "u", function () awful.spawn("xscreensaver-command -lock") end),
    awful.key({}, "XF86Display", xrandr),
    awful.key({ modkey, "Control" }, "p", xrandr),
    awful.key({ modkey,           }, "`", function () scratch.drop(terminal, "top", "center", 1, 0.16) end),
    awful.key({  }, "Print", function () awful.spawn("scrot -e 'mv $f ~/Pictures/screenshots/ 2>/dev/null'") end),
    awful.key({ modkey, "Control" }, "Print", function () awful.spawn("scrot -s -e 'mv $f ~/Pictures/screenshots/ 2>/dev/null'") end),
    -- Escape from keyboard focus trap (eg Flash plugin in Firefox)
    awful.key({ modkey, "Control" }, "Escape", function ()
         awful.spawn("xdotool getactivewindow mousemove --window %1 0 0 click --clearmodifiers 2")
    end),

    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
        },
        class = {
          "Arandr",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Wpa_gui",
          "pinentry",
          "veromix",
          "xtightvncviewer"},

        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
