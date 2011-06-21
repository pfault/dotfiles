require("vicious")

graphwidth  = 35
graphheight = 11

-- {{{ SPACERS
spacer = widget({ type = "textbox" })
spacer.text = " "
tab = widget({ type = "textbox" })
tab.text = "        "
-- }}}

-- {{{ PROCESSOR
-- cache
vicious.cache(vicious.widgets.cpu)
vicious.cache(vicious.widgets.cpuinf)

cpuicon = widget({ type = "imagebox" })
cpuicon.image = image(beautiful.widget_cpu)

-- core 0 freq
cpufreq = widget({ type = "textbox" })
vicious.register(cpufreq, vicious.widgets.cpuinf, function(widget, args)
return string.format("<span color='#d6d6d6'>cpu </span>%1.1fGHz", args["{cpu0 ghz}"])
end, 3000)

-- core 0 graph
cpugraph0 = awful.widget.graph()
cpugraph0:set_width(graphwidth):set_height(graphheight)
cpugraph0:set_background_color(beautiful.bg_widget)
cpugraph0:set_border_color(nil)
cpugraph0:set_border_color(beautiful.bg_widget)
cpugraph0:set_gradient_colors({
    beautiful.fg_end_widget,
    beautiful.fg_center_widget
})
cpugraph0:set_gradient_angle(0)
vicious.register(cpugraph0, vicious.widgets.cpu, "$2")

-- core 0 %
cpupct0 = widget({ type = "textbox" })
vicious.register(cpupct0, vicious.widgets.cpu, "$2%", 2)

-- core 1 graph
cpugraph1 = awful.widget.graph()
cpugraph1:set_width(graphwidth):set_height(graphheight)
cpugraph1:set_background_color(beautiful.bg_widget)
cpugraph1:set_border_color(nil)
cpugraph1:set_border_color(beautiful.bg_widget)
cpugraph1:set_gradient_colors({
  beautiful.fg_end_widget,
  beautiful.fg_center_widget
})
cpugraph1:set_gradient_angle(0)
vicious.register(cpugraph1, vicious.widgets.cpu, "$3")

-- core 1 %
cpupct1 = widget({ type = "textbox" })
vicious.register(cpupct1, vicious.widgets.cpu, "$3%", 2)

-- core 2 graph
cpugraph2 = awful.widget.graph()
cpugraph2:set_width(graphwidth):set_height(graphheight)
cpugraph2:set_background_color(beautiful.bg_widget)
cpugraph2:set_border_color(nil)
cpugraph2:set_border_color(beautiful.bg_widget)
cpugraph2:set_gradient_colors({
  beautiful.fg_end_widget,
  beautiful.fg_center_widget
})
cpugraph2:set_gradient_angle(0)
vicious.register(cpugraph2, vicious.widgets.cpu, "$4")

-- core 2 %
cpupct2 = widget({ type = "textbox" })
vicious.register(cpupct2, vicious.widgets.cpu, "$4%", 2)

-- core 3 graph
cpugraph3 = awful.widget.graph()
cpugraph3:set_width(graphwidth):set_height(graphheight)
cpugraph3:set_background_color(beautiful.bg_widget)
cpugraph3:set_border_color(nil)
cpugraph3:set_border_color(beautiful.bg_widget)
cpugraph3:set_gradient_colors({
  beautiful.fg_end_widget,
  beautiful.fg_center_widget
})
cpugraph3:set_gradient_angle(0)
vicious.register(cpugraph3, vicious.widgets.cpu, "$5")

-- core 3 %
cpupct3 = widget({ type = "textbox" })
vicious.register(cpupct3, vicious.widgets.cpu, "$5%", 2)
-- }}}

-- {{{ MEMORY
-- cache
vicious.cache(vicious.widgets.mem)

memicon = widget({ type = "imagebox" })
memicon.image = image(beautiful.widget_mem)

-- ram used
memused = widget({ type = "textbox" })
vicious.register(memused, vicious.widgets.mem, "<span color='#d6d6d6'>ram</span>$2MB", 5)

-- ram bar
membar = awful.widget.progressbar()
membar:set_vertical(false):set_width(graphwidth):set_height(graphheight)
membar:set_ticks(false):set_ticks_size(2)
membar:set_background_color(beautiful.bg_widget)
membar:set_border_color(nil)
membar:set_gradient_colors({
  beautiful.fg_widget,
  beautiful.fg_center_widget,
  beautiful.bg_widget
})
vicious.register(membar, vicious.widgets.mem, "$1", 13)

-- ram %
mempct = widget({ type = "textbox" })
vicious.register(mempct, vicious.widgets.mem, "$1%", 5)

swapicon = widget({ type = "imagebox" })
swapicon.image = image(beautiful.widget_swap)

-- swap used
swapused = widget({ type = "textbox" })
vicious.register(swapused, vicious.widgets.mem, "<span color='#d6d6d6'>swap</span>$6MB", 5)

-- swap bar
swapbar = awful.widget.progressbar()
swapbar:set_vertical(false):set_width(graphwidth):set_height(graphheight)
swapbar:set_ticks(false):set_ticks_size(2)
swapbar:set_background_color(beautiful.bg_widget)
swapbar:set_border_color(nil)
swapbar:set_gradient_colors({
  beautiful.fg_widget,
  beautiful.fg_center_widget,
  beautiful.bg_widget
})
vicious.register(swapbar, vicious.widgets.mem, "$5", 13)

-- swap %
swappct = widget({ type = "textbox" })
vicious.register(swappct, vicious.widgets.mem, "$5%", 5)
-- }}}

-- {{{ FILESYSTEM
-- cache
vicious.cache(vicious.widgets.fs)

rooticon = widget({ type = "imagebox" })
rooticon.image = image(beautiful.widget_fs2)

-- root used
rootfsused = widget({ type = "textbox" })
vicious.register(rootfsused, vicious.widgets.fs, "<span color='#d6d6d6'>root</span>${/ used_gb}GB", 97)

-- root bar
rootfsbar = awful.widget.progressbar()
rootfsbar:set_vertical(false):set_width(graphwidth):set_height(graphheight)
rootfsbar:set_ticks(false):set_ticks_size(2)
rootfsbar:set_background_color(beautiful.bg_widget)
rootfsbar:set_border_color(nil)
rootfsbar:set_gradient_colors({
  beautiful.fg_widget,
  beautiful.fg_center_widget,
  beautiful.bg_widget
})
vicious.register(rootfsbar, vicious.widgets.fs, "${/ used_p}", 97)

-- root %
rootfspct = widget({ type = "textbox" })
vicious.register(rootfspct, vicious.widgets.fs, "${/ used_p}%", 97)
-- }}}

-- {{{ NETWORK
-- cache
vicious.cache(vicious.widgets.net)

upicon = widget({ type = "imagebox" })
upicon.image = image(beautiful.widget_up)

-- tx
txwidget = widget({ type = "textbox" })
vicious.register(txwidget, vicious.widgets.net, "<span color='#d6d6d6'>up</span>${eth0 tx_mb}MB", 19)

-- up graph
upgraph = awful.widget.graph()
upgraph:set_width(graphwidth):set_height(graphheight)
upgraph:set_background_color(beautiful.bg_widget)
upgraph:set_border_color(nil)
upgraph:set_gradient_colors({
  beautiful.fg_end_widget,
  beautiful.fg_center_widget
})
upgraph:set_gradient_angle(0)
vicious.register(upgraph, vicious.widgets.net, "${eth0 up_kb}")

-- up speed
upwidget = widget({ type = "textbox" })
vicious.register(upwidget, vicious.widgets.net, "${eth0 up_kb}k/s", 2)


downicon = widget({ type = "imagebox" })
downicon.image = image(beautiful.widget_down)

-- rx
rxwidget = widget({ type = "textbox" })
vicious.register(rxwidget, vicious.widgets.net, "<span color='#d6d6d6'>down</span>${eth0 rx_mb}MB", 17)

-- down graph
downgraph = awful.widget.graph()
downgraph:set_width(graphwidth):set_height(graphheight)
downgraph:set_background_color(beautiful.bg_widget)
downgraph:set_border_color(nil)
downgraph:set_gradient_colors({
  beautiful.fg_end_widget,
  beautiful.fg_center_widget
})
downgraph:set_gradient_angle(0)
vicious.register(downgraph, vicious.widgets.net, "${eth0 down_kb}")

-- down speed
downwidget = widget({ type = "textbox" })
vicious.register(downwidget, vicious.widgets.net, "${eth0 down_kb}k/s", 2)
-- }}}

-- {{{ PACMAN
-- icon
pacicon = widget({ type = "imagebox" })
pacicon.image = image(beautiful.widget_pac)

-- upgrades
pacwidget = widget({ type = "textbox" })
vicious.register(pacwidget, vicious.widgets.pkg, function(widget, args)
  if args[1] > 0 then
    pacicon.image = image(beautiful.widget_pacnew)
  else
    pacicon.image = image(beautiful.widget_pac)
  end

  return args[1]
end, 1801, "Arch")

-- buttons
function popup_pac()
  local pac_updates = ""
  local f = io.popen("pacman -Qu")
  if f then
    pac_updates = f:read("*a"):match("(.*)\n$")
  end
  f:close()

  if not pac_updates then
    pac_updates = "System is up to date"
  end

  naughty.notify { text = pac_updates, timeout = 5, hover_timeout = 0.5 }
end
pacwidget:buttons(awful.util.table.join(awful.button({ }, 1, popup_pac)))
pacicon:buttons(pacwidget:buttons())
-- }}}

-- {{{ THERMAL
-- icon
tzsicon = widget({ type = "imagebox" })
tzsicon.image = image(beautiful.widget_temp)

-- cpu temp
tzswidget = widget({ type = "textbox" })
vicious.register(tzswidget, vicious.widgets.thermal, function(widget, args)
  if args[1] > 70 then
    tzsicon.image = image(beautiful.widget_temphot)
  elseif args[1] > 50 then
    tzsicon.image = image(beautiful.widget_tempwarm)
  else
    tzsicon.image = image(beautiful.widget_temp)
  end
  return args[1] .. " °C"
end, 19, {"coretemp.0", "core"})
-- }}}
