#!/usr/bin/env python

# From: http://askubuntu.com/questions/89279/listening-to-incoming-libnotify-notifications-using-dbus

import glib
import dbus
from dbus.mainloop.glib import DBusGMainLoop
from subprocess import call

def print_notification(bus, message):
  keys = ["app_name", "replaces_id", "app_icon", "summary",
          "body", "actions", "hints", "expire_timeout"]
  args = message.get_args_list()
  if len(args) == 8:
    notification = dict([(keys[i], args[i]) for i in range(8)])
    print 'APP:'+notification["app_name"], 'SUMMARY:'+notification["summary"], 'BODY:'+notification["body"]

    if notification["app_name"] == 'Wicd':
      call(["pkill", "-RTMIN+9", "i3blocks"])

      # Update public IP display
      if notification["body"] == 'Connection established' or notification["summary"] == 'Disconnected':
        call(["pkill", "-RTMIN+8", "i3blocks"])


loop = DBusGMainLoop(set_as_default=True)
session_bus = dbus.SessionBus()
session_bus.add_match_string("type='method_call',interface='org.freedesktop.Notifications',member='Notify',eavesdrop=true")
session_bus.add_message_filter(print_notification)

glib.MainLoop().run()
