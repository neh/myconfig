Notes on my custom key mappings
===============================

It turns out to be possible to do some key remapping via udev, which nicely 
takes care of making sure that a keyboard is properly (IMO) set up whenever 
it's connected/disconnected/reconnected/etc. This is also handy for working 
around a problem I've had with remapping control (using xmodmap) to the left 
shift key on my TypeMatrix keyboards. To use these files:

1. put neh-\* in /lib/udev/keymaps
2. put 95-keymap.rules in /etc/udev/rules.d
3. there is no step 3 (groan)

That will do things like make the left shift key into control and left alt into 
shift on a TypeMatrix 2030, and make capslock a control key on a Thinkpad X220, 
among other things. There's useful info here: 
http://stew.vireo.org/blog/posts/Udev_and_multiple_keyboards/
