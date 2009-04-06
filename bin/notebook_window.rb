#!/usr/bin/env ruby

def bring (notebook)
  system "wmctrl -i -R #{notebook}"
end

window_data = `wmctrl -x -l | grep NoteBook`
if window_data == ''
  n_cmd = ENV['HOME'] + '/bin/n'
  exec "urxvt -name NoteBook"# -e #{n_cmd}"
  exit
end

notebook = window_data.split(' ')[0]
bring notebook 
