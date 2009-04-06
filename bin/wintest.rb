#!/usr/bin/env ruby

windows = []
list = `wmctrl -x -l`
list.each do |window|
  window =~ /^(\S+)\s+(-?\d+)\s+(\S+)\s+(\w+)\s+(.*)$/
  windows.push [ $1, $2, $3, $4, $5 ]
end

windows.each { |window| puts window[2] + window[4] unless window[1] == '-1' }
