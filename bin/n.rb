#!/usr/bin/env ruby

require 'date'

# Change Date#to_s to return my dir structure: YYYY/mmdd
class Date

  def to_s 
	self.strftime("%Y/%m%d")
  end

end

# Array addon to filter out files by pattern. Default is to
# filter out hidden and backup files.
class Array

  def filter (pattern='(^\..*|.*~$)')
	entries = []
	self.each do |entry|
	  next if entry =~ /#{pattern}/
	  entries.push entry
	end
	entries
  end

end


# Notebook wraps my collection of text files, providing a nice
# interface to them
class Notebook

  def initialize (notesroot)
	@@dir_notesroot = notesroot
	@@dir_today = Date.today.to_s

	Dir.mkdir(@@dir_today) unless File.exist?(@@dir_today)
	@@today = Dir.new(@@dir_notesroot + '/' + @@dir_today)
	@@yesterday = Dir.new(@@dir_notesroot + '/' + (Date.today - 1).to_s)
  end

  def today
	@@today.entries.filter
  end

  def yesterday
	@@yesterday.entries.filter
  end

end


# Wraps a single text file in the Noteboook and makes them
# nice to work with
class Note

  def show
	puts "hello"
  end

end


def parse (cmd)
  case cmd
  when 'l', 'list'
	notes.today
  when 'q', 'quit'
	exit
  end
end


notesroot = "#{ENV['HOME']}/notes"
notes = Notebook.new(notesroot)

while 1
  print '>'
  cmd = gets.chomp

  case cmd
  when 'l', 'list'
	puts notes.today
  when 'l y', 'list y'
	puts notes.yesterday
  when 'q', 'quit'
	exit
  end
end

#puts "\n\tToday:"
#puts notes.today
#puts "\n\tYesterday:"
#puts notes.yesterday
