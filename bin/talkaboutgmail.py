#!/usr/bin/env python
import libgmail
ga = libgmail.GmailAccount("name", "pass")
ga.login()
messagecount = ga.getUnreadMsgCount()
print 'You have ' + str(messagecount) + ' new G Mail messages.'

folder = ga.getMessagesByFolder('inbox')
x = 1
for thread in folder:
	print 'Thread ' + str(x) + ', '
	print 'Length ' + str(len(thread)) + ', '
	for message in thread:
		print 'From ' + str(message.author) + ', '
		print 'About, ' + str(message.subject) + ', '
	x+=1
