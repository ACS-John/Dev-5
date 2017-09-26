00001   execute 'config option 48 on'
00010   let a$='123456789'
00020   rinput fields "2,2,C 10,,201": a$
00030   print 'fkey=';fkey
00040   stop 
