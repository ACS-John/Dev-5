00010 ! s:\test\retain1
00020   execute 'load "s:\test\retainLib.br",Resident'
00030   library 's:\test\retainLib.br': fntestretain
00040   print 'fntestRetain(1) called from '&program$&' returns '&str$(fntestretain(1))
00050   chain 'S:\test\retain2'
