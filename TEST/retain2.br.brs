00010 ! s:\test\retain1
00020   ! execute 'load "s:\test\retainLib.br",Resident'
00030   library 's:\test\retainLib.br': fntestretain
00040   returnedValue=fntestretain(1)
00042   pr 'fntestRetain(1) called from '&program$&' returns '&str$(returnedValue)
00050   if returnedValue<=20 then chain 'S:\test\retain2' else stop
