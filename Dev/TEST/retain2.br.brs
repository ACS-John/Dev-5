! execute 'load "s:\test\retainLib.br",Resident'
library 's:\test\retainLib.br': fntestretain
returnedValue=fntestretain(1)
pr 'fntestRetain(1) called from '&program$&' returns '&str$(returnedValue)
if returnedValue<=20 then chain 'S:\test\retain2' else stop
