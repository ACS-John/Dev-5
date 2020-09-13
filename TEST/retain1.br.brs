execute 'load "s:\test\retainLib.br",Resident'
library 's:\test\retainLib.br': fntestretain
pr 'fntestRetain(1) called from '&program$&' returns '&str$(fntestretain(1))
chain 'S:\test\retain2'
