library 'S:\Core\Library': fnTop
library 'S:\Core\FileIO\fileio.br': fnProgressBar
library 'S:\Core\FileIO\fileio.br': fnCloseBar
fnTop(program$)
dim caption$*255
dim messageRow$*255
caption$=env$('program_caption')
for percent=1 to 100
	pr f '24,10,C': str$(percent)&'%'
	fnProgressBar(percent/100) ! , color$,progressAfter,progressThreshhold,updateThreshhold,caption$,messageRow$)
	sleep(.1)
nex percent
fnCloseBar
end
