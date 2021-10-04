! Replace test\Label3.br
! -------------------------------------------------------------------
	autoLibrary
! -------------------------------------------------------------------
	dim mytext$*50,filename$*50
	prg$="test\Label3.br" : _
	fnprg(prg$,2)
! -------------------------------------------------------------------
	fnTos(sn$='label3')
	myline = 1 : mypos = 5 : mylen = 10 : myalign = 2 : _
	font_mod=0 : _
	mytext$="This is a two Line Label, Woo Hoo."
	fnLbl(myline,mypos,mytext$,mylen,myalign,font_mod)
	fnAcs(2,mat response$,ckey)
