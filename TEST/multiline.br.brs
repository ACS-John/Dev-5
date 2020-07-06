! Replace Test\MultiLine
! -------------------------------------------------------------------
	autoLibrary
	execute "Config Console Off"
	dim resp$(5)*400
! -------------------------------------------------------------------
	pr border: "Test MultiLine"
	prg$='test': fnprg(prg$,2)
	fnTos('test')
! -------------------------------------------------------------------
	fnmultiline(2,5,10,30) : _
	resp$(1)="This is a test"
	fnCmdKey("This is a test of dynamic button width",5,0,1) : _
	fnCmdKey("I",1)
	fnAcs(mat resp$,ckey)
	pr mat resp$
