! Replace Test\CSZ
! -----------------------------------------------------------------------
	autoLibrary
	dim response$(2)*80
! -----------------------------------------------------------------------
	fnTop("Test\CSZ","Test Combobox from File")
	fnTos(sn$="test_csz")
	fnLbl(4,1,"City, St Zip:",13,1,0,0)
	fncombof("CityStZip",4,15,30,"[Q]\Data\CityStZip.dat",1,28,0,0,"[Q]\Data\CityStZip.idx",0,0, "tool tip text",0,0)
	response$(1)=''
	fnCmdSet(2)
	fnAcs2(mat response$,ckey)
	pr response$(1)
 
	pr "response "&response$(1)
! fnpause
! fnXit
