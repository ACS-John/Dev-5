00020 ! Replace Test\CSZ
00040 ! -----------------------------------------------------------------------
00060   library 'S:\Core\Library': fncombof,fnAcs,fnTos,fnCmdSet,fntop,fnpause,fnxit,fnLbl
00080   dim response$(2)*80
00100 ! -----------------------------------------------------------------------
00120   fntop("Test\CSZ","Test Combobox from File")
00140   fnTos(sn$="test_csz")
00160   fnLbl(4,1,"City, St Zip:",13,1,0,0)
00180   fncombof("CityStZip",4,15,30,"[Q]\Data\CityStZip.dat",1,28,0,0,"[Q]\Data\CityStZip.idx",0,0, "tool tip text",0,0)
00200   response$(1)=''
00220   fnCmdSet(2)
00240   fnAcs(sn$,0,mat response$,ckey)
00260   pr response$(1)
00280 ! 
00300   pr "response "&response$(1)
00320 ! fnpause
00340 ! fnxit
