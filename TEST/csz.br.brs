00020 ! Replace Test\CSZ
00040 ! -----------------------------------------------------------------------
00060   library 'S:\Core\Library': fncombof,fnacs,fntos,fncmdset,fntop,fnpause,fnxit,fnlbl
00080   dim response$(2)*80
00100 ! -----------------------------------------------------------------------
00120   let fntop("Test\CSZ","Test Combobox from File")
00140   let fntos(sn$="test_csz")
00160   let fnlbl(4,1,"City, St Zip:",13,1,0,0)
00180   let fncombof("CityStZip",4,15,30,env$('Q')&"\Data\CityStZip.dat",1,28,0,0,env$('Q')&"\Data\CityStZip.idx",0,0, "tool tip text",0,0)
00200   let response$(1)=''
00220   let fncmdset(2)
00240   let fnacs(sn$,0,mat response$,ckey)
00260   print response$(1)
00280 ! 
00300   print "response "&response$(1)
00320 ! let fnpause
00340 ! let fnxit
