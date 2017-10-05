00040   library 'S:\Core\Library': fntop,fntos,fnlbl,fntxt,fncmdkey,fnacs
00070   dim cap$*128,resp$(30)*50
00210   fntop(program$,cap$="test fntxt")
00500   fntos(sn$="GLInput")
00502   mylen=5: mypos=mylen+3 : right=1
00510 ! fnfra(1,1,4,60,"Method of Entry","Choose the method of transaction entry.")
00580   fnlbl(4,1,"Test:",mylen,right)
00680   fntxt(4,mypos,8,0,right,"1001",0,"Process endings date must always be answered and will be the last day of the month or the last day of the period beding processed..",0 )
00682   resp$(1)=date$('mm/dd/ccyy') ! =str$(contra)
00690   fncmdkey("mmmK",1,1,0,"Allows you to enter transactions.")
00720   fnacs(sn$,0,mat resp$,ckey)
