00010 ! Replace S:\acsGL\AddCNo
00020 ! was GLCopy - but the functionality needed to be in addFRO_C_N_O, which there wasn't one of so I just renamed it to that for the time being and we need to fix and test addCno later.
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fntop,fnxit,fnchain,fntos,fnlbl,fncmbcno,fncmdset,fnacs,fnCopy,fnFree
00050   fntop(program$,cap$="Add Company")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim zer(57),resp$(10)*80
00090 ! ___________________________
00100   right=1
00120   fncno(to_cno)
00130 ! ___________________________
00140 MENU1: ! 
00150   fntos(sn$="GLAddCNo") !:
        mylen=37 : mypos=mylen+2
00160   fnlbl(1,1,"Copy Chart of Accounts from Company:",mylen,right)
00170   fncmbcno(1,mypos)
00180   fncmdset(2)
00190   fnacs(sn$,0,mat resp$,ckey)
00195   if ckey=5 then fro_cno=99999: goto L210 ! use company #99999 if no company to copy from
00200   fro_cno=val(resp$(1)(43:47))
00205   if fro_cno=0 then fro_cno=99999
00210 L210: if to_cno<1 or to_cno=fro_cno then goto MENU1
00220 ! ___________________________
00230   fnCopy(env$('Q')&"\GLmstr\*.h"&str$(fro_cno),env$('Q')&"\GLmstr\*.h"&str$(to_cno))
00240   open #20: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(to_cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(to_cno)&",NoShr",internal,outin,keyed 
00250 L250: read #20,using 'Form POS 87,PD 6.2': cb eof L280
00260   rewrite #20,using 'Form POS 81,42*PD 6.2,POS 333,2*PD 3,13*pd 6.2': mat zer
00270   goto L250
00280 L280: close #20: 
00290 ! ___________________________
00295   execute 'drop "'&env$('Q')&"GLmstr\GLTrans.H"&str$(to_cno)&'"' err ignore 
00350   fnFree(env$('Q')&"\GLmstr\ACTrans.h"&str$(to_cno))
00360   open #1: "Name="&env$('Q')&"\GLmstr\ACTrans.h"&str$(to_cno)&",Size=0,RecL=72,NoShr",internal,output 
00370   close #1: 
00380 XIT: fnchain("S:\acsGL\Company")
00390 IGNORE: continue 
00400 ! <Updateable Region: ERTN>
00410 ERTN: fnerror(program$,err,line,act$,"xit")
00420   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
00470 ! ______________________________________________________________________
