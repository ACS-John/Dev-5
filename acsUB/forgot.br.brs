10000 ! Replace S:\acsUB\Forgot
10100 ! forgot to zero YTD
10200 ! ______________________________________________________________________
10300   library 'S:\Core\Library': fntop,fnxit, fnerror,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fnacs,fnwait,fntxt,fnxit,fncmdset,fntop
10400   on error goto ERTN
10500 ! ______________________________________________________________________
10600   dim srv$(3)*1,cap$*128,txt$*80,tg(11)
10800   fntop(program$,cap$="Forgot to Zero Year-To-Date")
10900 MAIN: ! 
11000   sn$="forgot"
11100   fntos(sn$)
11200   txt$="First Billing Date of New Year (MMDDYY):"
11300   fnlbl(1,1,txt$,42,1)
11400   fntxt(1,44,8,0,0,"1")
11500   resp$(1)=str$(fbd)
11600   fncmdset(2): fnacs(sn$,0,mat resp$,ckey)
11700   fbd$=lpad$(str$(val(resp$(1))),6)
11800   fbd=val(resp$(1)) conv MAIN
11900   if ckey=5 then goto XIT
12000   txt$="Reconstructing Year-To-Date Usage beginning with "&fbd$(1:2)&"/"&fbd$(3:4)&"/"&fbd$(5:6)&"."
12100   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
12200   open #2: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&env$('cno')&",Shr",internal,input,keyed 
12300 READ_CUSTOMER: ! 
12400   read #1,using L210: z$ eof XIT
12500 L210: form pos 1,c 10
12600   watuse=eleuse=gasuse=0
12700   restore #2,key>=z$&"         ": nokey L330
12800 L240: read #2,using L250: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L330
12900 L250: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
13000   if p$<>z$ then goto L330
13100   if tcode<>1 then goto L240 ! only charge transactions
13200   x=fbd
13300   if tdate<fndate_mmddyy_to_ccyymmdd(x) then goto L240
13400   watuse=watuse+wu
13500   eleuse=eleuse+eu
13600   gasuse=gasuse+gu
13700   goto L240
13800 L330: rewrite #1,using L340: watuse,elecuse,gasuse
13900 L340: form pos 232,pd 5,pos 252,pd 5,pos 272,pd 5
14000   goto READ_CUSTOMER
14100 ! ______________________________________________________________________
14200 XIT: fnxit
14300 ! ______________________________________________________________________
14400 ! <Updateable Region: ERTN>
14500 ERTN: fnerror(program$,err,line,act$,"xit")
14600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
14700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
14800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
14900 ERTN_EXEC_ACT: execute act$ : goto ERTN
15000 ! /region
