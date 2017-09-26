00010 ! Replace S:\acsGL\glZer109
00020 ! Zero Year To Date Purchases
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnchain,fnprocess,fntos,fnlbl,fntxt,fncmdset,fnacs,fndate_mmddyy_to_ccyymmdd
00050   let fntop(program$,cap$="Zero Year To Date Purchases")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim adr(2),cnam$*40,cap$*128,de$*30,re$*12
00090 ! ______________________________________________________________________
00100   let fncno(cno,cnam$)
00110 ! 
00120   open #1: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L440
00130   open #2: "Name="&env$('Q')&"\GLmstr\gltr1099.h"&str$(cno),internal,outin,relative 
00140   print newpage
00150 SCR1: ! 
00160   let fntos(sn$="Glzer109") !:
        let lc=0 : let mylen=55 : let mypos=mylen+3 : let center=2 : let right=1
00170   let fnlbl(lc+=1,1,"* * *   Warning   * * *",60,center)
00180   let fnlbl(lc+=1,1,"This selection will dump all old purchase transactions from each",width,0)
00190   let fnlbl(lc+=1,1,"vendor (payee) record. This selection should only be run at year end",width,0)
00200   let fnlbl(lc+=1,1," after all 1099 forms have been printed.:",mylen,0)
00210   let fnlbl(lc+=1,1," Enter ZERO to continue:",mylen,right)
00220   let fntxt(lc,mypos,5) !:
        let resp$(1)=""
00230   let fncmdset(2)
00240   let fnacs(sn$,0,mat resp$,ckey)
00250   if ckey=5 then goto XIT
00260   let pas$=resp$(1)
00270   if lwrc$(pas$)<>lwrc$("zero") then goto SCR1
00280 OLDEST_DATE: ! 
00290   let fntos(sn$="Glzer1092") !:
        let lc=0 : let mylen=30 : let mypos=mylen+3 : let width=0
00300   let fnlbl(lc+=1,1,"Oldest Date to be Retained:",mylen,right)
00310   let fntxt(1,mypos,8,0,left,'CCYYMMDD',0,'For example, if you wantto dump all transactions up to the beginning of the new year, you would enter the first day of the new year.') !:
        let resp$(1)=str$(transactionendingdate)
00320   let fnlbl(lc,45,"",0,right)
00330   let fncmdset(2)
00340   let fnacs(sn$,0,mat resp$,ckey)
00350   if ckey=5 then goto XIT
00360   let lastdate=val(resp$(1))
00370 L370: read #2,using 'Form POS 1,C 8,N 6,PD 5.2,C 12,C 30': trvn$,da,amt,re$,de$ eof L400
00375   let x=fndate_mmddyy_to_ccyymmdd(da)
00380   if x<lastdate then delete #2,rec=rec(2): 
00390   goto L370
00400 L400: close #2: 
00410   execute "Copy "&env$('Q')&"\GLmstr\gltr1099.h"&str$(cno) & " x -D"
00420   execute "Copy X "&env$('Q')&"\GLmstr\gltr1099.h"&str$(cno)
00430   execute "Index "&env$('Q')&"\GLmstr\gltr1099.H"&str$(cno)&' '&env$('Q')&"\GLmstr\gltridx1.H"&str$(cno)&" 1 8 Replace DupKeys -N"
00440 L440: if fnprocess=1 then let fnchain("S:\acsGL\acglAuto")
00450 XIT: let fnxit
00460 ! ______________________________________________________________________
00470 ! <Updateable Region: ERTN>
00480 ERTN: let fnerror(program$,err,line,act$,"xit")
00490   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00500   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00510   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00520 ERTN_EXEC_ACT: execute act$ : goto ERTN
00530 ! /region
