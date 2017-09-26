00010 ! (formerly) S:\acsGL\glPrt109
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fntxt,fncmdset,fnacs,fndat,fncombof,fnfra,fnopt,fnask_1099_info,fn1099print_close,fn1099print
00050   let fntop(program$,cap$="Print 1099 Forms")
00060   on error goto ERTN
00080   dim vn$*8,nam$*30,ss$*11,box(11),ad$(3)*30
00090   dim cap$*128
00200   if ~fnask_1099_info(seltp,unused_type,minamt,beg_date,end_date) then goto XIT
00460   open #payee=1: "Name="&env$('Q')&"\GLmstr\paymstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PayIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
00470   open #trans=2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\gltridx1.h"&env$('cno')&",Shr",internal,outin,keyed 
32000   do
32020     read #payee,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11',release: vn$,nam$,mat ad$,typ,ss$ eof FINIS
32040     ytdp=fn_YearToDapPay(trans,vn$, beg_date,end_date)
32060     form pos 1,c 8,c 35,3*c 20,x 5,n 2,c 11
32080     if typ<>0 then 
32100       if ytdp=>minamt then 
32120         if seltp=0 or seltp=typ then 
32140           mat box=(0)
32160           if typ<1 or typ>8 then let typ=1
32180           let box(typ)=ytdp
32200           fn1099print(vn$,nam$,mat ad$,ss$,mat box)
32220         end if
32240       end if
32260     end if
32280   loop
36000   FINIS: !
36020   close #payee: ioerr ignore
36040   close #trans: ioerr ignore
36060   fn1099print_close
36080 XIT: let fnxit
38000 ! <Updateable Region: ERTN>
38020 ERTN: let fnerror(program$,err,line,act$,"xit")
38040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
38060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
38080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
38100 ERTN_EXEC_ACT: execute act$ : goto ERTN
38120 ! /region
40000 def fn_YearToDapPay(trans,key$; beg_date,end_date)
40020   ytdpReturn=0
40040   restore #trans,key>=key$: nokey ytdpFinis 
40060   do
40080     read #trans,using 'Form POS 1,c 8,N 6,PD 5.2',release: trvn$,dt,am eof ytdpFinis
40100     if trim$(key$)=trim$(trvn$) then 
40120       if beg_date=0 or fndate_mmddyy_to_ccyymmdd(dt)=>beg_date then 
40140         if end_date=0 or fndate_mmddyy_to_ccyymmdd(dt)<=end_date then 
40160           ytdpReturn+=am
40180         end if
40200       end if
40220     end if
40240   loop while trim$(key$)=trim$(trvn$)
40260   ytdpFinis: !
40280   fn_YearToDapPay=ytdpReturn 
40300 fnend
