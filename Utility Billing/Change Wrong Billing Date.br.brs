00010 ! formerly S:\acsUB\UBDATfix
00020 ! -- Change Wrong Billing Date
00030 ! r: setup
00040   library 'S:\Core\Library': fnxit,fnAcs,fnLbl,fnwait,fnTos,fnTxt,fnerror,fndate_mmddyy_to_ccyymmdd,fnCmdSet,fntop,fncmbact,fnmsgbox,fnAutomatedSavePoint
00050   on error goto ERTN
00060 ! 
00070   dim z$*10,o(2),bt1(14,2),badr(2),ba(13),txt$*40,tg(11),resp$(10)*80
00090   fntop(program$)
00100 ! /r
24000 SCREEN1: ! r:
24020   fnTos(sn$:="DatFix2")
24040   fnLbl(1,1,'Bad Billing Date:',24,1)
24060   fnTxt(1,26,8,0,0,"1") 
24080   resp$(1)=str$(d1)
24100   fnLbl(2,1,'Good Billing Date:',24,1)
24120   fnTxt(2,26,8,0,0,"1") 
24140   resp$(2)=str$(d2)
24160   fnLbl(4,1,"Starting Account:",24,1)
24180   fncmbact(4,26)
24200   resp$(3)="[All]"
24220   fnLbl(5,1,"Ending Account:",24,1)
24240   fncmbact(5,26)
24260   resp$(4)="[All]"
24280   fnCmdSet(2)
24300   fnAcs(sn$,0,mat resp$,ckey)
24320   if ckey=5 then goto XIT
24340   d1=val(resp$(1))
24360   d2=val(resp$(2))
24380   z_start$=resp$(3)(1:10) : if trim$(z_start$)='[All]' then z_start$=''
24400   z_end$=resp$(4)(1:10) : if trim$(z_end$)='[All]' then z_end$=''
24420   if d1=0 or d2=0 then goto SCREEN1 ! require a date in both fields
24440   hd1=d1: d1=fndate_mmddyy_to_ccyymmdd(d1)
24460   hd2=d2: d2=fndate_mmddyy_to_ccyymmdd(d2)
24480 goto Initialize ! /r
28000 Initialize: ! r:
28010   fnAutomatedSavePoint('before')
28020   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outIn,keyed 
28040   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,outIn,keyed 
28060 F_UBTRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
28080   gosub BUD1
28100 goto READ_UBTRANS ! /r
30000 READ_UBTRANS: ! r: main loop
30020   read #2,using F_UBTRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FINIS
30040   if z_start$<>'' and p$<z_start$ then goto READ_UBTRANS
30060   if z_end$<>'' and p$>z_end$ then goto READ_UBTRANS
30080   if tdate=d1 and tcode=1 then 
30100     read #1,using "Form POS 296,PD 4",key=p$: f
30120     if f><hd1 then goto READ_UBTRANS ! skip if not Last Billing Date
30140     rewrite #2,using "Form POS 11,n 8": d2
30160     rewrite #1,using "Form POS 296,PD 4": hd2
30180     recordUpdateCount+=1
30200     if bud1=1 then gosub BUD2
30220   end if
30240   goto READ_UBTRANS
30260 ! /r
32000 FINIS: ! r:
32020   close #1: ioerr ignore
32040   close #2: ioerr ignore
32060   dim mg$(1)*128
32080   mat mg$(1)
32100   mg$(1)=str$(recordUpdateCount)&' records updated.'
32120   fnmsgbox(mat mg$)
32140 goto XIT ! /r
34000 XIT: ! r:
34060 fnxit ! /r
36000 BUD1: ! r:
36020   bud1=0
36040   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outIn,keyed ioerr L490
36060   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outIn,relative 
36080   bud1=1
36100   L490:  !
36120 return ! /r
38000 BUD2: ! r:
38020   bd1=0 : mat bd1(5) : mat bd1=(0) : mat bd2=(0) : mat bd3=(0)
38040   if bud1=0 then goto XIT_BUD2
38060   read #81,using L550,key=p$: x$,mat ba,mat badr nokey XIT_BUD2
38080   L550: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
38100   ta1=badr(1)
38120   do until ta1=0
38140     read #82,using L590,rec=ta1: x$,mat bt1,nba noRec XIT_BUD2
38160     L590: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
38180     if bt1(1,1)=d1 then 
38200       bt1(1,1)=bt1(1,2)=d2 
38220       rewrite #82,using L610,rec=ta1: mat bt1 
38240       L610: form pos 11,2*pd 4,24*pd 5.2,2*pd 4
38260       goto XIT_BUD2
38280     end if
38300     ta1=nba
38320   loop
38340   XIT_BUD2: ! 
38360 return ! /r
42000 ! <Updateable Region: ERTN>
42020 ERTN: fnerror(program$,err,line,act$,"xit")
42040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
42060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
42080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
42100 ERTN_EXEC_ACT: execute act$ : goto ERTN
42120 ! /region
