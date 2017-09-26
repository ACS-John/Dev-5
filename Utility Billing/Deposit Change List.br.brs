00010 ! formerly S:\acsUB\ubDepChg
00020 ! -- Customer Deposit Change Listing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fntos,fnopenprn,fncloseprn,fnerror,fnxit,fndate_mmddyy_to_ccyymmdd,fncmdset,fntop,fngethandle
00050   on error goto ERTN
00060   fntop(program$)
00070   dim resp$(2)*20
00080   dim dp$*70
00130 ! ______________________________________________________________________
24000 MENU1: ! r:
24040   let fntos(sn$:="ubDepChg")
24060   let fnlbl(1,28," ",1,1)
24100   let fnlbl(1,1,"Starting Date:",16,1)
24140   let fntxt(1,18,8,0,0,"1",0,"Use mmddyy format for the oldest date to be listed.")
24160   let resp$(1)=str$(bd1)
24200   let fnlbl(2,1,"Ending Date:",16,1)
24240   let fntxt(2,18,8,0,0,"1",0,"Use mmddyy format for the latest date to be listed.")
24260   let resp$(2)=str$(ed1)
24280   let fncmdset(3)
24300   let fnacs(sn$,0,mat resp$,ckey)
26000   if ckey=5 then goto XIT
26020   let bd1=val(resp$(1)) 
26040   let bd1=fndate_mmddyy_to_ccyymmdd(bd1) 
26060   let ed1=val(resp$(2)) 
26080   let ed1=fndate_mmddyy_to_ccyymmdd(ed1)
26090 goto Report ! /r
31000 Report: ! r: start report
32000   if bd1=20000000 then let bd1=0
32020   if ed1=20000000 then let ed1=0
32060   open #hDeposit2:=fngethandle: 'Name='&env$('Q')&'\UBmstr\Deposit2.h'&env$('cno')&',KFName='&env$('Q')&'\UBmstr\Deposit2Index.h'&env$('cno')&',Shr,Use,RecL=73,KPs=1,KLn=10',internal,outin,keyed ! "Name="&env$('Q')&"\UBmstr\Deposit2.h"&env$('cno')&",Shr",internal,outin,relative 
32080   fDeposit2: form pos 1,c 10,n 8,c 32,2*n 10.2,pd 3
32120   let fnopenprn
32140   gosub HDR 
34000   dim da(2)
34020   ! gosub LegacyWay
34040   gosub NewWay
34060 goto DONE ! /r
38000 NEWPGE: print #255: newpage : gosub HDR : continue 
42000 HDR: ! r:
42020   let pg=pg+1
42040   print #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
42060   print #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
42080   print #255: "\qc  {\f181 \fs18 \b From: "&cnvrt$("PIC(####/##/##)",bd1)&"   Thru: "&cnvrt$("pic(####/##/##)",ed1)&"}"
42100   print #255,using 'form pos 1,c 12,pos 75,c 10': "\ql "&date$,"Page "&str$(pg)
42120   print #255: ""
42140   print #255: "{\ul Account No}  {\ul    Date   }  {\ul Description                     }  {\ul  Before }  {\ul   After }"
42160 return ! /r
44000 DONE: ! r:
44020   print #255: "" 
44040   print #255,using "Form POS 16,C 21,N 12.2": "Net Amount of Change:",t1
44060   let fncloseprn
44080 goto XIT ! /r
46000 XIT: let fnxit
52000 ! <Updateable Region: ERTN>
52020 ERTN: let fnerror(program$,err,line,act$,"xit")
52040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
52060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
52080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
52100 ERTN_EXEC_ACT: execute act$ : goto ERTN
52120 ! /region
56000 ! LegacyWay: ! r: old way whcich uses next transaction address technology
56020 !   read #hDeposit1,using "Form POS 1,C 10,2*PD 3": rk$,mat da eof FinisLegacyWay
56040 !   ! read #hDeposit1,using "Form POS 1,C 10": rk$ eof DONE
56060 !   let r32=da(1)
56080 !   do
56100 !   if r32=0 then goto LegacyWay
56120 !   read #hDeposit2,using fDeposit2,rec=r32: k32$,dt1,dp$,dp1,dp2,n32
56140 !   if dt1>=bd1 or bd1=0 then 
56160 !     if dt1<=ed1 or ed1=0 then 
56180 !       print #255,using 'form pos 1,c 12,pic(####/##/##bb),c 32,2*n 10.2': k32$,dt1,dp$,dp1,dp2 pageoflow NEWPGE
56220 !       let t1=t1+dp2-dp1
56240 !     end if
56260 !   end if
56280 !   let r32=n32
56300 !   loop
56320 !   FinisLegacyWay: !
56340 ! return ! /r
57000 NewWay: ! r: new way whcich uses indexes
57020   do
57040   read #hDeposit2,using fDeposit2: k32$,dt1,dp$,dp1,dp2 eof FinisNewWay
57060   if dt1>=bd1 or bd1=0 then 
57080     if dt1<=ed1 or ed1=0 then 
57100       print #255,using 'form pos 1,c 12,pic(####/##/##bb),c 32,2*n 10.2': k32$,dt1,dp$,dp1,dp2 pageoflow NEWPGE
57140       let t1=t1+dp2-dp1
57160     end if
57180   end if
57200   loop
57220   FinisNewWay: !
57240 return ! /r