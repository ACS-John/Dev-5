00010 ! Replace S:\acsGL\DumpPayeeTrans
00020 ! Vendor(Payee)  Dump old Transactions
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnopenprn,fncloseprn,fncno,fndat,fnprocess,fnchain, fntos,fnlbl,fntxt,fncmdset,fnacs,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,holdvn$*8,vcode$*8
00080   dim cnam$*40,dat$*20,adr(2),id1$*25
00090   dim rn$*12,de$*30,adr(2),tvn$*8,cap$*128
00100   dim scid$*79
00110   dim sd$(8),se$(8)*30,pl$(8,2)*35
00120 ! ______________________________________________________________________
00130   let fntop("S:\acsGL\VendorTransList",cap$="Dump Old Payee Transactions")
00140   let fncno(cno,cnam$) !:
        let fndat(dat$)
00150   open #payee=1: "Name="&env$('Q')&"\GLmstr\paymstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PayIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00160   open #payee2=11: "Name="&env$('Q')&"\GLmstr\paymstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\payidx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00170   open #trans=2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\gltridx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00180   let namtab=66-int(len(rtrm$(cnam$))/2)
00190   let dattab=66-int(len(rtrm$(dat$))/2)
00200 ! _________________________
00210 MENU1: ! 
00220 ASKDAT: ! 
00230   let fntos(sn$="DumpVendorTrans") !:
        let mylen=35 : let mypos=mylen+2
00240   let fnlbl(1,1,"Oldest Transaction Date to Retain:",mylen,right)
00250   let fntxt(1,mypos,8,0,left,'CCYYMMDD',0,'All payee transactions older than the date you enter here will be removed.') !:
        let resp$(1)=str$(oldestdate)
00255   let fnlbl(1,50,"")
00260   let fncmdset(2)
00270   let fnacs(sn$,0,mat resp$,ckey)
00280   if ckey=5 then goto XIT
00290   let oldestdate=val(resp$(1))
00300 L300: read #trans,using L320: trvn$,dt,am,rn$,de$ eof XIT
00310   if oldestdate > fndate_mmddyy_to_ccyymmdd(dt) then goto DELETEIT else goto L300
00320 L320: form pos 1,c 8,n 6,pd 5.2,c 12,c 30
00330   goto L350
00340 DELETEIT: ! 
00350 L350: delete #trans: 
00360   goto L300
00370 XIT: let fnxit
00380 ! ______________________________________________________________________
00390 ! <Updateable Region: ERTN>
00400 ERTN: let fnerror(program$,err,line,act$,"xit")
00410   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00420   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00430   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00440 ERTN_EXEC_ACT: execute act$ : goto ERTN
00450 ! /region
00460 ! ______________________________________________________________________
