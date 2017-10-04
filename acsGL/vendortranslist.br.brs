00010 ! Replace S:\acsGL\VendorTransList
00020 ! Vendor(Payee)  File - Transaction List
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnopenprn,fncloseprn,fncno,fndat,fnprocess,fnchain, fntos,fnlbl,fntxt,fncmdset,fnacs,fnconsole,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,holdvn$*8,vcode$*8
00080   dim cnam$*40,dat$*20,adr(2),id1$*25
00090   dim rn$*12,de$*30,adr(2),tvn$*8,cap$*128
00100   dim scid$*79
00110   dim sd$(8),se$(8)*30,pl$(8,2)*35
00120 ! ______________________________________________________________________
00130   fntop("S:\acsGL\VendorTransList",cap$="Payee Transaction List")
00140   fnconsole(off=0)
00150   fncno(cno,cnam$) !:
        fndat(dat$)
00160   open #payee=1: "Name="&env$('Q')&"\GLmstr\paymstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PayIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00170   open #payee2=11: "Name="&env$('Q')&"\GLmstr\paymstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\payidx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00180   open #trans=2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\gltridx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00190   let namtab=66-int(len(rtrm$(cnam$))/2)
00200   let dattab=66-int(len(rtrm$(dat$))/2)
00210   if fnprocess=1 then goto L380
00220 ! _________________________
00230 MENU1: ! 
00240 ASKDAT: ! 
00250   fntos(sn$="VendorTransList") !:
        let mylen=28 : let mypos=mylen+2
00260   fnlbl(1,1,"Report Heading Date:",mylen,right=1)
00270   fntxt(1,mypos,20) !:
        let resp$(1)=dat$
00280   fnlbl(3,1,"Transaction Starting Date:",mylen,right)
00290   fntxt(3,mypos,8,0,left,'CCYYMMDD',0,'(Blank for All)  Normally you would enter the first day of the year, but you can analyze any time frame.') !:
        let resp$(1)=str$(transactionstartingdate)
00300   fnlbl(4,1,"Transaction Ending Date:",mylen,right)
00310   fntxt(4,mypos,8,0,left,'CCYYMMDD',0,'(Blank for All)  Normally you would enter the last day of the year, but you can analyze any time frame.') !:
        let resp$(2)=str$(transactionendingdate)
00320   fncmdset(2)
00330   fnacs(sn$,0,mat resp$,ckey)
00340   if ckey=5 then goto XIT
00350   let dat$=resp$(1)
00360   let dattab=66-int(len(rtrm$(dat$))/2)
00370   fndat(dat$,2)
00380 L380: let fnopenprn
00390   gosub L630
00400 L400: read #1,using L420: vn$,nam$,ytdp eof L730
00410   if ytdp<-200000 then let ytdp=0
00420 L420: form pos 1,c 8,c 30,pos 129,pd 5.2
00430   let fst=0
00440   let ec$=""
00450   let tot=0
00460   restore #trans,key>=vn$: nokey L400
00470 L470: read #trans,using L520,release: trvn$,dt,am,rn$,de$ eof L400
00480   if trim$(trvn$) <> trim$(vn$) then let nomore=1: goto L590
00490   if transactionstartingdate>0 and fndate_mmddyy_to_ccyymmdd(dt)<transactionstartingdate then goto L470
00500   if transactionendingdate>0 and fndate_mmddyy_to_ccyymmdd(dt)>transactionendingdate then goto L470
00510   if am<-2000000 then am=0
00520 L520: form pos 1,c 8,n 6,pd 5.2,c 12,c 30
00530   let tot=tot+am
00540   if fst=0 then pr #255,using L550: vn$,nam$,dt,rn$,de$,am,tot,ec$ pageoflow L580 else pr #255,using L560: dt,rn$,de$,am,tot,ec$ pageoflow L580
00550 L550: form pos 1,c 8,pos 10,c 35,pos 46,pic(zz/zz/zz),pos 56,c 12,pos 70,c 30,pos 100,n 10.2,pos 115,n 10.2,pos 127,c 5
00560 L560: form pos 46,pic(zz/zz/zz),pos 56,c 12,pos 69,c 30,pos 100,n 10.2,pos 115,n 10.2,pos 127,c 5
00570   goto L590
00580 L580: gosub L620
00590 L590: let fst=1
00600   if nomore=1 then pr #255,using "form pos 50,c 20,n 10.2": "Total Transactions",tot: let nomore=0 else goto L470
00610   goto L400
00620 L620: pr #255: newpage
00630 L630: let p2=p2+1
00640   pr #255,using L650: date$('mm/dd/yy'),cnam$,time$,"Vendor Transaction Listing"
00650 L650: form skip 1,pos 1,c 8,pos 40,cc 40,skip 1,pos 1,c 8,pos 40,cc 40,skip 1
00660   let p1=66-int((len(rtrm$(dat$))+6)/2)
00670   pr #255,using L680: rtrm$("As of "&dat$),"Page",p2
00680 L680: form pos 40,cc 40,pos 110,c 4,n 5,skip 1
00690   pr #255,using "form pos 118,c 5": "Total"
00700   pr #255,using L710: "Vendor #","Vendor Name","Date","Reference #","Description","Amount","    Purchases"
00710 L710: form pos 1,c 8,pos 10,c 11,pos 48,c 4,pos 56,c 11,pos 70,c 11,pos 104,c 6,pos 112,c 13,skip 2
00720   return 
00730 L730: let fncloseprn
00740 XIT: let fnxit
00750 ! ______________________________________________________________________
00760 ! <Updateable Region: ERTN>
00770 ERTN: let fnerror(program$,err,line,act$,"xit")
00780   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00790   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00810 ERTN_EXEC_ACT: execute act$ : goto ERTN
00820 ! /region
00830 ! ______________________________________________________________________
