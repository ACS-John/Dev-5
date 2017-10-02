00010 ! Replace S:\acsCL\PayList
00020 ! payee listing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fndat,fnopenprn,fncloseprn,fncno,fnerror,fntop,fnxit,fntos,fnlbl,fncomboa,fntxt,fncmdset,fnacs,fnchk,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,holdvn$*8,vcode$*8
00080   dim cnam$*40,dat$*20,de$*30,io1$(2),text$*25,item1$(2)*20
00090   dim contact$*30,email$*50,fax$*12,myact$*20
00100   dim gl$*12,gldesc$*30,key$*19,tr$(5)*35,payeegl$*12,payeekey$*12
00110 ! ______________________________________________________________________
00120   let fntop(program$,cap$="Payee Listing")
00130   let fndat(dat$)
00140   let fncno(cno,cnam$)
00150   open #1: "Name="&env$('Q')&"\CLmstr\PAYMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PAYIDX1.h"&str$(cno)&",Shr",internal,outin,keyed 
00160   open #2: "Name="&env$('Q')&"\CLmstr\PAYMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PAYIDX2.h"&str$(cno)&",Shr",internal,outin,keyed 
00170   open #trmstr2=31: "Name="&env$('Q')&"\CLmstr\TRMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TRIDX2.h"&str$(cno)&",Shr",internal,outin,keyed 
00180   open #payeegl=3: "Name="&env$('Q')&"\CLmstr\payeeGLBreakdown.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\Payeeglbkdidx.h"&str$(cno)&",Shr",internal,outin,keyed 
00190   pr newpage
00200   let fntos("ubnamlst") !:
        let respc=0
00210   let text$="Report Heading Date:" !:
        let fnlbl(1,1,text$,25,1)
00220   let fntxt(1,27,20) !:
        let resp$(respc+=1)=dat$
00230   let fnlbl(2,1,"Print Order:",25,1)
00240   let item1$(1)="Payee Number" !:
        let item1$(2)="Alphabetic by Name"
00250   let fncomboa("paylist-srt",2,27,mat item1$,tt$) !:
        let resp$(respc+=1)=item1$(1)
00260   let fnchk(4,29,"Print G/L Breakdowns:",1) !:
        let resp$(respc+=1)="False"
00270   let fnchk(6,29,"Print Total Payments:",1) !:
        let resp$(respc+=1)="False"
00280   let fnlbl(8,1,"Transaction Starting Date:",25,1)
00290   let fntxt(8,27,8,0,0,"3",0,'Blank for All (Only applicable if need Total Payments Printed)') !:
        let resp$(respc+=1)=" "
00300   let fnlbl(9,1,"Transaction Ending Date:",25,1)
00310   let fntxt(9,27,8,0,0,"3",0,'Blank for All (Only applicable if need Total Payments Printed)') !:
        let resp$(respc+=1)=""
00320   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
00330   if ckey=5 then goto XIT
00340   let dat$=resp$(1)
00350   let seq$=resp$(2)(1:1)
00360   let fndat(dat$,2)
00370   if resp$(3)(1:1)="T" then let printgl=1 else let printgl=0 ! pr general ledger breakdowns
00380   if resp$(4)(1:1)="T" then let printtotal=1 else let printtotal=0 ! pr total payments
00390   begdate=val(resp$(5))
00400   let enddate=val(resp$(6)) ! ending date for adding purchases for period of time
00410   let namtab=66-int(len(rtrm$(cnam$))/2)
00420   let dattab=66-int(len(rtrm$(dat$))/2)
00430   let fnopenprn
00440   gosub L730
00450 L450: if seq$="A" then read #2,using L460,release: vn$,nam$,ad1$,ad2$,csz$,ph$ eof L790 else read #1,using L460,release: vn$,nam$,ad1$,ad2$,csz$,ph$,contact$,email$,fax$,myact$ eof L790
00460 L460: form pos 1,c 8,4*c 30,pos 153,c 12,c 30,c 50,c 12,c 20
00470   if printtotal=0 then goto L580
00480   let transactionstotal=0
00490   let key$=vn$&cnvrt$('pic(Z#)',0)&cnvrt$("pic(#)",1)&rpt$(chr$(0),8) !:
        restore #trmstr2,key>=key$: nokey EO_FLEX2
00500 READ_TRMSTR2: ! 
00510   read #trmstr2,using 'Form Pos 1,n 2,n 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof EO_FLEX2
00520   if trim$(vn$)<>trim$(tr$(4)) then goto EO_FLEX2
00530   if begdate<>0 and begdate>fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRMSTR2
00540   if enddate<>0 and enddate<fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRMSTR2
00550   let transactionstotal+=tr3
00560   goto READ_TRMSTR2
00570 EO_FLEX2: ! 
00580 L580: if printtotal= 0 then pr #255,using L690: vn$,nam$,ad1$,ad2$(1:25),csz$,ph$ else pr #255,using L690: vn$,nam$,ad1$,ad2$(1:25),csz$,ph$,transactionstotal pageoflow L710
00590   if printgl<>1 then goto L690
00600   restore #payeegl,key>=vn$: nokey EO_TEST
00610 READ_STANDARD_BREAKDOWNS: ! 
00620   read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_TEST
00630   if vn$<>payeekey$ then goto EO_TEST
00640   if trim$(payeegl$)="" or payeegl$="  0     0   0" then goto EO_TEST
00650   pr #255,using L660: payeegl$,percent,gldesc$
00660 L660: form pos 11,c 12,x 2,pic(----.zz),x 2,c 30,skip 1
00670   goto READ_STANDARD_BREAKDOWNS
00680 EO_TEST: ! 
00690 L690: form pos 1,c 8,x 2,2*c 30,c 25,c 30,c 12,pic($$$$,$$$.## cr),skip 1
00700   goto L450
00710 L710: pr #255: newpage: gosub L730
00720   continue 
00730 L730: pr #255,using L740: date$('mm/dd/yy'),cnam$,time$,"Payee Listing",dat$
00740 L740: form pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 60,c 20,skip 1,pos dattab,c 20,skip 2
00750   pr #255: "Payee No  Payee Name                    Address                       Address                  City, State Zip              Phone"
00760   pr #255: "________  __________________________    __________________________    _____________________    ___________________________  ____________"
00770   form pos 1,c 132,skip 1
00780   return 
00790 L790: let fncloseprn
00800   goto XIT
00810 ! ______________________________________________________________________
00820 XIT: let fnxit
00830 ! ______________________________________________________________________
00840 ! <Updateable Region: ERTN>
00850 ERTN: let fnerror(program$,err,line,act$,"xit")
00860   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00870   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00880   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00890 ERTN_EXEC_ACT: execute act$ : goto ERTN
00900 ! /region
