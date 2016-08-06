00010 ! Replace R:\acsCL\fnPayee
00020 ! Payee File - home to the fnAddPayee function
00030   def library fnaddpayee
00040     library 'R:\Core\Library': fncno,fndat,fnerror,fntos,fnlbl,fntxt,fncombof,fncmdset,fnacs,fnmsgbox,fnwait,fnfra,fnbutton,fnflexinit1,fnflexadd1,fncmdkey,fndate_mmddyy_to_ccyymmdd,fngethandle,fnqgl,fnagl$,fnrgl$
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,vcode$*8
00080     dim tr$(5)*35,item6$(11)*35
00090     dim item$(11)*50,cmask$(11),chdr$(11)*20
00100     dim contact$*30,email$*50,fax$*12,myact$*20
00110     dim cap$*128,key$*19
00120     dim ml$(3)*70,citystzip$*30,glitem$(5)*30,payeekey$*8,payeegl$*12
00130     dim gldesc$*30,resp$(60)*50
00140 ! ______________________________________________________________________
00150     let fncno(cno)
00160     let left=0: let right=1
00170     open #trmstr2:=fngethandle: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00180     open #paymstr:=fngethandle: "Name=Q:\CLmstr\PayMstr.h"&str$(cno)&",Version=1,KFName=Q:\CLmstr\PayIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00190     open #paymstr2:=fngethandle: "Name=Q:\CLmstr\PayMstr.h"&str$(cno)&",KFName=Q:\CLmstr\PayIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00200     open #payeegl:=fngethandle: "Name=Q:\CLmstr\PayeeGLBreakdown.h"&str$(cno)&",Version=1,KFName=Q:\CLmstr\Payeeglbkdidx.h"&str$(cno)&",Shr",internal,outin,keyed 
00210     open #citystzip:=fngethandle: "Name=Q:\Data\CityStZip.dat,KFName=Q:\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outin,keyed 
00220 ! 
00230 MENU1: ! 
00240     let fntos(sn$="payee-1") !:
          let respc=0
00250     mat chdr$(12) : mat cmask$(12) : mat item$(12) !:
          let chdr$(1)='Rec' !:
          let chdr$(2)='Payee Number' : let chdr$(3)='Payee Name' !:
          let chdr$(4)='Address' : let chdr$(5)='Address' !:
          let chdr$(6)='City, ST Zip' : let chdr$(7)='Type' !:
          let chdr$(8)='ID Number' : let chdr$(9)='Phone Number' !:
          let chdr$(10)='Contact Name' : let chdr$(11)='E-mail' !:
          let chdr$(12)='Fax'
00260     let cmask$(1)=cmask$(2)='' !:
          let cmask$(3)=cmask$(4)=cmask$(5)=cmask$(6)='80' !:
          let fnflexinit1('Hayee',1,1,20,100,mat chdr$,mat cmask$,1,0,frame) !:
          let editrec=0
00270     restore #paymstr: 
00280 READ_PAYMSTR_1: ! 
00290     read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',release: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ eof EO_FLEX1
00300     let item$(1)=str$(rec(paymstr)) !:
          let item$(2)=vn$ : let item$(3)=nam$ : let item$(4)=ad1$ !:
          let item$(5)=ad2$ : let item$(6)=csz$ : let item$(7)=str$(typ) !:
          let item$(8)=ss$ : let item$(9)=ph$ !:
          let item$(10)=contact$ : let item$(11)=email$ !:
          let item$(12)=fax$ !:
          let fnflexadd1(mat item$)
00310     goto READ_PAYMSTR_1
00320 EO_FLEX1: ! 
00330     let fncmdkey("&Add",1,0,0,"Add new payee records") !:
          let fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit or press Alt+E to change any existing payee record.") !:
          let fncmdkey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing payee record.") !:
          let fncmdkey("E&xit",5,0,1,"Exit to menu")
00340     let fnacs(sn$,0,mat resp$,ck)
00350     let add=edit=0: let holdvn$=""
00360     if ck=5 then goto XIT !:
          else if ck=1 then let add=1: goto ADD_NEW_PAYEE
00370   if ck=2 or ck=3 then let editrec=val(resp$(1))
00380   if editrec=0 then goto MENU1
00390   if ck=2 or ck=3 then !:
          read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',rec=editrec: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
00400   if ck=2 then let edit=1 : let holdvn$=vn$: goto EDIT_PAYEE
00410   if ck=3 then gosub DELETE_PAYEE : goto MENU1
00420 ! ______________________________________________________________________
00430 DELETE_PAYEE: ! 
00440 ! check for Linked Unpaid Invoices !:
        ! if there are any - than tell them, and don't delete.
00450   open #paytrans:=fngethandle: "Name=Q:\CLmstr\Paytrans.h"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00460   restore #paytrans,key>=vn$&rpt$(chr$(0),12): nokey L490
00470   read #paytrans,using 'Form Pos 1,C 8',release: x$
00480   if x$=vn$ then !:
          mat ml$(2) !:
          let ml$(1)="A Unpaid Invoice for this payee exists" !:
          let ml$(2)="You may not delete it." !:
          let fnmsgbox(mat ml$,resp$,cap$,0) !:
          goto EO_DELETE
00490 L490: ! 
00500   delete #paymstr,rec=editrec: 
00510   restore #payeegl,key>=vn$: nokey EO_DELETE_PAYEE
00520 DELETE_PAYEEGL_LOOP: ! 
00530   read #payeegl,using 'Form Pos 1,C 8': payeekey$ eof EO_DELETE_PAYEE
00540   if payeekey$=vn$ then !:
          delete #payeegl: !:
          goto DELETE_PAYEEGL_LOOP
00550 EO_DELETE_PAYEE: ! 
00560 ! 
00570   open #trans:=fngethandle: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00580   restore #trans, key>=holdvn$&rpt$(chr$(0),kln(trans)-len(holdvn$)): nokey EO_DEL_KEY_ON_TRANS
00590 L590: read #trans,using 'Form Pos 28,C 8': x$ eof EO_DEL_KEY_ON_TRANS
00600   if x$=vn$ then !:
          rewrite #trans,using 'Form Pos 28,Cr 8': '' !:
          goto L590
00610 EO_DEL_KEY_ON_TRANS: ! 
00620   close #trans: 
00630 EO_DELETE: return 
00640 ! ______________________________________________________________________
00650 ADD_NEW_PAYEE: ! 
00660   let vn$=nam$=ad1$=ad2$=csz$=ss$=ph$=contact$=email$=fax$=myact$="" !:
        let ytdp=typ=0
00670   goto EDIT_PAYEE
00680 ! ______________________________________________________________________
00690 EDIT_PAYEE: ! 
00710   let fntos(sn$="payee-2") !:
        let respc=0 !:
        let mylen=28 : let mypos=mylen+2
00720   let fnfra(1,1,12,70,"Payee Information"," ")
00730   let fnlbl(1,1,"Payee Number:",mylen,1,0,1)
00740   let fntxt(1,mypos,8,0,1,"",0,"",1) !:
        let resp$(respc+=1)=vn$
00750   let fnlbl(2,1,"Payee Name:",mylen,1,0,1)
00760   let fntxt(2,mypos,30,0,0,"",0,"",1) !:
        let resp$(respc+=1)=nam$
00770   let fnlbl(3,1,"Address:",mylen,1,0,1)
00780   let fntxt(3,mypos,30,0,0,"",0,"",1) !:
        let resp$(respc+=1)=ad1$
00790   let fnlbl(4,1,"Address:",mylen,1,0,1)
00800   let fntxt(4,mypos,30,0,0,"",0,"",1) !:
        let resp$(respc+=1)=ad2$
00810   let fnlbl(5,1,"City, St. Zip:",mylen,1,0,1)
00820   let fncombof("CityStZip",5,mypos,30,"Q:\Data\CityStZip.dat",1,30,0,0,"Q:\Data\CityStZip.idx",0,0, " ",1,0) !:
        let resp$(respc+=1)=csz$
00830   let fnlbl(6,1,"Type:",mylen,1,0,1)
00840   let fncombof("Payeetype",6,mypos,27,"R:\acsCL\PayeeType.dat",1,2,3,25,"",0,0, "The payee type is a code used to detemine which box should be used on a 1099 misc form.  Only enter a type code if the payee should get a 1099",1) !:
        let resp$(respc+=1)=str$(typ)
00850   let fnlbl(7,1,"Federal ID or SS No.",mylen,1,0,1)
00860   let fntxt(7,mypos,11,0,0,"",0,"",1) !:
        let resp$(respc+=1)=ss$
00870   let fnlbl(8,1,"Phone Number:",mylen,1,0,1)
00880   let fntxt(8,mypos,12,0,0,"",0,"",1) !:
        let resp$(respc+=1)=ph$
00890   let fnlbl(9,1,"Contact Name:",mylen,1,0,1)
00900   let fntxt(9,mypos,30,0,0,"",0,"",1) !:
        let resp$(respc+=1)=contact$
00910   let fnlbl(10,1,"E-mail Address:",mylen,1,0,1)
00920   let fntxt(10,mypos,30,50,0,"",0,"",1) !:
        let resp$(respc+=1)=email$
00930   let fnlbl(11,1,"Fax Number:",mylen,1,0,1)
00940   let fntxt(11,mypos,12,0,0,"",0,"",1) !:
        let resp$(respc+=1)=fax$
00950   let fnlbl(12,1,"My Account Number:",mylen,1,0,1)
00960   let fntxt(12,mypos,20,0,0,"",0,"",1) !:
        let resp$(respc+=1)=myact$
00970   let fnlbl(17,20,"Standard General Ledger Breakdowns",40,2,0,0)
00980 ! General Ledger Breakdown Grid
00990   mat chdr$(5) : mat cmask$(5) : mat glitem$(5) !:
        let chdr$(1)='Refenence': let chdr$(2)='Payee Number' !:
        let chdr$(3)='GL Number' !:
        let chdr$(4)='Percent' : let chdr$(5)='Description'
01000   let cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)='' !:
        let cmask$(4)='32' !:
        let fnflexinit1('PayeeGl',17,1,5,70,mat chdr$,mat cmask$,1,0,0)
01010   if trim$(vn$)="" then goto EO_FLEX3
01020   restore #payeegl,key>=vn$: nokey EO_FLEX3
01030 READ_PAYEE_GL: ! 
01040   read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,gldesc$ eof EO_FLEX3
01050   if vn$<>payeekey$ then goto EO_FLEX3
01060   let glitem$(1)=str$(rec(payeegl)) : let glitem$(2)=payeekey$ !:
        let glitem$(3)=payeegl$ : let glitem$(4)=str$(percent) !:
        let glitem$(5)=gldesc$ !:
        let fnflexadd1(mat glitem$)
01070   goto READ_PAYEE_GL
01080 EO_FLEX3: ! 
01090   let pas=1 ! don't redo combo boxes on gl
01100   let fnlbl(21,1,"",1,0,0,0) ! add space before buttons
01110   let fnbutton(lc=16,61,"Add",2,"Add a standard general ledger breakdowns",0,4) !:
        let fnbutton(lc,67,"Edit",7,"Edit or Delete a standard general ledger breakdowns")
01120   let fncmdkey("Save",1,1,0,"Saves and returns to Vendor selection") !:
        let fncmdkey("&Transactions",4,0,0,"List all checks for this payee") !:
        let fncmdkey("&Cancel",5,0,1,"Return to Vendor selection")
01130   let fnacs(sn$,0,mat resp$,ck)
01140   if ck=5 then goto MENU1
01150   let vn$=lpad$(trim$(resp$(1)(1:8)),8) !:
        let nam$=resp$(2) ! name !:
        let ad1$=resp$(3) ! address !:
        let ad2$=resp$(4) ! address !:
        let csz$=resp$(5) ! city state zip
01160   if add=1 then goto L1190
01170   if edit=1 and holdvn$<>vn$ then goto L1190
01180   goto L1210
01190 L1190: read #paymstr,using 'Form Pos 1,C 8',key=vn$: oldvn$ nokey L1210
01195   if add=1 then goto L1205
01200   mat ml$(2) !:
        let ml$(1)="You already have a payee number "&vn$ !:
        let ml$(2)="Click ok to Cancel." !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
01205 L1205: mat ml$(2) !:
        let ml$(1)="You already have a payee number "&vn$ !:
        let ml$(2)="Click ok to Change the number." !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto EDIT_PAYEE
01210 L1210: if trim$(vn$)="" then goto L1220 else goto L1230
01220 L1220: mat ml$(2) !:
        let ml$(1)="You must have a unique payee number for ." !:
        let ml$(2)="each vendor.  Click ok to assign a payee number" !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto EDIT_PAYEE
01230 L1230: read #citystzip,using 'Form POS 1,C 30',key=rpad$(ltrm$(csz$),30),release: citystzip$ nokey L1240 : goto L1250
01240 L1240: write #citystzip,using 'Form POS 1,C 30': csz$
01250 L1250: let typ=val(resp$(6)(1:2)) ! type !:
        let ss$=resp$(7) ! ss or fed id !:
        let ph$=resp$(8) ! phone !:
        let contact$=resp$(9) ! contact name !:
        let email$=resp$(10) ! email address
01260   let fax$=resp$(11) ! fax number !:
        let myact$=resp$(12) ! my account number with this vendor !:
        let gldistrec=val(resp$(13)) ! record number of gl distribution entry
01270   if ck=4 then gosub CHECK_HISTORY : goto EDIT_PAYEE
01280   if ck=2 then let percent=gldistrec=0: let payeekey$=gldesc$=payeegl$="" !:
          goto GL_BREAKDOWNS ! add gl breakdown
01290   if ck=7 then !:
          read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec,release: payeekey$,payeegl$,percent,gldesc$ !:
          goto GL_BREAKDOWNS ! edit gl breakdown
01300   let tac=0
01310 READ_STANDARD_BREAKDOWNS: ! 
01320   restore #payeegl,key>=vn$: nokey EO_TEST
01330 L1330: read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,gldesc$ eof EO_TEST
01340   if vn$<>payeekey$ then goto EO_TEST
01350   let tac+=percent
01360   goto L1330
01370 EO_TEST: ! 
01380   if tac=100 or tac=0 then goto SAVE_PAYEE
01390 MSGBOX4: ! percent breakdown doesn't add to 100 %
01400   mat ml$(3) !:
        let ml$(1)="Your percentage breakdowns total "&str$(tac)&"." !:
        let ml$(2)="The percentage breakdown must add to 100%." !:
        let ml$(3)="Correct the percentages." !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto EDIT_PAYEE
01410 SAVE_PAYEE: ! 
01420   if edit=1 and vn$<>holdvn$ then gosub KEY_CHANGE
01430   if edit=1 then !:
          rewrite #paymstr, using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
01440   if add=1 then !:
          write #paymstr,using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ duprec MSGBOX3
01450 ! 
01460   if ck=1 then goto MENU1
01470   goto MENU1
01480 ! ______________________________________________________________________
01490 KEY_CHANGE: ! 
01500 ! change the references to this file in the Transaction file
01510   open #trans:=fngethandle: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
01520   restore #trans,key>=holdvn$&rpt$(chr$(0),11): nokey EO_CHANGE_KEY_ON_TRANS
01530 L1530: read #trans,using 'Form Pos 28,C 8': x$ eof EO_CHANGE_KEY_ON_TRANS
01540   if x$=holdvn$ then !:
          rewrite #trans,using 'Form Pos 28,Cr 8',release: vn$ !:
          goto L1530
01550 EO_CHANGE_KEY_ON_TRANS: ! 
01560   close #trans: 
01570 ! 
01580 ! Change references to this file in the sub-file PayeeGLBreakdown
01590   restore #payeegl,key=holdvn$: nokey EO_CHANGE_KEY_ON_PAYEEGL
01600 L1600: read #payeegl,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYEEGL
01610   if x$=holdvn$ then !:
          rewrite #payeegl,using 'Form Pos 1,Cr 8': vn$ !:
          goto L1600
01620 EO_CHANGE_KEY_ON_PAYEEGL: ! 
01630 ! 
01640 ! Change references to this file in the linked file PayTrans
01650   open #paytrans:=fngethandle: "Name=Q:\CLmstr\Paytrans.h"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
01660   restore #paytrans,key>=holdvn$&rpt$(chr$(0),12): nokey EO_CHANGE_KEY_ON_PAYTRANS
01670 L1670: read #paytrans,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYTRANS
01680   if x$=holdvn$ then !:
          rewrite #paytrans,using 'Form Pos 1,Cr 8': vn$ !:
          goto L1670
01690 EO_CHANGE_KEY_ON_PAYTRANS: ! 
01700   close #paytrans: 
01710 ! 
01720 ! Change references to this file in the linked file UnPdAloc
01730   open #unpdaloc:=fngethandle: "Name=Q:\CLmstr\UnPdAloc.h"&str$(cno)&",KFName=Q:\CLmstr\UAIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
01740   restore #unpdaloc,key>=holdvn$&rpt$(chr$(0),kln(unpdaloc)-len(holdvn$)): nokey EO_CHANGE_KEY_ON_UNPDALOC
01750   read #unpdaloc,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_UNPDALOC
01760   if x$=holdvn$ then !:
          rewrite #unpdaloc,using 'Form Pos 1,Cr 8': vn$ !:
          goto L1670
01770 EO_CHANGE_KEY_ON_UNPDALOC: ! 
01780   close #unpdaloc: 
01790 ! 
01800   return 
01810 ! ______________________________________________________________________
01820 MSGBOX3: ! dupkey
01830   mat ml$(2) !:
        let ml$(1)="A record for payee number "&vn$&" already exists" !:
        let ml$(2)="You must select a different payee number." !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto EDIT_PAYEE
01840 ! ______________________________________________________________________
01850 ! <Updateable Region: ERTN>
01860 ERTN: let fnerror(cap$,err,line,act$,"xit")
01870   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01880   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01890   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01900 ERTN_EXEC_ACT: execute act$ : goto ERTN
01910 ! /region
01920 ! ______________________________________________________________________
01930 CHECK_HISTORY: ! 
01940   open #trans:=fngethandle: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,input,keyed 
01950   let fntos(sn$='payee_hist') !:
        let lc=0 : let mylen=25 : let mypos=mylen+2 : let width=50
01960   let lc+=1
01970   let fnlbl(lc+=1,30,'Check History Selection Criteria',width,center)
01980   let fnlbl(lc+=1,1,"Transaction Starting Date:",mylen,right)
01990   let fntxt(lc,mypos,8,0,left,'CCYYMMDD',0,'Blank for All') !:
        let resp$(1)=''
02000   let fnlbl(lc+=1,1,"Transaction Ending Date:",mylen,right)
02010   let fntxt(lc,mypos,8,0,left,'CCYYMMDD',0,'Blank for All') !:
        let resp$(2)=''
02020   let wbc=0: let bn$="[All]"
02030   let wit=0
02040   let tcde$="[All]]"
02050   let wpayee$=vn$
02060   let fnlbl(lc=6,40,'Transaction Grid')
02070   mat chdr$(11) : mat cmask$(11) : mat item6$(11) !:
        let chdr$(1)='Rec' : let chdr$(2)='Ck/Rf' : let chdr$(3)='Date' !:
        let chdr$(4)='Amount' : let chdr$(5)='Payee' !:
        let chdr$(6)='Name/Description' : let chdr$(7)='PC' !:
        let chdr$(8)='Stmt Clr Date' : let chdr$(9)='SC' !:
        let chdr$(10)='Bank' : let chdr$(11)='Type' !:
        let cmask$(1)=cmask$(2)='20' : let cmask$(3)='1' !:
        let cmask$(4)='10' : let cmask$(8)='1' !:
        let fnflexinit1('Gayee-'&str$(wbc)&'-'&str$(wtt),7,1,10,85,mat chdr$,mat cmask$,1,0,frame)
02080   let key$=vn$&cnvrt$('pic(Z#)',wbc)&cnvrt$("pic(#)",wtt)&rpt$(chr$(0),8) !:
        restore #trans,key>=key$: nokey EO_FLEX2 !:
        let transactionstotal=0
02090 READ_TRANS: ! 
02100   read #trans,using 'Form Pos 1,n 2,n 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof EO_FLEX2
02110   if trim$(vn$)<>trim$(tr$(4)) then goto EO_FLEX2
02120   if transactionstartingdate<>0 and transactionstartingdate>fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRANS
02130   if transactionendingdate<>0 and transactionendingdate<fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRANS
02140   let item6$(1)=str$(rec(trans)) : let item6$(2)=tr$(1) !:
        let item6$(3)=tr$(2) : let item6$(4)=str$(tr3) !:
        let item6$(5)=tr$(4) : let item6$(6)=tr$(5) !:
        let item6$(7)=str$(pcde) : let item6$(8)=str$(clr) !:
        let item6$(9)=str$(scd) : let item6$(10)=str$(bank_code) !:
        let item6$(11)=str$(tcde) !:
        let fnflexadd1(mat item6$) !:
        let transactionstotal+=tr3
02150   goto READ_TRANS
02160 EO_FLEX2: ! 
02170   let fnlbl(5,1,'Transactions Total:',mylen,right)
02180   let fntxt(5,mypos,12,0,right,"10",1,'This is the total of only the transactions shown in the Transaction Grid above. ') !:
        let resp$(3)=str$(transactionstotal)
02190   let fncmdkey('&Refresh',2,1,0,"If you select a date range, you must refresh the screen to see the transactions for that date range.") !:
        let fncmdkey('&Close',5,0,1)
02200   let fnacs(sn$,0,mat resp$,ck)
02210   if ck=5 or ck=cancel then goto EO_CHECK_HISTORY
02220   let transactionstartingdate=val(resp$(1)) !:
        let transactionendingdate=val(resp$(2))
02230   if ck=2 then goto CHECK_HISTORY ! goto the top of this function
02240 EO_CHECK_HISTORY: ! 
02250   close #trans: 
02260   return 
02270 ! ______________________________________________________________________
02280 GL_BREAKDOWNS: ! 
02290   let fntos(sn$='payee_gl_dist') !:
        let respc=0 : let mylen=28 : let mypos=mylen+2
02300   let fnlbl(1,25,"Breakdown for "&nam$(1:20),40)
02310   let fnlbl(3,1,"General Ledger Number:",mylen,right)
02320   let fnqgl(3,mypos) !:
        let resp$(respc+=1)=fnrgl$(payeegl$) ! think maybe here kj
02330   let fnlbl(4,1,'Percent:',mylen,right)
02340   let fntxt(4,mypos,6,0,0,'32',0,"Percent of total check to be charged to this g/l account.  Enter 25% as 25.00!") !:
        let resp$(respc+=1)=str$(percent)
02350   let fnlbl(5,1,"Description:",mylen,right)
02360   let fntxt(5,mypos,30) !:
        let resp$(respc+=1)=gldesc$
02370   let fncmdset(7)
02380   let fnacs(sn$,0,mat resp$,ck)
02390   if ck=5 then goto EDIT_PAYEE
02400   let payeekey$=vn$
02410   let payeegl$=fnagl$(resp$(1))
02420   let percent=val(resp$(2)) ! percent
02430   let gldesc$=resp$(3)
02440   if ck=4 and gldistrec>0 then !:
          delete #payeegl,rec=gldistrec: : goto EDIT_PAYEE
02450   if ck=1 and gldistrec=0 then !:
          write #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$
02460   if ck=1 and gldistrec>0 then !:
          rewrite #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: payeekey$,payeegl$,percent,gldesc$
02470   goto EDIT_PAYEE
02475   execute "Index Q:\CLmstr\payeeglbreakdown.H"&str$(cno)&" Q:\CLmstr\Payeeglbkdidx.H"&str$(cno)&" 1 8 Replace DupKeys -n"
02480 ! ______________________________________________________________________
02490 XIT: ! 
02500   close #trmstr2: ioerr L2510
02510 L2510: close #paymstr: ioerr L2520
02520 L2520: close #paymstr2: ioerr L2530
02530 L2530: close #payeegl: ioerr L2540
02540 L2540: close #citystzip: ioerr L2550
02550 L2550: fnend 
02560 ! ______________________________________________________________________
