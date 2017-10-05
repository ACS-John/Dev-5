00010 ! Replace S:\acsCL\fnReceipt
00020 ! Standard Receipt file
00030   def library fnaddreceipt
00040     library 'S:\Core\Library': fncno,fndat,fnerror,fntos,fnlbl,fntxt,fncmdset,fnacs,fnmsgbox,fnwait,fnfra,fnbutton,fnflexinit1,fnflexadd1,fncmdkey,fndate_mmddyy_to_ccyymmdd,fngethandle,fnqgl,fnagl$,fnrgl$
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim rec$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,vcode$*8
00080     dim tr$(5)*35,item6$(11)*35
00090     dim item$(11)*50,cmask$(11),chdr$(11)*20
00100     dim contact$*30,email$*50,fax$*12,myact$*20
00110     dim cap$*128,key$*19
00120     dim ml$(3)*70,citystzip$*30,glitem$(5)*30,receiptkey$*8,receiptgl$*12
00130     dim gldesc$*30,resp$(60)*50
00140 ! ______________________________________________________________________
00150     fncno(cno)
00155     execute "Index "&env$('Q')&"\CLmstr\Recmstr.h"&str$(cno)&' '&env$('Q')&"\CLmstr\Recidx1.h"&str$(cno)&" 1 8 Replace DupKeys,Shr" ioerr ignore
00160     left=0: right=1
00170     open #trmstr2:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00180     if exists(env$('Q')&"\CLmstr\RECmstr.H"&str$(cno))=0 then gosub CREATERECEIPTFILE
00190     open #receipt:=fngethandle: "Name="&env$('Q')&"\CLmstr\recmstr.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\CLmstr\recidx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00200     open #receiptgl:=fngethandle: "Name="&env$('Q')&"\CLmstr\ReceiptGLBreakdown.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\CLmstr\Receiptglbkdidx.h"&str$(cno)&",Use,RecL=56,KPs=1,KLn=8,Shr",internal,outin,keyed 
00210     open #citystzip:=fngethandle: "Name="&env$('Q')&"\Data\CityStZip.dat,KFName="&env$('Q')&"\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outin,keyed 
00220 ! 
00230 MENU1: ! 
00240     fntos(sn$="Receipt1") !:
          respc=0
00250     mat chdr$(3) : mat cmask$(3) : mat item$(3) !:
          chdr$(1)='Rec' !:
          chdr$(2)='Receipt Type' : chdr$(3)='Description'
00260     cmask$(1)=cmask$(2)='' !:
          cmask$(3)="80" !:
          fnflexinit1('Receipt1',1,1,10,35,mat chdr$,mat cmask$,1,0,frame) !:
          editrec=0
00270     restore #receipt: 
00280 READ_RECEIPT_1: ! 
00290     read #receipt,using 'Form Pos 1,C 8,c 30,',release: rec$,nam$ eof EO_FLEX1
00300     item$(1)=str$(rec(receipt)) !:
          item$(2)=rec$ : item$(3)=nam$ !:
          fnflexadd1(mat item$)
00310     goto READ_RECEIPT_1
00320 EO_FLEX1: ! 
00330     fncmdkey("&Add",1,0,0,"Add new receipt records") !:
          fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit or press Alt+E to change any existing receipt record.") !:
          fncmdkey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing receipt record.") !:
          fncmdkey("E&xit",5,0,1,"Exit to menu")
00340     fnacs(sn$,0,mat resp$,ck)
00350     add=edit=0
00360     if ck=5 then goto XIT !:
          else if ck=1 then add=1: goto ADD_NEW_RECEIPT
00370   if ck=2 or ck=3 then editrec=val(resp$(1))
00380   if editrec=0 then goto MENU1
00390   if ck=2 or ck=3 then !:
          read #receipt,using 'Form Pos 1,C 8,c 30',rec=editrec: rec$,nam$
00400   if ck=2 then edit=1 : goto EDIT_RECEIPT
00410   if ck=3 then gosub DELETE_RECEIPT : goto MENU1
00420 ! ______________________________________________________________________
00430 DELETE_RECEIPT: ! 
00440 ! check for Linked Unpaid Invoices !:
        ! if there are any - than tell them, and don't delete.
00450   open #paytrans:=fngethandle: "Name="&env$('Q')&"\CLmstr\Paytrans.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00460   restore #paytrans,key>=rec$&rpt$(chr$(0),12): nokey L490
00470   read #paytrans,using 'Form Pos 1,C 8': x$
00480   if x$=rec$ then !:
          mat ml$(2) !:
          ml$(1)="A Unpaid Invoice for this Receipt exists" !:
          ml$(2)="You may not delete it." !:
          fnmsgbox(mat ml$,resp$,cap$,0) !:
          goto EO_DELETE
00490 L490: ! 
00500   delete #receipt,rec=editrec: 
00510   restore #receiptgl,key>=rec$: nokey EO_DELETE_RECEIPT
00520 DELETE_RECEIPTGL_LOOP: ! 
00530   read #receiptgl,using 'Form Pos 1,C 8': receiptkey$ eof EO_DELETE_RECEIPT
00540   if receiptkey$=rec$ then !:
          delete #receiptgl: !:
          goto DELETE_RECEIPTGL_LOOP
00550 EO_DELETE_RECEIPT: ! 
00560 ! 
00570   open #trans:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00580   restore #trans, key>=holdrec$&rpt$(chr$(0),kln(trans)-len(holdrec$)): nokey EO_DEL_KEY_ON_TRANS
00590 L590: read #trans,using 'Form Pos 28,C 8': x$ eof EO_DEL_KEY_ON_TRANS
00600   if x$=rec$ then !:
          rewrite #trans,using 'Form Pos 28,Cr 8': '' !:
          goto L590
00610 EO_DEL_KEY_ON_TRANS: ! 
00620   close #trans: 
00630 EO_DELETE: return 
00640 ! ______________________________________________________________________
00650 ADD_NEW_RECEIPT: ! 
00660   rec$=nam$=""
00670   goto EDIT_RECEIPT
00680 ! ______________________________________________________________________
46000 EDIT_RECEIPT: ! 
46020   holdrec$=rec$
46040   fntos(sn$="Receipt2")
46060   respc=0
46080   mylen=28 : mypos=mylen+2
46100   fnfra(1,1,12,70,"Receipt Information"," ")
46120   fnlbl(1,1,"Receipt Type:",mylen,1,0,1)
46140   fntxt(1,mypos,8,0,1,"",0,"If a deposit ticket normally contains the same general breakdowns, you can create a receipt record for quickly displaying the breakdowns.  Assign each type of deposit a receipt type code.",1)
46160   resp$(respc+=1)=rec$
46180   fnlbl(2,1,"Description:",mylen,1,0,1)
46200   fntxt(2,mypos,30,0,0,"",0,"",1)
46220   resp$(respc+=1)=nam$
46240   fnlbl(15,20,"Standard General Ledger Breakdowns",40,2,0,0)
46260 ! General Ledger Breakdown GridPE)
46280   mat chdr$(5)
46300   chdr$(1)='Refenence'
46320   chdr$(2)='Receipt Type'
46340   chdr$(3)='GL Number'
46360   chdr$(4)='Percent'
46380   chdr$(5)='Description'
46400   mat cmask$(5)
46420   cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)=''
46440   cmask$(4)='32'
46460   mat glitem$(5)
46480   fnflexinit1('ReceiptGl',17,1,5,70,mat chdr$,mat cmask$,1,0,0)
46500   if trim$(rec$)="" then goto EO_FLEX3
46520   restore #receiptgl,search>=rec$: nokey EO_FLEX3
48000 READ_RECEIPT_GL: ! 
48020   read #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': receiptkey$,receiptgl$,percent,gldesc$ eof EO_FLEX3
48040   if rec$<>receiptkey$ then goto EO_FLEX3
48060   let glitem$(1)=str$(rec(receiptgl)) : let glitem$(2)=receiptkey$
48080   let glitem$(3)=receiptgl$ : let glitem$(4)=str$(percent)
48100   let glitem$(5)=gldesc$
48120   fnflexadd1(mat glitem$)
48140   goto READ_RECEIPT_GL
50000 EO_FLEX3: ! 
50020   pas=1 ! don't redo combo boxes on gl
50040   fnlbl(16,1,"",1,0,0,0) ! add space before buttons
50060   fnbutton(16,61,"Add",2,"Add a standard general ledger breakdown",0,4)
50080   fnbutton(16,67,"Edit",7,"Edit or Delete a standard general ledger breakdown")
50100   fncmdkey("Save",1,1,0,"Saves and returns to Receipt selection")
50120   fncmdkey("&Cancel",5,0,1,"Return to Receipt selection")
50140   fnacs(sn$,0,mat resp$,ck)
50160   if ck=5 then goto MENU1
50180   rec$=lpad$(trim$(resp$(1)(1:8)),8)
50200   nam$=resp$(2) ! name
50220   let gldistrec=val(resp$(3)) ! record number of gl distribution entry
50240   if ck=2 then 
50260     percent=gldistrec=0: receiptkey$=gldesc$=receiptgl$=""
50280     goto GL_BREAKDOWNS ! add gl breakdown
50300   else if ck=7 then 
50320     read #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: receiptkey$,receiptgl$,percent,gldesc$
50340     goto GL_BREAKDOWNS ! edit gl breakdown
50360   end if 
52000   tac=0
52020 READ_STANDARD_BREAKDOWNS: ! 
52040   restore #receiptgl,key>=rec$: nokey EO_TEST
52060 L1020: read #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': receiptkey$,receiptgl$,percent,gldesc$ eof EO_TEST
52080   if rec$<>receiptkey$ then goto EO_TEST
52100   tac+=percent
52120   goto L1020
52140 EO_TEST: ! 
52160   if tac=100 or tac=0 then goto SAVE_RECEIPT
54000 MSGBOX4: ! percent breakdown doesn't add to 100 %
54020   mat ml$(3)
54040   ml$(1)="Your percentage breakdowns total "&str$(tac)&"."
54060   ml$(2)="The percentage breakdown must add to 100%."
54080   ml$(3)="Correct the percentages."
54100   fnmsgbox(mat ml$,resp$,cap$,16)
54120   goto EDIT_RECEIPT
56000 SAVE_RECEIPT: ! 
56020   if edit=1 and rec$<>holdrec$ then gosub KEY_CHANGE
56040   if edit=1 then 
56060     rewrite #receipt, using 'Form Pos 1,Cr 8,C 30': rec$,nam$
56080   else if add=1 then 
56100     write #receipt,using 'Form Pos 1,Cr 8,C 30': rec$,nam$ duprec MSGBOX3
56120   end if 
56140 ! 
56160   if ck=1 then goto MENU1
56180   goto MENU1
56200 ! ______________________________________________________________________
58000 KEY_CHANGE: !  don't do on receipts
58020   goto L1500 ! don't change any other files
58040 ! change the references to this file in the Transaction file
58060   open #trans:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
58080   restore #trans,key>=holdrec$&rpt$(chr$(0),11): nokey EO_CHANGE_KEY_ON_TRANS
58100 L1230: ! 
58120   read #trans,using 'Form Pos 28,C 8': x$ eof EO_CHANGE_KEY_ON_TRANS
58140   if x$=holdrec$ then 
58160     rewrite #trans,using 'Form Pos 28,Cr 8',release: rec$
58180     goto L1230
58200   end if 
58220 EO_CHANGE_KEY_ON_TRANS: ! 
58240   close #trans: 
58260 ! 
58280 ! Change references to this file in the sub-file ReceiptGLBreakdown
58300   restore #receiptgl,search=holdrec$: nokey EO_CHANGE_KEY_ON_RECEIPTGL
58320 L1300: read #receiptgl,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_RECEIPTGL
58340   if x$=holdrec$ then 
58360     rewrite #receiptgl,using 'Form Pos 1,Cr 8': rec$
58380     goto L1300
58400   end if 
58420 EO_CHANGE_KEY_ON_RECEIPTGL: ! 
58440 ! 
58460 ! Change references to this file in the linked file PayTrans
58480   open #paytrans:=fngethandle: "Name="&env$('Q')&"\CLmstr\Paytrans.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
58500   restore #paytrans,key>=holdrec$&rpt$(chr$(0),12): nokey EO_CHANGE_KEY_ON_PAYTRANS
58520 L1370: read #paytrans,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYTRANS
58540   if x$=holdrec$ then 
58560     rewrite #paytrans,using 'Form Pos 1,Cr 8': rec$
58580     goto L1370
58600   end if 
58620 EO_CHANGE_KEY_ON_PAYTRANS: ! 
58640   close #paytrans: 
58660 ! 
58680 ! Change references to this file in the linked file UnPdAloc
58700   open #unpdaloc:=fngethandle: "Name="&env$('Q')&"\CLmstr\UnPdAloc.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UAIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
58720   restore #unpdaloc,key>=holdrec$&rpt$(chr$(0),kln(unpdaloc)-len(holdrec$)): nokey EO_CHANGE_KEY_ON_UNPDALOC
58740   read #unpdaloc,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_UNPDALOC
58760   if x$=holdrec$ then 
58780     rewrite #unpdaloc,using 'Form Pos 1,Cr 8': rec$
58800     goto L1370
58820   end if 
58840 EO_CHANGE_KEY_ON_UNPDALOC: ! 
58860   close #unpdaloc: 
58880 ! 
58900 L1500: ! 
58920   return 
58940 ! ______________________________________________________________________
60000 MSGBOX3: ! r: dupkey
60020   mat ml$(2)
60040   ml$(1)="A record for receipt type "&rec$&" already exists"
60060   ml$(2)="You must select a different receipt type."
60080   fnmsgbox(mat ml$,resp$,cap$,16)
60100   goto EDIT_RECEIPT ! /r
60120 ! ______________________________________________________________________
62000 ! <Updateable Region: ERTN>
62020 ERTN: fnerror(program$,err,line,act$,"xit")
62040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
62060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
62080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
62100 ERTN_EXEC_ACT: execute act$ : goto ERTN
62120 ! /region
64000 GL_BREAKDOWNS: ! r:
64020   fntos(sn$='receipt_gl_dist')
64040   respc=0 : mylen=28 : mypos=mylen+2
64060   fnlbl(1,25,"Breakdown for "&nam$(1:20),40)
64080   fnlbl(3,1,"General Ledger Number:",mylen,right)
64100   fnqgl(3,mypos)
64120   resp$(respc+=1)=fnrgl$(receiptgl$) ! think maybe here kj
64140   fnlbl(4,1,'Percent:',mylen,right)
64160   fntxt(4,mypos,6,0,0,'32',0,"Percent of total check to be charged to this g/l account.  Enter 25% as 25.00!")
64180   resp$(respc+=1)=str$(percent)
64200   fnlbl(5,1,"Description:",mylen,right)
64220   fntxt(5,mypos,30)
64240   resp$(respc+=1)=gldesc$
64260   fncmdset(7)
64280   fnacs(sn$,0,mat resp$,ck)
64300   if ck=5 then goto EDIT_RECEIPT
64320   receiptkey$=rec$
64340   receiptgl$=fnagl$(resp$(1))
64360   percent=val(resp$(2)) ! percent
64380   let gldesc$=resp$(3)
64400   if ck=4 and gldistrec>0 then 
64420     delete #receiptgl,rec=gldistrec: 
64460   else if ck=1 and gldistrec=0 then 
64500     write #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': receiptkey$,receiptgl$,percent,gldesc$
64520   else if ck=1 and gldistrec>0 then 
64560     rewrite #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: receiptkey$,receiptgl$,percent,gldesc$
64580   end if 
64600   goto EDIT_RECEIPT
64620 ! /r
66000 XIT: ! 
66020   close #trmstr2: ioerr ignore
66040   close #receipt: ioerr ignore
66060   close #receiptgl: ioerr ignore
66080   close #citystzip: ioerr ignore
66100 fnend 
66120 IGNORE: continue 
68000 CREATERECEIPTFILE: ! r:
68020 open #receipt:=fngethandle: "Name="&env$('Q')&"\CLmstr\recmstr.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\CLmstr\recidx1.h"&str$(cno)&",REPLACE,RecL=38,KPS=1,KLN=8",internal,outin,keyed 
68040 close #receipt: 
68060 execute "Index "&env$('Q')&"\CLmstr\Recmstr.h"&str$(cno)&' '&env$('Q')&"\CLmstr\Recidx1.h"&str$(cno)&" 1 8 Replace DupKeys,Shr"
68080 execute "Index "&env$('Q')&"\CLmstr\Receiptglbreakdown.h"&str$(cno)&' '&env$('Q')&"\CLmstr\receiptglbkdidx.h"&str$(cno)&" 1 8 Replace DupKeys,Shr"
68100 return  ! /r
