00010 ! formerly S:\acsCL\Payee
00030   fn_setup
00090   fntop(program$,"Payee")
00100   fn_addpayee
00110   goto XIT
00130 XIT: let fnxit
00150 ! <Updateable Region: ERTN>
00160 ERTN: let fnerror(program$,err,line,act$,"xit")
00170   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00180   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00190   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00200 ERTN_EXEC_ACT: execute act$ : goto ERTN
00210 ! /region
12000 def fn_setup
12020   if ~setup then
12040     setup=1
12060     library 'S:\Core\Library': fnerror,fntos,fnlbl,fntxt,fncombof,fncmdset,fnacs,fnmsgbox,fnfra,fnbutton,fnflexinit1,fnflexadd1,fncmdkey,fndate_mmddyy_to_ccyymmdd,fngethandle,fnqgl,fnagl$,fnrgl$
12080     library 'S:\Core\Library': fntop,fnxit,fnerror
12100     on error goto ERTN
12120   end if
12140 fnend
14000 def library fnaddpayee
14020   if ~setup then let fn_setup
14040   fnaddpayee=fn_addpayee
14060 fnend
16000 def fn_addpayee
16070   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11
16080   dim tr$(5)*35,item6$(11)*35
16090   dim item$(11)*50,cmask$(11),chdr$(11)*20
16100   dim contact$*30,email$*50,fax$*12,myact$*20
16110   dim cap$*128,key$*19
16120   dim ml$(3)*70,citystzip$*30,glitem$(5)*30,payeekey$*8,payeegl$*12
16130   dim gldesc$*30,resp$(60)*50
16140   ! ______________________________________________________________________
18020   let left=0: let right=1
18040   open #trmstr2:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
18060   open #paymstr:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
18080   open #paymstr2:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
18100   open #payeegl:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayeeGLBreakdown.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\CLmstr\Payeeglbkdidx.h"&env$('cno')&",Shr",internal,outin,keyed 
18120   open #citystzip:=fngethandle: "Name="&env$('Q')&"\Data\CityStZip.dat,KFName="&env$('Q')&"\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outin,keyed 
18140   ! 
22000   MENU1: ! 
22020   let fntos(sn$="payee-1") 
22040   let respc=0
22050   fnlbl(1,1,'Payee')
22060   mat chdr$(12) : mat cmask$(12) : mat item$(12) 
22080   let chdr$(1)='Rec' 
22100   let chdr$(2)='Payee Number' 
22120   let chdr$(3)='Payee Name' 
22140   let chdr$(4)='Address' 
22160   let chdr$(5)='Address' 
22180   let chdr$(6)='City, ST Zip' 
22200   let chdr$(7)='Type' 
22220   let chdr$(8)='ID Number' 
22240   let chdr$(9)='Phone Number' 
22260   let chdr$(10)='Contact Name' 
22280   let chdr$(11)='E-mail' 
22300   let chdr$(12)='Fax'
22320   let cmask$(1)=cmask$(2)='' 
22340   let cmask$(3)=cmask$(4)=cmask$(5)=cmask$(6)='80' 
22360   let fnflexinit1('Payee',2,1,20,100,mat chdr$,mat cmask$,1,0,frame) 
22380   let editrec=0
22400   restore #paymstr: 
22420   READ_PAYMSTR_1: ! 
22440   read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',release: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ eof EO_FLEX1
22460   let item$(1)=str$(rec(paymstr)) 
22480   let item$(2)=vn$
22500   let item$(3)=nam$ 
22520   let item$(4)=ad1$ 
22540   let item$(5)=ad2$ 
22560   let item$(6)=csz$ 
22580   let item$(7)=str$(typ) 
22600   let item$(8)=ss$ 
22620   let item$(9)=ph$ 
22640   let item$(10)=contact$ 
22660   let item$(11)=email$ 
22680   let item$(12)=fax$ 
22700   let fnflexadd1(mat item$)
22720   goto READ_PAYMSTR_1
24000   EO_FLEX1: ! 
24020   let fncmdkey("&Add",1,0,0,"Add new payee records") 
24040   let fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit or press Alt+E to change any existing payee record.") 
24060   let fncmdkey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing payee record.") 
24080   let fncmdkey("E&xit",5,0,1,"Exit to menu")
24100   let fnacs(sn$,0,mat resp$,ck)
24120   let add=edit=0: let holdvn$=""
24140   if ck=5 then 
24160     goto PayeeXIT 
24180   else if ck=1 then 
24200     let add=1
24220     goto ADD_NEW_PAYEE
24240   end if
24260   if ck=2 or ck=3 then let editrec=val(resp$(1))
24280   if editrec=0 then goto MENU1
24300   if ck=2 or ck=3 then 
24320     read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',rec=editrec: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
24340   end if
24360   if ck=2 then let edit=1 : let holdvn$=vn$: goto EDIT_PAYEE
24380   if ck=3 then gosub DELETE_PAYEE : goto MENU1
24400   ! ______________________________________________________________________
26000   DELETE_PAYEE: ! r:
26020   ! check for Linked Unpaid Invoices 
26040   ! if there are any - than tell them, and don't delete.
26060   open #paytrans:=fngethandle: "Name="&env$('Q')&"\CLmstr\Paytrans.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
26080   restore #paytrans,key>=vn$&rpt$(chr$(0),12): nokey L490
26100   read #paytrans,using 'Form Pos 1,C 8',release: x$
26120   if x$=vn$ then 
26140     mat ml$(2) 
26160     let ml$(1)="A Unpaid Invoice for this payee exists" 
26180     let ml$(2)="You may not delete it." 
26200     let fnmsgbox(mat ml$,resp$,cap$,0) 
26220     goto EO_DELETE
26240   end if
26260   L490: ! 
26280   delete #paymstr,rec=editrec: 
26300   restore #payeegl,key>=vn$: nokey EO_DELETE_PAYEE
26320   DELETE_PAYEEGL_LOOP: ! 
26340   read #payeegl,using 'Form Pos 1,C 8': payeekey$ eof EO_DELETE_PAYEE
26360   if payeekey$=vn$ then 
26380     delete #payeegl: 
26400     goto DELETE_PAYEEGL_LOOP
26420   end if
26440   EO_DELETE_PAYEE: ! 
26460   ! 
26480   open #trans:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
26500   restore #trans, key>=holdvn$&rpt$(chr$(0),kln(trans)-len(holdvn$)): nokey EO_DEL_KEY_ON_TRANS
26520   L590: !
26540   read #trans,using 'Form Pos 28,C 8': x$ eof EO_DEL_KEY_ON_TRANS
26560   if x$=vn$ then 
26580     rewrite #trans,using 'Form Pos 28,Cr 8': '' 
26600     goto L590
26620   end if
26640   EO_DEL_KEY_ON_TRANS: ! 
26660   close #trans: 
26680   EO_DELETE: !
26700   return ! /r
26720   ! ______________________________________________________________________
28000   ADD_NEW_PAYEE: ! r:
28020   let vn$=nam$=ad1$=ad2$=csz$=ss$=ph$=contact$=email$=fax$=myact$="" 
28040   let typ=0
28060   goto EDIT_PAYEE ! /r
32000   EDIT_PAYEE: ! r:
32020   let fntos(sn$="payee-2") 
32040   let respc=0 
32060   let mylen=28 : let mypos=mylen+2
32080   let fnfra(1,1,12,70,"Payee Information"," ")
32100   let fnlbl(1,1,"Payee Number:",mylen,1,0,1)
32120   let fntxt(1,mypos,8,0,1,"",0,"",1) 
32140   let resp$(respc+=1)=vn$
32160   let fnlbl(2,1,"Payee Name:",mylen,1,0,1)
32180   let fntxt(2,mypos,30,0,0,"",0,"",1) 
32200   let resp$(respc+=1)=nam$
32220   let fnlbl(3,1,"Address:",mylen,1,0,1)
32240   let fntxt(3,mypos,30,0,0,"",0,"",1) 
32260   let resp$(respc+=1)=ad1$
32280   let fnlbl(4,1,"Address:",mylen,1,0,1)
32300   let fntxt(4,mypos,30,0,0,"",0,"",1) 
32320   let resp$(respc+=1)=ad2$
32340   let fnlbl(5,1,"City, St. Zip:",mylen,1,0,1)
32360   let fncombof("CityStZip",5,mypos,30,env$('Q')&"\Data\CityStZip.dat",1,30,0,0,env$('Q')&"\Data\CityStZip.idx",0,0, " ",1,0) 
32380   let resp$(respc+=1)=csz$
32400   let fnlbl(6,1,"Type:",mylen,1,0,1)
32420   let fncombof("Payeetype",6,mypos,27,env$('Q')&"\CLmstr\PayeeType.dat",1,2,3,25,"",0,0, "The payee type is a code used to detemine which box should be used on a 1099 misc form.  Only enter a type code if the payee should get a 1099",1) 
32440   let resp$(respc+=1)=str$(typ)
32460   let fnlbl(7,1,"Federal ID or SS No.",mylen,1,0,1)
32480   let fntxt(7,mypos,11,0,0,"",0,"",1) 
32500   let resp$(respc+=1)=ss$
32520   let fnlbl(8,1,"Phone Number:",mylen,1,0,1)
32540   let fntxt(8,mypos,12,0,0,"",0,"",1) 
32560   let resp$(respc+=1)=ph$
32580   let fnlbl(9,1,"Contact Name:",mylen,1,0,1)
32600   let fntxt(9,mypos,30,0,0,"",0,"",1) 
32620   let resp$(respc+=1)=contact$
32640   let fnlbl(10,1,"E-mail Address:",mylen,1,0,1)
32660   let fntxt(10,mypos,30,50,0,"",0,"",1) 
32680   let resp$(respc+=1)=email$
32700   let fnlbl(11,1,"Fax Number:",mylen,1,0,1)
32720   let fntxt(11,mypos,12,0,0,"",0,"",1) 
32740   let resp$(respc+=1)=fax$
32760   let fnlbl(12,1,"My Account Number:",mylen,1,0,1)
32780   let fntxt(12,mypos,20,0,0,"",0,"",1) 
32800   let resp$(respc+=1)=myact$
32820   let fnlbl(17,20,"Standard General Ledger Breakdowns",40,2,0,0)
32840   ! General Ledger Breakdown Grid
32860   mat chdr$(5) : mat cmask$(5) : mat glitem$(5) 
32880   let chdr$(1)='Refenence'
32900   let chdr$(2)='Payee Number' 
32920   let chdr$(3)='GL Number' 
32940   let chdr$(4)='Percent' 
32960   let chdr$(5)='Description'
32980   let cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)='' 
33000   let cmask$(4)='32' 
33020   let fnflexinit1('PayeeGl',17,1,5,70,mat chdr$,mat cmask$,1,0,0)
33040   if trim$(vn$)="" then goto EO_FLEX3
33060   restore #payeegl,key>=vn$: nokey EO_FLEX3
33080   READ_PAYEE_GL: ! 
33100   read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,gldesc$ eof EO_FLEX3
33120   if vn$<>payeekey$ then goto EO_FLEX3
33140   let glitem$(1)=str$(rec(payeegl)) 
33160   let glitem$(2)=payeekey$ 
33180   let glitem$(3)=payeegl$ 
33200   let glitem$(4)=str$(percent) 
33220   let glitem$(5)=gldesc$ 
33240   let fnflexadd1(mat glitem$)
33260   goto READ_PAYEE_GL
34000   EO_FLEX3: ! 
34040   let fnlbl(21,1,"",1,0,0,0) ! add space before buttons
34060   let fnbutton(lc=16,61,"Add",2,"Add a standard general ledger breakdowns",0,4) 
34080   let fnbutton(lc,67,"Edit",7,"Edit or Delete a standard general ledger breakdowns")
34100   let fncmdkey("Save",1,1,0,"Saves and returns to Vendor selection") 
34120   let fncmdkey("&Transactions",4,0,0,"List all checks for this payee") 
34140   let fncmdkey("&Cancel",5,0,1,"Return to Vendor selection")
34160   let fnacs(sn$,0,mat resp$,ck)
34180   if ck=5 then goto MENU1
34200   let vn$=lpad$(trim$(resp$(1)(1:8)),8) 
34220   let nam$=resp$(2) ! name 
34240   let ad1$=resp$(3) ! address 
34260   let ad2$=resp$(4) ! address 
34280   let csz$=resp$(5) ! city state zip
34300   if add=1 then goto L1190
34320   if edit=1 and holdvn$<>vn$ then goto L1190
34340   goto L1210
34360   L1190: read #paymstr,using 'Form Pos 1,C 8',key=vn$: oldvn$ nokey L1210
34380   if add=1 then goto L1205
34400   mat ml$(2) 
34420   let ml$(1)="You already have a payee number "&vn$ 
34440   let ml$(2)="Click ok to Cancel." 
34460   let fnmsgbox(mat ml$,resp$,cap$,16) 
34480   goto MENU1
36000   L1205: ! r:
36020   mat ml$(2) 
36040   let ml$(1)="You already have a payee number "&vn$ 
36060   let ml$(2)="Click ok to Change the number." 
36080   let fnmsgbox(mat ml$,resp$,cap$,16) 
36100   goto EDIT_PAYEE ! /r
36120   L1210: if trim$(vn$)="" then goto L1220 else goto L1230
38000   L1220: ! r:
38020   mat ml$(2) 
38040   let ml$(1)="You must have a unique payee number for ." 
38060   let ml$(2)="each vendor.  Click ok to assign a payee number" 
38080   let fnmsgbox(mat ml$,resp$,cap$,16) 
38100   goto EDIT_PAYEE ! /r
42000   L1230: !
42020   read #citystzip,using 'Form POS 1,C 30',key=rpad$(ltrm$(csz$),30),release: citystzip$ nokey L1240 
42040   goto L1250
42060   L1240: !
42080   write #citystzip,using 'Form POS 1,C 30': csz$
42100   L1250: !
42120   let typ=val(resp$(6)(1:2)) ! type 
42140   let ss$=resp$(7) ! ss or fed id 
42160   let ph$=resp$(8) ! phone 
42180   let contact$=resp$(9) ! contact name 
42200   let email$=resp$(10) ! email address
42220   let fax$=resp$(11) ! fax number 
42240   let myact$=resp$(12) ! my account number with this vendor 
42260   let gldistrec=val(resp$(13)) ! record number of gl distribution entry
42280   if ck=4 then 
42300     gosub CHECK_HISTORY 
42320     goto EDIT_PAYEE
42340   else if ck=2 then 
42360     let percent=gldistrec=0
42380     let payeekey$=gldesc$=payeegl$="" 
42400     goto GL_BREAKDOWNS ! add gl breakdown
42420   else if ck=7 then 
42440     read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec,release: payeekey$,payeegl$,percent,gldesc$ 
42460     goto GL_BREAKDOWNS ! edit gl breakdown
42480   end if
42500   let tac=0
44000   ! READ_STANDARD_BREAKDOWNS: ! 
44020   restore #payeegl,key>=vn$: nokey EO_TEST
44040   do
44042     read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,gldesc$ eof EO_TEST
44060     if vn$=payeekey$ then let tac+=percent
44100   loop while vn$=payeekey$
44120   EO_TEST: ! 
44140   if tac=100 or tac=0 then goto SAVE_PAYEE
46000   ! MSGBOX4: ! percent breakdown doesn't add to 100 %
46020   mat ml$(3) 
46040   let ml$(1)="Your percentage breakdowns total "&str$(tac)&"." 
46060   let ml$(2)="The percentage breakdown must add to 100%." 
46080   let ml$(3)="Correct the percentages." 
46100   let fnmsgbox(mat ml$,resp$,cap$,16) 
46120   goto EDIT_PAYEE ! /r
48000   SAVE_PAYEE: ! r:
48020   if edit=1 and vn$<>holdvn$ then gosub KEY_CHANGE
48040   if edit=1 then 
48060     rewrite #paymstr, using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
48080   else if add=1 then 
48100     write #paymstr,using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ duprec MSGBOX3
48120   end if
48140   goto MENU1 ! /r
52000   KEY_CHANGE: ! r:
52020   ! change the references to this file in the Transaction file
52040   open #trans:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
52060   restore #trans,key>=holdvn$&rpt$(chr$(0),11): nokey EO_CHANGE_KEY_ON_TRANS
52080   L1530: read #trans,using 'Form Pos 28,C 8': x$ eof EO_CHANGE_KEY_ON_TRANS
52100   if x$=holdvn$ then 
52120     rewrite #trans,using 'Form Pos 28,Cr 8',release: vn$ 
52140     goto L1530
52160   end if
52180   EO_CHANGE_KEY_ON_TRANS: ! 
52200   close #trans: 
52220   ! 
52240   ! Change references to this file in the sub-file PayeeGLBreakdown
52260   restore #payeegl,key=holdvn$: nokey EO_CHANGE_KEY_ON_PAYEEGL
52280   L1600: read #payeegl,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYEEGL
52300   if x$=holdvn$ then 
52320     rewrite #payeegl,using 'Form Pos 1,Cr 8': vn$ 
52340     goto L1600
52360   end if
52380   EO_CHANGE_KEY_ON_PAYEEGL: ! 
52400   ! 
52420   ! Change references to this file in the linked file PayTrans
52440   open #paytrans:=fngethandle: "Name="&env$('Q')&"\CLmstr\Paytrans.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
52460   restore #paytrans,key>=holdvn$&rpt$(chr$(0),12): nokey EO_CHANGE_KEY_ON_PAYTRANS
52480   L1670: !
52500   read #paytrans,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYTRANS
52520   if x$=holdvn$ then 
52540     rewrite #paytrans,using 'Form Pos 1,Cr 8': vn$ 
52560     goto L1670
52580   end if
52600   EO_CHANGE_KEY_ON_PAYTRANS: ! 
52620   close #paytrans: 
52640   ! 
52660   ! Change references to this file in the linked file UnPdAloc
52680   open #unpdaloc:=fngethandle: "Name="&env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UAIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
52700   restore #unpdaloc,key>=holdvn$&rpt$(chr$(0),kln(unpdaloc)-len(holdvn$)): nokey EO_CHANGE_KEY_ON_UNPDALOC
52720   read #unpdaloc,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_UNPDALOC
52740   if x$=holdvn$ then 
52760     rewrite #unpdaloc,using 'Form Pos 1,Cr 8': vn$ 
52780     goto L1670
52800   end if
52820   EO_CHANGE_KEY_ON_UNPDALOC: ! 
52840   close #unpdaloc: 
52860   ! 
52880   return ! /r
54000   MSGBOX3: ! r: dupkey
54020   mat ml$(2) 
54040   let ml$(1)="A record for payee number "&vn$&" already exists" 
54060   let ml$(2)="You must select a different payee number." 
54080   let fnmsgbox(mat ml$,resp$,cap$,16) 
54100   goto EDIT_PAYEE ! /r
58000   CHECK_HISTORY: ! r:
58020   open #trans:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,input,keyed 
58040   let fntos(sn$='payee_hist') 
58060   let lc=0 : let mylen=25 : let mypos=mylen+2 : let width=50
58080   let lc+=1
58100   let fnlbl(lc+=1,30,'Check History Selection Criteria',width,center)
58120   let fnlbl(lc+=1,1,"Transaction Starting Date:",mylen,right)
58140   let fntxt(lc,mypos,8,0,left,'CCYYMMDD',0,'Blank for All') 
58160   let resp$(1)=''
58180   let fnlbl(lc+=1,1,"Transaction Ending Date:",mylen,right)
58200   let fntxt(lc,mypos,8,0,left,'CCYYMMDD',0,'Blank for All') 
58220   let resp$(2)=''
58240   let wbc=0
58320   let fnlbl(lc=6,40,'Transaction Grid')
58340   mat chdr$(11) : mat cmask$(11) : mat item6$(11) 
58360   let chdr$(1)='Rec' 
58380   let chdr$(2)='Ck/Rf'
58390   let chdr$(3)='Date' 
58400   let chdr$(4)='Amount' 
58420   let chdr$(5)='Payee' 
58440   let chdr$(6)='Name/Description' 
58460   let chdr$(7)='PC' 
58480   let chdr$(8)='Stmt Clr Date' 
58500   let chdr$(9)='SC' 
58520   let chdr$(10)='Bank' 
58540   let chdr$(11)='Type' 
58560   let cmask$(1)=cmask$(2)='20' 
58580   let cmask$(3)='1' 
58600   let cmask$(4)='10' 
58620   let cmask$(8)='1' 
58640   let fnflexinit1('Gayee-'&str$(wbc)&'-'&str$(wtt),7,1,10,85,mat chdr$,mat cmask$,1,0,frame)
58660   let key$=vn$&cnvrt$('pic(Z#)',wbc)&cnvrt$("pic(#)",wtt)&rpt$(chr$(0),8) 
58680   restore #trans,key>=key$: nokey EO_FLEX2 
58700   let transactionstotal=0
58720   READ_TRANS: ! 
58740   read #trans,using 'Form Pos 1,n 2,n 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof EO_FLEX2
58760   if trim$(vn$)<>trim$(tr$(4)) then goto EO_FLEX2
58780   if transactionstartingdate<>0 and transactionstartingdate>fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRANS
58800   if transactionendingdate<>0 and transactionendingdate<fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRANS
58820   let item6$(1)=str$(rec(trans)) : let item6$(2)=tr$(1) 
58840   let item6$(3)=tr$(2) : let item6$(4)=str$(tr3) 
58860   let item6$(5)=tr$(4) : let item6$(6)=tr$(5) 
58880   let item6$(7)=str$(pcde) : let item6$(8)=str$(clr) 
58900   let item6$(9)=str$(scd) : let item6$(10)=str$(bank_code) 
58920   let item6$(11)=str$(tcde) 
58940   let fnflexadd1(mat item6$) 
58960   let transactionstotal+=tr3
58980   goto READ_TRANS
59000   EO_FLEX2: ! 
59020   let fnlbl(5,1,'Transactions Total:',mylen,right)
59040   let fntxt(5,mypos,12,0,right,"10",1,'This is the total of only the transactions shown in the Transaction Grid above. ') 
59060   let resp$(3)=str$(transactionstotal)
59080   let fncmdkey('&Refresh',2,1,0,"If you select a date range, you must refresh the screen to see the transactions for that date range.") 
59100   let fncmdkey('&Close',5,0,1)
59120   let fnacs(sn$,0,mat resp$,ck)
59140   if ck=5 or ck=cancel then goto EO_CHECK_HISTORY
59160   let transactionstartingdate=val(resp$(1)) 
59180   let transactionendingdate=val(resp$(2))
59200   if ck=2 then goto CHECK_HISTORY ! goto the top of this function
59220   EO_CHECK_HISTORY: ! 
59240   close #trans: 
59260   return 
59280   ! ______________________________________________________________________
62000   GL_BREAKDOWNS: ! r:
62020   let fntos(sn$='payee_gl_dist') 
62040   let respc=0 : let mylen=28 : let mypos=mylen+2
62060   let fnlbl(1,25,"Breakdown for "&nam$(1:20),40)
62080   let fnlbl(3,1,"General Ledger Number:",mylen,right)
62100   let fnqgl(3,mypos) 
62120   let resp$(respc+=1)=fnrgl$(payeegl$) ! think maybe here kj
62140   let fnlbl(4,1,'Percent:',mylen,right)
62160   let fntxt(4,mypos,6,0,0,'32',0,"Percent of total check to be charged to this g/l account.  Enter 25% as 25.00!") 
62180   let resp$(respc+=1)=str$(percent)
62200   let fnlbl(5,1,"Description:",mylen,right)
62220   let fntxt(5,mypos,30) 
62240   let resp$(respc+=1)=gldesc$
62260   let fncmdset(7)
62280   let fnacs(sn$,0,mat resp$,ck)
62300   if ck=5 then goto EDIT_PAYEE
62320   let payeekey$=vn$
62340   let payeegl$=fnagl$(resp$(1))
62360   let percent=val(resp$(2)) ! percent
62380   let gldesc$=resp$(3)
62400   if ck=4 and gldistrec>0 then 
62420     delete #payeegl,rec=gldistrec:
62460   else if ck=1 and gldistrec=0 then 
62480     write #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$
62500   else if ck=1 and gldistrec>0 then 
62520     rewrite #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: payeekey$,payeegl$,percent,gldesc$
62540   end if
62560   goto EDIT_PAYEE ! /r
62580   ! execute "Index "&env$('Q')&"\CLmstr\payeeglbreakdown.H"&env$('cno')&' '&env$('Q')&"\CLmstr\Payeeglbkdidx.H"&env$('cno')&" 1 8 Replace DupKeys -n"
62600   ! ______________________________________________________________________
64000   PayeeXIT: ! 
64020   close #trmstr2: ioerr ignore
64040   close #paymstr: ioerr ignore
64060   close #paymstr2: ioerr ignore
64080   close #payeegl: ioerr ignore
64100   close #citystzip: ioerr ignore
64120 fnend 
