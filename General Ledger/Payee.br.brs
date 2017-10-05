00010 ! formerly S:\acsGL\fnglPayee and S:\acsGL\glPayee
00020 ! Payee File for general ledger system and home to the fnAddGLPayee function
00050 let fn_setup
00090 let fntop(program$)
00110 let fn_addglpayee
00120 goto XIT
00140 XIT: fnxit
00200 ignore: continue
18000 def fn_setup
18020   if ~setup then 
18040     setup=1
18060     library 'S:\Core\Library': fntop,fnxit,fnerror,fnindex_it,fnstatus_close
18080     library 'S:\Core\Library': fntos,fnlbl,fntxt,fncombof,fncmdset,fnacs,fnmsgbox,fnfra,fnbutton,fnflexinit1,fnflexadd1,fncmdkey,fndate_mmddyy_to_ccyymmdd,fngethandle,fnqgl,fnagl$,fnrgl$
18100     left=0: right=1
18140   end if
18160 fnend
22000 def library fnaddglpayee
22020   fn_setup
22040   fnaddglpayee=fn_addglpayee
22060 fnend
24000 def fn_addglpayee
24020   fn_setup
24060   ! ______________________________________________________________________
24080   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,de$*30
24100   dim item6$(11)*35
24120   dim item$(11)*50,cmask$(11),chdr$(11)*20
24140   dim contact$*30,email$*50,fax$*12,myact$*20
24160   dim cap$*128,key$*19
24180   dim ml$(3)*70,citystzip$*30,glitem$(5)*30,payeekey$*8,payeegl$*12
24200   dim gldesc$*30,resp$(60)*50
24220   ! r: setup files
26000   if ~exists(env$('Q')&"\GLmstr\paymstr.h"&env$('cno')) then 
26020     open #paymstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\PayMstr.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\GLmstr\PayIdx1.h"&env$('cno')&",RecL=276,kln=8,kps=1,Replace",internal,outin,keyed 
26040     close #paymstr: ioerr ignore
26060   end if
26080   if ~exists(env$('Q')&"\GLmstr\PayIdx1.h"&env$('cno')) or ~exists(env$('Q')&"\GLmstr\PayIdx2.h"&env$('cno')) then 
26100     fnindex_it(env$('Q')&'\GLmstr\paymstr.H'&env$('cno'),env$('Q')&'\GLmstr\Payidx1.H'&env$('cno'),'1 8 ')
26120     fnindex_it(env$('Q')&'\GLmstr\paymstr.H'&env$('cno'),env$('Q')&'\GLmstr\Payidx2.H'&env$('cno'),'9 38')
26140     fnstatus_close
26160   end if
26180   open #paymstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
26200   open #paymstr2:=fngethandle: "Name="&env$('Q')&"\GLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PayIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
26220   open #payeegl:=fngethandle: "Name="&env$('Q')&"\GLmstr\PayeeGLBreakdown.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\GLmstr\Payeeglbkdidx.h"&env$('cno')&",Use,RecL=56,KPs=1,KLn=8,Shr",internal,outin,keyed 
26240   open #citystzip:=fngethandle: "Name="&env$('Q')&"\Data\CityStZip.dat,KFName="&env$('Q')&"\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outin,keyed 
26260   ! /r
28000   MENU1: ! r:
28020     fntos(sn$="payee-1")
28040     respc=0
28060     mat chdr$(12) : mat cmask$(12) : mat item$(12)
28080     chdr$(1)='Rec' 
28100     chdr$(2)='Payee Number' 
28120     chdr$(3)='Payee Name'
28140     chdr$(4)='Address' 
28160     chdr$(5)='Address'
28180     chdr$(6)='City, ST Zip' 
28200     chdr$(7)='Type'
28220     chdr$(8)='ID Number' 
28240     chdr$(9)='Phone Number'
28260     chdr$(10)='Contact Name' 
28280     chdr$(11)='E-mail'
28300     chdr$(12)='Fax'
28320     cmask$(1)=cmask$(2)=''
28340     cmask$(3)=cmask$(4)=cmask$(5)=cmask$(6)='80'
28360     fnflexinit1('Hayee',1,1,20,100,mat chdr$,mat cmask$,1,0,frame)
28380     editrec=0
28400     restore #paymstr: 
28420     do
28440       read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',release: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ eof EO_FLEX1
28460       item$(1)=str$(rec(paymstr))
28480       item$(2)=vn$ 
28500       item$(3)=nam$ 
28520       item$(4)=ad1$
28540       item$(5)=ad2$ 
28560       item$(6)=csz$ 
28580       item$(7)=str$(typ)
28600       item$(8)=ss$ 
28620       item$(9)=ph$
28640       item$(10)=contact$ 
28660       item$(11)=email$
28680       item$(12)=fax$
28700       fnflexadd1(mat item$)
28720     loop
28740     EO_FLEX1: ! 
28760     fncmdkey("&Add",1,0,0,"Add new payee records") 
28780     fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit or press Alt+E to change any existing payee record.") 
28800     fncmdkey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing payee record.") 
28820     fncmdkey("E&xit",5,0,1,"Exit to menu")
28840     fnacs(sn$,0,mat resp$,ck)
28860     add=edit=0
28880     if ck=5 then 
28900       goto XitFn
28920     else if ck=1 then 
28940       add=1
28960       goto ADD_NEW_PAYEE
28980     else if ck=2 or ck=3 then 
29000       editrec=val(resp$(1))
29020     end if
29040     if editrec=0 then goto MENU1
29060     if ck=2 or ck=3 then 
29080       read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',rec=editrec: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
29100     end if
29120     if ck=2 then 
29140       edit=1 
29160       goto EDIT_PAYEE
29180     end if
29200     if ck=3 then gosub DELETE_PAYEE : goto MENU1
29220   goto MENU1 ! /r
32000   DELETE_PAYEE: ! r: a gosub routine
32020     delete #paymstr,rec=editrec: 
32040     restore #payeegl,key>=vn$: nokey EO_DELETE_PAYEE
32060     do 
32080       read #payeegl,using 'Form Pos 1,C 8': payeekey$ eof EO_DELETE_PAYEE
32100       if payeekey$=vn$ then 
32120         delete #payeegl: 
32140       end if
32160     loop
32180     EO_DELETE_PAYEE: ! 
32200     ! 
32220     open #trans:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLTR1099.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\gltrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
32240     if trim$(holdvn$)="" then goto EO_DEL_KEY_ON_TRANS
32260     restore #trans, key>=holdvn$: nokey EO_DEL_KEY_ON_TRANS
32280     L570: !
32300     read #trans,using 'Form Pos 1,C 8': trx$ eof EO_DEL_KEY_ON_TRANS
32320     if trx$=vn$ then 
32340       rewrite #trans,using 'Form Pos 1,Cr 8': '' 
32360       goto L570
32380     end if
32400     EO_DEL_KEY_ON_TRANS: ! 
32420     close #trans: ioerr ignore
32440     ! EO_DELETE: !
32460   return ! /r
34000   ADD_NEW_PAYEE: ! r:
34020     vn$=nam$=ad1$=ad2$=csz$=ss$=ph$=contact$=email$=fax$=myact$="" 
34040     typ=0
34060   goto EDIT_PAYEE  ! /r
36000   EDIT_PAYEE: ! r:
36020     holdvn$=vn$
36040     fntos(sn$="payee-2")
36060     respc=0
36080     mylen=28 : mypos=mylen+2
36100     fnfra(1,1,12,70,"Payee Information"," ")
36120     fnlbl(1,1,"Payee Number:",mylen,1,0,1)
36140     fntxt(1,mypos,8,0,1,"",0,"",1)
36160     resp$(respc+=1)=vn$
36180     fnlbl(2,1,"Payee Name:",mylen,1,0,1)
36200     fntxt(2,mypos,30,0,0,"",0,"",1)
36220     resp$(respc+=1)=nam$
36240     fnlbl(3,1,"Address:",mylen,1,0,1)
36260     fntxt(3,mypos,30,0,0,"",0,"",1)
36280     resp$(respc+=1)=ad1$
36300     fnlbl(4,1,"Address:",mylen,1,0,1)
36320     fntxt(4,mypos,30,0,0,"",0,"",1) 
36340     resp$(respc+=1)=ad2$
36360     fnlbl(5,1,"City, St. Zip:",mylen,1,0,1)
36380     fncombof("CityStZip",5,mypos,30,env$('Q')&"\Data\CityStZip.dat",1,30,0,0,env$('Q')&"\Data\CityStZip.idx",0,0, " ",1,0) 
36400     resp$(respc+=1)=csz$
36420     fnlbl(6,1,"Type:",mylen,1,0,1)
36440     fncombof("Payeetype",6,mypos,27,env$('Q')&"\GLmstr\PayeeType.dat",1,2,3,25,"",0,0, "The payee type is a code used to detemine which box should be used on a 1099 misc form.  Only enter a type code if the payee should get a 1099",1) 
36460     resp$(respc+=1)=str$(typ)
36480     fnlbl(7,1,"Federal ID or SS No.",mylen,1,0,1)
36500     fntxt(7,mypos,11,0,0,"",0,"",1) 
36520     resp$(respc+=1)=ss$
36540     fnlbl(8,1,"Phone Number:",mylen,1,0,1)
36560     fntxt(8,mypos,12,0,0,"",0,"",1) 
36580     resp$(respc+=1)=ph$
36600     fnlbl(9,1,"Contact Name:",mylen,1,0,1)
36620     fntxt(9,mypos,30,0,0,"",0,"",1)
36640     resp$(respc+=1)=contact$
36660     fnlbl(10,1,"E-mail Address:",mylen,1,0,1)
36680     fntxt(10,mypos,30,50,0,"",0,"",1)
36700     resp$(respc+=1)=email$
36720     fnlbl(11,1,"Fax Number:",mylen,1,0,1)
36740     fntxt(11,mypos,12,0,0,"",0,"",1)
36760     resp$(respc+=1)=fax$
36780     fnlbl(12,1,"My Account Number:",mylen,1,0,1)
36800     fntxt(12,mypos,20,0,0,"",0,"",1)
36820     resp$(respc+=1)=myact$
36840     fnlbl(15,20,"Standard General Ledger Breakdowns",40,2,0,0)
36860     ! r: General Ledger Breakdown Grid
36880     mat chdr$(5) : mat cmask$(5) : mat glitem$(5) 
36900     chdr$(1)='Refenence'
36920     chdr$(2)='Payee Number' 
36940     chdr$(3)='GL Number' 
36960     chdr$(4)='Percent' 
36980     chdr$(5)='Description'
37000     cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)='' 
37020     cmask$(4)='32' 
37040     fnflexinit1('PayeeGl',16,1,5,70,mat chdr$,mat cmask$,1,0,0)
37060     if trim$(vn$)="" then goto EO_FLEX3
37080     restore #payeegl,key>=vn$: nokey EO_FLEX3
37100     do
37120       read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_FLEX3
37140       if vn$<>payeekey$ then goto EO_FLEX3
37160       let glitem$(1)=str$(rec(payeegl)) : let glitem$(2)=payeekey$ 
37180       let glitem$(3)=payeegl$ : let glitem$(4)=str$(percent) 
37200       let glitem$(5)=gldesc$ 
37220       fnflexadd1(mat glitem$)
37240     loop
37260     EO_FLEX3: ! /r
37300     fnlbl(21,1,"",1,0,0,0) ! add space before buttons
37320     fnbutton(lc=21,61,"Add",2,"Add a standard general ledger breakdowns",0,4) 
37340     fnbutton(lc,67,"Edit",7,"Edit or Delete a standard general ledger breakdowns")
37360     fncmdkey("Save",1,1,0,"Saves and returns to payee selection") 
37380     fncmdkey("&Transactions",4,0,0,"List all checks for this payee") 
37400     fncmdkey("&Cancel",5,0,1,"Return to payee selection")
37420     fnacs(sn$,0,mat resp$,ck)
38000     if ck=5 then goto MENU1
38020     vn$=lpad$(trim$(resp$(1)(1:8)),8) 
38040     nam$=resp$(2) ! name 
38060     ad1$=resp$(3) ! address 
38080     ad2$=resp$(4) ! address 
38100     csz$=resp$(5) ! city state zip
38120     read #citystzip,using 'Form POS 1,C 30',key=rpad$(ltrm$(csz$),30),release: citystzip$ nokey L1150 : goto L1160
38140     L1150: write #citystzip,using 'Form POS 1,C 30': csz$
38160     L1160: typ=val(resp$(6)(1:2)) ! type 
38180     ss$=resp$(7) ! ss or fed id 
38200     ph$=resp$(8) ! phone 
38220     contact$=resp$(9) ! contact name 
38240     email$=resp$(10) ! email address
38260     let fax$=resp$(11) ! fax number 
38280     myact$=resp$(12) ! my account number with this payee 
38300     let gldistrec=val(resp$(13)) ! record number of gl distribution entry
38320     if ck=4 then 
38340       gosub PAYEE_TRANSACTIONS 
38360       goto EDIT_PAYEE
38380     else if ck=2 then  ! add gl breakdown
38400       percent=gldistrec=0: payeekey$=gldesc$=payeegl$="" 
38420       gosub GL_BREAKDOWNS
38430       goto EDIT_PAYEE
38440     else if ck=7 then  ! edit gl breakdown
38460       read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: payeekey$,payeegl$,percent,gldesc$ 
38480       gosub GL_BREAKDOWNS
38490       goto EDIT_PAYEE
38500     end if
38520     tac=fn_payeeTotalAllocationPercent
38540     if tac<>100 and tac<>0 then  ! r: percent breakdown doesn't add to 100 %
38560       mat ml$(3) 
38580       ml$(1)="Your percentage breakdowns total "&str$(tac)&"." 
38600       ml$(2)="The percentage breakdown must add to 100%." 
38620       ml$(3)="Correct the percentages." 
38640       fnmsgbox(mat ml$,resp$,cap$,16) 
38660       goto EDIT_PAYEE ! /r
38680     else
38700       goto SAVE_PAYEE
38720     end if ! /r
38740   SAVE_PAYEE: ! r:
38760     if edit=1 and vn$<>holdvn$ then gosub KEY_CHANGE
38780     if edit=1 then 
38800       rewrite #paymstr, using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
38820     else if add=1 then 
38840       write #paymstr,using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ duprec MSGBOX3
38860     end if
38880   goto MENU1 ! /r
42000   KEY_CHANGE: ! r: a gosub routine
42020     ! change the references to this file in the payee transaction file
42040     close #trans: ioerr ignore
42060     open #trans:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLTR1099.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\gltrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
42080     restore #trans,key>=holdvn$: nokey EO_CHANGE_KEY_ON_TRANS
42100     do    
42120       read #trans,using 'Form Pos 28,C 8': x$ eof EO_CHANGE_KEY_ON_TRANS
42140       if x$=holdvn$ then 
42160         rewrite #trans,using 'Form Pos 28,Cr 8',release: vn$ 
42170       end if
42180     loop while x$=holdvn$
42200     EO_CHANGE_KEY_ON_TRANS: ! 
42220     close #trans: ioerr ignore
42240     ! 
42260     ! Change references to this file in the sub-file PayeeGLBreakdown
42280     restore #payeegl,key=holdvn$: nokey EO_CHANGE_KEY_ON_PAYEEGL
42300     do  
42320       read #payeegl,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYEEGL
42340       if x$=holdvn$ then 
42360         rewrite #payeegl,using 'Form Pos 1,Cr 8': vn$ 
42380       end if
42400     loop while x$=holdvn$
42420     EO_CHANGE_KEY_ON_PAYEEGL: ! 
42440   return ! /r
44000   MSGBOX3: ! r: dupkey
44020     mat ml$(2) 
44040     ml$(1)="A record for payee number "&vn$&" already exists" 
44060     ml$(2)="You must select a different payee number." 
44080     fnmsgbox(mat ml$,resp$,cap$,16) 
44100   goto EDIT_PAYEE ! /r
54000   XitFn: ! r:
54020   close #trmstr2: ioerr ignore
54040   close #paymstr: ioerr ignore
54060   close #paymstr2: ioerr ignore
54080   close #payeegl: ioerr ignore
54100   close #citystzip: ioerr ignore
54110   ! /r
54120 fnend 
66000 PAYEE_TRANSACTIONS: ! r:
66020   close #trans: ioerr ignore
66040   open #trans:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLTR1099.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\gltrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
66060   fntos(sn$='payee_hist') 
66080   lc=0 : mylen=25 : mypos=mylen+2 : let width=50
66100   lc+=1
66120   fnlbl(lc+=1,30,'Payee Transactions',width,center)
66140   fnlbl(lc+=1,1,"Transaction Starting Date:",mylen,right)
66160   fntxt(lc,mypos,8,0,left,'CCYYMMDD',0,'Blank for All') 
66180   resp$(1)=str$(transactionstartingdate)
66200   fnlbl(lc+=1,1,"Transaction Ending Date:",mylen,right)
66220   fntxt(lc,mypos,8,0,left,'CCYYMMDD',0,'Blank for All') 
66240   resp$(2)=str$(transactionendingdate)
66260   let wbc=0
66280   fnlbl(lc=6,40,'Transaction Grid')
66300   mat chdr$(6) : mat cmask$(6) : mat item6$(6) 
66320   chdr$(1)='Rec' 
66340   chdr$(2)='Payee' 
66360   chdr$(3)='Date' 
66380   chdr$(4)='Amount' 
66400   chdr$(5)='Ref #' 
66420   chdr$(6)='Name/Description' 
66440   fnflexinit1('glPayee-'&str$(wbc)&'-'&str$(wtt),7,1,10,85,mat chdr$,mat cmask$,1,0,frame)
66460   key$=vn$ 
66480   transOnScreenCount=0
66500   restore #trans,key>=key$: nokey EO_FLEX2 
66520   transactionstotal=0
66540   do
66560     READ_TRANS: ! 
66580     read #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3',release: trvn$,dt,am,rn$,de$,nta eof EO_FLEX2
66600     if trim$(vn$)<>trim$(trvn$) then goto EO_FLEX2
66620     if transactionstartingdate<>0 and transactionstartingdate>fndate_mmddyy_to_ccyymmdd(dt) then goto READ_TRANS
66640     if transactionendingdate<>0 and transactionendingdate<fndate_mmddyy_to_ccyymmdd(dt) then goto READ_TRANS
66660     item6$(1)=str$(rec(trans)) : item6$(2)=trvn$ 
66680     item6$(3)=str$(dt): item6$(4)=str$(am) 
66700     item6$(5)=rn$ : item6$(6)=de$ 
66720     fnflexadd1(mat item6$) 
66740     transOnScreenCount+=1
66760     transactionstotal+=am
66780   loop
66800   EO_FLEX2: ! 
66820   fnlbl(5,1,'Transactions Total:',mylen,right)
66840   fntxt(5,mypos,12,0,right,"10",1,'This is the total of only the transactions shown in the Transaction Grid above. ') 
66860   resp$(3)=str$(transactionstotal)
66880   fncmdkey('&Refresh',2,0,0,"If you select a date range, you must refresh the screen to see the transactions for that date range.")
66900   fncmdkey('&Add',3,1,0,"Allows you to add a transaction that you would like to have included in the totals on a 1099 form.")
66920   if transOnScreenCount>0 then
66940     fncmdkey('&Edit',4,0,0,"Allows you to change or delete a transaction.")
66960   end if
66980   fncmdkey('&Close',5,0,1)
67000   fnacs(sn$,0,mat resp$,ck)
68000   edittrans=0
68020   if ck<>5 then 
68040     if ck=3 then 
68060       gosub ADD_TRANSACTIONS
68070       goto PAYEE_TRANSACTIONS
68080     else if ck=4 then 
68100       edittrans=1 
68120       editrec=val(resp$(3)) 
68140       gosub EDIT_TRANSACTIONS
68150       goto PAYEE_TRANSACTIONS
68160     else if ck=2 then 
68180       transactionstartingdate=val(resp$(1))
68200       transactionendingdate=val(resp$(2))
68220       goto PAYEE_TRANSACTIONS ! goto the top of this function
68240     end if
68260   end if
68280   close #trans: ioerr ignore
68300 return ! /r
72000 GL_BREAKDOWNS: ! r: sub routine
72020   fntos(sn$='payee_gl_dist') 
72040   respc=0 : mylen=28 : mypos=mylen+2
72060   fnlbl(1,25,"Breakdown for "&nam$(1:20),40)
72080   fnlbl(3,1,"General Ledger Number:",mylen,right)
72100   fnqgl(3,mypos) 
72120   resp$(respc+=1)=fnrgl$(payeegl$) ! think maybe here kj
72140   fnlbl(4,1,'Percent:',mylen,right)
72160   fntxt(4,mypos,6,0,0,'32',0,"Percent of total check to be charged to this g/l account.  Enter 25% as 25.00!") 
72180   resp$(respc+=1)=str$(percent)
72200   fnlbl(5,1,"Description:",mylen,right)
72220   fntxt(5,mypos,30) 
72240   resp$(respc+=1)=gldesc$
72260   fncmdset(7)
72280   fnacs(sn$,0,mat resp$,ck)
72300   if ck<>5 then 
72320     payeekey$=vn$
72340     payeegl$=fnagl$(resp$(1))
72360     percent=val(resp$(2)) ! percent
72380     let gldesc$=resp$(3)
72400     if ck=4 and gldistrec>0 then 
72420       delete #payeegl,rec=gldistrec: 
72430       goto GlBreakdownsXit
72440     else if ck=1 and gldistrec=0 then 
72460       write #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$
72480     else if ck=1 and gldistrec>0 then 
72500       rewrite #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: payeekey$,payeegl$,percent,gldesc$
72520     end if
72540   end if
72560   GlBreakdownsXit: ! 
72580 return ! /r
75000 def fn_payeeTotalAllocationPercent
75020   tac=0
75040   ! r: READ_STANDARD_BREAKDOWNS: !
75060   restore #payeegl,key>=vn$: nokey EO_TEST
75080   do    
75100     read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_TEST
75120     if vn$<>payeekey$ then goto EO_TEST
75140     tac+=percent
75160   loop while vn$=payeekey$
75180   EO_TEST: ! /r
75200   fn_payeeTotalAllocationPercent=tac
75220 fnend
76000 ! <Updateable Region: ERTN>
76010 ERTN: fnerror(program$,err,line,act$,"xit")
76020   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
76030   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76040   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76050 ERTN_EXEC_ACT: execute act$ : goto ERTN
76060 ! /region
82000 EDIT_TRANSACTIONS: ! r:
82020   read #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3',rec=editrec: vn$,dt,am,rn$,de$
82040   gosub ADD_TRANSACTIONS 
82060 return ! /r
84000 ADD_TRANSACTIONS: !  r: sub routnie - allows you to manually add a transaction
84020   if edittrans=0 then dt=am=0: rn$=de$=""
84040   fntos(sn$='add_trans')
84060   respc=0 : mylen=28 : mypos=mylen+2
84080   fnlbl(1,1,"Date:",mylen,right)
84100   fntxt(1,mypos,6,0,left,'1',0,'')
84120   resp$(1)=str$(dt)
84140   fnlbl(2,1,"Amount:",mylen,right)
84160   fntxt(2,mypos,12,0,0,'10',0,'')
84180   resp$(2)=str$(am)
84200   fnlbl(3,1,"Ref #:",mylen,right)
84220   fntxt(3,mypos,12,0,0,'',0,'')
84240   resp$(3)=rn$
84260   fnlbl(4,1,"Description:",mylen,right)
84280   fntxt(4,mypos,30,0,0,'',0,'')
84300   resp$(4)=de$
84320   fncmdkey("Save",1,1,0,"Saves any changes and returns to Payee selection")
84340   fncmdkey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing transaction.")
84360   fncmdkey("&Cancel",5,0,1,"Return to Payee selection screen.")
84380   fnacs(sn$,0,mat resp$,ck)
86000   if ck=5 then 
86020     goto XitTransactionAdd
86040   else if ck=3 then 
86060     mat ml$(2) 
86080     ml$(1)="You have chosen to delete this transaction." 
86100     ml$(2)="Click OK to delete or Cancel to retain the transaction." 
86120     fnmsgbox(mat ml$,resp$,cap$,49)
86140     if resp$="OK" then 
86160       delete #trans,rec=editrec: 
86180       goto XitTransactionAdd
86200     end if
86220   end if
86240   dt=val(resp$(1))
86260   am=val(resp$(2))
86280   rn$=resp$(3)
86300   de$=resp$(4)
86320   if edittrans=1 then 
86340     rewrite #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3',rec=editrec: vn$,dt,am,rn$,de$,0 
86360     edittrans=0
86380   else
86400     write #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3': vn$,dt,am,rn$,de$,0
86420   end if
86440   XitTransactionAdd: !
86460 return ! /r
