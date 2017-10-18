00010 ! formerly S:\acsUB\ubPdNot
00020 ! Past Due Notices
00030 ! _______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fntos,fnopt,fnerror,fnopenprn,fncloseprn,fnxit,fncomboa,fnfra,fncmbrt2,fncmbact,fnchk,fncmdkey,fndat,fnLastBillingDate,fncmdset,fntop,fngethandle,fngetdir2,fnreg_read,fnreg_write,fnpa_finis,fnprint_file_name$,fncreg_read,fncreg_write,fnpa_open,fnpa_newpage,fncustomer_address,fnpa_txt,fnfree,fnEditFile,fncopy
00050 ! _______________________________________________________________________
00060   on error goto ERTN
00080 ! r: dims
00090   dim z$*10,meter_address$*30,gb(10),d$(4)*20,b4$*30
00100   dim f$(3)*12,a(7),b(11),c(4),d(15),g(12)
00110   dim resp$(15)*512
00120   dim ln$*8800,flname$*256,l2$*8800,r1$(120)*30,ln3$*1
00130   dim extra$(11)*30,at$(3)*40,extra(23)
00132   dim tmp_rtf_filename$*1024
00140 ! /r
00142 ! r: top of programs, constants,initial setup, etc
00150   fntop(program$)
00170   tmp_rtf_filename$=fnprint_file_name$
00202   if env$('client')='French Settlement' or env$('client')='Granby' or env$('client')='Eldorado' then hard_coded=1 ! env$('client')='Merriam Woods' or
00210   fnLastBillingDate(d1)
00220   fndat(d$(4))
00230   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,input 
00240   read #21,using "Form POS 1,3*C 40": at$(1),at$(2),at$(3)
00250   close #21: 
00260   z=21
00270   at$(1)=trim$(at$(1))(1:z)
00280   x=len(at$(1)) : y=z-x
00290   at$(1)=rpt$(" ",int(y/2))&at$(1)
00300   z=26
00310   for j=2 to udim(at$)
00320     at$(j)=trim$(at$(j))(1:z)
00330     x=len(at$(j)) : y=z-x
00340     at$(j)=rpt$(" ",int(y/2))&at$(j)
00350   next j
00352 ! 
00354   deltype=0 : fnreg_read('UB - Past Due Notices - Delinquent Type',deltype$) : deltype=val(deltype$) conv ignore
00356 ! 
00360   open #adrbil=3: "Name="&env$('Q')&"\UBmstr\UBADRBIL.H"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00370   open #customer5=11: "Name="&env$('Q')&"\UBmstr\Customer.H"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBINDx5.H"&env$('cno')&",Shr",internal,input,keyed 
00380   open #customer1=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.H"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBIndex.H"&env$('cno')&",Shr",internal,input,keyed 
00382   F_CUSTOMER: form pos 1,c 10,c 30,x 90,c 12,pos 361,2*c 12,pos 143,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,10*pd 5.2,pos 1741,n 2,pos 1821,n 1,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00390 goto MENU1 ! /r
00400 MENU1: ! r:
00410   fntos(sn$="UBPdNot1")
00420   respc=0
00430   fnlbl(2,1,"@D1=Last Billing Date (mmddyy):",38,1)
00440   fntxt(2,40,8,0,1,"1")
00450   resp$(respc+=1)=str$(d1)
00460   fnlbl(3,1,"@D2= Payment Due Date (mmddyy):",38,1)
00470   fntxt(3,40,8,0,1,"1")
00480   resp$(respc+=1)=str$(d2)
00490   fnlbl(4,1,"@D3=    Shut Off Date (mmddyy):",38,1)
00500   fntxt(4,40,8,0,1,"1")
00510   resp$(respc+=1)=str$(d3)
00520   fnlbl(5,1,"@D4=            Date of Notice:",38,1)
00530   fntxt(5,40,20,20)
00540   resp$(respc+=1)=d$(4)
00550   fnlbl(6,1,"Minimum balance required:",38,1)
00560   fntxt(6,40,10,10,0,"10")
00570   resp$(respc+=1)=""
00580   fnfra(8,5,6,50,"Print all customers who:")
00590   fnopt(1,1,"Have not paid their current bill",0,1)
00600   if deltype<=1 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00610   fnopt(2,1,"Have not paid their prior month's bill",0,1)
00620   if deltype=2 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00630   fnopt(3,1,"Are Active",0,1)
00640   if deltype=3 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00650   fnopt(4,1,"Are Final Billed Customers and have not paid",0,1)
00660   if deltype=4 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00670   fnopt(5,1,"Have a balance",0,1)
00680   if deltype=5 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00690   fnopt(6,1,"Who Were Billed This Month",0,1)
00700   if deltype=6 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00710   fncmdset(2)
00720   fnacs(sn$,0,mat resp$,ckey)
00730   if ckey=5 then goto XIT
00740   d$(1)=cnvrt$("PIC(##/##/##)",d1=val(resp$(1)))
00750   d$(2)=cnvrt$("PIC(##/##/##)",d2=val(resp$(2)))
00760   d$(3)=cnvrt$("PIC(##/##/##)",d3=val(resp$(3)))
00770   d$(4)=rtrm$(d$(4)=resp$(4))
00780   minbal=val(resp$(5))
00790   for a=6 to 11
00800     if uprc$(resp$(a))=uprc$("True") then deltype=a-5
00810     resp$(a)=""
00820   next a
00822   fnreg_write('UB - Past Due Notices - Delinquent Type',str$(deltype))
01090   goto UBFORM ! /r
01100 ! ______________________________________________________________________
01110 PRINT_NEXT: ! r: the main read it and pr it routine
01120   if sel_indv$="Y" then goto ASK_NEXT_ACT
01130   if bk1>0 then 
01140     read #customer5,using F_CUSTOMER: z$,meter_address$,mat f$,mat a,mat b,mat c,mat d,bal,f,mat g,bra,mat gb,route,final,mat extra,mat extra$ eof EO_CUSTOMER
01160     if route>bk1 then goto EO_CUSTOMER
01170   else 
01180     read #customer1,using F_CUSTOMER: z$,meter_address$,mat f$,mat a,mat b,mat c,mat d,bal,f,mat g,bra,mat gb,route,final,mat extra,mat extra$ eof EO_CUSTOMER
01190   end if 
01200   if deltype<3 and bal<=1 then goto PRINT_NEXT
01210   if bal<minbal and minbal>0 then goto PRINT_NEXT ! skip if under minimum balance
01220   b4$=""
01230   pr f "1,1,Cc 80,R,N": str$(rec(customer5))&"/"&str$(lrec(customer5))
01240   if deltype=3 and final=0 then goto READ_ADRBIL ! pr ALL ACTIVE CUSTOMERS
01250   if deltype=4 and final>0 and bal>0 then goto READ_ADRBIL ! pr ALL INACTIVE CUSTOMERS WITH BAL
01260 ! IF UPRC$(NEWBIL$)="Y" AND F=D1 AND BAL=<G(11) THEN GOTO 440
01270   if deltype=1 and f=d1 and bal>0 then goto READ_ADRBIL ! pr ALL CUSTOMERS WHO HAVE NOT PAID THEIR MOST CURRENT BILL
01280   if deltype=2 and f=d1 and bal>g(11) then goto READ_ADRBIL ! pr all customers who owe more than last times bill
01290 ! IF DELTYPE=2 AND F<>D1 AND BAL>0 THEN GOTO READ_ADRBIL ! pr all customers who owe a prior bill but didn't get billed this time
01300   if deltype=5 and bal>0 then goto READ_ADRBIL
01310   if deltype=6 and f=d1 then goto READ_ADRBIL ! pr all customers who were billed last billing cycle
01320   goto PRINT_NEXT
01330 ! ______________________________________________________________________
01340 READ_ADRBIL: ! r:
01342   dim addr$(4)*30
01344   fncustomer_address(z$,mat addr$)
01346 ! dim altadr$(4)*30
01350 ! read #adrbil,using "Form POS 11,4*C 30",key=z$: mat altadr$ nokey L940
01360 ! if trim$(altadr$(1)&altadr$(2)&altadr$(3)&altadr$(4))<>"" then 
01390 !   e$(2)=altadr$(1)
01400 !   e$(3)=altadr$(2)
01410 !   b4$=altadr$(3)
01420 !   e$(4)=altadr$(4)
01430 ! end if 
01455 ! L940: ! /r
01460   if reminder=1 then 
01465     fn_vbprint
01470   else if env$('client')="Granby" then 
01472     fn_print_granby
01474   else if env$('client')="Eldorado" then 
01476     fn_eldorado
01478   else if env$('client')="French Settlement" then 
01480     fn_french_settlement_gas
01482 ! else if env$('client')="Merriam Woods" then
01484 !   fn_merriam_woods
01490   else if do_print_std_form=1 then 
01495     fn_print_standard_form
01500   else 
01505     fn_prnt1
01510   end if 
01515   fn_report_add
01520   goto PRINT_NEXT
01525 ! /r
16000   def fn_open_template
16020     if ~h_template then 
16040       open #h_template:=fngethandle: "Name="&env$('Q')&"\UBmstr\"&flname$&",RecL=1",external,input,relative 
16060     end if 
16080     fn_open_template=h_template
16100   fnend  ! fn_open_template
18000   def fn_prnt1
18020     if ~h_prnt1 then 
18040       open #h_prnt1:=fngethandle: "Name="&env$('at')&tmp_rtf_filename$&",eol=none,Replace",display,output 
18060     end if 
18080     fn_bldr1
18100     r=0
18120 P1_NEXT_LN: ! 
18140     ln$=""
18160 P1_L2250: read #h_template,using "Form POS 1,C 1",rec=r+=1: ln3$ eof P1_END1 norec P1_END1
18180     if ln3$=chr$(13) then goto P1_L2310
18200     ln$=ln$&ln3$
18220     if len(rtrm$(ln$))>3900 then pr #h_prnt1: ln$ : goto P1_NEXT_LN
18240     goto P1_L2250
18260 ! ___________________________
18280 P1_L2310: p3=len(ln$) : l2$="" : p1=0
18300 P1_L2320: p2=pos(ln$,"@",p1)
18320     if p2=0 then goto P1_L2570
18340     if p1>len(l2$) then l2$=rpad$(l2$,p1)
18360     l2$=l2$&ln$(p1:p2-1) : p4=pos(ln$," ",p2)
18380     if p4=0 then p4=p3 else p4=p4-1
18400 P1_L2370: if ln$(p4:p4)="." or ln$(p4:p4)="," or ln$(p4:p4)=":" or ln$(p4:p4)="\" or ln$(p4:p4)=";" then p4=p4-1 : goto P1_L2370
18420     if ln$(p2+2:p2+5)="\par" then p4=p2+1 ! if they don't space after the variable at the end of a line, it doesn't pr the variable
18440     if ln$(p2+3:p2+6)="\par" then p4=p2+2
18460     if ln$(p2+4:p2+7)="\par" then p4=p2+3
18480     if ln$(p2+5:p2+8)="\par" then p4=p2+3
18500     if ln$(p2+6:p2+9)="\par" then p4=p2+3
18520     v1=val(ln$(p2+1:p4)) conv P1_L2480
18540     if v1<>0 and v1<=udim(mat r1$) then l2$=l2$&r1$(v1)
18560 P1_L2450: p1=p4+1
18580     goto P1_L2320
18600 ! ___________________________
18620 P1_L2480: ! 
18640 ! if uprc$(ln$(p2+1:p2+2))><"B4" then
18660 !   if rtrm$(b4$)="" then b4$=extra$(1) : r1$(5)=e$(4)
18680 !   l2$=l2$&rtrm$(b4$)
18700 ! end if
18720     if uprc$(ln$(p2+1:p2+1))><"D" then goto P1_L2550
18740     v1=val(ln$(p2+2:p2+2)) conv P1_L2550
18760     if v1<1 or v1>4 then goto P1_L2550
18780     l2$=l2$&d$(v1)
18800 P1_L2550: goto P1_L2450
18820 ! ___________________________
18840 P1_L2570: l2$=l2$&ln$(p1:p3)
18860     pr #h_prnt1: l2$&chr$(13)
18880     goto P1_NEXT_LN
18900 ! ______________________________________________________________________
18920 P1_END1: ! 
18940     restore #h_template: 
18960     pr #h_prnt1: "\page"
18980   fnend  ! fn_prnt1
19000   def fn_bldr1 ! BUILD RECORD IN pr ARRAY
19020 ! if trim$(z$)='901246.40' then pr 'the beginning of it' : pause
19040 !   if trim$(b4$)="" then b4$=extra$(1)
19060 !   if trim$(e$(3))="" then e$(3)=b4$: b4$=""
19080 !   if trim$(b4$)="" then b4$=e$(4) ! : e$(4)="" ! blanking out of standard CSZ removed 7/25/11 to fix missing CSZ in @5
19100 !   if trim$(extra$(1))="" then extra$(1)=e$(4) : e$(4)='' ! re=added to make @108 the address line 2 or CSZ (if no addr line 2) and thusly @5 be blank.
19120     mat r1$=("")
19140     r1$(1)=ltrm$(z$)
19160     r1$(2)=rtrm$(meter_address$) ! meter address
19180     r1$(3)=rtrm$(addr$(1)) ! name
19200     r1$(4)=rtrm$(addr$(2)) ! rtrm$(e$(3)) ! address
19220     r1$(108)=rtrm$(addr$(3)) ! address
19240     r1$(5)=rtrm$(addr$(4)) ! city st zip
19260     for j=1 to 3 : r1$(j+5)=f$(j) : next j
19280     for j=1 to 7 : r1$(j+9)=str$(a(j)) : next j
19300     for j=1 to 11 : r1$(j+16)=ltrm$(cnvrt$("N 8.2",b(j))) : next j
19320     for j=1 to 4 : r1$(j+27)=ltrm$(cnvrt$("PIC(ZZ/ZZ/ZZ)",c(j))) : next j
19340     for j=1 to 15: r1$(j+31)=ltrm$(cnvrt$("N 10",d(j))) : next j
19360     r1$(47)=ltrm$(cnvrt$("N 10.2",bal))
19380     r1$(49)=ltrm$(cnvrt$("PIC(##/##/##)",f))
19400     for j=1 to 12 : r1$(j+49)=cnvrt$("N 10.2",g(j)) : next j
19420     for j=1 to 10 : r1$(j+61)=cnvrt$("N 10.2",gb(j)) : next j
19440     r1$(72)=cnvrt$("n 10.2",bal+(g(12)-g(11))) ! balance plus penalty (assume total penalties will be difference in gross and net bill)
19460     r1$(73)=ltrm$(cnvrt$("n 10.2",bal+(g(12)-g(11)))) ! balance plus penalty trimmed
19480     if env$('client')="White Hall" then r1$(73)=ltrm$(cnvrt$("n 10.2",bal+10)) ! balance plus $10 penalty trimmed
19500     r1$(74)=cnvrt$("n 10.2",bal-g(11)) ! past due balance
19520     r1$(75)=ltrm$(cnvrt$("n 10.2",bal-g(11))) ! past due balance trimed
19540     r1$(76)=cnvrt$("N 10.2",bal) ! balance not trimed
19560     r1$(77)=ltrm$(cnvrt$("PIC(##/##/##)",extra(3))) ! date read current
19580     if balance >0 then r1$(78)=ltrm$(cnvrt$("pic(zzzzzzz.##",max(0,bal+(g(12)-g(11))))) else r1$(78)=ltrm$(cnvrt$("pic(zzzzzzz.##",0)) ! pay after amount (balance plus penalty trimmed) nothing if balance <0
19600     r1$(80)=df$ ! bank draft Y
19620     r1$(81)=da$ ! bank Account
19640     r1$(82)=dc$ ! Account code
19660     r1$(83)=dc$ ! bank acct #
19680     r1$(84)=trim$(cnvrt$("N 2",extra(1))) ! route number
19700     r1$(85)=trim$(cnvrt$("N 7",extra(2))) ! sequence #
19720     r1$(86)=ltrm$(cnvrt$("PIC(ZZ/ZZ/ZZ)",extra(3))) ! current reading date
19740     r1$(87)=ltrm$(cnvrt$("PIC(ZZ/ZZ/ZZ)",extra(4))) ! prior   reading date
19760     r1$(88)=ltrm$(cnvrt$("PIC(zZzZZZZ)",extra(5))) ! sewer rediction
19780     r1$(89)=ltrm$(cnvrt$("n 10.2",extra(6))) conv L3040 ! security light charge
19800 L3040: r1$(90)=ltrm$(cnvrt$("PIC(ZZzZZzZZ)",extra(7))) ! security light count
19820     r1$(91)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)",extra(8))) ! electric multiplier
19840     r1$(92)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)",extra(9))) ! demand average usage
19860     r1$(93)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)",extra(10))) ! gas multiplier
19880     r1$(94)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)",extra(11))) ! service 6 rate code
19900     r1$(95)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)",extra(12))) ! service 7 rate code
19920     r1$(96)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)",extra(13))) ! service 8 rate code
19940     r1$(97)=ltrm$(cnvrt$("PIC(zZZ)",extra(14))) ! units per meter sewer
19960     r1$(98)=ltrm$(cnvrt$("PIC(zZZ)",extra(15))) ! units per meter electric
19980     r1$(99)=ltrm$(cnvrt$("PIC(zZZ)",extra(16))) ! units per meter gas
20000 ! r1$(100)  skipped
20020     r1$(101)=ltrm$(cnvrt$("PIC(ZZ/zz/zz)",extra(17))) ! final billing date
20040     r1$(102)=ltrm$(cnvrt$("PIC(ZZzzzzzzz)",extra(18))) ! average sewer usage
20060     r1$(103)=ltrm$(cnvrt$("PIC(ZZ/zz/zz)",extra(19))) ! estimated date
20080     extra(20)=0: r1$(104)=ltrm$(cnvrt$("PIC(ZZzzzzzz)",extra(20))) ! extra
20100     extra(21)=0: r1$(105)=ltrm$(cnvrt$("PIC(ZZzzzzzz)",extra(21))) ! extra
20120     extra(22)=0: r1$(106)=ltrm$(cnvrt$("PIC(ZZzzzzzz)",extra(22))) ! extra
20140     if extra(23)<-10000 then extra(23)=0
20160 ! r1$(107)=LTRM$(CNVRT$("PIC(n 10.2)",EXTRA(23))) ! escrow balance
20180 !   if trim$(extra$(1))<>"" then r1$(108)=extra$(1)
20200     for j=109 to 117
20220       r1$(j)=trim$(extra1$(j-107)) ! escrow balance thru end
20240     next j
20260 ! if trim$(z$)='901246.40' then pr 'the end of it' : pause
20280   fnend  ! fn_bldr1
30000   def fn_vbopenprint
30020     h_vb_pr_out:=20
30040     fnpa_open
30080     lyne=3
30120     spacer=0
30160   fnend  ! fn_vbopenprint
30180   def fn_vbprint
30200     pr #h_vb_pr_out: "Call Print.MyFontBold(True)"
30220     pr #h_vb_pr_out: 'Call Print.MyFontSize(16)'
30240     pr #h_vb_pr_out: 'Call Print.MyFont("Courier New")'
30260     pr #h_vb_pr_out: 'Call Print.AddText("'&at$(1)&'",'&str$(10)&','&str$(lyne*4+spacer)&')'
30280     pr #h_vb_pr_out: 'Call Print.MyFont("Lucida Console")'
30300     pr #h_vb_pr_out: 'Call Print.MyFontSize(12)'
30320     pr #h_vb_pr_out: 'Call Print.MyFontBold(False)'
30340     pr #h_vb_pr_out: 'Call Print.AddText("'&at$(2)&'",'&str$(10)&','&str$(lyne*6.5+spacer)&')'
30360     pr #h_vb_pr_out: 'Call Print.AddText("'&at$(3)&'",'&str$(10)&','&str$(lyne*8+spacer)&')'
30380     pr #h_vb_pr_out: "Call Print.MyFontBold(True)"
30400     pr #h_vb_pr_out: 'Call Print.MyFontSize(12)'
30420     pr #h_vb_pr_out: 'Call Print.AddLine('&str$(115)&','&str$(lyne*12+spacer)&',75,'&str$(30)&',True)'
30440     pr #h_vb_pr_out: 'Call Print.AddText("A Friendly Reminder....",'&str$(100)&','&str$(lyne+spacer)&')'
30460     pr #h_vb_pr_out: 'Call Print.MyFontSize(10)'
30480     pr #h_vb_pr_out: 'Call Print.MyFontBold(False)'
30500     pr #h_vb_pr_out: 'Call Print.AddText("If your check has already been mailed,please ",'&str$(100)&','&str$(lyne*3+spacer)&')'
30520     pr #h_vb_pr_out: 'Call Print.AddText("disregard this notice.  If not, your remittance by mail ",'&str$(100)&','&str$(lyne*4+spacer)&')'
30540     pr #h_vb_pr_out: 'Call Print.AddText("will be greatly appreciated.",'&str$(100)&','&str$(lyne*5+spacer)&')'
30560     pr #h_vb_pr_out: 'Call Print.AddText("Thank You!",'&str$(150)&','&str$(lyne*7+spacer)&')'
30580     pr #h_vb_pr_out: 'Call Print.AddText("Customer #:  '&z$&'",'&str$(125)&','&str$(lyne*14+spacer)&')'
30600     pr #h_vb_pr_out: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(zZZ/ZZ/ZZ)",d1)&'",'&str$(125)&','&str$(lyne*16+spacer)&')'
30620     pr #h_vb_pr_out: 'Call Print.AddText("Balance Due: '&cnvrt$("pic(---,---.##)",bal)&'",'&str$(125)&","&str$(lyne*18+spacer)&')'
30640     pr #h_vb_pr_out: 'Call Print.MyFontSize(13)'
30660     fnpa_txt(addr$(2),20,lyne*16+spacer)
30680     fnpa_txt(addr$(3),20,lyne*17.5+spacer)
30700     fnpa_txt(addr$(3),20,lyne*19+spacer)
30720     fnpa_txt(addr$(4),20,lyne*20.5+spacer)
30740     checkcounter+=1
30760     spacer+=90
30780     if checkcounter=3 then 
30800       fnpa_newpage
30820       checkcounter=0
30840       spacer=0
30860     end if  ! checkcounter=3
30880     fn_report_add
30900   fnend 
30920   def fn_report_close
30940     if h_ra then 
30960       dim ra_line$*256
30980       close #h_ra: 
31000       open #h_ra: 'Name='&env$('temp')&'\ubpdnot_summary_s'&session$&'.txt,RecL=256',display,input 
31020       fnopenprn(0,0,0,0, 'Summary')
31040       gosub RC_HDR
31060       do 
31080         linput #h_ra: ra_line$ eof RC_DONE
31100         pr #255: rtrm$(ra_line$) pageoflow RC_PGOF ! ,using 'form pos 1,c 256'
31120       loop 
31140 RC_DONE: ! 
31160       fncloseprn
31180       close #h_ra,free: 
31200       h_ra=0
31220     end if  ! h_ra
31240     goto RC_XIT
31260 RC_PGOF: ! 
31280     pr #255: newpage
31300     gosub RC_HDR
31320     continue  ! RC_PGOF
31340 RC_HDR: ! 
31360     rc_page+=1
31380     pr #255: "\qc "&cnam$
31400     pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
31420     pr #255,using "form pos 1,c 70,cr 14": "\ql "&date$,"Page "&str$(rc_page)
31440     pr #255: "{\ul Account No}  {\ul Customer Name            }  {\ul       Balance}  {\ul  Meter Address  }"
31460     return  ! RC_HDR
31480 RC_XIT: ! 
31500   fnend  ! fn_report_close
31520   def fn_report_add
31540     if ~h_ra then 
31560       open #h_ra:=fngethandle: 'Name='&env$('temp')&'\ubpdnot_summary_s'&session$&'.txt,RecL=256,replace',display,output 
31580       rc_page=0
31600     end if  ! ~h_ra
31620     pr #h_ra,using 'form pos 1,c 256': z$&'  '&addr$(1)&cnvrt$("pic(---,---.##)",bal)&'  '&meter_address$&'  '
31640 ! 
31660   fnend  ! fn_report_add
31666 IGNORE: continue 
31700 ! <Updateable Region: ERTN>
31720 ERTN: fnerror(program$,err,line,act$,"xit")
31740   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
31760   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
31780   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
31800 ERTN_EXEC_ACT: execute act$ : goto ERTN
31820 ! /region
31860   def fn_print_standard_form
31880     if a(1)=0 then water$="     " else water$="Water"
31900     if a(4)=0 then gas$="   " else gas$="Gas"
31920     fnopenprn(cp,33,220,process)
31940     for j=1 to 4 : pr #255: "" : next j
31960     for j=1 to 3 : pr #255,using "Form pos 7,C 40": at$(j) : next j
31980     for j=1 to 3 : pr #255: "" : next j
32000     pr #255,using "Form pos 8,C 80": trim$(at$(1))&" Final Disconnect Notice   "&cnvrt$("pic(zz/zz/zz",d1)&"   "&trim$(z$)
32020     pr #255: ""
32040     for j=1 to 4 : pr #255,using "Form pos 9,C 73": mis$(j) : next j
32060     pr #255: ""
32080     pr #255,using "Form pos 9,C 73": "Service  "&water$&"         "&gas$&"                Reconnection Fee: $"&str$(reconnect_fee)
32100     pr #255,using "Form POS 18,2*C 14,X 5,C 30": f$(1),f$(3),meter_address$
32120     pr #255: ""
32140     pr #255,using "Form pos 13,C 11,N 10.2": "Amount Due:",bal
32160     for j=1 to 4 : pr #255: "" : next j
32180     for j=1 to 4 : pr #255,using "Form pos 50,C 30": addr$(j) : next j
32200 ! 4 more lines from this point before next page
32220     pr #255: newpage
32240   fnend 
32260   def fn_print_granby
32280     fnopenprn
32300     pr #255,using L3620: e$(1)
32320 L3620: form pos 4,c 47,skip 4
32340     if gb(1)=0 then 
32360       pr #255,using L3640: "Your Utility Account is Past Due."
32380     else 
32400       pr #255,using L3650: "Your Utility Account is Past Due.","Water",gb(1)
32420     end if 
32440 L3640: form pos 4,c 47
32460 L3650: form pos 4,c 47,pos 53,c 10,pos 65,n 10.2
32480     if gb(4)=0 then 
32500       pr #255,using L3640: "Please pay the amount due by "&ltrm$(d$(2))
32520     else 
32540       pr #255,using L3650: "Please pay the amount due by "&ltrm$(d$(2)),"Gas",gb(4)
32560     end if 
32580     if gb(5)=0 then 
32600       pr #255,using L3640: "to avoid Utility Disconnection."
32620     else 
32640       pr #255,using L3650: "to avoid Utility Disconnection.","Sanitation",gb(5)
32660     end if 
32680     if gb(2)=0 then pr #255: else pr #255,using L3700: "Sewer",gb(2)
32700     if gb(7)=0 then pr #255: else pr #255,using L3700: "Primacy",gb(7)
32720     if gb(8)=0 then pr #255: else pr #255,using L3700: "Other",gb(8)
32740 L3700: form pos 53,c 10,pos 65,n 10.2
32760     if gb(9)=0 then pr #255: else pr #255,using L3700: "Sales Tax",gb(9)
32780     if gb(10)=0 then pr #255: "" else pr #255,using L3740: "Penalty",gb(10)
32800 L3740: form pos 53,c 10,pos 65,n 10.2
32820     pr #255: ''
32840     pr #255: ''
32860     pr #255: ''
32880     pr #255,using L3760: e$(2)
32900 L3760: form pos 10,c 30
32920     pr #255,using L3760: e$(3)
32940     pr #255,using L3790: e$(4),z$
32960 L3790: form pos 10,c 30,pos 52,c 10,skip 2
32980     pr #255,using L3810: bal
33000 L3810: form pos 63,n 10.2
33020     granby_print_count+=1
33040     if granby_print_count/3=int(granby_print_count/3) then 
33060       pr #255: newpage
33080     else 
33100       pr #255: '' : pr #255: '' : pr #255: '' : pr #255: '' : pr #255: ''
33120     end if 
33140   fnend  ! fn_print_granby
33160   def fn_eldorado
33180     fnopenprn
33200     pr #255,using 'form skip 8,pos 37,c 30': e$(2)
33220     pr #255,using 'form pos 37,c 30': e$(3)
33240     pr #255,using 'form pos 37,c 30': e$(4)
33260     pr #255: ''
33280     pr #255: ''
33300     pr #255: ''
33320     pr #255,using 'form pos 37,n 10.2,x 2,c 10': bal,z$
33340     pr #255: ''
33360     pr #255,using 'form pos 1,pic(zz/zz/zz),c 10,n 14.2': d2,z$,bal
33380     checkcounter+=1
33400     if checkcounter=3 then 
33420       pr #255: newpage
33440       checkcounter=0
33460     else 
33480       pr #255,using 'form pos 1,c 1,skip 8': ''
33500     end if 
33520   fnend  ! fn_eldorado
34000   def fn_french_settlement_gas
34020     fnopenprn
34040 ! ______________pre-print calculations__________________________________
34060     if pb<>0 then pb$="Prior Balance" else pb$=""
34080     if g(1)=0 then t1$="" else t1$="WTR"
34100     if g(2)=0 then t2$="" else t2$="SWR"
34120     if g(3)=0 then t3$="" else t3$="RPR"
34140     if g(4)=0 then t4$="" else t4$="GAS"
34160     if g(5)=0 then t5$="" else t5$="Purchased Gas Adj."
34180     if g(6)=0 then t6$="" else t6$="Inspection Fee"
34200     if g(7)=0 then t7$="" else t7$="Deposit Interest"
34220     if g(8)=0 then t8$="" else t8$="Other Charges"
34240     if g(8)<0 then t8$="Deposit Refund"
34260     if g(9)=0 then t9$="" else t9$="La. Sales Tax"
34280 ! If D(10)=1 Then eST$="Bill Estimated" Else eST$=""
34300     if c4>0 then final$="Final Bill" else final$=""
34320     if bal<=0 then g(10)=0
34340     gross=max(bal+g(10),0)
34360 ! ______________actual Bill Printing____________________________________
34380     pos_amt=28
34400     pos_r=39
34420 L310: form pos 1,nz 6,nz 7,nz 7,x 2,c 3,pos pos_amt,nz 8.2,pos pos_r,c 30
34440 L320: form pos 1,c 20,pos pos_amt,nz 8.2,pos pos_r,c 30
34460 L340: form pos 1,pic(## ## ##),x 1,pic(## ## ##),n 8.2,pos 27,pic(-----.--),pos 40,n 7.2,x 3,pic(## ## ##),pic(-----.--),skip 1
34480 ! __
34500 ! 
34520     pr #255,using L391: z$
34540 L391: form pos 25,c 10
34560     pr #255,using L411: z$
34580 L411: form pos 38,c 10,skip 4
34600     pr #255,using L310: d(9),d(10),d(11),t4$,g(4),e$(1)
34620     pr #255,using L320: t5$,g(5),e$(2)
34640     pr #255,using L320: t6$,g(6),e$(3)
34660     pr #255,using L320: t7$,g(7),e$(4)
34680 ! pr #255,Using 320: T8$,G(8)
34700     if budget>0 then bud$="Budgeted Amount:"&trim$(cnvrt$("Pic($$,$$$.##",budget)) else bud$=""
34720     pr #255,using L320: t9$,g(9),bud$(1:30)
34740     pr #255,using L320: pb$,bal-g(11)
34760     pr #255: ""
34780     pr #255,using L511: final$
34800 L511: form pos 22,c 10
34820     pr #255: ""
34840     pr #255: ""
34860     pr #255: ""
34880     pr #255: ""
34900     pr #255: ""
34920     count+=1: if count=2 then pr #255: ""
34940     if count=3 then pr #255: 
34960     pr #255,using L340: d(6),d(5),gross,bal,gross,d4,bal
34980     if count=1 then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: ! EXTRA LINE BETWEEN 1ST and 2nd bills
35000     if count=2 then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: ! EXTRA LINE BETWEEN 2nd & 3rd bill
35020     if count=3 then count=0 : pr #255: newpage
35040 ! pr #255,Using 360: MG$(1)
35060 ! If BUDGET>0 Then pr #255,Using 360: "Pay Budgeted Amount:"&CNVRT$("Pic($$,$$$.##",BUDGET) : Goto 610
35080 ! pr #255,Using 360: MG$(2)
35100 ! pr #255,Using 360: MG$(3)
35120 ! pr #255:
35140 ! pr #255:
35160 ! pr #255:
35180 ! _____________end_________________
35200   fnend 
36000 UBFORM: ! r: pr FROM TEXT FILE
36020   dim file_rtf$(1)*512
36060   fngetdir2(env$('Q')&"\UBmstr",mat file_rtf$, '/ON','*.rtf')
36080   fl1=udim(mat file_rtf$)
36100   for fl1=1 to udim(mat file_rtf$)
36120     file_rtf$(fl1)=file_rtf$(fl1)
36140   next fl1
36160   mat file_rtf$(fl1) : file_rtf$(fl1)="(Pre-Printed)"
36180   mat file_rtf$(fl1+=1) : file_rtf$(fl1)="(Reminder)"
36200   goto SELECT_SCREEN ! /r
38000 !  r: def fn_merriam_woods
38020 !     library 'S:\acsUB\PrintBill_Merriam_Woods': fnpast_due_notice,fnpast_due_notice_finis
38040 ! ! ______________pre-print calculations__________________________________
38060 !     if pb<>0 then pb$="Prior Balance" else pb$=""
38080 !     if g(1)=0 then t1$="" else t1$="WTR"
38100 !     if g(2)=0 then t2$="" else t2$="SWR"
38120 !     if g(3)=0 then t3$="" else t3$="RPR"
38140 !     if g(4)=0 then t4$="" else t4$="GAS"
38160 !     if g(5)=0 then t5$="" else t5$="Purchased Gas Adj."
38180 !     if g(6)=0 then t6$="" else t6$="Inspection Fee"
38200 !     if g(7)=0 then t7$="" else t7$="Deposit Interest"
38220 !     if g(8)=0 then t8$="" else t8$="Other Charges"
38240 !     if g(8)<0 then t8$="Deposit Refund"
38260 !     if g(9)=0 then t9$="" else t9$="La. Sales Tax"
38300 !     if c4>0 then final$="Final Bill" else final$=""
38320 !     if bal<=0 then g(10)=0
38340 !     gross=max(bal+g(10),0)
38360 ! ! ______________actual Bill Printing____________________________________
38560 !     fnpast_due_notice(z$)
39180 ! ! _____________end_________________
39200 ! /r  fnend
40000 SELECT_SCREEN: ! r:
40020   fn_report_close
40040   fntos(sn$="Select_and_Book")
40060   mat resp$=("")
40080   respc=0
40100   fnlbl(1,1,"File Name:",28,1)
40120   if hard_coded then 
40140     fntxt(1,30,10,0,0,'',1,'',0)
40160     resp$(respc+=1)='hard coded'
40180   else 
40200     fncomboa("PDNOTRTF",1,30,mat file_rtf$)
40220 ! resp$(respc+=1)=file_rtf$(1)
40240     if env$('client')='White Hall' then 
40260       resp$(respc+=1)='White_Ha'
40280     else if env$('client')='Ash Grove' then 
40300       resp$(respc+=1)='ashgrove.rtf'
40320     else 
40330       respc+=1
40332       fncreg_read('ubpdnot_file_name',resp$(respc))
40340       if resp$(respc)='' or srch(mat file_rtf$,resp$(respc))<=0 then resp$(respc)=file_rtf$(1)
40360     end if 
40380   end if 
40400   fnlbl(3,1,"Route Number:",28,1)
40420   fncmbrt2(3,30)
40440   resp$(respc+=1)="[All]"
40460   fnlbl(4,1,"Beginning Account:",28,1)
40480   fncmbact(4,30,1)
40500   resp$(respc+=1)="[All]"
40520   fnchk(6,30,"Print only Selected Accounts")
40540 ! fncmdset(102)
40560   fncmdkey("&Print",1,1)
40580   if ~hard_coded then let fncmdkey("E&dit",3)
40600   if ~hard_coded then let fncmdkey("&Add",4)
40620   if ~hard_coded then let fncmdkey("&Delete",7)
40640   if ~hard_coded then let fncmdkey("&Refresh",6)
40660   fncmdkey("&Cancel",5,0,1)
40680   fnacs(sn$,0,mat resp$,ckey)
40700   do_print_std_form=0
40720   dim flname$*256
40740   flname$=rtrm$(resp$(1))
40750   fncreg_write('ubpdnot_file_name',flname$)
40760   if resp$(2)<>"[All]" then 
40762     bk1=val(resp$(2))
40764     resp$(2)=""
40766   else 
40768     bk1=0
40770     resp$(2)=""
40772   end if 
40774   if trim$(resp$(3))<>"[All]" then 
40776     sz$=lpad$(trim$(resp$(3)(1:10)),10)
40778     resp$(3)=""
40780   else 
40782     sz$=""
40784     resp$(3)=""
40786   end if 
40788   if resp$(4)="True" then 
40790     resp$(4)=""
40792     sel_indv$="Y"
40794   else 
40796     resp$(4)=""
40798     sel_indv$="N"
40800   end if 
40820   if ckey=1 and resp$(1)="(Pre-Printed)" then 
40840     do_print_std_form=1
40860     goto PRINTING_BEGIN
40880   else if ckey=1 then ! pr the Past Due Notices
40900     count=0
40920     if resp$(1)="(Reminder)" then 
40940       fn_vbopenprint
40960       reminder=1
40980     else 
41000       resp$(1)=""
41020       if ~hard_coded then let fn_open_template
41040     end if 
41060     goto PRINTING_BEGIN
41080   else if ckey=5 then 
41100     goto MENU1
41120   else if ckey=6 then 
41140     goto UBFORM
41160   else if resp$(1)="(Pre-Printed)" or resp$(1)="(Reminder)" and ckey<>4 then 
41180     goto SELECT_SCREEN ! CANT EDIT STANDARD FORM
41200   else if ckey=4 then ! r: Add
41220     fntos(sn$="Select_Name")
41240     respc=0
41260     fnlbl(1,1,"File Name:",15,1)
41280     fntxt(1,17,40,64,1,"")
41300     resp$(respc+=1)=""
41320     fncmdset(2)
41340     fnacs(sn$,0,mat resp$,ckey)
41360     if ckey=5 then goto MENU1
41380     dim newname$*256
41400     newname$=trim$(resp$(1))&'.rtf' ! &".rtf" ! trim$(resp$(1)(1:8))&".rtf"
41420     fnCopy("S:\Core\default\plain.rtf",os_filename$(env$('Q')&"\UBmstr\"&newname$))
41440     fnEditFile('atlantis',env$('Q')&"\UBmstr\"&newname$)
41460     goto UBFORM ! /r
41480   else if ckey=3 then 
41500     fnEditFile('atlantis',env$('Q')&"\UBmstr\"&flname$)
41520     goto UBFORM
41540   else if ckey=7 then 
41560     fnFree(env$('Q')&"\UBmstr\"&trim$(flname$))
41580     goto UBFORM
41600   else 
41620     goto UBFORM
41640   end if 
41660 ! /r  end of SELECT_SCREEN
44000 PRINTING_BEGIN: ! r:
44020   checkcounter=0
44040   if trim$(sz$)="" and sel_indv$="Y" then goto ASK_NEXT_ACT ! selected to pick specific account but did not have one on screen
44060   if trim$(sz$)<>"" and sel_indv$="Y" then z$=sz$: goto READ_CUSTOMER ! selected to pirnt specific account and had an account on screen
44080 ! if rtrm$(sz$)="" then goto L1330
44100 !  L1330: !
44120   if bk1=0 and trim$(sz$)="" then goto NEXT_RECORD
44140   if bk1=0 then goto L1360
44160   if trim$(sz$)="" or bk1>0 then ! restore_for_route
44170     sz$=cnvrt$("N 2",bk1)&"       " conv PRINTING_BEGIN
44180     restore #customer5,key>=sz$: nokey PRINTING_BEGIN
44200   else ! restore_for_customer
44220     restore #customer1,key=sz$: nokey PRINTING_BEGIN
44240   end if 
44260 L1360: ! 
44270   goto NEXT_RECORD ! /r
44420 NEXT_RECORD: ! r:
44440   if sel_indv$="Y" then 
44460     z$=lpad$(trim$(resp$(2)(1:10)),10)
44480     goto READ_CUSTOMER
44500   else 
44520     goto PRINT_NEXT
44540   end if  ! /r
44560 ASK_NEXT_ACT: ! r:
44580   fntos(sn$="UBPdNot-5")
44600   respc=0
44620   fnlbl(1,1,"Next Account:",18,1)
44640   fncmbact(1,20,1)
44660   resp$(respc+=1)=""
44680   fncmdset(19)
44700   fnacs(sn$,0,mat resp$,ckey)
44720   if ckey=5 then 
44740     goto XIT
44760   else if ckey=2 then 
44780     goto EO_CUSTOMER
44800   else 
44820     sz$=lpad$(trim$(resp$(1)(1:10)),10)
44840     goto READ_CUSTOMER ! if ckey=1
44860   end if 
44880 ! ______________________________________________________________________
44900 READ_CUSTOMER: ! 
44920   read #customer1,using F_CUSTOMER,key=sz$: z$,meter_address$,mat f$,mat a,mat b,mat c,mat d,bal,f,mat g,bra,mat gb,route,final,mat extra,mat extra$ nokey ASK_NEXT_ACT
44940   b4$=""
44960   goto READ_ADRBIL ! /r
46000 EO_CUSTOMER: ! r:
46020 ! if env$('client')='Merriam Woods' then
46040 !   fnpast_due_notice_finis
46060 ! else 
46080   if ~reminder then 
46100     restore #customer1: ! Close #customer1: Ioerr 980
46120     granby_print_count=0
46140     if h_prnt1 then 
46160       close #h_prnt1: : h_prnt1=0
46180       fnEditFile('atlantis',tmp_rtf_filename$)
46200     else 
46220       fncloseprn
46240     end if 
46260     goto SELECT_SCREEN ! XIT
46280   end if 
46300   close #customer1: ioerr ignore
46320   customer1=0
46340   fnpa_finis(h_vb_pr_out)
46360 ! end if
46380 ! 
46400   if h_prnt1 then 
46420     close #h_prnt1: : h_prnt1=0
46440     fnEditFile('atlantis',tmp_rtf_filename$)
46460   else 
46480     fncloseprn
46500   end if  ! /r
48000 XIT: ! r:
48020   fn_report_close
48040   fnxit
48060 ! /r
