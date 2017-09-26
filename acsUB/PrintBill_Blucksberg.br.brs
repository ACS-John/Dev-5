00020 ! print bills for Blucksberg Mountain Water (full page)
12000 ! ______________________________________________________________________
12020   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fncombof,fnchk,fnerror,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fnpa_finis,fnpa_line,fnpa_txt,fnpa_open,fnpa_elipse,fngethandle,fnpa_newpage,fnpa_pic,fnpa_fontsize,fnpa_fontitalic,fnpa_fontbold,fntrans_total_as_of,fnget_services,fncreg_read,fncreg_write
12040   on error goto ERTN
12060 ! ______________________________________________________________________
12080   dim resp$(40)*128,mg$(13)*128,cap$*128
12100   dim z$*10,e$(4)*30,f$*12,g(12),d(15),b(11),extra1$*30
12120   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40
12140   dim usage(3),billdate(3),reads(3),tg(11)
12160 ! ______________________________________________________________________
14000   let fncno(cno,cnam$)
14020   let fnd1(d1)
14040   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input 
14060   read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
14080   close #21: 
14100   let at$(1)=cnam$
14120   let z=21
14140   let at$(1)=trim$(at$(1))(1:z)
14160   let x=len(at$(1)) : let y=z-x
14180   let at$(1)=rpt$(" ",int(y/2))&at$(1)
14200   let z=26
14220   for j=2 to udim(at$)
14240     let at$(j)=trim$(at$(j))(1:z)
14260     let x=len(at$(j)) : let y=z-x
14280     let at$(j)=rpt$(" ",int(y/2))&at$(j)
14300   next j
14320   dim servicename$(10)*20
14340   let fnget_services(mat servicename$)
14360   close #h_service: 
14380   for j=1 to udim(servicename$)
14400     let servicename$(j)=trim$(servicename$(j))
14420   next j
14440   gosub BULKSORT
14460   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
14480   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
14500   open #h_ubtransvb=fngethandle: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
14520 ! 
14540   open #11: "Name="&env$('Q')&"\UBmstr\Message.h"&str$(cno),internal,outin,relative ioerr MESSAGE_H_ALREADY_CONVERTED
14560   read #11,using "form pos 1,11*c 60",rec=1: mat mg$(1:11) norec MESSAGE_H_FINAL
14580   for mg_item=1 to 11
14600     let fncreg_write('print bill message board line '&str$(mg_item),mg$(mg_item))
14620   next mg_item
14640 MESSAGE_H_FINAL: ! 
14660   close #11,free: 
14680 MESSAGE_H_ALREADY_CONVERTED: ! 
14700   for mg_item=1 to udim(mg$)
14720     let fncreg_read('print bill message board line '&str$(mg_item),mg$(mg_item))
14740   next mg_item
16000 SCREEN1: ! r:
16020   let a$="" : let prtbkno=0
16040   let fntos(sn$="UBPrtBl1-1")
16060   let pf=26 : let ll=24
16080   let respc=0
16100   let fnlbl(3,1,"Due Date:",ll,1)
16120   let fntxt(3,pf,8,8,1,"1",0,tt$)
16140   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
16160   let fnlbl(5,1,"Service From Date:",ll,1)
16180   let fntxt(5,pf,8,8,1,"1",0,tt$)
16200   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d5)
16220   let fnlbl(6,1,"Service To Date:",ll,1)
16240   let fntxt(6,pf,8,8,1,"1",0,tt$)
16260   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d6)
16280   let scr_line=7
16300   let fnlbl(scr_line+1,1,"Message on Bill:",ll,1)
16320   let mg_len=95 : let mg_display_len=60
16340   for mg_item=1 to udim(mat mg$)
16360     let scr_line+=1
16380     let fntxt(scr_line,pf,mg_len,mg_display_len,2)
16400     let resp$(respc+=1)=mg$(mg_item)(1:mg_len)
16420   next mg_item
16440   let scr_line+=1
16460   let fnlbl(scr_line+=1,1,"Date of Billing:",ll,1)
16480   let fntxt(scr_line,pf,8,8,1,"1")
16500   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
16520   let fnlbl(scr_line+=1,1,"Prior Date of Billing:",ll,1)
16540   let fntxt(scr_line,pf,8,8,1,"1")
16560   let resp$(resp_billing_date_prior:=respc+=1)=cnvrt$("pic(zzzzzz)",billing_date_prior)
16580   let scr_line+=1
16600   let fnlbl(scr_line+=1,1,"Starting Account:",ll,1)
16620   let fncombof("ubm-act-nam",scr_line,pf,40,env$('Q')&"\UBmstr\Customer.h"&str$(cno),1741,9,41,30,env$('Q')&"\UBmstr\ubindx5.h"&str$(cno),2)
16640   let resp$(resp_start:=respc+=1)="[All]"
16660   let fnlbl(scr_line+=1,1,"Route Number:",ll,1)
16680   let fncmbrt2(scr_line,pf)
16700   let resp$(resp_route:=respc+=1)="[All]"
16720   let fnchk(scr_line+=1,pf,"Select Accounts to Print",1)
16740   let resp$(resp_select_accounts:=respc+=1)="False"
16760   let fncmdset(3)
16780   let fnacs(sn$,0,mat resp$,ck)
18000   if ck=5 then goto XIT
18020   let d1=val(resp$(17))
18040   let d4=val(resp$(1))
18060   let d5=val(resp$(2))
18080   let d6=val(resp$(3))
18100 ! 
18120   let mg_len_max=mg_len
18140 ! for mg_item=1 to udim(mat mg$)
18160 !   tmp_which_resp=mg_item+3
18180 !   mg_len_max=max(mg_len_max,len(trim$(resp$(tmp_which_resp))))
18200 ! nex mg_item
18220 ! !
18240   for mg_item=1 to udim(mat mg$)
18260     let tmp_which_resp=mg_item+3
18280     let mg$(mg_item)=rpt$(" ",(mg_len_max - len(trim$(resp$(tmp_which_resp)))) / 2)&trim$(resp$(tmp_which_resp))
18300   next mg_item
18320 ! 
18340   for mg_item=1 to udim(mg$)
18360     let fncreg_write('print bill message board line '&str$(mg_item),mg$(mg_item))
18380   next mg_item
18400 ! 
18420   let billing_date_prior=val(resp$(resp_billing_date_prior))
18440   let billing_date_prior=date(days(billing_date_prior,'mmddyy'),'ccyymmdd')
18460   if resp$(resp_start)="[All]" then let a$="" else let a$=lpad$(trim$(resp$(6)(1:9)),9)
18480   if resp$(resp_route)="[All]" then let prtbkno=0 else let prtbkno=val(resp$(7))
18500   if resp$(resp_select_accounts)="True" then let sl1=1: let z$="" else let sl1=0
18520   goto GET_STARTED
18540 ! /r
24000 GET_STARTED: ! r:
24020   if trim$(a$)<>"" then 
24040     read #2,using L570,key=a$: z$,route,sequence nokey SCREEN1
24060     let holdz$=z$
24080     let begin=1
24100   end if 
24120 L570: form pos 1,c 10,pos 1741,n 2,n 7
24140   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
24160   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
24180   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
24200 ! ______________________________________________________________________
24220   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
24240   let fnpa_open
24260   let lyne=3
24280 ! 
24300 ! on fkey 5 goto RELEASE_PRINT
24320 NEXT_CUSTOMER: ! 
24340   if sl1=1 then goto SCREEN_SELECT_ACCOUNT
24360 L680: ! 
24380   read #6,using 'form pos 22,c 10': z$ eof RELEASE_PRINT
24400   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L680 ! start with
24420   let begin=0 ! cancel starting account
24440   read #1,using L730,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,seweravg nokey L680
24460 L730: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1,pos 1712,c 1,pos 1822,n 9
24480   if prtbkno<>0 and prtbkno><route then goto RELEASE_PRINT
24500   if f><d1 then goto NEXT_CUSTOMER
24520 AFTER_READ_CUSTOMER: ! 
24540   gosub READALTADR
24560   let pb=bal-g(11)
24580   if bal<=0 then let g(9)=0 ! don't show penalty if balance 0 or less
24600   let activity_charge=fntrans_total_as_of(z$,billing_date_prior,1)
24620   let activity_penalty=fntrans_total_as_of(z$,billing_date_prior,2)
24640   let activity_payment=fntrans_total_as_of(z$,billing_date_prior,3)
24660   let activity_credit=fntrans_total_as_of(z$,billing_date_prior,4)
24680   let activity_debit=fntrans_total_as_of(z$,billing_date_prior,5)
24700   let prior_prior_balance=bal ! -g(11)
24720   let prior_prior_balance=prior_prior_balance-activity_charge
24740   let prior_prior_balance=prior_prior_balance-activity_penalty
24760   let prior_prior_balance=prior_prior_balance+activity_payment
24780   let prior_prior_balance=prior_prior_balance+activity_credit
24800   let prior_prior_balance=prior_prior_balance-activity_debit
24820 ! ______________print bill routine______________________________________
24840   gosub VBPRINT
24860 ! _____________end of print routine______________________________________
24880   let bct(2)=bct(2)+1 ! accumulate totals
24900   goto NEXT_CUSTOMER
24920 ! /r
28000 SCREEN_SELECT_ACCOUNT: ! r: pick individual accounts to print
28020   let fntos(sn$:="UBPrtBl1-2")
28040   let fnlbl(1,1,"Account (blank to stop)",31,1)
28060   if trim$(z$)<>"" then 
28080     let fnlbl(3,1,"Last Account entered was "&z$,44,1)
28100   end if 
28120   let fncmbact(1,17)
28140   let resp$(1)=z$
28160   let fncmdset(3)
28180   let fnacs(sn$,0,mat resp$,ck)
28200   let a$=lpad$(trim$(resp$(1)(1:10)),10)
28220   if trim$(a$)="" or ck=5 then goto RELEASE_PRINT
28240   read #1,using L730,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,seweravg nokey SCREEN_SELECT_ACCOUNT
28270   goto AFTER_READ_CUSTOMER
28280 ! /r
32000 RELEASE_PRINT: ! r:
32010   close #1: ioerr ignore
32020   close #3: ioerr ignore
32030   let fnpa_finis
32040   goto ENDSCR ! /r
34000 ENDSCR: ! r: print totals screen
34020   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
34040   let fntos(sn$="Bills-Total")
34060   let mylen=23 : let mypos=mylen+2
34080   let respc=0
34100   let fnlbl(1,1,"Total Bills Printed:",mylen,1)
34120   let fntxt(1,mypos,8,0,1,"",1)
34140   let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
34160   let fncmdset(52)
34180   let fnacs(sn$,0,mat resp$,ck) ! /r
34200 XIT: let fnxit
34220 ! 
38000 VBPRINT: ! r:
38040 ! -- Printer Program for Laser 1-Per Page Utility Bills
38060   gosub PRIOR_USAGES
38120   let fnpa_fontsize
38160 ! fnpa_txt("Blucksberg Mtn Water Association",158)
38180 ! fnpa_txt("8077 Blucksberg Drive",15,13)
38200 ! fnpa_txt("Sturgis, SD 57785",15,18)
38220   let fnpa_txt(trim$(pe$(1)),22,49)
38240   let fnpa_txt(trim$(pe$(2)),22,54)
38260   if trim$(pe$(3))="" then let pe$(3)=pe$(4): let pe$(4)=""
38280   let fnpa_txt(trim$(pe$(3)),22,59)
38300   let fnpa_txt(trim$(pe$(4)),22,64)
39000 ! fnpa_fontsize(18)
39020 ! fnpa_fontbold(1)
39030   let fnpa_pic('S:\acsub\logo_blucksberg.jpg',124,13)
39040   let fnpa_elipse(147,24,38,.5)
39060   let fnpa_elipse(147,24,37,.5)
39080 ! fnpa_fontbold
39100 ! fnpa_fontitalic(1)
39120 ! let fnpa_txt("Blucksberg Mtn",119,14)
39140 ! fnpa_fontsize(34)
39160 ! let fnpa_txt("Water",126,20)
39180 ! fnpa_fontsize(14)
39200 ! let fnpa_txt("Association",128,31)
39220   let fnpa_fontitalic(0)
39240   let fnpa_fontsize(9)
40000   let tmp_box_top=55
40010   let fnpa_line(tmp_box_left_pos=115,tmp_box_top,70,24, 1)
40020   let fnpa_txt('Billing Date:            '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),tmp_box_left_pos+5,tmp_box_top+4)
40030   let fnpa_txt("Account:      "&lpad$(trim$(z$),19),tmp_box_left_pos+5,tmp_box_top+8)
40040   let fnpa_txt('Due Date:                '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4),tmp_box_left_pos+5,tmp_box_top+12)
40050   let fnpa_txt("Billing Questions:   605-720-5013",tmp_box_left_pos+5,tmp_box_top+16)
40060   if final>0 then let fnpa_txt('Final Bill'&cnvrt$("PIC(ZZzZZzZZ)",0),80,tmp_box_top+15)
40070 ! 
40080   let lyne=65 : let adder=4
40090   let fnpa_txt("Meter Location: "&trim$(e$(1)) ,23,lyne+=adder)
40100   let fnpa_txt("Service From: "&cnvrt$("pic(zz/zz/zz)",d5)&" To: "&cnvrt$("pic(zz/zz/zz)",d6) ,23,lyne+=adder)
40110 ! 
40120   let fnpa_line(26,85,157)
40130 ! 
40140   let fnpa_fontsize
40150   let fnpa_fontitalic(1)
40160   let fnpa_fontbold(1)
41000 ! 
41060   let lyne=81
41080   let adder=4.5
41100   let fnpa_fontbold(1) : let fnpa_fontitalic
41120   let fnpa_txt("Activity Since "&date$(days(billing_date_prior,'ccyymmdd'),'mm/dd/yy'),80,lyne+=adder)
41140   let fnpa_fontbold(0)
41160   let fnpa_txt("Amount",170,lyne)
41180   let fn_add_activity_line("Balance as of "&date$(days(billing_date_prior,'ccyymmdd'),'mm/dd/yy'),prior_prior_balance)
41200   let fn_add_activity_line("Charges",activity_charge-g(11))
41220   let fn_add_activity_line("Penalties",activity_penalty)
41240   let fn_add_activity_line("Payments Received - Thank You",-activity_payment)
41260   let fn_add_activity_line("Credits",-activity_credit)
41280   let fn_add_activity_line("Debits",activity_debit)
41290   let fnpa_line(162,lyne+4,22)
41292   let fnpa_fontbold(1) ! on
41300   let fn_add_activity_line("Balance Forward",pb,1,110)
41660   let lyne+=adder
41680 ! let fnpa_fontbold(1) ! on
41700   let fnpa_line(26,lyne+=adder,157)
41720   let fnpa_txt("Current Charges",90,lyne+=1)
41740 ! let fnpa_txt("Current Charges",30,lyne+=8)
41750   let fnpa_fontbold
41760 ! let adder=5
41770   let fnpa_fontitalic
42000   let adder=4
42020   let lyne+=adder
42040   let fnpa_txt("Current",83,lyne) ! lyne=100
42060   let fnpa_txt("Reading",83,lyne+adder)
42080   let fnpa_txt("Previous",103,lyne)
42100   let fnpa_txt("Reading",103,lyne+adder)
42120   let fnpa_txt("Usage",131,lyne+adder)
42140   let fnpa_txt("Charge",170,lyne+adder)
42160   let add=4.5
43000   let lyne+=adder ! lyne=105
43020   if g(1)<>0 then 
43040     if g(1)>=14 then 
43060       let fnpa_txt("Base Water Service Fee",26,lyne+=adder)
43080       let fnpa_txt(cnvrt$("pic($$$$$$$$.## CR)",14),160,lyne)
43100     end if 
43120     let fnpa_txt("Water",26,lyne+=adder)
43140     let fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(1)), 79,lyne)
43160     let fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(2)),103,lyne)
43180     let fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(3)),123,lyne)
43200     let fnpa_txt(cnvrt$("pic($$$$$$$$.## CR)",g(1)-14),160,lyne)
43220   end if 
43240 ! 
43260   if g(2)<>0 then 
43280     let fnpa_txt("Sewer",26,lyne+=adder)
43300     if seweravg>0 then ! if have sewer average, use it for usage
43320       let fnpa_txt(cnvrt$("pic(zzzzzzzz#)",seweravg),121,lyne)
43340     else !  use water usage
43360       let fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(2)),121,lyne)
43380     end if 
43400     let fnpa_txt(cnvrt$("pic(--------.##)",g(2)),160,lyne)
43420   end if 
43440 ! 
43460   if g(3)<>0 then 
43480     let fnpa_txt("Association Fee *",26,lyne+=adder)
43500     let fnpa_txt(cnvrt$("pic($$$$$$$$.##)",g(3)),160,lyne)
43520   end if 
43540 ! 
43560   if g(4)<>0 then 
43580     let fnpa_txt("Gas",26,lyne+=adder)
43600     let fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(9)) ,078,lyne)
43620     let fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(11)) ,121,lyne)
43640     let fnpa_txt(cnvrt$("pic(--------.##)",g(4)),160,lyne)
43660   end if 
43680 ! 
43700 ! g(5)
43720 ! g(6)
43740 ! 
43760   if g(7)<>0 then 
43780     let fnpa_txt(servicename$(7),26,lyne+=adder)
43800     let fnpa_txt(cnvrt$("pic($$$$$$$$.##)",g(7)),160,lyne)
43820   end if 
43840 ! 
43860 ! g(8)
43880 ! 
43900   if g(9)<>0 then 
43920     let fnpa_txt("Tax",26,lyne+=adder)
43940     let fnpa_txt(cnvrt$("pic(--------.##)",g(9)),160,lyne)
43960   end if 
43980 ! 
44020   let fnpa_line(162,lyne+4,22) : let lyne+=1
44030   let fnpa_fontbold(1)
44050   let fn_add_activity_line("Total Current Charges",g(11), 1,110)
44060 ! lyne+=adder ! let fnpa_txt("Total Current Charges",110,lyne+=adder)
44080 ! let fnpa_txt(cnvrt$("pic(--------.##)",g(11)),160,lyne)
44090   let fnpa_fontbold(0)
44100   let lyne+=adder
44110   let fnpa_line(162,lyne+3,22)
44120   let fnpa_fontsize(14)
44140   let fnpa_fontbold(1)
44160   let fnpa_txt("Total Due",105,lyne+=adder)
44180   let fnpa_txt(cnvrt$("pic($$$$$$$$.## CR)",bal),150,lyne)
44200   let fnpa_line(162,lyne+=adder+1,22)
44220   let fnpa_line(162,lyne+=1,22)
44240   let fnpa_fontsize
44260   if uprc$(df$)="Y" then let fnpa_txt("Your bill has been scheduled for automatic withdrawal",85,lyne+=adder)
44280   let fnpa_fontsize
44300   let fnpa_fontbold
44320   if g(3)>0 then 
44340     let fnpa_txt('* Road maintenance and snow removal from main roads, vehicle and equipment',25,lyne+=adder)
44350     let fnpa_txt('repair and maintenance',25,lyne+=adder)
44360   end if 
44380   let fnpa_fontbold(1)
44400   let fnpa_line(26,165,157)
44420   let fnpa_fontsize
44440   let fnpa_fontitalic(1)
44460   let lyne=165
44480   let fnpa_txt("MESSAGE BOARD",92,lyne+=4)
44500   for j=1 to 13
44520     let fnpa_txt(mg$(j),5,lyne+=4)
44540   next j
44560   let fnpa_fontitalic
44580   let x=0
44600   for j=1 to 39
44620     let fnpa_line(x+=5,234,3,0) ! print #20: 'Call Print.AddLine('&str$(x+=5)&','&str$(234)&',3,0)'
44640   next j
54660   print #20: 'Call Print.MyFontSize(7)'
54680   let fnpa_txt("Please detach here and return with payment.  Mail to 8077 Blucksberg Dr or deposit in black box at bus stop.",18,236)
54700   let fnpa_fontsize
54720   let fnpa_txt("Account: "&lpad$(trim$(z$),16),40,243)
54740   let fnpa_txt('Due Date:        '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4),40,247)
54760   let fnpa_txt("Total Due:",40,251)
54780   let fnpa_txt(cnvrt$("pic(--------.##)",bal),70,251)
54800   if bal>0 then 
54820 !   let fnpa_txt("After "&cnvrt$("pic(##/##/##)",d4)&" pay "&cnvrt$("pic(---.##)",g(12)),40,255)
54830     let fnpa_txt("After "&cnvrt$("pic(##/##/##)",d4)&" Add "&cnvrt$("pic(---.##)",2.50),40,255)
54840   end if 
54860   let fnpa_txt(trim$(pe$(1)),130,243)
54880   let fnpa_txt(trim$(pe$(2)),130,247)
54900   if trim$(pe$(3))="" then let pe$(3)=pe$(4) : let pe$(4)=""
54920   let fnpa_txt(trim$(pe$(3)),130,251)
54940   let fnpa_txt(trim$(pe$(4)),130,255)
54960   let fnpa_newpage
54980   return  ! /r
62000   def fn_add_activity_line(aal_text$*80,aal_amt; aal_always_show,aal_desc_left_override)
62010     if aal_desc_left_override=0 then let aal_desc_left_override=30
62020     if aal_always_show or aal_amt<>0 then 
62040       let fnpa_txt(aal_text$,aal_desc_left_override,lyne+=adder)
62060       let fnpa_txt(cnvrt$("pic($$$$$$$$.## CR)",aal_amt),160,lyne)
62100     end if 
62120   fnend 
66000 BULKSORT: ! r: bulk sort order
66020   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
66040   open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=31",internal,output 
66060   do 
66080     read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof BS_EO_CUSTOMER
66100     write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
66120   loop 
68000 BS_EO_CUSTOMER: ! 
68020   close #1: ioerr ignore
68040   close #6: ioerr ignore
68060   execute "Index "&env$('Temp')&"\Temp."&session$&" "&env$('Temp')&"\Tempidx."&session$&" 1,19,Replace,DupKeys -n" ! ioerr L3120
68080   open #6: "Name="&env$('Temp')&"\Temp."&session$&",KFName="&env$('Temp')&"\Tempidx."&session$,internal,input,keyed 
68100 ! L3120: !
68120   return  ! /r
72000 IGNORE: continue 
73000 PRIOR_USAGES: ! r:
73140   mat usage=(0): mat billdate=(0) : mat reads=(0)
73150   restore #h_ubtransvb,key>=z$&"         ": nokey PU_XIT ! no average but active customer (use 0 usage)
73160 L3160: read #h_ubtransvb,using L3170: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PU_XIT
73170 L3170: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
73180   if p$<>z$ then goto PU_XIT
73190   if tcode<>1 then goto L3160 ! only charge transactions
73200   let usage(3)=usage(2): let billdate(3)=billdate(2) : let reads(3)=reads(2)
73210   let usage(2)=usage(1): let billdate(2)=billdate(1) : let reads(2)=reads(1)
73220   let usage(1)=wu: let billdate(1)=tdate : let reads(1)=wr
73230   goto L3160
73240 PU_XIT: ! 
73260   return  ! /r
76000 ! <updateable region: ertn>
76040 ERTN: let fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>
80000 READALTADR: ! r: read alternate billing address
80040   read #3,using 'form pos 11,4*c 30',key=z$: mat ba$ nokey RAA_2
80060   let e1=0 : mat pe$=("")
80080   for j=1 to 4
80100     if rtrm$(ba$(j))<>"" then 
80120       let e1=e1+1
80140       let pe$(e1)=ba$(j)
80160     end if 
80180   next j
80200   if trim$(pe$(2))="" then let pe$(2)=pe$(3): let pe$(3)=""
80220   if trim$(pe$(3))="" then let pe$(3)=pe$(4): let pe$(4)=""
80240   goto XIT_READALTADR
80260 ! 
80280 RAA_2: ! 
80300   let e1=0 : mat pe$=("")
80320   for j=2 to 4
80340     if rtrm$(e$(j))<>"" then let e1=e1+1 : let pe$(e1)=e$(j)
80360   next j
80380   if trim$(extra1$)<>"" then let pe$(4)=pe$(3): let pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
80400 XIT_READALTADR: ! 
80420   return  ! /r
