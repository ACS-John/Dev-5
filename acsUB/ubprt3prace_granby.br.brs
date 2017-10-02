00010 ! Replace S:\acsUB\ubprt3prace_granby
00020 ! pr bills for Carrizo Springs   3 per page prace
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnmsgbox,fnbarcode,fnpa_txt,fnpa_open,fnpa_finis,fnpa_line,fnpa_newpage
00030 ! ______________________________________________________________________
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(12)*60,txt$*100,mg$(3)*60,rw(22,13),cap$*128,fb$(3)*60
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100   dim servicename$(10)*20,service$(10)*2
00110   dim dueby$*30,usage(3),billdate(3),ml$(2)*80,tg(11)
00120 ! ______________________________________________________________________
00130   let fncno(cno,cnam$)
00140   let fnd1(d1)
00150   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input 
00160   read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
00170   close #21: 
00180   open #ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
00190   at$(1)=cnam$
00200   let z=23
00210   at$(1)=trim$(at$(1))(1:z)
00220   let x=len(at$(1)) : let y=z-x
00230   at$(1)=rpt$(" ",int(y/2))&at$(1)
00240   let z=26
00250   for j=2 to udim(at$)
00260     at$(j)=trim$(at$(j))(1:z)
00270     let x=len(at$(j)) : let y=z-x
00280     at$(j)=rpt$(" ",int(y/2))&at$(j)
00290   next j
00300   let linelength=62
00310 !  :  !
00320   let fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00330   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00340   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00350   open #ubtransvb=15: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00360   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative 
00370   read #20,using 'Form POS 1,10*C 20,10*C 2',rec=1: mat servicename$,mat service$
00380   close #20: 
00390 ! default message
00400   let mg$(1)="If you smell gas call"
00410   let mg$(2)="472-6556 or 472-3535."
00420 SCREEN1: ! 
00430   a$="" : let prtbkno=0
00440   let fntos(sn$="UBPrtBl1-1")
00450   let pf=26 : let ll=24
00460   let respc=0
00470   let fnlbl(1,1,"Current Reading Date:",ll,1)
00480   let fntxt(1,pf,8,8,1,"1",0,tt$)
00490   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",reading_date_cur)
00500   let fnlbl(2,1,"Previous Reading Date:",ll,1)
00510   let fntxt(2,pf,8,8,1,"1",0,tt$)
00520   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",reading_date_prior_s1)
00530   let fnlbl(3,1,"Penalty Due Date:",ll,1)
00540   let fntxt(3,pf,8,8,1,"1",0,tt$)
00550   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00560   let fnlbl(4,1,"Message on Bill:",ll,1)
00570   let fntxt(4,pf,30,30)
00580   let resp$(respc+=1)=mg$(1)
00590   let fntxt(5,pf,30,30)
00600   let resp$(respc+=1)=mg$(2)
00610   let fntxt(6,pf,30,30)
00620   let resp$(respc+=1)=mg$(3)
00630   let fnlbl(7,1,"Date of Billing:",ll,1)
00640   let fntxt(7,pf,8,8,1,"1")
00650   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00660   let fnlbl(8,1,"Starting Account:",ll,1)
00670 ! let fe$="ubm-act-nam"
00680 ! let datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno)
00690 ! let indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno)
00700 ! let kp=1741: let kl=9 : let dp=41 : let dl=30
00710 ! let fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2)
00712   let fncmbact(8,pf, 1) ! ,container,indexfile$*200)
00720   let resp$(respc+=1)="[All]"
00730   let fnlbl(9,1,"Route Number:",ll,1)
00740   let fncmbrt2(9,pf)
00750   let resp$(respc+=1)="[All]"
00760   let fnchk(10,pf,"Select Accounts to Print",1)
00770   let resp$(respc+=1)="False"
00780   let fncmdset(3)
00790   let fnacs(sn$,0,mat resp$,ck)
00800   if ck=5 then goto ENDSCR
00810   let reading_date_cur_s1=val(resp$(1))
00820   let reading_date_prior_s1=val(resp$(2))
00830   let d4 =val(resp$(3))
00840   let mg$(1)=resp$(4)
00850   let mg$(2)=resp$(5)
00860   let mg$(3)=resp$(6)
00870   let d1=val(resp$(7))
00880   if resp$(8)="[All]" then 
00890     a$=""
00900   else 
00910     a$=lpad$(trim$(resp$(8)(1:10)),10)
00920   end if 
00930   if resp$(9)="[All]" then 
00940     let prtbkno=0
00950   else 
00960     let prtbkno = val(resp$(9))
00970   end if 
00980   if resp$(10)="True" then let sl1=1: let z$="" else let sl1=0
00990   if trim$(a$)<>"" then 
01000     read #1,using 'form pos 1,c 10,pos 1741,n 2,n 7',key=a$: z$,route,sequence nokey SCREEN1
01010     let holdz$=z$: begin=1
01020     let st1=1
01030   end if 
01035 ! 
01040   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
01045   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
01050   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
01055   if exists(env$('Q')&"\UBmstr\Cass1.h"&str$(cno)) then let fn_sort1 else let fn_bulksort
01060 ! ______________________________________________________________________
01065   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
01070   let fnpa_open
01075 ! ______________________________________________________________________
01080 ! on fkey 5 goto RELEASE_PRINT
01082 L650: if sl1=1 then goto SCREEN3
01084 L660: read #h_temp,using L690: bc$,z$ eof RELEASE_PRINT
01086   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L660 ! start with
01088   begin=0 ! cancel starting account
01090 L690: form pos 1,c 12,c 10
01092   read #1,using F_CUSTOMER,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,dr$,bc,da$ nokey L660
01094 ! if bal<=0 then goto L660 ! skip anyone with a balance less than or equal to zero
01096   if estimatedate>0 then let est=1 else let est=0
01098 F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1,pos 1712,c 1,c 9,n 2,c 17
01100   if prtbkno=0 then goto L750
01102   if prtbkno><route then goto RELEASE_PRINT
01104 L750: if f><d1 then goto L650
01106 L760: let e1=0 : mat pe$=("")
01108   for j=2 to 4
01110     if rtrm$(e$(j))<>"" then 
01112       let e1=e1+1
01114       let pe$(e1)=e$(j)
01116     end if 
01118   next j
01120   if st1=0 then goto READALTADR
01122 ! If ST1$=Z$ Then Let ST1=0 Else Goto 560
01124 READALTADR: ! 
01126 ! read alternate billing address
01128   read #3,using L860,key=z$: mat ba$ nokey L950
01130   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L950
01132 L860: form pos 11,4*c 30
01134   let e1=0 : mat pe$=("")
01136   for j=1 to 4
01138     if rtrm$(ba$(j))<>"" then 
01140       let e1=e1+1
01142       let pe$(e1)=ba$(j)
01144     end if 
01146   next j
01148   if trim$(pe$(2))="" then let pe$(2)=pe$(3): let pe$(3)=""
01150   if trim$(pe$(3))="" then let pe$(3)=pe$(4): let pe$(4)=""
01152   goto L1070
01154 ! ______________________________________________________________________
01156 L950: ! 
01158   if trim$(extra1$)<>"" then let pe$(4)=pe$(3): let pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
01160   goto L1070
01162 ! ______________________________________________________________________
01164 RELEASE_PRINT: ! 
01166   close #1: ioerr L1010
01168 L1010: close #3: ioerr L1020
01170 L1020: ! 
01172   let fnpa_finis
01174   goto ENDSCR
01176 ! ______________________________________________________________________
01178 L1070: ! 
01180   let pb=bal-g(11)
01182 ! if bal<=0 then let g(9)=g(10)=0 ! don't show penalty if balance 0 or less
01184   let fb$(1)=mg$(1)
01186   let fb$(2)=mg$(2)
01188   let fb$(3)=mg$(3)
01190 ! If C4>0 Then Let FB$(1)="          Final Bill" : Let FB$(2)="": Let FB$(3)=""
01192 ! ______________print bill routine______________________________________
01194   let fn_vbprint
01196 ! _____________end of pr routine______________________________________
01198   bct(2)=bct(2)+1
01200 ! .   ! accumulate totals
01202   goto L650
01204 ! ______________________________________________________________________
01206 SCREEN3: ! r:
01208   let fntos(sn$:="UBPrtBl1-2")
01210   let txt$="Account (blank to stop)"
01212   let fnlbl(1,1,txt$,31,1)
01214   if trim$(z$)<>"" then 
01216     let txt$="Last Account entered was "&z$
01218     let fnlbl(3,1,txt$,44,1)
01220   else 
01222     let txt$=""
01224     let fnlbl(3,1,txt$,44,1)
01226   end if 
01228   let fncmbact(1,17)
01230   let resp$(1)=a$
01232   let fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
01234   a$=lpad$(trim$(resp$(1)(1:10)),10)
01236   if trim$(a$)="" then goto RELEASE_PRINT
01238   if ck=5 then goto RELEASE_PRINT
01240   read #1,using F_CUSTOMER,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,dr$,bc,da$ nokey SCREEN3
01242   goto L760 ! /r
01244 ! ______________________________________________________________________
01246 ENDSCR: ! 
01248   let fn_screen_totals
01250   : let fnxit
01252 ! ______________________________________________________________________
01254 ERTN: let fnerror(program$,err,line,act$,"xit")
01256   if uprc$(act$)<>"PAUSE" then goto L1780
01258   execute "List -"&str$(line) : pause : goto L1780
01260   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01780 L1780: execute act$
01790   goto ERTN
01800 ! ______________________________________________________________________
01810   def fn_screen_totals
01820     if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01830     let fntos(sn$="Bills-Total")
01840     let mylen=23 : let mypos=mylen+2
01850     let respc=0
01860     let fnlbl(1,1,"Total Bills Printed:",mylen,1)
01870     let fntxt(1,mypos,8,0,1,"",1)
01880     let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
02000     let fncmdset(52)
02010     let fnacs(sn$,0,mat resp$,ck)
02020   fnend 
02030   def fn_sort1 ! SELECT & SORT
02040     open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed 
02050     open #h_temp:=6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=22",internal,output 
02060     let s5=1
02070     if prtbkno=0 then 
02080       let routekey$=""
02090     else 
02100       let routekey$=cnvrt$("N 2",prtbkno)&"       "
02110 ! .    ! key off first record in route (route # no longer part of customer #)
02120     end if 
02130     restore #2,search>=routekey$: 
02135 L1380: read #2,using L1390: z$,f,route eof END5
02137 L1390: form pos 1,c 10,pos 296,pd 4,pos 1741,n 2
02139     if prtbkno=0 then goto L1420
02141     if prtbkno><route then goto END5
02143 L1420: if f><d1 then goto L1380
02145     cr$=""
02147     read #5,using "Form POS 96,C 12",key=z$: cr$ nokey L1450
02149 L1450: write #h_temp,using "Form POS 1,C 12,C 10": cr$,z$
02151     goto L1380
02153 ! 
02155 END5: close #h_temp: 
02157     open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
02159 L1500: form pos 1,c 128
02161     write #9,using L1500: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
02163     write #9,using L1500: "Mask 1,19,C,A"
02165     close #9: 
02167     execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1550
02169 L1550: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
02171     open #h_temp: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
02173     open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
02175   fnend 
02177   def fn_bulksort ! bulk sort order
02179     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
02181     open #h_temp:=6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
03020 L3020: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3050
03030     write #h_temp,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
03040     goto L3020
03050 L3050: close #1: ioerr L3060
03060 L3060: close #h_temp: ioerr L3070
03070 L3070: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L3090
03080     open #h_temp:=6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
03090 L3090: ! 
03100   fnend 
03110   def fn_prior_usages
03120     mat usage=(0): mat billdate=(0)
03130     restore #15,key>=z$&"         ": nokey L3210 ! no average but active customer (use 0 usage)
03140     do 
03150       read #ubtransvb,using L3140: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L3210
03155 L3140: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
03160       if p$<>z$ then goto L3210
03165       if tcode=1 then ! only charge transactions
03170         let usage(3)=usage(2): billdate(3)=billdate(2)
03175         let usage(2)=usage(1): billdate(2)=billdate(1)
03180         let usage(1)=wu: billdate(1)=tdate
03185       end if  ! tcode=1
03190     loop 
03210 L3210: ! 
03220   fnend 
03230   def fn_vbprint
03240 ! -- Printer Program for three per page  Utility Bills
03250     if reading_date_cur_s1=0 then let reading_date_cur=d3 else let reading_date_cur=reading_date_cur_s1
03260     if reading_date_prior_s1=0 then let reading_date_prior=d2 else let reading_date_prior=reading_date_prior_s1
03270     let line_height=3
03280     let pos_column_column=5 ! 20 !  pos_column_column=remit side position
03290     let pos_right_column=120 ! pos_right_column=customer side position
03300     let fnpa_line(pos_right_column+12,factor+0,62,line_height+12,1) ! the box around the company name and address
03310 ! pr #20: 'Call Print.AddLine('&STR$(4)&','&STR$(factor+2)&',64,'&str$(line_height*3+1)&',True)'
03320     pr #20: "Call Print.MyFontBold(True)"
03330     pr #20: 'Call Print.MyFontSize(12)'
03340     pr #20: 'Call Print.MyFont("Courier New")'
03350     let fnpa_txt(at$(1),pos_right_column+15,factor+line_height*1-1)
03360     pr #20: 'Call Print.MyFont("Lucida Console")'
03370     pr #20: 'Call Print.MyFontSize(10)'
03380     pr #20: 'Call Print.MyFontBold(False)'
03390     let fnpa_txt(at$(2),pos_right_column+13,factor+line_height*2+1-.2)
03400     let fnpa_txt(at$(3),pos_right_column+15,factor+line_height*3+1)
03410 PRESORTED_BOX: ! 
03420     pr #20: 'Call Print.MyFontSize(9)'
03430     let fnpa_txt("Please return this",pos_column_column+8,factor+line_height*1)
03440     let fnpa_txt("side with payment to:",pos_column_column+8,factor+line_height*2)
03450     let fnpa_txt(trim$(cnam$),pos_column_column+8,factor+line_height*3)
03460     let fnpa_txt(e$(2),pos_right_column+8,factor+line_height*7)
03470     let fnpa_txt('Billing Date:'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),pos_right_column+5,factor+line_height*8)
03480     let fnpa_txt(trim$(z$),pos_right_column+54,factor+line_height*8)
03490     pr #20: 'Call Print.AddLine('&str$(pos_right_column)&','&str$(factor+line_height*9+1)&',79,0)'
03500     pr #20: 'Call Print.MyFontSize(9)'
03510     let fnpa_txt("Service",pos_right_column,factor+line_height*9+2)
03520     let fnpa_txt("Prior",pos_right_column+18,factor+line_height*9+2)
03530     let fnpa_txt("Reading",pos_right_column+31,factor+line_height*9+2)
03540     let fnpa_txt("Usage",pos_right_column+51,factor+line_height*9+2)
03550     let fnpa_txt("Charge",pos_right_column+66,factor+line_height*9+2)
03560     pr #20: 'Call Print.MyFontSize(10)'
03570     let fnpa_txt('Billing Date:',pos_column_column-2,factor+line_height*8)
03580     let fnpa_txt(cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),pos_column_column+35,factor+line_height*8)
03590     let fnpa_txt('Past Due:',pos_column_column-2,factor+line_height*09)
03600     let fnpa_txt(fnformnumb$(pb,2,9),pos_column_column+30,factor+line_height*09)
03610     let fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',pos_column_column-2,factor+line_height*11)
03620     let fnpa_txt(fnformnumb$(bal,2,9),pos_column_column+30,factor+line_height*11)
03630     let fnpa_txt('After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',pos_column_column-2,factor+(line_height*12)+1)
03640     let fnpa_txt(fnformnumb$(bal+g(12)-g(11),2,9),pos_column_column+30,factor+(line_height*12)+1)
03650 ! let penbal=bal ! If BAL>0 Then Let PENBAL=BAL+MIN(ROUND(BAL*.10,2),g(10)) Else Let PENBAL=0  ! don't know what the penalty rate is
03660 ! if bal>0 then
03670 !  let fnpa_txt(fnformnumb$(penbal,2,9),pos_column_column+30,factor+(line_height*12)+1)
03680 ! else 
03690 !  let fnpa_txt(fnformnumb$(bal,2,9),pos_column_column+30,factor+(line_height*12)+1)
03700 ! end if  ! bal>0
03710     pr #20: 'Call Print.MyFontsize(10)'
03720     let fnpa_txt("From:"&cnvrt$("pic(zz/zz/zz)",reading_date_prior)&"  To:"&cnvrt$("pic(zz/zz/zz)",reading_date_cur),pos_column_column+6,factor+line_height+=13)
03730     let fnpa_txt("From:"&cnvrt$("pic(zz/zz/zz)",reading_date_prior)&"  To:"&cnvrt$("pic(zz/zz/zz)",reading_date_cur),pos_right_column+6,factor+line_height)
03740     let line_height+=14
03750     pr #20: 'Call Print.MyFontsize(9)'
03760     let fn_print_serivce_metered(1,d(2),d(1),d(3)) ! Water
03770     let fn_print_serivce_metered(2) ! Sewer
03780 ! let fn_print_serivce_metered(3,d(6),d(5),d(7)) ! Electric
03790     let fn_print_serivce_metered(4,d(10),d(9),d(11)) ! Gas
03800     if g(5)>0 then let fn_print_serivce_metered(5) ! Service 5
03810     if g(6)>0 then let fn_print_serivce_metered(6) ! Service 6
03820     if g(7)>0 then let fn_print_serivce_metered(7) ! Service 7
03830     if g(8)>0 then let fn_print_serivce_metered(8) ! Service 8
03840     if g(9)>0 then let fn_print_serivce_metered(9) ! Service 9
03850     pr #20: 'Call Print.MyFontsize(10)'
03860 ! allow for three forms
03870     if line_height<90 then 
03880       let line_height=62
03890     else if line_height<=180 then 
03900       let line_height=152
03910     else if line_height>180 then 
03920       let line_height=242
03930     end if 
03940     let fnpa_txt(z$,pos_column_column,factor+52)
03950 ! let fnbarcode(z$,103,factor+45)
03960     if pb><0 then 
03970       let de2$="Prior Balance"
03980       let txt$=de2$&"          "&cnvrt$("pic(-----.--)",pb)
03990       let fnpa_txt(txt$,pos_right_column+6,factor+line_height)
04000     else 
04010       let de2$="             "
04020     end if 
04030     if est=1 then let de2$="BILL ESTIMATED" : goto L2710 else let de2$="              "
04040     if final>0 then let de2$="   Final Bill  " : goto L2710 else let de2$="               "
04050     if df$="Y" then let de2$="   DRAFTED     ": goto L2710 else let de2$="               " : goto L2720
04055 L2710: let txt$=de2$
04060     let fnpa_txt(txt$,pos_right_column+6,factor+line_height+=7.0)
04062 L2720: pr #20: 'Call Print.MyFontSize(12)'
04064     let txt$=pe$(1)
04066     let fnpa_txt(txt$,pos_column_column,factor+58)
04068     pr #20: 'Call Print.MyFontsize(10)'
04070     pr #20: 'Call Print.AddLine('&str$(pos_right_column)&','&str$(factor+line_height+=3.5)&',79,0)'
04072     let fnpa_txt("Due by: "&cnvrt$("pic(zz/zz/zz)",d4),pos_right_column+6,factor+line_height+=2.5)
04074     pr #20: 'Call Print.MyFontSize(12)'
04076     let fnpa_txt(cnvrt$("pic(---,---.--)",bal),pos_right_column+48,factor+line_height)
04078     let fnpa_txt(pe$(2)(1:25),pos_column_column,factor+61.50)
04080     pr #20: 'Call Print.MyFontsize(10)'
04082     let fnpa_txt(e$(1)(1:18),pos_column_column,factor+42)
04084     pr #20: 'Call Print.MyFontSize(12)'
04086     let fnpa_txt(pe$(3),pos_column_column,factor+65)
04088 ! pr #20: 'Call Print.MyFontsize(10)'
04090 ! pr #20: 'Call Print.MyFontSize(12)'
04092     let fnpa_txt(pe$(4)(1:25),pos_column_column,factor+68.50)
04094     pr #20: 'Call Print.MyFontsize(10)'
04096     let fnpa_txt(mg$(1),pos_right_column+6,factor+line_height+=3.5)
04098     let fnpa_txt(mg$(2),pos_right_column+6,factor+line_height+=3.5)
04100     let fnpa_txt(mg$(3),pos_right_column+6,factor+line_height+=3.5)
04102     if line_height<90 then let updown=3
04104     if line_height>90 and line_height<180 then let updown=6.5
04106     if line_height>180 and line_height<270 then let updown=10
04108 ! if trim$(cr$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(3)&','&str$(updown)&',"'&cr$&'")'
04110     bills+=1
04112     if int(bills/3)=bills/3 then let fnpa_newpage: let factore=0: goto L2980
04114     let factor=factor+94 ! was 96
04116     if factor>=270 then let factor=0
04118 L2980: ! 
04120   fnend 
04122   def fn_print_serivce_metered(service_number; service_reading_prior,service_reading_current,service_usage)
04124     dim psm_txt$*512
04126     if g(service_number)>0 and service_reading_current=0 and service_reading_prior=0 then 
04128       let psm_txt$=rpad$(servicename$(service_number)(1:23),23)
04130     else if g(service_number)>0 and service_reading_current=0 then 
04132       let psm_txt$=rpad$(servicename$(service_number)(1:14),14)&cnvrt$("pic(zzzzzzzzz)",service_reading_prior)
04134     else if g(service_number)>0 then 
04136       let psm_txt$=rpad$(servicename$(service_number)(1:5),5)&cnvrt$("pic(zzzzzzzzz)",service_reading_prior)&cnvrt$("pic(zzzzzzzzz)",service_reading_current)
04138     else 
04140       let psm_txt$='  '
04142     end if 
04143 ! if service_reading_prior>service_reading_current then let psm_temp=service_reading_current : service_reading_current=service_reading_prior : service_reading_prior=psm_temp
04144     let fnpa_txt(psm_txt$&cnvrt$("pic(zzzzzzzz)",service_usage)&cnvrt$("pic(-----z.zz)",g(service_number)),pos_right_column,factor+line_height+=3.5)
04146     let fnpa_txt(servicename$(service_number)(1:5)&cnvrt$("pic(-----z.zz)",g(service_number)),pos_column_column+55,factor+line_height)
04148   fnend  ! fn_print_serivce_metered
