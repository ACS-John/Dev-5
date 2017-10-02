00010 ! Replace S:\acsUB\Conversion\Txt2UBmstr
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnerror,fnxit,fncno
00040   dim s_file$*40,cap$*128,line$*234,cnam$*40,line2$*41,tg(11),x(15)
00050   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
00060   dim rw4(22,13),gb(10),extra(23),extra$(11)*30,df$*1,dr$*9,dc$*2,dc$*17
00070   dim udf$*256,work$*256,work_addr$*256
00080 ! ______________________________________________________________________
00090   cap$="Convert from text file to Customer file"
00100   let fncno(cno,cnam$)
00110   let udf$=env$('temp')&'\'
00120 ! ______________________________________________________________________
00130 MENU1: ! 
00140   pr newpage
00150   pr fields "10,20,Cc 40,R,N": cap$
00160   pr fields "12,2,Cr 27,N": "Destination Company Number:"
00170   pr fields "13,2,Cr 27,N": "Source File Path and Name:"
00180   let io1$(1)="12,30,Nz 3,UT,N" !:
        let io1$(2)="13,30,C 40,UT,N"
00190   pr fields "15,21,Cc 19,B,1": "Start (F1)"
00200   pr fields "15,41,Cc 19,B,99": "Exit (Esc)"
00210   pr fields "18,01,Cc 80,R,N": "WARNING:  All Files in the Destination Company Number will"
00220   pr fields "19,01,Cc 80,R,N": "be duplicates of Company Number 1 with new merged records!"
00230   pr fields "21,01,Cc 80,R,N": "No other users may be using Utility Billing for this to run"
00240   let s_file$="C:\kentemp\ken.prn"
00250   rinput fields mat io1$: cno,s_file$
00260   if cmdkey=99 then goto XIT
00270   if cmdkey=1 then goto START
00280   goto MENU1
00290 ! ______________________________________________________________________
00300 XIT: let fnxit
00310 ! ______________________________________________________________________
00320 START: ! 
00330 ! 
00340   open #1: "Name="&s_file$&',RecL=234',display,input ioerr MENU1
00350   open #11: 'Name=c:\kentemp\lb2.prn,RecL=42',display,input ioerr MENU1
00360   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",RecL=2067,replace",internal,output 
00370   open #19: "Name="&env$('Q')&"\UBmstr\test.h"&str$(cno)&",RecL=132,replace",internal,output 
00380   open #15: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&str$(cno)&",Shr,Replace,RecL=102,KPs=1,KLn=19",internal,outin,keyed 
00390   let work$=env$('Q')&"\UBmstr\Reads_and_Chgs.h"&str$(cno) !:
        let work_addr$=env$('Q')&"\UBmstr\Reads_and_Chgs-Key.h"&str$(cno) !:
        ! synconrize these settings with S:\acsUB\ubCalk
00400   open #work=4: "Name="&work$&",KFName="&work_addr$&",RecL=74,KPs=1,KLn=10,Replace",internal,outin,keyed 
00410 LOOP_TOP: ! 
00420 L420: linput #1: line$ eof DONE
00430   if line$(1:7)='company' then goto L420
00440   if line$(1:3)="tax" then goto DONE
00500   if trim$(z$)<>"" and lpad$(trim$(line$(19:28)),10)<>z$ then goto L950 ! write this record  (there is a seperate record for each serice in the text file)
00510 L510: let z$=lpad$(trim$(line$(19:28)),10)
00520   let e$(2)=trim$(rtrm$(rtrm$(line$(31:43))&" "&rtrm$(line$(44:71)))(1:30)) ! Name
00530   let e$(1)=e$(3)=rtrm$(rtrm$(line$(72:79))&" "&rtrm$(line$(80:100))) ! street
00540   let e$(4)="CARRIZO SPRINGS, TX 78834"
00550   let extra(1)=val(line$(102:104))
00560   let extra(2)=val(line$(110:114))
00570   if extra(2)=0 then let extra(2)=1 ! INACTIVE ACCOUNTS WHO DON'T HAVE A SEQUENCE #
00580   let f=22810 ! ALWAYS FIX
00590   let rate$=trim$(line$(147:153))
00600   if rate$(1:3)="PKU" or rate$(1:3)="PKE" then goto L920 ! handle cannister pick up seperately
00610   if rate$(1:3)="SCH" then a(2)=19: let g(2)=val(line$(227:234)) !:
          let gb(2)=val(line$(219:226))+g(2): goto L920 ! handle school sewer seperately
00620   if rate$(1:3)="HS1" then a(5)=0: let g(5)=val(line$(227:234)) !:
          let gb(5)=val(line$(219:226))+g(5): b(5)=val(line$(227:234)): goto L920 ! handle school trash
00630   if rate$(1:3)="MES" then a(5)=0: let g(5)=val(line$(227:234)) !:
          let gb(5)=val(line$(219:226))+g(5): b(5)=val(line$(227:234)): goto L920 ! MIDDLE SCHOOL TRASH
00640   if rate$(1:3)="NES" then a(5)=0: let g(5)=val(line$(227:234)) !:
          let gb(5)=val(line$(219:226))+g(5): b(5)=val(line$(227:234)): goto L920 ! NORTH ELEMENTRY TRASH
00650   if rate$(1:3)="CME" then a(4)=2: a(6)=9: goto L920 ! handle exempt commercial sewer seperately as tax exempt commercial
00660   if rate$(1:3)="MHT" then a(5)=4: let extra(12)=1: goto L920 ! handle mobile home trash seperately as taxable
00670   if rate$(1:3)="ASF" then goto L920 ! handle assessmemt seperately
00680   if line$(149:149)<"0" or line$(149:149)>"9" then goto L920
00690   if rate$(1:3)="WR1" then a(1)=1 ! residential in
00700   if rate$(1:3)="WR2" then a(1)=2 ! residential out
00710   if rate$(1:3)="WR3" then a(1)=2 ! not sure about this - both seem to be residential outside of city
00720   if rate$(1:3)="WR5" then a(1)=5 ! senior citizens
00730   if rate$(1:3)="WC1" then a(1)=3 ! commercial in
00740   if rate$(1:3)="WC2" then a(1)=4 ! commercial out
00750   if rate$(1:3)="WR4" then a(1)=6 ! two house
00760   if rate$(1:2)="MR" then a(4)=1 ! residential
00770   if rate$(1:2)="MC" then a(4)=2 ! commercial
00780   if rate$(1:2)="SR" then a(2)=1 ! sewer residential
00790   if rate$(1:3)="MHS" then a(2)=3 ! mobil home sewer
00800   if rate$(1:2)="SC" then a(2)=2 ! commercial sewer
00810   if rate$(1:2)="TR" then a(5)=1: let extra(12)=1 ! assume taxable on trash unless changed to not taxable - residential trash
00820   if rate$(1:2)="TC" then a(5)=2: let extra(12)=1 ! assume taxable on trash unless changed to not taxable - commercial trash
00830   if rate$(1:3)="TCE" or rate$(1:3)="CNE" then a(5)=2: let extra(12)=9: let g(2)=val(line$(227:234)) !:
          let gb(2)=val(line$(219:226))+g(2) : goto L920 ! handle cannister pick up seperately
00840   let extra(4)=013110
00850   let extra(3)=22810
00860   if rate$(1:2)="WR" or rate$(1:2)="WC" then let d(2)=val(line$(171:178)) ! prior water reading
00870   if rate$(1:2)="MR" or rate$(1:2)="MC" then let d(10)=val(line$(171:178)) ! prior gas reading
00880   if rate$(1:2)="WR" or rate$(1:2)="WC" then let d(1)=val(line$(179:186)) ! current water reading
00890   if rate$(1:2)="MR" or rate$(1:2)="MC" then let d(9)=val(line$(179:186)) ! current gas reading
00900   if rate$(1:2)="WR" or rate$(1:2)="WC" then let d(3)=val(line$(187:194)) ! current water usage
00910   if rate$(1:2)="MR" or rate$(1:2)="MC" then let d(11)=val(line$(187:194)) ! current gas usage
00920 L920: let today=22810
00930   gosub LAST_PART_OF_RECORD
00940   goto LOOP_TOP
00950 L950: let g(11)=sum(g): let g(12)=g(11)
00960   bal=sum(gb) ! balance
00970   if sum(g)=(0) then let extra(17)=1 ! IF NO CHARGES ASSUME FINALED BILLED
00980   write #2,using L1030: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
01000   write #19,using "form pos 1,c 10,n 10.2": z$,total_balance
01010   let total_balance=0
01020   b(2)=0
01030 L1030: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
01040   let tdate=20100228: let tcode=1: let tamount=g(12)
01050   for j=1 to 10
01060     let tg(j)=g(j)
01070   next j
01080   write #15,using L1170: z$,tdate,tcode,tamount,mat tg,d(1),d(3),0,0,d(9),d(11),bal,pcode
01090   if bal>g(12) then goto L1100 else goto L1180
01100 L1100: for j=1 to 10
01110     let tg(j)=gb(j)-g(j)
01120   next j
01130   let tdate=20100228
01140   let tamount=bal-g(11)
01150   write #15,using L1170: z$,tdate,tcode,tamount,mat tg,0,0,0,0,0,0,bal-g(11),pcode
01160 L1160: form pos 1,c 10,4*pd 5,7*pd 4.2,3*pd 5,n 1
01170 L1170: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
01180 L1180: let x(1)=d(1) : let x(2)=d(9)
01190   if extra(17)>0 then goto L1210 ! SKIP FINAL BILLS
01200   write #work,using L1160,rec=rctr: z$,mat x
01210 L1210: mat a=(0): mat d =(0): let extra(11)=extra(12)=0: mat g=(0): mat gb=(0): mat b=(0): let extra(14)=0
01220   goto L510 ! process last record written
01230 ! ______________________________________________________________________
01240 ! ______________________________________________________________________
01250 ! ______________________________________________________________________
01260 DONE: ! 
01270   pr newpage
01280   pr fields "12,20,Cc 40,N": "Please wait: reIndexing Customer file..."
01290   close #1: ioerr L1300
01300 L1300: close #2: ioerr L1310
01310 L1310: execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&" 1 10 Replace DupKeys -n"
01320   close #19: 
01330   execute "Index "&env$('Q')&"\UBmstr\test.h"&str$(cno)&' '&env$('Q')&"\UBmstr\testindx.h"&str$(cno)&" 1 10 Replace DupKeys -n"
01340   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx2.h"&str$(cno)&" 354 7 Replace DupKeys -n"
01350   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx3.h"&str$(cno)&" 11 25 Replace DupKeys -n"
01360   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx4.h"&str$(cno)&" 361 12 Replace DupKeys -n"
01370   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx5.h"&str$(cno)&" 1741/1743 2/7 Replace DupKeys -n"
01380   close #15: 
01390   execute "Index "&env$('Q')&"\UBmstr\UBTransvb.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBTrindx.h"&str$(cno)&" 1 19 Replace DupKeys -n"
01400   pr newpage
01410   pr fields "11,20,Cc 40,N": "Completed Sucessfully."
01420   pr fields "12,20,Cc 40,B,X0D": "Press any key to continue..."
01430   goto XIT
01440 ! ______________________________________________________________________
01450 METER_ADDRESS: ! 
01460   let e$(1)=rtrm$(line$(93:117))(1:30)
01470   let x=pos(e$(1)," ",1)
01480   let x2=val(e$(1)(1:x)) conv L1500
01490   let e$(1)=rtrm$(e$(1)(x+1:30))&" "&e$(1)(1:x-1)
01500 L1500: return 
01510 ! ______________________________________________________________________
01520 ! <Updateable Region: ERTN>
01530 ERTN: let fnerror(program$,err,line,act$,"xit")
01540   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01550   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01560   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01570 ERTN_EXEC_ACT: execute act$ : goto ERTN
01580 ! /region
01590 LAST_PART_OF_RECORD: ! 
01600 L1600: linput #11: line2$ eof L2180
01610   if trim$(line2$(1:20))="" then goto L1600
01620   if line2$(1:3)="tax" then goto L1600
01630   if rate$(1:2)="WR" or rate$(1:2)="WC" then let g(1)=val(line$(227:234)): let gb(1)=val(line$(219:227))+g(1) ! previous balance of water plus current charge
01640   if rate$(1:3)="WR5" then let g(1)=round(val(line$(227:234))*.75,2): let gb(1)=val(line$(219:227))+g(1) ! previous balance of water plus current charge
01650   if rate$(1:3)="CME" or rate$(1:2)="MR" or rate$(1:2)="MC" then let g(4)=val(line$(227:234)): let gb(4)=val(line$(219:227))+g(4) ! previous balance of gas plus current charge
01660   if rate$(1:2)="SR" or rate$(1:2)="SC" then let g(2)=val(line$(227:234)): let gb(2)=val(line$(219:227))+g(2) ! previous balance of sewer plus current charge
01670   if rate$(1:2)="SR" or rate$(1:2)="SC" then let g(2)=val(line$(227:234)): let gb(2)=val(line$(219:227))+g(2) ! previous balance of sewer plus current charge
01671   if rate$(1:3)="MHS" then let g(2)=val(line$(227:234)): let gb(2)=val(line$(219:227))+g(2): b(2)=val(line$(227:234)) ! previous balance of sewer plus current charge  AND STANDARD CHARGE ON MOBILE HOMES
01680   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 11.25 then a(2)=2 ! residentisl 5 fixtures
01690   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 11.85 then a(2)=3 ! residentisl 6 fixtures
01700   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 12.37 then a(2)=4 ! residentisl 7 fixtures
01710   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 17.55 then a(2)=10 ! small business
01720   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 35.10 then a(2)=11 ! large business
01730   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 26.33 then a(2)=12 ! restaurants
01740   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 26.33 then a(2)=13 ! garages   (can't tell difference in restaurants
01750   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 52.50 then a(2)=14 ! courthouse
01760   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 17.55 then a(2)=15 ! churches   (cant tell difference in small business
01770   if (rate$(1:3)="HOS" or rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 192.98 then a(2)=16 !:
          let g(2)=val(line$(227:234)) !:
          let gb(2)=val(line$(219:226))+g(2) ! dcm hospital
01771   if trim$(z$)="0121046800" and val(line$(227:234))= 348.30 then a(2)=19 !:
          let g(2)=val(line$(227:234)) !:
          let gb(2)=val(line$(219:226))+g(2): b(2)=val(line$(227:234)) ! HIGFHSCHOOL SEWER
01780   if rate$(1:3)="HSA" and val(line$(227:234))>0 then a(5)=0 !:
          let g(5)=val(line$(227:234)) !:
          let gb(5)=val(line$(219:226))+g(5): b(5)=val(line$(227:234)) ! 
01790   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 224.70 then a(2)=17 ! nursing home
01800   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 87.75 then a(2)=18 ! junior high
01810   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 131.63 then a(2)=19 ! senior high high
01820   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 12.30 then a(2)=20 ! mobile home
01830   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 14.03 then a(2)=40 ! motel
01840   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 239.03 then a(2)=80: b(2)=val(line$(227:234)) ! housing aut
01850   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 189.53 then a(2)=80: b(2)=val(line$(227:234)) ! housing aut
01860   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 18.53 then a(2)=41: b(2)=val(line$(227:234)) ! motels
01870   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 23.03 then a(2)=41: b(2)=val(line$(227:234)) ! motels
01880   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 27.53 then a(2)=41: b(2)=val(line$(227:234)) ! motels
01890   for j=1 to 60
01900     if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 27.53+(4.50*j) then a(2)=41: b(2)=27.53+(4.50*j) ! motels
01910   next j
01920   for j=1 to 60
01930     if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 14.03+(j*4.50) then a(2)=41: b(2)=14.03+(j*4.50) ! motels
01940   next j
01950   if rate$(1:2)="SC" and a(2)=1 and val(line$(227:234))<>10.50 and val(line$(227:234))>0 then a(2)=5: b(2)=val(line$(227:234)) ! if sewer code 1 and not a normal charge the use as a standard chargerge
01960   if (rate$(1:3)="BPS" or rate$(1:3)="APT") and val(line$(227:234))<>10.50 and val(line$(227:234))>0 then a(2)=0: b(2)=val(line$(227:234)) !:
          let g(2)=val(line$(227:234)) !:
          let gb(2)=val(line$(219:226))+g(2) ! if sewer not a normal charge theN USe as a standard charge
01970 ! If TRIM$(Z$)="0121045200" Then Pause
01980   if rate$(1:2)="SR" and a(2)=2 and val(line$(227:234))<>17.55 and val(line$(227:234))>0 then a(2)=5: b(2)=val(line$(227:234)) ! if sewer code 2 and not a normal charge the use as a standard charge
01990   if rate$(1:2)="MT" and val(line$(227:234))>0 then a(2)=41: b(2)=val(line$(227:234)) !:
          let g(2)=val(line$(227:234)) !:
          let gb(2)=val(line$(219:226))+g(2) ! motels have a special sewer code
02000   if rate$(1:2)="TR" or rate$(1:2)="TC" then let g(5)=val(line$(227:234)): let gb(5)=val(line$(219:227))+g(5) ! previous balance of trash plus  current charge
02010   if rate$(1:3)="MHT" then let g(5)=val(line$(227:234)): let gb(5)=val(line$(219:227))+g(5): b(5)=val(line$(227:234)) ! previous balance of trash plus  current charge
02020   if rate$(1:3)="MHT" or rate$(1:2)="TR" or rate$(1:2)="TC" and g(5)>0 then let extra(12)=1 ! trash taxable
02030   if rate$(1:2)="CN" then let g(3)=val(line$(227:234)): let gb(3)=val(line$(219:227))+g(3): let extra(12)=1: a(3)=int(g(3)/15) ! previous balance of cannister plus current charge--set tax code to 1
02040   if rate$(1:3)="PKU" then let g(6)=val(line$(227:234)): let gb(6)=val(line$(219:227))+g(6): let extra(11)=int(g(6)/35) : let extra(12)=1 ! previous balance of cannister pickup plus current charge and also assign a code for cannister pickup and tax
02050   if rate$(1:3)="PK2" then let g(6)=val(line$(227:234)): let gb(6)=val(line$(219:227))+g(6): let extra(11)=int(g(6)/8.75) : let extra(12)=2 ! previous balance of cannister pickup plus current charge and also assign a code for cannister pickup and tax
02060   if rate$(1:3)="PKE" then let g(6)=val(line$(227:234)): let gb(6)=val(line$(219:227))+g(6): let extra(11)=int(g(6)/35): let extra(12)=1 ! previous balance of cannister pickup plus current charge and also assign a code 1 since no code # in code-- also set sales tax code
02070   if rate$(1:3)="ASF" then let g(8)=val(line$(227:234)): let gb(8)=val(line$(219:227))+g(8) ! assessment to other
02080   if rate$(1:2)="MR" then let g(9)=g(9)+val(line2$(1:9)): a(6)=1 : let gb(9)+=val(line2$(1:9)) ! residential tax on gas
02090   if rate$(1:2)="MC" then let g(9)=g(9)+val(line2$(1:9)): a(6)=2 : let gb(9)+=val(line2$(1:9)) ! commercial tax on gas
02100   if rate$(1:2)="CN" or rate$(1:3)="PKU" or rate$(1:3)="PK2" or rate$(1:3)="APT" or rate$(1:3)="MHT" or rate$(1:2)="TR" or rate$(1:2)="TC" then let g(7)=g(7)+val(line2$(1:9)): let extra(12)=1 : let gb(7)+=val(line2$(1:9)) ! tax on trash AND CANISTER
02110   if rate$(1:3)="TCE" or rate$(1:3)="CNE" then let extra(12)=9 ! tax exempt commercial trash
02120   if val(line$(227:234))=0 and val(line$(219:227))<>0 and trim$(line$(139:143))="WATER" then let gb(1)=val(line$(219:227))
02130   if val(line$(227:234))=0 and val(line$(219:227))<>0 and trim$(line$(139:143))="SEWER" then let gb(2)=val(line$(219:227))
02140   if val(line$(227:234))=0 and val(line$(219:227))<>0 and trim$(line$(139:143))="GAS" then let gb(4)=val(line$(219:227))
02150   if val(line$(227:234))=0 and val(line$(219:227))<>0 and trim$(line$(139:143))="TRASH" then let gb(5)=val(line$(227:234))
02160   if val(line$(227:234))=0 and val(line$(219:227))<>0 and trim$(line$(139:146))="ASSESMEN" then let gb(8)=val(line$(219:227)) ! PLUG ASSESMENT TO OTHER
02170   let total_balance+=val(line2$(17:25)) ! get net bill
02180 L2180: return 
