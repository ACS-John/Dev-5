00010 ! Replace S:\acsUB\Conversion\Txt2UBmstr_test
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnerror,fnxit,fncno
00040   dim s_file$*40,cap$*128,line$*234,cnam$*40,line2$*41,tg(11),x(15)
00050   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
00060   dim rw4(22,13),gb(10),extra(23),extra$(11)*30,df$*1,dr$*9,dc$*2,dc$*17
00070   dim udf$*256,work$*256,work_addr$*256
00080 ! ______________________________________________________________________
00090   let cap$="Convert from text file to Customer file"
00100   let fncno(cno,cnam$)
00110   let udf$=env$('temp')&'\'
00120 ! ______________________________________________________________________
00130 MENU1: ! 
00140   print newpage
00150   print fields "10,20,Cc 40,R,N": cap$
00160   print fields "12,2,Cr 27,N": "Destination Company Number:"
00170   print fields "13,2,Cr 27,N": "Source File Path and Name:"
00180   let io1$(1)="12,30,Nz 3,UT,N" !:
        let io1$(2)="13,30,C 40,UT,N"
00190   print fields "15,21,Cc 19,B,1": "Start (F1)"
00200   print fields "15,41,Cc 19,B,99": "Exit (Esc)"
00210   print fields "18,01,Cc 80,R,N": "WARNING:  All Files in the Destination Company Number will"
00220   print fields "19,01,Cc 80,R,N": "be duplicates of Company Number 1 with new merged records!"
00230   print fields "21,01,Cc 80,R,N": "No other users may be using Utility Billing for this to run"
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
00362   open #21: "Name="&env$('Q')&"\UBmstr\test.h"&str$(cno)&",RecL=132,replace",internal,output 
00370   open #15: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&str$(cno)&",Shr,Replace,RecL=102,KPs=1,KLn=19",internal,outin,keyed 
00380   let work$=env$('Q')&"\UBmstr\Reads_and_Chgs.h"&str$(cno) !:
        let work_addr$=env$('Q')&"\UBmstr\Reads_and_Chgs-Key.h"&str$(cno) !:
        ! synconrize these settings with S:\acsUB\ubCalk
00390   open #work=4: "Name="&work$&",KFName="&work_addr$&",RecL=74,KPs=1,KLn=10,Replace",internal,outin,keyed 
00400 LOOP_TOP: ! 
00410 L410: linput #1: line$ eof DONE
00420   if line$(1:7)='company' then goto L410
00430   if line$(1:3)="tax" then goto DONE
00440   if trim$(z$)<>"" and trim$(line$(19:26))<>trim$(z$) then goto L830 ! write this record  (there is a seperate record for each serice in the text file)
00450 L450: let z$=lpad$(line$(19:26),10)
00460   let e$(2)=trim$(rtrm$(rtrm$(line$(31:43))&" "&rtrm$(line$(45:71)))(1:30)) ! Name
00470   let e$(1)=e$(3)=rtrm$(rtrm$(line$(72:79))&" "&rtrm$(line$(80:100))) ! street
00480   let e$(4)="CARRIZO SPRINGS, TX 78834"
00490   let extra(1)=val(line$(102:104))
00500   let extra(2)=val(line$(110:114))
00510   let f=22810 ! ALWAYS FIX
00520   let rate$=trim$(line$(147:153))
00530   if rate$(1:3)="PKU" or rate$(1:3)="PKE" then goto L790 ! handle cannister pick up seperately
00550   if rate$(1:3)="SCH" then let a(2)=4: goto L790 ! handle school sewer seperately
00560   if rate$(1:3)="CME" then let a(4)=2: let a(6)=9: goto L790 ! handle school sewer seperately as tax exempt commercial
00570   if rate$(1:3)="MHT" then let a(5)=4: let extra(12)=1: goto L790 ! handle mobile home trash seperately as taxable
00580   if rate$(1:3)="ASF" then goto L790 ! handle assessmemt seperately
00590   if line$(149:149)<"0" or line$(149:149)>"9" then goto L790
00600   if rate$(1:3)="WR1" then let a(1)=1 ! residential in
00610   if rate$(1:3)="WR2" then let a(1)=2 ! residential out
00620   if rate$(1:3)="WR5" then let a(1)=5 ! senior citizens
00630   if rate$(1:3)="WC1" then let a(1)=3 ! commercial in
00640   if rate$(1:3)="WC2" then let a(1)=4 ! commercial out
00650   if rate$(1:3)="WR4" then let a(1)=6 ! two house
00660   if rate$(1:2)="MR" then let a(4)=1 ! residential
00670   if rate$(1:2)="MC" then let a(4)=2 ! commercial
00680   if rate$(1:2)="SR" then let a(2)=1 ! sewer residential
00690   if rate$(1:2)="SC" then let a(2)=2 ! commercial sewer
00700   if rate$(1:2)="TR" or rate$(1:2)="TC" then let a(5)=val(rate$(3:3)): let extra(12)=1 ! assume taxable on trash unless changed to not taxable
00705   if rate$(1:3)="TCE" then let a(5)=2: let extra(12)=9: goto L790 ! handle cannister pick up seperately
00710   let extra(4)=013110
00720   let extra(3)=22810
00730   if rate$(1:2)="WR" or rate$(1:2)="WC" then let d(2)=val(line$(171:178)) ! prior water reading
00740   if rate$(1:2)="MR" or rate$(1:2)="MC" then let d(10)=val(line$(171:178)) ! prior gas reading
00750   if rate$(1:2)="WR" or rate$(1:2)="WC" then let d(1)=val(line$(179:177)) ! current water reading
00760   if rate$(1:2)="MR" or rate$(1:2)="MC" then let d(9)=val(line$(179:177)) ! current gas reading
00770   if rate$(1:2)="WR" or rate$(1:2)="WC" then let d(3)=val(line$(187:194)) ! current water usage
00780   if rate$(1:2)="MR" or rate$(1:2)="MC" then let d(11)=val(line$(187:194)) ! current gas usage
00790 L790: let today=22810
00810   gosub LAST_PART_OF_RECORD
00820   goto LOOP_TOP
00830 L830: let gb(9)=gb(9)+g(9): let g(11)=sum(g): let g(12)=g(11)
00840   let bal=sum(gb) ! balance
00850   write #2,using L860: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
00851   write #21,using "form pos 1,c 10,n 10.2": z$,total_balance
00852   let total_balance=0
00860 L860: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00870   let tdate=20100228: let tcode=1: let tamount=g(12)
00880   for j=1 to 10
00890     let tg(j)=g(j)
00900   next j
00910   write #15,using L1000: z$,tdate,tcode,tamount,mat tg,d(1),d(3),0,0,d(9),d(11),bal,pcode
00920   if bal>g(12) then goto L930 else goto L1010
00930 L930: for j=1 to 10
00940     let tg(j)=gb(j)-g(j)
00950   next j
00960   let tdate=20100228
00970   let tamount=bal-g(11)
00980   write #15,using L1000: z$,tdate,tcode,tamount,mat tg,0,0,0,0,0,0,bal-g(11),pcode
00990 L990: form pos 1,c 10,4*pd 5,7*pd 4.2,3*pd 5,n 1
01000 L1000: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
01010 L1010: let x(1)=d(1) : let x(2)=d(9)
01020   write #work,using L990,rec=rctr: z$,mat x
01030   mat a=(0): mat d =(0): let extra(11)=extra(12)=0: mat g=(0): mat gb=(0)
01040   goto L450 ! process last record written
01050 ! ______________________________________________________________________
01060 ! ______________________________________________________________________
01070 ! ______________________________________________________________________
01080 DONE: ! 
01090   print newpage
01100   print fields "12,20,Cc 40,N": "Please wait: reIndexing Customer file..."
01110   close #1: ioerr L1120
01120 L1120: close #2: ioerr L1130
01130 L1130: execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&" 1 10 Replace DupKeys -n"
01131   close #21: 
01132   execute "Index "&env$('Q')&"\UBmstr\test.h"&str$(cno)&' '&env$('Q')&"\UBmstr\testindx.h"&str$(cno)&" 1 10 Replace DupKeys -n"
01140   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx2.h"&str$(cno)&" 354 7 Replace DupKeys -n"
01150   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx3.h"&str$(cno)&" 11 25 Replace DupKeys -n"
01160   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx4.h"&str$(cno)&" 361 12 Replace DupKeys -n"
01170   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx5.h"&str$(cno)&" 1741/1743 2/7 Replace DupKeys -n"
01180   close #15: 
01190   execute "Index "&env$('Q')&"\UBmstr\UBTransvb.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBTrindx.h"&str$(cno)&" 1 19 Replace DupKeys -n"
01200   print newpage
01210   print fields "11,20,Cc 40,N": "Completed Sucessfully."
01220   print fields "12,20,Cc 40,B,X0D": "Press any key to continue..."
01230   goto XIT
01240 ! ______________________________________________________________________
01250 METER_ADDRESS: ! 
01260   let e$(1)=rtrm$(line$(93:117))(1:30)
01270   let x=pos(e$(1)," ",1)
01280   let x2=val(e$(1)(1:x)) conv L1300
01290   let e$(1)=rtrm$(e$(1)(x+1:30))&" "&e$(1)(1:x-1)
01300 L1300: return 
01310 ! ______________________________________________________________________
01320 ! <Updateable Region: ERTN>
01330 ERTN: let fnerror(program$,err,line,act$,"xit")
01340   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01360   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01370 ERTN_EXEC_ACT: execute act$ : goto ERTN
01380 ! /region
01390 LAST_PART_OF_RECORD: ! 
01400 L1400: linput #11: line2$ eof L1840
01410   if trim$(line2$(1:20))="" then goto L1400
01420   if line2$(1:3)="tax" then goto L1400
01430   if rate$(1:2)="WR" or rate$(1:2)="WC" then let g(1)=val(line$(227:234)): let gb(1)=val(line$(220:227))+g(1) ! previous balance of water plus current charge
01431   if rate$(1:3)="WR5" then let g(1)=round(val(line$(227:234))*.75,2): let gb(1)=val(line$(220:227))+g(1) ! previous balance of water plus current charge
01440   if rate$(1:2)="MR" or rate$(1:2)="MC" then let g(4)=val(line$(227:234)): let gb(4)=val(line$(220:227))+g(4) ! previous balance of water plus current charge
01450   if rate$(1:2)="SR" or rate$(1:2)="SC" then let g(2)=val(line$(227:234)): let gb(2)=val(line$(220:227))+g(2) ! previous balance of sewer plus current charge
01460   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 11.25 then let a(2)=2 ! residentisl 5 fixtures
01470   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 11.85 then let a(2)=3 ! residentisl 6 fixtures
01480   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 12.38 then let a(2)=4 ! residentisl 7 fixtures
01490   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 17.55 then let a(2)=10 ! small business
01500   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 35.10 then let a(2)=11 ! large business
01510   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 26.33 then let a(2)=12 ! restaurants
01520   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 26.33 then let a(2)=13 ! garages   (can't tell difference in restaurants
01530   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 52.50 then let a(2)=14 ! courthouse
01540   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 17.55 then let a(2)=15 ! churches   (cant tell difference in small business
01550   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 192.98 then let a(2)=16 ! dcm hospital
01560   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 224.70 then let a(2)=17 ! nursing home
01570   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 87.75 then let a(2)=18 ! junior high
01580   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 131.63 then let a(2)=19 ! senior high high
01590   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 12.30 then let a(2)=20 ! mobile home
01600   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 14.03 then let a(2)=40 ! motel
01610   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 239.03 then let a(2)=80: let b(2)=val(line$(227:234)) ! housing aut
01620   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 189.53 then let a(2)=80: let b(2)=val(line$(227:234)) ! housing aut
01630   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 18.53 then let a(2)=41: let b(2)=val(line$(227:234)) ! motels
01640   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 23.03 then let a(2)=41: let b(2)=val(line$(227:234)) ! motels
01650   if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 27.53 then let a(2)=41: let b(2)=val(line$(227:234)) ! motels
01660   for j=1 to 60
01670     if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 27.53+(4.50*j) then let a(2)=41: let b(2)=27.53+(4.50*j) ! motels
01680   next j
01690   for j=1 to 60
01700     if (rate$(1:2)="SR" or rate$(1:2)="SC") and val(line$(227:234))= 14.03+(j*4.50) then let a(2)=41: let b(2)=14.03+(j*4.50) ! motels
01710   next j
01720   if rate$(1:2)="SC" and a(2)=1 and val(line$(227:234))<>10.50 and val(line$(227:234))>0 then let a(2)=5: let b(2)=val(line$(227:234)) ! if sewer code 1 and not a normal charge the use as a standard chargerge
01730   if rate$(1:2)="SR" and a(2)=2 and val(line$(227:234))<>17.55 and val(line$(227:234))>0 then let a(2)=5: let b(2)=val(line$(227:234)) ! if sewer code 2 and not a normal charge the use as a standard charge
01740   if rate$(1:2)="MT" and val(line$(227:234))>0 then let a(2)=41: let b(2)=val(line$(227:234)) ! motels have a special sewer code
01750   if rate$(1:2)="TR" or rate$(1:2)="TC" then let g(5)=val(line$(227:234)): let gb(5)=val(line$(220:227))+g(5) ! previous balance of trash plus  current charge
01751   if rate$(1:2)="TR" or rate$(1:2)="TC" and g(5)>0 then let extra(12)=1 ! trash taxable
01760   if rate$(1:2)="CN" then let g(3)=val(line$(227:234)): let gb(3)=val(line$(220:227))+g(3): let extra(12)=1: let a(3)=int(g(3)/15) ! previous balance of cannister plus current charge--set tax code to 1
01770   if rate$(1:3)="PKU" then let g(6)=val(line$(227:234)): let gb(6)=val(line$(220:227))+g(6): let extra(11)=int(g(6)/35) : let extra(12)=1 ! previous balance of cannister pickup plus current charge and also assign a code for cannister pickup and tax
01780   if rate$(1:3)="PKE" then let g(6)=val(line$(227:234)): let gb(6)=val(line$(220:227))+g(6): let extra(11)=int(g(6)/35): let extra(12)=1 ! previous balance of cannister pickup plus current charge and also assign a code 1 since no code # in code-- also set sales tax code
01790   if rate$(1:3)="ASF" then let g(8)=val(line$(227:234)): let gb(8)=val(line$(220:227))+g(8) ! assessment to other
01800   if rate$(1:2)="MR" then let g(9)=g(9)+val(line2$(1:9)): let a(6)=1 ! residential tax on gas
01810   if rate$(1:2)="MC" then let g(9)=g(9)+val(line2$(1:9)): let a(6)=2 ! commercial tax on gas
01820   if rate$(1:2)="TR" or rate$(1:2)="TC" then let g(7)=g(7)+val(line2$(1:9)): let extra(12)=1 ! tax on trash
01830   if rate$(1:3)="TCE" or rate$(1:3)="CNE" then let extra(12)=9 ! tax exempt commercial trash
01835   let total_balance+=val(line2$(17:25)) ! get net bill
01840 L1840: return 
