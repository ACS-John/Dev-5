00010 ! Replace S:\acsSU\Support_Conversion
00020 ! Converison of ACS client information concerning support
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,contact$(4)*50,systemid$*2,timeframe$*2
00080   dim z$*5,a$(5)*30,ph$*12,ss$*11,dd(10),sc(10),ca(10),ar(5),arta(2),cm$*70
00090   dim app(20),ma(20),ap2(20),ma2(20),st$(40)*24,cd$(40)*2
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$='Support Conversion')
00120   fncno(cno)
00130   gosub READ_NAMES
00140   gosub OPEN_FILE
00150   gosub READ_FILES
00160   goto XIT
00170 ! ______________________________________________________________________
00180 OPEN_FILE: ! 
00190 ! Open #SUPPORT=2: "Name="&env$('Q')&"\TMmstr\Support.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\TMmstr\Support-Idx.h"&env$('cno')&",replace,RecL=246,KPs=1,KLn=10,Shr",Internal,Outin,Keyed
00200   open #1: "Name=c:\acs\vol002\acs.inc\TMmstr\CLmstr.h1,KFName="&env$('Q')&"\TMmstr\CLIndex.h1",internal,outin,keyed 
00210   open #client=3: "Name="&env$('Q')&"\TMmstr\Client.h1,Version=1,KFName="&env$('Q')&"\TMmstr\Client-Idx.h1,replace,RecL=406,KPs=1,KLn=6",internal,outin,keyed 
00220   return 
00230 READ_FILES: ! 
00240   read #1,using L250: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2 eof XIT
00250 L250: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
00260   for j=1 to 40
00270     if j>20 then goto SECOND_TWENTY
00280     if app(j)=0 then goto L570
00290     if ma(j)=0 then goto L570 ! if no maintenance skip
00300     supportid$=z$&cnvrt$("PIC(#####)",j)
00310     clientid=val(z$) ! CLIENT ID TO CUSTOMER NUMBER
00320     systemid$=cd$(j) ! SET SYSTEM ID TO "ub" ETC
00330     stardingdate=0
00340     if ma(j)>1.00 then timeframe$="Mo"
00350     if ma(j)=-.01 then timeframe$="An"
00360     if ma(j)=0 then timeframe$="Na"
00370     endingdate=0
00380     cost=ma(j)
00390     contact$(1)=a$(1) ! name
00400     contact$(2)=a$(4) ! contact
00410     gosub WRITE_RECORDS ! WRITE THE RECORD AND SET SYSTEM ID
00420     goto L570
00430 SECOND_TWENTY: ! 
00440     if ap2(j-20)=0 then goto L570
00450     if ma(j-20)=0 then goto L570 ! if no maintenance skip
00460     supportid$=z$&cnvrt$("PIC(#####)",j)
00470     systemid$=cd$(j) ! SET SYSTEM ID TO "ub" ETC
00480     stardingdate=0
00490     if ma2(j-20)>1.00 then timeframe$="Mo"
00500     if ma2(j-20)=-.01 then timeframe$="An"
00510     if ma2(j-20)=0 then timeframe$="NA"
00520     endingdate=0
00530     cost=ma(j-20)
00540     contact$(1)=a$(1) ! name
00550     contact$(2)=a$(4) ! contact
00560     gosub WRITE_RECORDS ! WRITE THE RECORD AND SET SYSTEM ID
00570 L570: next j
00580   gosub WRITE_X
00590   goto READ_FILES
00600 WRITE_RECORDS: ! 
00610 ! Write #SUPPORT,Using "form pos 1,c 10,n 6,c 2,n 8,c 2,n 8,n 10.2,4*c 50": SUPPORTID$,CLIENTID,SYSTEMID$,STARTINGDATE,TIMEFRAME$,ENDINGDATE,COST,MAT CONTACT$
00620   return 
00630 WRITE_X: ! 
00640   write #client,using "form pos 1,n 6,8*c 50": val(z$),a$(1),a$(2),"",a$(3),e_mail$,a$(4),ph$,""
00641   pr z$
00650   return 
00660 XIT: fnxit
00670 READ_NAMES: ! 
00680   data " 1. GENERAL LEDGER"
00690   data " 2. ACCOUNTS RECEIVABLE"
00700   data " 3. ACCOUNTS PAYABLE"
00710   data " 4. Utility Billing"
00720   data " 5. PATIENT BILLING"
00730   data " 6. PROPERTY TAX"
00740   data " 7. HOSPITAL A/R"
00750   data " 8. FIXED ASSET"
00760   data " 9. TIME MANAGEMENT"
00770   data "10. CASH REGISTER"
00780   data "11. POINT OF SALE"
00790   data "12. INVOICING"
00800   data "13. INVENTORY"
00810   data "14. PAYROLL"
00820   data "15. PURCHASE ORDER"
00830   data "16. MUNICIPAL COURT"
00840   data "17. PCAnywhere"
00850   data "18. Checkbook"
00860   data "19. HARDWARE"
00870   data "20. OTHER"
00880   data "21. JOB COST"
00890   data "22. BUSINESS LICENSE"
00900   data "23. BUDGET MANAGEMENT"
00910   data "24. GAS AND DIESEL"
00920   data "25. ENERGY ASSISTANCE"
00930   data "26."
00940   data "27."
00950   data "28."
00960   data "29."
00970   data "30."
00980   data "31."
00990   data "32."
01000   data "33."
01010   data "34."
01020   data "35."
01030   data "36."
01040   data "37."
01050   data "38."
01060   data "39."
01070   data "40."
01080   read mat st$
01090   data "GL"
01100   data "AR"
01110   data "AP"
01120   data "UB"
01130   data "PB"
01140   data "PT"
01150   data "HA"
01160   data "FA"
01170   data "TM"
01180   data "CR"
01190   data "PS"
01200   data "IV"
01210   data "IN"
01220   data "PR"
01230   data "PO"
01240   data "MC"
01250   data "PC"
01260   data "CL"
01270   data "HW"
01280   data "OT"
01290   data "JC"
01300   data "BL"
01310   data "BM"
01320   data "GD"
01330   data "SR"
01340   data "EA"
01350   data "27"
01360   data "28"
01370   data "29"
01380   data "30"
01390   data "31"
01400   data "32"
01410   data "33"
01420   data "34"
01430   data "35"
01440   data "36"
01450   data "37"
01460   data "38"
01470   data "39"
01480   data "40"
01490   read mat cd$
01500   return 
01510 ! <Updateable Region: ERTN>
01520 ERTN: fnerror(program$,err,line,act$,"xit")
01530   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01540   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01550   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01560 ERTN_EXEC_ACT: execute act$ : goto ERTN
01570 ! /region
01580   cost=ma2(j)
