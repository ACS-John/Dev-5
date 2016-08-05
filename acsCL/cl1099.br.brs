00010 ! Replace R:\acsCL\cl1099
00020 ! print 1099 forms
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnopenprn,fncloseprn,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fntxt,fncmdset,fnacs,fndat,fncombof,fnfra,fnopt,fnpa_finis,fnpa_newpage
00050   let fntop(program$,cap$="Print 1099 Forms")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim vn$*8,nam$*30,ss$*11,a$(3)*40,box(11),ad1$*30,ad2$*30,csz$*30
00090   dim resp$(10)*60,txt$*80
00100   dim cnam$*40,cap$*128,ph$*12,fed$*12,io1$(4),de$*30,key$*19,tr$(5)*35
00110 ! ______________________________________________________________________
00120   if exists("R:\acsGL\Demo.wb")<>0 then !:
          print "This Program is disabled in the demo version" !:
          print "Press ENTER to continue" !:
          input fields "1,1,C 1,N": pause$ !:
          goto XIT
00140   let fncno(cno,cnam$)
00150   open #20: "Name=Q:\CLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative: read #20,using ' Form POS 1,3*C 40,C 12': mat a$,fed$ !:
        close #20: 
00160 ! ______________________________________________________________________
00170 SCR1: print newpage
00180 MAIN: ! 
00190   let fntos(sn$="Vendor1099") !:
        let mylen=40 : let mypos=mylen+3
00200   let fnlbl(1,1,"Federal Identification Number:",mylen,right=1)
00210   let fntxt(1,mypos,20) !:
        let resp$(1)=fed$
00220   let fnlbl(3,1,"Transaction Starting Date:",mylen,right)
00230   let fntxt(3,mypos,8,0,right,'CCYYMMDD',0,'(Blank for All)  Normally you would enter the first day of the year, but you can analyze any time frame.') !:
        let resp$(2)=str$(transactionstartingdate)
00240   let fnlbl(4,1,"Transaction Ending Date:",mylen,right)
00250   let fntxt(4,mypos,8,0,right,'CCYYMMDD',0,'(Blank for All)  Normally you would enter the last day of the year, but you can analyze any time frame.') !:
        let resp$(3)=str$(transactionendingdate)
00260   let fnlbl(6,1,"Type to Print (blank for all):",mylen,right=1)
00270   let fncombof("Payeetype",6,mypos,27,"R:\acsCL\PayeeType.dat",1,2,3,25,"",0,0, "The payee type is a code used to detemine which box should be used on a 1099 misc form.  Enter the code for the payee type to print.") !:
        let resp$(4)=str$(seltp)
00280   let fnlbl(7,1,"Phone Number:",mylen,right=1)
00290   let fntxt(7,mypos,12,0,left,"",0,"Enter the phone number where you can be reached concerning these 1099 forms.") !:
        let resp$(5)=ph$
00300   let fnfra(9,20,2,30,"Method of Printing:","Print to pre-printed 1099 form or export to another 1099 program.")
00310   let fnopt(1,3,"Laser",0,1) !:
        let resp$(6)="True"
00320   let fnopt(2,3,"Export",0,1) !:
        let resp$(7)="False"
00330   let fnlbl(14,1,"Minimum Amount to Print:",mylen,right=1)
00340   let fntxt(14,mypos,10,0,0,"10",0,"Enter the minimum amount required for a 1099 to print.") !:
        let resp$(8)="600.00"
00350   let fnlbl(16,1,"Top Margin:",mylen,right=1)
00360   let fntxt(16,mypos,3,0,0,"30",0,"Decrease Top Margin to move print up on top 1099. Increase to lower print."): let resp$(9)="10"
00370   let fnlbl(17,1,"Top Margin on Bottom 1099",mylen,right=1)
00380   let fntxt(17,mypos,3,0,0,"30",0,"Decrease Bottom Margin to move print up on bottom 1099. Increase to lower print."): let resp$(10)="150"
00390   let fncmdset(2)
00400   let fnacs(sn$,0,mat resp$,ckey)
00410 ! 
00420   if ckey=5 then goto XIT
00430   let fed$=resp$(1)
00440   let transactionstartingdate=val(resp$(2))
00450   let transactionendingdate=val(resp$(3))
00460   let selty=val(resp$(4)(1:2))
00470   let ph$=resp$(5)
00480   if resp$(7)="True" then let lz1$="E" else let lz1$="L"
00490   let minamt=val(resp$(8))
00500   let top=val(resp$(9)) ! top margin on top 1099
00510   let bottom=val(resp$(10)) ! top margin on bottom 1099
00520   if lz1$="L" then gosub VBOPENPRINT
00530   open #payee=1: "Name=Q:\CLmstr\PayMstr.h"&str$(cno)&",KFName=Q:\CLmstr\PayIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00540   open #trmstr2=2: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00550   let margin=top ! set top margin to answer on previous screen
00560   if lz1$="E" then open #5: "Name=\1099ETC.W01\1099DAT.PRN,Replace",display,output 
00570 L570: read #payee,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11',release: vn$,nam$,ad1$,ad2$,csz$,typ,ss$ eof DONE
00580   gosub READ_TRANSACTIONS
00590   let ytdp=0: gosub READ_TRANSACTIONS
00600   form pos 1,c 8,c 35,3*c 20,x 5,n 2,c 11
00610   if typ=0 then goto L570
00620   if ytdp<minamt then goto L570
00630   if seltp=0 or seltp=typ then goto L650 else goto L570
00640 ! ______________________________________________________________________
00650 L650: mat box=(0)
00660   if typ<1 or typ>8 then let typ=1
00670   let box(typ)=ytdp
00680   if lz1$="E" then gosub PRINT_E else gosub PRINT_NOT_E
00690   goto L570
00700 ! ______________________________________________________________________
00710 DONE: close #1: ioerr L720
00720 L720: if lz1$="E" then close #5: else !:
          gosub RELEASE_PRINT
00730 XIT: let fnxit
00740 ! ______________________________________________________________________
00750 PRINT_NOT_E: ! 
00755   print #20: 'Call Print.MyFontSize(12)'
00760   let lyne=margin
00770   let txt$=a$(1) !:
        let lyne=lyne+3.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
00780   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(1)) ! Rents
00790   print #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
00800   let txt$=a$(2) !:
        let lyne=lyne+8.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
00810   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(2)) ! royalties
00820   let lyne+=4 !:
        print #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
00830   let txt$=a$(3) !:
        let lyne=lyne+4.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
00840   let txt$=ph$ !:
        let lyne=lyne+8.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
00850   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(3)) ! Other income
00860   print #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
00870   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(4)) ! fed income
00880   print #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
00890   let txt$=fed$ !:
        let lyne=lyne+12.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
00900   let txt$=ss$ !:
        print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+45)&','&str$(lyne)&')'
00910   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(5)) ! fishing
00920   print #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
00930   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(6)) ! medical
00940   print #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
00950   form pos 24,c 12,pos 40,nz 11.2,nz 13.2,skip 3,pos 7,2*c 15,pos 40,nz 11.2,nz 13.2,skip 4
00960   let txt$=nam$(1:30) !:
        let lyne=lyne+18: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
00970   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(7)) ! Non-employee
00980   print #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
00990   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(8)) ! Substitue payment
01000   print #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
01010   if trim$(ad1$)="" then let ad1$=ad2$: let ad2$=""
01020   if trim$(ad2$)="" then let ad2$=csz$: let csz$=""
01030 ! print #255,Using 860: AD1$
01040   let txt$=ad1$ !:
        let lyne=lyne+8.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
01050   let txt$=cnvrt$("pic(zz,zzz,zzz.##",box(10)) ! Crop Insurance
01060   let lyne+=4 !:
        print #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
01070   let txt$=ad2$ !:
        let lyne=lyne+8.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
01080   let txt$=csz$ !:
        let lyne=lyne+8.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
01090   let txt$=trim$(vn$) !:
        let lyne=lyne+15.5: print #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
01100   let x=x+1
01110   if x=1 then let margin=bottom: goto L1130
01120   if x=2 then let fnpa_newpage : let margin=top: let x=0: goto L1130
01130 L1130: return 
01140 ! ______________________________________________________________________
01150 PRINT_E: ! 
01160   print #5: "01 ";" "
01170   print #5: "02 ";ph$
01180   print #5: "03 ";a$(1)
01190   print #5: "04 ";box(1)
01200   print #5: "05 ";" "
01210   print #5: "06 ";a$(2)
01220   print #5: "07 ";box(2)
01230   print #5: "08 ";a$(3)
01240   print #5: "09 ";box(3)
01250   print #5: "10 ";box(4)
01260   print #5: "11 ";b$(1)
01270   print #5: "12 ";ss$
01280   print #5: "13 ";box(5)
01290   print #5: "14 ";box(6)
01300   print #5: "15 ";box(7)
01310   print #5: "16 ";box(8)
01320   print #5: "17 ";nam$
01330   print #5: "18 ";" "
01340   print #5: "19 ";" "
01350   print #5: "20 ";box(10)
01360   print #5: "21 ";ad1$
01370   print #5: "22 ";csz$
01380   print #5: "23 ";" "
01390   print #5: "24 ";0
01400   print #5: "25 ";vn$
01410   print #5: "26 ";" "
01420   print #5: "27 ";0
01430   print #5: "28 ";" "
01440   print #5: "29 ";0
01450   print #5: "30 ";" " ! 0
01460 ! Print #5: "31 ";" "
01470 ! Print #5: "32 ";0
01480   print #5: "*"
01490   return 
01500 ! ______________________________________________________________________
01510 ! <Updateable Region: ERTN>
01520 ERTN: let fnerror(cap$,err,line,act$,"xit")
01530   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01540   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01550   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01560 ERTN_EXEC_ACT: execute act$ : goto ERTN
01570 ! /region
01580 ! ______________________________________________________________________
01590 READ_TRANSACTIONS: !:
        let wbc=0: let wtt=1 ! all banks and only checks
01600   let key$=vn$&cnvrt$('pic(Z#)',wbc)&cnvrt$("pic(#)",wtt)&rpt$(chr$(0),8) !:
        restore #trmstr2,key>=key$: nokey L1680 !:
        let ytdp=0
01610 READ_TRANS: ! 
01620   read #trmstr2,using 'Form Pos 1,n 2,n 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof L1680
01630   if trim$(vn$)<>trim$(tr$(4)) then goto L1680
01640   if transactionstartingdate<>0 and transactionstartingdate>fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRANS
01650   if transactionendingdate<>0 and transactionendingdate<fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRANS
01660   let ytdp+=tr3
01670   goto READ_TRANS
01680 L1680: return 
01690 VBOPENPRINT: ! 
01700   if file(20)=-1 then 
01710     open #20: "Name=Q:\CLmstr\1099"&wsid$&".txt,Replace,RecL=5000",display,output 
01720     print #20: 'Call Print.MyOrientation("Portrait")'
01730     let lyne=margin ! starting of 1st line
01740     let column1=15 !:
          let column2=95 !:
          let column3=130
01750   end if 
01760   return 
01770 RELEASE_PRINT: ! 
01780 ! 
01790   let fnpa_finis
01810   return 
