00010 ! Replace S:\acsPR\CheckFile.br
00020 ! Payroll check breakdown information
00030   def library fncheckfile(hact$*8,filnum)
00040     library 'S:\Core\Library': fntop,fntos,fnacs,fncmdkey,fnxit,fnerror,fnfra,fnopt,fnlbl,fntxt,fncmdset,fncmbact,fngethandle,fnopenprn,fncloseprn,fnchk, fnflexinit1,fnflexadd1,fnerror,fncmbemp,fnmsgbox,fncombof,fnbutton,fnDedNames,fnGetPayrollDates
00050     on error goto ERTN
00060     ! ___________________________________________________________________
00070     dim prg$*30,cm$(47)*2,resp$(60)*80
00080     dim hf(46) ,hf$(46),gridname$*30,oldgridname$*30
00090     ! ___________________________________________________________________
00100     fntop(program$,cap$="Payroll Check History")
00120     cancel=5 : back=2 : edit=1 : save=1 : disable=1 
00122     add=4
00124     transtype$(1)="Check Only" 
00126     transtype$(2)="Departmental Breakdowns" 
00128     transtype$(3)="Grand Totals" 
00130     transtype$(4)="Quarterly Totals" 
00132     transtype$(5)="Annual Totals"
00140 ! 
00150 ! ___________________________________________________________________
00160     fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
00210 ! ___________________________________________________________________
00220     open #9: "Name="&env$('Q')&"\PRmstr\GridNames.H"&env$('cno')&",USE,RecL=30",internal,outin,relative 
00230     if lrec(9)=0 then oldgridname$= gridname$="[All]                         ": write #9,using "form pos 1,c 30",rec=1: gridname$
00240     fnDedNames(mat dednames$)
00250     mat hf=(1)
00270     for j=1 to 20
00280       if trim$(dednames$(j))="" then hf(j+25)=0 ! default the (All) to only those deductions that have names
00290       dednames$(j)=trim$(dednames$(j))
00300     next j
00320 L320: if exists(env$('Q')&"\PRmstr\payrollreports.H"&env$('cno')) =0 then gosub SETUP_REPORTS
00322     if exists(env$('Q')&"\PRmstr\reportidx.H"&env$('cno')) =0 then gosub CREATE_INDEX
00330     open #29: "Name="&env$('Q')&"\PRmstr\payrollreports.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\reportidx.H"&env$('cno')&",Shr",internal,outin,keyed 
00340     if kln(29)<>30 then close #29: : execute "Index "&env$('Q')&"\PRmstr\payrollreports.H"&env$('cno')&' '&env$('Q')&"\PRmstr\reportidx.H"&env$('cno')&" 1 30 Replace DupKeys -n" : goto L320
00370     justopen=1: gosub SELECT_COLUMNS: justopen=0
00380 ! ___________________________________________________________________
00390 SCREEN1: ! 
00400     let qtr1printed=qtr2printed=qtr3printed=qtr4printed=0
00410     mat annualtdc=(0): mat annualtcp=(0): mat employeetdc=(0): mat employeetcp=(0)
00420     mat qtr1tcp=(0): mat qtr2tcp= (0): mat qtr3tcp=(0): mat qtr4tcp=(0)
00430     mat tcp=(0): mat tdc=(0) 
00432     mat qtr1tdc=(0): mat qtr2tdc=(0): mat qtr3tdc=(0): mat qtr4tdc=(0)
00440     holdeno=holdckno=holdtdn=holdprd=eno=prd=ckno=tdn=0
00450     mat totaltdc=(0): mat totaltcp=(0) : mat grand2tcp=(0) : : mat grand2tdc=(0)
00460     goto ASKTRANSET ! fnASKTRANSET(CKEY,SEL_CODE,BEG_DATE,END_DATE,Z$,HACT$)
00470 L470: printit=0 : if ckey=2 then printit=1
00480     if ckey=2 and trim$(z$)="" then goto SCREEN1 ! don't allow pr to work if no customer selected
00490 ! If CKEY=2 AND TRIM$(Z$)="[All]" Then Goto SCREEN1
00500     if ckey=2 then let fnopenprn: goto READ_CHECKS ! read headings for reports then start reading thru the checks same as a grid
00510     if ckey=1 then goto SCREEN2 ! READ_CHECKS
00520     if ckey=cancel then goto XIT else goto SCREEN2
00530 ! ___________________________________________________________________
00540 SCREEN2: ! 
00550     fntos(sn$="CheckHistory-2")
00560     goto FLEXGRID: ! 
00570 L570: fncmdkey('&Edit',edit,1,0) !:
          fncmdkey('&Back',back,0,0) !:
          fncmdkey('&Add',add,0,0) !:
          fncmdkey('&Close',cancel,0,1)
00580     fnacs(sn$,0,mat resp$,ckey) ! check history building grid
00590     addcode=0 : holdeno=0
00600     if ckey=back then goto SCREEN1
00610     if ckey=cancel then goto SCREEN1
00615     if ckey=edit and checkonly=1 then goto L616 else goto L620
00616 L616: ! 
00617     mat mg$(3)
00618     mg$(1)="You cannot edit a record when you have selected to display "
00619     mg$(2)="by Departmental Details.  You must select Check only"
00620     mg$(3)="before you can make changes."
00621     fnmsgbox(mat mg$,resp$,cap$,0)
00622     goto SCREEN1
00624 L620: if ckey=edit then editrec=val(resp$(1)) conv SCREEN2 : mat employeetdc=(0): mat employeetcp=(0): goto SCREEN3
00630     if ckey=add then 
00632       if trim$(hact$)<>'[All]' then eno=val(hact$) else eno=0
00634       tdn=prd=ckno=0 : mat tdc=(0) : mat tcp=(0) : addcode=1
00636       goto SCREEN3_ADD
00638     end if
00640     goto SCREEN2
00650     goto XIT
00660 ! ___________________________________________________________________
00670 SCREEN3: ! 
00680     read #filnum,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",rec=editrec: heno,tdn,prd,ckno,mat tdc,mat tcp norec SCREEN2
00690     mat holdtdc=tdc: mat holdtcp=tcp ! hold arrays to see if dollar changes
00700 SCREEN3_ADD: ! 
00710     fntos(sn$="CheckHistory-3")
00712     lc=rc=0 : mylen=20 : mypos=mylen+3
00720     if addcode=1 then disablecode=0 else disablecode=1
00730     fnlbl(lc+=1,1,"Employee #:",mylen,1)
00740     fntxt(lc,mypos,8,0,0,"30",disablecode) !:
          resp$(rc+=1)=str$(heno)
00750     fnlbl(lc+=1,1,"Department:",mylen,1)
00760     fntxt(lc,mypos,3,0,0,"",0) !:
          resp$(rc+=1)=str$(tdn)
00770     fnlbl(lc+=1,1,"Date:",mylen,1)
00780     fntxt(lc,mypos,10,0,0,"3") !:
          resp$(rc+=1)=str$(prd)
00790     fnlbl(lc+=1,1,"Check #:",mylen,1)
00800     fntxt(lc,mypos,10,0,0,"30") !:
          resp$(rc+=1)=str$(ckno)
00810     fnlbl(lc+=1,1,"Regular Hours:",mylen,1)
00820     fntxt(lc,mypos,10,0,0,"32") !:
          resp$(rc+=1)=str$(tdc(1))
00830     fnlbl(lc+=1,1,"Overtime Hours:",mylen,1)
00840     fntxt(lc,mypos,10,0,0,"32") !:
          resp$(rc+=1)=str$(tdc(2))
00850     fnlbl(lc+=1,1,"Sick Hours:",mylen,1)
00860     fntxt(lc,mypos,10,0,0,"32") !:
          resp$(rc+=1)=str$(tdc(3))
00870     fnlbl(lc+=1,1,"Vacation Hours:",mylen,1)
00880     fntxt(lc,mypos,10,0,0,"32") !:
          resp$(rc+=1)=str$(tdc(4))
00890     fnlbl(lc+=1,1,"Holiday Hours:",mylen,1)
00900     fntxt(lc,mypos,10,0,0,"32") !:
          resp$(rc+=1)=str$(tdc(5))
00910     fnlbl(lc+=1,1,"Regular Earnings:",mylen,1)
00920     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(26))
00930     fnlbl(lc+=1,1,"Overtime Earnings:",mylen,1)
00940     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(27))
00950     fnlbl(lc+=1,1,"Other Compensation:",mylen,1)
00960     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(28))
00970     fnlbl(lc+=1,1,"Meals:",mylen,1)
00980     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(29))
00990     fnlbl(lc+=1,1,"Tips:",mylen,1)
01000     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(30))
01010     fnlbl(lc+=1,1,"Total Wage:",mylen,1)
01020     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(31))
01030     fnlbl(lc+=1,1,"Net Pay:",mylen,1)
01040     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(32))
01050     fnlbl(lc+=2,1,"Workmans Comp Wages:",mylen,1)
01060     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tdc(6))
01070     fnlbl(lc+=1,1,"SS Wages:",mylen,1)
01080     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tdc(7))
01090     fnlbl(lc+=1,1,"Medicare Wages:",mylen,1)
01100     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tdc(8))
01110     fnlbl(lc+=1,1,"Federal U/C Wages:",mylen,1)
01120     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tdc(9))
01130     fnlbl(lc+=1,1,"State U/C Wages:",mylen,1)
01140     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tdc(10))
01150     mypos+=37: fnlbl(lc=1,38,"Federal Wh:",mylen,1)
01160     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(1))
01170     fnlbl(lc+=1,38,"SS Withholdings:",mylen,1)
01180     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(2))
01190     fnlbl(lc+=1,38,"Medicare Wh:",mylen,1)
01200     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(3))
01210     fnlbl(lc+=1,38,"State Wh:",mylen,1)
01220     fntxt(lc,mypos,10,0,0,"10") !:
          resp$(rc+=1)=str$(tcp(4))
01230     for j=1 to 20
01240       if trim$(dednames$(j))="" then goto L1250 else goto L1260
01250 L1250: fnlbl(lc+=1,38,dednames$(j)&" ",mylen,1) : goto L1270
01260 L1260: fnlbl(lc+=1,38,dednames$(j)&":",mylen,1)
01270 L1270: fntxt(lc,mypos,10,0,0,"10") !:
            resp$(rc+=1)=str$(tcp(j+4))
01280     next j
01290     fncmdkey('&Save',save,1,0) !:
          fncmdkey('&Delete',4,0,0) !:
          fncmdkey('&Cancel',cancel,0,1)
01300     fnacs(sn$,0,mat resp$,ckey) ! correcting check
01310     if ckey=cancel then goto SCREEN2
01320     if ckey=4 then goto L1330 else goto L1360
01330 L1330: mat mg$(3) !:
          mg$(1)="You are deleting a check.  This will change the " !:
          mg$(2)="earnings. It will change the quarterly and annual reports." !:
          mg$(3)="Click OK to delete; else Cancel to retain the record." !:
          fnmsgbox(mat mg$,resp$,cap$,49)
01340     if resp$="OK" then delete #filnum,rec=editrec: : eno=holdeno
01350     goto SCREEN2
01360 L1360: heno=val(resp$(1)) ! employee #
01370     tdn=val(resp$(2)) ! dept #
01380     prd=val(resp$(3)) ! date
01390     ckno=val(resp$(4)) ! check #
01400     tdc(1)=val(resp$(5)) ! reg hrs
01410     tdc(2)=val(resp$(6)) ! ot hrs
01420     tdc(3)=val(resp$(7)) ! sick hrs
01430     tdc(4)=val(resp$(8)) ! vac hrs
01440     tdc(5)=val(resp$(9)) ! hol hrs
01450     tcp(26)=val(resp$(10)) ! reg pay
01460     tcp(27)=val(resp$(11)) ! ot pay
01470     tcp(28)=val(resp$(12)) ! other comp
01480     tcp(29)=val(resp$(13)) ! Meals
01490     tcp(30)=val(resp$(14)) ! Tips
01500     tcp(31)=val(resp$(15)) ! Total Pay
01510     tcp(32)=val(resp$(16)) ! Net pay
01520     tdc(6)=val(resp$(17)) ! Workman comp wages
01530     tdc(7)=val(resp$(18)) ! ss wages
01540     tdc(8)=val(resp$(19)) ! Medicare wages
01550     tdc(9)=val(resp$(20)) ! Federal uc
01560     tdc(10)=val(resp$(21)) ! state uc
01570     tcp(1)=val(resp$(22)) ! fed wh
01580     tcp(2)=val(resp$(23)) ! ss  wh
01590     tcp(3)=val(resp$(24)) ! med wh
01600     tcp(4)=val(resp$(25)) ! state wh
01610     for j=1 to 20
01620       tcp(j+4)=val(resp$(j+25)) ! std deductions
01630     next j
01640     if addcode=1 then write #filnum,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp : addcode=0 : goto SCREEN2
01650     if sum(holdtdc)<>sum(tdc) or sum(holdtcp)<>sum(tcp) then goto L1660 else goto L1680
01660 L1660: mat mg$(3) !:
          mg$(1)="You have changed dollar amounts on a real check! " !:
          mg$(2)="This will change the quarterly and annual reports.." !:
          mg$(3)="Click OK to continue; else Cancel to exit without saving the changes." !:
          fnmsgbox(mat mg$,resp$,cap$,49)
01670     if resp$="OK" then goto L1680 else goto L1700
01680 L1680: if ckey=save then rewrite #filnum,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",rec=editrec: heno,tdn,prd,ckno,mat tdc,mat tcp: eno=heno
01690 ! Close #filnum:
01700 L1700: goto SCREEN2
01710 ! ___________________________________________________________________
01720 ! <Updateable Region: ERTN>
01730 ERTN: fnerror(program$,err,line,act$,"xit")
01740     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01750     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01760     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01770 ERTN_EXEC_ACT: execute act$ : goto ERTN
01780 ! /region
01790 ! ___________________________________________________________________
01800 ASKTRANSET: ! 
01810     fntos(sn$="CHECKhISTORY") !:
          rc=cf=0
01820     fnfra(1,1,3,26,"Informatin to be Shown","You can choose to have the checks listed as one total or have the department breakdowns shown.  You cannot select both!",0) !:
          cf+=1 : let fratype=cf
01830     fnopt(1,3,"Departmental Details",0,fratype)
01832     if checkonly=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
01840     fnopt(2,3,"Check only",0,fratype)
01842     if details=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
01845     if details=0 and checkonly=0 then resp$(rc)="True"
01850     fnfra(6,1,6,26,"Print options","You can get totals by any combination of the following options.",0) !:
          cf+=1 : let fratype=cf
01860     fnchk(1,3,"Grand Totals",0,fratype) !:
          if grand=1 then resp$(rc+=1)="True" else !:
            resp$(rc+=1)="False"
01870     fnchk(2,3,"Quarterly Totals",0,fratype) !:
          if quarterly=1 then resp$(rc+=1)="True" else !:
            resp$(rc+=1)="False"
01880     fnchk(3,3,"Annual Totals",0,fratype) !:
          if annual=1 then resp$(rc+=1)="True" else !:
            resp$(rc+=1)="False"
01890     fnchk(4,3,"Employee Totals",0,fratype) !:
          if employee=1 then resp$(rc+=1)="True" else !:
            resp$(rc+=1)="False"
01900     fnfra(1,30,6,42,"Date Range","You can transactions for any date range or leave these blank to see all transactions.") !:
          cf+=1 : let fradate=cf : mylen=26 : mypos=mylen+2
01910     fnlbl(1,1,"Starting Date:",mylen,1,0,fradate)
01920     fntxt(1,mypos,10,0,1,"3",0,empty$,fradate) !:
          resp$(rc+=1)=str$(beg_date)
01930     fnlbl(2,1,"Ending Date:",mylen,1,0,fradate)
01940     fntxt(2,mypos,10,0,1,"3",0,empty$,fradate) !:
          resp$(rc+=1)=str$(end_date)
01950     fnlbl(3,1,"1st Day of 1st quarter:",mylen,1,0,fradate)
01960     fntxt(3,mypos,10,0,1,"3",0,empty$,fradate) !:
          resp$(rc+=1)=str$(qtr1)
01970     fnlbl(4,1,"1st Day of 2nd quarter:",mylen,1,0,fradate)
01980     fntxt(4,mypos,10,0,1,"3",0,empty$,fradate) !:
          resp$(rc+=1)=str$(qtr2)
01990     fnlbl(5,1,"1st Day of 3rd quarter:",mylen,1,0,fradate)
02000     fntxt(5,mypos,10,0,1,"3",0,empty$,fradate) !:
          resp$(rc+=1)=str$(qtr3)
02010     fnlbl(6,1,"1st Day of 4th quarter:",mylen,1,0,fradate)
02020     fntxt(6,mypos,10,0,1,"3",0,empty$,fradate) !:
          resp$(rc+=1)=str$(qtr4)
02030     fnfra(10,30,2,60,"Employee","You can review check information for all employees or for an individual.") !:
          cf+=1 : let fraaccount=cf
02040     fnlbl(1,1,"Employee:",8,1,0,fraaccount)
02050     fncmbemp(1,10,1,fraaccount) !:
          rc+=1 !:
          if trim$(hact$)<>"" then resp$(rc)=hact$ else !:
            if resp$(rc)="" or trim$(resp$(rc))="True" then resp$(rc)="[All]"
02060     fnlbl(15,20,"Column format to use:",40,1)
02070     fncombof("payrollrpt",15,62,30,env$('Q')&"\PRmstr\payrollreports.h"&env$('cno'),1,30,0,0,env$('Q')&"\PRmstr\reportidx.h"&env$('cno'),0,pas, "",frame) !:
          resp$(rc+=1)=gridname$
02080     fncmdkey("&Display Grid",1,1,0,"Displays a list of checks on the scree using the format you have selected.")
02090     fncmdkey("&Print Report",2,0,0,"Prints a check listing using the columns selected.")
02100     fncmdkey("&Maintain column selections",3,0,0,"Allows you to add or change columns that should be displayed.")
02110     fncmdkey("&Back",5,0,1,"Returns to employee record")
02120     fnacs(sn$,0,mat resp$,ckey) ! dates and options
02130     printit=0: f1=0
02140     if ckey=cancel then goto DONE
02150     checkonly=details=grand=quarterly=annual=employee=0 : holdnam$=""
02160     eno=holdeno=printeno=holdckno=printckno=0 : mat cp1=(0)
02170     if resp$(1)="True" then checkonly=1
02180     if resp$(2)="True" then details=1
02190     if resp$(3)="True" then let grand=1
02200     if resp$(4)="True" then let quarterly=1
02210     if resp$(5)="True" then annual=1
02220     if resp$(6)="True" then employee=1
02230     beg_date=val(resp$(7)) !:
          end_date=val(resp$(8)) !:
          let qtr1=val(resp$(9)) !:
          let qtr2=val(resp$(10)) !:
          let qtr3=val(resp$(11)) !:
          let qtr4=val(resp$(12)) !:
          let z$=holdz$=hact$=resp$(13)(1:8) : let z$=holdz$=hact$=lpad$(trim$(z$),8)
02240     let qtr5=val(resp$(12)(1:4))*10000+1231
02250     begin_year=val(resp$(12)(1:4))*10000+0101
02260     end_year=val(resp$(12)(1:4))*10000+1231
02270     let gridname$=rpad$(trim$(resp$(14)),30) : rewrite #9,using "form pos 1,c 30",rec=1: gridname$
02280     if checkonly=1 and details=1 then goto L2290 else goto L2300
02290 L2290: mg$(1)="You cannot select 'Checkonly' and Details " !:
          mg$(2)="at the same time. Click OK to correct." !:
          fnmsgbox(mat mg$,resp$,cap$,0) !:
          goto SCREEN1
02300 L2300: if ckey<>3 and checkonly+details+grand+quarterly+annual+employee=0 then goto L2310 else goto L2370
02310 L2310: mg$(1)="You must select at least one type of information to be shown. " !:
          mg$(2)="                Click OK to correct." !:
          fnmsgbox(mat mg$,resp$,cap$,0) !:
          goto SCREEN1
02320 ! fnTOS(SN$="msgbox1")
02330 ! fnLBL(1,1,MG$(1),80,1)
02340 ! fnCMDKEY('&OK',5,0,1)
02350 ! fnACS(SN$,0,MAT RESP$,CKEY)
02360 ! Goto SCREEN1
02370 L2370: if ckey=3 then gosub SELECT_COLUMNS: goto ASKTRANSET
02380     justopen=1: gosub SELECT_COLUMNS: justopen=0
02390     goto L470
02400 ! ___________________________________________________________________
02410 SELECT_COLUMNS: ! 
02420     dim dat$*20,scr1$(10)*30,alloc(10),nam$*30,cap$*128,dednames$(20)*20
02430     dim holdnam$*30
02440     dim name$(46)*11
02450     dim r(20,4),hd1$*255,servicename$(10)*20,tg(11),end_date$*60,metraddr$*30
02460     dim cp0(45),cp1(45),cp2(45),hs1(45)
02470     name$(1)="Emp #" : name$(2)="Dept" !:
          name$(3)="Date" !:
          name$(4)="Check #" : name$(5)="Reg Hrs" !:
          name$(6)="OT Hrs" : name$(7)="Sick Hrs" !:
          name$(8)="Vac Hrs" : name$(9)="Hol Hrs"
02480     name$(10)="Reg Pay" : name$(11)="OT Pay" !:
          name$(12)="Other Pay" : name$(13)="Meals" !:
          name$(14)="Tips" : name$(15)="Total Pay" !:
          name$(16)="Net Pay"
02490     name$(17)="W/C Wage" : name$(18)="SS Wage" !:
          name$(19)="Med Wage" : name$(20)="Fed UC Wage" !:
          name$(21)="St U/C Wage": name$(22)="Fed Wh" !:
          name$(23)="SS Wh" : name$(24)="Med Wh" !:
          name$(25)="St Wh"
02500     for j=1 to 20
02510       name$(j+25)=dednames$(j)(1:11)
02520     next j
02530     name$(46)="EIC"
02540 OPEN_SELECTIONS: ! 
02550     if lrec(9)=0 then goto L2590
02560     read #9,using "form pos 1,c 30",rec=1: gridname$
02570     read #29,using "form pos 1,c 30,46*n 1",key=gridname$: gridname$,mat hf nokey L2600
02580     goto L2620
02590 L2590: write #9,using "form pos 1,c 30",rec=1: gridname$
02600 L2600: oldgridname$= gridname$="[All]                         "
02610     write #29,using "form pos 1,c 30,46*n 1": gridname$,mat hf
02620 L2620: oldgridname$=gridname$
02630 L2630: resp$(1)=gridname$
02640     for j=1 to udim(hf)
02650       if hf(j)=1 then resp$(j+1)="True"
02660       hf$(j)=rtrm$(name$(j))&r$
02670     next j
02680     if justopen=1 then goto L2930
02690 L2690: fntos(sn$="Checkprint") !:
          rc=cf=0 : linecnt=2
02700     fnlbl(1,1,"Grid or Report Name:",20,1)
02710     fncombof("payrollrpt",1,22,30,env$('Q')&"\PRmstr\payrollreports.h"&env$('cno'),1,30,0,0,env$('Q')&"\PRmstr\reportidx.h"&env$('cno'),0,pas, "",frame) !:
          resp$(rc+=1)=resp$(1)
02720     for j=1 to 23
02730       fnchk(linecnt+=1,16,name$(j),1,rratype) !:
            if hf(j)=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
02740     next j
02750     linecnt=2
02760     for j=1 to 22
02770       fnchk(linecnt+=1,35,name$(j+23),1,rratype) !:
            if hf(j+23)=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
02780     next j
02790     fnchk(linecnt+=1,35,name$(46),1,rratype) !:
          if hf(46)=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
02800     fncmdkey("&Next",1,1,0,"Begins printing your report.")
02810     if addone=0 then let fncmdkey("&Add",2,0,0,"Allows you to add another report or grid format..")
02820     if addone=1 then let fncmdkey("&Save This Format",4,0,0,"Save this format for later use.")
02830     fncmdkey("&Use This Format",3,0,0,"Use the format as now displayed.")
02840     fncmdkey("&Cancel",5,0,1,"Cancel without saving the format selections.")
02850     fnacs(sn$,0,mat resp$,ckey) !:
          if ckey=5 then goto L3150 ! select columns
02860     addone=0
02870     if ckey=2 then addone=1: gosub ADD_GRID: goto L2690
02880     let gridname$=rpad$(trim$(resp$(1)),30)
02890     if oldgridname$<>gridname$ then read #29,using "form pos 1,c 30,46*n 1",key=gridname$: gridname$,mat hf nokey L3150 : oldgridname$=gridname$: goto L2630
02900     for j=1 to 46
02910       if resp$(j+1)="True" then hf(j)=1 else hf(j)=0
02920     next j
02930 L2930: hfm$="FORM POS 1,c 12"
02940     let ul$=hd$="            "
02950     hs1=0: hs2=0
02960     for j=1 to udim(hf$)
02970       if hf(j)=0 then goto L3070
02980       hs2=hs2+1
02990       if j=1 then hfm$=hfm$&",NZ 8" : hs1=hs1+8 : hz1=hs2 !:
              let ul$=ul$&" -------": hs3=8 ! employee #
03000       if j=2 then hfm$=hfm$&",NZ 5" : hs1=hs1+5 : hz1=hs2 !:
              let ul$=ul$&" ----": hs3=5 ! dept #
03010       if j=3 then hfm$=hfm$&",pic(bzzzz/zz/zz)" : hs1=hs1+11 !:
              hz1=hs2 !:
              let ul$=ul$&" ----------": hs3=11 ! date
03020       if j=4 then hfm$=hfm$&",NZ 7" : hs1=hs1+7 : hz1=hs2 !:
              let ul$=ul$&" ------": hs3=7 ! check number
03030       if j>4 and j<10 then hfm$=hfm$&",G 8.2" : hs1=hs1+8 !:
              let ul$=ul$&" -------" : hs3=8 ! hours
03040       if j>9 and j<17 then hfm$=hfm$&",G 10.2" : hs1=hs1+10 !:
              let ul$=ul$&" ---------" : hs3=10 ! wages
03050       if j>16 and j<47 then hfm$=hfm$&",G 10.2" : hs1=hs1+10 !:
              let ul$=ul$&" ---------" : hs3=10 ! deductions
03060       hd$=hd$&lpad$(trim$(name$(j)(1:hs3-1)),hs3)
03070 L3070: next j
03080     mat cp0(hs2)
03090     mat cp1(hs2)
03100     mat cp2(hs2)
03110     rewrite #9,using "form pos 1,c 30",rec=1: gridname$
03120     rewrite #29,using "form pos 1,c 30,46*n 1",key=oldgridname$: gridname$,mat hf
03130     f1=0
03140     if ckey=10 then goto SELECT_COLUMNS
03150 L3150: return 
03160 ! ___________________________________________________________________
03170 PRINT_DETAILS: ! 
03180     if f1=0 then gosub HDR
03190     if printit=0 and employee=1 and holdeno>0 and checkonly=0 and holdeno><eno then gosub EMPLOYEE_TOTALS
03200     mat cp0=(0)
03210     ds$=item$(2)
03220     for j=1 to 45
03230       hs1(j)=val(item$(j+2))
03240     next j
03250     form pos 1,n 8,n 3,pd 6,n 7,5*pd 3.2,37*pd 5.2
03260     hs3=0
03270     for j=1 to udim(hs1)
03280       if j=1 and eno<>holdeno then empz$=lpad$(str$(hs1(1)),8): nam$="": read #1,using "form pos 9,c 25",key=empz$: nam$ nokey L3290
03290 L3290: if hf(j)=0 then goto L3330
03300       hs3=hs3+1
03310       cp0(hs3)=hs1(j)
03315       if eofcode=1 and grand=1 and desc$="Grand Total" then goto L3330 ! skip accumulating if finished
03316       if trim$(desc$)="1st Qtr" or trim$(desc$)="2nd Qtr" or trim$(desc$)="3rd Qtr" or trim$(desc$)="4th Qtr" or trim$(desc$)="YTD" or trim$(desc$)="Grand Total" or trim$(desc$)="Employee Total" then goto L3330
03320       cp2(hs3)+=hs1(j) ! accumulate totals
03330 L3330: next j
03340     if trim$(nam$)<>"" and holdnam$<>nam$ then desc$=nam$(1:12): holdnam$=nam$: nam$=""
03350     if trim$(desc$)="Total Ck" then goto L3360 ! don't pr total check on printout
03360 L3360: pr #255,using hfm$: desc$(1:12),mat cp0
03361     if desc$(1:12)="Employee Tot" then pr #255: 
03362     desc$=""
03370     mat cp1=cp1+cp0
03380 ! Mat CP2=CP2+CP0  kj 1/30/08
03390     mat cp0=(0)
03400     return 
03410 PGOF: pr #255: newpage
03420     gosub HDR
03430     continue 
03440 ! ___________________________________________________________________
03450 HDR: ! 
03460 ! need date$,time$
03470     pr #255: "\qc  {\f181 \fs20 \b "&trim$(env$('cnam'))&" }"
03480     pr #255: "\qc  {\f181 \fs28 \b "&trim$(gridname$)&" }"
03490     if beg_date<>0 and end_date<>0 then !:
            pr #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",beg_date)& "  To "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
03500     pr #255: ""
03510     pr #255: "\ql "
03520     pr #255: hd$
03530     pr #255: ul$
03540     f1=1
03550     return 
03560 ! ___________________________________________________________________
03570 GRAND_TOTAL: ! 
03580     if grand=0 then goto L3750
03590     if printit=1 then pr #255: ul$
03600     if printit=1 then pr #255: "     <Grand Totals>"
03610     if hf(1)=1 then cp2(1)=0 ! no totals on employee numbers
03620     if hf(1)=1 and hf(2)=1 then cp2(2)=0 ! no totals on departments
03630     if hf(1)=0 and hf(2)=1 then cp2(1)=0 ! no totals on departments
03640     if hf(1)=1 and hf(2)=1 and hf(3)=1 then cp2(3)=0 ! no totals on date
03650     if hf(1)=0 and hf(2)=1 and hf(3)=1 then cp2(2)=0 ! no totals on date
03655     if hf(1)=1 and hf(2)=0 and hf(3)=1 then cp2(2)=0 ! no totals on date
03660     if hf(1)=0 and hf(2)=0 and hf(3)=1 then cp2(1)=0 ! no totals on date
03670     if hf(1)=1 and hf(2)=1 and hf(3)=1 and hf(4)=1 then cp2(4)=0 ! no totals on ck num
03672     if hf(1)=0 and hf(2)=0 and hf(3)=1 and hf(4)=1 then cp2(2)=0 ! no totals on ck num ! new 11/9/15 could this be right?
03680     if hf(1)=0 and hf(2)=0 and hf(3)=0 and hf(4)=1 then cp2(1)=0 ! no totals on cknum
03682     if hf(1)=1 and hf(2)=0 and hf(3)=1 and hf(4)=1 then cp2(3)=0 ! no totals on cknum
03690     if printit=1 then pr #255,using hfm$: "",mat cp2
03700     if printit=0 then desc$="Grand Total": mat totaltcp=grand2tcp: mat totaltdc=grand2tdc: gosub PRINT_GRID
03710     mat grand2tcp=(0) : : mat grand2tdc=(0)
03720     mat cp2=(0)
03730     form pos 1,c 29,18*g 10.2,skip 1
03740     if printit=1 then pr #255: ul$
03750 L3750: return 
03760 ! ___________________________________________________________________
03770 EMPLOYEE_TOTALS: ! 
03780     if hf(1)=1 then cp1(1)=0 ! no totals on employee numbers
03790     if hf(1)=1 and hf(2)=1 then cp1(2)=0 ! no totals on departments
03800     if hf(1)=0 and hf(2)=1 then cp1(1)=0 ! no totals on departments
03810     if hf(1)=1 and hf(2)=1 and hf(3)=1 then cp1(3)=0 ! no totals on date
03815     if hf(1)=1 and hf(2)=0 and hf(3)=1 then cp1(2)=0 ! no totals on date
03820     if hf(1)=0 and hf(2)=1 and hf(3)=1 then cp1(2)=0 ! no totals on date
03830     if hf(1)=0 and hf(2)=0 and hf(3)=1 then cp1(1)=0 ! no totals on date
03840     if hf(1)=1 and hf(2)=1 and hf(3)=1 and hf(4)=1 then cp1(4)=0 ! no totals on ck num
03850     if hf(1)=0 and hf(2)=1 and hf(3)=1 and hf(4)=1 then cp1(3)=0 ! no totals on cknum
03860     if hf(1)=0 and hf(2)=0 and hf(3)=1 and hf(4)=1 then cp1(2)=0 ! no totals on cknum
03870 ! 
03880     pr #255,using hfm$: "   Emp Total",mat cp1
03890     mat cp1=(0)
03900     pr #255: "" pageoflow PGOF
03910     return 
03920 ! ___________________________________________________________________
03930 FLEXGRID: ! 
03940     dim colmask$(48),colhdr$(48)*20,item$(48)*70,transtype$(5)*40,tcp(32)
03950     dim totaltcp(32),totaltdc(10),holdtotaltcp(32),holdtotaltdc(10)
03960     dim tdc(10),qtr1tdc(10),qtr2tdc(10),qtr3tdc(10),qtr4tdc(10)
03970     dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32)
03980     dim annualtdc(10),annualtcp(32),mg$(2)*80,hfm$*500,ul$*500,hd$*500
03990     dim employeetdc(10),employeetcp(32),holdtdc(10),holdtcp(32),grand2tcp(32)
04000     dim grand2tdc(10)
04010 ! ______________________________________________________________________
04020     if trim$(z$)="[All]" then let z$=""
04030     if trim$(z$)<>"" then let z$=lpad$(trim$(z$),8)
04040     mat colhdr$(48) : mat colmask$(48)
04050     let x=2
04060     colhdr$(1)="Rec" : colhdr$(2)="Desc"
04070     colmask$(1)="30": colmask$(2)=""
04080     if hf(1)=1 then colhdr$(x+=1)=name$(1) : colmask$(x)="30" ! employee #
04090     if hf(2)=1 then colhdr$(x+=1)=name$(2) : colmask$(x)="30" ! dept #
04100     if hf(3)=1 then colhdr$(x+=1)=name$(3) : colmask$(x)="3" ! Payroll Date
04110     if hf(4)=1 then colhdr$(x+=1)=name$(4) : colmask$(x)="30" ! check stop
04120     if hf(5)=1 then colhdr$(x+=1)=name$(5) : colmask$(x)="32"
04130     if hf(6)=1 then colhdr$(x+=1)=name$(6) : colmask$(x)="32" ! ot hours
04140     if hf(7)=1 then colhdr$(x+=1)=name$(7) : colmask$(x)="32"
04150     if hf(8)=1 then colhdr$(x+=1)=name$(8) : colmask$(x)="32"
04160     if hf(9)=1 then colhdr$(x+=1)=name$(9) : colmask$(x)="32"
04170     if hf(10)=1 then colhdr$(x+=1)=name$(10) : colmask$(x)="10"
04180     if hf(11)=1 then colhdr$(x+=1)=name$(11) : colmask$(x)="10"
04190     if hf(12)=1 then colhdr$(x+=1)=name$(12) : colmask$(x)="10"
04200     if hf(13)=1 then colhdr$(x+=1)=name$(13) : colmask$(x)="10"
04210     for j=14 to 46
04220       if hf(j)=1 then colhdr$(x+=1)=name$(j) : colmask$(x)="10"
04230     next j
04240     mat colhdr$(x) : mat colmask$(x) : mat printitem$(x)
04250     fnflexinit1("prchecks",1,1,20,100,mat colhdr$,mat colmask$,1)
04260 READ_CHECKS: ! 
04270     if trim$(hact$)="[All]" then restore #filnum: : goto READ_BREAKDOWNS
04280     restore #filnum,key>=lpad$(hact$,8)&cnvrt$("pd 6",beg_date)&"   ": nokey L570 ioerr L570
04290 READ_BREAKDOWNS: ! 
04300     holdeno=eno !:
          holdckno=ckno !:
          holdtdn=tdn !:
          holdprd=prd
04310 L4310: read #filnum,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",release: eno,tdn,prd,ckno,mat tdc,mat tcp eof CONSIDER_ANNUAL_EOF
04320     if trim$(hact$)<>"[All]" and hact$<>cnvrt$("pic(zzzzzzzz)",eno) then goto CONSIDER_ANNUAL
04330     if beg_date<>0 and prd<beg_date then goto L4310
04340     if end_date><0 and prd>end_date then goto L4310
04350     dim printitem$(48)*70
04360     if holdeno>0 and eno<>holdeno and trim$(hact$)<>"[All]" then goto CONSIDER_ANNUAL_EOF ! not same account and should treated as end of file
04370     if holdeno>0 and eno<>holdeno then goto CONSIDER_ANNUAL !:
            ! not same account
04380 L4380: if annual=1 and prd>=begin_year and prd<=end_year then mat annualtdc=annualtdc+tdc: mat annualtcp=annualtcp+tcp
04390     if prd>=beg_date and prd<=end_date then mat employeetdc=employeetdc+tdc: mat employeetcp=employeetcp+tcp : mat grand2tcp=grand2tcp+tcp: mat grand2tdc=grand2tdc+tdc
04400 ! need to start analyzing quarters, etc here
04410     if quarterly=0 then goto L4550
04420     mat holdtotaltcp=totaltcp: mat holdtotaltdc=totaltdc ! hold these subtotals in case the quarterly destroys them
04430     if prd=>qtr2 and qtr1printed=0 and sum(totaltdc)+sum(totaltcp)<>0 then gosub PRINT_GRID : holdckno=0 ! last check not printed yet
04440     if prd=>qtr2 and qtr1printed=0 and sum(qtr1tdc)+sum(qtr1tcp)>0 then mat totaltdc=qtr1tdc: mat totaltcp=qtr1tcp: let qtr1printed=1: desc$="1st Qtr" : gosub PRINT_GRID : holdckno=0
04450     if prd=>qtr3 and qtr2printed=0 and sum(totaltdc)+sum(totaltcp)<>0 then gosub PRINT_GRID ! last check not printed yet
04460     if prd=>qtr3 and qtr2printed=0 and sum(qtr2tdc)+sum(qtr2tcp)>0 then mat totaltdc=qtr2tdc: mat totaltcp=qtr2tcp: let qtr2printed=1: desc$="2nd Qtr" : gosub PRINT_GRID : holdckno=0
04470     if prd=>qtr4 and qtr3printed=0 and sum(totaltdc)+sum(totaltcp)<>0 then gosub PRINT_GRID ! last check not printed yet
04480     if prd=>qtr4 and qtr3printed=0 and sum(qtr3tdc)+sum(qtr3tcp)>0 then mat totaltdc=qtr3tdc: mat totaltcp=qtr3tcp: let qtr3printed=1: desc$="3rd Qtr": gosub PRINT_GRID: holdckno=0
04490     if prd=>qtr5 and qtr4printed=0 and sum(totaltdc)+sum(totaltcp)<>0 then gosub PRINT_GRID ! last check not printed yet
04500     if prd>qtr5 and qtr4printed=0 and sum(qtr4tdc)+sum(qtr4tcp)>0 then mat totaltdc=qtr4tdc: mat totaltcp=qtr4tcp: let qtr4printed=1: desc$="4th Qtr": gosub PRINT_GRID : goto CONSIDER_ANNUAL
04510     if prd>=qtr1 and prd<qtr2 then mat qtr1tdc=qtr1tdc+tdc : : mat qtr1tcp=qtr1tcp+tcp
04520     if prd>=qtr2 and prd<qtr3 then mat qtr2tdc=qtr2tdc+tdc : mat qtr2tcp=qtr2tcp+tcp
04530     if prd>=qtr3 and prd<qtr4 then mat qtr3tdc=qtr3tdc+tdc : mat qtr3tcp=qtr3tcp+tcp
04540     if prd>=qtr4 and prd=<qtr5 then mat qtr4tdc=qtr4tdc+tdc : mat qtr4tcp=qtr4tcp+tcp
04550 L4550: if checkonly=1 and holdckno=0 then !:
            mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp !:
            goto READ_BREAKDOWNS ! same check, no details
04560     if checkonly=1 and holdckno<>0 and holdckno=ckno then !:
            mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp !:
            goto READ_BREAKDOWNS ! same check, no details
04570     if checkonly=1 and holdckno<>0 and holdckno<>ckno and (sum(tdc)+sum(tcp))>0 then !:
            desc$="Total Ck" !:
            holdrecnum=0 !:
            gosub PRINT_GRID !:
            mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp: !:
            desc$="Total Ck"
04580     if details=1 then !:
            enoprint=eno !:
            tdnprint=tdn !:
            prdprint=prd !:
            cknoprint=ckno !:
            mat totaltdc=totaltdc+tdc !:
            mat totaltcp=totaltcp+tcp
04590     if details=1 then gosub PRINT_GRID
04600     goto READ_BREAKDOWNS
04610 PRINT_GRID: ! 
04620     recnum=rec(filnum): if trim$(desc$)<>"" then recnum=0
04630     if trim$(desc$)="1st Qtr" or trim$(desc$)="2nd Qtr" or trim$(desc$)="3rd Qtr" or trim$(desc$)="4th Qtr" or trim$(desc$)="YTD" or trim$(desc$)="Grand Total" or trim$(desc$)="Employee Total" then !:
            enoprint=tdnprint=prdprint=cknoprint=0 !:
            item$(1)=item$(3)=item$(4)=item$(5)=item$(6)="": item$(2)=desc$ !:
            desc$="": goto L4690
04640     if details=0 then enoprint=holdeno: tdnprint=holdtdn: prdprint=holdprd: cknoprint=holdckno
04645     if printit=1 then employeekey$=cnvrt$("pic(zzzzzzzz)",eno) : goto L4660 ! use different key for pr instead of grid
04650     if printit=1 or details=1 then employeekey$=cnvrt$("pic(zzzzzzzz)",eno) : goto L4660 ! use different key for pr instead of grid
04653     employeekey$=cnvrt$("pic(zzzzzzzz)",holdeno) ! key for grids
04660 L4660: if sum(totaltcp)=(0) and sum(totaltdc)=(0) then goto L4920
04670     read #1,using "form pos 9,c 18",key=employeekey$: desc$ nokey L4680
04680 L4680: item$(1)=cnvrt$("pic(zzzzzzz)",recnum) : : item$(2)=desc$ !:
          item$(3)=cnvrt$("pic(zzzzzzzz)",enoprint) !:
          item$(4)=cnvrt$("pic(zzz)",tdnprint): item$(5)=str$(prdprint) !:
          item$(6)=cnvrt$("pic(zzzzzzz)",cknoprint)
04690 L4690: item$(7)=str$(totaltdc(1)): item$(8)=str$(totaltdc(2)) !:
          item$(9)=str$(totaltdc(3)): item$(10)=str$(totaltdc(4)) !:
          item$(11)=str$(totaltdc(5)): item$(12)=str$(totaltcp(26)) !:
          item$(13)=str$(totaltcp(27)): item$(14)=str$(totaltcp(28)) !:
          item$(15)=str$(totaltcp(29)): item$(16)=str$(totaltcp(30))
04700     item$(17)=str$(totaltcp(31)): item$(18)=str$(totaltcp(32)) !:
          item$(19)=str$(totaltdc(6)) !:
          item$(20)=str$(totaltdc(7)): item$(21)=str$(totaltdc(8)) !:
          item$(22)=str$(totaltdc(9)): item$(23)=str$(totaltdc(10)) !:
          item$(24)=str$(totaltcp(1)): item$(25)=str$(totaltcp(2)) !:
          item$(26)=str$(totaltcp(3)): item$(27)=str$(totaltcp(4))
04710     items=27
04720     for j=1 to 20
04730       item$(items+=1)=cnvrt$("pic(-------.zz)",totaltcp(j+4))
04740     next j
04750     item$(items+=1)=str$(totaltcp(25)) ! eic
04760     if printit=1 then desc$=item$(2): gosub PRINT_DETAILS: goto L4840
04770     let x=2
04780     printitem$(1)=item$(1): printitem$(2)=item$(2)
04790     for j=1 to 46
04800       if hf(j)=1 then printitem$(x+=1)=item$(j+2)
04810     next j
04820 L4820: fnflexadd1(mat printitem$)
04830     holdeno=eno !:
          holdckno=ckno !:
          holdtdn=tdn !:
          holdprd=prd
04840 L4840: if repeatit=1 then repeatit=0: goto L4860
04850     if trim$(desc$)="1st Qtr" or trim$(desc$)="2nd Qtr" or trim$(desc$)="3rd Qtr" or trim$(desc$)="4th Qtr" then !:
            mat item$=(""): repeatit=1 !:
            goto L4820
04860 L4860: if trim$(printitem$(2))="Employee Total" then !:
            mat printitem$=(""): fnflexadd1(mat printitem$)
04862     mat totaltdc=(0): mat totaltcp=(0)
04870     desc$=""
04880     if qtr1printed=1 then let qtr1printed=2: return 
04890     if qtr2printed=1 then let qtr2printed=1: return 
04900     if qtr3printed=1 then let qtr3printed=2: return 
04910     if qtr4printed=1 then let qtr4printed=2: return 
04920 L4920: return 
04930 CONSIDER_ANNUAL_EOF: ! 
04940     eofcode=1
04950 CONSIDER_ANNUAL: ! 
04960 ! If EOFCODE=1 AND EMPLOYEE=1 AND PRD>=BEG_DATE AND PRD<=END_DATE Then Mat EMPLOYEETDC=EMPLOYEETDC+TDC: Mat EMPLOYEETCP=EMPLOYEETCP+TCP : Mat GRAND2TCP=GRAND2TCP+TCP: Mat GRAND2TDC=GRAND2TDC+TDC
04970 ! If EOFCODE=1 AND ANNUAL=1 AND PRD>=BEGIN_YEAR AND PRD<=END_YEAR Then Mat ANNUALTDC=ANNUALTDC+TDC: Mat ANNUALTCP=ANNUALTCP+TCP
04980     if sum(totaltdc)+sum(totaltcp)<>0 then gosub PRINT_GRID ! last check not printed yet
04990     if sum(totaltdc)+sum(totaltcp)<>0 and checkonly=1 then desc$="Total Ck"
05000     if (sum(qtr1tdc)>0 or sum(qtr1tcp)>0) and qtr1printed=0 then !:
            mat totaltdc=qtr1tdc: mat totaltcp=qtr1tcp !:
            let qtr1printed=1: desc$="1st Qtr": holdnam$="": gosub PRINT_GRID
05010     if (sum(qtr2tdc)>0 or sum(qtr2tcp)>0) and qtr2printed=0 then !:
            mat totaltdc=qtr2tdc: mat totaltcp=qtr2tcp: let qtr2printed=1 !:
            desc$="2nd Qtr": holdnam$="": gosub PRINT_GRID
05020     if (sum(qtr3tdc)>0 or sum(qtr3tcp)>0) and qtr3printed=0 then !:
            mat totaltdc=qtr3tdc: mat totaltcp=qtr3tcp: let qtr3printed=1 !:
            desc$="3rd Qtr": holdnam$="" : gosub PRINT_GRID
05030     if (sum(qtr4tdc)>0 or sum(qtr4tcp)>0) and qtr4printed=0 then !:
            mat totaltdc=qtr4tdc: mat totaltcp=qtr4tcp: let qtr4printed=1 !:
            desc$="4th Qtr": holdnam$="": gosub PRINT_GRID
05040     if annual=1 then enoprint=tdnprint=prdprint=cknoprint=0 !:
            mat totaltdc=annualtdc: mat totaltcp=annualtcp !:
            annual_printed=1 : desc$="YTD": gosub PRINT_GRID !:
            mat annualtcp=(0) : mat annualtdc=(0)
05050 ! If PRINTIT=1 Then Goto 4810 ! don't use the totals if pr report
05060 ! If PRINTIT=1 Then Gosub EMPLOYEE_TOTALS
05070     if employee=1 and holdeno<>0 then !:
            mat totaltdc=employeetdc: mat totaltcp=employeetcp !:
            employee_printed=1 : desc$="Employee Total": gosub PRINT_GRID: mat employeetcp=(0): mat employeetdc=(0)
05080     if eofcode=1 and grand=1 then gosub GRAND_TOTAL
05090     if eofcode=1 then eofcode=0: holdeno=0: goto L5120
05100     if trim$(hact$)="[All]" and quarterly=1 and holdeno>0 then !:
            let qtr1printed=qtr2printed=qtr3printed=qtr4printed=0 !:
            mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0) : mat qtr4tcp=(0) !:
            mat qtr1tdc=(0): mat qtr2tdc=(0): mat qtr3tdc=(0): mat qtr4tdc=(0) !:
            mat totaltdc=(0): mat totaltcp=(0) !:
            mat annualtcp=(0) !:
            goto L4380
05110     if trim$(hact$)="[All]" then goto L4380
05120 L5120: if printit=1 then let fncloseprn : goto SCREEN1
05130     goto L570
05140 ! ______________________________________________________________________
05150 SETUP_REPORTS: ! 
05160 ! 1 - 30 Name c 30 !:
          ! 31 - 76 selections 46*n 1
05170     open #29: "Name="&env$('Q')&"\PRmstr\payrollreports.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\reportidx.H"&env$('cno')&",RecL=85,kps=1,kln=30,replace",internal,outin,keyed 
05180     close #29: 
05185 CREATE_INDEX: ! 
05190     execute "Index "&env$('Q')&"\PRmstr\payrollreports.H"&env$('cno')&' '&env$('Q')&"\PRmstr\reportidx.H"&env$('cno')&" 1 30 Replace DupKeys -n"
05200     return 
05210 ADD_GRID: ! 
05220     fntos(sn$="Addgrid") !:
          lc=rc=0 : mylen=20 : mypos=mylen+3
05230     fnlbl(1,1,"Grid or Report Name:",20,1)
05240     fntxt(1,mypos,30,0,0,"") !:
          resp$(1)=""
05250     fncmdkey('&Save',1,1,0,"Adds this new grid or report to your selections.") !:
          fncmdkey('&Cancel',5,0,1,"Returns to selection screen without adding this report.")
05260     fnacs(sn$,0,mat resp$,ckey) ! add grid name
05270     if ckey=5 then goto L5330
05280     oldgridname$=gridname$=rpad$(trim$(resp$(1)),30)
05290     rewrite #9,using "form pos 1,c 30",rec=1: gridname$
05300     mat hf=(0)
05310     write #29,using "form pos 1,c 30,46*n 1": gridname$,mat hf
05320     mat resp$=(""): resp$(1)=gridname$
05330 L5330: return 
05340 NOKEY_ON_GRID: ! 
05350     oldgridname$=gridname$=resp$(1)
05360     rewrite #9,using "form pos 1,c 30",rec=1: gridname$
05370     mat hf=(0)
05380     write #29,using "form pos 1,c 30,46*n 1": gridname$,mat hf
05390     mat resp$=(""): resp$(1)=gridname$
05400     goto L2620
05410 DONE: ! 
05420 XIT: ! 
05430     close #9: ioerr L5440
05440 L5440: close #29: ioerr L5450
05450 L5450: fnend 
