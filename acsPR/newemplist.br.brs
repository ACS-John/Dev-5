00010 ! Replace S:\acsPR\newEmpList
00020 ! pr Employee List
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fnerror,fnxit,fntop,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fntxt,fncmdkey,fnacs,fncomboa,fncombof,fnGetPayrollDates
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim a$*40,em$(1)*30,ta(2),cp(22),tcp(22),hc(5),thc(5),dat$*20,whc(10)
00080   dim dedcode(10),calcode(10),dedfed(10),message$*40,cap$*128
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Active Employee List")
00120 ! 
00130 ! ______________________________________________________________________
00140   on fkey 5 goto DONE
00150 ! ______________________________________________________________________
00160   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,ppd,dat$)
00190   gosub ASKFORMAT
00200 ! ______________________________________________________________________
00210   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",Shr",internal,input,relative 
00220   open #5: "Name="&env$('Temp')&"\Temp1."&session$&",Size=0,RecL=66,Replace",internal,output 
00230   fnopenprn
00240 ! ______________________________________________________________________
00250 TOPOFLOOP: ! 
00260 L260: read #1,using L280: eno,em$(1),lpd,em4 eof L340
00270   gosub L770
00280 L280: form pos 1,n 8,c 30,pos 162,n 6,pos 118,n 2
00285   if status=0 then goto L300 ! allow all to print
00290   if em4=status then goto L260 ! skip terminated employees
00300 L300: ! If fndate_mmddyy_to_ccyymmdd(LPD)><PPD Then Goto 260
00310   write #5,using L320: eno,last$,first$,mid$,lpd,em4
00320 L320: form pos 1,n 8,c 20,c 15,c 15,n 6,n 2
00330   goto L260
00340 L340: ! 
00350   gosub HDR
00360   close #5: 
00370   execute "INDEX "&env$('Temp')&"\Temp1."&session$&" "&env$('Temp')&"\TempIdx."&session$&" 9 50 Replace DupKeys"
00380   open #5: "Name="&env$('Temp')&"\Temp1."&session$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,outin,keyed 
00390 L390: read #5,using L320: eno,last$,first$,mid$,lpd,em4 eof DONE
00400   pr #255,using L410: eno,trim$(first$)&" "&trim$(mid$)&" "&trim$(last$),lpd,em4 pageoflow PGOF
00410 L410: form pos 1,n 8,x 3,c 30,x 3,pic(zz/zz/zz),x 9,n 2,skip 1
00420   goto L390
00430 ! ______________________________________________________________________
00440 PGOF: ! 
00450   pr #255: newpage
00460   gosub HDR
00470   continue 
00480 ! ______________________________________________________________________
00490 HDR: ! 
00500   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00510   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00520   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
00530   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
00540   pr #255: "\ql   "
00545   pr #255,using "form pos 1,c 80": "Employee #  Name                      Last Payroll Date   Status"
00550   return 
00560 DONE: ! 
00570   close #5: ioerr L580
00580 L580: let fncloseprn
00590   goto XIT
00600 ! ______________________________________________________________________
00610 ERTN: ! 
00620   if err=61 then pr f "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L640
00630   goto L680
00640 L640: pr newpage
00650   if err=4148 then pr f "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L670
00660   goto L680
00670 L670: pr f "23,1,C 80,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00680 L680: pr f "24,1,C 80,N": "Press Enter to Retry or Q to Quit:"
00690   input fields "24,60,C 1,N": quitcode$
00700   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00710   pr f "23,1,C 80,N": ""
00720   pr f "24,1,C 80,N": ""
00730   retry 
00740 ! ______________________________________________________________________
00750 XIT: let fnxit
00760 ! ______________________________________________________________________
00770 L770: dim first$*15,mid$*15,last$*20,item1$(2)*30
00780   em$(1)=uprc$(rtrm$(em$(1)))
00790   let x1=pos(em$(1)," ",1)
00800   let x2=pos(em$(1)," ",x1+1)
00810   let x3=pos(em$(1)," ",x2+1)
00820   if uprc$(namcde$)="L" then goto L870
00830   let first$=trim$(em$(1)(1:max(x1-1,1)))
00840   if x2>0 then let mid$=trim$(em$(1)(x1+1:x2-1)): last$=trim$(em$(1)(x2+1:len(em$(1))))
00850   if x2=0 then last$=trim$(em$(1)(x1+1:len(em$(1)))): let mid$=""
00860   goto L910
00870 L870: ! last name first
00880   if x1>0 and em$(1)(x1-1:x1-1)="," then last$=trim$(em$(1)(1:x1-2)) else last$=trim$(em$(1)(1:max(x1-1,1)))
00890   if x2>0 then let first$=trim$(em$(1)(x1+1:x2-1)): let mid$=trim$(em$(1)(x2+1:len(em$(1))))
00900   if x2=0 then let first$=trim$(em$(1)(x1+1:len(em$(1)))): let mid$=""
00910 L910: ! pr FIRST$,MID$,LAST$
00920   return 
00930 ASKFORMAT: ! 
00940   fntos(sn$="Emplist") !:
        let respc=0
00950   fnlbl(1,1,"Order for Printing Name:",28,1)
00960   let fi$="Emplist1" !:
        let item1$(1)="First Name First" !:
        let item1$(2)="Last Name First": let fncomboa(fi$,1,31,mat item1$,"How is the employee name entered in the employee record?).") !:
        let resp$(respc+=1)=item1$(1)
00970   fnlbl(2,1,"Status Code:",28,1)
00980   fncombof("EmpStatus",2,30,25,env$('Q')&"\PRmstr\EmpStatus.dat",1,2,3,25,env$('Q')&"\PRmstr\EmpStatus.idx",0,0, "Indicate the code used for terminated employees",fracustinfo,0) !:
        let resp$(respc+=1)=str$(status)
00990   fncmdkey("&Next",1,1,0,"Proceed with printing." ) !:
        fncmdkey("E&xit",5,0,1,"Returns to menu")
01000   fnacs(sn$,0,mat resp$,ckey) ! ask employee #
01010   if ckey=5 then goto XIT
01020   let namcde$=resp$(1)(1:1)
01030   status=val(resp$(2)(1:2))
01040   return 
