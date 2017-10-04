00010 ! Replace S:\acsCL\ChartOfAccounts
00020 ! Checkbook Transaction Allocation File - Hamster !:
        ! pretty useless to the end user - but quite usefull to the programmer
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4)
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$='Chart of Accounts')
00100   fncno(cno)
00110   open #20: "Name="&env$('Q')&"\CLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using 'Form POS 150,2*N 1',rec=1: d(1),d(2) !:
        close #20: 
00120   gosub BUILD_LAYOUT
00130   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00135   gosub CLOSE_FILE
00136   execute "Index "&env$('Q')&"\CLmstr\GLmstr.H"&str$(cno)&' '&env$('Q')&"\GLmstr\glindex.h"&str$(cno)&"1 12,replace,DupKeys" ioerr L140
00140 L140: goto XIT
00150 ! ______________________________________________________________________
00160 OPEN_FILE: ! !:
        open_file_count=0 ! this value is used in the close_file sub routine
00170   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\GLmstr.H"&str$(cno)&",Version=0,KFName="&env$('Q')&"\CLmstr\GLIndex.h"&str$(cno)&",Use,RecL=62,KPs=1,KLn=12,Shr",internal,outin,keyed 
00180   return 
00190 ! ______________________________________________________________________
00200 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00230   fncno(cno)
00240 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00250   lbl$(ic+=1)="Department" !:
        lbl$(ic+=1)="Account" !:
        lbl$(ic+=1)="Sub Account" !:
        lbl$(ic+=1)="Description"
00260 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        ccyymmdd=10
00270   let tln(ic+=1)=3 !:
        let tln(ic+=1)=6 !:
        let tln(ic+=1)=3 !:
        let tln(ic+=1)=50
00280 ! ** Field Types ** !:
        let ic=0
00290   fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='C'
00300 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : ccyymmdd=8
00310   sln(ic+=1)=3 !:
        sln(ic+=1)=6 !:
        sln(ic+=1)=3 !:
        sln(ic+=1)=50
00320 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00330   let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=0
00340   if d(1)=0 then let mask(1)+=10000
00350   if d(2)=0 then let mask(3)+=10000
00360   return 
00370 ! ______________________________________________________________________
00380 HAMSTER: ! 
00390   fnhamster("ChartOfAccounts",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
00400   return 
00410 ! ______________________________________________________________________
00420 XIT: let fnxit
00430 ! ______________________________________________________________________
00440 ! <Updateable Region: ERTN>
00450 ERTN: let fnerror(program$,err,line,act$,"xit")
00460   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00470   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00480   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00490 ERTN_EXEC_ACT: execute act$ : goto ERTN
00500 ! /region
00510 ! ______________________________________________________________________
