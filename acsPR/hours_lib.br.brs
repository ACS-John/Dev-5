00010 ! Replace S:\acsPR\hours.br
00020 ! enter and track houly breakdowns of time for comp time, etc
00030   def library fnhours(eno)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fntop,fnxit, fncno,fnopenprn,fncloseprn,fntos,fnfra,fnlbl,fntxt,fnchk,fnflexinit1,fnflexadd1,fnbutton,fnacs,fnerror,fnmsgbox,fncmdset,fncombof,fncmdkey
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim cnam$*40,cap$*128,message$*40,resp$(10)*40
00090     dim oldclass$*5,class$*5,classification$*30,impname$*25,empname$*30
00100     dim flxitm$(8)*30,key$*21,ml$(3)*80
00110 ! ______________________________________________________________________
00120     let fncno(cno,cnam$)
00130 ! ______________________________________________________________________
00140 ! 
00150 ! 
00160 ! Let FNTOP("S:\acsPR\hourclassification2",CAP$="Time Classification")
00170     open #breakdown=31: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno)&",Shr",internal,outin,keyed 
00180     open #classification=32: "Name="&env$('Q')&"\PRmstr\HourClass.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\HourClass-idx.H"&str$(cno)&",Shr",internal,outin,keyed ioerr MSGBOX3
00190     open #prmstr=33: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00200 MAIN: ! 
00210     addhours=edithours=0
00220     let fntos(sn$="Main") !:
          let respc=0 : let lc=0 : mat resp$=('') !:
          let mylen=20 : let mypos=mylen+2
00230     let fnlbl(lc+=1,1,'Employee Number:',mylen,1,0,0)
00240     let fncombof("PRmstr",lc,mypos,0,env$('Q')&"\PRmstr\rpmstr.h"&str$(cno),1,8,9,30,env$('Q')&"\PRmstr\Rpindex.h"&str$(cno),2,pas, "Enter the employee number you wish to work with.",0)
00241     if hact$="[All]" then let resp$(1)="[All]" else !:
            let resp$(1)=str$(eno)
00242     let fnbutton(lc+=2,32,"&Refresh",46,"",0,0,0,0,1) !:
          !   let fnbutton(lc,41,"&Add",43) !:
          !   let fnbutton(lc,46,"&Edit",45) !:
          !   let fnbutton(lc,52,"&Delete",44) !:
          !   let fnbutton(lc,60,"&Cancel",5)
00250     mat chdr$(8) : mat cmask$(8) : mat flxitm$(8) !:
          chdr$(1)="Ref #" : chdr$(2)="Emp #" : chdr$(3)="Name" : chdr$(4)="Classification" !:
          chdr$(5)="Date" : chdr$(6)="Increase" !:
          chdr$(7)="Decrease" : chdr$(8)="Balance" !:
          cmask$(5)='3' : cmask$(6)='10' : cmask$(7)="10" !:
          cmask$(8)="10" !:
          let fnflexinit1('Hours',lc+2,1,15,66,mat chdr$,mat cmask$,1) !:
          let lc+=18
00255     if hact$="[All]" then restore #breakdown: nokey EOBREAKDOWN !:
            balance=0 : let oldclass$="" : goto READHOURBREAKDOWN
00260     let key$=lpad$(str$(eno),8)&"             " !:
          restore #breakdown,key>=key$: nokey EOBREAKDOWN !:
          balance=0 : let oldclass$=""
00270 READHOURBREAKDOWN: let holdempno=empno: let oldclass$=class$: read #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",release: empno,class$,tdate,increase,decrease eof EOBREAKDOWN
00275     if hact$="[All]" then let empkey$=lpad$(str$(empno),8): goto L310
00290     if empno<>eno then goto EOBREAKDOWN
00300     let empkey$=lpad$(str$(eno),8)
00310 L310: let empname$="": read #prmstr,using "form pos 9,c 30",key=empkey$,release: empname$ nokey L320
00320 L320: if trim$(oldclass$)<>"" and oldclass$<>class$ then mat flxitm$=(""): balance=0: let fnflexadd1(mat flxitm$)
00321     if hact$="[All]" and holdempno<>empno then mat flxitm$=(""): balance=0: let fnflexadd1(mat flxitm$)
00330     balance+=increase-decrease
00340     classification$="": read #classification,using "form pos 6,c 30",key=class$,release: classification$ nokey L350
00350 L350: let flxitm$(1)=str$(rec(breakdown))
00360     let flxitm$(2)=str$(empno): let flxitm$(3)=empname$ !:
          let flxitm$(4)=classification$ : let flxitm$(5)=str$(tdate) !:
          let flxitm$(6)=str$(increase): let flxitm$(7)=str$(decrease) !:
          let flxitm$(8)=str$(balance)
00370     let fnflexadd1(mat flxitm$)
00380     goto READHOURBREAKDOWN
00390 EOBREAKDOWN: ! 
00395     if hact$="[All]" then let hact$=""
00400 !   let fnlbl(lc,85,'',0,1)
00410 !   let fnbutton(lc,32,"&Refresh",46,"",0,0,0,0,1) !:
          !   let fnbutton(lc,41,"&Add",43) !:
          !   let fnbutton(lc,46,"&Edit",45) !:
          !   let fnbutton(lc,52,"&Delete",44) !:
          !   let fnbutton(lc,60,"&Cancel",5)
00412 !   let fncmdkey("&Refresh",46,1) !:
          let fncmdkey("&Add",43) !:
          let fncmdkey("&Edit",45) !:
          let fncmdkey("&Delete",44) !:
          let fncmdkey("&Cancel",5,0,1)
00420     let fnacs(sn$,0,mat resp$,ck) !:
          if ck=5 then goto XIT
00430     let hact$=trim$(resp$(1)(1:8))
00440     if hact$="[All]" then goto MAIN
00450     let empno=eno=val(resp$(1)(1:8))
00460     let editrec=val(resp$(2)) ! record # if edit
00470     if ck=45 then let edithours=1 else let edithours=0
00480     if ck=43 then addhours=1 else addhours=0
00490     if ck=44 then goto MSGBOX1 ! delete a record
00500     if ck=46 then goto MAIN ! refresh grid
00510 ADDFM: ! add hours
00520     let holdeno=eno ! allow then to enter time on more than one employee while here, but warn them
00530     if empno=0 then let empno=eno ! assign to default employee if adding
00540     let empkey$=lpad$(str$(eno),8)
00550     let empname$="": read #prmstr,using "form pos 9,c 30",key=empkey$,release: empname$ nokey L560
00560 L560: if addhours=1 then class$="": let increase=decrease=0
00570     if edithours=1 then !:
            read #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",rec=editrec: empno,class$,tdate,increase,decrease norec ADD_FM_DONE
00580     let fntos(sn$="Addfm") !:
          let respc=0 : let lc=0 : let mylen=21 : let mypos=mylen+2: mat resp$=(""): let right=1
00590     let fnfra(1,9,8,70,"Hourly Information - "&empname$,"",0) : let frame1=1
00600     let fnlbl(lc+=1,1,'Employee Number:',mylen,right,0,frame1)
00610     let fncombof("PRmstr",lc,mypos,0,env$('Q')&"\PRmstr\rpmstr.h"&str$(cno),1,8,9,30,env$('Q')&"\PRmstr\Rpindex.h"&str$(cno),0,pas, "Enter the employee number to whom the time should be recorded",frame1) !:
          let resp$(1)=str$(empno)
00620     let fnlbl(lc+=1,1,'Classification:',mylen,right,0,frame1)
00630     let fncombof("Hours",lc,mypos,0,env$('Q')&"\PRmstr\Hourclass.h"&str$(cno),1,5,6,30,env$('Q')&"\PRmstr\Hourclass-idx.h"&str$(cno),0,pas, "Enter the proper classification of hours. If you need a new classification, you must add it under a different menu option",frame1) !:
          let resp$(2)=class$
00640     let fnlbl(lc+=1,1,'Date:',mylen,right,0,frame1)
00645     if addhours=1 then let tdate=0
00650     let fntxt(lc,mypos,10,0,right,'1003',0,"",frame1 ) !:
          let resp$(3)=str$(tdate)
00660     let fnlbl(lc+=1,1,'Increase:',mylen,right,0,frame1)
00670     let fntxt(lc,mypos,10,0,right,'32',0,"",frame1 ) !:
          let resp$(4)=str$(increase)
00680     let fnlbl(lc+=1,1,'Decrease:',mylen,right,0,frame1)
00690     let fntxt(lc,mypos,10,0,right,'32',0,"",frame1 ) !:
          let resp$(5)=str$(decrease)
00700     let fncmdset(4)
00710     let fnacs(sn$,0,mat resp$,ck) !:
          if ck=5 then goto MAIN
00720     let empno=val(resp$(1)(1:8)) !:
          class$=resp$(2)(1:5) !:
          let tdate=val(resp$(3)) !:
          let increase=val(resp$(4)) !:
          let decrease=val(resp$(5))
00730     if empno<>holdeno then goto MSGBOX2 ! attempting to enter time on different employee
00740 L740: if increase=0 and decrease=0 and addhours=1 then goto ADDFM !:
            ! .     ! do not add blank records
00750     if addhours=1 then !:
            write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": empno,class$,tdate,increase,decrease !:
            goto ADDFM
00760     if edithours=1 then !:
            rewrite #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",rec=editrec: empno,class$,tdate,increase,decrease !:
            let edithours=0: goto ADD_FM_DONE
00770 ADD_FM_DONE: goto MAIN
00780 MSGBOX1: ! delete this record?
00790     mat ml$(3) !:
          let ml$(1)="You have chosen to delete the "&classification$&" for " !:
          let ml$(2)="employee "&str$(empno)&". Click on Yes to delete the entry, else" !:
          let ml$(3)="No to return to the display screen" !:
          let fnmsgbox(mat ml$,resp$,cap$,52)
00800     if resp$="Yes" then goto L810 else goto MAIN
00810 L810: delete #breakdown,rec=editrec: 
00820     goto MAIN
00830 MSGBOX2: ! changing employees
00840     mat ml$(3) !:
          let ml$(1)="You are attempting to enter hours on a different employee." !:
          let ml$(2)="You were assigned to employee "&str$(holdeno)&"." !:
          let ml$(3)="Do you wish to change to employee "&str$(empno)&"?" !:
          let fnmsgbox(mat ml$,resp$,cap$,52)
00850     if resp$="Yes" then let eno=empno: goto L740 else let empno=holdeno: goto ADDFM
00860 MSGBOX3: ! set up classifications of time
00870     mat ml$(3) !:
          let ml$(1)="You must set up the classification file before you can use" !:
          let ml$(2)="this feature.  Go to Files on the main menu and then " !:
          let ml$(3)="take Time Classifications." !:
          let fnmsgbox(mat ml$,resp$,cap$,65)
00880     goto XIT
00890 ! ______________________________________________________________________
00900 ! <Updateable Region: ERTN>
00910 ERTN: let fnerror(program$,err,line,act$,"xit")
00920     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00930     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00940     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00950 ERTN_EXEC_ACT: execute act$ : goto ERTN
00960 ! /region
00970 ! ______________________________________________________________________
00980 IGNORE: continue 
00982 XIT: ! 
00990     close #31: ioerr ignore
01000     close #32: ioerr ignore
01010     close #33: ioerr ignore
01030   fnend 
01040 SETUP: ! 
01050   goto L1090 ! don't allow run to delete files
01060   open #breakdown=31: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno)&",RecL=39,KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno)&",kps=1/9/14,kln=8/5/8,replace",internal,outin,keyed 
01070   close #breakdown: 
01080   execute "Index "&env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno)&' '&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno)&" 1/9/14 8/5/8 Replace DupKeys"
01090 L1090: stop 
