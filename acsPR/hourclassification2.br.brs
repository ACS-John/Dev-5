00010 ! Replace S:\acsPR\hourclassification2
00020 ! enter and track houly breakdowns of time for comp time, etc
00030   def library fnhours(eno)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fntop,fnxit, fnwait,fncno,fnwin3,fnopenprn,fncloseprn,fntos,fnfra,fnlbl,fntxt,fnchk,fnflexinit1,fnflexadd1,fnbutton,fnacs,fnerror,fnmsgbox,fncmdset,fncombof
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim cnam$*40,cap$*128,message$*40,resp$(10)*40
00090     dim oldclass$*5,class$*5,classification$*30,impname$*25,empname$*30
00100     dim flxitm$(8)*30,key$*21
00110 ! ______________________________________________________________________
00120     let fncno(cno,cnam$)
00130 ! ______________________________________________________________________
00140 ! 
00150 ! 
00160     let fntop("S:\acsPR\hourclassification2",cap$="Time Classification")
00170     open #breakdown=1: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno)&",Shr",internal,outin,keyed 
00180     open #classification=2: "Name="&env$('Q')&"\PRmstr\HourClass.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\HourClass-idx.H"&str$(cno)&",Shr",internal,outin,keyed 
00190     open #prmstr=3: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00200 MAIN: ! 
00210     let addhours=edithours=0
00220     let fntos(sn$="Main") !:
          let respc=0 : let lc=0 : mat resp$=('') !:
          let mylen=20 : let mypos=mylen+2
00230     let fnlbl(lc+=1,1,'Employee Number:',mylen,1,0,0)
00240     let fncombof("PRmstr",lc,mypos,0,env$('Q')&"\PRmstr\rpmstr.h"&str$(cno),1,8,9,30,env$('Q')&"\PRmstr\Rpindex.h"&str$(cno),0,pas, "Enter the employee number you wish to work with.",0) !:
          let resp$(1)=str$(eno)
00250     mat chdr$(8) : mat cmask$(8) : mat flxitm$(8) !:
          let chdr$(1)="Ref #" : let chdr$(2)="Emp #" : let chdr$(3)="Name" : let chdr$(4)="Classification" !:
          let chdr$(5)="Date" : let chdr$(6)="Increase" !:
          let chdr$(7)="Decrease" : let chdr$(8)="Balance" !:
          let cmask$(5)='3' : let cmask$(6)='10' : let cmask$(7)="10" !:
          let cmask$(8)="10" !:
          let fnflexinit1('Hours',lc+2,1,15,120,mat chdr$,mat cmask$,1) !:
          let lc+=18
00260     let key$=lpad$(str$(eno),8)&"             " !:
          restore #breakdown,key>=key$: nokey EOBREAKDOWN !:
          let balance=0 : let oldclass$=""
00270 READHOURBREAKDOWN: let oldclass$=class$: read #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",release: empno,class$,tdate,increase,decrease eof EOBREAKDOWN
00280     if empno<>eno then goto EOBREAKDOWN
00290     let empkey$=lpad$(str$(eno),8)
00300     let empname$="": read #prmstr,using "form pos 9,c 30",key=empkey$,release: empname$ nokey L310
00310 L310: if trim$(oldclass$)<>"" and oldclass$<>class$ then mat flxitm$=(""): let balance=0: let fnflexadd1(mat flxitm$)
00315     let balance+=increase-decrease
00320     let classification$="": read #classification,using "form pos 6,c 30",key=class$,release: classification$ nokey L325
00325 L325: let flxitm$(1)=str$(rec(breakdown))
00330     let flxitm$(2)=str$(empno): let flxitm$(3)=empname$ !:
          let flxitm$(4)=classification$ : let flxitm$(5)=str$(tdate) !:
          let flxitm$(6)=str$(increase): let flxitm$(7)=str$(decrease) !:
          let flxitm$(8)=str$(balance)
00340     let fnflexadd1(mat flxitm$)
00350     goto READHOURBREAKDOWN
00360 EOBREAKDOWN: ! 
00370     let fnlbl(lc,100,'')
00380     let fnbutton(lc,37,"&Edit",45,"",0,0,0,0,1) !:
          let fnbutton(lc,44,"&Add",43) !:
          let fnbutton(lc,50,"&Refresh",46) !:
          let fnbutton(lc,60,"&Delete",44)
00390     let fnacs(sn$,0,mat resp$,ck) !:
          if ck=5 then goto XIT
00395     let eno=val(resp$(1)(1:8))
00400     let editrec=val(resp$(2)) ! record # if edit
00410     if ck=45 then let edithours=1 else let edithours=0
00420     if ck=43 then let addhours=1 else let addhours=0
00430     if ck=44 then goto MSGBOX1 ! delete a record
00435     if ck=46 then goto MAIN ! refresh grid
00440 ADDFM: ! add hours
00450     let holdeno=eno ! allow then to enter time on more than one employee while here, but warn them
00460     if empno=0 then let empno=eno ! assign to default employee if adding
00470     let empkey$=lpad$(str$(eno),8)
00480     let empname$="": read #prmstr,using "form pos 9,c 30",key=empkey$,release: empname$ nokey L490
00490 L490: if addhours=1 then let class$="": let increase=decrease=0
00500     if edithours=1 then !:
            read #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",rec=editrec: empno,class$,tdate,increase,decrease norec ADD_FM_DONE
00510     let fntos(sn$="Addfm") !:
          let respc=0 : let lc=0 : let mylen=21 : let mypos=mylen+2: mat resp$=(""): let right=1
00520     let fnfra(1,9,8,70,"Hourly Information - "&empname$,"",0) : let frame1=1
00530     let fnlbl(lc+=1,1,'Employee Number:',mylen,right,0,frame1)
00540     let fncombof("PRmstr",lc,mypos,0,env$('Q')&"\PRmstr\rpmstr.h"&str$(cno),1,8,9,30,env$('Q')&"\PRmstr\Rpindex.h"&str$(cno),0,pas, "Enter the employee number to whom the time should be recorded",frame1) !:
          let resp$(1)=str$(empno)
00550     let fnlbl(lc+=1,1,'Classification:',mylen,right,0,frame1)
00560     let fncombof("Hours",lc,mypos,0,env$('Q')&"\PRmstr\Hourclass.h"&str$(cno),1,5,6,30,env$('Q')&"\PRmstr\Hourclass-idx.h"&str$(cno),0,pas, "Enter the proper classification of hours. If you need a new classification, you must add it under a different menu option",frame1) !:
          let resp$(2)=class$
00570     let fnlbl(lc+=1,1,'Date:',mylen,right,0,frame1)
00580     let fntxt(lc,mypos,10,0,right,'3',0,"",frame1 ) !:
          let resp$(3)=str$(tdate)
00590     let fnlbl(lc+=1,1,'Increase:',mylen,right,0,frame1)
00600     let fntxt(lc,mypos,10,0,right,'32',0,"",frame1 ) !:
          let resp$(4)=str$(increase)
00610     let fnlbl(lc+=1,1,'Decrease:',mylen,right,0,frame1)
00620     let fntxt(lc,mypos,10,0,right,'32',0,"",frame1 ) !:
          let resp$(5)=str$(decrease)
00630     let fncmdset(4)
00640     let fnacs(sn$,0,mat resp$,ck) !:
          if ck=5 then goto MAIN
00650     let empno=val(resp$(1)(1:8)) !:
          let class$=resp$(2)(1:5) !:
          let tdate=val(resp$(3)) !:
          let increase=val(resp$(4)) !:
          let decrease=val(resp$(5))
00660     if empno<>holdeno then goto MSGBOX2 ! attempting to enter time on different employee
00670 L670: if increase=0 and decrease=0 and addhours=1 then goto ADDFM !:
            ! do not add blank records
00680     if addhours=1 then !:
            write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": empno,class$,tdate,increase,decrease !:
            goto ADDFM
00690     if edithours=1 then !:
            rewrite #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",rec=editrec: empno,class$,tdate,increase,decrease !:
            let edithours=0: goto ADD_FM_DONE
00700 ADD_FM_DONE: goto MAIN
00720 MSGBOX1: ! delete this record?
00730     mat ml$(3) !:
          let ml$(1)="You have chosen to delete the "&classification$&" for " !:
          let ml$(2)="employee "&str$(empnum)&". Click on Yes to delete the entry, eles" !:
          let ml$(3)="No to return to the display screen" !:
          let fnmsgbox(mat ml$,resp$,cap$,52)
00740     if resp$="Yes" then goto L750 else goto MAIN
00750 L750: delete #1,rec=editrec: 
00760     goto MAIN
00770 MSGBOX2: ! changing employees
00780     mat ml$(3) !:
          let ml$(1)="You are attempting to enter hours on a different employee." !:
          let ml$(2)="You were assigned to employee "&str$(holdeno)&"." !:
          let ml$(3)="Do you wish to change to employee "&str$(empno)&"?" !:
          let fnmsgbox(mat ml$,resp$,cap$,52)
00790     if resp$="Yes" then let eno=empno: goto L670 else let empno=holdeno: goto ADDFM
00800 ! ______________________________________________________________________
00810 ! <Updateable Region: ERTN>
00820 ERTN: let fnerror(program$,err,line,act$,"xit")
00830     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00840     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00850     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00860 ERTN_EXEC_ACT: execute act$ : goto ERTN
00870 ! /region
00880 ! ______________________________________________________________________
00881 XIT: fnend 
00890 SETUP: ! 
00900   open #breakdown=1: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno)&",RecL=39,KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno)&",kps=1,kln=5,replace",internal,outin,keyed 
00910   close #breakdown: 
00920   execute "Index "&env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno)&' '&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno)&" 1/9/14 8/5/8 Replace DupKeys"
00930   stop 
