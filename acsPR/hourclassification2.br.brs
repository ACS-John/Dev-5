00010 ! Replace S:\acsPR\hourclassification2
00020 ! enter and track houly breakdowns of time for comp time, etc
00030   def library fnhours(eno)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fntop,fnxit, fnwin3,fnopenprn,fncloseprn,fnTos,fnFra,fnLbl,fnTxt,fnChk,fnflexinit1,fnflexadd1,fnButton,fnAcs,fnerror,fnmsgbox,fnCmdSet,fncombof
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim cap$*128,message$*40,resp$(10)*40
00090     dim oldclass$*5,class$*5,classification$*30,impname$*25,empname$*30
00100     dim flxitm$(8)*30,key$*21
00110 ! ______________________________________________________________________
00140 ! 
00150 ! 
00160     fntop(program$,cap$="Time Classification")
00170     open #breakdown=1: "Name=[Q]\PRmstr\HourBreakdown.H[cno],KFName=[Q]\PRmstr\HourBreakdown-idx.H[cno],Shr",internal,outIn,keyed 
00180     open #classification=2: "Name=[Q]\PRmstr\HourClass.H[cno],KFName=[Q]\PRmstr\HourClass-idx.H[cno],Shr",internal,outIn,keyed 
00190     open #prmstr=3: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,input,keyed 
00200 MAIN: ! 
00210     addhours=edithours=0
00220     fnTos(sn$="Main") !:
          respc=0 : lc=0 : mat resp$=('') !:
          mylen=20 : mypos=mylen+2
00230     fnLbl(lc+=1,1,'Employee Number:',mylen,1,0,0)
00240     fncombof("PRmstr",lc,mypos,0,"[Q]\PRmstr\rpmstr.h[cno]",1,8,9,30,"[Q]\PRmstr\Rpindex.h[cno]",0,pas, "Enter the employee number you wish to work with.",0) !:
          resp$(1)=str$(eno)
00250     mat chdr$(8) : mat cmask$(8) : mat flxitm$(8) !:
          chdr$(1)="Ref #" : chdr$(2)="Emp #" : chdr$(3)="Name" : chdr$(4)="Classification" !:
          chdr$(5)="Date" : chdr$(6)="Increase" !:
          chdr$(7)="Decrease" : chdr$(8)="Balance" !:
          cmask$(5)='3' : cmask$(6)='10' : cmask$(7)="10" !:
          cmask$(8)="10" !:
          fnflexinit1('Hours',lc+2,1,15,120,mat chdr$,mat cmask$,1) !:
          lc+=18
00260     key$=lpad$(str$(eno),8)&"             " !:
          restore #breakdown,key>=key$: nokey EOBREAKDOWN !:
          balance=0 : oldclass$=""
00270 READHOURBREAKDOWN: oldclass$=class$: read #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",release: empno,class$,tdate,increase,decrease eof EOBREAKDOWN
00280     if empno<>eno then goto EOBREAKDOWN
00290     empkey$=lpad$(str$(eno),8)
00300     empname$="": read #prmstr,using "form pos 9,c 30",key=empkey$,release: empname$ nokey L310
00310 L310: if trim$(oldclass$)<>"" and oldclass$<>class$ then mat flxitm$=(""): balance=0: fnflexadd1(mat flxitm$)
00315     balance+=increase-decrease
00320     classification$="": read #classification,using "form pos 6,c 30",key=class$,release: classification$ nokey L325
00325 L325: flxitm$(1)=str$(rec(breakdown))
00330     flxitm$(2)=str$(empno): flxitm$(3)=empname$ !:
          flxitm$(4)=classification$ : flxitm$(5)=str$(tdate) !:
          flxitm$(6)=str$(increase): flxitm$(7)=str$(decrease) !:
          flxitm$(8)=str$(balance)
00340     fnflexadd1(mat flxitm$)
00350     goto READHOURBREAKDOWN
00360 EOBREAKDOWN: ! 
00370     fnLbl(lc,100,'')
00380     fnButton(lc,37,"&Edit",45,"",0,0,0,0,1) !:
          fnButton(lc,44,"&Add",43) !:
          fnButton(lc,50,"&Refresh",46) !:
          fnButton(lc,60,"&Delete",44)
00390     fnAcs(sn$,0,mat resp$,ck) !:
          if ck=5 then goto XIT
00395     eno=val(resp$(1)(1:8))
00400     editrec=val(resp$(2)) ! record # if edit
00410     if ck=45 then edithours=1 else edithours=0
00420     if ck=43 then addhours=1 else addhours=0
00430     if ck=44 then goto MSGBOX1 ! delete a record
00435     if ck=46 then goto MAIN ! refresh grid
00440 ADDFM: ! add hours
00450     holdeno=eno ! allow then to enter time on more than one employee while here, but warn them
00460     if empno=0 then empno=eno ! assign to default employee if adding
00470     empkey$=lpad$(str$(eno),8)
00480     empname$="": read #prmstr,using "form pos 9,c 30",key=empkey$,release: empname$ nokey L490
00490 L490: if addhours=1 then class$="": increase=decrease=0
00500     if edithours=1 then !:
            read #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",rec=editrec: empno,class$,tdate,increase,decrease noRec ADD_FM_DONE
00510     fnTos(sn$="Addfm") !:
          respc=0 : lc=0 : mylen=21 : mypos=mylen+2: mat resp$=(""): right=1
00520     fnFra(1,9,8,70,"Hourly Information - "&empname$,"",0) : frame1=1
00530     fnLbl(lc+=1,1,'Employee Number:',mylen,right,0,frame1)
00540     fncombof("PRmstr",lc,mypos,0,"[Q]\PRmstr\rpmstr.h[cno]",1,8,9,30,"[Q]\PRmstr\Rpindex.h[cno]",0,pas, "Enter the employee number to whom the time should be recorded",frame1) !:
          resp$(1)=str$(empno)
00550     fnLbl(lc+=1,1,'Classification:',mylen,right,0,frame1)
00560     fncombof("Hours",lc,mypos,0,"[Q]\PRmstr\Hourclass.h[cno]",1,5,6,30,"[Q]\PRmstr\Hourclass-idx.h[cno]",0,pas, "Enter the proper classification of hours. If you need a new classification, you must add it under a different menu option",frame1) !:
          resp$(2)=class$
00570     fnLbl(lc+=1,1,'Date:',mylen,right,0,frame1)
00580     fnTxt(lc,mypos,10,0,right,'3',0,"",frame1 ) !:
          resp$(3)=str$(tdate)
00590     fnLbl(lc+=1,1,'Increase:',mylen,right,0,frame1)
00600     fnTxt(lc,mypos,10,0,right,'32',0,"",frame1 ) !:
          resp$(4)=str$(increase)
00610     fnLbl(lc+=1,1,'Decrease:',mylen,right,0,frame1)
00620     fnTxt(lc,mypos,10,0,right,'32',0,"",frame1 ) !:
          resp$(5)=str$(decrease)
00630     fnCmdSet(4)
00640     fnAcs(sn$,0,mat resp$,ck) !:
          if ck=5 then goto MAIN
00650     empno=val(resp$(1)(1:8)) !:
          class$=resp$(2)(1:5) !:
          tdate=val(resp$(3)) !:
          increase=val(resp$(4)) !:
          decrease=val(resp$(5))
00660     if empno<>holdeno then goto MSGBOX2 ! attempting to enter time on different employee
00670 L670: if increase=0 and decrease=0 and addhours=1 then goto ADDFM !:
            ! do not add blank records
00680     if addhours=1 then !:
            write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": empno,class$,tdate,increase,decrease !:
            goto ADDFM
00690     if edithours=1 then !:
            rewrite #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",rec=editrec: empno,class$,tdate,increase,decrease !:
            edithours=0: goto ADD_FM_DONE
00700 ADD_FM_DONE: goto MAIN
00720 MSGBOX1: ! delete this record?
00730     mat ml$(3) !:
          ml$(1)="You have chosen to delete the "&classification$&" for " !:
          ml$(2)="employee "&str$(empnum)&". Click on Yes to delete the entry, eles" !:
          ml$(3)="No to return to the display screen" !:
          fnmsgbox(mat ml$,resp$,cap$,52)
00740     if resp$="Yes" then goto L750 else goto MAIN
00750 L750: delete #1,rec=editrec: 
00760     goto MAIN
00770 MSGBOX2: ! changing employees
00780     mat ml$(3) !:
          ml$(1)="You are attempting to enter hours on a different employee." !:
          ml$(2)="You were assigned to employee "&str$(holdeno)&"." !:
          ml$(3)="Do you wish to change to employee "&str$(empno)&"?" !:
          fnmsgbox(mat ml$,resp$,cap$,52)
00790     if resp$="Yes" then eno=empno: goto L670 else empno=holdeno: goto ADDFM
00800 ! ______________________________________________________________________
00810 ! <Updateable Region: ERTN>
00820 ERTN: fnerror(program$,err,line,act$,"xit")
00830     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00840     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00850     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00860 ERTN_EXEC_ACT: execute act$ : goto ERTN
00870 ! /region
00880 ! ______________________________________________________________________
00881 XIT: fnend 
00890 SETUP: ! 
00900   open #breakdown=1: "Name=[Q]\PRmstr\HourBreakdown.H[cno],RecL=39,KFName=[Q]\PRmstr\HourBreakdown-idx.H[cno],kps=1,kln=5,replace",internal,outIn,keyed 
00910   close #breakdown: 
00920   execute "Index [Q]\PRmstr\HourBreakdown.H[cno]"&' '&"[Q]\PRmstr\HourBreakdown-idx.H[cno] 1/9/14 8/5/8 Replace DupKeys"
00930   stop 
