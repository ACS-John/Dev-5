00050 ! PROPERTY OF ADVANCED INFORMONICS CORPORATION - CONCORD, MA
00052 ! replace fnsnap.dll/vol002
00054 ! IBRARY ENV$("PD")&"APSYSC\FNSNAP.DLL":
00200   def library fninit(;sysdir$,sys$) !:
          ! +----------------------------------------------------------------+!:
          ! | Initializes variables for all functins in this library        | !:
          ! | If run more than once it may overwrite updated variables      | !:
          ! +----------------------------------------------------------------+!
00210     datfmt$="MM-DD-CCYY" !:
          maxsrows=22 : ssav=106 !:
          windev=owindev=69 : mga$="24,2,c 78," !:
          mgac$="24,2,CC 78," : pfk=23 !:
          ! Common Variables Almost Always Required By Fnsnap
00213     library env$("PD")&"WORKMENU.br": fnmenu
00214     library release,env$("PD")&"Core\fnsnap\FNPRTMAT.DLL": fnprtmat
00215     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprtpag,fnpfkey,fnprtpickbar,fnpm,fnsetall,fnsetsel,fnwin,fnkeysel_ex,fnzero,fnsavpart,fnauto,fnclswin,fnrelpart,fnzlpad$,fndialog$,fnsize,fnpick,fnerrtrap,fngetk$,fnclkbuf,fnbutton,fnclrbutton,fnnokey,fnok,fnpickwin,fntype
00220     pgup=90 : pgdn=91 : esc=99 !:
          esc$=chr$(27) !:
          up=102 : left=103 : dn=104
00222     upfld=105 : dnfld=106 : foflow=107 !:
          home=112 : end=113 : fldplus=114 !:
          fldminus=115
00225     if file(windev)>-1 then 
00226       windev+=1 !:
            goto 225
00227     end if 
00250   fnend 
00500   def library fnwindev !:
          ! +----------------------------------------------------------------+!:
          ! | Returns the current FNSNAP window number to calling program   | !:
          ! | Generally not used after BR4.16                               | !:
          ! +----------------------------------------------------------------+!
00510     fnwindev=windev
00520   fnend 
00600   def library fnprintscreen !:
          !    !:
          ! | Stuffs the KEYBOARD to create a printscreen                 | !:
          ! |                                                             | !:
          !    !
00610     msg("KB","|CTRL+|P|CTRL-|")
00620   fnend 
00640   def library fnprintdir$*100 !:
          !    !:
          ! | Returns the location name of the PRINTDIR                   | !:
          ! |                                                             | !:
          !    !
00641     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnstatus$
00642     fnprintdir$=fnstatus$("PRINTDIR")
00643   fnend 
00645   def library fnstatus$*100(_a$) !:
          !    !:
          ! | Returns the valu of any STATUS CONFIG statement specified   | !:
          ! | as the value of _A$                                         | !:
          !    !
00650     dim config$(1)*100
00660     env$("STATUS",mat config$,_a$) !:
          fnstatus$=""
00670     for _a =1 to udim(mat config$)
00680       if config$(_a)(1:len(_a$))=_a$ then !:
              fnstatus$=trim$(srep$(config$(_a),_a$,"")) !:
              goto 700
00690     next _a
00700   fnend 
00710   def library fnenv !:
          !    !:
          ! | Display Environmental variables in a GRID window and allow  | !:
          ! | printing of the displayed values                            | !:
          !    !
00715     if not env then execute "PROC "&env$("PD")&"Core\fnsnap\tt"
00720     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnstatus$,fnwinrowcol,fnlistspec$,fnsrchcrit$,fnwinbuttons
00725     library env$("PD")&"Core\fnsnap\RTFLIB.DLL": fnrtf,fnamt$,fntext$,fnlistprint
00730     dim config$(1)*100,header$(1)*100,forms$(1)*100,widths(1),srchstring$*50
00735     env$("STATUS",mat config$,"")
00740     ad=udim(mat config$)
00745     mat headers$(1) !:
          headers$(1)="Environment"
00750     mat widths(1) !:
          widths(1)=100
00755     mat forms$(1) !:
          forms$(1)="V 100"
00760     fnwinrowcol(0,lrow,lcol) ! function is in S:\Core\fnsnap\NEWSCRN.DLL
00765     arows=0 : srow=4 : scol=3 !:
          lcols=min(lcol-srow-2,sum(mat widths)+2) !:
          lrows=min(lrow-srow+2,max(3,ad+2+arows))
00775     listspec$=fnlistspec$(listwin,srow,scol,lrows,lcols,arows,mat headers$,mat widths,mat forms$,"Select Line",grid$,parent:=0,head:=2) !:
          ! add ,GRID$ after "select Invoice" to make a grid where grid$="GRID"
00777     fnwinbuttons(-1,"^F2:Filter ^F12:Print ^ESC:End",listwin)
00780     pr #listwin, fields listspec$&",=R" : (mat config$)
00785     mat select(1)=(0) !:
          curfld(1,max(1,select(1)))
00790     input #listwin, fields listspec$&",rowsub,sel ": ax !:
          fk=fkey !:
          if fk=201 then fk=0
00795     if fk=12 and wbversion$>"4.30" then let fnlistprint(listwin,listspec$,"Environmental Information [BOTLINE]","Status","Environmental Variables ",mat dummy,nolines:=1,nosort:=0,nototals$=rpt$("0",udim(headings$)),nosubtotals:=0) !:
            goto 790
00800     if fk=2 then 
00805       if fk=2 and wbversion$=>"4.30" then rinput #listwin, fields str$(lrows+1)&",2,15/FILTER 50/10,[D],1,1,FULLROW,ALL": srchstring$ : cx=1 : goto 790
00810       srchstr$=fnsrchcrit$(str$(round(lrows/2,0)),"2",2,lcols-4,listwin,"Item to search for") !:
            mat select(1)=(0) !:
            ! fnLISTSRCH(MAT PDINVNR$,SRCHSTR$,MAT SELECT) !:
            ! fnLISTSRCHN(MAT PDPAID,SRCHSTR$,MAT SELECT) !:
            ! Turn these on to search for stings in matrixes
00815       if select(1)=0 and udim(select)=1 then !:
              msgbox("No matching criterea","Search Results","OK","EXCL") !:
              goto 790 !:
            else !:
              ff=1 : curfld(1,select(ff)) : goto 790
00820     end if 
00825     if fk=3 then 
00830       if ff<udim(mat select) then ff+=1 else ff=1
00835       curfld(1,select(ff)) !:
            goto 790
00840     end if 
00845     if not (fk=0 or fk=esc or fk=close) then goto 790
00850     close #listwin: !:
          listwin=0
00855   fnend 
01000 ! ---------------------------------
01010   def library fnsize(flnm$*100,fllen) ! Function to change file size !:
          ! +----------------------------------------------------------------+!:
          ! | Used in install program to update file size based on passed   | !:
          ! | parameters                                                    | !:
          ! +----------------------------------------------------------------+!
01012     execute "copy "&flnm$&" work.tmp -D -"&str$(fllen)&" -N"
01014     execute "free "&flnm$&" -N"
01016     execute "copy work.tmp "&flnm$&" -N"
01018     execute "free work.tmp -N"
01020     fnsize=fllen
01022   fnend 
01028   def library fnverone(flnm$*100) ! Function to change verson !:
          ! +----------------------------------------------------------------+!:
          ! | Used in install program to update file size based on passed   | !:
          ! | parameters                                                    | !:
          ! +----------------------------------------------------------------+!
01030     dim f$*32000
01031     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle
01032     execute "copy "&flnm$&" work"&session$&".tmp -D -N"
01034     open #(flin:=fngethandle): "name=work"&session$&".tmp",internal,outin 
01036     execute "free "&flnm$&" -N"
01038     open #(flout:=fngethandle): "NAME="&flnm$&",recl="&str$(rln(flin))&",replace,version=1",internal,output 
01040     flfrm$=cform$("FORM C "&str$(rln(flin)))
01042     read #flin,using flfrm$: f$ eof 1050
01044     write #flout,using flfrm$: f$
01046     goto 1042
01050     close #flin,free: !:
          flin=0
01052     close #flout: !:
          flout=0
01054   fnend 
01070 ! -----------------------------------
01080   def library fnfileok(number,name$*100,length,file_version;&old_version) !:
          ! +----------------------------------------------------------------+!:
          ! | Checks a file size and version and if it does not exist       | !:
          ! | creates it.  Used in installing and updating systems          | !:
          ! +----------------------------------------------------------------+!
01082     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnsize,fnverone
01090 ! 0 = starting point !:
          ! 1 = new file was created !:
          ! 2 = record length was adjusted, but versions matched !:
          ! 3 = versions do not match existing version is newer  !:
          ! 4 = versions do not match existing version is older
01100     pr newpage
01110     pr f "10,10,c 60,n/W:T": "Processing file #"&str$(number)&"  "&name$
01120     fnfileok=0
01130     open #number: "NAME="&name$,internal,outin ioerr NEWFILE
01132     if version(number)=0 then close #number: !:
            fnverone(name$) !:
            goto 1130
01135     old_version=version(number)
01136     if file_version<old_version then let fnfileok=3
01138     if file_version>old_version then let fnfileok=4
01139     old_length=rln(number)
01140     if rln(number)=length and old_version=file_version then goto FILEOK
01150     pr f "12,1,CC 79,n/W:T": "Record length is "&str$(rln(number))&" it should be "&str$(length)
01152     pr f "14,1,CC 79,n/W:t": "File version is "&str$(old_version)&" it should be "&str$(file_version)
01160     close #number: 
01170     fnsize(name$,length)
01180     open #number: "NAME="&name$,internal,outin ioerr NEWFILE
01190     if file_version=version(number) then let fnfileok=2
01200 FILEOK: ! 
01202     if file(number)>-1 then close #number: 
01204     fnsize(name$,max(length,old_length))
01210     goto ZFILEOK
01220 NEWFILE: fnfileok=1
01230     open #number: "NAME="&name$&",RECL="&str$(length)&",new,version="&str$(max(1,file_version)),internal,output ioerr ZFILEOK
01240     goto ZFILEOK
01250 ZFILEOK: close #number: ioerr 1260
01260   fnend 
01300   def library fntimeout(;seconds) !:
          ! +----------------------------------------------------------------+!:
          ! | Displays a box indicating that a timeout has occured          | !:
          ! |                                                               | !:
          ! +----------------------------------------------------------------+!
01310     if not seconds then seconds=10
01320     if env$("guimode")="ON" then let msgbox("You have waited more than "&str$(seconds)&" seconds to make an entry.  The system has timed out.","Time Out","OK","EXCL") else let fndialog$("16","5",70,"You have waited more than "&str$(seconds)&" seconds to make an entry.  The system has timed out.","","","",1,1,1,1)
01330   fnend 
01350   def library fncurdrv$*100 !:
          !    !:
          ! | Returns the current drive and path                          | !:
          ! |                                                             | !:
          !    !
01370     curdrv=10
01372     dim curdrv$*100
01375     if file(curdrv)>-1 then curdrv+=1: goto 1375
01380     open #curdrv: "name=drv."&wsid$&",replace",display,output 
01385     curdrv$=srep$(file$(curdrv),"drv."&wsid$,"")
01392     fncurdrv$=curdrv$
01395     close #curdrv,free: 
01400   fnend 
01420   def library fnrtfstart(header$*300,footer$*300,title$*500,mat header$;cellno) !:
          !    !:
          ! | Open a source file to prepare to build and RTF file using   | !:
          ! | FNRTF from RTFLIB.dll - also uses FNRTFEND                  | !:
          !    !
01430     outfile=10
01440     if file(outfile)>-1 then outfile+=1 : goto 1440
01450     open #outfile: "name=out"&session$&".txt,eol=none,replace",display,output 
01460     crlf$=chr$(13)&chr$(10)
01470     if header$>"" then pr #outfile: "H|"&header$&crlf$
01480     if footer$>"" then pr #outfile: "F|"&footer$&crlf$
01490     if title$>"" then pr #outfile: "T|"&title$&crlf$ : !:
            pr #outfile: "D| "&crlf$
01500     cellno$=cnvrt$("N 1",max(1,cellno))
01505     if cellno>0 then 
01510       for a=1 to udim(mat header$)
01520         if a=1 then !:
                pr #outfile: trim$(cellno$)&"|"&header$(a)&"|" else !:
                pr #outfile: header$(a)&"|"
01530       next a
01540       pr #outfile: crlf$
01545     end if 
01550     fnrtfstart=outfile
01560   fnend 
01580   def library fnrtfend$*100(rtfno,rtfname$*100,rtfspec$*100;word) !:
          !    !:
          ! | Turn a source file into a finished RTF file using FNRTF     | !:
          ! | from RTFLIB.dll                                             | !:
          !    !
01585 ! RTFNO       Number of already open source txt file for the RTF file !:
          ! RTFNAME$    The name of the RTF file to be created !:
          ! RTFSPEC$    The name of the specification file for creating the RTF file!:
          ! WORD        If true WORD will be called to view the file
01587     library env$("PD")&"Core\fnsnap\rtflib.dll": fnrtf
01588     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fn_cs,fnCopys2c
01589     _cs=fn_cs
01590     dim rtfno$*100,rtfout$*100,csout$*100
01595     rtfno$=file$(rtfno) !:
          close #rtfno: 
01600     open #rtfno: "name="&rtfno$,display,input 
01605     rtfout=rtfno+1
01610     if file(rtfout)>-1 then rtfout+=1 : goto 1610
01615     open #rtfout: "name="&rtfname$&",eol=none,replace",display,output 
01620     fnrtf(rtfno,rtfspec$,rtfout)
01625     rtfout$=file$(rtfout)
01630     close #rtfout: !:
          close #rtfno: 
01635     if word and _cs then !:
            fnCopys2c(rtfout$,csout$:="@:"&env$("C_DIR")&"temp\temp"&session$&".rtf",1) !:
            execute "sys  -w -c "&fnmsexe$("winword.exe")&" "&os_filename$(csout$) else !:
            if word then execute "sys  -w -c "&fnmsexe$("winword.exe")&" "&os_filename$(rtfout$)
01640     fnrtfend$=rtfout$
01645   fnend 
01700   def library fnreference(ptype$,reference$;pfile,lgl) !:
          ! +----------------------------------------------------------------+!:
          ! | Prints a file reference in the bottom right corner of a report| !:
          ! | Used for indexing printed reports                             | !:
          ! +----------------------------------------------------------------+!
01710 ! PTYPE$ =  printer - HP is designated by first two characters of name !:
          ! REFERENCE = Page reference to place on workpaper !:
          ! PFILE  = Printer file reference if different from 255 (0=255 by default)
01720     if not pfile then pfile=255
01730     if uprc$(ptype$(1:2))="HP" then 
01732       if lgl then pr #pfile,using "FORM C 100,SKIP 0": chr$(27)&"&a9360v4950H "&reference$&chr$(27)&"&a0002v0002H" else pr #pfile,using "FORM C 100,SKIP 0": chr$(27)&"&a7360v4950H "&reference$&chr$(27)&"&a0002v0002H"
01738     end if 
01740   fnend 
01799 ! ----------------------------
01800   def library fndatefwd(datein;century) !:
          ! +----------------------------------------------------------------+!:
          ! | Convert YYMMDD to MMDDYY with optional addition of century    | !:
          ! | MMDDCCYY if century >0                                        | !:
          ! +----------------------------------------------------------------+!
01810     if century then let fndatefwd=date(days(datein,"yymmdd"),"mmddccyy") else let fndatefwd=date(days(datein,"yymmdd"),"mmddyy")
01820   fnend 
01849 ! ----------------------------
01850   def library fndaterev(datein;century) !:
          ! +----------------------------------------------------------------+!:
          ! |  Convert MMDDYY to YYMMDD with optional addition of century   | !:
          ! |  MMDDCCYY to YYMMDD if century >0                             | !:
          ! +----------------------------------------------------------------+!
01860     if century then let fndaterev=date(days(datein,"mmddccyy"),"yymmdd") else let fndaterev=date(days(datein,"mmddyy"),"yymmdd")
01870   fnend 
01879 ! ----------------------------
01880   def library fnmdy2ymd(datein;century) !:
          ! +----------------------------------------------------------------+!:
          ! | Convert MDY date to YMD format If CENTURY is true then        | !:
          ! ³the Y is expected to be CY                                     | !:
          ! +----------------------------------------------------------------+!
01890     if century then let fnmdy2ymd=date(days(datein,"mmddccyy"),"yymmdd") else let fnmdy2ymd=date(days(datein,"mmddyy"),"yymmdd")
01900   fnend 
01909 ! ----------------------------
01910   def library fnymd2mdy(datein;century) !:
          ! +----------------------------------------------------------------+!:
          ! |  Convert YMD to MDY format.  If CENTURY is true               | !:
          ! |  then the Y is expected to be CY                              | !:
          ! +----------------------------------------------------------------+!
01920     if century then let fnymd2mdy=date(days(datein,"yymmdd"),"mmddccyy") else let fnymd2mdy=date(days(datein,"yymmdd"),"mmddyy")
01930   fnend 
02000   def library fndate$(daysin) !:
          ! +----------------------------------------------------------------+!:
          ! | Converts DAYS to a formatted date with words                  | !:
          ! |                                                               | !:
          ! +----------------------------------------------------------------+!
02010     dim months$(12)*15
02020     months$(1)="January" !:
          months$(2)="February" !:
          months$(3)="March"
02021     months$(4)="April" !:
          months$(5)="May" !:
          months$(6)="June"
02022     months$(7)="July" !:
          months$(8)="August" !:
          months$(9)="September"
02023     months$(10)="October" !:
          months$(11)="November" !:
          months$(12)="December"
02030     m=date(daysin,"MM")
02032     d=date(daysin,"dd")
02034     y=date(daysin,"ccyy")
02036     fndate$=months$(m)&" "&str$(d)&", "&str$(y)
02040   fnend 
02100 ! --------------------------------
02110   def library fnbldsort(innm$*100,outnm$*100,abr$,mask$*128;mat record$,_indir$,_indrv$,_outdir$,_outdrv$) !:
          !    !:
          ! | Build a sort control file, then sort the file and erase     | !:
          ! | the sort control file.                                      | !:
          !    !
02120 ! --------------------------------
02130 ! INNM$       File name to be sorted, can include path  !:
          ! OUTNM$      File name top be created by sort, can include path  !:
          ! ABR$        Type of output file !:
          !                A=Address in PD3 format !:
          !                B=Address in B4 format  !:
          !                R=Record out (the default)
02140 ! MASK$       The mask statement for the sort excluding the word "MASK" !:
          !  ex "123,1,C,A"                                                       !:
          ! Mat RECORD$ An array containing the Inlude and/or Exclude statements  !:
          !  ex rECORD$(1)='I,162,1,C,"""&low$&""","""&high$&"""'             !:
          ! INDIR$,INDRV$,OUTDIR$ and OUTDRV$ are intended to NOT be included     !:
          !             and will therefore be set to NULL                         !
02150     dim srtcnt$*100
02160     srtcnt=10
02170     if file(srtcnt)>-1 then srtcnt+=1 : goto 2170
02180     open #srtcnt: "name="&env$("TEMP")&"\srtcnt."&session$&",recl=128,replace",internal,output 
02190     srtcnt$=file$(srtcnt)
02200     goto 2220
02210     for a=1 to 4 !:
            write #srtcnt,using "form C 128": "!"&rpt$(" ",126)&"!" !:
          next a
02220     write #srtcnt,using "form c 128": "FILE "&innm$&","&_indir$&","&_indrv$&","&outnm$&","&_outdir$&","&_outdrv$&","&env$("TEMP")&",,"&abr$&",N,REPLACE"
02230     for a=1 to udim(record$)
02240       if trim$(record$(a))>"" then !:
              write #srtcnt,using "form c 128": "RECORD "&record$(a) !:
              ! I,49,1,C,"""&" "&""","""&" "&""""
02250     next a
02260     write #srtcnt,using "form c 128": "MASK "&mask$ !:
          ! 11,5,C,A,23,3,C,A,26,4,PD,A,16,7,C,A"
02270     close #srtcnt: ! 
02280     execute "SORT "&srtcnt$
02290 ! FRMSRT: FORM PD 3
02300     execute "free "&srtcnt$ ioerr 2310
02310   fnend 
02350   def library fnmap(street$*100,city$*50,state$,zip$) !:
          !    !:
          ! | Load explorer and accesses GOOGLE MAPs for a location       | !:
          ! |                                                             | !:
          !    !
02360     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$
02370     dim data$*2000
02380     if trim$(street$)>"" then data$=srep$(street$," ","+")&","
02390     if trim$(city$)>"" then data$=data$&srep$(trim$(city$)," ","+")&","
02400     if trim$(state$)>"" then data$=data$&srep$(trim$(state$)," ","+")&","
02410     if trim$(zip$)>"" then data$=data$&srep$(trim$(zip$)," ","+")&","
02420     data$=data$(1:len(data$)-1)
02430     execute "sys -w "&fnmsexe$("iexplore.exe")&' http://maps.google.com/maps?q='&data$
02440   fnend 
02499 ! ----------------------------
02500   def library fnindex(flnr,flnm$*100,kfnm$*100,ks$,kl$,dups,kfmsg$*200) !:
          ! +----------------------------------------------------------------+!:
          ! | Creates an INDEX file based on passed parameters and if       | !:
          ! | duplicate kes are found prints a report to screen showing recs| !:
          ! | Allows deletion of duplicate keys by removing the newest      | !:
          ! +----------------------------------------------------------------+!
02510 ! flnr=file number !:
          ! ks$=STRING FOR KEY STARTING POSITION !:
          ! KL$=STRING FOR KEY LENGHTHS !:
          ! DUPS 0=NO DUPLICATE KEYS ALLOWED !:
          !      1=DUPLICATE KEYS ALLOWED!:
          ! KFMSMG$=MESSAGE TO DISPLAY WHILE INDEX IS BEING BUILT
02520     pr #0,fields "10,10,cc 70,N/W:T": kfmsg$
02525     execute "free ldup.[wsid] -N" ioerr 2529
02529     if not dups then 
02531       execute "INDEX "&flnm$&" "&kfnm$&" "&ks$&" "&kl$&" -"&env$("TEMP")&" LISTDUPKEYS >LDUP.[WSID] REPLACE -N" duprec 2550 locked LOCKERR ioerr 2550
02532     else 
02534       execute "INDEX "&flnm$&" "&kfnm$&" "&ks$&" "&kl$&" DUPKEYS REPLACE -N" locked LOCKERR ioerr 2550
02536     end if 
02540     fnindex=1 : goto 2580
02550     fnindex=0: gosub DELDUPS
02555     if dupa=2 then let fnindex=2: goto 2580
02560     if dupa=3 then let fnindex=3: goto 2580
02570     goto 2529
02580   fnend 
02600 ! ----------------------------
02610   def library fnupdate_version(filename$,dirname$,mat versions,dircopy$,oldform$*500,newform$*500;lastrec,delete_lastrec) !:
          ! +----------------------------------------------------------------+!:
          ! |  Updates a specified file based on FORM statement passed      | !:
          ! |                                                               | !:
          ! +----------------------------------------------------------------+!
02620     dim a$(2)*400,a(2)
02630     old=1 !:
          new=2 !:
          ver=1 !:
          str=2 !:
          num=3
02640     fnupdate_version=0
02650     if not exists("dircopy$") then execute "MKDIR "&dircopy$ ioerr 2655
02655     execute "copy "&dirname$&"\"&filename$&" "&dircopy$&"\"&filename$&" -n"
02660     open #120: "name="&dirname$&"\"&filename$&",version="&str$(versions(old,ver)),internal,outin ioerr WRONG_VERSION
02670     if version(120)=versions(new,ver) then goto WRONG_VERSION
02680     open #121: "name="&dirname$&"\workfile.[WSID],recl="&str$(rln(120))&",replace,version="&str$(versions(new,ver)),internal,output 
02690     mat a(max(versions(old,num),versions(new,num))) !:
          mat a$(max(versions(old,str),versions(new,str)))
02692     if lastrec then 
02693       if not delete_lastrec then write #121,using "form L 9": lrec(120)
02694       if delete_lastrec then read #120,using "form L 9": y norec UPDATE_VERSION_1 eof ZUPDATE_VERSION
02696     end if 
02700 UPDATE_VERSION_1: read #120,using oldform$: mat a$(1:versions(old,str)),mat a(1:versions(old,num)) eof ZUPDATE_VERSION
02710     write #121,using newform$: mat a$(1:versions(new,str)),mat a(1:versions(new,num))
02720     goto UPDATE_VERSION_1
02730 ZUPDATE_VERSION: close #120: 
02740     close #121: 
02750     execute "copy "&dirname$&"\workfile.[WSID] "&dirname$&"\"&filename$&" -n"
02760     execute "free "&dirname$&"\workfile.[WSID] -n"
02770     goto ZZZUPDATE_VERSION
02780 WRONG_VERSION: if file(120)>-1 then close #120: 
02790     fnupdate_version=1
02800 ZZZUPDATE_VERSION: ! 
02810   fnend 
02850   def library fnsoflow(;_soflow$*1) !:
          !    !:
          ! | Returns 1 (true) if SOFLOW is set to IGNORE                 | !:
          ! | Returns 0 (false) if SOFLOW is set to SYSTEM                | !:
          !    !
02855     msgbox("FNSOFLOW should not be called as a library function.  The SOFLOW state of the library is not shared with the calling program.  To properly use this function list lines 2850 through 2880 to a text file and add then to your program or other library, where you want to test the SOFLOW state.  Then disable this message box.","SOFLOW STATE")
02860     _soflow$="1111" error 2870
02870     if _soflow$="1" then let fnsoflow=1 else let fnsoflow=0
02880   fnend 
02900   def library fnbrhelp$(;topic$*100) !:
          !    !:
          ! | Accesses and displays help from the BR WIKI for the subject | !:
          ! | specified in TOPIC$.  If no TOPIC$ is specified a window    | !:
          ! | opens to ask for a TOPIC$.  If the ESC key is pressed you   | !:
          ! | are taken to the MAIN BR-HELP page of the WIKI              | !:
          !    !
02902     execute "PROC="&env$("PD")&"Core\fnsnap\tt"
02905     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle,fn_cs,fnCopys2c,fnCopyc2s
02906     dim helpfil$*100
02910     if topic$<="" then 
02915       open #(helpwin:=fngethandle): "srow=10,scol=10,rows=5,cols=40,parent=NONE,name=BRHELP,relative",display,outin 
02920       pr #helpwin, fields "2,2,c ": "Key word for search"
02925       rinput #helpwin,fields "2,25,c 15,[D]": topic$ !:
            af=curfld !:
            ak=fkey
02930       if ak=esc then goto 2946
02935       if not ak and trim$(topic$)>" " then goto 2946
02940       goto 2925
02945     end if 
02946     _cs=fn_cs
02950     if _cs then !:
            open #(helpfil:=fngethandle): "name=@:temp\brhelp.htm,recl=2000,replace",display,output else !:
            open #(helpfil:=fngethandle): "name="&env$("TEMP")&"\brhelp.htm,recl=2000,replace",display,output 
02955     pr #helpfil: "<html>"
02960     pr #helpfil: "<head>"
02965     pr #helpfil: "<meta http-equiv=""Content-Type"" content=""text/html; charset=windows-1252"">"
02970     if topic$>"" then !:
            pr #helpfil: "<meta http-equiv=""refresh""content=""0;URL=http://brwiki.brulescorp.com/index.php?title="&topic$&""">" !:
          else !:
            pr #helpfil: "<meta http-equiv=""refresh""content=""0;URL=http://brwiki.brulescorp.com/index.php?title=BR_Manual"">"
02975     pr #helpfil: "</head>" !:
          pr #helpfil: "<body>" !:
          pr #helpfil: "</body>" !:
          pr #helpfil: "</html>"
02976     helpfil$=os_filename$(file$(helpfil))
02979 ! pr HELPFIL$ ! pause
02980     close #helpfil: 
02985 ! EXECUTE "sys -c-M"&ENV$("TEMP")&"\brhelp.htm -w"
02986     execute "sys -c -M "&helpfil$&" -w"
02987 ! sLEEP(10) : IF _CS THEN EXECUTE "FREE @:"&HELPFIL$ ELSE EXECUTE "FREE "&HELPFIL$
02990   fnend 
03000 ! ----------------------
03010   def library fnputpickwin(mat ppickwin) !:
          ! +----------------------------------------------------------------+!:
          ! | Updates FNSNAP with the window number being used in a program | !:
          ! | for FNPICK - not needed after version 4.16 FNPICK obsolete    | !:
          ! +----------------------------------------------------------------+!
03020 ! ----------------------
03030     mat pickwin(udim(ppickwin))
03040     mat pickwin=ppickwin
03050   fnend 
03060 ! ----------------------
03070   def library fngetpickwin(mat ppickwin) !:
          ! +----------------------------------------------------------------+!:
          ! | Transfers the PICKWIN matrix back to the calling program      | !:
          ! | for use with FNPICK                                           | !:
          ! +----------------------------------------------------------------+!
03080 ! ----------------------
03090     mat pickwin(udim(ppickwin))
03100     mat ppickwin=pickwin
03110   fnend 
03120 ! ----------------------
03130   def library fnpickwin(win,valwin) !:
          ! +----------------------------------------------------------------+!:
          ! |  Updates the PICKWIN matrix for FNPICK                        | !:
          ! |                                                               | !:
          ! +----------------------------------------------------------------+!
03131 ! WIN  MATRIX NUMBER FOR MULTIPLE PICKS IN SAME PROGRAM !:
          ! VALWIN     WINDOW NUMBER OF OPEN WINDOW OR ZERO FOR CLOSED WINDOW
03132     if udim(pickwin)<win then mat pickwin(win)
03140     pickwin(win)=valwin
03150   fnend 
03200 ! --------------------------
03210   def library fnfont$*30(symbol_set$,proportional,chr_per_inch,style$,weight$,typeface$) !:
          ! +----------------------------------------------------------------+!:
          ! | Returns a font configuration based on parameters passed       | !:
          ! | Superceded generally by the 2005 PRINTER.SYS substitutions    | !:
          ! +----------------------------------------------------------------+!
03220 ! symbol_set      8U  Roman !:
          !                 8M  math
03230 ! Proportional   0    Fixed width !:
          !           1    proportional
03240 ! Chr_per_inch   number translates to 1/72 inch pitch
03250 ! Style$          upright !:
          !                 italic !:
          !                 condensed !:
          !                 compressed  !:
          !                 expanded  !:
          !                 outline                shadowed
03260 ! weight$         thin   !:
          !                 light  !:
          !                 medium        !:
          !                 bold  !:
          !                 black
03270 ! Typeface$     \ Arial  !:
          !                 Times     !:
          !                 Times_new      !:
          !                 gothic
03280     dim s$(2),ss$(2),st$(7),sts$(7),sw$(5),sws$(5),tf$(4),tfs$(4)
03290     s$(1)="ROMAN" !:
          s$(2)="MATH"
03300     ss$(1)="8U" !:
          ss$(2)="8M"
03310     st$(1)="UPRIGHT" !:
          st$(2)="ITALIC" !:
          st$(3)="CONDENSED" !:
          st$(4)="COMPRESSED" !:
          st$(5)="EXPANDED" !:
          st$(6)="OUTLINE" !:
          st$(7)="SHADOWED"
03320     sts$(1)='0' !:
          sts$(2)="1" !:
          sts$(3)="4" !:
          sts$(4)="8" !:
          sts$(5)="24" !:
          sts$(6)="32" !:
          sts$(7)="128"
03330     sw$(1)="THIN" !:
          sw$(2)="LIGHT" !:
          sw$(3)="MEDIUM" !:
          sw$(4)="BOLD" !:
          sw$(5)="BLACK"
03340     sws$(1)="-5" !:
          sws$(2)="-3" !:
          sws$(3)="0" !:
          sws$(4)="3" !:
          sws$(5)="5"
03350     tf$(1)="ARIA" !:
          tf$(2)="TIMES" !:
          tf$(3)="TIMES_NEW" !:
          tf$(4)="GOTHIC"
03360     tfs$(1)="16602" !:
          tfs$(2)="4101" !:
          tfs$(3)="16901" !:
          tfs$(4)="4102"
03370     x=srch(mat s$,uprc$(symbol_set$))
03380     if x>0 then symbol_set$=ss$(x)
03390     if proportional>0 then proportional$="1" else proportional$="0"
03400     pitch$=str$(round(72/chr_per_inch,4))
03410     x=srch(mat st$,uprc$(style$))
03420     if x>0 then style$=sts$(x) else style$="0"
03430     x=srch(mat sw$,uprc$(weight$))
03440     if x>0 then weight$=sws$(x) else weight$="0"
03450     x=srch(mat tf$,uprc$(typeface$))
03460     if x>0 then typeface$=tfs$(x) else typeface$="4099"
03470     fnfont$=chr$(27)&"("&symbol_set$&chr$(27)&"(s"&proportional$&"p"&pitch$&"v"&style$&"s"&weight$&"b"&typeface$&"T"
03480   fnend 
03500   def library fnloadfont$*50(number$,fontcall$*50;font$*100,outfile) !:
          ! +----------------------------------------------------------------+!:
          ! |  Loads a soft font to a printer.  Generaly not necessary      | !:
          ! | after BR4.16 release with PRINTER.SYS substitutions           | !:
          ! +----------------------------------------------------------------+!
03510 ! FILNUM =    DYNAMICALLY SET STARTING AT 126 !:
          ! NUMBER$ =   NUMBER OF "FONT$(#)" TO SET !:
          ! FONTCALL$ = ESCAPE CODE USED TO INVOKE FONT  !:
          ! FONT$ =     FILE NAME OF FONT TO LOAD (optional) !:
          ! outfile   File nuber of existing open file to type to (use this instead of an intermediary)
03515     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fntype
03520 ! -------------LOAD SOFT FONT---------------------
03530     fnloadfont$=fontcall$
03540     if outfile>0 and file(outfile)>-1 then !:
            filnum=outfile !:
            goto 3570
03550     filnum=126
03560     if file(filnum)>-1 then !:
            filnum+=1 !:
            goto 3560
03570     if exists(font$)>0 then 
03610       pr #filnum: 
03620       pr #filnum: chr$(27)&"*c"&trim$(number$)&"D"
03670       fntype(font$,filnum)
03700       pr #filnum: 
03710       pr #filnum: chr$(27)&"*c5F"
03770     end if 
03780   fnend 
03800   def library fnwaitwin(message$*1000;title$*50,buttons$*100,default,bkgrnd$*100,waittime) !:
          !    !:
          ! |  Displays the MESSAGE in a no-parent window and returns the | !:
          ! | number of the window to the calling program                 | !:
          ! | MESSAGE can be multilined using \n to separate each line the| !:
          ! | message will be centered in the window                      | !:
          ! | BUTTONS$ are separated by a |                               | !:
          ! | DEFAULT is the button default if ENTER is pressed           | !:
          ! | BKGRND  is a graphic file to be used as the window background³!:
          !    !
03810     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle,fnwinrowcol,fncount,fnwinbuttons
03812     if len(buttons$)>0 then !:
            str2mat(buttons$,mat buttons$,"|") !:
            buttons=1 !:
            mat _buttons$(udim(mat buttons$)) else !:
            buttons=0
03813     if buttons then for a=1 to udim(mat buttons$) !:
              _buttons$(a)=uprc$(trim$(buttons$(a))(1:1)) !:
              buttons$(a)="^F"&str$(50+a)&":"&trim$(buttons$(a)) !:
            next a !:
            mat2str(mat buttons$,buttons$," ")
03815     dim message$(1)*100,mwrk$*40
03816     if bkgrnd$<="" then bkgrnd$="NONE" else !:
            if exists(env$("PD")&"icons\"&bkgrnd$)=2 then bkgrnd$=env$("PD")&"icons\"&bkgrnd$ else !:
              if exists(trim$(bkgrnd$))=2 then bkgrnd$=trim$(bkgrnd$)
03818     execute "config attribute [MSG]N/W:T,font=SWISS:MEDIUM" ! BOLD"
03819     if not len(message$) then message$="Processing - Please wait"
03820     fnwinrowcol(0,rows,cols) !:
          sr=1.
03830     maxrows=fncount(message$,"\n",maxlen) !:
          srow=int(rows/2-2) !:
          scol=int(cols/2-len(message$)/2)
03832     if maxrows>0 then let str2mat(message$,mat message$,"\n") !:
            mat mwrk$(udim(mat message$)) !:
            for a=1 to udim(mat message$) !:
              maxlen=max(len(message$(a)),maxlen,34) !:
            next a !:
          else maxlen=max(sr*len(message$),34)
03835     srow=int(rows/2-2) !:
          scol=int(cols/2-maxlen/2) !:
          if maxrows>0 then !:
            for a=1 to udim(mwrk$) !:
              mwrk$(a)=str$(a)&","&str$(int(sr*maxlen/2))&",0/cc "&str$(maxlen)&",[MSG]" !:
            next a
03840     open #(waitwin:=fngethandle): "srow="&str$(int(srow-maxrows/2))&",scol="&str$(int(scol))&",rows="&str$(4+int(maxrows))&",cols="&str$(min(70,int(sr*maxlen)))&",parent=NONE,picture="&bkgrnd$&",caption="&title$&",name="&title$&",Font.labels=Swiss:medium,Font.buttons=Swiss:small,NO_TASK_BAR",display,outin 
03845     if not maxrows then pr #waitwin, fields "2,"&str$(min(70,len(message$))/2)&",cc ,[MSG]": message$ !:
            fnwaitwin=waitwin !:
          else pr #waitwin, fields mat mwrk$: mat message$ !:
            fnwaitwin=waitwin
03850     if buttons then 
03853       fnwinbuttons(0,buttons$,waitwin,1)
03854       ab$=kstat$(1) !:
            _ab$=unhex$(ab$) !:
            ab=srch(mat _buttons$,uprc$(ab$)) !:
            if _ab$="0D" then !:
              fnwaitwin=default : close #waitwin: : waitwin=0 : goto ZWAITWIN !:
            else if ab>0 then !:
              fnwaitwin=ab : close #waitwin: : waitwin=0 : goto ZWAITWIN !:
            else if len(_ab$)<4 then goto 3854
03855   if fkey=99 or fkey=93 then let fnwaitwin=0 : goto ZWAITWIN !:
        else if fkey>50 and fkey<90 then !:
          fnwaitwin=fkey-50 : close #waitwin: : waitin=0 : goto ZWAITWIN !:
        else goto 3854
03858 end if 
03860 ZWAITWIN: lastb=0 !:
      fnend 
03861 def library fnwaitmsg(message$*1000) !:
        !    !:
        ! | Replaces the message in the window opened by FNWAITWIN      | !:
        ! | Because the Window is already open its size is set,         | !:
        ! | consequently the new message needs to fit within the size   | !:
        ! | and number of elements of the original message              | !:
        !    !
03862   dim blank$*100
03863   if exists(env$("PD")&"icons\blank.gif")=2 then !:
          blank$=env$("PD")&"icons\blank.gif" !:
          rinput #waitwin, fields "1,1,p 1/1",wait=.1: blank$ timeout 3864 !:
        else goto ZWAITMSG !:
          ! This changes focus to this window and forces it to the top
03864   if maxrows>0 then !:
          str2mat(message$,mat message$,"\n") !:
          mat message$(maxrows)
03865   if not maxrows and len(message$)=0 then message$="  "
03867   if not maxrows then pr #waitwin, fields "1,1,c": rpt$(" ",1000) !:
          pr #waitwin, fields "2,1,cc "&str$(maxlen)&",N/W:T": message$ error 3868 !:
        else pr #waitwin, fields "1,1,c": rpt$(" ",1000) !:
          pr #waitwin, fields mat mwrk$: mat message$ error 3868
03868 ZWAITMSG: fnend 
03870 def library fncount(_a$*1000,_b$;&_maxlen) !:
        !    !:
        ! | Returns the number of elements in a string that is separated| !:
        ! | by delimiters _b$. And returns the maximum length of the    | !:
        ! | elements                                                    | !:
        !    !
03880   _x=_maxlen=qx=0
03890   px=pos(_a$,_b$,qx) !:
        if px>0 then _maxlen=max(_maxlen,px-qx) !:
          qx=px+len(_b$)
03900   if px=0 and _x=0 then _maxlen=len(a$) !:
        else !:
          if px>0 then _x+=1 : px+=len(_b$) : goto 3890
03910   fncount=_x
03920 fnend 
03930 def library fnwaitbar(totbar,curbar) !:
        !    !:
        ! | Displays a progress bar within a previously opened waitwin  | !:
        ! |                                                             | !:
        !    !
03935   winb=int(min(totbar,curbar)/totbar*20)
03936   dim dummy$*100
03937   wins=(int(maxlen/2)-10) !:
        if not maxrows then winr$="3" else winr$=str$(udim(mwrk$)+2)
03940   if winb>lastb then 
03941     dummy$=env$("PD")&"icons\blue.bmp" !:
          rinput #waitwin, fields winr$&","&str$(wins+1)&",p 1/1",wait=.1: dummy$ timeout 3945
03945     for _a=max(1,lastb) to winb !:
            pr #waitwin, fields winr$&","&str$(wins+_a)&",p 1/1": env$("PD")&"Core\fnsnap\blue.bmp" !:
          next _a
03950   end if 
03960 fnend 
03999 ! -----------------------------
04000 def library fnbutton(button_text$,fk;btn) !:
        ! +----------------------------------------------------------------+!:
        ! | Displays a button in the button row at the bottom of the screen³!:
        ! | buttons can be added incrementally see also FNCLRBUTTON       | !:
        ! +----------------------------------------------------------------+!
04010   dim buttons(8)
04020   if btn=0 then 
04030     if button>=8 then goto ZBUTTON
04040     button+=1 !:
          b=button
04050   else b=max(btn,1)
04060   display buttons "1,"&str$(b*10-9)&",c 9,,"&str$(fk): button_text$
04070   fnbutton=button !:
        if b>0 then buttons(b)=1
04080 ZBUTTON: ! 
04090   btn=b=0
04100 fnend 
04110 ! -----------------------------
04120 def library fnclrbutton(;btn) !:
        ! +----------------------------------------------------------------+!:
        ! | Removes one or more buttons that were added by FNBUTTON       | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
04130   if btn>8 then goto ZCLRBUTTON
04140   if not btn then 
04150     if button<=0 then goto ZCLRBUTTON
04160     b=max(button,1)
04170   else b=btn
04180   if b>0 then display buttons "1,"&str$(b*10-9)&",c 9,,1": ""
04190   button-=1 !:
        button=max(button,0)
04200 ZCLRBUTTON: ! 
04210   if btn=99 then 
04220     for b=1 to 8
04230       display buttons "1,"&str$(b*10-9)&",c 9,,1": ""
04240       buttons(b)=0 !:
            button-=1 !:
            button=max(0,button)
04250     next b
04260   end if 
04270   btn=b=0
04280 fnend 
04290 def library fnweekofyear(d) !:
        ! +----------------------------------------------------------------+!:
        ! | Returns the number of times a specified day of the week based | !:
        ! | on the days function has occurred in the calendar year of D   | !:
        ! +----------------------------------------------------------------+!
04295 ! D is the days value of the date to be determined   !:
        ! fnweekofyear returns the number of times that day !:
        ! of the week has occoured !:
        ! in the current calendar year
04300   yr=date(d,"CY")
04305   wkday=mod(d,7) ! Monday=1 Sunday=0
04310   styear=days(yr*10000+101,"cymd")
04315   x=d-styear
04320   fnweekofyear=ip(x/7)+1
04325 fnend 
04330 ! -----------------------------------
04335 def library fnweekofmonth(d) !:
        ! +----------------------------------------------------------------+!:
        ! | Returns the number of time the specified day of the week has  | !:
        ! | occured in the month specified by D in days                   | !:
        ! +----------------------------------------------------------------+!
04340 ! d is the days value of the date to be determined   !:
        ! fnweekofyear returns the number of times that day !:
        ! of the week has occired !:
        ! in the current calendar year
04345   yrm=date(d,"CYM")
04350   wkday=fp(d/7)*7 ! Monday=1 Sunday=0
04355 ! -----------------------------------
04360   x=d-stmonth
04365   if x>0 then let fnweekofmonth=ip(x/7)+1 else let fnweekofmonth=0
04370 fnend 
04375 def library fndayofyear(d) !:
        ! +----------------------------------------------------------------+!:
        ! | Returns the ordinal number of days from the beginning of the  | !:
        ! | calendar year specified by D                                  | !:
        ! +----------------------------------------------------------------+!
04380 ! d is the days value of the date to be determined   !:
        ! fnweekofyear returns the number of times that day !:
        ! of the week has occired !:
        ! in the current calendar year
04385   yr=date(d,"CY")
04390   styear=days(yr*10000+101,"cymd")-1
04395 ! sTDAY=FP((STMONTH)/7)*7
04400   x=d-styear
04405   if x>0 then let fndayofyear=x else let fndayofyear=0
04410 fnend 
04415 def library fnmonthend(d) !:
        !    !:
        ! | Returns the date in days of the last day of the month       | !:
        ! | represented by D in days.                                   | !:
        !    !
04420   m=date(d,"M") !:
        cy=date(d,"CY") !:
        if m=12 then monthend=days((cy+1)*10000+0101)-1 else !:
          monthend=days(cy*10000+(m+1)*100+1)-1
04422   fnmonthend=monthend
04425 end def 
04430 def library fnquarterend(d) !:
        !    !:
        ! | Returns the date in days of the last day of the quarter     | !:
        ! | represented by D in days.                                   | !:
        !    !
04435   m=date(d,"M") !:
        cy=date(d,"CY")
04436   if m>0 and m<4 then m=3 !:
        else if m>3 and m<7 then m=6 !:
        else if m>6 and m<10 then m=9 !:
        else m=12
04440 if m=12 then quarterend=days((cy+1)*10000+0101)-1 else !:
        quarterend=days(cy*10000+(m+1)*100+1)-1
04442 fnquarterend=quarterend
04445 end def 
04450 def library fnyearend(d) !:
        !    !:
        ! | Returns the date in days of the last day of the calendar year³!:
        ! | represented by D in days.                                   | !:
        !    !
04455   m=date(d,"M") !:
        cy=date(d,"CY")
04456   m=12
04460   yearend=days((cy+1)*10000+0101)-1
04462   fnyearend=yearend
04465 end def 
04540 ! -----------------------------
04550 def library fnsortarray(mat l$,start,length;desending,header,footer) !:
        ! +----------------------------------------------------------------+!:
        ! | Sorts an array based on a substring within the array and      | !:
        ! | optionally omits lines at the top and bottom from the sort    | !:
        ! +----------------------------------------------------------------+!
04551 ! Sorts MAT L$ in ascending order !:
        ! OPtionally based on starting and ending positions !:
        ! In Descending order if DESending !:
        ! starting down from top to allow for a HEADER !:
        ! stopping before the bottom to allow for a FOOTER
04560   dim m$(1)*80 ! SORT_WRK$(1)*80,
04570   elements=udim(l$) !:
        mat idx(elements-header-footer) !:
        mat sort_wrk$(elements) !:
        mat m$(elements-header-footer)
04580   a=header !:
        a1=0
04590   do while a<udim(mat l$)-footer
04600     a+=1
04610     a1+=1 !:
          m$(a1)=l$(a)(start:start+length)
04620   loop 
04630   if udim(mat m$)=udim(mat l$)-header-footer then mat idx=aidx(m$) else mat idx=aidx(l$)
04640   if desending and udim(mat m$)=udim(mat l$)-header-footer then mat idx=didx(m$)
04650   elements=udim(l$) !:
        mat idx(elements-header-footer) !:
        mat sort_wrk$(elements) !:
        mat sort_wrk$=l$ !:
        for i=1 to elements-header-footer !:
          sort_wrk$(i+header)=l$(idx(i)+header) !:
        next i !:
        mat l$=sort_wrk$
04660 fnend 
04670 def library fntype(infile$*100,outfile) !:
        ! +----------------------------------------------------------------+!:
        ! | Moves the contents of one file into another without using     | !:
        ! | the BR TYPE command. OUTFILE must be an existing open file.   | !:
        ! +----------------------------------------------------------------+!
04672 ! INPUT FIELDS "23,60, C1": PAUSE$
04674 ! LIBRARY env$("PD")&"Core\fnsnap\fnsnap_dll.br": FNPROGRESS
04676   infile=30
04678   if file(infile)<0 then goto 4682
04680   infile+=1 !:
        goto 4678
04682   open #infile: "name="&infile$&",recl=1",external,input 
04684   infile_lrec=lrec(infile)
04686   close #infile: 
04688   infile_recl=min(32000,infile_lrec)
04690   open #infile: "name="&infile$&",RECL="&str$(infile_recl),external,input,relative 
04692   infile_rec=0
04694   infile_frm$="FORM C "&str$(infile_recl)
04696   if infile_recl=32000 then 
04698     if infile_rec*infile_recl+infile_recl<=infile_lrec then 
04700       read #infile,using infile_frm$: inrec$ !:
            pr #outfile: inrec$ !:
            ! fnPROGRESS(PROGWIN,INFILE_LREC,REC(INFILE),"10",INFILE$) !:
            infile_rec+=1
04702       goto 4698
04704     else 
04706       infile_frm$="FORM C "&str$(infile_lrec-infile_rec*32000)
04708       close #infile: 
04710       open #infile: "name="&infile$&",RECL="&str$(infile_lrec-infile_rec*32000),external,input,relative 
04712     end if 
04714   end if 
04716   read #infile,using infile_frm$,pos=infile_rec*infile_recl+1: inrec$ !:
        pr #outfile: inrec$ !:
        ! fnPROGRESS(PROGWIN,INFILE_LREC,REC(INFILE),"10",INFILE$)
04718 FRMINPUT: form c 1
04720   dim inrec$*32500
04722 ! CLOSE #PROGWIN: !:
        ! pROGWIN=FNCLSWIN(1)
04724 ZTYPE: close #infile: 
04726 ! CLOSE #PROGWIN: !:
        ! pROGWIN=FNCLSWIN(1)
04728 fnend 
04730 ! ----------------------------------------------------------
04740 def library fnltype(infile$*100,outfile;_inrec$*32700) !:
        !    !:
        ! | Moves records from a specified DISPLAY file to an open file | !:
        ! | Useful in printing files TYPE in 4.2g+ can be used instead  | !:
        !    !
04742   infile=10
04744   if file(infile)>-1 then infile+=1 : goto 4744
04746   open #infile: "NAME="&infile$,display,input 
04748   linput #infile: _inrec$ eof ZLTYPE
04750   pr #outfile: _inrec$&crlf$
04752   goto 4748
04754 ZLTYPE: close #infile: 
04756 fnend 
04800 def library fngethandle(;seed,down) !:
        !    !:
        ! | Returns the next unused file handle increaseing or optionally³!:
        ! | decreasing from an optional starting point                  | !:
        !    !
04805   if down then _handle=-1 else _handle=1
04810   if not seed then 
04811     if down then handle=999 else handle=1
04812   else handle=seed
04813   if file(handle)>-1 then handle+=_handle : goto 4813
04815   fngethandle=handle
04816 end def 
04820 ! ----------------------------------------------------------
04900 def library fnawplite$*1000(caption$*100,arows,acols,alen,results$*100,awtext$*1000) !:
        !    !:
        ! | Caption  caption in the AWP window during spellcheck        | !:
        ! | AROWS  the approx number of rows for the spell check window | !:
        ! | ACOLS  the approc number of columns for the spell check win | !:
        ! | ALEN   the allowed length of the returned phrase            | !:
        ! | RESULTS$ file name for the transfer file                    | !:
        ! | AWTEXT$ the text to be spell checked by AWPLite             | !:
        !    !
04902   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle
04903   execute 'proc '&env$("PD")&'Core\fnsnap\tt'
04904   fnawplite$=awtext$
04905   open #(awp:=fngethandle): "name="&env$("PD")&"Core\fnsnap\awp"&session$&".txt,replace",display,output 
04910   dim awp$*100,awt$*1000
04915 !    !:
        ! |  Open the parameters file for AWPlite                       | !:
        ! |                                                             | !:
        !    !
04920   awp$=os_filename$(file$(awp))
04925   pr #awp: "[Params]"
04930   pr #awp: "Caption="&caption$
04935   pr #awp: "ClientWidth="&str$(acols*12)
04940   pr #awp: "ClientHeight="&str$(arows*20)
04945   pr #awp: "MaxChars="&str$(alen)
04950   pr #awp: "Font Face=Arial"
04955   pr #awp: "Font Size=11"
04960   pr #awp: "SaveBtn_Caption=Save"
04965   pr #awp: "CancelBtn_Caption=Cancel"
04970   pr #awp: "ResultFile="&os_filename$(env$("PD")&"Core\fnsnap\awr"&session$)
04975   close #awp: 
04980 !    !:
        ! | Create the transfer file for data to be used by AWPlite     | !:
        ! |                                                             | !:
        !    !
04985   open #(awt:=fngethandle(awp)): "name="&env$("PD")&"Core\fnsnap\awt"&session$&".txt,recl="&str$(alen)&",replace",display,output 
04990   pr #awt: srep$(awtext$,"\n",crlf$)
04995   awt$=os_filename$(file$(awt))
05000   close #awt: 
05005 !    !:
        ! | Load and run the AWPlite program against the text           | !:
        ! |                                                             | !:
        !    !
05010   execute "sys -w "&os_filename$(env$("PD")&"Core\fnsnap\awplite.exe")&" "&awp$&" "&awt$
05015 !    !:
        ! | Check to see if AWPlite has created the RESULTS file that   | !:
        ! | indicaetes that spell check has completed or aborted        | !:
        !    !
05020   if exists(env$("PD")&"Core\fnsnap\awr"&session$)=2 then goto 5025 else let sleep(1) : goto 5020
05025   open #(awr:=fngethandle(awt)): "name="&env$("PD")&"Core\fnsnap\awr"&session$,display,input 
05030   linput #awr: result$
05035   close #awp: 
05040   execute "free "&env$("PD")&"Core\fnsnap\awr"&session$
05045 !    !:
        ! | If the user has SAVED the checked text then replace the     | !:
        ! | original, if CANCEL then leave the original                 | !:
        !    !
05050   if trim$(uprc$(result$))="SAVE" then 
05055     open #(awt:=fngethandle(awt)): "name="&env$("PD")&"Core\fnsnap\awt"&session$&".txt",display,input 
05056     awx=0
05060     linput #awt: awtext$ eof 5070 !:
          awx+=1
05065     if awx>1 then awt$(inf:inf)="\n"&awtext$ else awt$=awtext$
05067     goto 5060
05070     close #awt: 
05072     fnawplite$=awt$
05075   end if 
05080 fnend 
05100 def library fnprint_file(file_name$*100;indent,greybar,select) !:
        ! +----------------------------------------------------------------+!:
        ! | Prints any display file to a printer and optionally places    | !:
        ! | the output on GRAYBAR paper.                                  | !:
        ! +----------------------------------------------------------------+!
05101 ! IF SELECT THEN gREYBAR=0 !:
        ! MACROS are not compatable with NWP
05102   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngreybar$,fngreybar,fngethandle
05103   dim gbmacro$*100,printfile$*100
05105 ! pRINTFILE=5
05106 ! IF FILE(PRINTFILE)>-1 THEN pRINTFILE+=1 : GOTO 5106
05110   if not select then !:
          open #(printfile:=fngethandle): "NAME=//20,recl=2000,PAGEOFLOW=50",display,output  !:
        else !:
          open #(printfile:=fngethandle): "NAME=preview:/select,recl=2000,PAGEOFLOW=50",display,output 
05111   printfile$=file$(printfile)
05120   ! pr #PRINTFILE,USING SETPRINT: "[RESET+]" ! normal print
05130   pr #printfile,using SETPRINT: "[LETTER][CPI=15][TOPLEFT]" ! condensed print
05140 ! pr #PRINTFILE,USING SETPRINT: "[LETTER]" ! condensed print
05145   if greybar then macro=300 !:
          v=.1 : h=.2 : bv=10.75 : bh=7.75 !:
          shade=10 : head=4/6 : bar=3/6
05146   if greybar and not select then macro=300 !:
          gbmacro$=fngreybar$(macro,printfile,v,h,bv,bh,shade,head,bar)
05150 SETPRINT: form c 25,skip 0
05160   dim apf$*1000
05170   if indent>0 then indent$=rpt$(" ",indent) else indent$=rpt$(" ",5)
05180   f=0
05190   f+=1 : if file(f)>-1 then goto 5190
05200   open #f: "NAME="&file_name$,display,input 
05210 FORM1: form c 106,"[CPI=15]"
05220   pr #printfile,using "form skip 5": ! GOSUB HEADER
05230 ! -----------------------------------
05235   linecnt=0
05240 RDLOOP: mat b$=("")
05250   linput #f: apf$ eof ZPRINT_FILE
05255   if linecnt>50 then gosub PRINT_FILE_NWPG
05260   if len(apf$)<101 then 
05270     pr #printfile: indent$&apf$ : linecnt+=1
05280   else 
05290     pr #printfile: indent$&apf$(1:100) : linecnt+=1
05300     apf$=apf$(101:len(apf$))
05310     goto 5260
05320   end if 
05330   goto RDLOOP
05340 ! ------------------------------------
05342 PRINT_FILE_NWPG: gosub HEADER
05344   return 
05350 HEADER: ! 
05352 ! INPUT FIELDS "23,64,c 1": PAUSE$
05355   pr #printfile: "[TOPLEFT]"
05360   if greybar and not select then pr #printfile: gbmacro$
05362   if greybar and select then pr #printfile: fngreybar(printfile,v,h,bv,bh,shade,head,bar)
05365 ! INPUT FIELDS "23,64,c 1": PAUSE$
05370   pagecnt+=1
05380   pr "PRINTING PAGE "&str$(pagecnt)
05390   pr #printfile: indent$&"PAGE ";pagecnt;"       DATE ";date$;"       TIME ";time$;"   FILE "&file_name$
05400   pr #printfile: " "
05410   pr #printfile: indent$&"1........1.........2.........3.........4.........5.........6.........7.........8.........9.........0"
05420   pr #printfile: " "
05423   if not file_end then !:
          pr #printfile: newpage !:
          pr #printfile,using "form skip 5": 
05425   linecnt=0
05430   return 
05440 ZPRINT_FILE: close #f: !:
        f=0
05450   file_end=1 !:
        gosub HEADER !:
        pagecnt=0 !:
        file_end=0
05455   if file(printfile)>-1 then close #printfile: ioerr 5460
05460 ZZPRINT_FILE: ! 
05470 fnend 
05490 ! ------------------------
05500 def library fnfilesize(filename$*66) !:
        ! +----------------------------------------------------------------+!:
        ! | Checks an internal file size against a passed parameter       | !:
        ! | Useful in connection with updating versions of files          | !:
        ! +----------------------------------------------------------------+!
05510   fs=300
05520   if file(fs)>-1 then fs+=1 : goto 5520
05530   open #fs: "name="&trim$(filename$)&",recl=1",external,input 
05540   fnfilesize=lrec(fs)
05550   close #fs: 
05555   fs=0
05560 fnend 
05600 def library fnprinterselect$*50(;srchstr$*50)
05602   dim _prl$(1)*100,_prn$(1)*100,_prd$(1)*100
05604   printer_list(mat _prl$)
05606   mat _prn$(udim(mat _prl$)) !:
        mat _prd$(udim(mat _prl$))
05608   for _p=1 to udim(_prl$)
05610     _px=pos(_prl$(_p),"@") !:
          _prn$(_p)=_prl$(_p)(1:_px-1) !:
          _prd$(_p)=_prl$(_p)(_px+1:len(_prl$(_p)))
05612   next _p
05614   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnlistspec$,fnwinrowcol,fnsrchcrit$,fnlistsrch,fnlistsrchn,fnwinbuttons
05616   mat headers$(1) !:
        headers$(1)="Printer"
05618   mat widths(1) !:
        widths(1)=20
05620   mat forms$(1) !:
        forms$(1)="V 50"
05622   fnwinrowcol(0,lrow,lcol)
05624   arows=0 : srow=4 : scol=3 !:
        lcols=min(lcol-srow-2,sum(mat widths)+2) !:
        lrows=min(lrow-srow-2,max(3,udim(mat _prn$)+arows))
05626   listspec$=fnlistspec$(listwin,srow,scol,lrows,lcols,arows,mat headers$,mat widths,mat forms$,"Select Printer","",0,2)
05628   fnwinbuttons(0,"^F2:Search ^F3:Repeat",listwin)
05630   pr #listwin, fields listspec$&",=R" : (mat _prn$)
05632   mat select(1)=(0) !:
        fnlistsrch(mat _prn$,srchstr$,mat select) !:
        ! fnLISTSRCHN(MAT three,SRCHSTR$,MAT SELECT) !:
        ! Turn these on to search for stings in matrixes !:
        curfld(1,max(1,select(1)))
05634   input #listwin, fields listspec$&",rowsub,selone ": px !:
        fk=fkey !:
        if fk=201 then fk=0
05636   if fk=2 then 
05638     srchstr$=fnsrchcrit$(str$(round(lrows/2,0)),"2",2,lcols-4,listwin,"Printer to search for") !:
          mat select(1)=(0) !:
          fnlistsrch(mat _prn$,srchstr$,mat select) !:
          ! fnLISTSRCHN(MAT PDPAID,SRCHSTR$,MAT SELECT) !:
          ! Turn these on to search for stings in matrixes
05640     if select(1)=0 and udim(select)=1 then !:
            msgbox("No matching criterea","Search Results","OK","EXCL") !:
            goto 5634 !:
          else !:
            ff=1 : curfld(1,select(ff)) : goto 5634
05642   end if 
05644   if fk=3 then 
05646     if ff<udim(mat select) then ff+=1 else ff=1
05648     curfld(1,select(ff)) !:
          goto 5634
05650   end if 
05652   if not (fk=0 or fk=99) then goto 5634
05654   close #listwin: !:
        listwin=0
05656   if fk=99 then let fnprinterselect$="" else let fnprinterselect$=_prn$(px)
05658 fnend 
05900 def library fnwikierror(ecode$) !:
        !    !:
        ! | John Bowman access to the BR wiki for an error codes lookup | !:
        ! |                                                             | !:
        !    !
05910   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$
05920   dim epath$*100
05930   epath$=fnmsexe$("iexplore.exe")
05940   if exists(epath$)=2 then !:
          execute "Sys -w -C "&epath$&" http://brwiki.ads.net/index.php/Special:Search?search="&ecode$ else !:
          execute "Sys -C start http://brwiki.ads.net/index.php/Special:Search?search="&ecode$
05950 fnend 
06000 def library fnrowsum(mat l,r) !:
        ! +----------------------------------------------------------------+!:
        ! | Function to sum the amounts in a row of a matrix              | !:
        ! | R is the specified row.  Returns 0 if R is outside range      | !:
        ! +----------------------------------------------------------------+!
06010   on error ignore 
06020   lcols=0 : if udim(l,2)>0 then lcols=udim(l,2)
06030   lrows=udim(l)
06040   on error system 
06050   x=0
06060   if r>lrows or r<1 then goto 6100
06070   for a=1 to lcols
06080     if lcols then x+=l(r,a) else x+=l(a)
06090   next a
06100   fnrowsum=x
06110 fnend 
06120 ! --------------------------------------------
06130 def library fncolsum(mat l,c;srow,erow) !:
        ! +----------------------------------------------------------------+!:
        ! | Function to sum the amounts in a column of a matrix           | !:
        ! | C is the column specified.  Returns 0 if outside the range    | !:
        ! +----------------------------------------------------------------+!
06140   on error ignore 
06150   lcols=0 : if udim(l,2)>0 then lcols=udim(l,2)
06160   lrows=udim(l)
06170   on error system 
06180   x=0
06182   if srow<1 then srow=1
06183   if erow>udim(mat l) or erow<srow then erow=udim(mat l)
06190   if c>lcols or c<1 then goto 6250
06200   if lcols then 
06210     for a=srow to erow
06220       x+=l(a,c)
06230     next a
06240   else x=l(c)
06250   fncolsum=x
06260 fnend 
06270 def library fnrowsrch(mat l$,srchrow,srchstring$*50;nocase) !:
        !    !:
        ! | IF CASE is true then search is case INsensitve              | !:
        ! | Searches a row of an array for a match                      | !:
        ! | returns the column number of the first match as the value   | !:
        !    !
06275   srchfound=xsrch=0
06280   xsrch=udim(mat l$,2) error SRCHROW1
06285   goto SRCHROW2
06290 SRCHROW1: if nocase=0 and srchstring$=l$(srchrow) then !:
          srchfound=srchrow else !:
          if nocase>0 and uprc$(srchstring$)=uprc$(l$(srchrow)) then !:
            srchfound=srchrow !:
          else srchfound=0
06295   goto ZSRCHROW
06300 SRCHROW2: for asrch=1 to xsrch
06305     srchfound=0
06310     if nocase then 
06315       if uprc$(l$(srchrow,asrch))=uprc$(srchstring$) then srchfound=asrch !:
              goto ZSRCHROW
06320     else 
06325       if l$(srchrow,asrch)=srchstring$ then srchfound=asrch : goto ZSRCHROW
06330     end if 
06335   next asrch
06340 ZSRCHROW: fnrowsrch=srchfound
06345 fnend 
06350 def library fncolsrch(mat l$,srchcol,srchstring$*50;nocase) !:
        !    !:
        ! | IF NOCASE is true then search is case INsensitve            | !:
        ! | Searches a column of an array for a match                   | !:
        !    !
06355   srchfound=xsrch=0
06360   xsrch=udim(mat l$,1) error SRCHCOL1
06365   goto SRCHCOL2
06370 SRCHCOL1: if nocase=0 then srchfound=max(0,srch(mat l$,srchstring$)) !:
        else if nocase>0 then srchfound=max(0,srch(mat l$,"^"&srchstring$)) !:
        else srchfound=0
06375 goto ZSRCHCOL
06380 SRCHCOL2: for asrch=1 to xsrch
06385   srchfound=0
06390   if nocase then 
06395     if uprc$(l$(asrch,srchcol))=uprc$(srchstring$) then srchfound=asrch !:
            goto ZSRCHCOL
06400   else 
06405     if l$(asrch,srchcol)=srchstring$ then srchfound=asrch : goto ZSRCHCOL
06410   end if 
06415 next asrch
06420 ZSRCHCOL: fncolsrch=srchfound
06421 ! IF SRCHFOUND>0 THEN PAUSE
06425 fnend 
06450 def library fnfil(filloc$*100,filnm$,&filbatch;sufx$) !:
        !    !:
        ! | Create a file number and batch number for a file that       | !:
        ! | will increment by 1 each time a new batch is created        | !:
        !    !
06452 ! FILNM$ is the beginning of the file name before the sequence number !:
        ! filbatch is the file sequnce number being returned !:
        ! sufx$ is an optional file suffix (not currently implemented)
06454   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle,fnfil
06456   filloc$=trim$(filloc$)
06458   lf=len(filloc$)
06460   if not filloc$(lf:lf)="\" then filloc$=filloc$&"\"
06462 !    !:
        ! |  Create a file number and file name of an ascending sequence| !:
        ! |                                                             | !:
        !    !
06464   crlf$=chr$(13)&chr$(10) !:
        a=0
06466   dim a$*2000
06468 ! IF FILE(FIL)>-1 THEN fiL+=1 : GOTO 6550
06470 !    !:
        ! | Determine the next batch file number from existing files    | !:
        ! |                                                             | !:
        !    !
06472   if filbatch>0 and sufx$>"" then goto 6492
06474   execute "dir "&filloc$&"*.* >dirb.txt"
06476   open #(fil:=fngethandle): "name=dirb.txt",display,input 
06478   linput #fil: a$ eof 6486
06480   if not uprc$(a$)(1:len(filnm$))=uprc$(filnm$) then goto 6478
06482   a=max(a,val(a$(len(filnm$)+1:pos(a$," ")-1))) conv 6478
06484   goto 6478
06486   filbatch=a+1
06488 ! INPUT FIELDS "25,70,c 1": PAUSE$
06490   close #fil,free: 
06492 !    !:
        ! |  Set the function equal to the file number                  | !:
        ! |                                                             | !:
        !    !
06494   fnfil=fil
06496 fnend 
06500 def library fnhist(mask$,from$*100,to$*100) ! !:
        ! ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿!:
        ! | Copies files from one location to another and then deletes     | !:
        ! | the original file after it exists in the new location          | !:
        ! | MASK$ The leading protion of the file name                     | !:
        ! | FROM$ The directory from which the file will move              | !:
        ! | TO$   The directory into which the file will be moved          | !:
        ! |                                                                | !:
        ! | FNHIST("mawh",env$("PD")&"efile\ma",env$("PD")&"efile\ma\mawh")| !:
        ! ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ!
06502   execute "dir "&from$&"\"&mask$&"*.* >hist"&session$&".txt"
06504   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle
06506   open #(infile:=fngethandle): "name=hist"&session$&".txt",display,input 
06508   dim ahist$*100,dhist$(1)*100
06510   dh=0
06512 HIST1: linput #infile: ahist$ eof HIST2
06514   if uprc$(ahist$(46:len(mask$)+45))=uprc$(mask$) and pos(ahist$,"<DIR>")<1 and pos("1234567890",ahist$(46+len(mask$):46+len(mask$)))>0 then 
06516     histfile$=ahist$(46:len(ahist$))
06518     dh+=1 !:
          mat dhist$(dh) !:
          dhist$(dh)=histfile$
06520   end if 
06522   goto HIST1
06524 HIST2: close #infile: 
06526   execute "mkdir "&to$ ioerr 6528
06528   if exists("hist"&session$&".txt")=2 then execute "free hist"&session$&".txt -N"
06530   for a=1 to dh-3
06532     execute "copy "&from$&"\"&dhist$(a)&" "&to$&"\"&dhist$(a)
06534     if exists(to$&"\"&dhist$(a))=2 then !:
            execute "free "&from$&"\"&dhist$(a)&" -N"
06536   next a
06538 ZHIST: if file(infile)>-1 then close #infile: 
06540   if exists("hist"&session$&".txt")=2 then execute "free hist"&session$&".txt -N"
06542   fnhist=dh-1
06544 fnend 
06700 ! =====================================================================
06710 def library fnbusinessday(xdate) !:
        !    !:
        ! | Increase indate until it is not a weekend or legal holiday  | !:
        ! |                                                             | !:
        !    !
06730   if mod(xdate,7)>5 or mod(xdate,7)<1 then xdate+=1 : goto 6730 !:
          ! If Saturday or Sunday go to the next weekday
06740   if date(xdate,"md")=0101 then xdate+=1 : goto 6730 else !:
          if mod(xdate,7)=5 and date(xdate,"md")=1231 then xdate+=1 : goto 6730 : else !:
            if mod(xdate,7)=1 and date(xdate,"md")=0102 then xdate+=1 : goto 6730 !:
              ! Ne Year's Day
06750   if mod(xdate,7)=1 and date(xdate,"md")>0114 and date(xdate,"md")<0122 then xdate+=1 : goto 6730 !:
          ! Martin Luther King day
06760   if mod(xdate,7)=1 and date(xdate,"md")>0214 and date(xdate,"md")<0223 then xdate+=1 : goto 6730 !:
          ! George Washington's Birhday
06770   if mod(xdate,7)=1 and date(xdate,"md")>0414 and date(xdate,"md")<0423 then xdate+=1 : goto 6730 !:
          ! Patriot's Day
06780   if mod(xdate,7)=1 and date(xdate,"md")>0523 and date(xdate,"md")<0601 then xdate+=1 : goto 6730 !:
          ! Memorial Day
06790   if date(xdate,"md")=704 then xdate+=1 : goto 6730 else !:
          if mod(xdate,7)=5 and date(xdate,"md")=703 then xdate+=1 : goto 6730 : else !:
            if mod(xdate,7)=1 and date(xdate,"md")=705 then xdate+=1 : goto 6730 !:
              ! Independance Day
06800   if mod(xdate,7)=1 and date(xdate,"md")>0831 and date(xdate,"md")<0908 then xdate+=1 : goto 6730 !:
          ! Labor Day
06810   if mod(xdate,7)=1 and date(xdate,"md")>1007 and date(xdate,"md")<1015 then xdate+=1 : goto 6730 !:
          ! Columbus Day
06820   if date(xdate,"md")=1111 then xdate+=1 : goto 6730 else !:
          if mod(xdate,7)=1 and date(xdate,"md")=1112 then xdate+=1 : goto 6730 !:
            ! Veterans Day
06830   if mod(xdate,7)=4 and date(xdate,"md")>1121 and date(xdate,"md")<1129 then xdate+=1 : goto 6730 !:
          ! Thanksgiving Day
06840   if date(xdate,"md")=1225 then xdate+=1 : goto 6730 else !:
          if mod(xdate,7)=5 and date(xdate,"md")=1224 then xdate+=1 : goto 6730 : else !:
            if mod(xdate,7)=1 and date(xdate,"md")=1226 then xdate+=1 : goto 6730 !:
              ! Christmas Day
06850   fnbusinessday=xdate
06860 fnend 
06900 ! =====================================================================
06910 def library fnpriorbusinessday(xdate) !:
        !    !:
        ! | Decrease indate until it is not a weekend or legal holiday  | !:
        ! |                                                             | !:
        !    !
06930   if mod(xdate,7)>5 or mod(xdate,7)<1 then xdate-=1 : goto 6930 !:
          ! If Saturday or Sunday go to the prior weekday
06940   if date(xdate,"md")=0101 then xdate-=1 : goto 6930 else !:
          if mod(xdate,7)=5 and date(xdate,"md")=1231 then xdate-=1 : goto 6930 : else !:
            if mod(xdate,7)=1 and date(xdate,"md")=0102 then xdate-=3 : goto 6930 !:
              ! New Year's Day
06950   if mod(xdate,7)=1 and date(xdate,"md")>0115 and date(xdate,"md")<0123 then xdate-=3 : goto 6930 !:
          ! Martin Luther King day
06960   if mod(xdate,7)=1 and date(xdate,"md")>0214 and date(xdate,"md")<0223 then xdate-=3 : goto 6930 !:
          ! George Washington's Birhday
06970 ! IF MOD(XDATE,7)=1 AND DATE(XDATE,"md")>0414 AND DATE(XDATE,"md")<0423 THEN xDATE-=3 : GOTO 6930 !:
        ! Patriot's Day
06980   if mod(xdate,7)=1 and date(xdate,"md")>0523 and date(xdate,"md")<0601 then xdate-=3 : goto 6930 !:
          ! Memorial Day
06990   if date(xdate,"md")=704 then xdate-=1 : goto 6930 else !:
          if mod(xdate,7)=5 and date(xdate,"md")=703 then xdate-=1 : goto 6930 : else !:
            if mod(xdate,7)=1 and date(xdate,"md")=705 then xdate-=3 : goto 6930 !:
              ! Independance Day
07000   if mod(xdate,7)=1 and date(xdate,"md")>0831 and date(xdate,"md")<0908 then xdate-=3 : goto 6930 !:
          ! Labor Day
07010   if mod(xdate,7)=1 and date(xdate,"md")>1007 and date(xdate,"md")<1015 then xdate-=3 : goto 6930 !:
          ! Columbus Day
07020   if date(xdate,"md")=1111 then xdate-=1 : goto 6930 else !:
          if mod(xdate,7)=1 and date(xdate,"md")=1112 then xdate-=3 : goto 6930 !:
            ! Veterans Day
07030   if mod(xdate,7)=4 and date(xdate,"md")>1121 and date(xdate,"md")<1129 then xdate-=1 : goto 6930 !:
          ! Thanksgiving Day
07040   if date(xdate,"md")=1225 then xdate-=1 : goto 6930 else !:
          if mod(xdate,7)=5 and date(xdate,"md")=1224 then xdate-=1 : goto 6930 : else !:
            if mod(xdate,7)=1 and date(xdate,"md")=1226 then xdate-=3 : goto 6930 !:
              ! Christmas Day
07050   fnpriorbusinessday=xdate
07060 fnend 
07200 def library fncheckamount$*200(amount;length,opt) !:
        !    !:
        ! | Amount   The value to be converted to words                 | !:
        ! | Length   The size to be left padded with tildes - optional  | !:
        ! | OPT=0    The words "Dollars"and "Cents" are included        | !:
        ! | OPT=1    Cents are made a fraction and preceed "Dollars"    | !:
        ! | OPT=2    Cents are made a fraction and no "Dollars" printed | !:
        !    !
07220 ! 
07230   dim ckamt(1),ckamt$(1)*100
07240   mat ckamt(5)=(0)
07250   mat ckamt$(5)=("")
07260   if amount<=0 then 
07270     if length>10 then !:
            fncheckamount$=rpt$("~",int(length/2-4))&"V O I D"&rpt$("~",length/2-4) else !:
            fncheckamount$="~~V O I D~~"
07280     goto ZCHECKAMOUNT
07290   end if 
07300   ckamt(1)=round(fp(amount),2)*100
07310   amount=int(amount)
07320   for a=2 to 5
07330     ckamt(a)=fp(amount/1000)*1000
07340     amount=int(amount/1000)
07350   next a
07360 MILLION: ckamt$(4)=fnwords$(ckamt(4),"million") !:
        !    !:
        ! | Set the values of the millions                              | !:
        ! |                                                             | !:
        !    !
07370 THOUSAND: ckamt$(3)=fnwords$(ckamt(3),"thousand") !:
        !    !:
        ! | Set the values of the thousands places                      | !:
        ! |                                                             | !:
        !    !
07380 HUNDRED: ckamt$(2)=fnwords$(ckamt(2),"") !:
        !    !:
        ! | Set the value of the ones, tens and hundreds                | !:
        ! |                                                             | !:
        !    !
07390   if ckamt(5)+ckamt(4)+ckamt(3)+ckamt(2)>0 then 
07400     if opt=0 then ckamt$(2)=rtrm$(ckamt$(2))&" Dollars"
07410   else 
07420     if opt=0 then ckamt$(2)=" No Dollars"
07430     if opt=1 then ckamt$(2)=" NONE"
07440     if opt=2 then ckamt$(2)=" NONE"
07450   end if 
07460 CENTS: ckamt$(1)=fnwords$(ckamt(1),"") !:
        !    !:
        ! | Set the CENTS values if not whole dollars                   | !:
        ! |                                                             | !:
        !    !
07470   if ckamt(1)>0 and not opt then !:
          ckamt$(1)=" and "&trim$(ckamt$(1))&" Cents" else !:
          if not opt then ckamt$(1)=" and No Cents"
07480   if ckamt(1)>0 and opt=1 then !:
          ckamt$(1)=" and "&str$(ckamt(1))&"/100 Dollars" else !:
          if opt=1 then ckamt$(1)=" and XX/100 Dollars"
07490   if ckamt(1)>0 and opt=2 then !:
          ckamt$(1)=" and "&str$(ckamt(1))&"/100" else !:
          if opt=2 then ckamt$(1)=" and XX/100"
07500 !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
07510   if length then !:
          fncheckamount$=lpad$(ckamt$(4)&ckamt$(3)&ckamt$(2)&ckamt$(1),length,"~") else !:
          fncheckamount$=ckamt$(4)&ckamt$(3)&ckamt$(2)&ckamt$(1)
07520 ZCHECKAMOUNT: fnend 
07530 ! 
07540 def fnwords$*200(vamt,caption$): !:
        !    !:
        ! | Set the values for various three digit sets                 | !:
        ! |                                                             | !:
        !    !
07550   dim vamt$*100
07560   dim ones$(10),tens$(10),teens$(10)
07570   ones$(1)="One" : ones$(2)="Two": ones$(3)="Three" !:
        ones$(4)="Four": ones$(5)="Five": ones$(6)="Six" !:
        ones$(7)="Seven": ones$(8)="Eight": ones$(9)="Nine" !:
        ones$(10)="Ten"
07580   tens$(1)="" : tens$(2)="Twenty": tens$(3)="Thirty" !:
        tens$(4)="Forty": tens$(5)="Fifty": tens$(6)="Sixty" !:
        tens$(7)="Seventy": tens$(8)="Eighty": tens$(9)="Ninety" !:
        tens$(10)=""
07590   teens$(1)="Eleven" : teens$(2)="Twelve": teens$(3)="Thirteen" !:
        teens$(4)="Forteen": teens$(5)="Fifteen": teens$(6)="Sixteen" !:
        teens$(7)="Seventeen": teens$(8)="Eighteen": teens$(9)="Nineteen" !:
        teens$(10)=""
07600   vamt$=""
07610   if vamt<=0 then goto ZWORDS
07620   x$=cnvrt$("pic(###)",vamt)
07630   if x$(1:1)>"0" then vamt$=" "&ones$(val(x$(1:1)))&" Hundred"
07640   if x$(2:2)>"1" and x$(3:3)>"0" then !:
          vamt$=vamt$&" "&tens$(val(x$(2:2))) !:
          vamt$=vamt$&"-"&ones$(val(x$(3:3))) !:
        else !:
          if x$(2:2)>"1" and x$(3:3)="0" then !:
            vamt$=vamt$&" "&tens$(val(x$(2:2)))
07650   if x$(2:2)="1" and not x$(3:3)="0" then !:
          vamt$=vamt$&" "&teens$(val(x$(3:3))) !:
        else !:
          if x$(2:2)="1" and x$(3:3)="0" then !:
            vamt$=vamt$&" "&ones$(val(x$(2:3)))
07660   if x$(2:2)="0" and not x$(3:3)="0" then !:
          vamt$=vamt$&" "&ones$(val(x$(3:3)))
07670   if vamt>0 and caption$>"" then !:
          caption$=lwrc$(trim$(caption$)) !:
          caption$(1:1)=uprc$(caption$(1:1)) !:
          vamt$=vamt$&" "&caption$
07680 ZWORDS: fnwords$=vamt$
07690 fnend 
07700 def library fnactiverow(awinno,aspec$,udimh,anxtrow,&alastrow) !:
        !    !:
        ! | Color the first cell in a grid for the active line          | !:
        ! | AWINNO  the window number the grid exists in                | !:
        ! | ASPEC$  the grid specification 1,1,22/23,GRID               | !:
        ! | UDIMH   number of columns usually the DIM of MAT HEADERS$   | !:
        ! | ANXTROW nxtrow set by BR must be manually set initially     | !:
        ! | ALASTROW the last row that was highlighted                  | !:
        ! |                                                             | !:
        ! |   nEXTROW=1                                             | !:
        ! |   fnACTIVEROW(w,as$,udim(mat headers$),NEXTROW,LASTROW) | !:
        !    !
07710   dim arow(2),arow$(2)*50
07720   if pos(uprc$(aspec$),"GRID")>0 then 
07730     execute "config attribute [AA_ROW]N/#000000:#F7D688"
07740     mat arow(2) !:
          mat arow$(2)
07750     arow(1)=alastrow*udimh-(udimh-1) !:
          arow(2)=anxtrow*udimh-(udimh-1)
07760     arow$(1)="" !:
          arow$(2)="[AA_ROW]"
07770     ! IF NOT CURROW=ALASTROW THEN !:                                                            aROW(1)=1 !
07780     pr #awinno, fields aspec$&",attr": (mat arow, mat arow,mat arow$)
07790   end if 
07800   alastrow=anxtrow
07810 fnend 
08000 def library fnmask$*50(s$*50,n) !:
        !    !:
        ! | Replace leading digits or characters with "X" except for the| !:
        ! | last N items of the string.  Used for masking SSNs or credit| !:
        !    !
08010   s$=trim$(s$)
08020   nx=len(s$)
08030 ! s$=SREP$(SREP$(SREP$(SREP$(SREP$(SREP$(SREP$(SREP$(SREP$(SREP$(S$(1:NX-N),"0","X"),"1","X"),"2","X"),"3","X"),"4","X"),"5","X"),"6","X"),"7","X"),"8","X"),"9","X")&S$(NX-N+1:NX)
08031   for _s=1 to nx-n
08032     s$(_s:_s)=srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(s$(_s:_s),"0","X"),"1","X"),"2","X"),"3","X"),"4","X"),"5","X"),"6","X"),"7","X"),"8","X"),"9","X")
08033   next _s
08040   fnmask$=s$
08050 fnend 
08100 def library fnextractpdf(source$*100,destination$*100,page) !:
        !    !:
        ! | Extracts a PDF page from a PDF file and puts it in a one    | !:
        ! | page PDF file created by the BR PDF facility                | !:
        ! | Used for creating single page PDF's for the FORMPRINT routine³!:
        ! |                                                             | !:
        !    !
08110   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle,fngetfile$,fntop$,fnleft$,fnlines$
08120   execute 'proc '&env$("PD")&'Core\fnsnap\tt'
08130   dim pdffile$*100,macno$(1)*40
08140   open #(filnum:=fngethandle): "name=PDF:/READER,PrintFile="&destination$&",eol=none,use",display,output  !:
        pr #filnum: fntop$(1)&fnleft$(0)&fnlines$(0)
08150   pr #filnum: esc$&"pdf='"&str$(page)&","&source$&"'"
08160   close #filnum: 
08170   filnum=0
08180 fnend 
08200 def library fnlowercase(post$*100) !:
        !    !:
        ! | Renames all files in the POST$ directory under the current  | !:
        ! | location to lowercase                                       | !:
        ! | Needed for files copied to a Linux envirnment               | !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
08210   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwaitwin,fnwaitmsg,fngethandle
08220   waitwin=fnwaitwin("Changing "&post$&" files to lowercase names.")
08230   execute "mkdir "&env$("temp")&"" ioerr 8240
08240   post$=trim$(post$)
08250   execute "sys -M dir "&os_filename$(post$)&" >"&os_filename$(env$("PD"))&"\temp\dirfile.txt"
08260   if pos(post$,"\")<1 and post$>"" then post$=post$&"\"
08270   open #(dirfile:=fngethandle): "name="&env$("temp")&"\dirfile.txt",display,input 
08280   dim a$*500,f$*32000
08290 LWRCSTART: linput #dirfile: a$ eof LWRCEOJ
08300   pr a$
08310   if pos(a$,"<DIR>")>1 then goto LWRCSTART
08320   if pos(a$,"dirfile.txt")>1 then goto LWRCSTART
08330   if pos("01",a$(1:1))<1 then goto LWRCSTART
08340   f$=trim$(a$(40:inf))
08350   execute "rename "&post$&f$&" "&lwrc$(post$&f$) ioerr 8380
08360   fnwaitmsg(f$&" renamed to "&lwrc$(f$))
08370 ! INPUT FIELDS "2,2,c 1,[D]x": C$ ! PAUSE
08380   goto LWRCSTART
08390 LWRCEOJ: close #dirfile,free: 
08400 ZLWRCEOJ: fnend 
10000 ! ------------------------
10010 DELDUPS: !:
      ! +----------------------------------------------------------------+!:
      ! | Subroutine used with FNINDEX that lists or deletes duplicate  | !:
      ! | index records rom an index file being updated by FNINDEX      | !:
      ! +----------------------------------------------------------------+!
10015 library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprint_file,fndialog$
10020 ! ------------------------
10022 ! DIM A$*2000
10025 dupa=0
10030 open #flnr: "NAME="&flnm$&",KFNAME="&kfnm$&",SHR",internal,outin,keyed 
10040 keylen=kln(flnr)
10060 savecurfld=curfld
10065 close #flnr: 
10070 choice$=fndialog$("10","10",60,"File "&flnm$&" has duplicate key records. This condition will not prevent the system from working, but could cause inefficiencies and some lost data. You can continue without deleting the duplicates or elect to delete them now","Delete","Continue","List",1,dupa,0,0)
10080 if choice$="Delete" then goto DELDUPS0
10082 if choice$="Continue" then dupa=2
10090 if choice$="List" then 
10092   open #flnr: "name=ldup.[WSID]",display,output  !:
        pr #flnr: "Index error in file "&flnm$&" using index "&kfnm$ !:
        close #flnr: 
10093   fnprint_file("ldup."&wsid$,10,1,1) !:
        ! INPUT FIELDS "23,64,c 1,ax": CHOICE$
10094   choice$="" !:
        dupa=3 !:
        goto 10070
10095 end if 
10098 if choice$="" then dupa=3
10100 return 
10109 ! -----------------------------
10110 DELDUPS0: open #flnr: "NAME="&flnm$&",NOSHR",internal,outin,relative 
10120 open #flnr+1: "NAME=LDUP.[WSID]",display,input 
10130 lstkey$=""
10140 DELDUPS1: input #flnr+1: a$ eof ZDELDUPS1
10145 dim key$*50,lstkey$*50 ! A$*2000,
10150 aln=len(a$)
10160 key$=a$(1:keylen)
10170 keyrec=val(a$(keylen+1:aln)) conv DELDUPS1
10180 if key$=lstkey$ then delete #flnr,rec=keyrec: 
10190 lstkey$=key$
10200 pr f "10,10,C 60,N,N": "RECORD "&str$(keyrec)&" FOR FILE "&flnm$&" DELETED"
10210 goto DELDUPS1
10219 ! -----------------------------
10220 ZDELDUPS1: close #flnr: 
10230 close #flnr+1,free: 
10240 return 
10300 ! ------------------------------------------------
10310 def library fnleadzero$(number,length) !:
        ! +----------------------------------------------------------------+!:
        ! | Pads a number with leading zeros.  Generally it is easier to  | !:
        ! | use CNVRT$("PIC(######)",number)                              | !:
        ! +----------------------------------------------------------------+!
10320   a$=rpt$("#",length)
10330   fnleadzero$=cnvrt$("pic("&a$&")",number)
10340 fnend 
10350 ! -----------------------------
10500 def library fnchrmat$(mat chrmat$,mat nummat,format$;blanks) !:
        ! +----------------------------------------------------------------+!:
        ! |  Change a numeric matrix into a formatted text matrix         | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
10510 ! CHRMAT$  - chracter matrix that will be output !:
        ! NUMMAT   - Numeric matrix to be converted !:
        ! FORMAT$  - conversion format such as PIC(ZZ#.##) !:
        ! BLANKS   - optional flag to make a zero value into !:
        !            a blank of length blank
10520   mat chrmat$(udim(mat nummat))
10530   for a=1 to udim(nummat)
10540     if nummat(a)=0 and blanks then chrmat$(a)=rpt$(" ",blanks) else chrmat$(a)=cnvrt$(format$,nummat(a))
10550   next a
10560 fnend 
10690 ! -----------------------------------------
10700 def library fnopen(&flnm$,&flpath$;printdesc$*80,llen,printtype$,save_days,_pageo) !:
        !    !:
        ! | Used to open a file for archiving prior to printing         | !:
        ! | CLose the file after creating and then use FNPRINT to       | !:
        ! | send it to a printer, leaving the archived file available   | !:
        ! | for reprint using the REPRINT utility                       | !:
        ! | Creates a log file for use in FNREPRINT                     | !:
        !    !
10701 ! flnm$   seed for retruned name                  !:
        ! flpath$  where to store archived file    !:
        ! printdesc$  Description for pr log    !:
        ! llen     recl - if 0 or null then EOL=NONE !:
        ! PRINTTYPE$ DIRECT for PCL only  !:
        ! SAVE_DAYS  number of days to keep archived reports 0 is forever !:
        ! PAGEO    PAGEOVERFLOW NUMBER if not using system default
10702   dim eol$*50
10703 ! IF NOT ESC THEN EXECUTE "PROC=*"&env$("PD")&"Core\fnsnap\tt"
10710   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnnextfil$
10720   flpath$=trim$(lwrc$(flpath$))
10730   if trim$(printdesc$)<="" then askdesc=1 else askdesc=0
10740   if not flpath$(len(flpath$):len(flpath$))="\" then flpath$=flpath$&"\"
10745   execute "mkdir "&flpath$ ioerr 10746
10746   execute "mkdir "&flpath$&date$("CYM") ioerr 10747
10747   flpath$=flpath$&date$("cym")&"\"
10750   flnm$=fnnextfil$(flnm$,flpath$)
10760   filno=10
10770   if file(filno)>-1 then filno+=1 : goto 10770
10780   if uprc$(printtype$)="DIRECT" then printtype$="DIRECT" !:
        else if uprc$(printtype$)="MATRIX" then printtype$="MATRIX" !:
        else if uprc$(printtype$)="MICR" then printtype$="MICR" !:
        else printtype$="ANY"
10790 if llen<1 then !:
        eol$=",EOL=NONE" else !:
        if llen=0 then eol$=",recl=32000" else !:
          eol$=",recl="&str$(llen)
10795 if pageo>0 then !:
        _pageo$=",pageoflow="&str$(pageo) else !:
        _pageo$=""
10800 open #filno: "name="&flpath$&flnm$&_pageo$&eol$&",replace",display,output 
10810 crlf$=chr$(13)&chr$(10) !:
      ff$=chr$(12) !:
      tab$=chr$(9)
10820 MAKE_LOG: ! Create a log file for saved reports
10830 dim printdesc$*200,_pageo$*50
10840 reportlog=10
10850 if file(reportlog)>-1 then reportlog+=1 : goto 10850
10860 if askdesc then 
10870   open #reportlog: "srow=10,scol=10,rows=5,cols=40,border=s,picture=NONE,parent=0,font.LABELS=Swiss:medium",display,outin 
10880   pr #reportlog,fields "2,2,C 20,N/W:T": "Report description"
10890   rinput #reportlog,fields "3,2,V 80,N/W:W": printdesc$
10900   if fkey=99 then goto 10910 else if not fkey then goto 10880 else goto 10910
10910 close #reportlog: 
10920 end if 
10925 if save_days>0 then del_day$=str$(days(date)+save_days) else del_day$="0"
10940 open #reportlog: "name=reportlog.fil,recl=300,kfname=reportlog.idx,kps=112,kln=75,use",internal,outin,keyed 
10941 FRMLOG: form c 8,c 3,c 40,3*c 20,c 15,c 60,3*zd 6,c 10,c 80
10942 delby$="" !:
      delday=0
10944 rewrite #reportlog,using FRMLOG,key=rpad$(lwrc$(flnm$),15)&rpad$(lwrc$(flpath$),60): env$("MENU_NAME"),env$("MENU_SEQ"),env$("PROGRAM"),login_name$(1:20),env$("USER_NAME")(1:20),delby$(1:20),lwrc$(flnm$),lwrc$(flpath$),days(date),save_days,delday,printtype$,printdesc$ nokey 10946
10945 goto 10950
10946 write #reportlog,using FRMLOG: env$("MENU_NAME"),env$("MENU_SEQ"),env$("PROGRAM"),login_name$(1:20),env$("USER_NAME")(1:20),delby$(1:20),lwrc$(flnm$),lwrc$(flpath$),days(date),save_days,delday,printtype$,printdesc$
10950 close #reportlog: 
10960 fnopen=filno
10970 end def 
11000 ! ------------------------------------------
11010 def library fnprint(filnm$*100,printer$*100) !:
        !    !:
        ! | Sends a stored pr file to a designated printer           | !:
        ! |                                                             | !:
        !    !
11020   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fntype,fnltype,fnsrchcrit$,fnlistspec$,fnpfkeyline
11025   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle
11030 !  pFILE=10
11040 !  IF FILE(PFILE)>-1 THEN pFILE+=1 : GOTO 11040
11050   open #(pfile:=fngethandle): "name="&printer$&",eol=NONE",display,output 
11060   fntype(filnm$,pfile)
11070   close #pfile: ioerr 11080
11080 end def 
11090 ! ------------------------------------------
11100 def library fnreprint(;all,logname$*100,logkey$*100) !:
        !    !:
        ! | Displays a dialog listing of available stored files and     | !:
        ! | allows user to reprint to a selected printer                | !:
        !    !
11110 ! ALL         Show all entries regardless of security rights      !:
        ! LOGNAME$    File name of the log file                           !:
        ! LOGKEY$     File name of the index file for the logfile
11115   if not esc then execute "PROC="&env$("PD")&"Core\fnsnap\tt"
11120   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fntype,fnltype,fndelrow,fndelrow$,fnpfkeyline,fnlistspec$,fnmenuaccess,fncleanlog,fnprint,fnwinbuttons,fnsrtnary,fnsrtary
11122   library env$("PD")&"Core\fnsnap\rtflib.dll": fnlistprint
11130   if not esc then execute "PROC="&env$("PD")&"Core\fnsnap\tt"
11140   if logname$<="" or not exists(logname$) then !:
          logname$="reportlog.fil" !:
          logkey$="reportlog.idx"
11150   reportog=10
11155   if not exists(logname$) then !:
          msgbox("No report log file exists.") !:
          goto ZZREPRINT
11160   if file(reportlog)>-1 then reportlog+=1: goto 11160
11170   open #reportlog: "name="&logname$&",kfname="&logkey$&",shr",internal,outin,keyed 
11180   dim dmu$(1),dsq$(1),dpg$(1)*40,dnm$(1)*50,dapath$(1)*65,dpath$(1)*65,ddate(1),dtyp$(1)*15,ddsc$(1)*80,duser$(1)*20,ddel(1),d$*2000
11190   dim dpg$*40,dlnm$*20,dunm$*20,ddelby$*20,dnm$*50,dflpath$*60,dpath$*65,ddate,dtyp$*15,ddsc$*80,duser$*20,ddel
11200   dim dmnth$(1),dmonth$(1)*15,dqty(1)
11210   mat dmnth$(1)=("") !:
        d=0 !:
        mat dqty(1)=(0)
11220 !    !:
        ! | Read the log file and display a summary by month created of | !:
        ! | available reports for reprint                               | !:
        !    !
11230 REPRINT1: read #reportlog,using FRMLOG,release: dmu$,dsq$,dpg$,dlnm$,dunm$,ddelby$,dflnm$,dflpath$,ddat,sday,ddel,dtyp$,ddsc$ eof REPRINT1A
11240   dmonth$=date$(ddat,"cym") !:
        x=srch(dmnth$,dmonth$) !:
        if x<1 then d+=1 !:
          mat dmnth$(d): mat dqty(d) : mat dmnth(d) !:
          dmnth$(d)=dmonth$ !:
          dmnth(d)=days(date(ddat,"cym")*100+1) !:
          dqty(d)=1 !:
          goto REPRINT1
11250   dqty(x)+=1 !:
        goto REPRINT1
11260 REPRINT1A: !:
        !    !:
        ! | Display the matrix of available reports by month and allow  | !:
        ! | selection of one or more months                             | !:
        !    !
11262   fnsrtnary(mat dmnth,mat dmnth$,desending:=1) !:
        fnsrtnary(mat dqty,mat dmnth$,desending) !:
        fnsrtary(mat dmnth$,dmnth$,desending)
11265   if not d then !:
          msgbox("No records exist in the Report Log") !:
          goto ZREPRINT
11270   dim d1listspec$*50,d1h$(1)*30,diw(1),d1f$(1)*30,d1sel(1)
11280   mat d1h$(2) !:
        d1h$(1)="Month created" !:
        d1h$(2)="Number"
11290   mat d1w(2) !:
        d1w(1)=20 !:
        d1w(2)=10
11300   mat d1f$(2) !:
        d1f$(1)="date(Month d, CY)" !:
        d1f$(2)="pic(ZZZ,ZZ#)"
11310   d1sr=4 !:
        d1sc=20 !:
        d1lrows=min(20,udim(dmnth)+4) !:
        d1lcols=sum(mat d1w)+2 !:
        d1arows=2
11320   mat d1sel(1)=(0)
11330   d1listspec$=fnlistspec$(d1listwin,d1sr,d1sc,d1lrows,d1lcols,d1arows,mat d1h$,mat d1w,mat d1f$,"Select Months","",0,2) ! ;HTEXT$*100,G$,PARENT)
11340   pr #d1listwin, fields d1listspec$&",=R" : (mat dmnth,mat dqty)
11350   mat select(1)=(0) !:
        ! fnLISTSRCH(MAT one$,SRCHSTR$,MAT SELECT) !:
        ! fnLISTSRCHN(MAT three,SRCHSTR$,MAT SELECT) !:
        ! Turn these on to search for stings in matrixes !:
        ! cURFLD(1,MAX(1,SELECT(1)))
11360   fnwinbuttons(-1,"^F4:Clean log (F4) ^Esc:End (Esc)",d1listwin) : ! fnPFKEYLINE(-1,"^F4  Clean log ^Esc  End",D1LISTWIN) !:
        if fr then let curfld(1,fr+1)
11370   input #d1listwin, fields d1listspec$&",rowcnt,sel ": d1x !:
        mat d1sel(d1x) !:
        fk=fkey !:
        fr=currow !:
        input #d1listwin, fields d1listspec$&",rowsub,sel,nowait ": mat d1sel !:
        if fk=201 then fk=0 : else if fk=close then fk=esc !:
          ! parameters are !:
          ! GRID cnt,sub,cell !:
          ! LIST rowsub,rowcnt,cell!:
          ! cur,sel,all
11372 if fk=4 then !:
        restore #reportlog: !:
        fncleanlog(reportlog) !:
        restore #reportlog: : close #d1list: !:
        mat dqty(1)=(0) : mat dmnth(1)=(0) : mat dmnth$(1)=("") : d=0 !:
        goto REPRINT1
11380 if not (fk=0 or fk=esc) then goto 11360
11390 if not fk then 
11400   mat d1sel$(udim(d1sel))
11410   for a=1 to udim(d1sel) !:
          d1sel$(a)=date$(dmnth(d1sel(a)),"cym") !:
        next a
11420   fnpfkeyline(-1," ",d1listwin) !:
        gosub REPRINT2 !:
        restore #reportlog: : close #d1list: !:
        mat dqty(1)=(0) : mat dmnth(1)=(0) : mat dmnth$(1)=("") : d=0 !:
        goto REPRINT1
11430 end if 
11440 close #listwin: !:
      listwin=0
11450 ZREPRINT: close #reportlog: !:
      reportlog=0
11460 ZZREPRINT: fnend 
11470 REPRINT2: !:
      !    !:
      ! | Subroutine of FNREPRINT to display and pr archived       | !:
      ! | reports                                                     | !:
      !    !
11480 restore #reportlog: !:
      d2=0
11490 dim d2mu$(1),d2sq$(1),d2pg$(1)*40,d2lnm$(1)*20,d2unm$(1)*20,d2delby$(2)*20,d2flnm$(1)*15,d2flpath$(1)*60,d2dat(1),d2sdat(1),d2del(1),d2typ$(1)*10,d2dsc$(1)*80,d2del$(1)*16
11500 dim d2pg$*40,d2lnm$*20,d2unm$*20,d2delby$*20,d2flnm$*15,d2flpath$*60,d2typ$*10,d2dsc$*80,d2del$*16
11510 read #reportlog,using FRMLOG,release: d2mu$,d2sq$,d2pg$,d2lnm$,d2unm$,d2delby$,d2flnm$,d2flpath$,d2dat,d2sdat,d2del,d2typ$,d2dsc$ eof REPRINT2A
11520 if srch(mat d1sel$,date$(d2dat,"cym"))<1 then goto 11510
11530 if all then !:
        goto 11540 else !:
        if not fnmenuaccess(d2mu$,d2sq$,d2pg$) then !:
          goto 11510
11540 d2+=1
11550 mat d2mu$(d2) : mat d2sq$(d2) : mat d2pg$(d2) : mat d2lnm$(d2) !:
      mat d2unm$(d2) : mat d2delby$(d2) : mat d2flnm$(d2) !:
      mat d2flpath$(d2) : mat d2dat(d2) : mat d2sdat(d2) : mat d2del(d2) !:
      mat d2typ$(d2) : mat d2dsc$(d2) : mat d2del$(d2)
11560 d2mu$(d2)=d2mu$ : d2sq$(d2)=d2sq$ : d2pg$(d2)=d2pg$ !:
      d2lnm$(d2)=d2lnm$ : d2unm$(d2)=d2unm$ !:
      d2delby$(d2)=d2delby$ : d2flnm$(d2)=d2flnm$ !:
      d2flpath$(d2)=d2flpath$ : d2dat(d2)=d2dat !:
      d2sdat(d2)=d2sdat : d2del(d2)=d2del !:
      d2typ$(d2)=d2typ$ : d2dsc$(d2)=d2dsc$
11570 if d2sdat>0 then !:
        d2del$(d2)=date$(d2dat(d2)+d2sdat,"M/D/CY") else !:
        d2del$(d2)="None"
11580 if d2del>0 then d2del$(d2)="Deleted "&date$(d2del,"m/d/y")
11590 goto 11510
11600 REPRINT2A: !:
      !    !:
      ! | Display the selection of available reports for the selected | !:
      ! | months.                                                     | !:
      !    !
11610 dim d2h$(3)*50,d2w(3),d2f$(3)*50
11620 mat d2h$(5) !:
      d2h$(1)="Date Created" !:
      d2h$(2)="Description" !:
      d2h$(3)="Created by" !:
      d2h$(4)="Delete Date" !:
      d2h$(5)="Printer Type"
11630 mat d2w(5) !:
      d2w(1)=10 !:
      d2w(2)=30 !:
      d2w(3)=12 !:
      d2w(4)=12 !:
      d2w(5)=10
11640 mat d2f$(5) !:
      d2f$(1)="date(m/d/cy)" !:
      d2f$(2)="C 80" !:
      d2f$(3)="c 20" !:
      d2f$(4)="c 16" !:
      d2f$(5)="CU 10"
11650 d2sr=2 : d2sc=3 !:
      d2lrows=min(20,udim(d2dat)+2): d2lcols=sum(mat d2w) !:
      d2arows=1
11660 d2spec$=fnlistspec$(d2win,d2sr,d2sc,d2lrows,d2lcols,d2arows,mat d2h$,mat d2w,mat d2f$,"Select Report","",0,2) ! ;HTEXT$*100,G$,PARENT)
11670 ! 
11680 fnwinbuttons(-1,"^F2:Filter(F2) ^F4:Delete(F4) ^f12:Print ^Esc:End (Esc)",d2win)
11681 ! fnPFKEYLINE(-1,"^Enter  pr file ^F4  Delete ^Esc  End",D2WIN)
11690 pr #d2win,fields d2spec$&",=R": (mat d2dat,mat d2dsc$,mat d2unm$,mat d2del$,mat d2typ$)
11700 input #d2win,fields d2spec$&",rowsub,selone": d2r !:
      f2k=fkey !:
      if f2k=201 then f2k=0
11701 if f2k=2 and wbversion$=>"4.30" then rinput #d2win, fields str$(d2lrows+1)&",2,15/FILTER 10,[D],1,1,FULLROW,ALL": srchstring$ : d2r=1 : goto 11700
11702 if wbversion$>='4.3' and f2k=12 then let fnlistprint(d2win,d2spec$,"Stored Reports for "&date$(d2dat(1),"Month CCYY"),"","Stored Reports [RTFLINE]",mat dummy,nolines:=0,nosort:=0,nototals$="11212",nosubtotal:=1) : goto 11700
11710 if f2k=4 then 
11720   if msgbox("You have elected to permanently delete the stored report "&d2unm$(d2r)&" - "&trim$(d2dsc$(d2r))&".  "&chr$(13)&chr$(13)&"Do you want to delete this report?","Delete Report","yN","QST")=2 then 
11730     if exists(trim$(d2flpath$(d2r))&trim$(d2flnm$(d2r))) then !:
            execute "free "&trim$(d2flpath$(d2r))&trim$(d2flnm$(d2r))
11740     rewrite #reportlog,using "form pos 91,c 20,pos 198,zd 6",key=rpad$(d2flnm$(d2),15)&rpad$(d2flpath$(d2),60): env$("USER_NAME"),days(date)
11750     d2del(d2r)=days(date) !:
          d2del$(d2r)="Deleted "&date$(d2del(d2r),"m/d/y")
11760     curfld(1,d2r+1)
11770 ! 
11780     goto 11690
11790   end if 
11800   curfld(1,d2r+1) : goto 11690
11810 end if 
11820 if not f2k then 
11830   pfile=10
11840   if file(pfile)>-1 then pfile+=1 : goto 11840
11850   setenv("PRINT_DOCUMENT_NAME","Reprinted Report") ! PAUSE
11860   if uprc$(trim$(d2typ$(d2r)))="ANY" then open #pfile: "name=preview:/select,eol=none",display,output  !:
          goto 11900
11870   if uprc$(trim$(d2typ$(d2r)))="DIRECT" then let msgbox("The report that you are reprinting contains characters that are not compatable with the preview mode."&cr$&cr$&"The report must be sent directly to a PCL compatable laser printer.  Please select an appropriate printer for this report.") !:
          open #pfile: "name=direct:/select,eol=none",display,output  !:
          goto 11900
11880   if uprc$(trim$(d2typ$(d2r)))="MATRIX" then let msgbox("The report that you are reprinting contains characters that are not compatable with the preview mode."&cr$&cr$&"The report must be sent directly to a MATRIX printer.  "&c$&cr$&"Please select an appropriate printer for this report.") !:
          open #pfile: "name=direct:/select,eol=none",display,output  !:
          goto 11900
11882   if uprc$(trim$(d2typ$(d2r)))="MICR" then let msgbox("The report that you are reprinting contains characters that are not compatable with the preview mode."&cr$&cr$&"The report must be sent directly to a MICR capable printer.  "&c$&cr$&"Please select an appropriate printer for this report.") !:
          open #pfile: "name=direct:/select,eol=none",display,output  !:
          goto 11900
11890   msgbox("An invalid PRINTER TYPE was specified in the REPORTLOG file.  You must correct the report type before printing can occur.") !:
        goto 11690
11900   if exists(trim$(d2flpath$(d2r))&trim$(d2flnm$(d2r))) then 
11910     fntype(trim$(d2flpath$(d2r))&trim$(d2flnm$(d2r)),pfile)
11920   else 
11930     if msgbox("The selected file "&trim$(d2flpath$(d2r))&trim$(d2flnm$(d2r))&" does not exist."&cr$&cr$&"Do you want to remove the entry from the log?","No Report",yn$,"QST")=2 then !:
            delete #reportlog,key=d2flnm$(d2r)&d2flpath$(d2r): !:
            close #d2win: !:
            goto REPRINT2
11940   end if 
11950   close #pfile: ioerr 11955
11952   setenv("PRINT_DOCUMENT_NAME","")
11955   if err=6245 then d2typ$(d2r)="DIRECT" : goto 11870
11960   goto 11690
11970 end if 
11980 close #d2win: 
11990 return 
12000 def library fnmenuaccess(mname$*10,mseq$*3,mpgm$*50) !:
        !    !:
        ! | Check menu access rights for a reprint request against the  | !:
        ! | original access rights in WORKMENU                          | !:
        !    !
12010   library env$("PD")&"workmenu.br": fnmenushow
12020   local=val(env$("LOCAL"))
12030   fnmenushow(trim$(mname$),mseq$,1) !:
        !    !:
        ! | The 1 at the end prevents WORKMENU form displaying the menu | !:
        ! | the function FNMENUSHO simply reinstates the menu to LOCAL  | !:
        !    !
12040   lr=lrec(local) !:
        restore #local, rec=1: !:
        r=0
12050   dim mtype$*150,mopts$(1)*150,mstat$(1)*150,mpgm$(1)*150,upgm$(1)*150,mlvl(1)
12060   fnmenuaccess=0
12070 ! PAUSE
12080   read #local: mtype$ eof ZMENUACCESS
12090   if not mtype$="MENU" then goto 12080
12100   reread #local: mtype$,ma$,mb$,mc$,mopts
12110 ! PAUSE
12120   if not (trim$(uprc$(mname$))=trim$(uprc$(ma$)) and mseq$=mb$) then goto 12080
12130   mat mopts$(mopts) !:
        mat mpgm$(mopts) !:
        mat upgm$(mopts) !:
        mat mstat$(mopts) !:
        mat mlvl(mopts)
12140   reread #local: mtype$,ma$,mb$,mc$,mopts,mat mopts$,mat mpgm$,mat mstat$,mat mlvl
12150   for a=1 to udim(mpgm$) !:
          upgm$(a)=trim$(lwrc$(mpgm$(a)(2:len(mpgm$(a))))) !:
        next a
12160   m=srch(mat upgm$,lwrc$(mpgm$))
12170   if m<1 then goto ZMENUACCESS
12180   useraccess=val(env$("USERACCESS")) !:
        read #useraccess,using "Form pos 74,n 1",key=rpad$(env$("USER_NAME"),10)&rpad$(mname$,10)&mseq$&rpad$(mpgm$,50): mlvl nokey 12200
12190   if mlvl<2 then goto ZMENUACCESS
12200   fnmenuaccess=1
12210 ZMENUACCESS: fnend 
12220 def library fncleanlog(;reportlog) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
12230   if reportlog>0 then cleanlog=1 : goto CLEANLOG1
12240   if not esc then execute "PROC="&env$("PD")&"Core\fnsnap\tt"
12250   if logname$<="" or not exists(logname$) then !:
          logname$="reportlog.fil" !:
          logkey$="reportlog.idx"
12260   reportog=10
12270   if file(reportlog)>-1 then reportlog+=1: goto 12270
12280   open #reportlog: "name="&logname$&",kfname="&logkey$&",shr,release",internal,outin,keyed 
12290 CLEANLOG1: read #reportlog,using FRMLOG: dmu$,dsq$,dpg$,dlnm$,dunm$,ddelby$,dflnm$,dflpath$,ddat,sday,ddel,dtyp$,ddsc$ eof ZCLEANLOG
12295   if not sday then !:
          sday=30 !:
          rewrite #reportlog,using "form pos 193,zd 6",key=dflnm$&dflpath$: sday
12300   if not exists(trim$(dflpath$)&trim$(dflnm$)) then 
12310     if days(date)-ddel<30 and ddel then !:
            goto 12320 else !:
            delete #reportlog,key=dflnm$&dflpath$: !:
            goto 12370
12320   end if 
12322   if exists(trim$(dflpath$)&trim$(dflnm$)) then 
12323     if ddel>0 then rewrite #reportlog,using "form pos 199,zd 6",key=dflnm$&dflpath$: 0
12324   end if 
12330   if ddat+sday<days(date) and not ddel then 
12340     execute "free "&trim$(dflpath$)&trim$(dflnm$) ioerr 12350
12350     rewrite #reportlog,using "form pos 199,zd 6",key=dflnm$&dflpath$: days(date)
12360   end if 
12370   goto 12290
12380 ZCLEANLOG: if cleanlog then !:
          cleanlog=0 !:
          goto 12410
12390   close #reportlog: 
12400   reportlog=0
12410 end def 
12500 def library fnvh$(f$) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
12510   f$=uprc$(f$)
12520   if pos(f$,"V")>0 then 
12530     f=val(srep$(f$,"V","")) !:
          f$=lwrc$(f$)&str$(120/f)&"H"
12540   else 
12550     f=val(srep$(f$,"H","")) !:
          f$=lwrc$(f$)&str$(120/f)&"V"
12560   end if 
12570   fnvh$=f$
12580 end def 
12600 def library fnformtext(_filnum,_v,_h,_ll,_lm,_lh,_lw,_lf$*50,_lfs,_tf$*100) !:
        !    !:
        ! | pr multicolumn text                                      | !:
        ! | _V   Vertical start in inches                               | !:
        ! | _H   Horizontal start in inches                             | !:
        ! | _LL  Line length in number of characters per line           | !:
        ! | _LM  Number of inches per column                            | !:
        ! | _LH  Number inches per line 1/8 1/13 etc.                   | !:
        ! | _LW  Width of each column in inches (Line Width)            | !:
        ! | _LF$ Font to be used as in "Arial Narrow"                   | !:
        ! | _LFS Font point size vertical height                        | !:
        ! | _TF$ Text file name for text source                         | !:
        !    !
12610   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintnwp$,fnprintbox$,fndrawbox$,fnlines$,fngethandle
12620   execute "PROC="&env$("PD")&"Core\fnsnap\tt"
12630   dim _a$*2000,_lt$*20000
12640   v=_v : h=_h !:
        vh=_lm : ll=_ll : vx=_lm/_lh : vl=0 !:
        !    !:
        ! | pr the instruction text box                              | !:
        ! |                                                             | !:
        !    !
12650   open #(in:=fngethandle): "name="&_tf$,display,input 
12660   pr #_filnum: fnlines$(0)&"[TOP("&str$(2+_lines)&")][BOXVERTICALS][SETFONT("&_lf$&")]"&esc$&"(s"&str$(_lfs)&"V"
12670   linput #in: _a$ eof ZLTEXT
12680   if len(_a$)>_ll-1 then !:
          _ae=min(len(_a$),pos(_a$(1:_ll)," ",-1)) else !:
          _ae=len(_a$)
12690   pr #_filnum: fnprintnwp$(v,h,"L",trim$(_a$(1:_ae))) !:
        v+=_lh : vl+=1
12700   if vl>vx then gosub NEWLCOL
12710   if _ae>0 and _ae<len(trim$(_a$)) then _a$=_a$(_ae+1:len(_a$)) : goto 12680
12720   goto 12670
12730 NEWLCOL: ! 
12740   v=_v : h+=_lw : vl=1
12760   return 
12770 ZLTEXT: close #in: 
12790 fnend 
12800 def library fnformtext$*20000(_v,_h,_ll,_lm,_lh,_lw,_lf$*50,_lfs,_tf$*100) !:
        !    !:
        ! | pr multicolumn text                                      | !:
        ! | _V   Vertical start in inches                               | !:
        ! | _H   Horizontal start in inches                             | !:
        ! | _LL  Line length in number of characters per line           | !:
        ! | _LM  Number of inches per column                            | !:
        ! | _LH  Number inches per line 1/8 1/13 etc.                   | !:
        ! | _LW  Width of each column in inches (Line Width)            | !:
        ! | _LF$ Font to be used as in "Arial Narrow"                   | !:
        ! | _LFS Font point size vertical height                        | !:
        ! | _TF$ Text file name for text source                         | !:
        !    !
12810   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintnwp$,fnprintbox$,fndrawbox$,fnlines$,fngethandle
12820   execute "PROC="&env$("PD")&"Core\fnsnap\tt"
12830   dim _a$*2000,_lt$*20000
12840   v=_v : h=_h !:
        vh=_lm : ll=_ll : vx=_lm/_lh : vl=0 !:
        !    !:
        ! | pr the instruction text box                              | !:
        ! |                                                             | !:
        !    !
12850   open #(in:=fngethandle): "name="&_tf$,display,input 
12860   _lt$=fnlines$(0)&"[TOP("&str$(2+_lines)&")][BOXVERTICALS][SETFONT("&_lf$&")]"&esc$&"(s"&str$(_lfs)&"V"
12870   linput #in: _a$ eof Z_LTEXT
12880   if len(_a$)>_ll-1 then !:
          _ae=min(len(_a$),pos(_a$(1:_ll)," ",-1)) else !:
          _ae=len(_a$)
12890   _lt$=_lt$&fnprintnwp$(v,h,"L",trim$(_a$(1:_ae))) !:
        v+=_lh : vl+=1
12900   if vl>vx then gosub NEW_LCOL
12910   if _ae>0 and _ae<len(trim$(_a$)) then _a$=_a$(_ae+1:len(_a$)) : goto 12880
12920   goto 12870
12930 NEW_LCOL: ! 
12940   v=_v : h+=_lw : vl=1
12950 ! PAUSE
12960   return 
12970 Z_LTEXT: close #in: 
12980   fnformtext$=_lt$
12990   _lt$=""
13000 fnend 
29900 def library fnaddrow(mat add,addrow;addamt) !:
        !    !:
        ! | Add row to ADD from mat ADD and increase mat add by one row | !:
        ! |                                                             | !:
        !    !
29910   a=udim(mat add)
29920   mat add(a+1)
29930   if addrow=a then !:
          add(addrow+1)=addamt else !:
          mat add(addrow+1:a+1)=add(addrow:a) !:
          add(addrow)=addamt
29940 fnend 
29950 def library fnaddrow$(mat add$,addrow;addtxt$*200) !:
        !    !:
        ! | Add row to ADD from mat ADD and increase mat add by one row | !:
        ! |                                                             | !:
        !    !
29960   a=udim(mat add$)
29970   mat add$(a+1)
29980   if addrow=a then !:
          add$(addrow+1)=addtxt$ else !:
          mat add$(addrow+1:a+1)=add$(addrow:a) !:
          add$(addrow)=addtxt$
29990 fnend 
30000 ! --------------
30001 def library fnrowdel(mat x,row)
30002   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fndelrow
30003   fndelrow(mat x,row)
30004 fnend 
30010 def library fndelrow(mat x,row) !:
        !    !:
        ! | Delete row ROW from numeric array X and shrink the array    | !:
        ! | by one row                                                  | !:
        !    !
30012   onecol=rx=cx=1
30014   on error goto 30018
30016   rx=udim(mat x,1)
30018   on error goto 30022
30020   cx=udim(mat x,2)
30021   onecol=0
30022   on error system 
30024   dim l(1,1)
30026   if row>rx then goto ZDELNMAT
30030   mat l(rx*cx)
30032   mat x(rx*cx)
30034   mat l=x
30036   for adel=(row-1)*cx+1 to (rx-1)*cx : l(adel)=x(adel+cx) : next adel
30038   mat l(rx-1,cx) : mat x(rx-1,cx)
30040   mat x=l
30041   if onecol then mat x(udim(x))
30042 ZDELNMAT: fnend 
30060 ! --------------
30065 def fnrowdel$(mat x$,row)
30066   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fndelrow$
30067   fndelrow$(mat x$,row)
30068 fnend 
30070 def library fndelrow$(mat x$,row) !:
        !    !:
        ! | Delete a ROW from an array and shorten the array by one ROW | !:
        ! |                                                             | !:
        !    !
30072   onecol=rx=cx=1
30074   on error goto 30078
30076   rx=udim(mat x$,1)
30078   on error goto 30082
30080   cx=udim(mat x$,2)
30081   onecol=0
30082   on error system 
30084   dim l$(1,1)*10000
30086   if row>rx then goto ZDELSMAT
30090   mat l$(rx*cx)
30092   mat x$(rx*cx)
30094   mat l$=x$
30096   for adel=(row-1)*cx+1 to (rx-1)*cx : l$(adel)=x$(adel+cx) : next adel
30098   mat l$(rx-1,cx) : mat x$(rx-1,cx)
30100   mat x$=l$
30101   if onecol then mat x$(udim(x$))
30102 ZDELSMAT: fnend 
30110 ! ------------------------------------------------
30150 def library fncf$*2000(a$*2000) !:
        !    !:
        ! | Condense a FORM statement                                   | !:
        ! |                                                             | !:
        !    !
30160   dim as$(1)*2000,an(10),ap(1),acf$*2000
30170   x=y=0 !:
        a$=uprc$(a$)
30180 STARTCF: x=pos(a$,",") !:
        !    !:
        ! |  Determine the size of the current parameter                | !:
        ! |                                                             | !:
        !    !
30190   if x>0 then !:
          xa=x else !:
          xa=len(a$)+1
30200   y+=1 !:
        mat as$(y) : mat an(y) : mat ap(y)
30210   as$(y)=a$(1:pos(a$," ")-1)
30220   an(y)=val(trim$(srep$(a$(1:xa-1),as$(y),"")))
30230   ap(y)=sum(mat an)-an(y)+1
30240   if x>0 then !:
          a$=a$(xa+1:len(a$)) !:
          goto 30180 !:
          !    !:
          ! | Not the last parameter of the statement                     | !:
          ! |                                                             | !:
          !    !
30250   x=0
30260   for a=1 to udim(mat as$)
30270     if a<udim(as$) and as$(a)=as$(a+1) and an(a)=an(a+1) then 
30280       x+=1
30290     else 
30300       if x>0 then !:
              acf$=acf$&str$(x+1)&"*"&as$(a)&" "&str$(an(a))&"," else !:
              acf$=acf$&as$(a)&" "&str$(an(a))&","
30310       x=0
30320     end if 
30330   next a
30340   acf$(len(acf$):len(acf$))="" !:
        !    !:
        ! | Trim the final comma from the string                        | !:
        ! |                                                             | !:
        !    !
30350   fncf$=acf$
30360 fnend 
30370 def library fncform$*2000(a$*2000) !:
        !    !:
        ! | Make a compile FORM statement using FNCF$                   | !:
        ! |                                                             | !:
        !    !
30375   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fncf$
30380   fncform$=cform$("FORM "&fncf$(a$))
30390 fnend 
30400 ! ================================================================
30410 ! 
30420 def library fnsrchcrit$*50(sr$,sc$,lrows,lcols,parent;message$*100) !:
        ! +----------------------------------------------------------------+!:
        ! | Open a query box for search string and pass back              | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
30430   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwinhead
30440   dim srchcrit$*100
30450   critwin=300
30460   if file(critwin)>-1 then !:
          critwin+=1 !:
          goto 30460
30462   if wbversion$>"4.19z" then 
30465     open #critwin: "srow="&str$(val(sr$)*50)&",scol="&str$(val(sc$)*50)&",rows="&str$(lrows+2)&",cols="&str$(lcols)&",Parent=NONE,caption=Enter search term",display,outin  ! pane.bmp
30468   else 
30470     open #critwin: "srow="&sr$&",scol="&sc$&",erow="&str$(val(sr$)+lrows+1)&",ecol="&str$(val(sc$)+lcols)&",Parent="&str$(parent)&",picture="&env$("PD")&"window.gif",display,outin  ! pane.bmp
30480     fnwinhead(critwin,message$,lcols-1)
30490   end if 
30500   if wbversion$>"4.19z" then !:
          input #critwin,fields "2,2,50/cu "&str$(lcols-4): srchcrit$ !:
          srchcrit$=trim$(srchcrit$) !:
        else !:
          input #critwin,fields "3,2,50/cu "&str$(lcols-4): srchcrit$ !:
          srchcrit$=trim$(srchcrit$)
30510   close #critwin: 
30520   critwin=0 !:
        fnsrchcrit$=trim$(srchcrit$)
30530 fnend 
30590 ! 
30600 def library fnlistsrch(mat l$,srchstr$*100,mat select;strt) !:
        ! +----------------------------------------------------------------+!:
        ! |  Search a matrix for elements that match a string             | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
30610 ! MAT L$ is the matrix to search for matches
30620 ! SRCHSTR$ is the string for which to search
30630 ! MAT SELECT is the returned list of elements in which there is a match!:
        ! STRT if TRUE (non-zero) matches only the first chatacters of the string
30640   srchstr$=uprc$(trim$(srchstr$))
30650   if select(1)>0 then !:
          as=udim(select) else !:
          as=0
30660   for a=1 to udim(mat l$)
30670     if not strt then ax=pos(uprc$(l$(a)),srchstr$) else !:
            if uprc$(l$(a)(1:len(srchstr$)))=srchstr$ then ax=1 else !:
              ax=0
30680     if ax>0 then 
30690       as+=1 !:
            mat select(as) !:
            select(as)=a
30700 !  INPUT FIELDS "23,60,c 1": P$
30710     end if 
30720   next a
30730 ZLISTSRCH: fnlistsrch=as
30740 !   INPUT FIELDS "23,60,c 1": P$
30750   if udim(mat select)>1 then 
30760     mat temp(udim(select))=aidx(select)
30770     mat aselect(1)
30780     a=1 !:
          b=0
30790     if select(temp(a))>0 then b+=1 !:
            mat aselect(b) !:
            aselect(b)=select(temp(a))
30800     a+=1 !:
          if a>udim(select) then goto 30830
30810     if b>0 and select(temp(a))=aselect(b) then goto 30800
30820     goto 30790
30830     mat select(udim(aselect))
30840     mat select=aselect
30850   end if 
30860 fnend 
30870 def library fnlistsrchn(mat l,srchstr$*100,mat select;strt,smask$) !:
        ! +----------------------------------------------------------------+!:
        ! |  Search a numeric matrix for elements that match a string     | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
30880 ! convert numeric to string and search                              !:
        ! strt if TRUE matches only the beginning of the string             !:
        ! SMASK if not null is the format that the number will be converted !:
        ! to for the search
30890   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnlistsrch
30900   lx=udim(mat l)
30910   mat l$(lx)
30920   la=0
30930 LISTSRCHN_1: la+=1
30940   if not strt then !:
          l$(la)=str$(l(la)) else !:
          l$(la)=cnvrt$(smask$,l(la))
30950   if la<lx then goto LISTSRCHN_1
30960   fnlistsrch(mat l$,srchstr$,mat select,strt)
30970 fnend 
31000 def library fnprogress(&pct_windev,pct_total,pct_done;sr$,caption$*55) !:
        ! +----------------------------------------------------------------+!:
        ! |  Display a progess bar for a task as it progresses            | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
31010 ! pct_windev      window number to open     !:
        ! pct_total       number of items to process !:
        ! pct_done        number of items processed !:
        ! SR$             Starting row number  !:
        ! caption$        window title
31012   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprog
31014   prow=10 : pcol=3 : fnprog(prow,pcol,pct_done,pct_total) : goto 31140
31020   dim readrec$*70
31030   if trim$(caption$)<="" then caption$="BUILDING DATA FILE"
31040   if not pct_windev then 
31045     execute "config attribute [BLANK]N/W:T,font=SYSTEMPC"
31050     pct_windev=windev
31060     if file(pct_windev)>-1 then pct_windev+=1 : goto 31060
31070     if sr$="" then sr$="10"
31080     fnwin(sr$,"5",str$(val(sr$)+2),"72",srep$(caption$,","," "),"DS[X]","[W]",pct_windev,1)
31090     pctwrk$="1,5,C 62,N/W:T": !:
          pr #pct_windev,fields pctwrk$: "0     1     2     3     4     5     6     7     8     9     0"
31100     pctwrk$="2,5,C 62,N/W:HRGB": !:
          pr #pct_windev,fields pctwrk$: rpt$(" ",62)
31110   end if 
31120 ! rEADREC$=RPT$(CHR$(219),MIN(60,ROUND((PCT_DONE/PCT_TOTAL)*60,0)))
31121   pctwrk$="2,4,c "&str$(ip(min(60,round((pct_done/pct_total)*60,0))))&",[BLANK]"
31122   readrec$=rpt$(" ",min(60,round((pct_done/pct_total)*60,0)))
31130   pr #pct_windev,fields pctwrk$: readrec$
31140 fnend 
31150 def library fnweekday$(weekday) !:
        ! +----------------------------------------------------------------+!:
        ! | Return the Text day of the week for a days date               | !:
        ! |  WEEKDAY is a DAYS date                                       | !:
        ! +----------------------------------------------------------------+!
31160   dim weekday$(7)
31170   weekday$(1)="Sunday" !:
        weekday$(2)="Monday" !:
        weekday$(3)="Tuesday" !:
        weekday$(4)="Wednesday" !:
        weekday$(5)="Thursday" !:
        weekday$(6)="Friday" !:
        weekday$(7)="Saturday"
31180   fnweekday$=weekday$(fp(weekday/7)*7+1)
31190 fnend 
31200 !    !:
      ! | Send MACRO/FONT/GRAPHIC to open pr file                  | !:
      ! 
31210 def library fnprintform$*40(filnum,formfile,shortname$) !:
        ! +----------------------------------------------------------------+!:
        ! | Send MACRO/FONT/GRAPHIC/FORM to open pr file               | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
31220 ! FILNUM      =  Open pr file that includes recl=32000 and eol=none !:
        ! FORMFILE    =  open keyed file that contains the MACRO/GRAPHIC/FONT  !:
        ! SHORTNAMR$  =  Key used to call the specific item from keyed FILNUM
31230   dim frmform$*200,formin$*32000,pfont$*40,pdesc$*30,mfont$*40
31240   crlf$=chr$(13)&chr$(10) !:
        esc$=chr$(27)
31250 FRMSTART: form c 8,c 3,n 7,n 5,c 1,c 40,c 30
31260   restore #formfile,key=shortname$&"001": nokey ZPRINTFORM
31270   read #formfile,using FRMSTART: pshort$,pseq$,plen,pmacro,ptype$,pfont$,pdesc$ eof ZPRINTFORM
31290   if not uprc$(pshort$)=uprc$(shortname$) then goto ZPRINTFORM
31310   plen$=str$(plen)
31320   frmform$=cform$("form pos 95,c "&plen$)
31330   reread #formfile,using frmform$: formin$
31340   if pmacro and pseq$="001" and uprc$(ptype$)="P" then !:
          macrono=pmacro : mtype$=uprc$(ptype$) !:
          ! Do not do anything this is a straight page print
31350   if pseq$="001" and ptype$="F" then mtype$=uprc$(ptype$) !:
          mfont$=pfont$ !:
          pr #filnum: crlf$ !:
          pr #filnum: esc$&"*c"&str$(pmacro)&"D"
31360   if pmacro and pseq$="001" and uprc$(ptype$)="M" then !:
          macrono=pmacro : mtype$=uprc$(ptype$) !:
          pr #filnum: esc$&"&f"&str$(macrono)&"y0X"&esc$&"&l-35u-180Z"
31370   pr #filnum: formin$
31380   goto 31270
31390 ZPRINTFORM: if mtype$="M" and macrono then !:
          fnprintform$=esc$&"&f"&str$(macrono)&"y3X" !:
          pr #filnum: esc$&"&f1X"&esc$&"&"&str$(macrono)&"y10X" !:
        else if uprc$(mtype$)="F" then let fnprintform$=trim$(mfont$) !:
          pr #filnum: esc$&"*c5F"
31400 macrono=0
31410 fnend 
31430 def library fnmacrotemp(filnum,macnr$*20;delete) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
31432   if delete then !:
          pr #filnum: srep$(macnr$,"3X","8X") else !:
          pr #filnum: srep$(macnr$,"3X","9X")
31434 fnend 
31440 def library fnmacroperm(filnum,macnr$*40;delete) !:
        !    !:
        ! |  Makes a temporary MACRO permanent or optionally removes    | !:
        ! |  a permanent MACRO fro the HP printer                       | !:
        !    !
31442   if delete then !:
          pr #filnum: srep$(macnr$,"3X","8X") else !:
          pr #filnum: srep$(macnr$,"3X","10X")
31444 fnend 
31500 def library fnprog(prow,pcol,pcur,ptot) !:
        !    !:
        ! |  Displays a progress bar.  FNWAITWIN and FNWAITBAR are a    | !:
        ! |  a better option                                            | !:
        !    !
31505   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle
31510   progx=int(min(ptot,pcur)/ptot*10)
31520   if progx>y then 
31530     y=progx
31540     if not progbar then 
31550 ! 
31560 ! 
31570       open #(progwin:=fngethandle): "srow="&str$(prow)&",scol="&str$(pcol)&",rows=12,cols=3,picture="&env$("PD")&"icons\black.bmp",display,outin 
31580       execute "config attribute [PROG]n/#FFFFFF:T,FONT=ARIAL:SLANT:SMALL"
31590       for apg=1 to 10 : pr #progwin, fields str$(12-apg)&",1,c 2,[PROG]": str$(apg) : next apg
31620       srow=2+10-progx !:
            open #(progbar:=fngethandle): "srow="&str$(srow)&",scol=2,rows="&str$(max(2,progx))&",cols=2,picture="&env$("PD")&"icons\green.bmp",display,outin 
31630     end if 
31640     if progx<6 and progx>=0 then 
31650       close #progbar: 
31660       srow=2+10-progx !:
            open #progbar: "srow="&str$(srow)&",scol=2,rows="&str$(max(1,progx))&",cols=1,parent="&str$(progwin)&",picture="&env$("PD")&"icons\green.bmp",display,outin 
31670     end if 
31680     if progx<8 and progx>5 then 
31690       close #progbar: 
31700       srow=2+10-progx !:
            open #progbar: "srow="&str$(srow)&",scol=2,rows="&str$(max(1,progx))&",cols=1,parent="&str$(progwin)&",picture="&env$("PD")&"icons\yellow.bmp",display,outin 
31710     end if 
31720     if progx>7 then 
31730       close #progbar: 
31740       srow=2+10-progx !:
            open #progbar: "srow="&str$(srow)&",scol=2,rows="&str$(max(1,progx))&",cols=1,parent="&str$(progwin)&",picture="&env$("PD")&"icons\red.bmp",display,outin 
31750     end if 
31760   end if 
31770   if pcur>=ptot then close #progwin: !:
          progwin=progbar=0
31780   fnprog=progwin
31790 fnend 
31800 def library fnmakepcl(infile$*100,outfile$*100;pcllines) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
31801 !    !:
        ! | Converts a PRN file created with an HP6L driver to a PCL    | !:
        ! | file ready to be made into a MACRO or overlay see FNPRINTFORM³!:
        !    !
31802 ! PCLLINES sets a number of lines at the top for the MACRO !:
        ! the default is zero
31805   dim pcl$*32000
31810   infile=1
31811   if file(infile)>-1 then infile+=1 : goto 31811
31812   open #infile: "name="&infile$&",recl=1",external,input 
31815   inrec=lrec(infile)-12
31820   close #infile: 
31825   lr=min(5000,inrec)
31830   dim inform$*500
31835   inform$=cform$("form c "&str$(lr))
31840   open #infile: "name="&infile$&",recl="&str$(lr),external,input 
31845   outfile=infile+1
31846   if file(outfile)>-1 then outfile+=1 : goto 31846
31847   open #outfile: "name="&outfile$&",eol=NONE,replace",display,output 
31848   acnt=0
31850   if acnt*lr+lr<inrec then 
31855     read #infile,using inform$: pcl$
31860     acnt+=1
31865     if acnt=1 and pos(pcl$,chr$(27)&"E") then 
31870       x=pos(pcl$,chr$(27)&"E")
31875       pcl$=pcl$(x+2:lr)
31880       pcl$=srep$(pcl$,chr$(27)&"&l0O","")
31885       pcl$=srep$(pcl$,chr$(27)&"&l1H","")
31890       pcl$=srep$(pcl$,chr$(27)&"&l1X","")
31895       pcl$=srep$(pcl$,chr$(27)&"*r0F","")
31900 ! pCL$=SREP$(PCL$,CHR$(27)&"&l2a8c1E",CHR$(27)&"&l8c1E")
31901       pcl$=srep$(pcl$,chr$(27)&"&l2a8c1E",chr$(27)&"&l8c"&str$(pcllines)&"E")
31902 !   IF PCLLINES>0 THEN pr #OUTFILE: CHR$(27)&"&l"&STR$(PCLLINES)&"E"
31905     end if 
31910     pr #outfile: pcl$
31915     goto 31850
31920   else 
31925     close #infile: 
31930     open #infile: "name="&infile$&",recl="&str$(inrec-acnt*lr),external,input,relative 
31935     inform$=cform$("form c "&str$(inrec-acnt*lr))
31940     read #infile,using inform$,pos=acnt*lr+1: pcl$ eof ZMAKEPCL
31945     pr #outfile: pcl$
31950   end if 
31955 ZMAKEPCL: close #infile: !:
        close #outfile: 
31960 fnend 
32000 def library fnpic2prn$*200 !:
        !    !:
        ! | Using an HP6l printer driver convert a picture file to a full³!:
        ! | page PRN file for use with FORMPRN.br forms library import. | !:
        !    !
32010   execute "PROC=*"&env$("PD")&"Core\fnsnap\tt"
32020   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fntop$,fnleft$,fnlines$,fngethandle
32030   dim png$*200
32040   png$=""
32050   msgbox("Select an HP6L printer that has had the driver configured for printing to a file."&crlf$&"This will properly format the file for further conversion to PCL5 and storage in the FORMS library.")
32060   open #(outfile:=fngethandle): "name=WIN:/SELECT,eol=NONE",display,output 
32070   if file(outfile)<0 then goto ZPIC2PRN
32080   open #(infile:=fngethandle): "name=open:"&env$("PD")&".\*.png,recl=1",external,input 
32090   if file(infile)<0 then goto ZPIC2PRN
32100   _rec=lrec(infile)
32110   png$=file$(infile)
32120   close #infile: 
32140   if _rec>32000 then arec=32000 else arec=_rec
32150   if _rec>32000 then brec=_rec-int(_rec/32000)*32000 else brec=0
32160   pr #outfile: fntop$(0)&fnleft$(0)&fnlines$(0)&chr$(27)&"&a0v0H"&chr$(27)&"picture='8.2,10.9,"&png$&":ISOTROPIC'"
32170   close #outfile: 
32180 ZPIC2PRN: ! 
32190   outfile=infile=0
32200 fnend 
33000 ! **** Fnbarcode2 ****    Actually pr The Bar Code
33010 def library fnbarcodem(odev,zip$;indent) !:
        ! +----------------------------------------------------------------+!:
        ! |  pr Bar Code to a matrix printer                           | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
33015   if not indent then indent=10
33020   dim bc$(10)*5,bcl(11),bcs(11),bcu(11)
33030   if not bc$(1)="SSSLL" then gosub BARCODEARRAY
33040   gosub BARCODEZIP
33050   if trim$(zip$)="" then goto 33280
33060   shx$=hex$("1B5A")
33070   l1=len(zip$)
33080   l2=l1*11*5+2*11
33090   n2=int(l2/256)
33100   n1=l2-n2*256
33110 !  pr Zip$,L1
33120   pr #odev,using 33130: shx$&chr$(n1)&chr$(n2)&ehx$
33130   form pos indent,c 20,skip 0
33140   mat bcu=bcl : gosub PTBARCD
33150   for i=1 to l1
33160     if zip$(i:i)="0" then zippos=10 else zippos=val(zip$(i:i))
33170 ! pr I;Zippos
33180     for j2=1 to 5
33190       if bc$(zippos)(j2:j2)="L" then mat bcu=bcl else mat bcu=bcs
33200       gosub PTBARCD
33210     next j2
33220   next i
33230   mat bcu=bcl : gosub PTBARCD
33240   goto 33280
33250 PTBARCD: for j=1 to 11 : pr #odev: chr$(bcu(j));
33260   next j
33270   return 
33280 fnend 
33290 ! **** Fnbarcode1 ****    Set Up For Bar Code Printing
33300 BARCODEARRAY: ! Set Arrays
33310 restore 33330
33320 !      DIM BC$(10)*5,BCL(11),BCS(11),BCU(11)
33330 data 255,255,255,0,0,0,0,0,0,0,0
33340 read mat bcl
33350 data 15,15,15,0,0,0,0,0,0,0,0
33360 read mat bcs
33370 bc$(10)="LLSSS"
33380 bc$(1)="SSSLL"
33390 bc$(2)="SSLSL"
33400 bc$(3)="SSLLS"
33410 bc$(4)="SLSSL"
33420 bc$(5)="SLSLS"
33430 bc$(6)="SLLSS"
33440 bc$(7)="LSSSL"
33450 bc$(8)="LSSLS"
33460 bc$(9)="LSLSS"
33470 return 
33480 BARCODEZIP: ! **** Fnbarcodezip ****    Change Zip$ To All Numbers
33490 number$="1234567890" !:
      a$=rpt$(" ",9)
33500 k=1 : if trim$(zip$)="" then goto 33560
33510 for j=1 to len(zip$) : loc=pos(number$,zip$(j:j)) ioerr 33550 : if loc<=zz then goto 33550
33520   a$(k:k)=zip$(j:j) : k+=1
33530 next j
33540 zip$=trim$(a$) : goto 33560
33550 zip$=""
33560 return 
34000 def library fnprint_grid(filnum): !:
        !    !:
        ! | pr an alignment overlay to help set up coordinates.      | !:
        ! |                                                             | !:
        !    !
34005   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox
34010   v=h=bv=bh=tv=th=0
34020   for a=0 to 11
34030     bv=0.01 !:
          bh=8.5
34040     fnprintbox(filnum,a,h,bv,bh,100,tv) !:
          if a>0 and a<11 then let fnprintbox(filnum,a,h,0,0,0,0,.1,str$(a),10,"[FONT ARIAL]")
34050     for b=1 to 9
34060       bv=.005 !:
            if mod(b,2) then shade=20 else shade=50
34070       if a+b*.1<11 then let fnprintbox(filnum,a+b/10,h,bv,bh,shade)
34080     next b
34090   next a
34100   v=h=bv=bh=tv=th=0
34110   for a=0 to 8
34120     bv=11 : bh=.01
34130     fnprintbox(filnum,v,a,bv,bh,100) !:
          fnprintbox(filnum,v,a,0,0,0,.1,th,str$(a),10,"[FONT ARIAL]")
34140     for b=1 to 9
34150       bh=.005 !:
            if mod(b,2) then shade=20 else shade=50
34160       if a+b*.1<8.5 then let fnprintbox(filnum,v,a+b/10,bv,bh,shade)
34170     next b
34180   next a
34190 fnend 
34200 def library fncs_env(;cs$) !:
        ! +----------------------------------------------------------------+!:
        ! | Sets BR ENV equivalent to either Server or Client ENV from    | !:
        ! | the operating system Pass C for Client S for Server           | !:
        ! +----------------------------------------------------------------+!
34202   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle,fnCopyc2s,fnCopys2c
34204   dim tempenv$*2048, addstr$*3, leftstr$*2048, rightstr$*2048, cs_textfile$*256
34206   cs_textfile$="cs-"&session$&".txt"
34207   if uprc$(cs$)="C" or cs$="" then !:
          execute "*sys -M -@ set > "&cs_textfile$ !:
          fnCopyc2s("@:\"&cs_textfile$,cs_textfile$,1) !:
          setenv("C_DIR",os_filename$("@:")&"\") !:
        else !:
          if uprc$(cs$)="S" then execute "*sys -M -S set > "&cs_textfile$ !:
            ! EXECUTE "copy "&CS_TEXTFILE$&" @:"&CS_TEXTFILE$&" -n"
34209   open #(cs_text:=fngethandle): "NAME="&cs_textfile$,display,input error XIT_FNCS_ENV
34212 STARTCSLOOP: ! 
34214   addstr$=uprc$(cs$)&"_"
34216   do 
34218     linput #cs_text: tempenv$ error XIT_CSLOOP
34220     gw_wholeline=len(rtrm$(tempenv$)) !:
          gw_posfnwp=pos(uprc$(tempenv$),"=")
34222     if gw_posfnwp>0 then 
34224       gw_equal =pos(tempenv$,'=')
34226       if gw_equal > 0 then 
34228         leftstr$ = addstr$&tempenv$(1:gw_posfnwp-1)
34230         rightstr$ = tempenv$(gw_posfnwp+1:gw_wholeline)
34232         setenv(leftstr$,rightstr$) error 34234
34234 ! Should SETENV FAIL, Ignore it
34236       end if 
34238     end if 
34240   loop while rec(cs_text)<lrec(cs_text)
34242 XIT_CSLOOP: ! End of Startloop
34244   close #cs_text,free: error 34246
34246 ! 
34248 XIT_FNCS_ENV: ! 
34250 fnend 
34300 def library fnCopyc2s(clientfile$*100,serverfile$*100;overwrite) !:
        ! +----------------------------------------------------------------+!:
        ! | Copies a file from the CLIENT to the SERVER in a client       | !:
        ! | server set-up                                                 | !:
        ! | OVERWWRITE if true will not ask for replacement confirmation  | !:
        ! +----------------------------------------------------------------+!
34302   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": 
34304   if not exists(clientfile$)=2 then let msgbox("The file "&trim$(clientfile$)&" could not be found.  The copy is aborted.","File Not Found") : goto ZCOPYC2S
34306   if not overwrite and exists(serverfile$)=2 and not msgbox("The destination file exists.  Do you want to overwrite it?","Overwrite Server File","Yn","QST")=2 then goto ZCOPYC2S
34307   clientfile$=srep$(clientfile$,"@:","")
34308   execute "copy '@:"&clientfile$&"' '"&lwrc$(serverfile$)&"' -N"
34310 ZCOPYC2S: fnend 
34320 def library fnCopys2c(serverfile$*100,clientfile$*100;overwrite) !:
        ! +----------------------------------------------------------------+!:
        ! | Copies a file from the SERVER to the CLIENT in a client       | !:
        ! | server set-up                                                 | !:
        ! | OVERWWRITE if true will not ask for replacement confirmation  | !:
        ! +----------------------------------------------------------------+!
34322   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": 
34324   if not exists(serverfile$)=2 then let msgbox("The file "&trim$(sereverfile$)&" could not be found.  The copy is aborted.","File Not Found") : goto ZCOPYS2C
34326   if not overwrite and exists(clientfile$)=2 and not msgbox("The destination file exists.  Do you want to overwrite it?","Overwrite Client File","Yn","QST")=2 then goto ZCOPYS2C
34327   clientfile$=srep$(clientfile$,"@:","")
34328   execute "copy '"&serverfile$&"' '@:"&env$("C_DIR")&clientfile$&"' -N"
34330 ZCOPYS2C: fnend 
38000 ! ----------------------------------------
38010 def library fndlg(dialog_dat,dlnr;dispanykey,keywait,suffix$*300) !:
        ! +----------------------------------------------------------------+!:
        ! | Displays a BR dialog box from data in DIALOG.DAT file         | !:
        ! | Maintained by DIALOGMN.br and uses FNDIALOG for display       | !:
        ! +----------------------------------------------------------------+!
38011 ! ----------------------------------------
38012 ! DIALOG_DAT  File number for dialog box information !:
        ! DLNR        Dialog box number (similar to screen number)     !:
        ! DISPANYKEY  Flag to indiacte the phrase "Press any key to continue" !:
        ! KEYWAIT     Flag to cause program to wait for a key to be pressed !:
        ! SUFFIX$     Optional suffix to append to dialog test for message
38019   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnradiochk$,fnclkbuf
38020   dim dopts$(3)*40,dtext$*1000,opt$*80,dtitle$*80 ! ,TEXTSTRING$*1000
38025   fnclkbuf
38030   mat dopts$(3)
38035   if not len(suffix$) then dtext$="Processing please wait" !:
          dcol$=str$(ip((80-len(dtext$))/2)) !:
          drow$="10" !:
          dlen=len(dtext$) !:
          dremove=colwidth=0
38040   read #dialog_dat,rec=dlnr: drec,dcol,drow,dlen,dsep,mat dopts$,dfltopt,dremove,dtitle$,dtext$ norec 38045 eof 38045
38042   dcol$=str$(dcol) !:
        drow$=str$(drow)
38045   dtext$=dtext$&suffix$
38046 ! IF DSEP THEN dTEXT$=DTEXT$&" "&RPT$(hex$('C4'),DLEN-2)
38050 ! oPT$=FNDIALOG$(DROW$,DCOL$,DLEN,DTITLE$&DTEXT$,DOPTS$(1),DOPTS$(2),DOPTS$(3),DREMOVE,DFLTOPT,DISPANYKEY,KEYWAIT)
38051   textstring$=dtext$
38052   if trim$(dopts$(1))>"" then !:
          textstring$=textstring$&"|"&trim$(dopts$(1)) !:
          default$="0"
38053   if trim$(dopts$(2))>"" then !:
          textstring$=textstring$&"|"&trim$(dopts$(2)) !:
          default$="00"
38054   if trim$(dopts$(3))>"" then !:
          textstring$=textstring$&"|"&trim$(dopts$(3)) !:
          default$="000"
38055   default$(dfltopt:dfltopt)="1"
38056   colwidth=(dlen/len(default$)-len(default$)*2)*10
38057 ! INPUT FIELDS "23,64,C 1": PAUSE$
38059   opt$=fnradiochk$(dtitle$,"",val(dcol$)*12,val(drow$)*12,allow,default$,"B",1,1,colwidth,0,textstring$)
38060 ! fnDLG=SRCH(DOPTS$,OPT$)
38061   fndlg=pos(opt$,"1")
38080 fnend 
38190 ! -----------------------------------------
38200 def library fndays_to_mmddyy(&dat) !:
        ! +----------------------------------------------------------------+!:
        ! | Convert DAYS to MDY format                                    | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38210   dat=date(days(dat),"MMDDYY")
38220 fnend 
38230 def library fndays_to_mmddccyy(&dat) !:
        ! +----------------------------------------------------------------+!:
        ! | Converts days to Month Day Century Year format                | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38240   dat=date(days(dat),"mmddccyy")
38250 fnend 
38260 def library fnmmddyy_to_days(&dat) !:
        ! +----------------------------------------------------------------+!:
        ! | Convert MDY to DAYS                                           | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38270   dat=days(dat,"mmddyy")
38280 fnend 
38285 ! ------------------------------------------
38290 def library fnmmddccyy_to_days(&dat) !:
        ! +----------------------------------------------------------------+!:
        ! | Convert MDCY date to DAYS                                     | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38300   dat=days(dat,"mmddccyy")
38310 fnend 
38318 ! ------------------------------------------
38320 def library fnyymmdd_to_days(&dat) !:
        ! +----------------------------------------------------------------+!:
        ! | Convert YMD format to DAYS                                    | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38330   dat=days(dat,"yymmdd")
38340 fnend 
38348 ! -----------------------------------------
38350 def library fnccyymmdd_to_days(&dat) !:
        ! +----------------------------------------------------------------+!:
        ! | Convert CYMD to days format                                   | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38360   dat=days(dat,"ccyymmdd")
38370 fnend 
38500 ! -------------------------------
38510 def library fnfilename$*80(;name$*80,header$*80) !:
        ! +----------------------------------------------------------------+!:
        ! | Open a window and suggest a filename                          | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38520 ! NAME = DEFAULT FILE NAME TO DISPLAY OR BLANK IF NO DEFAULT
38522   if header$<="" then header$="Output File" else header$=trim$(header$)
38530   pdfwin=fnwin("10","10","11","70",header$,"DS","[X]",pdfwin,1)
38540   pr #pdfwin, fields "2,2,c ": "File name"
38550   rinput #pdfwin, fields "2,12,45/c 80,[D]",attr "[A]": name$
38560   if not fkey=esc and not fnok then goto 38550
38570   fnfilename$=trim$(name$)
38580   fnclswin(1) !:
        pdfwin=0
38590 fnend 
38595 ! ---------------------------------------------
38600 def library fngetfilename$*80(seed$;d) !:
        ! +----------------------------------------------------------------+!:
        ! | Create a suggested filename with optional date suffix         | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38601 ! seed$  a basic file name for the function such as AP !:
        ! d    if true then append the month and day to the seed$ for a file name
38604   dim filename$*80
38610   if env$("OS")="Windows_NT" then 
38620     filename$=env$("USERPROFILE")&"\My Documents"
38621 ! fiLENAME$="C:\My Documents"
38630   else filename$="C:\My Documents"
38640   if d then 
38650     filename$=filename$&"\"&trim$(seed$)&date$("MM-DD")
38660   else filename$=filename$&"\"&trim$(seed$)
38670   fngetfilename$=filename$
38680 fnend 
38695 ! ----------------------------------------------
38700 def library fnradnum(mat v$) !:
        ! +----------------------------------------------------------------+!:
        ! | Determin the number of the radio button selected              | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38702 ! returns the value of the radio button selected
38710   fnradnum=0
38720   for v=1 to udim(mat v$)
38730     if v$(v)(1:1)="^" then let fnradnum=v : goto ZRADNUM
38740   next v
38750 ZRADNUM: fnend 
38755 def library fncheck(l$*100) !:
        ! +----------------------------------------------------------------+!:
        ! | Returns 1 if position 1:1 is ^ else returns 0 (false)         | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38760   if l$(1:1)="^" then let fncheck=1 else let fncheck=0
38762 fnend 
38770 def library fncheck$*100(l$*100,l) !:
        ! +----------------------------------------------------------------+!:
        ! | If L is true then the L$(1:1) is set to ^ else any leading ^  | !:
        ! | is stripped from the variable L$                              | !:
        ! +----------------------------------------------------------------+!
38772   if l then let fncheck$="^"&srep$(l$,"^","") else let fncheck$=srep$(l$,"^","")
38775 fnend 
38780 def library fnx$(l) !:
        ! +----------------------------------------------------------------+!:
        ! | Returns X if l is true BLANK "" if l is false                 | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
38782   if l then let fnx$="X" else let fnx$=""
38785 fnend 
38800 def library fngetfile$*200(lookin$*200,lookfor$*50) !:
        ! +----------------------------------------------------------------+!:
        ! | Creates a Windows file dialog using FILEDIALOG.exe to return  | !:
        ! | a filename and path of an existing file.                      | !:
        ! +----------------------------------------------------------------+!
38802   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle
38810   fngetfile$=""
38820   dim getfile$*200,progpath$*100
38830   progpath$=env$("PD")&"Core\fnsnap\" !:
        lookin$=trim$(lookin$) !:
        lookfor$=trim$(lookfor$)
38840   x=len(lookin$)
38850   if not lookin$(x:x)="\" then lookin$=lookin$&"\"
38855   if wbversion$<"4.20" then 
38860     execute "sys -W "&os_filename$(progpath$&"filedialog.exe")&" -T1 -B"&wsid$&" -P"&lookin$&" -X"&lookfor$
38870     if exists(progpath$&"dbde"&wsid$&".txt") then goto 38880 else goto 38960
38880     getfile=50
38890     if file(getfile)>-1 then getfile+=1 : goto 38890
38900     open #getfile: "name="&progpath$&"dbde"&wsid$&".txt",display,input 
38910     linput #getfile: getfile$
38920     close #getfile,free: 
38930     getfile=0
38940     if trim$(uprc$(getfile$))="CANCEL" then getfile$=""
38942     if trim$(uprc$(getfile$))="TIMEOUT" then getfile$=""
38950     fngetfile$=trim$(srep$(getfile$,".*",""))
38952   else 
38953     open #(getfile:=fngethandle): "name=open:"&lookin$&lookfor$,display,input ioerr 38955
38954     fngetfile$=file$(getfile) !:
          close #getfile: !:
          getfile=0 !:
          goto 38960
38955     msgbox("No file was selected") !:
          fngetfile$=""
38958   end if 
38960 fnend 
38970 ! --------------------------------------------------
39000 def library fnputfile$*200(lookin$*200,lookfor$*50) !:
        ! +----------------------------------------------------------------+!:
        ! | Returns a path and file name for a file to be created         | !:
        ! | the file does not have to exist for the name to be returned   | !:
        ! +----------------------------------------------------------------+!
39010   fnputfile$=""
39020   dim putfile$*200 ! ,PROGPATH$*100
39030   progpath$=env$("PD")&"Core\fnsnap\" !:
        lookin$=trim$(lookin$) !:
        lookfor$=trim$(lookfor$)
39040   x=len(lookin$)
39050   if not lookin$(x:x)="\" then lookin$=lookin$&"\"
39060   execute "sys -W "&os_filename$(progpath$&"filedialog.exe")&" -T2 -B"&wsid$&" -P"&lookin$&" -X"&lookfor$
39070   if exists(progpath$&"dbde"&wsid$&".txt") then goto 39080 else goto 39160
39080   putfile=50
39090   if file(putfile)>-1 then putfile+=1 : goto 39090
39100   open #putfile: "name="&progpath$&"dbde"&wsid$&".txt",display,input 
39110   linput #putfile: putfile$
39120   close #putfile,free: 
39130   putfile=0
39140   if trim$(uprc$(putfile$))="CANCEL" then putfile$=""
39142   if trim$(uprc$(putfile$))="TIMEOUT" then putfile$=""
39150   fnputfile$=trim$(srep$(putfile$,"*",""))
39160 fnend 
39170 ! -------------------------------------------
39300 def library fnhelptip(progpath$*100,textfile$*50,title$*50,record,hrow,hcol;no_wait) !:
        ! +----------------------------------------------------------------+!:
        ! | Creates a pop-up windows help box that displays the contents  | !:
        ! | of a record in TEXTFILE$                                      | !:
        ! +----------------------------------------------------------------+!
39310 ! Progpath$  - location of helptip.exe AND textfile !:
        ! textfile  -  name of text file containing data    !:
        ! record    -  record within text file to display     !:
        ! hrow     - row for upper left corner of window     !:
        ! hcol   -    column number for upper left corner of window !:
        ! If nowait is true now -W is issued
39320   if hrow+hcol=0 then position$="" else 
39330     if hrow>12 then hrow$="100" else hrow$="400"
39340     if hcol>40 then hcol$="100" else hcol$="500"
39350     position$=hcol$&" "&hrow$
39360   end if 
39365   if no_wait>0 then w$=" " else w$=" -W "
39370   if exists(progpath$&"helptip.exe") then execute "sys"&w$&os_filename$(progpath$&"helptip.exe")&" -R"&str$(record)&" -F"&textfile$&" -C"""&title$&""" -L"&str$(hcol*40)&" -T"&str$(hrow*40)&" -W700"
39380 fnend 
39390 ! ------------------------------------------------
39400 def library fnparseres(w$,mat scrnres,mat winres,mat conres) !:
        ! +----------------------------------------------------------------+!:
        ! | Uses RESOLUTION.exe to return screen resolution and BR window | !:
        ! | location and size.  Parses reults for a specific WSID Session | !:
        ! +----------------------------------------------------------------+!
39410 ! w$ is the workstation ID being queried. If zero defaults to current session
39420 ! SCRNRES two element matrix of current terminal rows columns in pixels
39430 ! winres five element matrix !:
        ! 1    0 is maximized ("M-") 1 is windowed ("A-")!:
        ! 2    rows in pixels of window  !:
        ! 3    columns in pixels of window !:
        ! 4    row position of upper left corner in pixels of window  !:
        ! 5    columns position of upper left corner in pixels of window
39440 ! conres five element matrix !:
        ! 1    0 is maximized ("M-") 1 is windowed ("A-")!:
        ! 2    rows in pixels of window  !:
        ! 3    columns in pixels of window !:
        ! 4    row position of upper left corner in pixels of window  !:
        ! 5    columns position of upper left corner in pixels of window
39450 ! confont$      default font for console
39460   dim res$*250
39470   mat scrnres(2) !:
        mat winres(5) !:
        mat conres(5)
39480   if w$>"0" then !:
          w=fp(val(w$)/10)*10 conv ZSETRES else !:
          w=fp(val(wsid$)/10)*10 conv ZSETRES
39490   execute "sys -w "&os_filename$(env$("PD")&"Core\fnsnap\resolution.exe")&" -"&os_filename$(env$("PD")&"Core\fnsnap\res"&wsid$&".txt")
39500   resfil=130
39510   if file(resfil)>-1 then resfile+=1 : goto 39510
39520   open #resfil: "name="&env$("PD")&"Core\fnsnap\res"&wsid$&".txt",display,input 
39530 READ_RES: linput #resfil: res$ eof 39570
39540   res$=trim$(res$)
39550   gosub SETRES
39560   goto READ_RES
39570   close #resfil,free: !:
        resfil=0
39580 fnend 
39582 ! -----------------
39590 SETRES: !:
      ! +----------------------------------------------------------------+!:
      ! | Sub routine used by FNPARSERES                                | !:
      ! |                                                               | !:
      ! +----------------------------------------------------------------+!
39600 if pos(res$,"screen:")=1 then 
39610   res$=srep$(res$,"screen:","")
39620   resx=pos(uprc$(res$),"X")
39630   scrnres(2)=val(res$(1:resx-1)) !:
        scrnres(1)=val(res$(resx+1:len(res$)))
39640   goto ZSETRES
39650 end if 
39660 if pos(res$,"Window")=1 then 
39670   res$=res$(7:len(res$))
39680   if not val(res$(1:1))=w then goto ZSETRES
39690   if pos(res$,"Position")>0 then 
39700     if pos(res$," = A-")>0 then winres(1)=1 else winres(1)=0
39710     res$=res$(pos(res$," = ")+5:len(res$))
39720     x=pos(res$,",")
39730     winres(4)=val(res$(x+1:len(res$))) !:
          winres(5)=val(res$(1:x-1))
39740   end if 
39750   if pos(res$,"Size")>0 then 
39760     res$=res$(pos(res$," = ")+3:len(res$))
39770     x=pos(res$,"x")
39780     winres(2)=val(res$(x+1:len(res$))) !:
          winres(3)=val(res$(1:x-1))
39790   end if 
39800 end if 
39810 if pos(res$,"Console")=1 then 
39820   res$=res$(8:len(res$))
39830   if not val(res$(1:1))=w then goto ZSETRES
39840   if pos(res$,"Position")>0 then 
39850     if pos(res$," = A-")>0 then conres(1)=1 else conres(1)=0
39860     res$=res$(pos(res$," = ")+5:len(res$))
39870     x=pos(res$,",")
39880     conres(4)=val(res$(x+1:len(res$))) !:
          conres(5)=val(res$(1:x-1))
39890   end if 
39900   if pos(res$,"Size")>0 then 
39910     res$=res$(pos(res$," = ")+3:len(res$))
39920     x=pos(res$,"x")
39930     conres(2)=val(res$(x+1:len(res$))) !:
          conres(3)=val(res$(1:x-1))
39940   end if 
39950 end if 
39960 ZSETRES: return 
39970 ! --------------------------------------
40000 def library fnwinrowcol(winno,&wrows,&wcols) !:
        ! +----------------------------------------------------------------+!:
        ! | Returns the number of ROWS and Columns for a specific window  | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
40010   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwinsize
40020   fnwinsize(mat winno,mat swrow,mat swcol,mat ewrow,mat ewcol,mat wrows,mat wcols,mat wparent)
40030   for x=1 to udim(mat winno)
40040     if winno=winno(x) then wrows=wrows(x) !:
            wcols=wcols(x) !:
            fnwinrowcol=wparent(x)
40050   next x
40060 fnend 
40065 ! ---------------------------------------
40070 def library fnwinsize(mat s_winno,mat s_srow,mat s_scol,mat s_erow,mat s_ecol,mat s_rows,mat s_cols,mat s_parent) !:
        ! +----------------------------------------------------------------+!:
        ! | Creates matrixes holding all the screen size parameters       | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
40080 ! 
40090 ! 
40100 ! 
40110   execute "status files >s_files."&wsid$
40120   s_files=10
40130   if file(s_files)>-1 then s_files+=1 : goto 40130
40140   open #s_files: "name=s_files."&wsid$,display,input 
40150   dim s_a$*1000
40160   s_a=0
40170 S_FILES_1: linput #s_files: s_a$ eof S_FILES_2
40180   if not s_a$(1:11)="Open File #" or pos(s_a$,":CON:")<11 then goto S_FILES_1
40190   s_a+=1
40200   mat s_winno(s_a) !:
        mat s_srow(s_a) !:
        mat s_scol(s_a)
40210   mat s_erow(s_a) !:
        mat s_ecol(s_a) !:
        mat s_rows(s_a) !:
        mat s_cols(s_a) !:
        mat s_parent(s_a)
40220   s_s=pos(s_a$,"#")+1 !:
        s_e=pos(s_a$,":CON:",12)-1
40230   s_winno(s_a)=val(trim$(s_a$(s_s:s_e)))
40240   linput #s_files: s_a$ eof S_FILES_2
40250   x=pos(s_a$,"SROW=")+5 !:
        y=pos(s_a$,",",x)-1 !:
        s_srow(s_a)=val(trim$(s_a$(x:y)))
40260   x=pos(s_a$,"SCOL=")+5 !:
        y=pos(s_a$,",",x)-1 !:
        s_scol(s_a)=val(trim$(s_a$(x:y)))
40270   x=pos(s_a$,"EROW=")+5 !:
        y=pos(s_a$,",",x)-1 !:
        s_erow(s_a)=val(trim$(s_a$(x:y)))
40280   x=pos(s_a$,"ECOL=")+5 !:
        y=pos(s_a$,",",x)-1 !:
        s_ecol(s_a)=val(trim$(s_a$(x:y)))
40290   s_rows(s_a)=s_erow(s_a)-s_srow(s_a)
40300   s_cols(s_a)=s_ecol(s_a)-s_scol(s_a)
40310   x=pos(s_a$,"PARENT=#")+8 !:
        y=pos(s_a$,":",x)-1 !:
        s_ecol(s_a)=val(trim$(s_a$(x:y)))
40320   goto S_FILES_1
40330 S_FILES_2: ! 
40340   s_a$=file$(s_files)
40350   close #s_files: 
40360   execute "free "&s_a$&" -N" ioerr 40370
40370   s_a=s_files=0
40380 fnend 
40390 ! ---------------------------------------
40400 def library fnoptions(mat o$;default,title$*100,message$*1000,waittime,srow,scol) !:
        ! +----------------------------------------------------------------+!:
        ! | Displays a radio dot list of various options and optionally   | !:
        ! | a message box that contains a message or instructions.        | !:
        ! | The differnce between this and FNOPTS following this function | !:
        ! | is that FNOPTS will NOT use the external RADIOCHK.exe         | !:
        ! | written by Dave Blankenship but only use the BR options       | !:
        ! +----------------------------------------------------------------+!
40410 ! Mat O$  Matrix containing options !:
        ! DEFAULT   Option number to default to !:
        ! Title$    Text at the top of the options box    !:
        ! Message$   Text at the bottom of the options box
40420   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnradnum,fnwinrowcol,fnradiochk$,fngethandle
40430   dim owrk$(1)*100,default$*100
40440   fnwinrowcol(0,rows,cols)
40450   default=min(max(1,default),udim(mat o$))
40460   message$=rtrm$(message$)
40470   alen=len(rtrm$(title$)) !:
        blen=len(message$) !:
        if trim$(title$)>"" then t=1 else t=0
40471   if exists(env$("PD")&"xS:\Core\fnsnap\radiochk.exe") then 
40473     textstring$=trim$(message$) !:
          default$=""
40474     for a=1 to udim(o$) !:
            textstring$=textstring$&"|"&trim$(o$(a)) !:
            default$=default$&"0" !:
          next a !:
          default$(default:default)="1"
40476     caption$=title$ : infile$="": left=0 !:
          top=0 : allow=0 : type$="R" : locate=0 !:
          nocols=ceil(udim(mat o$)/15) : colwidth=0 : waittime=20 !:
          fnoptions=pos(fnradiochk$(caption$,infile$,scol*20,srow*20,allow,default$,type$,locate,nocols,colwidth,waittime,textstring$),"1")
40478 !  GOTO 40652
40479   else 
40480     gosub OPTS
40481     fnoptions=opt
40482   end if 
40485 fnend 
40500 def library fnopts(mat o$;default,title$*100,message$*1000,waittime,srow,scol) !:
        ! +----------------------------------------------------------------+!:
        ! | Displays a radio dot list of various options and optionally   | !:
        ! | a message box that contains a message or instructions.        | !:
        ! +----------------------------------------------------------------+!
40502 ! Mat O$  Matrix containing options !:
        ! DEFAULT   Option number to default to !:
        ! Title$    Text at the top of the options box    !:
        ! Message$   Text at the bottom of the options box
40504   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnradnum,fnwinrowcol,fnradiochk$,fngethandle
40506   dim owrk$(1)*100,default$*100
40508   fnwinrowcol(0,rows,cols)
40510   default=min(max(1,default),udim(mat o$))
40512   message$=rtrm$(message$)
40514   alen=len(rtrm$(title$)) !:
        blen=len(message$) !:
        if trim$(title$)>"" then t=1 else t=0
40520   gosub OPTS
40524   fnopts=opt
40530 fnend 
40598 OPTS: ! 
40599 if udim(mat o$)>20 then !:
        ocols=ceil(udim(mat o$)/20) !:
        pcols=20-ip((20*ocols-udim(mat o$))/ocols) !:
      else ocols=1 : pcols=udim(mat o$)
40600 for a=1 to udim(o$)
40602   if default<=udim(o$) then 
40604     if a=default and not o$(a)(1:1)="^" then o$(a)="^"&o$(a)
40606     if not a=default and o$(a)(1:1)="^" then o$(a)(1:1)=""
40608   end if 
40610   alen=max(alen,len(o$(a)))
40612 next a
40614 owin=fngethandle !:
      arows=ceil(len(message$)/alen)+t+1
40616 if file(owin)>-1 then owin+=1 : goto 40616
40618 al=alen+4 !:
      al$=str$(al) !:
      bl$=str$(blen+4) !:
      slen$=str$(ip(.6*(al)))
40619 ! PAUSE
40620 open #owin: "srow="&str$(max(srow,8))&",scol="&str$(max(scol,10))&",rows="&str$(pcols+arows+2)&",cols="&str$(max(35,ocols*ip(.6*al+2)))&",border=s,picture=none,caption="&title$&",parent=NONE,font.LABELS=Swiss:small,MODAL",display,outin 
40622 fnwinrowcol(owin,rows,cols)
40623 for a=1 to udim(o$) !:
        mat owrk$(a)
40624   if a<=pcols then !:
          owrk$(a)=str$(a+t)&",2,"&slen$&"/radio "&al$&",,"&str$(1000+a) !:
          goto 40629
40625   if a<=2*pcols then !:
          owrk$(a)=str$(a-pcols+t)&","&str$(3+ip(.6*alen+4))&","&slen$&"/radio "&al$&",,"&str$(1000+a) !:
          goto 40629
40626   if a<=3*pcols then !:
          owrk$(a)=str$(a-2*pcols+t)&","&str$(3+2*ip(.6*alen+4))&","&slen$&"/radio "&al$&",,"&str$(1000+a) !:
          goto 40629
40627   if a<=4*pcols then !:
          owrk$(a)=str$(a-2*pcols+t)&","&str$(3+3*ip(.6*alen+4))&","&slen$&"/radio "&al$&",,"&str$(1000+a) !:
          goto 40629
40628   if a<=5*pcols then !:
          owrk$(a)=str$(a-2*pcols+t)&","&str$(3+4*ip(.6*alen+4))&","&slen$&"/radio "&al$&",,"&str$(1000+a) !:
          goto 40629
40629 next a
40630 if t then !:
        execute "config attribute [OPTITLE] N/B:T,FONT=ARIAL:LARGE:BOLD" !:
        pr #owin, fields str$(t)&","&str$(ip(cols/2))&",CC 0,[OPTITLE]": trim$(title$)
40631 execute "config attribute [OPLINE] N/#FFFFFF:#CCCCCC" ! HGB"
40632 if trim$(message$)>"" then rinput #owin, fields str$(a+t)&",2,"&str$(ip(.8*(blen+4)))&"/C "&bl$&",N/W:W",wait=.01: message$ timeout 40634
40634 pr #owin, fields str$(rows+1)&","&str$(ceil(cols/2-3))&",Cc 5,N/W:W,B20": "OK"
40636 if waittime>0 then waittime=waittime else waittime=-1
40637 curfld(default)
40638 rinput #owin,fields mat owrk$,attr "[OPLINE]",wait=waittime: mat o$ timeout 40644 !:
      of=curfld : ok=fkey
40639 if ok=99 or ok=93 then opt=0 : goto 40648
40640 if ok=20 or ok=0 or ok>1000 then goto 40644
40642 if not ok then let curfld(of+1,ok): goto 40638 else let curfld(of,ok) : goto 40638
40644 opt=fnradnum(mat o$)
40646 if not opt then waittime$="" : goto 40638
40648 close #owin: 
40650 owin=0
40652 return 
40730 ! -------------------------------------------
40740 def library fnxlcol$(colno) !:
        !    !:
        ! | Returns the letter or two letter code for the COLUMN in an  | !:
        ! | Excel spreadsheet based on COLNO                            | !:
        !    !
40745   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmod
40750   if not int((colno-1)/26) then 
40755     fnxlcol$=chr$(colno+64)
40760   else 
40765     fnxlcol$=chr$(64+int((colno-1)/26))&chr$(fnmod(colno,26)+64)
40770   end if 
40775 fnend 
40776 def library fnxlref$(colno,rowno) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
40777   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnxlcol$
40778   fnxlref$=fnxlcol$(colno)&str$(round(rowno,0))
40779 end def 
40780 def library fnxlrange$(colno1,rowno1,colno2,rowno2) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
40781   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnxlref$
40782   fnxlrange$=fnxlref$(colno1,rowno1)&":"&fnxlref$(colno2,rowno2)
40783 end def 
40800 def library fntextbox$*4000(&textwin,srow,scol,rows,cols,tlen,parent,text$*4000;border,tkey$) !:
        ! +----------------------------------------------------------------+!:
        ! | Displays and allows input from a windows text box with text   | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
40810   execute "CONFIG DATAHILITE OFF"
40820   if not textwin then 
40830     textwin=300
40832     if file(textwin)>-1 then textwin+=1 : goto 40832
40840     if border then border$="s" else border$="NONE"
40850 !  IF FILE(TEXTWIN)>-1 THEN tEXTWIN+=1 : GOTO 40850
40860     open #textwin: "srow="&str$(srow)&",scol="&str$(scol)&",rows="&str$(rows)&",cols="&str$(cols)&",parent="&str$(parent)&",border="&border$,display,outin 
40870     twrk$="1,1,"&str$(rows*cols)&"/v "&str$(tlen)&",,"&tkey$
40880   end if 
40890   rinput #textwin,fields twrk$: text$
40900   fntextbox$=rtrm$(text$)
40910   execute "CONFIG DATAHILITE ON"
40920 fnend 
40930 ! --------------------------------
40940 def library fnmod(cel,cols) !:
        ! +----------------------------------------------------------------+!:
        ! |  Returns the COLUMN number of the cell referred               | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+!
40950   if not mod(cel,cols) then let fnmod=cols else let fnmod=mod(cel,cols)
40960 fnend 
40970 ! --------------------------------
41000 def library fnoptions$*100(mat o$;default$*100,title$*100,message$*1000,waittime,not_one) !:
        ! +----------------------------------------------------------------+!:
        ! | Displays a check box list of various options and optionally   | !:
        ! | a message box that contains a message or instructions.        | !:
        ! +----------------------------------------------------------------+!
41010 ! Mat O$  Matrix containing options !:
        ! DEFAULT$   Options defaults 1=checked 0=not checked !:
        ! Title$    Text at the top of the options box    !:
        ! Message$   Text at the bottom of the options box !:
        ! not_one  Allow no items to be selected
41020   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwinrowcol,fnradiochk$
41030   dim textstring$*2200 ! OWRK$(1)*100
41035   textstring$=""
41040   fnwinrowcol(0,rows,cols)
41050   default=min(max(1,default),udim(mat o$))
41060   message$=rtrm$(message$)
41070   alen=len(rtrm$(title$)) !:
        blen=len(message$) !:
        if trim$(title$)>"" then t=1 else t=0
41071   if exists(env$("PD")&"Core\fnsnap\radiochk.exe") then 
41072     textstring$=""
41073     for a=1 to udim(o$) !:
            textstring$=textstring$&"|"&trim$(o$(a)) !:
          next a
41074     if not_one then allow=1 else allow=0
41077     caption$=title$ : infile$="": left=200 !:
          top=200 : type$="C" : locate=0 !:
          nocols=ceil(udim(mat o$)/20) : colwidth=0 !:
          fnoptions$=fnradiochk$(caption$,infile$,left,top,allow,default$,type$,locate,nocols,colwidth,waittime,textstring$)
41078     goto 41330
41079   end if 
41080   for a=1 to udim(o$)
41090     if default$(a:a)="1" and not o$(a)(1:1)="^" then o$(a)="^"&o$(a)
41100     if default$(a:a)="0" and o$(a)(1:1)="^" then o$(a)(1:1)=""
41110     alen=max(alen,len(o$(a)))
41120   next a
41130   owin=10 !:
        arows=ceil(len(message$)/alen)+t+1
41140   if file(owin)>-1 then owin+=1 : goto 41140
41150   open #owin: "srow=2,scol=2,rows="&str$(udim(mat o$)+arows)&",cols="&str$(ip(.8*(alen+6)))&",border=s,picture=none,parent=NONE,font.LABELS=Swiss:medium,MODAL",display,outin 
41160   al$=str$(ip(.8*(alen+4)))&"/"&str$(alen+4) !:
        bl$=str$(ip(.8*(blen+4)))&"/"&str$(blen+4)
41170   fnwinrowcol(owin,rows,cols)
41180   for a=1 to udim(o$)
41190     mat owrk$(a) !:
          owrk$(a)=str$(a+t)&",2,check "&al$ ! ,,"&STR$(20)
41200   next a
41210   if t then !:
          execute "config attribute [OPTITLE] N/B:T,FONT=ARIAL:LARGE:BOLD" !:
          pr #owin, fields str$(t)&",2,CC "&str$(alen)&",[OPTITLE]": trim$(title$)
41220   if trim$(message$)>"" then !:
          rinput #owin, fields str$(a+t)&",2,C "&bl$&",N/W:Wfont=SWISS",wait=.01: message$ timeout 41230
41230   pr #owin, fields str$(rows+1)&","&str$(ceil(cols/2-3))&",Cc 5,N/W:Wfont=SWISS,B20": "OK"
41240   if waittime>0 then waittime=waittime else waittime=-1
41250   rinput #owin,fields mat owrk$,attr "[L]",wait=waittime: mat o$ timeout 41290 !:
        of=curfld : ok=fkey
41260   if (not ok or ok=20) then goto 41280
41270   if not ok then !:
          curfld(of+1,ok) !:
          goto 41250 else let curfld(of,ok) !:
          goto 41250
41280   opt$=""
41281   for a=1 to udim(mat o$)
41282     if o$(a)(1:1)="^" then !:
            opt$=opt$&"1" else !:
            opt$=opt$&"0"
41285   next a
41290   fnoptions$=opt$
41300 ! IF NOT OPT THEN wAITTIME$="" : GOTO 40660
41310   close #owin: 
41320   owin=0
41330 fnend 
41340 ! -------------------------------------------
41350 def library fn_cs: !:
        !    !:
        ! | Set the value of FN_CS to 1 if client server is running     | !:
        ! | else set FN_CS to 0                                         | !:
        !    !
41360   if env$("BR_MODEL")="CLIENT/SERVER" then let fn_cs=1 else let fn_cs=0
41370 fnend 
41400 def library fnradiochk$*100(caption$*80,infile$*60,left,top,allow,default$*100,type$,locate,nocols,colwidth,waittime;textstring$*2400) !:
        !    !:
        ! | Display a radio/checkbox set of options and return a string | !:
        ! | of 0's and 1's as the function result                       | !:
        !    !
41410 ! Caption$   Windows bar title !:
        ! INFILE$   File name to use as intermediary   !:
        ! LEFT   Position in pixels of top left   !:
        ! TOP      Position in Pixels of top left  !:
        ! ALLOW   allow no responses if true require at least 1 if false
41420 ! DEFAULT$    String of 0 and 1 for default answers !:
        !                       - if Radio then only last 1 is used !:
        ! BOXWIDTH      if false calculates width else uses number in pixels  !:
        ! TYPE$     R for Radio  C for Check Box   !:
        ! LOCATE     Record number in file to use as data      !:
        ! NOCOLS     Number of columns
41422 ! COLWIDTH   Width of columns or button if not automatic  !:
        ! WAITTIME   Number of seconds to wait before returning default answer !:
        !                   zero to wait forever                   !:
        ! TEXTSTRING$  text to use for display if not using an existing file
41425   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnCopys2c,fnCopyc2s,fn_cs
41426   _cs=fn_cs
41428   if _cs and not exists("@:S:\Core\fnsnap\radiochk.exe")=2 then !:
          fnCopys2c(env$("PD")&"Core\fnsnap\radiochk.exe","@:vol002")
41429   if _cs and not exists("@:temp")=1 then execute "MKDIR @:temp"
41430   dim commandline$*1000,response$*100
41440   if infile$="" then 
41450     infile$=env$("PD")&"Core\fnsnap\radchk."&session$
41460     rcfile=10 !:
          locate=1
41470     if file(rcfile)>-1 then rcfile+=1 : goto 41470 else !:
            open #rcfile: "name="&infile$&",replace,recl=3200",display,output 
41480     pr #rcfile: textstring$
41490     close #rcfile: 
41500   end if 
41501   if _cs then let fnCopys2c(infile$,"@:S:\Core\fnsnap\"&infile$(pos(infile$,"\",-1)+1:len(infile$)),1)
41502   if file(rccfile)>-1 then rccfile+=1 : goto 41502 else !:
          open #rccfile: "name=command."&session$&",replace,recl=3200",display,output 
41503   dim rccfile$*65
41505   rccfile$=file$(rccfile)
41510 ! 
41520   if _cs then pr #rccfile: "-F"&os_filename$(srep$(infile$,env$("PD"),"@:")) !:
          pr #rccfile: "-B"&session$&" " !:
        else !:
          pr #rccfile: "-F"&os_filename$(infile$) !:
          pr #rccfile: "-B"&session$&" "
41530 ! 
41531   if left>0 then pr #rccfile: "-L"&str$(left)
41540 ! 
41541   if top>0 then pr #rccfile: "-T"&str$(top)
41550 ! 
41551   if caption$>"" then pr #rccfile: "-C"&caption$
41560 ! 
41561   if allow>0 then pr #rccfile: "-U1"
41570 ! 
41571   if default$>"" then pr #rccfile: "-V"&default$
41580 ! 
41581   if colwidth>0 then pr #rccfile: "-W"&str$(colwidth)
41590 ! 
41591   if pos("RCB",uprc$(type$(1:1)))>0 then pr #rccfile: "-X"&uprc$(type$(1:1))
41600 ! 
41601   if locate>0 then pr #rccfile: "-Y"&str$(locate)
41610 ! 
41611   if nocols>1 then pr #rccfile: "-Z"&str$(nocols)
41612   if waittime>0 then pr #rccfile: "-G"&str$(waittime)
41614   pr #rccfile: "-$br32.exe" ! |"&STR$(TOP/20)&"|"&STR$(LEFT/20)&"|"&STR$(VAL(ENV$("ER"))+1)&"|"&ENV$("EC")
41615 ! if BR32.exe is not running then the box will center on the screen
41616 ! PAUSE
41618   close #rccfile: 
41620   if _cs then let fnCopys2c(rccfile$,"@:temp\command."&session$,1)
41621   if _cs then !:
          execute "sys -w "&os_filename$("@:S:\Core\fnsnap\radiochk.exe")&" -#"&os_filename$("@:temp\"&"command."&session$) !:
        else !:
          execute "sys -w "&os_filename$(env$("PD")&"Core\fnsnap\radiochk.exe")&" -#"&os_filename$(rccfile$)
41625   if _cs and exists("@:S:\Core\fnsnap\dbde"&session$&".txt")=2 then let fnCopyc2s("@:S:\Core\fnsnap\dbde"&session$&".txt",env$("PD")&"Core\fnsnap\dbde"&session$&".txt",1)
41630   if not exists(env$("PD")&"Core\fnsnap\dbde"&session$&".txt") then goto 41700
41640   rcfile=10
41650   if file(rcfile)>-1 then rcfile+=1 : goto 41650 !:
        else open #rcfile: "name="&env$("PD")&"Core\fnsnap\dbde"&session$&".txt",display,input 
41670   linput #rcfile: response$
41680   close #rcfile,free: !:
        rcfile=0
41690   fnradiochk$=response$
41700 fnend 
41710 def library fnhelp(hpath$*60,hfile$*20,hbase$,hfld,hrow,hcol;htitle$*80) !:
        !    !:
        ! | Open a tip box associated with a field in a screen using    | !:
        ! | a simple text file as input                                 | !:
        ! | HPATH$ the location of the help file                        | !:
        ! | HFILE$ the file name within the path                        | !:
        ! | HBASE$ the anchor keyword marking the base record           | !:
        ! | HFLD the record number after the HBASE$ for the text line   | !:
        ! | HROW HCOL location of top left corner of help window        | !:
        ! | HTITLE$ title to display in the window                      | !:
        ! |                                                             | !:
        !    !
41712   dim h$*10000
41715   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnhelptip,fnwaitwin
41720   htxt=10 : hbase=0 : hrec=0
41722   if not hpath$(len(hpath$):len(hpath$))="\" then hpath$=hpath$&"\"
41730   if file(htxt)>-1 then htxt+=1 : goto 41730
41740   open #htxt: "NAME="&trim$(hpath$)&hfile$,display,input 
41750   linput #htxt: h$ eof 41780
41755   hrec+=1
41760   if trim$(uprc$(hbase$))=trim$(uprc$(h$)) then goto 41770 else goto 41750
41770   hbase=hrec
41771   for _a=1 to hfld : linput #htxt: h$ : next _a
41780   close #htxt: : htxt=0
41785 ! fnWAITWIN(H$(POS(H$,"|")+1:INF),H$(1:POS(H$,"|")-1),"OK",1) !:
        ! INPUT FIELDS "28,75,c 1": PAUSE$
41790   fnhelptip(hpath$,hfile$,htitle$,hbase+hfld,hrow,hcol)
41800   fnhelp=hbase
41810 fnend 
41850 def library fnmsexe$*100(l$) !:
        !    !:
        ! | Returns the installed path of Microsoft programs such as    | !:
        ! | WinWord i.e. FNMSEXE$("winword.exe")                        | !:
        !    !
41855   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fn_cs,fnCopyc2s,fnCopys2c
41856   _cs=fn_cs
41857   if _cs and not exists("@:S:\Core\fnsnap\brregister2.exe") then let fnCopys2c(env$("PD")&"Core\fnsnap\brregister2.exe", "@:vol002")
41858   dim tempfile$*100
41860   if _cs then !:
          execute "config shell default client" !:
          execute "sys -W "&os_filename$("@:S:\Core\fnsnap\brregister2.exe")&" -B"&session$&" -N"&l$ !:
          ! EXECUTE "config shell default server" !:
        else !:
          execute "sys -W "&os_filename$(env$("PD")&"Core\fnsnap\brregister2.exe")&" -B"&session$&" -N"&l$
41867   if _cs then !:
          fnCopyc2s(lwrc$("@:S:\Core\fnsnap\dbde"&session$&".txt"),env$("PD")&"Core\fnsnap\dbde"&session$&".txt")
41870   exefil=1
41880   if file(exefil)>-1 then exefil+=1 : goto 41880
41890   open #exefil: "name="&env$("PD")&"Core\fnsnap\dbde"&session$&".txt",display,input 
41900   dim exefil$*100
41910   linput #exefil: exefil$
41920   close #exefil,free: 
41930   fnmsexe$=exefil$
41940   exefil=0
41950 fnend 
42000 ! ----------------
42010 def library fnnextmonth(indate) !:
        ! +----------------------------------------------------------------+!:
        ! | Creates a smilar date in the next month taking into considera | !:
        ! |                                                               | !:
        ! +----------------------------------------------------------------+! creates a similar date in the fol
42020 ! ----------------
42030   m=date(indate,"MM")
42040   y=date(indate,"CCYY")
42050   d=date(indate,"DD")
42060   if m=12 then 
42070     dy=y+1 !:
          dm=1
42080   else 
42090     dm=m+1 !:
          dy=y
42100   end if 
42110   x=days(dm*1000000+10000+dy,"MMDDCCYY")
42120   if x-indate<4 then 
42130     if dm=12 then 
42140       dm=1 !:
            dy+=1
42150     else 
42160       dm+=1
42170     end if 
42180     fnnextmonth=days(dm*1000000+10000+dy,"MMDDCCYY")-(x-indate)
42190   else 
42200     fnnextmonth=days(dm*1000000+d*10000+dy,"MMDDCCYY")
42210   end if 
42220 fnend 
42300 !    !:
      ! | Determin the next sequnce number for a keyed sequence       | !:
      ! 
42310 def library fnseq(filnr,filkey$*50,filfrm$*200;_fk$*50): !:
        ! determine next sequence number for a posted :!:
        ! master file
42320 ! -----------------:!:
        ! filnr is the number of an existing open file:!:
        ! filekey$ is the complete key excluding the sequence number:!:
        ! filfrm$ is the form statement used to read the keyed file
42330   fnseq=fseq=fs=_fs=0
42331   dim _fs$(1)*5
42332   _fs$(1)=cnvrt$("ZD 5",0) !:
        _fs=1
42340   restore #filnr,search=filkey$: nokey ZFNSEQ eof ZFNSEQ
42350 LOOPSEQ: ! read the file looping until the key no longer matches
42360   read #filnr,using filfrm$: _fk$,fs eof LOOPSEQ1
42361   if _fk$=filkey$ then !:
          _fs+=1 !:
          mat _fs$(_fs) !:
          _fs$(_fs)=cnvrt$("zd 5",fs) !:
          goto LOOPSEQ
42370 ! 
42371 LOOPSEQ1: for _fs=0 to udim(_fs$)-1
42372     if srch(mat _fs$,cnvrt$("zd 5",_fs))<1 then !:
            fseq=_fs !:
            goto ZFNSEQ
42375   next _fs
42380 ! GOTO LOOPSEQ
42390 ZFNSEQ: fnseq=fseq: !:
        fseq=fs=_fs=0
42400 fnend 
42495 !    !:
      ! | Create a file name with the next sequence number            | !:
      ! 
42500 def library fnnextfil$*100(nfil$,npath$*100) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
42510   nextfil=112 !:
        crlf$=chr$(13)&chr$(10) !:
        a=0
42520 ! DIM A$*2000
42530   nfil$=trim$(nfil$) !:
        nflen=len(trim$(nfil$))
42540   if file(nextfil)>-1 then nextfil+=1 : goto 42540
42542   npath$=trim$(npath$) !:
        nplen=len(npath$)
42543   if npath$(nplen:nplen)="\" then npath$(nplen:nplen)=""
42550   execute "dir "&npath$&"\*.* >dirb.txt"
42560   open #nextfil: "name=dirb.txt",display,input 
42570   linput #nextfil: a$ eof 42610
42580   if not uprc$(a$)(1:nflen)=uprc$(nfil$) then goto 42570
42590   a=max(a,val(a$(nflen+1:8))) conv 42570
42600   goto 42570
42610   mask$="pic("&rpt$("#",8-nflen)&")"
42620   fnnextfil$=nfil$&cnvrt$(mask$,a+1)
42630 ! INPUT FIELDS "25,70,c 1": PAUSE$
42640   close #nextfil,free: 
42650 end def 
42710 def library fnview_ext(;infile$*100,inlen) !:
        !    !:
        ! | Read an external file and create a mirror TXT file in the   | !:
        ! | TEMP directory then load the TXT file in EditPadPro to view | !:
        !    !
42720   dim in_a$*512,outfile$*100,frm_infile$*100
42730   if infile$<="" then infile$=env$("PD")&"efile\w2*.*"
42735   outfile$=env$("TEMP")&"\viewext.txt"
42740   if not inlen then inlen=512
42750   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle,fnmsexe$
42760   if infile$="efile\w2*.*" then !:
          open #(in:=fngethandle): "name=open:"&infile$&",recl="&str$(inlen),external,input else !:
          open #(in:=fngethandle): "name="&infile$&",recl="&str$(inlen),external,input 
42770   frm_infile$=cform$("FORM C "&str$(inlen))
42780   open #(save:=fngethandle): "name="&outfile$&",recl="&str$(inlen+2)&",replace",display,output 
42790   outfile$=os_filename$(file$(save))
42800 READ_IN: read #in,using frm_infile$: in_a$ eof ZVIEW_EXT
42810   pr #save: in_a$
42820   goto READ_IN
42830 ZVIEW_EXT: close #in: 
42840   close #save: 
42850   execute "sys -w -c "&fnmsexe$("editpadpro.exe")&" """&outfile$&""""
42860 fnend 
43700 ! =============================
43710 def library fnwinbuttons(brow,btext$*100,bwin;center,bfont$*50,blen) !:
        !    !:
        ! ³BROW 0 bottom of window                                      | !:
        ! |     -x rows up from bottom of window                        | !:
        ! |      x rows down from top of window                         | !:
        ! | BTEXT$ "^F12:text ^F11:text" FKEY to return and button text | !:
        ! | BWIN window number in which to display                      | !:
        ! | CENTER centers button in window else right justified        | !:
        ! | FONT$ changes font to be used in window                     | !:
        ! | BLEN  overrides calculated width of buttons                 | !:
        !    !
43712   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwinrowcol
43714   if pos("^","^")<1 then !:
          charoff=1 !:
          execute "config SEARCH_CHAR OFF" else !:
          charoff=0
43720   fnwinrowcol(bwin,br,bc)
43730   if brow<1 then brow$=str$(br+brow+1) else brow$=str$(brow)
43735   pr #bwin, fields brow$&",1,c "&str$(bc)&",N/W:T": rpt$(" ",bc)
43740   dim bbut$(1)*50,bbuttxt$(1),bbutlen(1),bbutcol$(1),bfkey$(1),bwrk$(1)*40
43750   ebut=bb=bmax=0
43755 !    !:
        ! | Build the arrays for printing buttons                       | !:
        ! |                                                             | !:
        !    !
43760 START_BUT: ebut=pos(btext$,"^",2)
43770   if ebut<=2 then !:
          ebut=len(btext$) : bend=1 else !:
          bend=0
43780   bb+=1 !:
        mat bbut$(bb) !:
        mat bbuttxt$(bb) !:
        mat bbutlen(bb) !:
        mat bbutcol(bb) !:
        mat bfkey$(bb) !:
        mat bwrk$(bb)
43790   if bend then bbut$(bb)=btext$ else !:
          bbut$(bb)=btext$(1:ebut-1) !:
          btext$=btext$(ebut:len(btext$))
43800   x=pos(bbut$(bb),":")
43810 !    !:
        ! | Set the FKEY value for pressing a button                    | !:
        ! |                                                             | !:
        !    !
43820   bkey$(bb)=srep$(srep$(bbut$(bb)(1:x),"^",""),":","")
43830   if uprc$(bkey$(bb))="PGUP" then bkey$(bb)="B90"
43840   if uprc$(bkey$(bb))="PGDN" then bkey$(bb)="B91"
43850   if uprc$(bkey$(bb))="ESC" then bkey$(bb)="B99"
43860   if uprc$(bkey$(bb))="CLOSE" then bkey$(bb)="B93"
43870   bkey$(bb)=srep$(srep$(bkey$(bb),"F","B"),"f","B")
43880 !    !:
        ! | Remove the Fkey value from the text being parsed            | !:
        ! |                                                             | !:
        !    !
43890   bbut$(bb)=trim$(bbut$(bb)(x+1:len(bbut$(bb))))
43900   bbutlen(bb)=len(bbut$(bb))
43910   bmax=max(6,bmax,bbutlen(bb),blen)
43920   if not bend then goto START_BUT
43925   if center then !:
          bcent=round((bc-(bmax+1)*udim(mat bbut$))/2-bmax/2,0) else !:
          bcent=0
43930   for a=1 to udim(mat bbutlen)
43940     bbutcol(a)=bc-bcent-(bmax+1)*(1+bb-a)
43950     bwrk$(a)=brow$&","&str$(bbutcol(a))&",CC "&str$(bmax)&","&bfont$&","&bkey$(a)
43960   next a
43970   pr #bwin,fields mat bwrk$: mat bbut$
43975   if charoff then execute "CONFIG SEARCH_CHAR 5E" : charoff=0
43980 end def 
44000 def library fnpicbuttons(picrow,picbuttons$*1000;picwin,picpath$*100,picw,pich) !:
        !    !:
        ! | Display a row of graphical buttons in a window              | !:
        ! |                                                             | !:
        !    !
44001 ! PICROW  - rows up from the bottom + rows down from the top !:
        ! PICBUTTONS$ String to define FKEY GRAPIC IMAGE and HELP !:
        ! "^F3:GRAPHIC.GIF|HELP TEXT ^F5:GRAPHIC.GIF|HELP TEXT" for example  !:
        ! PICWIN  The window number to display the buttons in (uses fnsnap.dll/FNWINROWCOL
44002 ! PICPATH$  the location of the graphic files in the PICBUTTONS$ string  !:
        ! PICW   column width of the displayed buttons  !:
        ! PICH  row height of the displayed buttons
44005   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwinrowcol
44010   if pos("^","^")<1 then !:
          charoff=1 !:
          execute "config SEARCH_CHAR OFF" else !:
          charoff=0 ! Test to see if config search_char is on or off
44015   fnwinrowcol(picwin,br,bc)
44020   if picrow<1 then !:
          brow$=str$(br+picrow+1) !:
        else brow$=str$(picrow) ! set the display row for buttons
44025   pr #picwin, fields brow$&",1,c "&str$(bc)&",N/W:T": rpt$(" ",bc) ! clear the row
44030   if not picw then picw=2 ! set default width
44035   if not pich then pich=1 ! set default height
44040   dim picmat$(1)*40,picpic$(1)*100,ebut(2)
44045 ! pICPATH$=":icons\"
44050   dim bbut$(1)*50,hbut$(1)*100,bbutpic$(1),bbutlen(1),bbutcol$(1),bfkey$(1),bwrk$(1)*40
44055   ebut(1)=ebut(2)=bb=bmax=0
44060 START_PIC: ebut(2)=pos(picbuttons$,"^",2)
44065   if ebut(2)<=2 then !:
          ebut(2)=len(picbuttons$) : bend=1 else !:
          bend=0
44070   ebut(1)=pos(picbuttons$,"|")
44075   if ebut(1)<1 or ebut(1)>ebut(2) then ebut(1)=ebut(2)
44080   bb+=1 !:
        mat bbut$(bb) !:
        mat hbut$(bb) !:
        mat bbutpic$(bb) !:
        mat bbutlen(bb) !:
        mat bbutcol(bb) !:
        mat bfkey$(bb) !:
        mat bwrk$(bb) !:
        mat picmat$(bb)
44085   if bend then bbut$(bb)=picbuttons$(1:ebut(1)-1) !:
          hbut$(bb)="3B;"&picbuttons$(ebut(1)+1:ebut(2))&";" !:
        else !:
          bbut$(bb)=picbuttons$(1:ebut(1)-1) !:
          hbut$(bb)="3B;"&picbuttons$(ebut(1)+1:ebut(2)-1)&";" !:
          picbuttons$=picbuttons$(ebut(2):len(picbuttons$))
44090   x=pos(bbut$(bb),":")
44095 !    !:
        ! | Set the FKEY value for pressing a button                    | !:
        ! |                                                             | !:
        !    !
44100   bkey$(bb)=srep$(srep$(bbut$(bb)(1:x),"^",""),":","")
44105   if uprc$(bkey$(bb))="PGUP" then bkey$(bb)="B90"
44110   if uprc$(bkey$(bb))="PGDN" then bkey$(bb)="B91"
44115   if uprc$(bkey$(bb))="ESC" then bkey$(bb)="B99"
44120   if uprc$(bkey$(bb))="CLOSE" then bkey$(bb)="B93"
44125   bkey$(bb)=srep$(srep$(bkey$(bb),"F","B"),"f","B")
44130 !    !:
        ! | Remove the Fkey value from the text being parsed            | !:
        ! |                                                             | !:
        !    !
44135   bbut$(bb)=trim$(bbut$(bb)(x+1:len(bbut$(bb))))
44140   bbutlen(bb)=len(bbut$(bb))
44145   bmax=picw+.5 ! MAX(BMAX,BBUTLEN(BB))
44150   if not bend then goto START_PIC
44155   for a=1 to udim(mat bbutlen)
44160     bbutcol(a)=bc-(bmax+1)*(1+bb-a)
44165     picmat$(a)=brow$&","&str$(bbutcol(a))&",p "&str$(pich)&"/"&str$(picw)&",,"&bkey$(a)
44167     bbut$(a)=picpath$&bbut$(a)
44170   next a
44175   pr #picwin, fields mat picmat$,help mat hbut$: mat bbut$
44180   if charoff=1 then !:
          execute "config SEARCH_CHAR 5E" !:
          charoff=0
44185 fnend 
44200 def library fnspic$*20(mask$,number) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
44205   mask$=uprc$(mask$)
44210   dim spmask$(2)*20
44215   mat spmask$(2)=("")
44220   number=fp(number/1000)*1000
44225   spmask$(1)=srep$(srep$(srep$(mask$,"Z","*")(1:pos(mask$,"#")-1),"PIC(",""),"D","-")
44230   spmask$(2)="PIC("&mask$(pos(mask$,"#"):len(mask$))
44235   fnspic$=spmask$(1)&cnvrt$(spmask$(2),number)
44240 fnend 
44300 def library fnunderscore$*500(v,h,l;opt,ht,w) !:
        ! v   vertical position in inches !:
        ! h   horizontal position in inches  !:
        ! L   line length in inches  !:
        ! OPT  0  underscore !:
        !      1  under and overscore   !:
        !      2  double underscore and single overscore !:
        !      3  double underscore only !:
        ! ht    height of line for overscore default is 1/8 inch !:
        ! w     weight of line default is 0.01 inch
44305   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox$
44310   dim u$*500
44315   if not ht then ht=1/8
44320   if not w then w=.01
44325   u$=fnprintbox$(v,h-l,w,l,100)
44330   if opt>0 then u$=u$&fnprintbox$(v-ht,h-l,w,l,100)
44335   if opt>1 then u$=u$&fnprintbox$(v+2*w,h-l,w,l,100)
44340   if opt=3 then u$=fnprintbox$(v,h-l,w,l,100)&fnprintbox$(v+2*w,h-l,w,l,100)
44345   fnunderscore$=u$
44350 fnend 
44400 def library fnhpinit$*2000 !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
44405   execute "PROC=*"&env$("PD")&"Core\fnsnap\tt"
44410   dim hpinit$*2000
44415   hpinit$=chr$(27)&"%-12345X@PJL COMMENT HP INIT String"&crlf$ !:
        hpinit$(inf:inf)="@PJL SET PAGEPROTECT=OFF"&crlf$ !:
        hpinit$(inf:inf)="@PJL SET PAGEPROTECT=AUTO"&crlf$ !:
        hpinit$(inf:inf)="@PJL SET RET=ON"&crlf$ !:
        hpinit$(inf:inf)="@PJL SET ECONOMODE=OFF"&crlf$ !:
        hpinit$(inf:inf)="@PJL SET RESOLUTION=600"&crlf$ !:
        hpinit$(inf:inf)="@PJL ENTER LANGUAGE=PCL"&crlf$&crlf$&chr$(27)&"E"
44420   fnhpinit$=hpinit$
44425 fnend 
44500 def library fnxgridtot(gridamount,gridrow,gridcol,gridwin,mat gridwidth,mat gridform$)
44502   gridmask$=gridform$(gridcol) ! (1:POS(GRIDFORM$(GRIDCOL),",")-1)
44504   pr #gridwin,fields str$(gridrow)&","&str$(sum(mat gridwidth(1:gridcol-1)))&","&gridmask$&",W/W:T": gridamount
44506 end def 
50000 ! ----------PRINT-BOX-MACRO-------------------------
50010 def library fnprintbox(printfile,v,h,bv,bh,shade;tv,th,text$*6000,cpi,font$*100) !:
        ! V= verticl position in inches from the top !:
        ! H= Horizontal position from the left margin to start of box !:
        ! BV= the verticle depth of the box in inches !:
        ! BH= The Horizontal widht of the box in inches
50011 ! Shade is a number from 0 to 100 indicating how dark the shading is !:
        ! 0= no shade 100= slid black !:
        ! TV= Verticle distance below the top of the box to start text !:
        ! TH= Horizontal position from left of box to start text -1=centered !:
        ! CPI= Characters per inch of text needed for centering calculation !:
        ! FONT$= the escape sequence neede to call the font for the text
50020 ! set source and pattern transparency mode to 0 for pattern 3
50025   dim center$*2500
50030   pr #printfile: chr$(27)&"*v0n0o3T";
50040   d=720 ! 720 is the number of pixels per inch
50042   if fp(shade/10)*10>0 then pattern$=str$(fp(shade/10)*10) else pattern$="2"
50043   if pattern$="3" then shade=ip(shade/10)
50050   pr #printfile: chr$(27)&"&a"&str$(h*d)&"h"&str$(v*d)&"V"&chr$(27)&"*p0R"&chr$(27)&"*c"&str$(bh*d)&"h"&str$(bv*d)&"v"&str$(shade)&"g"&pattern$&"P"&chr$(27)&"*p0x0Y"
50060   if cpi then lastcpi=cpi else cpi=10
50065   if th=-1 then text$=trim$(text$)
50066   white$="1" : black$="0" !:
        if shade>80 then pattern$=black$ else pattern$=white$
50070   if len(trim$(text$))<1 then goto 50090
50073   te=pos(font$,"v")-1 !:
        ts=0 !:
        tf=len(font$)
50074   ts+=1 : x=val(font$(ts:te)) conv 50074
50075   center$="" !:
        if th=-1 then 
50076     if (pos(file$(printfile),"WIN:")<1 and pos(file$(printfile),"PREVIEW:")<1) then 
50077       pitch$=str$(round(val(font$(ts:te))/2,1)) : c5$="" !:
            center$=chr$(27)&"&a"&str$(h*d+bh*d/2)&"h180P"&chr$(27)&"*v0o"&pattern$&"T"&font$(1:ts-1)&pitch$&font$(te+1:tf)&srep$(srep$(text$,chr$(27)&"*v1o0T",""),chr$(27)&"*v1o1T","")&chr$(27)&"&a0P"&chr$(27)&"*v1o0T"
50078     else center$=chr$(27)&"center" : c5$="[|]"
50079   end if 
50080   if len(font$)<=2 then pr #printfile: chr$(27)&"&a"&str$((h+th)*d)&"h"&str$((v+tv)*d)&"V"&chr$(27)&"(8U"&chr$(27)&"(s"&str$(cpi)&"H"&text$&chr$(27)&"*p0x0Y"
50081   if len(font$)>2 then 
50082     if len(center$)>0 then !:
            pr #printfile: chr$(27)&"&a"&str$((h)*d)&"h"&str$((v+tv)*d)&"V"&center$&font$&c5$&text$&chr$(27)&"&a"&str$((h+bh)*d)&"H"&c5$&chr$(27)&"*p0x0Y" !:
          else !:
            pr #printfile: chr$(27)&"&a"&str$((h+th)*d)&"h"&str$((v+tv)*d)&"V"&font$&text$&chr$(27)&"*p0x0Y"
50084   end if 
50090 fnend 
50100 def library fnprintbox$*2000(v,h,bv,bh,shade) !:
        ! similar to FNPRINTBOX but meant for use with NWP
50110   dim pb$*2000
50115   pb$=chr$(27)&"*v0n03T" !:
        d=720 !:
        if fp(shade/10)*10>0 then pattern$="3" else pattern$="2"
50116   if pattern$="3" then shade=ip(shade/10)
50120   pb$= chr$(27)&"&a"&str$(h*d)&"h"&str$(v*d)&"V"&chr$(27)&"*p0R"&chr$(27)&"*c"&str$(bh*d)&"h"&str$(bv*d)&"v"&str$(shade)&"g"&pattern$&"P"&chr$(27)&"*p0x0Y"
50130   fnprintbox$=pb$
50140 fnend 
50150 def library fndrawbox$*2000(vp,hp,vl,hl,weight;fill) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
50152 ! HP = STARTING POSTION HORIZONTAL !:
        ! VP = STARTING POSITION VERTICLE !:
        ! HL = LENGHT OF HOIZONTAL !:
        ! VL = LENGTH OF VERTICLE !:
        ! WEIGHT = THICKNESS OF BORDER (0 IF NO BORDER) !:
        ! FILL = FILL DENSITY IF ANY FOR INSIDE OF BOX
50153   dim dpb$*2000
50154   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox$
50156   v=vp !:
        h=hp !:
        bv=weight !:
        bh=hl !:
        shade=100 !:
        dpb$=fnprintbox$(v,h,bv,bh,shade) !:
        ! prints top bar of box
50158   v=vp+weight !:
        h=hp !:
        bv=vl-weight !:
        bh=weight !:
        shade=100 !:
        dpb$=dpb$&fnprintbox$(v,h,bv,bh,shade) !:
        ! pr left hand bar of box
50160   v=vp+vl-weight !:
        h=hp !:
        bv=weight !:
        bh=hl-weight !:
        shade=100 !:
        dpb$=dpb$&fnprintbox$(v,h,bv,bh,shade) !:
        ! prints bottom bar of box
50162   v=vp+weight !:
        h=hp+hl-weight !:
        bv=vl-weight !:
        bh=weight !:
        shade=100 !:
        dpb$=dpb$&fnprintbox$(v,h,bv,bh,shade) !:
        ! prints right hand bar
50164   if fill>0 then 
50166     v=vp !:
          h=hp !:
          bv=vl !:
          bh=hl !:
          shade=fill !:
          dpb$=dpb$&fnprintbox$(v,h,bv,bh,shade)
50168   end if 
50170   fndrawbox$=dpb$
50175 fnend 
50200 def library fndrawbox(printfile,vp,hp,vl,hl,weight;fill) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
50210 ! HP = STARTING POSTION HORIZONTAL !:
        ! VP = STARTING POSITION VERTICLE !:
        ! HL = LENGHT OF HOIZONTAL !:
        ! VL = LENGTH OF VERTICLE !:
        ! WEIGHT = THICKNESS OF BORDER (0 IF NO BORDER) !:
        ! FILL = FILL DENSITY IF ANY FOR INSIDE OF BOX
50215   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox
50220   v=vp !:
        h=hp !:
        bv=weight !:
        bh=hl !:
        shade=100 !:
        fnprintbox(printfile,v,h,bv,bh,shade) !:
        ! prints top bar of box
50230   v=vp+weight !:
        h=hp !:
        bv=vl-weight !:
        bh=weight !:
        shade=100 !:
        fnprintbox(printfile,v,h,bv,bh,shade) !:
        ! pr left hand bar of box
50240   v=vp+vl-weight !:
        h=hp !:
        bv=weight !:
        bh=hl-weight !:
        shade=100 !:
        fnprintbox(printfile,v,h,bv,bh,shade) !:
        ! prints bottom bar of box
50250   v=vp+weight !:
        h=hp+hl-weight !:
        bv=vl-weight !:
        bh=weight !:
        shade=100 !:
        fnprintbox(printfile,v,h,bv,bh,shade) !:
        ! prints right hand bar
50260   if fill>0 then 
50270     v=vp !:
          h=hp !:
          bv=vl !:
          bh=hl !:
          shade=fill !:
          fnprintbox(printfile,v,h,bv,bh,shade)
50280   end if 
50290 fnend 
50300 ! -----------------------------------
50340 def library fnsignbox$(filnum,v,h,sigfil,short$,&pass$) !:
        !    !:
        ! | pr a PCL signature without leaving a ZERO at the insertion³!:
        ! | point                                                       | !:
        !    !
50341   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnsignbox
50342   fnsignbox(filnum,v,h,sigfil,short$,pass$)
50343 end def 
50350 def library fnsignbox(filnum,v,h,sigfil,short$,&pass$) !:
        !    !:
        ! |  pr a PCL signature                                      | !:
        ! |                                                             | !:
        !    !
50360   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fninit
50370   if not esc then let fninit
50380   dim sigin$*32000,hstart$*2000
50390 FORMSIG: form c 8,c 10,c 32000
50392   lastrec=lrec(sigfil) : srec=1
50400   restore #sigfil,rec=1: 
50405 ! input fields "23,64,c 1":pause$
50410   if not uprc$(short$)=uprc$(pshort$) then pass=0
50415 ! INPUT FIELDS "23,64,c 1": PAUSE$
50420   if srec<=lastrec then !:
          read #sigfil,using FORMSIG,rec=srec: pshort$,ppass$ norec 50430 eof ZSIGNBOX !:
        else goto ZSIGNBOX
50430   if not uprc$(trim$(pshort$))=uprc$(trim$(short$)) then srec+=1 : goto 50420
50440   if not pass and trim$(pass$)<="" and trim$(ppass$)>"" then gosub GET_PASSWORD
50450   pass+=1
50460   if not trim$(ppass$)=trim$(pass$) then goto ZSIGNBOX
50470   reread #sigfil,using FORMSIG: pshort$,ppass$,sigin$
50480   as=x=0
50490   if pos(sigin$,"||"&esc$,x+1) then !:
          x=pos(sigin$,"||"&esc$,x+1) !:
        else goto 50510
50500   as+=1 !:
        goto 50490
50510   sigin$=trim$(sigin$)
50520   d=720
50530   hstart$=esc$&"&a"&str$(h*d)&"H"
50540   pr #filnum : esc$&"*t600R"&esc$&"&u600D" !:
        ! Graphic resolution to 600 per inch !:
        ! PCL units to 600 per inch
50550   x=pos(sigin$,"||"&esc$)
50560   if x>0 then sigin$(x:x+1)=hstart$ !:
          goto 50550
50570 ! pr #FILNUM: ESC$&"&a"&STR$(V*D-AS*120)&"V"
50571   pr #filnum: esc$&"&a"&str$(v*d)&"V"
50572   pr #filnum: sigin$
50580 ZSIGNBOX: fnend 
50585 ! ----------------------------------------------------------------
50590 GET_PASSWORD: ! 
50600 passwin=fnwin("10","10","12","50","Enter Password","DS","X",passwin,0)
50610 pr #passwin,fields "2,2,c": "Enter password"
50620 rinput #passwin,fields "2,20,c 10,Ix": pass$
50630 passwin=fnclswin(1)
50640 return 
50700 ! ----------Print-PostNet Barcode at specified location ----------
50705 def library fnpostnet$*4000(text$*20) !:
        ! TEXT$=Zipcode (numeric or with dash, 5 or 9 digit)
50710   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fninit,fnprintbox
50715   if not esc then let fninit
50720   dim postpat$(10),postnet$*4000 ! ,OUTSTRING$*100
50725   fnpostnet$=postnet$=outstring$=""
50730   postpat$(1)="SSSLL" !:
        postpat$(2)="SSLSL" !:
        postpat$(3)="SSLLS" !:
        postpat$(4)="SLSSL" !:
        postpat$(5)="SLSLS" !:
        postpat$(6)="SLLSS" !:
        postpat$(7)="LSSSL" !:
        postpat$(8)="LSSLS" !:
        postpat$(9)="LSLSS" !:
        postpat$(10)="LLSSS"
50735   outstring$="" !:
        text$=srep$(text$,1,"-","") !:
        textsum=rejected=0
50740   for position=1 to len(text$)
50745     foundpat=pos("1234567890",text$(position:position),1)
50750     if foundpat>0 then !:
            goto 50755 else !:
            rejected+=1 !:
            goto 50760
50755     outstring$=outstring$&postpat$(foundpat) !:
          textsum+=foundpat
50760   next position
50765   if rejected>0 then goto REJECTZIP
50770   lenout=len(outstring$)
50775   if lenout=25 or lenout=45 then !:
          goto 50780 else !:
          goto REJECTZIP
50780   checkdigit=10-fp(textsum/10)*10
50785   outstring$="L"&outstring$&postpat$(checkdigit)&"L"
50790   lenout=len(outstring$)
50795 ! Set Source And Pattern Transparency Mode To 0 For Pattern 3
50800   postnet$=chr$(27)&"*v0n0o3T" !:
        ! Source transparent  = Transparent (0) !:
        ! Pattern transparent = Transparent (0) !:
        ! Pattern area        = solid (0)
50805   d=720 !:
        ! 720 is the number of decipoints
50810   for position=1 to lenout
50815     if outstring$(position:position)="S" then !:
            bv=.051 !:
            bo=.074 !:
          else !:
            bv=.125 !:
            bo=-.0
50820     postnet$=postnet$&chr$(27)&"&a+"&str$(.0450*d)&"h+"&str$(bo*d)&"V"&chr$(27)&"*p0R"&chr$(27)&"*c"&str$(.0160*d)&"h"&str$(bv*d)&"v0P"&chr$(27)&"&a-"&str$(bo*d)&"V"
50825   next position
50830 REJECTZIP: ! invalid character - nothing prints
50835   fnpostnet$=postnet$
50840 fnend 
50900 ! ----------Print-PostNet Barcode at specified location ----------
50910 def library fnpostnet(printfile,v,h,text$*20) !:
        !     V= vertical position in inches from the top !:
        !     H= horizontal position from the left margin to start of box!:
        ! TEXT$=Zipcode (numeric or with dash, 5 or 9 digit)
50920   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fninit,fnprintbox,fnpostnet$
50930   if not esc then let fninit
50935   dim ziptext$*5000
50936   ziptext$=fnpostnet$(text$)
50940   fnprintbox(printfile,v,h,0,0,0,0,0,ziptext$)
50945   ziptext$=""
50950 fnend 
50960 def library fndrawcorners(filnum,v,h,bv,bh,w,l;inside) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
50970 ! FILNUM    Already open pr file !:
        ! V       Top Left vertical starting position           !:
        ! H       Top left horizontal starting position         !:
        ! BV      Outside veritcal dimention                    !:
        ! BH      Outside horizontal dimension                  !:
        ! W       Width of corner bar                           !:
        ! L       Length of corner bar
50980 ! INSIDE  If TRUE then outside dimensions above are the !:
        !         inside corners of the corners !:
        !         the default is to be the outside corners of the corneres
50990 ! 
51000   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox
51010 ! 
51020 ! top left
51030   if inside then !:
          fnprintbox(filnum,v-w,h-w,w,l,100) !:
        else !:
          fnprintbox(filnum,v,h,w,l,100) ! --
51040   if inside then !:
          fnprintbox(filnum,v,h-w,l-w,w,100) !:
        else !:
          fnprintbox(filnum,v+w,h,l-w,w,100) ! |
51050 ! 
51060 ! top right
51070   if inside then !:
          fnprintbox(filnum,v-w,h+bh-l+w,w,l,100) !:
        else !:
          fnprintbox(filnum,v,h+bh-l,w,l,100) ! --
51080   if inside then !:
          fnprintbox(filnum,v,h+bh,l-w,w,100) !:
        else !:
          fnprintbox(filnum,v+w,h+bh-w,l-w,w,100) ! |
51090 ! 
51100 ! bottom left
51110   if inside then !:
          fnprintbox(filnum,v+bv,h-w,w,l,100) !:
        else !:
          fnprintbox(filnum,v+bv-w,h,w,l,100) ! --
51120   if inside then !:
          fnprintbox(filnum,v+bv-l+w,h-w,l-w,w,100) !:
        else !:
          fnprintbox(filnum,v+bv-l,h,l-w,w,100) ! |
51130 ! 
51140 ! BOTTOM RIGHT
51150   if inside then !:
          fnprintbox(filnum,v+bv,h+bh-l+w,w,l,100) !:
        else !:
          fnprintbox(filnum,v+bv-w,h+bh-l,w,l,100) ! --
51160   if inside then !:
          fnprintbox(filnum,v+bv-l+w,h+bh,l-w,w,100) !:
        else !:
          fnprintbox(filnum,v+bv-l,h+bh-w,l-w,w,100) ! |
51170 ! 
51180 fnend 
52100 ! -------------------
52110 def library fnnum$(namt,dcml,lngth) !:
        ! namt= amount to be converted into a string:!:
        ! decimal= how many decimal places the number is to be carried to:!:
        ! length=the total length of the string:!:
        ! ---------------------------
52120   if dcml then 
52130     nfrac$=fnzlpad$(fp(abs(namt))*10**dcml,dcml,0)
52135     addl=0
52140     if len(nfrac$)>dcml then 
52142       addl=1
52143       nfrac$=nfrac$(2:len(nfrac$))
52146     end if 
52150     namt$=str$(abs(ip(namt))+addl)
52160     if namt=>0 then let fnnum$=lpad$(namt$&"."&nfrac$,lngth-1)&" "
52170     if namt<0 then let fnnum$="("&lpad$(namt$&"."&nfrac$,lngth-2)&")"
52180   else 
52185     namt$=str$(abs(ip(namt)))
52190     if namt=>0 then let fnnum$=lpad$(namt$,lngth-1)&" "
52200     if namt<0 then let fnnum$="("&lpad$(namt$,lngth-2)&")"
52210   end if 
52220 fnend 
52900 ! ----------------------------------------------------------
52910 def library fnposition$*100(_v,_h)=chr$(27)&"&a"&str$(_v*720)&"v"&str$(_h*720)&"H" !:
      ! creates a POSITION parameter using inch input to position a cursor !:
      ! on a PCL or NWP page at a specific location
52912 def library fntop$(_l)=chr$(27)&"&l"&str$(_l)&"E" !:
      ! Sets the top margin in ROWS
52914 def library fnleft$(_l)=chr$(27)&"&a"&str$(_l)&"L" !:
      ! Sets the left margin in PCL
52916 def library fnlines$(_l)=chr$(27)&"&l"&str$(_l)&"F" !:
      ! Sets number of lines per page zero (0) indicated no NWP limit
52920 def library fnprintnwp$*2000(vpos,hpos,justify$,fdata$*20000;font$*100,size$*50) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
52925 !    !:
        ! | FILNUM  Already open NWP  file number                       | !:
        ! | VPOS vertical p osition in inches of the pr location     | !:
        ! | HPOS horizontal position in inches of the pr location    | !:
        ! | JUSTIFY$ L R or C to indicate left right or centered on HPOS| !:
        ! | FDATA$ the information to be printed                        | !:
        ! | FONT$ example "[SETFONT(ARIAL)]"                            | !:
        ! | SIZE$ example "[MEDIUM]"                                    | !:
        !    !
52926   if not esc then execute "PROC="&env$("PD")&"Core\fnsnap\tt"
52927   dim just$*20,justr$*20,justl$*20
52930   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnposition$
52935   if justify$="L" then just$=esc$&"left_justify" else !:
          if justify$="R" then just$=esc$&"right_justify" else !:
            if justify$="D" then justr$=esc$&"right_justify" : justl$=esc$&"left_justify" else !:
              if justify$="C" then just$=esc$&"center" else !:
                just$=""
52936   if justify$="D" then 
52937     if pos(trim$(fdata$),".")>0 then !:
            dpos=pos(trim$(fdata$),".")-1 else !:
            dpos=len(trim$(fdata$))
52938   end if 
52940   if justify$="L" then let fnprintnwp$=fnposition$(vpos,hpos)&chr$(5)&just$&font$&size$&trim$(fdata$)&chr$(5)&crlf$
52945   if justify$="C" then !:
          fnprintnwp$=fnposition$(vpos,hpos-.05-.5*len(trim$(fdata$))/10)&chr$(5)&just$&font$&size$&trim$(fdata$)&crlf$
52950   if justify$="R" then !:
          fnprintnwp$=fnposition$(vpos,hpos-.1-len(trim$(fdata$))/10)&chr$(5)&just$&font$&size$&trim$(fdata$)&fnposition$(vpos,hpos)&crlf$
52952   if justify$="D" then !:
          fnprintnwp$=fnposition$(vpos,hpos-.1-dpos/10)&chr$(5)&justr$&font$&size$&trim$(fdata$(1:dpos))&fnposition$(vpos,hpos)&justl$&trim$(fdata$(dpos+1:len(fdata$)))&chr$(5)&crlf$
52955   if justify$<=" " then !:
          fnprintnwp$=fnposition$(vpos,hpos)&chr$(5)&font$&size$&trim$(fdata$)&chr$(5)&crlf$
52960 end def 
52990 ! ----------------------------------------------------------
53000 def library fnenvelope(prtfile,datafile,size$;supret,mat innames$,nolbls,noclose,mat retadd$) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
53010 ! prtfile    = number of open pr file, !:
        !              if zero then a file will be opened !:
        ! DATAFILE   = number of existing open data file for addresses !:
        !              if nul then information from MAT NAMES$ will be used !:
        ! SIZE$      = indicator of envelope size #10, 9 etc. !:
        ! SUPRET     = Suppress return address even if overlay file exists !:
        ! if 1 supresses retuirn address if 2 prints the passed address !:
        ! MAT INNAMES$ = matrix containing single address for envelope!:
        ! NOLBLS  Number of envelopes to pr  !:
        ! NOCLOSE  if TRUE then the opened files is not closed at the end of function!:
        ! MAT RETADD$ Optional return address for envelope
53020 ! SIZE$ !:
        ! #7  = 7 1/2 x 3 7/8   !:
        ! A9  = 9 x 5 1/2  !:
        ! #9  = 9 x 4  !:
        ! #10 = 9 1/2 x 4 1/4    !:
        ! DL  = 110mm x 220mm    !:
        ! C5  = 162mm x 229mm  !:
        ! B5  = 176mm x 250mm
53030 ! MAT NAMES$  !:
        ! (1)  =  Name       !:
        ! (2)  =  Address 1  !:
        ! (3)  =  Address 2  !:
        ! (4)  =  City       !:
        ! (5)  =  State      !:
        ! (6)  =  Zip code  (#####-####) or (#####) or (#########)
53031 ! MAT RETADD$  !:
        ! (1)  =  Name       !:
        ! (2)  =  Line 2  !:
        ! (3)  =  Line 3  !:
        ! (4)  =  Line 4
53040 ! format of data file is unformatted internal data file with !:
        ! matrix layout same as mat names$
53050   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnpostnet$,fnfont$,fnprintbox,fnpostnet,fntype,fnprint
53060   overlay=0 !:
        manual=0
53065   black$=chr$(27)&"*v0o0T"
53070 !    !:
        ! | If passed data file is not open ignore passed file          | !:
        ! 
53080   if file(datafile)>-1 then restore #datafile: !:
        else datafile=0
53090   dim print_file$*100,frmaddr$*1000,names$(6)*50,cuscistzi$*50 ! ,F10$*50,POSTNET$*4000
53100   if not datafile and udim(innames$)>=6 then mat names$=innames$(1:6)
53110 !    !:
        ! |  Set default envelope to #10 business size                  | !:
        ! 
53120   h=4 !:
        v=1.5 !:
        es$="81"
53128   if size$="A9" then !:
          h=4.0 !:
          v=2.0 !:
          es$="81"
53130   if size$="#9" then !:
          h=3.9 !:
          v=1.5 !:
          es$="80"
53140   if size$="#10" then !:
          h=4 !:
          v=1.5 !:
          es$="81"
53150   if size$="DL" then !:
          h=4.5 !:
          v=1.5 !:
          es$="90"
53160   if size$="C5" then !:
          h=4.7 !:
          v=2.5 !:
          es$="91"
53170   if size$="B5" then !:
          h=5 !:
          v=2.5 !:
          es$="100"
53175   if size$="XM" then !:
          h=3.5 !:
          v=2.5 !:
          es$="81" !:
          ! Speccial set up for Christmas Cards - calls a #10 envelope
53180   if prtfile<1 then 
53190     prtfile=126
53200     prtfile+=1 !:
          if file(prtfile)>-1 then goto 53200 !:
          else !:
            open #prtfile: "name="&env$("TEMP")&"\tempfil.[WSID],replace,recl=32000,eol=none",display,output 
53205   end if 
53210   print_file$=file$(prtfile)
53212   pr #prtfile: " "
53230 !    !:
        ! | If return address is not surpressed load overlay            | !:
        ! 
53240   if not supret then 
53250 ! CLOSE #PRTFILE:
53260     if exists("envelope.ovl")=2 then !:
            fntype("envelope.ovl",prtfile) !:
            overlay=1
53270 ! 
53280   end if 
53290 !    !:
        ! |  Tell the printer that an envelope is being printed         | !:
        ! |   l81a = business envelope #10  (ES$)                       | !:
        ! |   l3h = manual feed envelope                                | !:
        ! |   l1O = landscape orientation                               | !:
        ! 
53300   if manual then pr #prtfile: chr$(27)&"&l"&es$&"al3hl1O" else pr #prtfile: chr$(27)&"&l"&es$&"al1O"
53320 !    !:
        ! |    Set font to Times New Roman Bold 10 CPI                  | !:
        ! 
53331   symbol_set$="ROMAN" !:
        proportional=1 !:
        chr_per_inch=6 !:
        style$="UPRIGHT" !:
        weight$="BOLD" !:
        typeface$="TIMES_NEW"
53332   f10$=chr$(27)&"*v0o0T"&fnfont$(symbol_set$,proportional,chr_per_inch,style$,weight$,typeface$) !:
        f8$=chr$(27)&"(s1p10v0s0b16901T" !:
        f8b$=chr$(27)&"(s1p10v0s3b16901T"
53340 !    !:
        ! |  Begin processing envelopes                                 | !:
        ! 
53350 ENVELOPE_1: ! 
53360   if datafile then read #datafile: mat names$ eof ZENVELOPE
53370   for a=1 to max(1,nolbls) step 1
53380 !    !:
          ! |   Set the cursor to upper left corner of page               | !:
          ! 
53390     pr #prtfile, using "form c 20,skip 6,c 1": chr$(27)&"*p0x0Y",""
53395     zsep$=""
53396     zi=val(srep$(names$(6),"-","")) conv 53400 !:
          zsep$="-"
53400     cuscistzi$=trim$(names$(4))&", "&trim$(names$(5))&"  "&trim$(names$(6))(1:5)
53405     bv=bh=tv=th=shade=0
53410     if len(trim$(names$(6)))>5 then !:
            cuscistzi$=cuscistzi$&zsep$&trim$(names$(6))(6:9)
53420     if trim$(names$(1))>" " then let fnprintbox(prtfile,v,h,bv,bh,shade,tv,th,names$(1),10,f10$)
53430     if trim$(names$(2))>" " then !:
            tv+=1/6 !:
            fnprintbox(prtfile,v,h,bv,bh,shade,tv,th,names$(2),10,f10$)
53440     if trim$(names$(3))>" " then tv+=1/6 !:
            fnprintbox(prtfile,v,h,bv,bh,shade,tv,th,names$(3),10,f10$)
53450     tv+=1/6 !:
          fnprintbox(prtfile,v,h,bv,bh,shade,tv,th,cuscistzi$,10,f10$)
53460 !    !:
          ! |  pr postnet bar code                                     | !:
          ! 
53470 ! fnPOSTNET(PRTFILE,V+TV+1/6,H,NAMES$(6))
53471     fnpostnet(prtfile,v-2/6,h,names$(6))
53480     if exists("envelope.ovl") and not supret then !:
            pr #prtfile: chr$(27)&"*p0x0Y"&chr$(27)&"&f198y3X" !:
            ! Activate overlay letterhead for MACRO 198
53483     if udim(mat retadd$)>0 and supret=2 then !:
            fnprintbox(prtfile,0,0,bv:=0,bh:=0,100,tv:=-.200,th:=0,retadd$(1),8,f8b$) !:
            fnprintbox(prtfile,0,0,bv:=0,bh:=0,100,tv+=1/8,th:=0,retadd$(2),8,f8$) !:
            fnprintbox(prtfile,0,0,bv:=0,bh:=0,100,tv+=1/8,th:=0,retadd$(3)) !:
            fnprintbox(prtfile,0,0,bv:=0,bh:=0,100,tv+=1/8,th:=0,retadd$(4))
53485     if a<max(1,nolbls) then pr #prtfile: newpage
53490   next a
53500   if datafile then !:
          pr #prtfile: newpage !:
          goto ENVELOPE_1
53510 !    !:
        ! |   Reset the printer to letter paper portrait mode           | !:
        ! 
53520 ZENVELOPE: pr #prtfile,using "form c 50,skip 0": chr$(27)&"&l2al1hl0O"
53530 !    !:
        ! |  If an overlay was being used delete it from printer memory | !:
        ! 
53540   if overlay then !:
          pr #prtfile,using "form c 50,skip 0": chr$(27)&"&f198y8X" !:
          ! Delete MACRO 198
53542   if noclose then goto 53570
53550   close #prtfile: 
53560   ! EXECUTE "sys "&OS_FILENAME$(ENV$("PD")&"SPOOLBAT.BAT")&" "&OS_FILENAME$(PRINT_FILE$)&" HPENVELOPE" ERROR 53570
53561   fnprint(print_file$,"ENVELOPE_PRINTER")
53562 ! fnPRINT(PRINT_FILE$,"direct:/select")
53570 fnend 
53580 !    !:
      ! |  Create a MACRO and pass the calling string back to program | !:
      ! 
53600 def library fngreybar$(macro,printfile,v,h,bv,bh,shade,head,bar)
53602   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fninit,fngreybar
53604   if not esc then let fninit
53605   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngreybar
53610   pr #printfile: esc$&"&f"&str$(macro)&"y0X"
53640   fngreybar(printfile,v,h,bv,bh,shade,head,bar)
53650   pr #printfile: esc$&"&f1x10X"
53660   fngreybar$=esc$&"&f"&str$(macro)&"y3X"
53690 fnend 
53695 !    !:
      ! |  Create a greybar PCL file                                  | !:
      ! 
53700 def library fngreybar(printfile,v,h,bv,bh,shade,head,bar) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
53701 ! printfile  open printfile into which to add greybar !:
        ! V          verticle position in inches for top left corner !:
        ! H          horizontal position in inches for top left corner  !:
        ! BV         verticle position in inches of bottom left corner  !:
        ! BH         Horizontl length of greybars  !:
        ! SHADE      density of dots of greybar 0=clear  100=solid black !:
        ! HEAD       Verticle height in inches of top box of greybar !:
        ! BAR        verticle size in inches of greybars
53702 ! Borders of greybar are 2X the desity of the greybar with a !:
        ! minimum of 10 and a maximum of 100 !:
        ! recommended shade is 10  !:
        ! shade used is rounded to a multiple of 10
53705   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox,fndrawbox,fninit
53710   if not esc then let fninit
53720   end=ip((bv-head-bar)/bar) !:
        weight=.01 !:
        offset=0/6 !:
        ! WEIGHT defines the width of the borders !:
        ! OFFSET raises the top of the box above the "normal" top of page !:
        ! END is th number of times a greybar will be printed
53730   shade=round(shade,-1)
53740   shade=max(0,min(100,shade))
53750   if not shade then bshade=100 else bshade=min(shade*2,100)
53755   pr #printfile,using "form c 20,skip 0": esc$&"&l1E"&esc$&"*p0x0Y"
53760   fnprintbox(printfile,v-offset,h,weight,bh,bshade) !:
        ! Line across top of page
53770   fnprintbox(printfile,v-offset*1.5,h,head+end*bar+offset,weight,bshade) !:
        ! Line down left side of page
53780   fnprintbox(printfile,v-offset*1.5,h+bh-weight,head+end*bar+offset,.01,bshade) !:
        ! Line down right side of page
53800   fnprintbox(printfile,v+head+end*bar,h,weight,bh,bshade) !:
        ! Line across bottom of page
53810   v+=head
53820   fnprintbox(printfile,v,h,.01,bh,bshade)
53830   fnprintbox(printfile,v,h,bar,bh,shade)
53840   fnprintbox(printfile,v+bar,h,.01,bh,bshade)
53850   v+=2*bar
53860   if v+bar+.14<bv then goto 53820
53870 fnend 
53900 def library fnlabel(filnum,mat fadd$,mat tadd$;start,number) !:
        !    !:
        ! | Create 3 1/3 x 4 labels on a laser with a from address and  | !:
        ! | a to address beginning at any label and printing 1 tp 6     | !:
        !    !
53910 !    !:
        ! | FILNUM  Already open pr file                             | !:
        ! | MAT FADD$ Return address 3 elements                         | !:
        ! | MAT TADD$  To address 3 or 4 elements                       | !:
        ! | START  Label number to start with left to right top to bottom³!:
        ! | NUMBER Number of labels to repeat 1 to 6                    | !:
        ! | Note if NUMBER is 6 and start is 2 then only 5 labels will  | !:
        ! |      pr                                                  | !:
        !    !
53920   dim f12$*50,f10$*50,f10a$*50
53921   flines=udim(mat fadd$,1)
53930   f10$=chr$(27)&"(s1p10v0s1b16901T"
53940   f12$=chr$(27)&"(s1p12v0s1b16901T"
53950   f10a$=chr$(27)&"(s1p10v0s1b16602T"
53960   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox,fndrawbox,fnpostnet,fngetzip$,fnprintnwp$
53970   execute "PROC="&env$("PD")&"Core\fnsnap\tt"
53972   if pos(uprc$(file$(filnum)),"WIN:")>0 or pos(uprc$(file$(filnum)),"PREVIEW:")>0 then win=1 else win=0
53980   if trim$(tadd$(4))>" " then zipt$=fngetzip$(trim$(tadd$(4))) else !:
          if trim$(tadd$(3))>" " then zipt$=fngetzip$(trim$(tadd$(3))) else !:
            if trim$(tadd$(2))>" " then zipt$=fngetzip$(trim$(tadd$(2)))
53990 !    !:
        ! | Set initial coordinates to zero                             | !:
        ! |                                                             | !:
        !    !
54000   v=h=b=0
54010   a=max(start,1)
54020 PRINT_LABEL: if filnum>0 and file(filnum)>-1 then pr #filnum: crlf$
54021   if a<3 then v=0 !:
        else if a<5 then v=3+2/6 !:
        else if a<7 then v=6+4/6 !:
        else a=1 : pr #filnm: newpage : goto 54021
54030 if mod(a,2) then h=0 else h=4.1 !:
        !    !:
        ! | Determine whether column 1 or column 2                      | !:
        ! |                                                             | !:
        !    !
54040 va1=v+.25 : va2=va1+1 : va3=va1+3
54050 ha1=h+.10 : ha2=ha1+3.75
54060 weight=.03
54070 !    !:
      ! | pr the FROM address in the top section of label          | !:
      ! |                                                             | !:
      !    !
54075 ! PAUSE
54080 tv=2/6 : th=-1 !:
      if win then pr #filnum: fnprintnwp$(va1+tv,ha1+(ha2-ha1)*.5,"C",fadd$(1),f12$) !:
      else !:
        fnprintbox(filnum,va1,ha1,0,ha2-ha1,0,tv,th,fadd$(1),10,f12$) !:
        pr #filnum: crlf$
54082 if flines=3 then 
54084   tv+=3/12 : th=-1 !:
        if win then pr #filnum: fnprintnwp$(va1+tv,ha1+(ha2-ha1)*.5,"C",fadd$(2),f10$) !:
        else !:
          fnprintbox(filnum,va1,ha1,0,ha2-ha1,0,tv,th,fadd$(2),10,f10$) !:
          pr #filnum: crlf$
54086   tv+=1/6 : th=-1 !:
        if win then pr #filnum: fnprintnwp$(va1+tv,ha1+(ha2-ha1)*.5,"C",fadd$(3),f10$) !:
        else !:
          fnprintbox(filnum,va1,ha1,0,ha2-ha1,0,tv,th,fadd$(3),10,f10$) !:
          pr #filnum: crlf$
54088 else 
54089   for atop=2 to udim(mat fadd$)
54090     tv+=.7/flines : th=-1
54091     if win then !:
            pr #filnum: fnprintnwp$(va1+tv,ha1+(ha2-ha1)*.5,"C",fadd$(atop),f10$) !:
          else !:
            fnprintbox(filnum,va1,ha1,0,ha2-ha1,0,tv,th,fadd$(atop),10,f10$)
54092   next atop
54105 end if 
54110 !    !:
      ! | pr the TO address in the bottom section of the label     | !:
      ! |                                                             | !:
      !    !
54120 tv=va2-va1+4/6 : th=0.5 !:
      if win then pr #filnum: fnprintnwp$(va1+tv,ha1+th,"L",tadd$(1),f10a$) !:
      else !:
        fnprintbox(filnum,va1,ha1,0,ha2-ha1,0,tv,th,tadd$(1),10,f10a$) !:
        pr #filnum: crlf$
54130 if trim$(tadd$(2))>" " then 
54131   tv+=1/6
54132   if win then !:
          pr #filnum: fnprintnwp$(va1+tv,ha1+th,"L",tadd$(2),f10a$) !:
        else !:
          fnprintbox(filnum,va1,ha1,0,ha2-ha1,0,tv,th,tadd$(2),10,f10a$) !:
          pr #filnum: crlf$
54133 end if 
54140 if trim$(tadd$(3))>" " then 
54141   tv+=1/6
54142   if win then !:
          pr #filnum: fnprintnwp$(va1+tv,ha1+th,"L",tadd$(3),f10a$) !:
        else !:
          fnprintbox(filnum,va1,ha1,0,ha2-ha1,0,tv,th,tadd$(3),10,f10a$) !:
          pr #filnum: crlf$
54143 end if 
54150 if trim$(tadd$(4))>" " then 
54151   tv+=1/6
54152   if win then !:
          pr #filnum: fnprintnwp$(va1+tv,ha1+th,"L",tadd$(4),f10a$) !:
        else !:
          fnprintbox(filnum,va1,ha1,0,ha2-ha1,0,tv,th,tadd$(4),10,f10a$) !:
          pr #filnum: crlf$
54153 end if 
54160 if trim$(zipt$)>" " then let fnpostnet(filnum,va2+2/6,ha1+th,zipt$) !:
        pr #filnum: crlf$
54170 !    !:
      ! | pr the label box and shading for each label              | !:
      ! |                                                             | !:
      !    !
54180 fndrawbox(filnum,va1,ha1,va3-va1,ha2-ha1,.03,0)
54190 fndrawbox(filnum,va1,ha1,va2-va1,ha2-ha1,0,10)
54200 fnprintbox(filnum,va2,ha1,.03,ha2-ha1,100)
54205 ! pr #FILNUM: CRLF$
54210 b+=1
54220 if b<number and a<6 then a+=1 : goto PRINT_LABEL !:
      else if b<number and a>=6 then number=max(1,number-b) !:
        b=0 : pr #filnum: newpage: : a=1 : goto PRINT_LABEL
54230 ! pr #FILNUM: NEWPAGE
54240 fnlabel=a
54250 fnend 
54260 def library fngetzip$(add$*50) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
54270   x=pos(add$," ",-1)
54280   if len(add$)-x>10 then goto NOZIP
54290   dummy=val(srep$(add$(x+1:len(add$)),"-","")) conv NOZIP
54300   fngetzip$=add$(x+1:len(add$))
54310   goto ZGETZIP
54320 NOZIP: fngetzip$=""
54330 ZGETZIP: fnend 
54400 def library fnprt3x10(filnum,mat addr$;lbl,cusnr$) !:
        !    !:
        ! | Function to pr labels on a 3 x 10 sheet of labels        | !:
        ! | lable number increments 1 each time one is printed          | !:
        !    !
54409   library env$("PD")&"Core\fnsnap\ahold.dll": fngeta,fnputa
54410   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox,fngethandle,fnmod
54415   if udim(mat addr$)<5 then mat addr$(5)
54420   if not lbl then 
54430     open #(lblno:=fngethandle): "srow=5,scol=10,rows=3,cols=25,parent=0,border=S",display,output 
54440     pr #lblno, fields "2,2,c 15,N/W:T": "Lable number"
54442     lbl=fngeta("FNPRT3X10")
54450     rinput #lblno, fields "2,18,n 3,N/W:W": lbl conv 54450
54460     if fkey=99 or fkey=93 then goto ZPRT3X10 else if not lbl>0 then goto 54450
54470   close #lblno: !:
        lblno=0
54480 end if 
54490 f8$=chr$(27)&"(s1p09v0s1b16901T"
54491 f10$=chr$(27)&"(s1p10v0s1b16901T"
54500 dim lbl$*2000,f8$*50
54510 lcol=fnmod(lbl,3) ! IF MOD(LBL,3) THEN lCOL=MOD(LBL,3) ELSE lCOL=3
54520 lrow=ip((lbl-1)/3)
54530 w=2.75 !:
      v=lrow*1+1/6 !:
      h=lcol*w-w+.1 !:
      bv=bh=shade=tv=th=0
54540 fnprintbox(filnum,v,h,bv,bh,shade,tv,th,addr$(1),cpi,f8$) !:
      if trim$(cusnr$)>"" then !:
        fnprintbox(filnum,v,h+1.9,bv,bh,shade,tv,th,cusnr$,cpi,f10$)
54550 if trim$(addr$(2))>" " then let fnprintbox(filnum,v,h,bv,bh,shade,tv,th,addr$(1),cpi,f8$)
54560 if trim$(addr$(2))>" " then let fnprintbox(filnum,v:=v+1/6,h,bv,bh,shade,tv,th,addr$(2),cpi,f8$)
54570 if trim$(addr$(3))>" " then let fnprintbox(filnum,v:=v+1/6,h,bv,bh,shade,tv,th,addr$(3),cpi,f8$)
54580 if trim$(addr$(4))>" " then let fnprintbox(filnum,v:=v+1/6,h,bv,bh,shade,tv,th,addr$(4),cpi,f8$)
54590 if trim$(addr$(5))>" " then let fnprintbox(filnum,v:=v+1/6,h,bv,bh,shade,tv,th,addr$(5))
54595 if lbl>=30 then !:
        pr #filnum: newpage !:
        lbl=0
54600 fnprt3x10=lbl+1
54610 ZPRT3X10: ! 
54615 fnputa("FNPRT3X10",lbl+1)
54620 fnend 
55000 ! ----------Print-Code3of9 Barcode At Specified Location ----------
55020 def library fncode3of9(printfile,v,h,text$*30,prntxt$;height,checkd) !:
        !      V= vertical position in inches from the top !:
        !      H= horizontal position from the left margin !:
        !      Text$=text to be coded!:
        !      Prntxt$=y,n to include readable text below the barcode
55022 ! Checkd (0=None(Default),1=Yes)!:
        !  height=height of barcode (.25 min)
55030   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox
55038   dim codepat$(44),literal$*50,outstring$*250,longtext$*100
55040 PATTERN_3OF9: ! 
55041   literal$="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%"
55043   data NNNWWNWNN,WNNWNNNNW,NNWWNNNNW,WNWWNNNNN,NNNWWNNNW,WNNWWNNNN,
55044   data NNWWWNNNN,NNNWNNWNW,WNNWNNWNN,NNWWNNWNN,WNNNNWNNW,NNWNNWNNW,
55045   data WNWNNWNNN,NNNNWWNNW,WNNNWWNNN,NNWNWWNNN,NNNNNWWNW,WNNNNWWNN,
55046   data NNWNNWWNN,NNNNWWWNN,WNNNNNNWW,NNWNNNNWW,WNWNNNNWN,NNNNWNNWW,
55047   data WNNNWNNWN,NNWNWNNWN,NNNNNNWWW,WNNNNNWWN,NNWNNNWWN,NNNNWNWWN,
55048   data WWNNNNNNW,NWWNNNNNW,WWWNNNNNN,NWNNWNNNW,WWNNWNNNN,NWWNWNNNN,
55049   data NWNNNNWNW,WWNNNNWNN,NWWNNNWNN,NWNWNWNNN,NWNWNNNWN,NWNNNWNWN,
55050   data NNNWNWNWN,NWNNWNWNN
55052   restore PATTERN_3OF9: read mat codepat$
55054   height=min(max(.25,height),.5)
55056   if uprc$(prntxt$)="Y" then offset=height else offset=height
55080   longtext$=outstring$="": textsum=rejected=0: text$=uprc$(trim$(text$)): texth=h
55100   for position=1 to len(text$)
55120     foundpat=pos(literal$,text$(position:position),1)
55140     if foundpat>0 then goto 55150 else rejected+=1: goto 55180
55150     longtext$=longtext$&" "&text$(position:position)&" "
55160     outstring$=outstring$&codepat$(foundpat)&"N": textsum+=foundpat
55180   next position
55200   if rejected>0 then goto REJECT3OF9
55220   lenout=len(outstring$)
55260   if checkd=1 then checkdigit=fp(textsum/43)*43: outstring$=outstring$&codepat$(checkdigit)&"N"
55280   outstring$=codepat$(44)&"N"&outstring$&codepat$(44)&"N"
55290   longtext$="  "&longtext$
55300   lenout=len(outstring$)
55320 ! Set Source And Pattern Transparency Mode To 0 For Pattern 3
55340   pr #printfile: escape$&"*v0n0o3T";
55360   d=720 ! 720 is the number of decipoints
55380   for position=1 to lenout step 2
55400     if outstring$(position:position)="W" then cw=.03 else cw=.01
55420     pr #printfile: escape$&"&a"&str$(h*d)&"h"&str$((v)*d)&"V"&escape$&"*p0R"&escape$&"*c"&str$(cw*d)&"h"&str$(height*d)&"v0P"
55430     if outstring$(position+1:position+1)="W" then cs=.03 else cs=.01
55440     h=h+cw+cs
55460   next position
55470   if uprc$(prntxt$)="Y" then let fnprintbox(printfile,vert+offset,texth,.4,0,0,.09,0,longtext$,17,hex$("1B")&"(11U"&hex$("1B")&"(s0p16.67h8.5v0s0b0T")
55480 REJECT3OF9: ! Invalid Character - Nothing Prints
55500 fnend 
55530 ! ----------Print-UPC-A Barcode At Specified Location ----------
55540 def library fncodeupc(printfile,v,h,text$*30;height) !:
        !           V= vertical position in inches from the top !:
        !           H= horizontal position from the left margin !:
        !           Text$=text to be coded (11 chr required)
55550 ! Checkdigit required for UPC barcode                                              !  Height=Height Of Barcode (.25 Min)
55555   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprintbox
55560   if len(text$)><11 then goto REJECTUPC
55570 !   DIM CODEPAT$(20),LITERAL$*50,OUTSTRING$*250,LONGTEXT$*100
55580 PATTERN_UPC: ! 
55590   literal$="0123456789"
55600   data 3211,2221,2122,1411,1132,1231,1114,1312,1213,3112
55610   data 3211,2221,2122,1411,1132,1231,1114,1312,1213,3112
55620   leftguard$="111": rightguard$="1111": middleguard$="11111"
55630   restore PATTERN_UPC: read mat codepat$
55640   height=min(max(.8,height),2.): texth=h: h=h+(.1*height): pitch=12/height
55650 ! adjust left margin to pr 1st character and
55660   offset=height
55670   longtext$=outstring$="": even=odd=rejected=0
55680   for position=1 to len(text$)
55690     foundpat=pos(literal$,text$(position:position),1)
55700     if foundpat>0 then goto 55710 else rejected+=1: goto 55750
55710     longtext$=longtext$&" "&text$(position:position)&" "
55720     if position>6 then foundpat+=10
55730     outstring$=outstring$&codepat$(foundpat)
55740     if int(position/2)*2=position then even+=val(text$(position:position)) else odd+=val(text$(position:position))
55750   next position
55760   if rejected>0 then goto REJECTUPC
55770   checkdigit=(10-fp((even+(odd*3))/10)*10)+1
55780   if checkdigit=11 then checkdigit=1
55790   outstring$=outstring$&codepat$(checkdigit+10)
55800   outstring$=leftguard$&outstring$(1:24)&middleguard$&outstring$(25:48)&rightguard$
55810 !  lONGTEXT$="  "&LONGTEXT$
55820   lenout=len(outstring$)
55830 ! Set Source And Pattern Transparency Mode To 0 For Pattern 3
55840   pr #printfile: escape$&"*v0n0o3T";
55850   d=720 ! 720 is the number of decipoints
55860   for position=1 to lenout step 2
55870     if (position<9 or position>51) or (position>27 and position<33) then adjust=0 else adjust=.10*height
55880     wb=val(outstring$(position:position))*.014*height ! Width of bar
55890     pr #printfile: escape$&"&a"&str$(h*d)&"h"&str$((v)*d)&"V"&escape$&"*p0R"&escape$&"*c"&str$(wb*d)&"h"&str$((height-adjust)*d)&"v0P"
55900     ws=val(outstring$(position+1:position+1))*.014*height ! Width of space
55910     h=h+wb+ws
55920   next position
55930   fnprintbox(printfile,vert+height-.08,texth,.125,0,0,.09,0,text$(1:1),17,hex$("1B")&"(s0p"&str$(pitch)&"h0s0b4099T")
55940   fnprintbox(printfile,vert+height-.08,texth+(.28*height),.125,0,0,.09,0,text$(2:6),17,hex$("1B")&"(s0p"&str$(pitch)&"h0s0b4099T")
55950   fnprintbox(printfile,vert+height-.08,texth+(.83*height),.125,0,0,.09,0,text$(7:11),17,hex$("1B")&"(s0p"&str$(pitch)&"h0s0b4099T")
55960   fnprintbox(printfile,vert+height-.08,texth+(1.45*height),.125,0,0,.09,0,str$(checkdigit-1),17,hex$("1B")&"(s0p"&str$(pitch)&"h0s0b4099T")
55970 REJECTUPC: ! Invalid Character - Nothing Prints
55980 fnend 
59800 def library fnarrays(mat one$,mat two$;&_x) !:
        !    !:
        ! | Compare two character arrays and returns false if they do   | !:
        ! | not match.  Number of elements if they do. Handles multi    | !:
        ! | dimensioned arrays                                          | !:
        ! | Optionally _x is the number of the first non-matching element³!:
        !    !
59805   multi=_a=_b=_x=0
59806   if not udim(one$)=udim(two$) then !:
          _x=min(udim(one$),udim(two$)) !:
          goto 59845
59810   multi=udim(mat one,2) error 59820
59815   if multi then _b=1
59820   do while _a<udim(one$)
59825     _a+=1 !:
          _x+=1
59830     if not multi and not one$(_a)=two$(_a) then let fnarrays=0 : goto ZARRAYS !:
          else !:
            if multi and not one$(_a,_b)=two$(_a,_b) then let fnarrays=0 : goto ZARRAYS
59835   loop 
59840   if multi and _b<multi then !:
          _b+=1 !:
          _a=0 !:
          goto 59820
59845   fnarrays=_x
59850 ZARRAYS: end def 
59855 def library fnarraysn(mat one,mat two;_x) !:
        !    !:
        ! | Compare two numeric arrays and returns false if they do     | !:
        ! | not match.  Number of elements if they do. Handles multi    | !:
        ! | dimensioned arrays                                          | !:
        ! | Optionally _x is the number of the first non-matching element³!:
        !    !
59860   multi=_a=_b=_x=0
59862   if not udim(one$)=udim(two$) then !:
          _x=min(udim(one),udim(two)) !:
          goto 59900
59865   multi=udim(mat one,2) error 59875
59870   if multi then _b=1
59875   do while _a<udim(one)
59880     _a+=1 !:
          _x+=1
59885     if not multi and not one(_a)=two(_a) then let fnarraysn=0 : goto ZARRAYSN !:
          else !:
            if multi and not one(_a,_b)=two(_a,_b) then let fnarraysn=0 : goto ZARRAYSN
59890   loop 
59895   if multi and _b<multi then !:
          _b+=1 !:
          _a=0 !:
          goto 59875
59900   fnarraysn=_x
59905 ZARRAYSN: end def 
60050 def library fnmgclr !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! Clear message and reset error processing flags
60060   gosub MGCLR
60070 fnend 
60080 MGCLR: pr #fullscr, fields mga$&"N": "" !:
      bad=clrnxt=0 !:
      return  ! Left in as a subroutine for prior version compatibility
60150 def library fnok !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
60152   fnok=0
60155   if env$("GUIMODE")="ON" then 
60160     if msgbox("Is the information correct","OK","Yn","QST")=2 then ans$="Y" else ans$="N"
60165   else 
60170     fnpm(" Data Correct?  Y  (Y or N)")
60175     input #fullscr, fields "24,18,CU 1,AE[AB],0": ans$ !:
          if not (cmdkey=0 or fkey=200) then pr #fullscr, fields "24,21,C 13,[IB]": "(Y or N ONLY)" !:
            goto 60175
60180     pr #fullscr, fields mga$&"N": ""
60185   end if 
60190   if ans$="Y" then let fnok=1 !:
          ! any answer but Y is considered false or no
60195 fnend 
60240 def library fnsavpart(sr$,sc1$,er$,ec$,clearit) !:
        ! Save part of the screen at Starting Row, Starting Column, Ending Row,!:
        ! Ending Column, Clear the part saved? (1=YES, 0=NO)
60250   ssav+=1 !:
        open #ssav: "SROW="&sr$&",SCOL="&sc1$&",EROW="&er$&",ECOL="&ec$&",N=[S]",display,outin  !:
        if clearit then !:
          pr #ssav: newpage
60260   fnsavpart=ssav !:
      fnend 
60270 def library fnrelpart(scrref,restscr) !:
        ! Release (CLOSE) the screen part saved as SCRREF, RESTSCR - Restore !:
        ! the screen part saved? (1=YES, 0=NO)
60280   if restscr then close #scrref: !:
        else !:
          close #scrref,free: 
60290   ssav-=1 !:
        fnrelpart=0 !:
      fnend 
60320 def library fnsrtary(mat l$;mat m$,desending,header,footer) !:
        ! Sorts MAT L$ in ascending order !:
        ! OPtionally based on MAT M$ !:
        ! In Descending order if DESending !:
        ! starting down from top to allow for a HEADER !:
        ! stopping before the bottom to allow for a FOOTER
60322   dim sort_wrk$(1)*500
60325   elements=udim(l$) !:
        mat idx(elements-header-footer) !:
        mat sort_wrk$(elements)
60326   if udim(mat m$)=udim(mat l$)-header-footer then mat idx=aidx(m$) else mat idx=aidx(l$)
60327   if desending and udim(mat m$)=udim(mat l$)-header-footer then mat idx=didx(m$)
60330   elements=udim(l$) !:
        mat idx(elements-header-footer) !:
        mat sort_wrk$(elements) !:
        mat sort_wrk$=l$ !:
        for i=1 to elements-header-footer !:
          sort_wrk$(i+header)=l$(idx(i)+header) !:
        next i !:
        mat l$=sort_wrk$
60332 fnend 
60340 def library fnsrtnary(mat l;mat m$,desending,header,footer) !:
        ! Sorts MAT L in ascending order !:
        ! OPtionally based on MAT M$ !:
        ! In Descending order if DESending !:
        ! starting down from top to allow for a HEADER !:
        ! stopping before the bottom to allow for a FOOTER
60342   dim sort_wrk(1)
60345   elements=udim(l) !:
        mat idx(elements-header-footer) !:
        mat sort_wrk(elements)
60346   if udim(mat m$)=udim(mat l)-header-footer then mat idx=aidx(m$) else mat idx=aidx(l)
60347   if desending and udim(mat m$)=udim(mat l)-header-footer then mat idx=didx(m$)
60350   elements=udim(l) !:
        mat idx(elements-header-footer) !:
        mat sort_wrk(elements) !:
        mat sort_wrk=l !:
        for i=1 to elements-header-footer !:
          sort_wrk(i+header)=l(idx(i)+header) !:
        next i !:
        mat l=sort_wrk
60352 fnend 
60400 def library fntrue(v) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! Set FNTRUE to one if v>0 else let FNTRUE=0
60410   if v>0 then let fntrue=1 else let fntrue=0
60420 fnend 
60480 def library fnzero(v,dv) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! SET Variable equal to the Default Variable if zero
60490   if v then let fnzero=v !:
        else !:
          fnzero=dv
60500 fnend 
60600 def library fntimmilreg(miltim,&hour,&minutes,&ampm$) !:
        ! Create the HOUR, MINUTES, and AM / PM (AMPM$) character designator !:
        ! for the military time MILTIM (Utilizes variables passed by reference.)
60610   if not miltim then !:
          hour=minutes=0 !:
          ampm$="" !:
          goto 60650
60620   t$=str$(10000+miltim)(2:5) !:
        hour=val(t$(1:2)) !:
        minutes=val(t$(3:4)) !:
        ! create the HOUR & MINUTES variables to be used after call is made
60630   if hour>=12 then ampm$="P" !:
        else !:
          ampm$="A"
60640   if hour>=13 then hour-=12 !:
        else !:
          if hour<1 then hour+=12
60650 fnend 
60700 def library fnerrtxt(erno) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
60710   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle
60715   if not esc then execute "PROC="&env$("PD")&"Core\fnsnap\tt"
60720   open #(errtxt:=fngethandle): "name="&env$("PD")&"Core\fnsnap\errors.txt",display,input 
60730   x$="Error code "&cnvrt$("PIC(####)",erno)
60740   dim er$*2000
60750   linput #errtxt: er$ eof ZERRTXT
60760   if not x$=er$(1:15) then goto 60750
60770   msgbox(srep$(er$(17:inf),"\n",crlf$),"Error Description")
60780 ZERRTXT: close #errtxt: !:
        errtxt=0
60790 fnend 
61450 def library fnclkbuf !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! Clear the keyboard buffer
61460   if kstat$<>"" then goto 61460
61470 fnend 
62900 def library fndialogold$*40(srow$,scol$,dbwidth,txtstr$*900,opt1$*40,opt2$*40,opt3$*40,remove,dfltopt,dispanykey,keywait) !:
        ! Dialog Box with optional user interaction, at Starting Row, Starting !:
        ! Column, Dialog Box Width, Text String to be displayed as dialog      !
62910   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fndialog$
62920   fndialogold$=fndialog$(srow$,scol$,dbwidth,txtstr$,opt1$,opt2$,opt3$,remove,dfltopt,dispanykey,keywait,1) !:
        ! Dialog Box with optional user interaction, at Starting Row, Starting !:
        ! Column, Dialog Box Width, Text String to be displayed as dialog !
62930 fnend 
63000 def library fndialog$*40(srow$,scol$,dbwidth,txtstr$*900,opt1$*40,opt2$*40,opt3$*40,remove,dfltopt,dispanykey,keywait;useold) !:
        ! Dialog Box with optional user interaction, at Starting Row, Starting !:
        ! Column, Dialog Box Width, Text String to be displayed as dialog!
63002   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnradiochk$
63010 ! OPT1$-OPT3$ are valid options 1-3 (If all three are null, and !:
        ! DISPANYKEY is non-zero the message 'Strike a key when ready . . .' is !:
        ! displayed. In this case, if KEYWAIT is also true, a key is required !:
        ! to exit this function. TXTSTR$ & DSCR$ are dimmed to 10 lines of 50 !:
        ! chrs each. REMOVE is 1-YES, 0-NO to remove the dialog box prior to !:
        ! exiting the function, DFLTOPT is the optional default option to !:
        ! postion the light bar at upon entry
63015   dim dscr$*1000,selform$*80,dopt$(1)*40,ddscr$(1)*100,ddwrk$(1)*40
63016   mat ddscr$(1)=("") !:
        mat ddwrk$(1)=("") !:
        dd=0
63017   winwidth=dbwidth !:
        indent=1 !:
        if env$("GUIMODE")="ON" then dbwidth=dbwidth*1 !:
          indent=1+int(dbwidth*0)
63018   indent$=str$(indent)
63019   dim caption$*80
63020   if exists(env$("PD")&"Core\fnsnap\radiochk.exe") and not useold then 
63021     if opt1$>"" then txtstr$=txtstr$&"|"&opt1$ !:
            default$="0" !:
            mat dopt$(1) : dopt$(1)=opt1$
63022     if opt2$>"" then txtstr$=txtstr$&"|"&opt2$ !:
            default$=default$&"0" !:
            mat dopt$(2) : dopt$(2)=opt2$
63023     if opt3$>"" then txtstr$=txtstr$&"|"&opt3$ !:
            default$=default$&"0" !:
            mat dopt$(3) : dopt$(3)=opt3$
63024     default$(dfltopt:dfltopt)="1" !:
          dcols=len(default$)
63025     txtstr$=srep$(txtstr$,hex$('C4'),"")
63026     caption$=dtitle$ : infile$="": left=val(scol$)*20 !:
          top=val(srow$)*20 : allow=0 : type$="B" : locate=0 !:
          nocols=1 : colwidth=150 !:
          opt=pos(fnradiochk$(caption$,infile$,left,top,allow,default$,type$,locate,nocols,colwidth,waittime,txtstr$),"1")
63027     if opt then let fndialog$=dopt$(opt) else let fndialog$=""
63028     goto ZDIALOG
63029   end if 
63030   numopts=toptlen=0: selform$="": mat dopt$(3)=("") !:
        if opt1$<>"" then numopts+=1 !:
          ol=len(opt1$) : dopt$(numopts)=opt1$ !:
          selform$="ROW#,17,C "&str$(ol)&",E;" : toptlen+=ol
63040   if opt2$<>"" then numopts+=1 !:
          ol=len(opt2$): dopt$(numopts)=opt2$ !:
          selform$=selform$&"ROW#,"&str$(toptlen+15+numopts*2)&",C "&str$(ol)&",E;": toptlen+=ol
63050   if opt3$<>"" then numopts+=1 !:
          ol=len(opt3$): dopt$(numopts)=opt3$ !:
          selform$=selform$&"ROW#,"&str$(toptlen+15+numopts*2)&",C "&str$(ol)&",E;": toptlen+=ol
63060   mat dopt$(numopts) !:
        if not numopts and dispanykey then minlen=30 !:
        else !:
          minlen=15+toptlen+numopts*2 !:
          ! Accomodates  'Strike a key when ready . . .' !:
          ! -OR- 'Select choice: ' + length of all options added up
63070   dbwidth=min(dbwidth,len(txtstr$)) !:
        dbwidth=max(dbwidth,minlen+1) !:
        dbrows=ceil(len(txtstr$)/dbwidth)+1 !:
        if len(txtstr$)<=dbwidth then dbwidth+=2 !:
          dbrows-=1: dscr$=" "&rpad$(txtstr$,dbwidth): goto 63092 !:
        else !:
          dscr$="" : mat ddscr$=("") : dd=0 !:
          for dbl=1 to dbrows-1
63072     if dbl=1 then spacpos=pos(txtstr$(1:winwidth-1)," ",-1) else spacpos=pos(txtstr$(1:dbwidth-1)," ",-1)
63080     if not spacpos then dscr$=dscr$&" "&txtstr$ !:
            dbrows-=1: goto 63092
63085     adj=dbwidth-spacpos !:
          dscr$=dscr$&" "&rpad$(txtstr$(1:dbwidth-adj),dbwidth-1) !:
          dd+=1 : mat ddscr$(dd) : mat ddwrk$(dd) !:
          ddwrk$(dd)=str$(dd)&","&indent$&",c " !:
          ddscr$(dd)=" "&rtrm$(txtstr$(1:dbwidth-adj)) !:
          lstlen=len(txtstr$(1:dbwidth-adj)) !:
          txtstr$(1:dbwidth-adj)="" !:
        next dbl
63090   if len(txtstr$)+lstlen<=dbwidth-2 then !:
          dscr$=rtrm$(dscr$)&" "&txtstr$ !:
          dbrows-=1 !:
        else !:
          dscr$=dscr$&" "&rpad$(txtstr$,dbwidth)
63092   if not numopts and not dispanykey then dbrows-=1
63095 ! INPUT FIELDS "23,64,C 1": PAUSE$
63100   dialwin=fnwin(srow$,scol$,str$(val(srow$)+dbrows),str$(val(scol$)+max(min(winwidth,len(dscr$))-1,minlen)),"","DS[X]","[W]",dialwin,1) !:
        pr #dialwin,fields "1,1,C": dscr$ !:
        if not numopts and not keywait then goto 63130
63103   if udim(ddscr$)<2 then !:
          pr #dialwin,fields "1,1,C": dscr$ else !:
          pr #dialwin,fields mat ddwrk$: mat ddscr$
63108   if not numopts and not keywait then goto 63130
63110   r$=str$(dbrows+1) !:
        if not numopts and dispanykey then !:
          pr #dialwin,fields r$&",2,C": "Strike a key when ready . . ."
63112   if not numopts then x$=kstat$(1) !:
          cmdkey(ord(x$)) error 63130 !:
          goto 63130 !:
        else !:
          selform$=srep$(selform$,"ROW#",r$) !:
          pr #dialwin,fields r$&",2,C": "Select choice: "
63120   curfld(dfltopt) !:
        rinput #dialwin, select selform$,attr "[L]": mat dopt$ !:
        if not cmdkey then let fndialog$=dopt$(curfld)
63130   dialwin=fnclswin(remove)
63140 ZDIALOG: fnend 
63200 def library fnkeysel(srow$,scol$,pp,&key$,filenbr,form$*100,bt$*80,btp$,maxl,hk$*40,hlpfil$*80,hlpele) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
63210 ! Select a key & description or name from a Point-And-Shoot selection !:
        ! Window - A direct disk access version of FNPICK. NOTE that this is !:
        ! Version 1 of this function, Version 2 is named FNKEYSEL_EX and is !:
        ! required for this version to run. See FNKEYSEL_EX for a description !:
        ! of the parameters passed.
63220   fnkeysel_ex(srow$,scol$,pp,key$,filenbr,form$,0,0,0,99,bt$,btp$,maxl,hk$,hlpfil$,hlpele) !:
        fnkeysel=cmdkey
63230 fnend 
65000 def library fnwin(sr$,sc1$,er$,ec$,wintitl$*80,bordtyp$*32,wincol$,winnum,dimlst;font$*50) !:
        ! Open a window at Starting Row, Starting Column, Enfing Row, Ending !:
        ! Column, using Window Title, Border Type AND Color string, Window !:
        ! interior color (ie; N=), Window number currently assigned, Dim (to !:
        ! screen colors) the last window border 1=YES, 0=NO.
65004   dim lsttitle$*80
65010   if dimlst and not moving and file(windev)<>-1 and winnum<>windev and lsttitle$<>"" then !:
          pr #windev,border "S": lsttitle$ !:
          ! Normal border to last window opened
65020   if not winnum then windev+=1 !:
        else !:
          windev=winnum !:
          ! Create another window device if not already assigned
65024   dim pict$*100
65030   if file(windev)=-1 then !:
          open #windev: "SROW="&sr$&",SCOL="&sc1$&",EROW="&er$&",ECOL="&ec$&font$&",N="&wincol$&",BORDER="&bordtyp$&",CAPTION="&wintitl$,display,outin  !:
          pr #windev: newpage !:
        else !:
          pr #windev,border bordtyp$: wintitl$
65040   lstwin=windev !:
        lsttitle$=wintitl$ !:
        fnwin=windev !:
      fnend 
65070 def library fnpfkey(r,c,f$,txt$*78) !:
        ! pr function key nbr and label at Row R, Column C, Key Nbr F$ !:
        ! (ie; "F10"), In light bart colors, TXT$ (ie; "End") in display only !:
        ! colors
65080   pr #fullscr,fields str$(r)&","&str$(c)&",C ,[L]": f$ !:
        pr #fullscr,fields str$(r)&","&str$(c+len(f$)+1)&",C ,[P]": txt$ !:
      fnend 
65440 def library fnnokey(c) !:
        ! Check to see if any function or cmdkey was pressed, and if so, !:
        ! produce an error for field # C
65450   if cmdkey then !:
          fnpm(srep$(srep$(srep$(" F"&str$(cmdkey)," F99"," Esc")," F90"," PgUp")," F91"," PgDn")&" is NOT a valid key.") !:
          curfld(c) !:
          fnnokey=1
65460 fnend 
65470 def library fngetk$(x) !:
        ! Get X key stroke(s) and return the upper case (if it's a letter) 'UNHEX'ed value
65471   if wbversion$(1:4)>"4.14" then let menu(0)
65472   fmenu=menu
65480   a$=kstat$(x) !:
        if len(a$)>1 then let fngetk$=unhex$(a$) !:
        else !:
          fngetk$=unhex$(uprc$(a$))
65482   if not menu=fmenu then let fnmenu
65490 fnend 
65510 def library fnclswin(clrwin) !:
        ! Manage window closed; CLRWIN - Clear the window being closed (or !:
        ! leave it there) 1=YES, 0=NO
65520   if clrwin and file(windev)<>-1 then close #windev: !:
        else !:
          if file(windev)<>-1 then close #windev,free: 
65530   windev=max(windev-1,owindev) !:
        lstwin=fnclswin=0 !:
        lsttitle$=""
65532   if file(windev)<>-1 then pr #windev,border "DS[X]": 
65535 fnend 
65750 def library fnencrypt$(pw$) !:
        ! Simple encryption routine to ward off SNOOPS, NOT serious hackers
65760   for wl=1 to len(rtrm$(pw$)) !:
          pw$(wl:wl)=chr$(ord(pw$(wl:wl))+7) !:
        next wl !:
        fnencrypt$=pw$
65770 fnend 
65780 def library fndecrypt$(pw$) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! Undo what FNENCRYPT did
65782   for wl=1 to len(rtrm$(pw$)) !:
          pw$(wl:wl)=chr$(ord(pw$(wl:wl))-7) !:
        next wl !:
        fndecrypt$=pw$
65784 fnend 
66000 def library fnpopup(mat mopt$,mat hotkey$,srow$,scol$,menutitle$*80,menuborder$,mat hm$,hmrow,hk$*40,hlpfil$*60,oplen,popnum;popreset,waittime) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
66001 ! Pop-Up Light Bar Menu - Menu OPTions, Optional - HOT Keys (Valid !:
        ! letter choices, if ommited, the first letter of the menu option will !:
        ! be used), Startiung Row, Starting Column, Menu Title, Menu Border !:
        ! (DO NOT include the color spec)
66002 ! Optional help messages, Help Message Row, Help Key, Help File, Length !:
        ! Of Longest Option, Pop-Up Menu Sequence Number
66003   if exists(env$("PD")&"Core\fnsnap\radiochk.exe") then 
66004     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnradiochk$
66005     textstring$=trim$(menutitle$) !:
          default$=""
66006     for a=1 to udim(mopt$) !:
            textstring$=textstring$&"|"&trim$(mopt$(a)) !:
            default$=default$&"0" !:
          next a
66009     default$(hmrow:hmrow)="1"
66010     colwidth=0 ! (opLEN/LEN(DEFAULT$)-LEN(DEFAULT$)*2)*10
66011 ! INPUT FIELDS "23,64,C 1": PAUSE$
66012     opt$=fnradiochk$(dummy$,"",val(scol$)*20,val(srow$)*20,allow,default$,"R",1,1,colwidth,waittime,textstring$)
66013 !   fnDLG=SRCH(DOPTS$,OPT$)
66014     fnpopup=pos(opt$,"1")
66015     goto 66420
66018   end if 
66030   dim mscr$*1794,hotkey_sav$(1)*1,popkcodes$(56)*4,popmcodes$(24)*4,popfldmat$(1)*30,popscrmat$(1)*60
66031   if env$("GUIMODE")="ON" then 
66032     popwin=popnum !:
          if file(popnum)>-1 then popnum+=1 !:
            goto 66032
66033     prows=min(max(udim(mat mopt$),2),20) !:
          mat pheadings$(1) !:
          mat pwidths(1) !:
          mat pforms$(1) !:
          pcols=oplen
66034     pwidths(1)=oplen !:
          pforms$(1)="c "&str$(oplen) !:
          pheadings$(1)=menutitle$ !:
          pparent=0 !:
          lcols=sum(mat pwidths)+2 !:
          lrows=min(20,max(3,udim(mat mopt$)+1)) !:
          arows=-1
66035     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnlistspec$
66036     plistspec$=fnlistspec$(popwin,val(srow$),val(scol$),lrows,lcols,arows,mat pheadings$,mat pwidths,mat pforms$,"Options","",0,2)
66037     pr #popwin, fields plistspec$&",=R" : (mat mopt$) !:
          curfld(1,1)
66038     input #popwin, fields plistspec$&",rowsub,sel ": pselection !:
          pk=fkey
66039     close #popwin: !:
          if not fkey then let fnpopup=pselection else let fnpopup=0
66040     goto 66420
66050   end if 
66100   if popnum>udim(popwin) then !:
          mat popwin(popnum) !:
          mat popbar(popnum) !:
          ! Establish req. nbr of elements for pointer and window arrays
66110   if popreset then popwin(popnum)=0
66120   if popkcodes$(1)="" then !:
          restore POPKEYCOD !:
          read mat popkcodes$ !:
          mat popmcodes$=popkcodes$(33:56)
66130 POPKEYCOD: data 02,05,06,0A,0B,17,09,08,07,0D,0100,19,0200,0300,0400,0500,0600,0700,0800,0900,0A00,0B00,0C00,0D00,0E00,0F00,1000,1100,1200,1300,1400,6300,2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,2A00,2B00,2C00,2D00,2E00,2F00,3000,3100,3200,3300,3400,3500,3600,3700
66140 ! MAT POPKCODES$ - Key codes for: PgUp, End, PgDn, Dn Arrow, Up Arrow,!:
        ! Home, Tab, Backspace, Shift-Tab, Enter, F1, Ctrl-Y, F2-F20, & Esc
66150   popstat=popwin(popnum) !:
        mo=udim(mopt$) !:
        mat hotkey_sav$(mo) !:
        popwin(popnum)=fnwin(srow$,scol$,str$(val(srow$)+mo-1),str$(val(scol$)+oplen+3),menutitle$,menuborder$&"[X]","[W]",popwin(popnum),1) !:
        bar=fnzero(popbar(popnum),1)
66160   if not moving then 
66170     if not popstat or lpopnum<>popnum then 
66180       lpopnum=popnum !:
            mscr$="" !:
            mat popscrmat$(mo)=("") !:
            mat popfldmat$(mo)=("") !:
            for i=1 to mo !:
              mscr$=mscr$&"  "&rpad$(mopt$(i),oplen+2) !:
              popscrmat$(i)="  "&rpad$(mopt$(i),oplen+1) !:
              popfldmat$(i)=str$(i)&",1,C "&str$(oplen+3)&",[W],X"&popmcodes$(i)
66190 !    ELSE GOTO 67040
66200         if i>udim(hotkey$) or hotkey$(i)="" then !:
                hotkey_sav$(i)=mopt$(i)(1:1) !:
              else !:
                hotkey_sav$(i)=hotkey$(i)
66210         if i>udim(hotkey$) or hotkey$(i)="" then !:
                hotkey_sav$(i)=mopt$(i)(1:1) !:
              else !:
                hotkey_sav$(i)=hotkey$(i)
66220       next i
66230     else goto 66260
66240   end if 
66250   pr #windev,fields mat popfldmat$: mat popscrmat$ !:
        ! pr #WINDEV,FIELDS "1,1,C": MSCR$
66260   pr #windev,fields str$(bar)&",2,C "&str$(oplen+2)&",[L],X"&popmcodes$(bar): " "&mopt$(bar) !:
        ! ON MOVING GOTO 67152 !:
        if hmrow and hm$(bar)<>"" then pr #fullscr,fields str$(hmrow)&",2,C 78,[L],X"&popmcodes$(bar): hm$(bar) !:
        else !:
          if hmrow then pr #fullscr,fields str$(hmrow)&",2,C 78,[S]": ""
66270   hit$=fngetk$(1) !:
        on clrnxt gosub MGCLR
66280   mov=srch(popkcodes$,hit$) !:
        ele=srch(hotkey_sav$,hex$(hit$)) !:
        if mov=-1 and ele=-1 then goto 66270 !:
        else !:
          if hit$<>"0D" and len(hit$)<=2 then !:
            pr #windev,fields str$(bar)&",2,C "&str$(oplen+2)&",[W],X"&popmcodes$(bar): " "&mopt$(bar)
66290   if ele>=1 and ele<=mo then bar=ele !:
          goto POPOUT !:
        else !:
          on mov goto POPFST,POPLST,POPLST,POPDN,POPUP,POPFST,POPDN,POPUP,POPUP,POPOUT,POPHLP,POPHLP none POPOUT
66300 POPHLP: help$(hk$&","&hlpfil$,bar) error 66260 !:
        goto 66260 !:
        ! 
66310 POPFST: bar=1 !:
        goto 66260 !:
        ! 
66320 POPLST: bar=mo !:
        goto 66260 !:
        ! 
66330 POPDN: if bar=mo then bar=1 !:
          goto 66260 !:
        else !:
          bar+=1 !:
          goto 66260 !:
          ! 
66340 POPMS: if srch(popmcodes$,hit$)=bar then !:
          hit$="0D" !:
          mov=10 !:
          pr #windev,fields "1,1,C": mscr$ !:
          goto 66280 !:
        else !:
          bar=srch(popmcodes$,hit$) !:
          pr #windev,fields mat popfldmat$: popscrmat$ !:
          goto 66260
66350 POPUP: if bar=1 then bar=mo !:
          goto 66260 !:
        else !:
          bar-=1 !:
          goto 66260 !:
          ! 
66360 POPOUT: if hmrow then pr #fullscr,fields str$(hmrow)&",2,C 78,[S]": ""
66370   xkey=took=0 !:
        if len(hit$)<=2 then took=bar !:
          goto 66400
66380   if srch(popmcodes$,hit$)>0 then took=bar=srch(popmcodes$,hit$) !:
          pr #windev,fields "1,1,C": mscr$ !:
          pr #windev,fields str$(took)&",2,C "&str$(oplen+2)&",[L],x"&popmcodes$&str$(took): " "&mopt$(took) !:
          goto 66410
66390   x=ord(hex$(hit$)) !:
        if x=>1 and x<=99 then xkey=x
66400   if hit$<>"0D" and ele<>-1 then !:
          pr #windev,fields str$(took)&",2,C "&str$(oplen+2)&",[L],x"&popmcodes$&str$(took): " "&mopt$(took)
66410   popbar(popnum)=bar !:
        fnpopup=took !:
        cmdkey(xkey)
66420 fnend 
66500 def library fnparmat(mat m$,sub$;noref) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
66510 ! Function reformats a matrix into a two dimensional matrix !:
        ! breaks occur at graphic characters designated SUB$   !:
        ! reformats to a one dimensional matrix if only one row !:
        ! unless NOREF is true
66520   col=0 !:
        cx=0 !:
        row=udim(mat m$) !:
        mat m$(row)
66530 COUNTCOLS: cx=pos(m$(1),sub$,cx+1)
66540   col+=1 : if cx>0 then goto COUNTCOLS
66550   dim mx$(1)*80
66560   mat mx$(1,col)
66570   for mr=1 to udim(mat m$)
66580     mat mx$(mr,col)
66590     for mc=1 to col
66600       if mc=1 then ms=mc else ms=me+2
66610       if mc<col then me=pos(m$(mr),sub$,ms)-1 else me=len(m$(mr))
66620       mx$(mr,mc)=srep$(srep$(srep$(srep$(m$(mr)(ms:me),hex$('C4')," "),"Ú"," "),"¿"," "),"<","")
66630     next mc
66640   next mr
66650   mat m$(row,col)=mx$
66655   if row=1 and not noref then mat m$(col)
66660 fnend 
66670 def library fngridtot(gridamount,gridrow,gridcol,gridwin,mat gridwidth,mat gridform$;gridadj) !:
        !    !:
        ! | Displays a total under a column in a grid or list           | !:
        ! | GRIDAMOUNT  Total amount to display                         | !:
        ! | GRIDROW     Row in the window where the total should be     | !:
        ! | GRIDCOL     The column under which to display               | !:
        ! | MAT GRIDWIDTH The array used to set column widths           | !:
        ! | MAT GRIDFORM$  The FORMS$ array for formatting the total    | !:
        ! | GRIDFONT   Optional font                                    | !:
        !    !
66672   dim gridmask$*50
66673   gridmask$=gridform$(gridcol)
66680   if not gridadj then !:
          pr #gridwin,fields str$(gridrow)&","&str$(sum(mat gridwidth(1:gridcol-1)))&","&gridmask$&",W/W:T": gridamount else !:
          pr #gridwin,fields str$(gridrow)&","&str$(sum(mat gridwidth(1:gridcol-1))+gridadj)&","&gridmask$&",W/W:T": gridamount
66685 end def 
66700 def library fnlistspec$*50(&listwin,sr,sc,lrows,lcols,arows,mat h$,mat w,mat f$;htext$*100,g$,parent,head) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
66702   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwinhead
66704   htext$=srep$(htext$,","," ")
66710   listwin=max(listwin,300) !:
        if file(listwin)>-1 then listwin+=1 !:
          goto 66710
66711   if wbversion$<"4.20a" then 
66712     if len(trim$(htext$))<1 then border$="S" else border$="NONE"
66713     if len(trim$(htext$))<1 then pict$="NONE" else pict$=env$("PD")&"window.gif" ! "window.bmp" ! "window.gif" ! "pane.bmp"
66715     open #listwin: "srow="&str$(sr)&",scol="&str$(sc)&",erow="&str$(sr+lrows+2)&",ecol="&str$(sc+lcols+1)&",border="&border$&",Parent="&str$(parent)&",picture="&pict$,display,outin  !:
          setenv("FILE"&str$(listwin)&"_parent",cnvrt$("pic(###)",parent))
66716     fnwinhead(listwin,htext$,lcols)
66718   else 
66720     if head=0 then border$="NONE" !:
            open #listwin: "srow="&str$(sr)&",scol="&str$(sc)&",rows="&str$(lrows+2)&",cols="&str$(lcols+1)&",border="&border$&",Parent="&str$(parent)&",picture="&pict$,display,outin  !:
            setenv("FILE"&str$(listwin)&"_parent",cnvrt$("pic(###)",parent))
66722     if head=1 then border$="S" !:
            open #listwin: "srow="&str$(sr)&",scol="&str$(sc)&",rows="&str$(lrows+2)&",cols="&str$(lcols+1)&",border="&border$&",Parent="&str$(parent)&",picture="&pict$,display,outin  !:
            setenv("FILE"&str$(listwin)&"_parent",cnvrt$("pic(###)",parent))
66724     if head=2 then open #listwin: "srow="&str$(sr)&",scol="&str$(sc)&",rows="&str$(lrows+2)&",cols="&str$(lcols+1)&",Parent=NONE, font.buttons=Arial:small,caption="&htext$&",picture="&pict$&",NO_TASK_BAR",display,outin  !:
            setenv("FILE"&str$(listwin)&"_parent",cnvrt$("pic(###)",parent))
66729   end if 
66730   if uprc$(g$)="GRID" then !:
          listspec$="1,1,GRID "&str$(max(3,lrows-arows))&"/"&str$(lcols) !:
        else listspec$="1,1,LIST "&str$(max(3,lrows-arows))&"/"&str$(lcols)
66735 ! IF WBVERSION$<"4.2a" THEN LET FNWINHEAD(LISTWIN,HTEXT$,LCOLS)
66737   lkey$=","&str$(1000+listwin)
66738   execute "config attribute [H]n/#000000:#CCCCCC,font=swiss:small:bold"
66740   pr #listwin, fields listspec$&",HEADERS,[H]": (mat h$, mat w, mat f$)
66741 ! pr #LISTWIN, FIELDS LISTSPEC$&",HEADERS": (MAT H$, MAT W, MAT F$)
66750   fnlistspec$=listspec$
66760 fnend 
66800 def library fnwinhead(hwin,htext$*100,hlen) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
66810 ! HWIN       Window number to display in !:
        ! HTEXT$     Text to display in header !:
        ! HLEN       length of window in which displayed
66811   hlen+=2
66820   open #hwin+1: "srow=1,scol=1,rows=1,cols="&str$(hlen)&",parent="&str$(hwin)&",picture=icons\bhead.bmp",display,outin 
66822   execute "config attribute [T]N/#FFFFFF:T,font=SWISS:SMALL:BOLD"
66830   pr #hwin+1,fields "1,3,c ,[T]": htext$ !:
        pr #hwin+1,fields "1,"&str$(hlen-1)&",p 1/2,,99": "icons\x.gif" !:
        pr #hwin+1,fields "1,1,p 1/1,,99": "icons\blank.gif"
66840 ! pr #HWIN, FIELDS "1,"&STR$(HLEN-1)&",P 1/3,,99": ENV$("PD")&"X.bmp"
66841 ! pr #HWIN, FIELDS "1,"&STR$(HLEN)&",P 1/3,,99": ENV$("PD")&"X.bmp"
66850 fnend 
66860 def library fnselection(selection,mat sel$,mat sel;many) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
66870 ! many set at 1 allows unlimited selections not in sequence !:
        ! many greater than 1 allows a limited number of selections sequenced
66880   if many then 
66890     if not sel(selection) then 
66900       if many>1 then !:
              sel$(selection)=cnvrt$("pic(###)",sum(mat sel)+1) !:
            else !:
              sel$(selection)="SELECTED"
66910       sel(selection)=1
66920     else 
66930       if many=1 then !:
              sel$(selection)="" !:
              sel(selection)=0 !:
            else !:
              gosub REORDER_SEL
66940     end if 
66950   end if 
66960   goto ZFNSELECTION
66970 REORDER_SEL: ! 
66980   if not many then return 
66990   delnumb=val(sel$(selection))
67000   if not delnumb or delnumb=sum(sel)+1 then return 
67010   sel$(selection)="" : sel(selection)=0
67020   for a=1 to udim(sel$)
67030     if val(sel$(a))>delnumb then !:
            sel$(a)=cnvrt$("pic(###)",val(sel$(a))-1)
67040   next a
67050   return 
67060 ZFNSELECTION: fnselection=sum(mat sel)
67070 fnend 
67200 def library fnpm(txt$*78;center) !:
        ! pr a message on the message line in message colors
67210   if not center then pr #fullscr, fields mga$&"[I]": txt$ else pr #fullscr,fields mgac$&"[I]": txt$
67211   clrnxt=1 !:
      fnend 
67300 def library fnpick_ex(pick_ops,srow$,scol$,pp,mat l$,wintitle$*80,bordtype$,maxl,hk$*40,hlpfil$*80,ptyp,hlpele,many,rptfcol,autosel,mat sel_types$,&mustrset,searchon,sstr$*40;mat sel,&scpt,&xkey,mat pickwin) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
67310   fnpick_ex=fnpick(pick_ops,srow$,scol$,pp,mat l$,wintitle$,bordtype$,maxl,hk$,hlpfil$,ptyp,hlpele,many,rptfcol,autosel,mat sel_types$,mustrset,searchon,sstr$,mat sel,scpt,xkey)
67320 fnend 
67400 def library fnpick(pick_ops,srow$,scol$,pp,mat l$,wintitle$*80,bordtype$,maxl,hk$*40,hlpfil$*80,ptyp,hlpele,many,rptfcol,autosel,mat sel_types$,&mustrset,searchon,sstr$*40;mat sel,&scpt,&xkey) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
67401 ! Pick a character string: # of pick options, Start Row, Start Column,!:
        ! Items Per Page, Array of lines containing items to be selected, !:
        ! Window Title, Windo Type, Max Length of text options, Help Key, Pick !:
        ! Type/seq nbr (For multiple selections in same prog.), Help element !:
        ! number, Pick MANY Flag:  0=NO, 1=YES ALL, 2-n=YES W/ Level # up to !:
        ! 'MANY', -1=YES with specific types (see SEL_TYPES$ below).
67402 ! Repeat Search Function Key column-leave 0 to exit w/cmdkey=5, AUTOSEL !:
        ! - Automatic Select if 1 character can be entered, MAT SEL_TYPES$ - !:
        ! Valid sorted types in regular character format, -OR- If MANY=-1 then !:
        ! these are the specific type descriptions whose 1st chr will be used !:
        ! as the HOT KEY selection for that type
67403 ! &MUSTRSET - Must reset all pointers and window display string PSCR$ !:
        ! gets initialized, Start W/ SEARCHON (1=YES,0=NO) -OR- if AUTOSEL is !:
        ! true then this is the default element number to position bar at when !:
        ! coming into the function, otherwise, If true then starting search !:
        ! string SSTR$ should be provided
67404 ! MAT SEL returns a matrix containing a 1 for each element selected or!:
        ! the sequential value of a multiple selection !:
        ! &SCPT returns the value of the screen pointer
67405 ! LIBRARY ENV$("PD")&"fnlistbox.br": FNLISTBOX
67406   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnprtpag,fnpfkey,fnwin,fninit
67407   if not esc then let fninit
67409   dim headings$(1)*80,widths(1),forms$(1)*80
67410   goto 67415 ! IF ENV$("GUIMODE")="ON" THEN
67414 ! END IF
67415   dim pscr$*1840,srchstr$*40,seldes$*80,obord$*80,sel_tkeys$(5),kcodes$(56)*4,mousecodes$(24)*4
67416   if kcodes$(1)="" then !:
          restore KEYCOD: !:
          read mat kcodes$ !:
          mat mousecodes$=kcodes$(33:56) !:
          max_sel_types=5 ! sets max nbr of selection types when MANY=-1
67417 ! fnBUTTON("Prnt list",17) ! DISPLAY BUTTONS "1,12,c 10,,17": "Print"
67418 KEYCOD: data 02,05,06,0A,0B,17,09,08,07,0D,0100,19,0200,0300,0400,0500,0600,0700,0800,0900,0A00,0B00,0C00,0D00,0E00,0F00,1000,1100,1200,1300,1400,6300,2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,2A00,2B00,2C00,2D00,2E00,2F00,3000,3100,3200,3300,3400,3500,3600,3700
67420 ! MAT KCODES$ - Key codes for: PgUp, End, PgDn, Dn Arrow, Up Arrow,!:
        ! Home, Tab, Backspace, Shift-Tab, Enter, F1, Ctrl-Y, F2-F20, & Esc
67422   optlen=maxl !:
        addl=typele=0 !:
        if many then 
67424     mat sel(pick_ops) !:
          if many>1 then addl=17 ! accom length for ' <SELECTED-LevNN> ' where NN is equal to 01-99 levels !:
          else !:
            if many=1 then addl=11 ! accom length for ' <SELECTED> '
67430     if many=-1 then 
67432       addl=0 !:
            seldes$="" !:
            mat sel_tkeys$(max_sel_types)=("") !:
            for j=1 to udim(sel_types$) !:
              seldes$=seldes$&sel_types$(j)(1:1)&"-"&sel_types$(j)&", " !:
              addl=max(addl,len(sel_types$(j))+3)
67434         sel_tkeys$(j)=sel_types$(j)(1:1) !:
            next j !:
            l=len(seldes$) !:
            seldes$(l-1:l)="." !:
            mat sel_tkeys$(j-1) ! 
67436     end if 
67438   end if 
67440   if ptyp>udim(bpts) then !:
          mat bpts(ptyp) !:
          mat ats(ptyp) !:
          mat scpts(ptyp) !:
          mat lsts$(ptyp) !:
          mat pickwin(ptyp) ! Establish req. nbr of elements for pointer arrays!:
          ! 
67442   maxl+=addl: pp=lpp=min(pp,pick_ops): srchstr$="" !:
        if mustrset then bpts(ptyp)=ats(ptyp)=scpts(ptyp)=0 !:
          obord$=""
67450   bpts(ptyp)=max(1,bpts(ptyp)): scpts(ptyp)=max(1,scpts(ptyp)) !:
        bpt=min(bpts(ptyp),pick_ops): scpt=min(scpts(ptyp),pick_ops) !:
        ats(ptyp)=max(lpp,ats(ptyp)): at=min(ats(ptyp),pick_ops) ! pointers
67460   at=min(at,pick_ops): fnprtpag(max(1,at-lpp+1),at) !:
        if moving then goto GETIT !:
        else !:
          if rtrm$(lsts$(ptyp))<>"" and rptfcol then !:
            fnpfkey(23,rptfcol,"F3","Repeat Search")
67470   if autosel then s=searchon: goto 67590 !:
        else !:
          if searchon then !:
            soset=0: srchstr$=sstr$: goto LOOKST !:
            ! these statements allow the bar to be pointed at a 'default' selctn
67480 GETIT: soset=1: if insr then goto 67590 !:
        else !:
          fnprtpickbar("[L]",bpt) !:
          on moving goto PICKOUT ! prt color bar & exit if moving window !:
          ! 
67490   if many then 
67492     selspec$=str$(val(srow$)+pp)&","&scol$&",C " !:
          if obord$="" or prev_maxl<>maxl then !:
            input #fullscr,fields selspec$&str$(maxl)&",G": obord$ !:
            prev_maxl=maxl
67496     if not sel(bpt) then 
67500       if many=-1 then 
67504         pr #fullscr,fields selspec$: " Press "&seldes$&" "
67508       else pr #fullscr,fields selspec$: " Press Enter to 'SELECT'. "
67509     else 
67510       if many=-1 then 
67512         pr #fullscr,fields selspec$: " Press "&seldes$&" "
67516       else pr #fullscr,fields selspec$: " Press Enter to 'UN-SELECT'. "
67518     end if 
67519   end if 
67520   hit$=kstat$(1) !:
        hit$=unhex$(hit$) ! must allow for mixed case, so FNGETKEY can't be used
67530   if (many=-1 and (sel_num:=srch(sel_tkeys$,uprc$(hex$(hit$))))>0) then goto PICKOUT !:
        else !:
          if (mov:=srch(kcodes$,hit$))=-1 then goto SSRCH !:
          else !:
            if hit$<>"0D" and len(hit$)<=2 then !:
              fnprtpickbar("[W]",bpt)
67540   on clrnxt gosub MGCLR
67550   if hit$<>"0D" then srchstr$=""
67560   on mov goto PPGUP,LSTPG,PPGDN,PBDN,PBUP,FSTPG,PBDN,PBUP,PBUP,PICKOUT,PICKHLP,PICKHLP,PICKOUT,LSTSRCH,PICKOUT,PICKOUT none PICKOUT
67570 PICKHLP: help$(hk$&","&hlpfil$,hlpele) error GETIT !:
        goto GETIT
67580 SSRCH: if ord(uprc$(hex$(hit$)))<32 or ord(uprc$(hex$(hit$)))>255 then goto 67520 !:
        else !:
          if autosel then typele=srch(sel_types$,hex$(hit$)): goto PICKOUT !:
          else !:
            hit$=uprc$(hex$(hit$)): on soflow ignore : srchstr$=srchstr$&hit$: on soflow goto ERRTRAP: fnpm(" Building search string: "&srchstr$): s=0
67590   if s<=0 or s>pick_ops then goto GETIT !:
        else !:
          if s>at then insr=1: goto PPGDN !:
          else !:
            if s<at-lpp+1 then insr=1: goto PPGUP !:
            else !:
              fnprtpickbar("[W]",bpt)
67600   if insr then goto 67620 !:
        else !:
          if s>pp then scpt=pp-(at-s) !:
          else !:
            if bpt=s or scpt=bpt then scpt=s !:
            else !:
              scpt+=((s-bpt))
67610   bpt=s: if (autosel and typele>0) then !:
          fnprtpickbar("[L]",typele) : goto PICKDONE !:
        else !:
          if many then goto GETIT !:
          else !:
            on clrnxt gosub MGCLR: goto GETIT
67620   bpt=s: scpt=savscpt: insr=0 !:
        if (autosel and typele>0) then !:
          fnprtpickbar("[L]",typele) : goto PICKDONE !:
        else !:
          if many then goto GETIT !:
          else !:
            on clrnxt gosub MGCLR: goto GETIT
67630 LOOKST: if rptfcol then gosub MGCLR !:
          fnpfkey(23,rptfcol,"F3","Repeat Search") !:
        else !:
          if not searchon then !:
            fnpm(" Press F3 to REPEAT the last search.") !:
            clrnxt=0 !:
          else !:
            searchon=0
67632   for k=bpt+soset to pick_ops !:
          if pos(uprc$(l$(k)),rtrm$(srchstr$)) then s=k !:
            lsts$(ptyp)=srchstr$(1:18) !:
            srchstr$="" !:
            goto 67590
67640   next k !:
        fnpm(" Search string: "&srchstr$&" was NOT found.") !:
        lsts$(ptyp)=srchstr$(1:18) !:
        srchstr$="" !:
        goto GETIT
67650 PPGUP: if scpt<>1 and barup=0 then scpt=1 !:
          bpt=max(1,at-pp+1) !:
          goto GETIT
67660   if at=lpp then bpt=1: scpt=1: goto GETIT !:
        else !:
          at=max(lpp,at-lpp): fnprtpag(max(at-(lpp)+1,1),at)
67670   if not barup then scpt=1: bpt=max(1,at-pp+1): goto GETIT !:
        else !:
          barup=0: scpt=min(pp,at): bpt=max(pp,at): goto GETIT
67680 FSTPG: if at=lpp then bpt=1: scpt=1: goto GETIT !:
        else !:
          at=lpp: bpt=1: scpt=1 !:
          fnprtpag(1,lpp): goto GETIT !:
          ! 
67690 LSTPG: if at=pick_ops then bpt=at: scpt=min(pp,at): goto GETIT !:
        else !:
          at=pick_ops: bpt=max(1,at-pp+1): scpt=1 !:
          fnprtpag(max(1,(pick_ops-lpp)+1),pick_ops): goto GETIT !:
          ! 
67700 PPGDN: if scpt<min(pp,at) then !:
          scpt=min(pp,at): bpt=max(pp,at): goto GETIT
67710   if at=pick_ops then bpt=at: scpt=min(pp,at): goto GETIT !:
        else !:
          savat=at: at=min(at+lpp,pick_ops): bpt=max(1,at-pp+1) !:
          scpt=1: fnprtpag(min(savat+1,(pick_ops-lpp)+1),at) !:
          goto GETIT
67720 PBDN: if scpt<>pp then scpt+=1: bpt=bpt+1: goto GETIT
67730   if at=pick_ops then goto GETIT !:
        else !:
          bpt+=1: at+=1 !:
          fnprtpag(at-lpp+1,min(at,pick_ops)): goto GETIT
67740 PBUP: if scpt<>1 then scpt-=1: bpt=bpt-1: goto GETIT
67750   if bpt=1 then goto GETIT !:
        else !:
          bpt-=1: at-=1 !:
          fnprtpag(at-lpp+1,at): goto GETIT
67752 MSCLK: if srch(mousecodes$,hit$)=scpt then hit$="0D" !:
          goto PICKOUT else !:
          mpt=srch(mousecodes$,hit$) !:
          bpt=min(bpt+(mpt-scpt),pick_ops) !:
          scpt=mpt !:
          fnprtpag(max(1,at-lpp+1),min(at,pick_ops)): !:
          goto GETIT
67760 LSTSRCH: if rtrm$(lsts$(ptyp))="" then 
67762     if rptfcol then let fnpm(" Search String NOT yet entered. Enter the Search String to search for first."): goto GETIT !:
          else !:
            goto PICKOUT
67764   else srchstr$=lsts$(ptyp) !:
        goto LOOKST
67770 PICKOUT: if srch(mousecodes$,hit$)>0 then goto MSCLK else bpts(ptyp)=bpt: ats(ptyp)=at: scpts(ptyp)=scpt
67780   xkey=took=0 !:
        if len(hit$)>2 then goto 67784 !:
        else !:
          if srchstr$<>"" then goto LOOKST !:
          else !:
            if autosel and hit$<>"0D" then took=typele !:
            else !:
              took=bpt
67782   if autosel and typele>0 then s=typele !:
          goto 67590 !:
        else !:
          goto 67790
67784   x=ord(hex$(hit$)) !:
        if x=>1 and x<=99 then xkey=x
67790   if xkey=7 and many>0 then let fnsetall(0): goto 67820 : else !:
          if xkey=8 and many=1 then let fnsetall(1): goto 67820 : else !:
            if (xkey=4 or xkey=2 or (xkey>10 and xkey<17)) and not many then took=bpt !:
              goto 67830 : else !:
              if xkey=17 then let fnprtmat(mat l$,"//20",env$("L1"),wintitle$) !:
                goto 67830 : else !:
                if xkey<>0 then goto 67830
67800   if hit$<>"0D" and not autosel then !:
          fnprtpickbar("[L]",took)
67810   if not many then goto 67830 !:
        else !:
          if many=-1 and hit$="0D" then !:
            fnprtpickbar("[W]",bpt) !:
            goto PBDN
67812   if (hit$<>"0D" or not sel(took)) and (many<=1 or sum(sel)<many) then !:
          fnsetsel(took,1) !:
        else !:
          if sel(took) then let fnsetsel(took,0)
67820   lsttop=lstbot=lstmaxl=0 !:
        fnprtpag(max(1,at-lpp+1),min(at,pick_ops)) !:
        if many>1 and sum(sel)=many then goto 67830 !:
        else !:
          if not took then goto GETIT !:
          else !:
            goto PBDN
67830 PICKDONE: fnpick=took !:
        cmdkey(xkey) !:
        gosub MGCLR !:
        fnclrbutton
67832   mat l$(udim(mat l$)) : fnend 
67840 def library fnsetall(sflg) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! Set ALL elements of MAT SEL & MAT L$ 1/0
67850   for i1=1 to pick_ops !:
          fnsetsel(i1,sflg) !:
        next i1 !:
      fnend 
67860 def library fnprtpag(topele,botele) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !  ! build and then pr the available options string
67865   dim pscrmat$(1)*80,pscrfld$(1)*20,c$*30,c1$*30,c2$*30
67870   pickwin(ptyp)=fnwin(srow$,scol$,str$(val(srow$)+pp-1),str$(val(scol$)+maxl+3),wintitle$,bordtype$&"[X]","[W]",pickwin(ptyp),1)
67872   if many then r$=str$(val(srow$)-1)&"," !:
        else !:
          r$=str$(val(srow$)+pp)&"," !:
          ! if MANY is true, then disp the PgUp/PgDn opts in the header so they!:
          ! don't collide with the STATUS messages displayed in the footer
67873   c$=str$(val(scol$)+maxl+3-11)&",C ,[L],x02" !:
        c1$=str$(val(scol$)+maxl+3-6)&",C ,[L],x02" !:
        c2$=str$(val(scol$)+maxl+3-6)&",C ,[L],x06"
67874   ! c$=STR$(VAL(SCOL$)+MAXL+3-11)&",C ,[L],"&STR$(PGUP) !:                      c1$=STR$(VAL(SCOL$)+MAXL+3-6)&",C ,[L],"&STR$(PGUP) !:                      c2$=STR$(VAL(SCOL$)+MAXL+3-6)&",C ,[L],"&STR$(PGDN)
67876   if at<>pick_ops and at-lpp>0 then !:
          pr f r$&c$: "PgUp/" !:
          pr f r$&c2$: "PgDn" !:
        else !:
          if at-lpp>0 then pr f r$&c1$: "PgUp" !:
          else !:
            if at<>pick_ops then pr f r$&c2$: "PgDn"
67878   if mustrset then mustrset=0 !:
        else !:
          if lsttop=topele and lstbot=botele and lstmaxl=maxl and lstptyp=ptyp then !:
            goto 67900
67880   pscr$="": lstmaxl=maxl: lsttop=topele: lstbot=botele !:
        lstptyp=ptyp: j1=0
67882   mat pscrmat$(botele-topele+1)=("") !:
        mat pscrfld$(botele-topele+1)=("")
67885   for j=topele to botele !:
          j1+=1 !:
          pscr$=pscr$&"  "&rpad$(l$(j),maxl+2) !:
          if insr and j=s then savscpt=j1
67887     pscrmat$(j1)="  "&rpad$(l$(j),maxl+1)
67888     pscrfld$(j1)=str$(j1)&",1,C "&str$(maxl+3)&",[W],"&str$(31+j1)
67890   next j !:
        if j1<>pp then pscr$=rpad$(pscr$,maxl*pp)
67900   pr #windev,fields mat pscrfld$: mat pscrmat$ !:
        ! pr #WINDEV,FIELDS "1,1,C ,[W]": PSCR$ !:
      fnend 
67910 def library fnsetsel(ele,setflg) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! Set L$(ELE) for setflg 1-ON, 0-OFF
67920   sel(ele)=setflg !:
        l$(ele)=rpad$(l$(ele),maxl) !:
        on setflg+1 goto 67930,67940
67930   gosub REORGLEV !:
        l$(ele)(optlen+1:maxl)="" !:
        goto 67950
67940   if many=-1 then !:
          l$(ele)(optlen+1:maxl)=" <"&sel_types$(sel_num)&">" !:
          sel(ele)=sel_num !:
        else !:
          if many=1 then l$(ele)(optlen+1:maxl)=" <SELECTED>" !:
          else !:
            l$(ele)(optlen+1:maxl)=" <SELECTED-Lev"&str$(100+sum(sel))(2:3)&">"
67950 fnend 
67960 REORGLEV: if many<=1 then return  !:
      else !:
        delnum=val(l$(ele)(maxl-2:maxl-1)) !:
        if not delnum or delnum=sum(sel)+1 then return  ! not a limited select or last one deleted - no reorg necessary
67970 for j=1 to pick_ops !:
        srtnum=val(l$(j)(maxl-2:maxl-1)) !:
        if srtnum>delnum then srtnum-=1 !:
          l$(j)(maxl-2:maxl-1)=str$(100+srtnum)(2:3)
67980 next j !:
      return 
67982 def library fnprtpickbar(color$,pick_ele) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! pr the fnpick light bar for COLOR$, element PICK_ELE
67984   pr #windev,fields str$(scpt)&",2,C "&str$(optlen+addl+2)&","&color$&","&str$(scpt+31): " "&l$(pick_ele)&" "
67986 fnend 
68100 def library fnauto(lastfld) !:
        ! Determines if INPUT/RINPUT fields was exited VIA an auto exit !:
        ! (control attribute E), returns 1-YES or 0-NO, LASTFLD is the field # !:
        ! that was last 'current'.
68110   if fkey>=100 then let curfld(lastfld,fkey) !:
          fnauto=1 !:
          ! if fkey>=100 then a function key or enter was NOT pressed, send !:
          ! user back to input
68120 fnend 
75000 def library fnkeysel_ex(srow$,scol$,per_win,&key$,filenbr,kform$*100,nbrfields,retkeyfield,actkeyfield,cancelkey,wintitle$*80,bordtype$,totlength,hk$*40,hlpfil$*40,hlpele;key_check,key_check_field$*30) !:
        ! Select a record from a Point-And-Shoot selection Window. This is a !:
        ! direct disk access version of FNPICK. Includes Paging and scrolling.!:
        ! EX stands for extra as in multiple fields and alternate index support
75010 ! Starting Row, Starting Column, items Per Page,  KEY field (passed !:
        ! by reference), Keyed FILE Ref Number, Form Statement, OPTIONAL Number!:
        ! of fields, Returned & Actual KEY FIELD sequence numbers specified by !:
        ! FORM$ (If omitted, nbr of key fields will be 2, ret & act key field !:
        ! nbrs will be 1), OPTIONAL Cancel 'F' key (If omitted, no Function key !:
        ! processing will occur.), Window Title, Window Type, The TOTAL length !:
        ! of all fields added together, Help Key, Help File, Help Element
75011 ! KEY_CHECK if other than 0 is the number of the field that must match!:
        ! KEY_CHECK_FIELD$ for the record to be selected
75015   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnzero,fnpfkey,fnauto,fnkeysel_ex,fninit,fnwin,fnclswin
75016   if not esc then let fninit
75020   dim keyselect$(1)*80,retkeysave$(1)*80,actkeysave$(1)*80,selfields$(1)*80,keyselatr$*300
75030   nbrfields=max(fnzero(nbrfields,2),2) !:
        retkeyfield=fnzero(retkeyfield,1) !:
        actkeyfield=fnzero(actkeyfield,1) !:
        mat keyselect$(per_win): mat retkeysave$(per_win) !:
        mat actkeysave$(per_win): mat selfields$(nbrfields) !:
        mat mastrrn(per_win) !:
        totlength+=(nbrfields+1) !:
        if rtrm$(key$)="" then moreup=0 else moreup=1
75040   if lrec(filenbr)>per_win then moredown=1 !:
        else !:
          moredown=0
75050   x$=",2,C "&str$(totlength)&"," : keyselatr$="" !:
        for i=1 to per_win !:
          if i=1 or i=per_win then xatr$="X[W];" !:
          else !:
            xatr$="[W];" !:
            ! First and last fields use the FORCED EXIT 'X' attribute to !:
            ! facilitate scrolling when arrow keys are pressed
75060     keyselatr$=keyselatr$&str$(i)&x$&xatr$ !:
        next i !:
        ! IF CANCELKEY THEN
75070 !   IF CANCELKEY>9 THEN cOL=70 ELSE cOL=71
75090 ! END IF
75100   font$=",font=systempc" !:
        keyselwin=fnwin(srow$,scol$,str$(val(srow$)+per_win-1),str$(val(scol$)+totlength+1),wintitle$,bordtype$&"[X]","[W]",0,1,font$)
75110   botrow$=str$(val(srow$)+per_win)&"," !:
        input fields botrow$&scol$&",C 11,G": obord$ !:
        longcol$=str$(val(scol$)+totlength-11+1)&",C ,[L]" !:
        shortcol$=str$(val(scol$)+totlength-6+1)&",C ,[L]"
75120   restore #filenbr,search>=rtrm$(key$): nokey 75130 !:
        goto 75140
75130   restore #filenbr: 
75140   startval=1: endval=per_win: stepval=1
75150   mat keyselect$=(""): mat retkeysave$=("") !:
        mat actkeysave$=(""): mat mastrrn=(0): lstkeycur=0
75160   for key_num=startval to endval step stepval
75170     if stepval>0 then 
75180       read #filenbr,using kform$,release: mat selfields$ locked 75190,eof SELENDFIL
75181       if key_check and not selfields$(key_check)=key_check_field$ then goto 75180
75184       goto 75210
75190       mat selfields$=("") !:
            read #filenbr,using "FORM C "&str$(kln(filenbr)),keyonly: selfields$(retkeyfield) !:
            if retkeyfield>1 then desfield=1 !:
            else !:
              desfield=retkeyfield+1
75200       selfields$(desfield)="** LOCKED **"
75210     else 
75220       read #filenbr,using kform$,prior,release: mat selfields$ locked 75230,eof SELENDFIL
75221       if key_check and not selfields$(key_check)=key_check_field$ then goto 75220
75224       goto 75260
75230       mat selfields$=("") !:
            read #filenbr,using "FORM C "&str$(kln(filenbr)),prior,keyonly: selfields$(retkeyfield) !:
            if retkeyfield>1 then desfield=1 !:
            else !:
              desfield=retkeyfield+1
75240       selfields$(desfield)="** LOCKED **"
75250     end if 
75260     retkeysave$(key_num)=selfields$(retkeyfield) !:
          actkeysave$(key_num)=selfields$(actkeyfield) !:
          mastrrn(key_num)=rec(filenbr) : keyselect$(key_num)=" " !:
          for i=1 to nbrfields !:
            keyselect$(key_num)=keyselect$(key_num)&selfields$(i)&hex$('B3') !:
          next i !:
          keyselect$(key_num)(len(keyselect$(key_num)):len(keyselect$(key_num)))=" "
75270   next key_num !:
        if not mastrrn(per_win) or lrec(filenbr)<=per_win then moredown=0 !:
        else !:
          moredown=1
75280   goto KEYPGDISP
75290 SELENDFIL: if stepval<0 then 
75300     key_num+=1: key_needed=key_num-endval !:
          for i=1 to per_win
75310       if key_num>per_win then !:
              keyselect$(i)=retkeysave$(i)=actkeysave$(i)="" !:
              mastrrn(i)=0 !:
            else !:
              keyselect$(i)=keyselect$(key_num) !:
              retkeysave$(i)=retkeysave$(key_num) !:
              actkeysave$(i)=actkeysave$(key_num) !:
              mastrrn(i)=mastrrn(key_num)
75320       key_num+=1
75330     next i !:
          startval=max(1,per_win-key_needed+1) !:
          endval=per_win !:
          stepval=1 !:
          moreup=0 !:
          if startval=1 then goto 75160
75340     read #filenbr,key=actkeysave$(startval-1),release: locked 75140
75350     if rec(filenbr)<>mastrrn(startval-1) then !:
            read #filenbr,release: !:
            goto 75350 !:
            ! dupkey situation, read until you hit the required record !:
          else !:
            goto 75160
75360   else moredown=0
75370 KEYPGDISP: pr f botrow$&srep$(longcol$,"[L]","[X]"): obord$ !:
        ! 
75380   if moreup and moredown then !:
          pr f botrow$&longcol$&",90": "PgUp\" !:
          pr f botrow$&shortcol$&",91": "PgDn" !:
        else !:
          if moreup and not moredown then !:
            pr f botrow$&shortcol$&",90": "PgUp" !:
          else !:
            if not moreup and moredown then !:
              pr f botrow$&shortcol$&",91": "PgDn"
75390 KEYSELINP: curfld(lstkeycur) !:
        ! 
75400 KEYSELINP_NP: rinput #keyselwin,select keyselatr$,attr "[L]": mat keyselect$ help 75410 !:
        lstkeycur=curfld !:
        on cmdkey goto 75410 none 75430
75410   help$(hk$&","&hlpfil$,hlpele) error 75420
75420   curfld(curfld) !:
        goto KEYSELINP_NP
75430   if cmdkey=91 or fkey=106 or fkey=109 or fkey=104 then ! Page Down, Down/Right Arrow or Tab
75440     if not cmdkey and lstkeycur=1 then !:
            lstkeycur+=1 !:
            goto KEYSELINP
75450     if not mastrrn(per_win) then goto KEYSELINP !:
            ! No record on this line
75460     read #filenbr,key=actkeysave$(per_win),release: locked KEYSELINP
75470     if rec(filenbr)<>mastrrn(per_win) then !:
            read #filenbr,release: !:
            goto 75470 !:
            ! dupkey situation, read until you hit the required record
75480     moredown=0 !:
          restore #filenbr,next: nokey KEYPGDISP,eof KEYPGDISP !:
          moreup=moredown=1 !:
          if cmdkey=91 then goto 75140
75490     for i=1 to per_win-1
75500       keyselect$(i)=keyselect$(i+1) !:
            retkeysave$(i)=retkeysave$(i+1) !:
            actkeysave$(i)=actkeysave$(i+1) !:
            mastrrn(i)=mastrrn(i+1)
75510     next i !:
          startval=per_win: endval=per_win !:
          stepval=1 !:
          goto 75160
75520   else if cmdkey=90 or fkey=105 or fkey=108 or fkey=102 then ! Page Up, Up/Left Arrow or Shift-Tab
75530     if not cmdkey and lstkeycur=per_win then !:
            lstkeycur-=1 !:
            goto KEYSELINP
75540     read #filenbr,key=actkeysave$(1),release: locked KEYSELINP
75550     if rec(filenbr)<>mastrrn(1) then !:
            read #filenbr,release: !:
            goto 75550 !:
            ! dupkey situation, read until you hit the required record
75560     moreup=0 !:
          restore #filenbr,prior: nokey KEYPGDISP,eof KEYPGDISP !:
          moreup=1 !:
          if cmdkey=90 then !:
            startval=per_win: endval=1 !:
            stepval=-1 !:
            goto 75150
75570     for i=per_win to 2 step -1
75580       keyselect$(i)=keyselect$(i-1) !:
            retkeysave$(i)=retkeysave$(i-1) !:
            actkeysave$(i)=actkeysave$(i-1) !:
            mastrrn(i)=mastrrn(i-1)
75590     next i !:
          startval=1: endval=1 !:
          stepval=1 !:
          goto 75160
75600   end if 
75610   if fnauto(lstkeycur) then goto KEYSELINP_NP
75620   key$=retkeysave$(lstkeycur) !:
        if rtrm$(key$)<>"" and not cmdkey then let fnkeysel_ex=1 !:
          ! Returns a 'true' value if line selected isn't blank and a function !:
          ! key wasn't pressed.
75630   keyselwin=fnclswin(1) !:
        if fkey_save then fkey_save=fnrelpart(fkey_save,1)
75640 fnend 
76010 def library fnfkey(akey) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
76020   fnfkey=fp(akey/1000)*1000
76030 fnend 
76060 def library fnpfkeyline(row,txt$*100;fkwin,prow,pcol,b$) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
76061 ! Note prow pcol and B$ are ignored.  They appear here only !:
        ! for cmpatability with prior versions !
76062   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwinrowcol
76063   execute "config attribute [BLANK]N/W:T,font=Arial"
76065   fnwinrowcol(fkwin,prow,pcol)
76066   b$="B"
76070   dim pftxt$(1)*80,pfmat$(1)*80
76091   if row>0 then pfrow=min(prow+1,row) else pfrow=max(1,prow+1+row)
76095   pr #fkwin, fields str$(pfrow)&",2,c "&str$(pcol-1)&",[BLANK]": rpt$(" ",pcol-3)
76110   pf=0 !:
        mat pftxt$(1)=("") !:
        mat pfmat$(1)=("")
76120   if pos("^","^")<1 then !:
          charoff=1 !:
          execute "config SEARCH_CHAR OFF" else !:
          charoff=0
76130   pfkz=pcol-len(txt$) : pfkx=0
76150   if (pfkx:=pos(txt$,"^",pfkx)) then 
76160     pf+=1 !:
          mat pftxt$(pf) !:
          mat pfmat$(pf)
76170     pfkey$="" !:
          pfky=pos(txt$," ",pfkx) !:
          pfkey$=b$&cnvrt$("pic(###)",val(txt$(pfkx+2:pfky-1))) conv 76200 !:
          pfmat$(pf)=str$(pfrow)&","&str$(pfkz+pfkx)&",C,R/HRGB:R,"&pfkey$ !:
          pftxt$(pf)=txt$(pfkx+1:pfky-1)
76175     execute "config attribute [PFKEY]N/#8F0107:T,font=Times New Roman:medium:bold"
76180     pf+=1 !:
          mat pftxt$(pf) !:
          mat pfmat$(pf) !:
          pfmat$(pf)=str$(pfrow)&","&str$(pfkz+pfkx+(pfky-pfkx-1))&",CL,[PFKEY]": txt$(pfky:len(txt$)) !:
          if pos(txt$,"^",pfky)>0 then !:
            pftxt$(pf)=txt$(pfky+1:pos(txt$,"^",pfky)-1) else !:
            pftxt$(pf)=txt$(pfky+1:len(txt$))
76190     goto 76240
76200     if uprc$(txt$(pfkx+1:pfky-1))="ESC" then pfkey$=b$&"099" !:
          else if uprc$(txt$(pfkx+1:pfky-1))="ENTER" then pfkey$=b$(1:1)&"X0D" !:
          else if uprc$(txt$(pfkx+1:pfky-1))="PGUP" then pfkey$=b$&"090" !:
          else if uprc$(txt$(pfkx+1:pfky-1))="PGDN" then pfkey$=b$&"091"
76210 pfmat$(pf)=str$(pfrow)&","&str$(pfkz+pfkx)&",C,R/HRGB:R,"&pfkey$ !:
      pftxt$(pf)=txt$(pfkx+1:pfky-1)
76220 goto 76180
76230 ! 
76240 pfkx+=1 : goto 76150
76250 end if 
76260 if pf then pr #fkwin, fields mat pfmat$: mat pftxt$
76270 ZFNPFKEYLINE: if charoff then !:
        execute "CONFIG SEARCH_CHAR 5E" !:
        charoff=0
76272 fnend 
76300 def library fnkeyline(row,txt$*100;fkwin,fkalign$,fkcolor$) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
76310 ! Prints a message line in a window
76320   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnwinrowcol
76330   execute "config attribute [BLANK]N/W:T,font=Arial"
76340   if fkcolor$>"" then !:
          execute "config attribute [FKL]N/"&fkcolor$&":T,font=Times New Roman" !:
        else !:
          execute "config attribute [FKL]N/#8F0107:T,font=Times New Roman"
76350   fnwinrowcol(fkwin,prow,pcol)
76360 ! DIM PFTXT$(1)*100,PFMAT$(1)*100
76370   if row>0 then !:
          pfrow=min(prow+1,row) else !:
          pfrow=max(1,prow+1+row)
76380   pr #fkwin, fields str$(pfrow)&",2,c "&str$(pcol-1)&",[BLANK]": rpt$(" ",pcol-3) !:
        ! Clear the message line
76390   pf=0 !:
        mat pftxt$(1)=("") !:
        mat pfmat$(1)=("")
76400   pfmat$(1)=str$(pfrow)&",2,c"&fkalign$&" "&str$(pcol-1)&",[FKL]"
76410   pftxt$(1)=txt$
76420   pfkz=pcol-len(txt$) : pfkx=0
76430   pr #fkwin, fields mat pfmat$: mat pftxt$
76440 ZFNKEYLINE: fnend 
78000 ! -------------------------------------
78001 ERRTRAP: ! Routine To Trap Unlocated Errors And pr To Display File:!:
      ! -------------------------------------
78002 if fnerrtrap(program$,line,err,cnt,variable$,curfld,"S:\Core\fnsnap\MENU") then retry 
78003 continue 
78005 def library fnerrtrap(eprog$*50,eline,eerr,ecount,evariable$,&ecurfld,emenu$) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
78010 ! EPROG$  = CURRENT PROGRAM NAME !:
        ! ELINE   = LINE NUMBER WHERE ERROR OCCURRED !:
        ! EERR    = ERROR NUMBER !:
        ! ECOUNT  = NUMBER OF FIELDS PROCESSED BEFOR THE ERROR OCCURED !:
        ! ECURFLD = THE FIELD OCCUPIED BY CURSOR WHEN ERROR OCCURED !:
        ! EMENU$  = MENU TO CHAIN TO IF ERROR CAN NOT BE RESOLVED
78015   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnemailfile,fnhelptip,fngethandle
78020   dim aic$(1)*100,dummy$(2)*50,errfile$*100
78025   aic$(1)="gtisdale@tisdalecpa.com" !:
        crlf$=chr$(13)&chr$(10)
78030 ! ERRTRAP: ! Routine To Trap Unlocated Errors And pr To Display File:!:
        ! -------------------------------------
78035   fnerrtrap=0
78040   if eerr=4228 or eerr=4229 then goto PRINTERR
78045   if eerr=61 or eerr=4148 then goto LOCKERR
78050 ERRTRAP1: on error system 
78055   fnhelptip(env$("PD")&"Core\fnsnap\","errors.txt",eprog$&" line "&cnvrt$("pic(#####)",eline)&" Count "&str$(ecount),eerr,5,20)
78060   ecufld=curfld
78065   count=cnt
78070   open #(errwin:=fngethandle): "srow=4,scol=4,erow=20,ecol=75,border=S,N=h/rgb:r,caption=PROGRAM_ERROR",display,outin 
78075   pr #errwin: newpage
78080   pr #errwin,fields "2,2,c 65": "An error has occured in the program.  Please make a note"
78085   pr #errwin,fields "3,2,c 65": "of the following information and have it ready when you "
78090   pr #errwin,fields "4,2,c 65": "call for support."
78095   pr #errwin, fields "6,4,c 65": "Program name "
78100   pr #errwin, fields "8,4,c 65": "Line number  "
78105   pr #errwin, fields "10,4,c 65": "Error number"
78110   pr #errwin, fields "12,4,c 65": "Count       "
78115   pr #errwin, fields "14,2,c 65": "Press ENTER to retry the operation or "
78120   pr #errwin, fields "15,2,c 65,n,n": "F1 for more help              "
78125   pr #errwin, fields "16,2,c 65,n,n": "F9 to return to the main menu."
78130   pr #errwin, fields "17,2,c 70,n,n": "For assistance call Advanced Informonics Corp. at (978) 287-0200"
78135   pr #errwin,fields "6,20,c 40,n,n": eprog$
78140   pr #errwin,fields "8,20,n 10,n,n": eline
78145   pr #errwin,fields "10,20,n 10,n,n": eerr
78150   pr #errwin,fields "12,20,n 10,n,n": ecount
78155   open #(errlog:=fngethandle): "name="&env$("PD")&"errlog.txt,rln=2500,use",display,output 
78156   pr #errlog: str$(days(date))&chr$(9)&session$&chr$(9)&eprog$&chr$(9)& str$(eline)&chr$(9)&str$(eerr)&chr$(9)&str$(ecount)&chr$(9)&wbversion$&chr$(9)&env$("USER_NAME")&chr$(9)&login_name$
78157   close #errlog: 
78160   open #(errfil:=fngethandle): "name=ERRLOG.[WSID],use",display,output 
78165   pr #errfil: " Date         = "&date$("MM/DD/CCYY") !:
        pr #errfil: " Workstation  = "&wsid$
78170   pr #errfil: " Program      = "&eprog$ !:
        pr #errfil: " Error        = "&str$(eerr) !:
        pr #errfil: " Line         = "&str$(eline)
78175   pr #errfil: " Count        = "&str$(ecount) !:
        pr #errfil: " Variable     = "&evariable$ !:
        pr #errfil: " Field        = "&str$(ecurfld)
78180   pr #errfil: " ------------------------------------------" !:
        errfile$=file$(errfil) !:
        close #errfil: !:
        errfil=0
78185   fnemailfile(env$("PD")&"emailq","client@client.com","Error "&cnvrt$("pic(####)",eerr)&" "&wsid$&" "&eprog$,mat aic$,errfile$,mat dummy$)
78190   input #errwin,fields "17,68,c 1,ie,n": epause$
78195   if cmdkey=0 then 
78200     close #errwin: !:
          errwin=0
78205     curfld(ecurfld)
78210     on error goto ERRTRAP
78215     fnerrtrap=1
78220     goto ZFNERRTRAP
78225   end if 
78230   if cmdkey=1 then let help$("ERR"&cnvrt$("pic(####)",eerr)&","&env$("PD")&"WBCMD.WBH",ecurfld)
78235   if cmdkey=9 then chain env$("PD")&emenu$
78240   goto 78190
78245 PRINTERR: yn$="Yn" !:
        if msgbox("A printer error has been detected."&crlf$&"Please check to see that the printer is filled with paper and is on line."&crlf$&crlf$&"Do you want to retry the operation?","Print Error",yn$,"QST")=2 then goto ZFNERRTRAP else goto ERRTRAP1
78250 LOCKERR: yn$="Yn" !:
        if msgbox("The record that you are trying to access is in use by another workstation."&crlf$&crlf$&"You can access the record as soon as the other workstation releases it, or you can abort your request and enter the general error routine."&crlf$&crlf$&"Do you want to retry the operation?","Record Lock",yn$,"QST")=2 then goto ZFNERRTRAP else goto ERRTRAP1
78255 ZFNERRTRAP: fnend 
78399 ! ------------------------------------
78400 def library fnemail(senddir$*100,mailfrom$*50,subject$*100,mat mailto$,mat message$;mat attach$,smailq$*100) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
78401 !    !:
        ! | SENDDIR$     =  Directory to place email into               | !:
        ! | MAILFROM$    =  reply email address                         | !:
        ! | SUBJECT$     =  Phrase to show as subject of email          | !:
        ! | MAT MAILTO$  =  List of email addresses to which to send    | !:
        ! | MAT MESSAGE  =  Message body to include in email            | !:
        ! | MAT ATTACH$  =  List of files to attach to emails incl dir  | !:
        ! | SMAILQ$      =  LOCATION THE SERVER SEES FOR THE EMAILQ     | !:
        ! |                 Obsolete in 2010 path is relative           | !:
        ! |   In 2011 changed to set RENAME to true if anything is in   | !:
        ! |   this variable$.  RENAME changes the file name to the 1st  | !:
        ! |   2 letters of the name followed by the time without ":"    | !:
        !    !
78405   dim wmlfile$*100
78410   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle
78435   open #(fileno:=fngethandle): "NAME="&senddir$&"\"&wsid$(1:2)&srep$(time$,":","")&".txt,RECL=500,NEW",display,output ioerr 78435
78440   pr #fileno: "<TO>"
78445   for a=1 to udim(mat mailto$)
78450     pr #fileno: trim$(mailto$(a))
78455   next a
78460   pr #fileno: "</TO>"
78465   pr #fileno: "<FROM>"
78470   pr #fileno: mailfrom$
78475   pr #fileno: "</FROM>"
78480   pr #fileno: "<SUBJECT>"
78485   pr #fileno: subject$
78490   pr #fileno: "</SUBJECT>"
78495   pr #fileno: "<BODY>"
78500   for a=1 to udim(mat message$)
78505     pr #fileno: message$(a)
78510   next a
78515   pr #fileno: "</BODY>"
78520   if udim(attach$) and trim$(attach$(1))>"" then 
78525     pr #fileno: "<ATTACH>"
78526     if smailq$>"" then !:
            rename=1 else !:
            rename=0
78530     for a=1 to udim(mat attach$) step 1
78531 ! PAUSE
78532       if pos(attach$(a),".",-1)>0 then !:
              dpos=pos(attach$(a),".",-1) else !:
              dpos=len(attach$(a)) !:
              !    !:
              ! | Set the decimal positon                                     | !:
              ! |                                                             | !:
              !    !
78533       if pos(attach$(a),"\",-1)>0 then !:
              spos=pos(attach$(a),"\",-1) else !:
              spos=0 !:
              !    !:
              ! |  Set the SLASH position for a backslash                     | !:
              ! |                                                             | !:
              !    !
78534       if not spos and pos(attach$(a),"/",-1)>0 then !:
              spos=pos(attach$(a),"/",-1)
78535       epos=len(attach$(a)) !:
            !    !:
            ! | Set the position for the end of the file name               | !:
            ! |                                                             | !:
            !    !
78536 ! 
78537       if not rename then 
78540         execute "COPY "&attach$(a)&" "&senddir$&"\attach"
78542         attach$(a)="attach\"&attach$(a)(pos(attach$(a),"\",-1)+1:len(rtrm$(attach$(a))))
78543       else 
78544         filtim$=srep$(time$,":","") !:
              fil_attach$=attach$(a)(spos+1:spos+2)&filtim$&attach$(a)(dpos:epos)
78545 ! PAUSE
78546         execute "COPY "&attach$(a)&" "&senddir$&"\attach\"&fil_attach$
78547         attach$(a)="attach\"&fil_attach$ !:
              ! aTTACH$(A)=SENDDIR$&"\attach\"&FIL_ATTACH$
78550       end if 
78555       pr #fileno: attach$(a)
78560     next a
78565     pr #fileno: "</ATTACH>"
78570   end if 
78575   wmlfile$=file$(fileno)
78580   close #fileno: 
78585 ! EXECUTE "RENAME "&WMLFILE$&" "&SREP$(WMLFILE$,".txt",".wml") IOERR EMAIL_RETRY
78586   execute "copy "&wmlfile$&" "&srep$(wmlfile$,".txt",".wml") ioerr EMAIL_RETRY
78587   sleep(2) !:
        swml+=1 !:
        if exists(srep$(wmlfile$,".txt",".wml"))=2 then !:
          execute "free "&wmlfile$ !:
        else if swml<5 then goto 78587
78588 swml=0
78590 fnend 
78592 EMAIL_RETRY: sleep(5) !:
      retry 
78594 ! ----------------------------------------
78595 def library fnemailfile(senddir$*80,mailfrom$*50,subject$*100,mat mailto$,textfile$*100;mat attach$,smailq$*80) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
78600   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnemail
78605 ! function feeds FNEMAIL reading a file into MAT MESSAGE$ then passing!:
        ! control to FNEMAIL
78610   dim message$*1000,message$(1)*1000
78615   fileno=102
78620   fileno+=1 !:
        if file(fileno)>-1 then goto 78620
78625   open #fileno: "name="&trim$(textfile$),display,input 
78630   mat message$(1)
78635 FILE_1: linput #fileno: message$ eof FILE_2
78640   fl+=1 !:
        mat message$(fl)
78645   message$(fl)=message$
78650   goto FILE_1
78655 FILE_2: close #fileno: 
78660   if udim(attach$) and trim$(attach$(1))>"" then !:
          fnemail(senddir$,mailfrom$,subject$,mat mailto$,mat message$,mat attach$) else !:
          fnemail(senddir$,mailfrom$,subject$,mat mailto$,mat message$)
78665 fnend 
80035 ! ----------------------------------------------
80040 def library fnzlpad$(number,length,decimals) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
80045 ! ----------------------------------------------
80050   dxl=len(str$(fp(number)))
80060   if dxl<decimal then dx$=rpt$("0",decimal-dxl)
80070   if fp(number)=0 and decimal=0 then nx$=ltrm$(str$(round(number,decimal)))
80080   if fp(number)=0 and decimal>0 then nx$=ltrm$(str$(round(number,decimal)))&"."&dx$
80090   if fp(number)>0 and dxl<decimal then nx$=ltrm$(str$(round(number,decimal)))&dx$
80100   if fp(number)>0 and dxl>=decimal then nx$=ltrm$(str$(round(number,decimal)))
80110   lx=len(nx$)
80120   fnzlpad$=rpt$("0",length-lx)&nx$
80130 fnend 
80160 ! ====================================================================
80170 def library fnscreen(scrno;screenfile,mat scratr$,mat screen$,mat inwrk$,mat inflda$,mat inwrkh$,nopaint) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    ! ! Retrieve and display screen
80180 ! SCRNO        record number of screen to display !:
        ! SCREENFILE    file number of already open screen file !:
        ! MAT SCRATR$   21 x 15 Matrix of screen line attributes    !:
        ! MAT SCREEN    21 x 80 picture of screen to display !:
        ! MAT INWRK$    60 x 20/40 Matrix for screen field attributes !:
        ! MAT INFLDA$   60 x 20/40 Matrix for screen attrtributes !:
        ! MAT INWRKH$   60 x 210 matrix for help fields !:
        ! NOPAINT       flag to prevent screen from repainting
80190 ! ====================================================================
80200   if not scrno then 
80210     mat screen$=(chr$(225))
80220   else 
80230     recno=scrno+1
80240 SCRNFORM: form x 50,21*c 78,60*c 20
80250 SCRNFORMV2: form x 50,21*c 78,60*c 40
80260     if screen><scrno then 
80270       if version(screenfile)<2 then !:
              read #screenfile,using SCRNFORM,rec=recno: mat screen$,mat inflda$ !:
            else !:
              read #screenfile,using SCRNFORMV2,rec=recno: mat screen$,mat inflda$
80271       if version(screenfile)<2 then !:
              read #screenfile,using SCRNFORM,rec=recno: mat screen$,mat inflda$ !:
            else !:
              read #screenfile,using SCRNFORMV2,rec=recno: mat screen$,mat inflda$
80280       mat inwrkh$=("X")
80290       restore #screenfile+1,search=cnvrt$("pic(###)",scrno): nokey 80360 eof 80360
80300       read #screenfile+1,using "form n 3,n 2": hscrno,hfldno eof 80360
80310       if hscrno=scrno then !:
              reread #screenfile+1, using "form x 5,c 1,c 2,c 200": hlevel$,hloc$,htext$ else !:
              goto 80360
80320       if hfldno>0 then inwrkh$(hfldno)=hlevel$&rtrm$(hloc$)&";"&rtrm$(htext$)&";"
80330       goto 80300
80340     end if 
80350   end if 
80360 DSPSCRN: if not nopaint then 
80362     pr newpage ! -               Write image to display
80364     pr f mat scratr$: mat screen$
80370   end if 
80380   mat inwrk$=inflda$
80390   fnscreen=scrno
80400 fnend 
80500 ! ---------------------
80510 def library fnproper$*60(a_in$*60) !:
        ! -----------------------
80520 ! DIM A$*2000
80530   a$=rtrm$(lwrc$(a_in$)): !:
        a$(1:1)=uprc$(a$(1:1))
80531   if a$(1:1)=" " or a$(1:1)="%" then a$(2:2)=uprc$(a$(2:2))
80533   a=1: !:
        b=min(pos(a$," ",2),pos(a$,".",2)): !:
        goto 80580
80540   a+=1
80550   if not pos(a$," ",a) then goto ZPROPER
80560   a=pos(a$," ",a): !:
        b=min(pos(a$," ",a+1),pos(a$,".",a+1)) !:
        a$(a+1:a+1)=uprc$(a$(a+1:a+1))
80570   a=pos(a$," ",a): !:
        b=min(pos(a$," ",a+1),pos(a$,"'",a+1)) !:
        a$(a+1:a+1)=uprc$(a$(a+1:a+1))
80580   if b-a>0 and b-a<3 then 
80590     if a$(b:b)><"." then a$(a+1:b)=uprc$(a$(a+1:b))
80600   end if 
80610   goto 80540
80620 ZPROPER: ! 
80630   x=pos(a$," Mac") !:
        x1=pos(a$," ",x) !:
        if x and (len(a$)>x+6 or x1>x+6) then a$(x+4:x+4)=uprc$(a$(x+4:x+4))
80640   x=pos(a$," Mc") !:
        if x then a$(x+3:x+3)=uprc$(a$(x+3:x+3))
80650   x=pos(a$," O'") !:
        if x then a$(x+3:x+3)=uprc$(a$(x+3:x+3))
80660   x=pos(a$,"-") !:
        if x then a$(x+1:x+1)=uprc$(a$(x+1:x+1))
80670   x=pos(a$,".") !:
        if x then a$(x+1:x+1)=uprc$(a$(x+1:x+1))
80672   x=pos(a$,"&") !:
        if x then a$(x+1:x+1)=uprc$(a$(x+1:x+1))
80673   x=pos(a$,"%") !:
        if x then a$(x+1:x+1)=uprc$(a$(x+1:x+1))
80675   a$=rtrm$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(a$&" "," And "," and ")," Ii"," II")," Iii"," III")," IIi"," III")," Md"," MD")," Pc"," PC")," Rn"," RN"),"P.o.","P.O.")," Cpa"," CPA"),"Llc","LLC"),"Llp","LLP"),"C/o ","c/o "),"D/b/a ","d/b/a "),"MacHine","Machine"),"M.d.","M.D."),"Adp ","ADP ")," Of "," of "),"Po Box","PO Box")," A'h"," A'H"),"At&T","AT&T"),"Adp ","ADP "),"Cpa ","CPA "),"Cch ","CCH "),"Jsp ","JSP "),"Mcw","McW")," Dds"," DDS"))
80676   a$=rtrm$(srep$(srep$(srep$(srep$(srep$(a$&" ","P.o.b.","P.O.B.")," Pto "," PTO ")," Vp"," VP"),"Sbli ","SBLI "),"Ibm ","IBM "))
80677   a$=rtrm$(srep$(srep$(srep$(a$&" "," Or "," OR ")," Nm "," NM ")," Co "," CO "))
80678   a$=rtrm$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(a$&" "," Ma "," MA ")," Me "," ME ")," Ri "," RI ")," Ct "," CT ")," Nh "," NH ")," Vt "," VT ")," Ny "," NY ")," Nj "," NJ ")," Pa "," PA ")," Dc "," DC ")," Va "," VA ")," Nc "," NC ")," Sc "," SC ")," Ga "," GA ")," Fl "," FL "),", Al ",", AL ")," Mi "," MI "),", Ms ",", MS "),", La ",", LA ")," Mo "," MO ")," Tn "," TN ")," Ky "," KY "),", Oh ",", OH ")," Il "," IL "),", Ca ",", CA "))
80679   a$=srep$(srep$(srep$(a$," Leann"," LeAnn"),"Ornac","ORNAC"),"Smartparent","SmartParent")
80680   fnproper$=a$
80690 fnend 
80900 ! ----------------------------
80902 def library fnsrchchart(cfil1,cfila,&cstart$;afkey,gl) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
80904 ! ----------------------------
80905   library env$("PD")&"apsysc\apsrch.dll": fnsrchchart
80906   fnsrchchart=fnsrchchart(cfil1,cfila,cstart$,afkey,gl)
80908 fnend 
81000 ! ----------------------------
81010 def library fnsrchap(cfil1,cfila,&cstart$;afkey) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
81020 ! ----------------------------
81025   library env$("PD")&"apsysc\apsrch.dll": fnsrchap
81030   fnsrchap=fnsrchap(cfil1,cfila,cstart$,afkey)
81040 fnend 
81150 ! ----------------------------
81160 def library fnsrchjc(cfil1,cfila,&cstart$;afkey) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
81170 ! ----------------------------
81180 ! DIM CFILFORM$*255
81190   cfil=val(cstart$) conv SRCHJCA
81200   cfil=cfil1 : !:
        ckey=1: !:
        cflds=2: !:
        cfilform$=cform$("form c 5,POS 6,c 30")
81210   restore #cfil,search=cstart$: nokey SRCHJC1 ioerr ZSRCHJC
81220   fnsrchjc=0
81230   if not afkey=2 then goto ZSRCHJC else goto SRCHJC1
81240 SRCHJCA: cfil=cfila : !:
        ckey=3: !:
        cflds=3: !:
        cfilform$=cform$("form pos 6,c 5,pos 6,c 30,pos 1,c 5")
81250 SRCHJC1: fnkeysel_ex("3","20",min(lrec(cfil1),18),cstart$,cfil,cfilform$,cflds,ckey,1,99,"SELECT JOB","DS",48," "," ",0)
81260 ! 
81270 ZSRCHJC: ! 
81280 fnend 
81290 ! ----------------------------
81300 def library fnsrchjccd(cfil1,cfila,&cstart$;afkey) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
81310 ! ----------------------------
81320 ! DIM CFILFORM$*255
81330   cfil=val(cstart$) conv SRCHJCCDA
81340   cfil=cfil1 : !:
        ckey=1: !:
        cflds=2: !:
        cfilform$=cform$("form c 4,POS 5,c 30")
81350   restore #cfil,search=cstart$: nokey SRCHJCCD1 ioerr ZSRCHJCCD
81360   fnsrchjccd=0
81370   if not afkey=2 then goto ZSRCHJCCD else goto SRCHJCCD1
81380 SRCHJCCDA: cfil=cfila : !:
        ckey=3: !:
        cflds=3: !:
        cfilform$=cform$("form pos 5,c 4,pos 5,c 30,pos 1,c 4")
81390 SRCHJCCD1: fnkeysel_ex("3","20",18,cstart$,cfil,cfilform$,cflds,ckey,1,99,"SELECT COST CODE","DS",48," "," ",0)
81400 ! 
81410 ZSRCHJCCD: ! 
81420 fnend 
81430 ! ----------------------------
81440 def library fnsrchar(cfil1,cfila,&cstart$;afkey) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
81450 ! ----------------------------
81460   library env$("PD")&"arsysc\arsrch.dll": fnsrchar
81470   fnsrchar(cfil1,cfila,cstart$,afkey)
81480 fnend 
81630 ! ----------------------------
81640 def library fnsrchshpto(cfil1,cfilcus,cusnr$,&cstart$;afkey,reset) !:
        ! ----------------------------
81650   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnlistspec$
81660   library env$("PD")&"fnlistbox.br": fnkeysel_ex
81670   dim cfilform$*255,csdetail$(100)*80,title$*100
81680   dim cusnm$*30,cusa1$*25,cusa2$*25,cusci$*18
81690   dim scusnm$*30,sshpnm$*30,sshpa1$*25,sshpa2$*25,sshpci$*18,scusa1$*25,scusa2$*25,scusci$*18
81700   dim stcusnr$(1),stcusnm$(1)*30,stcusa1$(1)*25,stcusa2$(1)*25,stcusci$(1)*18,stcusst$(1)*2
81710 FRMCUSMASM: form pos 8,c 30,2*c 25,c 18,c 2,c 9
81720   read #cfilcus,using FRMCUSMASM,key=cusnr$: cusnm$,cusa1$,cusa2$,cusci$,cusst$,cuszi$ nokey ZSRCH_SHIPTO
81730 ! rESET=1
81740 ! -----------------------
81750 SRCH_SHIPTO: ! 
81760 ! -----------------------
81770   restore #cfil1,search=cusnr$: nokey ZSRCH_SHIPTO
81780   a=1
81790 ! cSDETAIL$(A)="00"&" "&CUSNM$(1:20)&" "&CUSA1$(1:20)&", "&RTRM$(CUSCI$)&", "&CUSST$
81800   csdetail$(a)="00"&hex$('B3')&cusnm$(1:20)&hex$('B3')&cusa1$(1:20)&hex$('B3')&cusci$(1:15)&hex$('B3')&cusst$
81810   mat stcusnr$(a) !:
        mat stcusnm$(a) !:
        mat stcusa1$(a) !:
        mat stcusa2$(a) !:
        mat stcusci$(a) !:
        mat stcusst$(a) !:
        mat stcuszi$(a)
81820   stcusnr$(a)="00" !:
        stcusnm$(a)=cusnm$ !:
        stcusa1$(a)=cusa1$ !:
        stcusa2$(a)=cusa2$ !:
        stcusci$(a)=cusci$ !:
        stcusst$(a)=cusst$ !:
        stcuszi$(a)=cuszi$
81830   restore #cfil1,search=cusnr$: nokey ZSRCH_SHIPTO
81840 FRMSHP: form c 5,c 2,pos 120,c 30,pos 8,c 25,c 25,c 18,c 2,c 9
81850 SRCH_SHIPTO1: read #cfil1,using FRMSHP: scusnr$,sshpnr$,sshpnm$,sshpa1$,sshpa2$,sshpci$,sshpst$,sshpzi$ eof SRCH_SHIPTO2
81860   if cusnr$=scusnr$ then 
81870     a+=1
81880     csdetail$(a)=sshpnr$&hex$('B3')&sshpnm$(1:20)&hex$('B3')&sshpa1$(1:20)&hex$('B3')&sshpci$(1:15)&hex$('B3')&sshpst$
81890     mat stcusnr$(a) !:
          mat stcusnm$(a) !:
          mat stcusa1$(a) !:
          mat stcusa2$(a) !:
          mat stcusci$(a) !:
          mat stcusst$(a) !:
          mat stcuszi$(a)
81900     stcusnr$(a)=sshpnr$ !:
          stcusnm$(a)=sshpnm$ !:
          stcusa1$(a)=sshpa1$ !:
          stcusa2$(a)=sshpa2$ !:
          stcusci$(a)=sshpci$ !:
          stcusst$(a)=sshpst$ !:
          stcuszi$(a)=sshpzi$
81910     goto SRCH_SHIPTO1
81920   end if 
81930 SRCH_SHIPTO2: ! 
81940   mat headers$(7) !:
        headers$(1)="No" !:
        headers$(2)="Ship-to name" !:
        headers$(3)="Address 1" !:
        headers$(4)="Address 2" !:
        headers$(5)="City" !:
        headers$(6)="St" !:
        headers$(7)="Zip"
81950   mat widths(7) !:
        widths(1)=2 !:
        widths(2)=25 !:
        widths(3)=14 !:
        widths(4)=14 !:
        widths(5)=12 !:
        widths(6)=2 !:
        widths(7)=5
81960   mat forms$(7) !:
        forms$(1)="c 2" !:
        forms$(2)="c 30" !:
        forms$(3)="c 25" !:
        forms$(4)="c 25" !:
        forms$(5)="c 18" !:
        forms$(6)="c 2" !:
        forms$(7)="pic(#####D####)"
81970   lcols=sum(mat widths)+2 !:
        lrows=min(20,max(3,a+2)) !:
        srow$="2" !:
        scol$="2"
81980   if env$("GUIMODE")="ON" then 
81985 ! INPUT FIELDS "23,64,c 1": PAUSE$
81990     listspec$=fnlistspec$(listwin,val(srow$),val(scol$),lrows,lcols,arows,mat headers$,mat widths,mat forms$,"Select Ship-to","",0,2)
82000     pr #listwin, fields listspec$&",=R" : (mat stcusnr$,mat stcusnm$,mat stcusa1$,mat stcusa2$,mat stcusci$,mat stcusst$,mat stcuszi$)
82010     input #listwin, fields listspec$&",rowsub,CUR ": cs !:
          fk=fkey
82011     if fk=pgdn then let curfld(1,min(udim(mat stcusnr$),cs+lrows)): goto 82010 !:
          else if fk=pgup then let curfld(1,max(1,cs-lrows)) : goto 82010
82013   if not (fk=0 or fk=99) then goto 82010
82020   close #listwin: !:
        listwin=0
82030 else 
82040   autosel=o !:
        ! rESET=1
82050   cs=fnpick(a,"2","3",min(15,a),mat csdetail$,"SHIP TO LOCATIONS","DS",66,env$("PD")&sysdir$&"\"&sys$&"HELP","A1",1,0,0,0,autosel,mat dummy$,reset,1,cstart$)
82060   pickwin(1)=fnclswin(1)
82070 end if 
82080 if cs then cstart$=csdetail$(cs)(1:2)
82090 goto ZSRCH_SHIPTO
82100 ! -------------------
82110 NOSHIPTO: ! 
82120 ! -------------------
82130 shpnr=0 !:
      shpnr$="00" !:
      shipto=0
82140 continue 
82150 ! ===============
82160 ZSRCH_SHIPTO: fnend 
82170 ! -----------------------------------------------
82180 def library fnphone$(x) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
82190 ! -----------------------------------------------
82200   x$=cnvrt$("pic(##########)",x)
82210   fnphone$="("&x$(1:3)&")"&x$(4:6)&"-"&x$(7:10)
82220 fnend 
83000 ! ====================================================================
83010 def library fnwinscrn(sfil,scrno,winno,winlin,winlen,mat sinflda$,mat shelp$;display) !:
        ! Retrieve and display screen !:
        ! SFIL            SCREEN FILE NUMBER SCREEN HELP FILE SHOULD BE!:
        !                   ONE GREATER !:
        ! WINNO           WINDOW NUMBER TO USE FOR DISPLAY
83020 ! WINLIN          NUMBER OF LINES IN THE WINDOW !:
        ! WINLEN          LENGTH OF THE LONGEST LINE IN WINDOW  !:
        ! MAT SSCREEN$    MATRIX CONTAINING THE SCREEN INFORMATION !:
        ! MAT SINWRK$     MATRIX CONTAINING INPUT FIELD PARAMETERS !:
        ! MAT SHELP$      MATRIX CONTAINING HELP SCREEN INFORMATION !:
        ! DISPLAY         IF NOT ZERO DO NOT DISPLAY SCREEN
83030 ! ====================================================================
83040   on soflow ignore 
83050   dim wscrnform$*250
83060 ! -------------------------------
83070 !  Screen Constants
83080 ! ------------------------------
83090   dim scratr$(21)*15,htext$*200,windwrk$(60)*40
83100   dim sscreen$(21)*78,inflda$(60)*40
83110   for a=1 to 21 step 1
83120     scratr$(a)=rpad$(str$(a),2)&",2,C "&str$(winlen-2)
83130   next a
83140   recno=scrno+1
83150   if version(sfil)<2 then !:
          wscrnform$="Form X 50,21*(C "&str$(winlen)&",X "&str$(78-winlen)&"),60*C 20" !:
        else !:
          wscrnform$="Form X 50,21*(C "&str$(winlen)&",X "&str$(78-winlen)&"),60*C 40"
83160   wscrnform$=cform$(wscrnform$)
83170   if screen><scrno then 
83180     read #sfil,using wscrnform$,rec=recno: mat sscreen$,mat sinflda$
83190     mat shelp$=("X")
83200     restore #(sfil+1),search=cnvrt$("pic(###)",scrno): nokey 83260 eof 83260
83210     read #sfil+1,using "form n 3,n 2": hscrno,hfldno eof 83260
83220     if hscrno=scrno then reread #sfil+1, using "form POS 6,c 1,c 2,c 200": hlevel$,hloc$,htext$ else goto 83260
83230     if hfldno>0 then shelp$(hfldno)=hlevel$&rtrm$(hloc$)&";"&rtrm$(htext$)&";"
83240     goto 83210
83250   end if 
83260 DSPWINSCRN: if not display then pr #winno: newpage ! -               Write image to display
83270   if not display then pr #winno, fields mat scratr$: mat sscreen$(1:winlin)
83280 ! MAT INWRK$=INFLDA$
83290   fnwinscrn=scrno
83300   on soflow system 
83310 fnend 
83400 def library fnwinclose(winno) !:
        !    !:
        ! |                                                             | !:
        ! |                                                             | !:
        !    !
83410 ! closes the parent window based on the value in an environmental variable
83415   fnwinclose=winno
83420   x=val(env$("FILE"&cnvrt$("PIC(###)",winno)&"_parent")) conv ZWINCLOSE
83430   if x>0 then close #x: else if winno>0 then close #winno: 
83440 setenv("FILE"&cnvrt$("PIC(###)",winno)&"_parent","")
83445 fnwinclose=0
83450 ZWINCLOSE: fnend 
