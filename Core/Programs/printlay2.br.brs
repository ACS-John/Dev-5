00020 ! Replace S:\Core\Programs\PrintLay2
00040 ! Print Layout(s)
00060 ! ______________________________________________________________________
00080   library 'S:\Core\Library': fnerror,fnwin3b,fnxit,fnopenprn,fncloseprn,fnconsole,fntop,fngetdir2
00100   on error goto ERTN
00120 ! ______________________________________________________________________
00140   dim a$(200,3)*80,h1$*55,rm$(4)*128,filename$*50,ln$*120
00160   dim a(200,6),a$*132,prg$*20,mo$(12),cap$*128,source_path$*150
00180 ! ______________________________________________________________________
00200   let fntop("S:\Core\PrintLay",cap$="Print Layout(s)")
00220   data January,February,March,April,May,June,July,August,September,October,November,December
00240   read mat mo$
00260 ! ______________________________________________________________________
00280   let fnconsole(on=1)
00300   let dat$=mo$(val(date$("MM")))&" "&date$("DD")&", "&date$("CCYY")
00320   let fnwin3b(win=101,cap$,4,65,0,2)
00340   print #win,fields "2,2,Cr 18,N": "Path and Filename:"
00360   if wbversion$<'4.20hi' then let io1$(1)="2,21,C 40/150,U" else let io1$(1)="2,21,40/C 150,U"
00380 ! 
00400 ! let source_path$="S:\acsPR\Layouts\rptrail.lay"
00420   let source_path$="S:\acsTM\Layouts\*.lay"
00440 ! let source_path$="S:\acsUB\Layouts\*.lay"
00460 ! let source_path$="S:\acsUB\Layouts\UBmstr-vb.lay"
00480 L210: ! 
00500   rinput #win,fields io1$(1): source_path$
00520   if cmdkey=5 then goto XIT
00540   let source_path$=rtrm$(source_path$)
00560   if source_path$="" then goto L210
00580   let fn_printlay3(source_path$) : goto L210
00600   def fn_old
00620     let fnopenprn(cp,58,220,process)
00640     execute "DROP DirFile" ioerr ignore
00660     execute "DIR "&source_path$&" >DirFile"
00680     open #2: "Name=DirFile",display,input 
00700     let p1=pos(source_path$,"/",1)
00740 L330: let f2=f2+1: let rl=l=a=col_6=ino=pg=j3=0: mat a=(0)
00760     linput #2: a$ eof L1380
00800     let f2=1: let f1=2
00820     open #1: "Name="&source_path$,display,input ioerr L330
00840     goto OLD_READ_LINE
01060 OLD_READ_LINE: ! 
01080     linput #1: ln$ eof L1180
01100     if pos(srep$(ln$,'^','~'),'~')<=0 and pos(uprc$(ln$),'DATA ')>0 then let ln$=srep$(ln$,',','^')
01120     if uprc$(ln$(7:10))=uprc$("LET ") then goto LETLN
01140     if uprc$(ln$(7:10))=uprc$("DATA") then goto DATALN
01160     if uprc$(ln$(7:7))=uprc$("!") then print #255: ln$(9:len(ln$))
01180     let p1=pos(ln$,"REPLACE",6)
01200     if p1=0 then goto L690
01220     let p1=p1+8
01240     let p2=pos(ln$,",",p1)
01260     let prg$=ln$(p1:p2-1)
01280 L690: goto OLD_READ_LINE
01300 ! ______________________________________________________________________
01320 LETLN: let p2=len(rtrm$(ln$))-1
01340     let p1=pos(ln$,"H1$",1)
01360     if p1>0 then let h1$=ln$(p1+5:p2) : goto OLD_READ_LINE
01380     let p1=pos(ln$,"FILETYPE$",1)
01400     if p1>0 then let filetype$=ln$(p1+11:p2): goto OLD_READ_LINE
01420     let p1=pos(ln$,"FILENAME$",1)
01440     if p1>0 then let filename$=ln$(p1+11:p2): goto OLD_READ_LINE
01460     let p1=pos(ln$,"VOLID$",1)
01480     if p1>0 then let volid$=ln$(p1+8:p2): goto OLD_READ_LINE
01500     let p1=pos(ln$,"RM$",1)
01520     if p1=0 then goto L840
01540     let rm=val(ln$(p1+4:p1+4))
01560     let rm$(rm)=ln$(p1+8:p2)
01580 L840: goto OLD_READ_LINE
01600 ! ______________________________________________________________________
01620 DATALN: ! 
01630     let j3=j3+1
01640     let p1=11
01660     let p2=pos(srep$(ln$,'^','~'),'~',p1+1) ! pos(ln$,"^",p1+1)
01680     let p3=pos(srep$(ln$,'^','~'),'~',p2+1) ! pos(ln$,"^",p2+1)
01700     let p4=len(rtrm$(ln$))
01720     let a$(j3,1)=ln$(p1:p2-1)
01740     let a$(j3,2)=ln$(p2+1:p3-1)
01760     let a$(j3,3)=ln$(p3+1:p4)
01780 L940: form c 9,skip 0
01800     if rtrm$(a$(j3,3))="" then goto L1160
01820     let p1=pos(a$(j3,3)," ",1)+1
01840     let p2=pos(a$(j3,3),".",1)+1
01860     let p3=len(rtrm$(a$(j3,3)))
01880     let p4=pos(a$(j3,3),"*",1)
01900     if p4=0 then let m1=1 else let m1=val(a$(j3,3)(1:p4-1))
01920     let l=int(val(a$(j3,3)(p1:pos(srep$(a$(j3,3),'^','~'),'~')-1))) ! FIELD STORAGE LENGTH
01940 ! pos(srep$(a$(j3,3),'^','~'),'~')-1         was      P3
01960     if p2>1 then let dp=val(a$(j3,3)(p2:pos(srep$(a$(j3,3),'^','~'),'~')-1)) else let dp=0 ! DECIMAL POS.
01980 ! pos(srep$(a$(j3,3),'^','~'),'~')-1      was     P3
02000     if uprc$(a$(j3,3)(1:p1-2))="PD" then let al=l*2-1 else let al=l !   ACTUAL FIELD LENGTH
02020     let l=l*m1 ! TOTAL STORAGE LENGTH
02040     let col_6=a+l
02060     let a=a+1
02080     let ino=ino+1
02100     let a(j3,1)=ino
02120     let a(j3,2)=al
02140     let a(j3,3)=dp
02160     let a(j3,4)=l
02180     let a(j3,5)=a
02200     let a(j3,6)=col_6
02220     let a=col_6
02240     let rl=rl+int(val(a$(j3,3)(p1:pos(srep$(a$(j3,3),'^','~'),'~')-1)))*m1 ! pos(srep$(a$(j3,3),'^','~'),'~')-1      was    P3
02260 L1160: goto OLD_READ_LINE
02280 ! ______________________________________________________________________
02300 L1180: let pgo=ceil(j3/24)
02320     gosub HDR
02340     for j=1 to j3
02360       let p1=pos(a$(j,3)," ",1)
02380       let p2=len(a$(j,3))
02400       let l=val(a$(j,3)(p1:pos(srep$(a$(j,3),'^','~'),'~')-1)) ! pos(srep$(a$(j,3),'^','~'),'~')-1   was    P2
02420       if l>0 then goto L1290
02440       print #255,using L1270: a$(j,1)
02460 L1270: form pos 13,c 43,skip 2
02480       goto L1330
02500 L1290: if rtrm$(a$(j,1))="" then goto L1330
02520       let a$(j,3)=a$(j,3)(1:pos(srep$(a$(j,3),'^','~'),'~')-1)
02540       print #255,using L1320: a(j,1),a$(j,1),a$(j,2),a(j,2),a(j,3),a$(j,3),a(j,4),a(j,5),a(j,6)
02560 L1320: form pos 5,n 5,x 3,c 43,c 21,n 7,n 10,x 5,c 11,n 7,2*n 9,skip 1 ! c 11 was c 11
02580 L1330: next j
02620     print #255,using L940: hex$("1B40")
02640     close #1: 
02660     if f2<f1 then goto L330
02680 L1380: close #2,free: 
02700     let fncloseprn
02720     goto XIT
02740 ! ______________________________________________________________________
02760 XIT: let fnxit
02780 ! ______________________________________________________________________
02900 HDR: ! 
02920     let pg=pg+1
02940     print #255,using L1520: h1$
02960 L1520: form pos 3,c 46
02980     print #255,using L1540: prg$,dat$,pg,pgo
03000 L1540: form pos 51,c 40,c 20,"Page ",n 3,"  of ",n 3
03020     print #255,using L1560: str$(rl),rm$(1)
03040 L1560: form pos 5,"Record Length: ",c 5,pos 30,c 45
03060     print #255,using L1590: rm$(2)
03080     print #255,using L1590: rm$(3)
03100 L1590: form pos 30,c 45
03120     print #255,using L1610: filetype$,rm$(4)
03140 L1610: form pos 5,"File Type: ",c 10,pos 30,c 128
03160     print #255,using L1630: filename$,volid$
03180 L1630: form pos 5,"File Name: ",c 50,"Directory: ",c 10
03200     print #255: ""
03220     print #255: "   Item #   Field Description                           Name                 Length    Decimals    Format    Storage     From      To  "
03240     print #255: "   ______   ________________________________________   __________________    ______    ________   _______    _______    _____    _____"
03260     return 
03280   fnend 
03300 ! ______________________________________________________________________
03320 ! <Updateable Region: ERTN>
03340 ERTN: let fnerror(program$,err,line,act$,"xit")
03360   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03380   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03400   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
03420 ERTN_EXEC_ACT: execute act$ : goto ERTN
03440 ! /region
03460 ! ______________________________________________________________________
03480   def fn_file_to_array(fta_file$*256,mat fta_line$)
03500     open #1: 'Name='&fta_file$,display,input 
03520     let fta_line_item=0
03540     do 
03560       let fta_line_item+=1
03580       mat fta_line$(fta_line_item)
03600       linput #1: fta_line$(fta_line_item) eof FTA_DONE
03620     loop 
03640 FTA_DONE: ! 
03660     close #1: 
03680   fnend  ! fn_file_to_array
03700   def fn_printlay3(source_path$*256)
03720     dim p3_filename$(1)*256,p3_filter$*20
03740     if pos(source_path$,'*')>0 or pos(source_path$,'?')>0 then 
03760       let source_path$=trim$(source_path$)
03780       let p3_pos_slash_last=pos(source_path$,'\',-1)
03800       let p3_pos_slash_last=pos(source_path$,'\',-1)
03820       let p3_filter$=source_path$(p3_pos_slash_last+1:len(source_path$))
03840       let source_path$=source_path$(1:p3_pos_slash_last)
03860       let fngetdir2(source_path$,mat p3_filename$,'',p3_filter$)
03880     else 
03900       let p3_filename$(1)=rtrm$(source_path$,'\')
03910       let source_path$=''
03920     end if  ! 
03940     dim p3_line$(1)*256
03960     let fnopenprn(cp,58,220,process)
03980 P3_FORM_PRINT: form pos 1,g 4,x 1,c 5,x 1,c 5,x 1,c 30,x 1,c 80
04000     for p3_file_item=1 to udim(mat p3_filename$)
04020 !  if trim$(p3_filename$(p3_file_item))=trim$(source_path$) then let source_path$=''
04040       let fn_file_to_array(source_path$&p3_filename$(p3_file_item),mat p3_line$)
04060       if p3_file_item>1 then print #255: newpage
04080       print #255: os_filename$(source_path$&p3_filename$(p3_file_item))
04100       let p3_spos=1
04120       for p3_line_item=1 to udim(mat p3_line$)
04140         dim p3_tmp$*256
04160         let p3_tmp$=p3_line$(p3_line_item)
04180         let p3_tmp$(1:6)='' ! strip line number and leading space
04200         if lwrc$(p3_tmp$(1:5))='data ' then 
04220           if p3_spos=1 then 
04240             print #255,using P3_FORM_PRINT: 'spos','fmt','len','var','description'
04260           end if  ! p3_spos=1
04280           let p3_tmp$(1:5)=''
04300           let p3_tmp$=srep$(p3_tmp$,chr$(9),' ')
04320 ! ie remaing data might be something like:  Customer Number^Z$^C 10^Customer #
04340           dim p3_tmp_field$(1)*80
04360           let str2mat(p3_tmp$,mat p3_tmp_field$,'^')
04380           if udim(p3_tmp_field$)<3 then 
04400             print #255: '    '&p3_tmp$
04420           else 
04440             let str2mat (p3_tmp_field$(3),mat p3_tmp_format$,' ')
04460             let str2mat (p3_tmp_field$(3),mat p3_tmp_format$,' ')
04480             print #255,using P3_FORM_PRINT: p3_spos,p3_tmp_format$(1),p3_tmp_format$(2),p3_tmp_field$(2),p3_tmp_field$(1)
04500             let p3_pos_star=pos(p3_tmp_format$(1),'*')
04520             if p3_pos_star<=0 then 
04540               let p3_spos+=int(val(p3_tmp_format$(2)))
04560             else 
04580               let p3_tmp=int(val(p3_tmp_format$(2)))
04600               let p3_tmp=p3_tmp*val(p3_tmp_format$(1)(1:p3_pos_star-1))
04620               let p3_spos+=p3_tmp
04640             end if  ! p3_pos_star<=0   /   else 
04660           end if  ! udim(p3_tmp_field$)=1   /   else 
04680         else 
04700           print #255: p3_tmp$
04720         end if  ! lwrc$(p3_tmp$(1:5))='data '   /   else 
04740       next p3_line_item
04760     next p3_file_item
04780     let fncloseprn
04800   fnend  ! fn_printlay3
